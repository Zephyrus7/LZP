#==============================================================================#
#       VERİ AKTARIM ARACI (Tüm Kontroller ve Basit Sütun Seçimi)              #
#==============================================================================#
# AMAÇ: Bu script, verileri okur, temizler, anomali kontrolü yapar ve
#       veriyi MySQL'e YALNIZCA BİR KEZ yazar. BİR UYGULAMA DEĞİLDİR.
#==============================================================================#

#--- 1. GEREKLİ KÜTÜPHANELERİ VE BAĞLANTIYI YÜKLE ---
source("00_Config.R")
source("00_DB_Connector.R") # Sadece 'db_pool' oluşturur.
library(tools)

cat("Veritabanı bağlantısı kuruldu.\n\n")

#--- 2. YARDIMCI FONKSİYONLAR ---
read_any_format <- function(file_path) {
  ext <- tolower(file_ext(file_path))
  if (ext == "csv") { cat("CSV formatı algılandı. read_csv kullanılıyor...\n"); return(read_csv(file_path, col_types = cols(.default = "c")))
  } else if (ext %in% c("xlsx", "xls")) { cat("Excel formatı algılandı. read_excel kullanılıyor...\n"); return(read_excel(file_path, col_types = "text"))
  } else { stop("Desteklenmeyen dosya formatı: ", ext, call. = FALSE) }
}
show_file_chooser <- function(title) { cat(sprintf("Lütfen '%s' dosyasını seçin...\n", title)); file_path <- file.choose(); if (length(file_path) == 0) stop("Dosya seçimi iptal edildi.", call. = FALSE); cat("Seçildi: ", basename(file_path), "\n"); return(file_path) }

#--- 3. GÜVENLİK UYARISI VE TABLOLARI TEMİZLEME ---
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ÖNEMLİ UYARI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
# Bu script her çalıştığında, mükerrer veriyi önlemek için tabloları temizler.
dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 0;")
dbExecute(db_pool, "TRUNCATE TABLE b2b_kargolar;")
dbExecute(db_pool, "TRUNCATE TABLE sikayetler;")
dbExecute(db_pool, "TRUNCATE TABLE gonderiler;")
dbExecute(db_pool, "TRUNCATE TABLE tahmini_teslimatlar;")
dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 1;")
cat("Mevcut tablolar başarıyla temizlendi.\n\n")

#--- 4. DOSYALARI OKU ---
ana_veri_path <- show_file_chooser("Ana Veri")
sikayet_path <- show_file_chooser("Şikayet Datası")
teslimat_path <- show_file_chooser("Tahmini Teslimat")
b2b_list_path <- show_file_chooser("B2B Kargo Listesi")

#--- 5. VERİYİ İŞLE VE TABLOLARA YAZ ---
# 5.1. 'gonderiler' Tablosunu Doldur
cat("\nİşleniyor: 'gonderiler' tablosu...\n")
ana_veri_raw <- read_any_format(ana_veri_path) %>% clean_names()
gonderiler_for_processing <- ana_veri_raw %>%
  select(
    kargo_no, kargo_turu, kargo_durumu = teslim_durumu, gonderici = marka_adi,
    sehir, ilce, desi, toplam_teslim_suresi_saat, kargo_tarihi, teslim_tarihi,
    tahmini_teslimat_tarihi = tahmini_teslimat_zamani, son_hareket_tarihi
  )
gonderiler_to_db <- gonderiler_for_processing %>%
  mutate(across(ends_with("_tarihi"), ~ parse_date_time(., orders = c("d.m.Y H:M", "d.m.Y H:M:S", "Y-m-d H:M:S"), tz = "Europe/Istanbul", quiet = TRUE)), desi = as.numeric(desi), toplam_teslim_suresi_saat = as.numeric(toplam_teslim_suresi_saat)) %>%
  group_by(kargo_no) %>% arrange(desc(son_hareket_tarihi)) %>% slice(1) %>% ungroup()
cat("Tarih verileri kontrol ediliyor...\n")
max_tarih_veride <- max(gonderiler_to_db$son_hareket_tarihi, na.rm = TRUE)
bugun_arti_bir_gun <- Sys.time() + days(1)
if (is.infinite(max_tarih_veride) || max_tarih_veride > bugun_arti_bir_gun) {
  stop(sprintf("HATA: Veri içinde geleceğe ait veya geçersiz bir tarih bulundu (%s).", max_tarih_veride), call. = FALSE)
}
cat("Tarih kontrolü başarılı. En güncel tarih:", format(max_tarih_veride, "%Y-%m-%d"), "\n")
cat(nrow(ana_veri_raw) - nrow(gonderiler_to_db), "adet mükerrer gönderi kaydı temizlendi.\n")
tryCatch({dbWriteTable(db_pool, "gonderiler", gonderiler_to_db, append = TRUE, row.names = FALSE); cat(nrow(gonderiler_to_db), "adet benzersiz kayıt 'gonderiler' tablosuna başarıyla yazıldı.\n\n")}, error = function(e) {stop("HATA: 'gonderiler' tablosuna yazarken sorun oluştu: ", e$message)})

# 5.2. 'sikayetler' Tablosunu Doldur
cat("İşleniyor: 'sikayetler' tablosu...\n")
sikayet_raw <- read_any_format(sikayet_path) %>% clean_names()
sikayetler_to_db <- sikayet_raw %>%
  mutate(kargo_no = str_extract(konu, "\\d{10,}") %>% sub("^0+", "", .), sikayet_tarihi = parse_date_time(olusturulma_tarihi, orders = c("d.m.Y H:M", "d.m.Y H:M:S", "Y-m-d H:M:S"), tz = "Europe/Istanbul", quiet = TRUE)) %>%
  filter(!is.na(kargo_no) & kargo_no %in% gonderiler_to_db$kargo_no) %>% select(kargo_no, konu, sikayet_tarihi)
tryCatch({dbWriteTable(db_pool, "sikayetler", sikayetler_to_db, append = TRUE, row.names = FALSE); cat(nrow(sikayetler_to_db), "adet kayıt 'sikayetler' tablosuna başarıyla yazıldı.\n\n")}, error = function(e) {stop("HATA: 'sikayetler' tablosuna yazarken sorun oluştu: ", e$message)})

# 5.3. 'tahmini_teslimatlar' Tablosunu Doldur
cat("İşleniyor: 'tahmini_teslimatlar' tablosu...\n")
teslimat_raw <- read_any_format(teslimat_path) %>% clean_names()
teslimatlar_to_db <- teslimat_raw %>% select(sehir, yeni_max_teslimat_suresi) %>% mutate(yeni_max_teslimat_suresi = as.numeric(yeni_max_teslimat_suresi))
tryCatch({dbWriteTable(db_pool, "tahmini_teslimatlar", teslimatlar_to_db, append = TRUE, row.names = FALSE); cat(nrow(teslimatlar_to_db), "adet kayıt 'tahmini_teslimatlar' tablosuna başarıyla yazıldı.\n\n")}, error = function(e) {stop("HATA: 'tahmini_teslimatlar' tablosuna yazarken sorun oluştu: ", e$message)})

# 5.4. 'b2b_kargolar' Tablosunu Doldur
cat("İşleniyor: 'b2b_kargolar' tablosu...\n")
b2b_raw <- read_any_format(b2b_list_path) %>% clean_names()
b2b_to_db <- b2b_raw %>% select(kargo_no) %>% mutate(kargo_no = sub("^0+", "", kargo_no)) %>% filter(!is.na(kargo_no) & kargo_no != "") %>% distinct()
tryCatch({dbWriteTable(db_pool, "b2b_kargolar", b2b_to_db, append = TRUE, row.names = FALSE); cat(nrow(b2b_to_db), "adet benzersiz B2B kargo numarası 'b2b_kargolar' tablosuna başarıyla yazıldı.\n\n")}, error = function(e) {stop("HATA: 'b2b_kargolar' tablosuna yazarken sorun oluştu: ", e$message)})

#--- 6. İŞLEMİ BİTİR ---
cat("----------------------------------------------------------\n")
cat("VERİ AKTARIMI BAŞARIYLA TAMAMLANDI!\n")
cat("Artık Shiny uygulamasını çalıştırıp analiz yapabilirsiniz.\n")
cat("----------------------------------------------------------\n")

# Bu bir script olduğu için, sonunda bağlantı havuzunu güvenle kapatır.
try(poolClose(db_pool), silent = TRUE)
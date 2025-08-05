# 00_Live_Data_Migration_Tool.R - WORK STEPS HATA AYIKLAMA (v1.8 - Tam Hali)

#==============================================================================#
#       CANLI VERİ AKTARIM ARACI (Work Steps Yazma Onarımı v1.8)               #
#==============================================================================#
# GÜNCELLEME: 'live_work_steps' tablosunun boş olduğu tespit edildi.
#             Bu tabloyu yazan kod bloğu, sorunu tespit etmek için özel
#             hata ayıklama mesajları ile güncellendi.
#==============================================================================#

#--- 1. KURULUM: Gerekli Kütüphaneler ve Veritabanı Bağlantısı ---
cat("Gerekli kütüphaneler ve veritabanı bağlantısı kuruluyor...\n")
source("00_Config.R")
source("00_DB_Connector.R") # Sadece 'db_pool' nesnesini oluşturur.
library(tools)             # Dosya uzantısını almak için
library(dplyr)             # Pipe (%>%) operatörü ve veri manipülasyonu için gerekli

cat("Veritabanı bağlantısı 'db_pool' başarıyla oluşturuldu.\n\n")


#--- 2. YARDIMCI FONKSİYONLAR ---
read_any_format <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  cat(sprintf("'%s' formatı algılandı, okuma işlemi başlıyor...\n", ext))
  if (ext == "csv") {
    return(read_csv(file_path, col_types = cols(.default = "c")))
  } else if (ext %in% c("xlsx", "xls")) {
    return(read_excel(file_path, col_types = "text"))
  } else {
    stop("Desteklenmeyen dosya formatı: ", ext, ". Lütfen .csv veya .xlsx kullanın.", call. = FALSE)
  }
}

show_file_chooser <- function(title) {
  cat(sprintf("\nLütfen '%s' dosyasını seçin...\n", title))
  file_path <- file.choose()
  if (length(file_path) == 0) {
    stop("Dosya seçimi iptal edildi. İşlem durduruldu.", call. = FALSE)
  }
  cat("Seçilen Dosya: ", basename(file_path), "\n")
  return(file_path)
}

safe_parse_datetime <- function(datetime_string) {
  tryCatch({
    parse_date_time(datetime_string, 
                    orders = c("Y-m-d H:M:S", "d.m.Y H:M", "Y/m/d H:M:S"), 
                    tz = "Europe/Istanbul", 
                    quiet = TRUE)
  }, error = function(e) {
    as.POSIXct(NA)
  })
}


#--- 3. VERİTABANI HAZIRLIĞI ---
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ÖNEMLİ UYARI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
cat("Canlı Analiz tabloları ('live_' ile başlayanlar) sıfırlanacak.\n")

tryCatch({
  dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 0;")
  
  dbExecute(db_pool, "DROP TABLE IF EXISTS live_orders;")
  cat("'live_orders' tablosu (varsa) başarıyla silindi.\n")
  
  other_live_tables <- c("live_work_steps", "live_companies", "live_cities", "live_districts", "live_edd_cities", "live_edd_days")
  for (table_name in other_live_tables) {
    if (dbExistsTable(db_pool, table_name)) {
      dbExecute(db_pool, paste0("TRUNCATE TABLE ", table_name, ";"))
      cat(sprintf("'%s' tablosu başarıyla temizlendi.\n", table_name))
    }
  }
  
  dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 1;")
  cat("Tüm canlı analiz tabloları bir sonraki yazma işlemi için hazır.\n")
}, error = function(e) {
  stop("Veritabanı tabloları hazırlanırken bir hata oluştu: ", e$message)
})


#--- 4. HAM VERİ DOSYALARINI SEÇME ---
orders_path      <- show_file_chooser("Orders (Olay Kayıtları)")
work_steps_path  <- show_file_chooser("Work Steps (Durum Sözlüğü)")
companies_path   <- show_file_chooser("Companies (Operasyon Türleri/Firmalar)")
cities_path      <- show_file_chooser("Cities (İller)")
districts_path   <- show_file_chooser("Districts (İlçeler)")
edd_cities_path  <- show_file_chooser("EDD Cities (Tahmini Teslimat Süreleri)")
edd_days_path    <- show_file_chooser("EDD Days (İş Günü Başlangıç Kuralları)")


#--- 5. VERİYİ İŞLEME VE VERİTABANINA YAZMA ---

# 5.1. `live_orders` Tablosu
tryCatch({
  cat("\nİşleniyor: 'live_orders' tablosu...\n")
  orders_raw <- read_any_format(orders_path)
  
  char_cols <- sapply(orders_raw, is.character)
  orders_raw[char_cols] <- lapply(orders_raw[char_cols], function(x) {
    iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  })
  
  orders_raw <- orders_raw %>% clean_names()
  
  orders_to_db <- orders_raw %>%
    transmute(
      event_id = suppressWarnings(as.integer(id)),
      order_tracking_no = as.character(special_tracking_number),
      status_id = suppressWarnings(as.integer(status)),
      sub_status_id = suppressWarnings(as.integer(sub_status_id)),
      company_id = suppressWarnings(as.integer(company_id)),
      company_customer_id = suppressWarnings(as.integer(company_customer_id)),
      event_timestamp = safe_parse_datetime(created_at),
      cargo_date = safe_parse_datetime(cargo_date),
      updated_at = safe_parse_datetime(updated_at),
      completed_at = safe_parse_datetime(completed_at),
      estimated_delivery_date = safe_parse_datetime(estimated_delivery_date)
    )
  
  cat(sprintf("Veritabanına yazılmadan önce 'orders_to_db' veri çerçevesinde %d satır ve %d sütun var.\n", nrow(orders_to_db), ncol(orders_to_db)))
  
  if(nrow(orders_to_db) > 0) {
    cat("'live_orders' tablosu R veri çerçevesine göre oluşturuluyor ve veriler yazılıyor...\n")
    dbExecute(db_pool, "DROP TABLE IF EXISTS live_orders;")
    dbWriteTable(db_pool, "live_orders", orders_to_db, row.names = FALSE)
    
    rows_in_db <- dbGetQuery(db_pool, "SELECT COUNT(*) AS n FROM live_orders;")$n
    cat(sprintf("YAZMA İŞLEMİ TAMAMLANDI. Veritabanındaki 'live_orders' tablosunda şu an %d satır var.\n", rows_in_db))
    
    if(rows_in_db == 0) {
      stop("KRİTİK HATA: Tablo oluşturulup yazılmasına rağmen hala 0 satır var. Beklenmedik bir sorun.")
    } else {
      cat("'live_orders' tablosu başarıyla oluşturuldu ve veriler yazıldı!\n")
    }
  } else {
    warning("YAZMA ATLANDI. 'orders_to_db' veri çerçevesi boş olduğu için veritabanına yazma işlemi yapılmadı.")
  }
  
}, error = function(e) { stop("KRİTİK HATA: 'live_orders' işlenirken işlem durdu: ", e$message) })


# 5.2. Sözlük ve Kural Tablolarını Yeniden Oluştur
create_and_write_table <- function(pool, table_name, data_to_write) {
  tryCatch({
    cat(sprintf("\nİşleniyor: '%s' tablosu...\n", table_name))
    
    if(nrow(data_to_write) > 0) {
      dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s;", table_name))
      dbWriteTable(pool, table_name, data_to_write, row.names = FALSE)
      rows_in_db <- dbGetQuery(pool, sprintf("SELECT COUNT(*) AS n FROM %s;", table_name))$n
      cat(sprintf("BAŞARILI: '%s' tablosu oluşturuldu ve %d kayıt yazıldı.\n", table_name, rows_in_db))
    } else {
      warning(sprintf("ATLANDI: '%s' için veri çerçevesi boş.", table_name))
    }
  }, error = function(e) { stop(sprintf("KRİTİK HATA: '%s' işlenirken işlem durdu: ", table_name, e$message)) })
}

# === GÜNCELLEME: `live_work_steps` İÇİN ÖZEL KONTROL ===
tryCatch({
  cat("\n--- ÖZEL KONTROL: 'live_work_steps' işleniyor... ---\n")
  
  # Adım A: Dosyayı oku ve ham veriyi göster
  cat("Adım A: 'work_steps_path' dosyasından veri okunuyor...\n")
  ws_raw <- read_any_format(work_steps_path)
  cat(sprintf("Adım A BAŞARILI: Ham veride %d satır ve %d sütun okundu.\n", nrow(ws_raw), ncol(ws_raw)))
  print(head(ws_raw))
  
  # Adım B: Veriyi temizle ve dönüştür (transmute)
  cat("\nAdım B: Veri temizleniyor ve dönüştürülüyor (transmute)...\n")
  ws_to_db <- ws_raw %>% 
    clean_names() %>% 
    transmute(
      status_id = as.integer(id), 
      status_name = as.character(name), 
      status_value = as.character(value)
    )
  cat(sprintf("Adım B BAŞARILI: Dönüştürülmüş veride %d satır var.\n", nrow(ws_to_db)))
  print(head(ws_to_db))
  
  # Adım C: Veritabanına yaz ve sonucu kontrol et
  cat("\nAdım C: Veritabanına yazma işlemi başlatılıyor...\n")
  create_and_write_table(db_pool, "live_work_steps", ws_to_db)
  
}, error = function(e) {
  stop("KRİTİK HATA: 'live_work_steps' özel kontrol bloğunda işlem durdu: ", e$message)
})

co_to_db <- read_any_format(companies_path) %>% clean_names() %>% transmute(company_id = as.integer(id), company_name = as.character(name), company_type = as.character(type))
create_and_write_table(db_pool, "live_companies", co_to_db)

cities_to_db <- read_any_format(cities_path) %>% clean_names() %>% transmute(city_id = as.integer(id), city_name = as.character(name))
create_and_write_table(db_pool, "live_cities", cities_to_db)

districts_to_db <- read_any_format(districts_path) %>% clean_names() %>% transmute(district_id = as.integer(id), city_id = as.integer(city_id), district_name = as.character(name))
create_and_write_table(db_pool, "live_districts", districts_to_db)

edd_c_to_db <- read_any_format(edd_cities_path) %>% clean_names() %>% transmute(city_id = as.numeric(id), min_delivery_hour = as.numeric(min_delivery_hour), max_delivery_hour = as.numeric(max_delivery_hour))
create_and_write_table(db_pool, "live_edd_cities", edd_c_to_db)

edd_d_to_db <- read_any_format(edd_days_path) %>% clean_names() %>% transmute(day_name = as.character(day), start_hour = as.integer(start_hour), end_hour = as.integer(end_hour), added_day = as.integer(added_day), added_hour = as.integer(added_hour))
create_and_write_table(db_pool, "live_edd_days", edd_d_to_db)


#--- 6. İŞLEMİ BİTİR ---
cat("\n----------------------------------------------------------\n")
cat("CANLI VERİ AKTARIMI BAŞARIYLA TAMAMLANDI!\n")
cat("Tüm canlı analiz tabloları lokal veritabanında oluşturuldu/güncellendi.\n")
cat("UYARI: Bu script veritabanı bağlantı havuzunu kapattı.\n")
cat("Ana Shiny uygulamasını çalıştırmadan önce R oturumunu yeniden başlatın (Ctrl+Shift+F10).\n")
cat("----------------------------------------------------------\n")

try(poolClose(db_pool), silent = TRUE)
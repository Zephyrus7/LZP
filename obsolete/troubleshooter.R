#==============================================================================#
#       VERİ TUTARSIZLIĞI TEŞHİS ARACI (En Güncel Kargoları Listeler)           #
#==============================================================================#
# AMAÇ: Bu script, veritabanındaki mevcut durumu (tarih aralığı ve en son
#       eklenen gönderiler) raporlar ve kaynak dosya ile arasındaki farkları
#       aykırı değerleri de dikkate alarak analiz eder.
#       VERİTABANINDA HİÇBİR DEĞİŞİKLİK YAPMAZ (SADECE OKUMA YAPAR).
#==============================================================================#

# --- 1. KURULUM ---
source("00_Config.R")
source("00_DB_Connector.R")
library(tools)
library(svDialogs)

cat("Teşhis aracı başlatıldı. Veritabanı bağlantısı (salt okunur) kuruldu.\n\n")

# --- 2. YARDIMCI FONKSİYONLAR ---
# ... (Bu fonksiyonlarda değişiklik yok) ...
is_gunu_saati_hesapla_super_vectorized<-function(baslangic_vektoru,bitis_vektoru){brut_saat<-as.numeric(difftime(bitis_vektoru,baslangic_vektoru,units="hours"));start_days<-as.Date(baslangic_vektoru);end_days<-as.Date(bitis_vektoru);num_sundays<-(as.integer(end_days)-as.integer(start_days)+wday(start_days,week_start=1))%/%7;pazar_saati_dusulecek<-num_sundays*24;baslangic_pazar_mi<-wday(baslangic_vektoru,week_start=1)==7;baslangic_duzeltme<-if_else(baslangic_pazar_mi,24-(hour(baslangic_vektoru)+minute(baslangic_vektoru)/60+second(baslangic_vektoru)/3600),0);bitis_pazar_mi<-wday(bitis_vektoru,week_start=1)==7;bitis_duzeltme<-if_else(bitis_pazar_mi,hour(bitis_vektoru)+minute(bitis_vektoru)/60+second(bitis_vektoru)/3600,0);net_saat<-brut_saat-pazar_saati_dusulecek+baslangic_duzeltme+bitis_duzeltme;net_saat[is.na(baslangic_vektoru)|is.na(bitis_vektoru)|bitis_vektoru<baslangic_vektoru]<-NA_real_;return(pmax(0,net_saat,na.rm=TRUE))}
read_any_format<-function(file_path){ext<-tolower(file_ext(file_path));if(ext=="csv"){return(read_csv(file_path,col_types=cols(.default="c")))}else if(ext%in%c("xlsx","xls")){return(read_excel(file_path,col_types="text"))}else{stop("Desteklenmeyen dosya formatı: ",ext,call.=FALSE)}}
show_file_chooser<-function(title){cat(sprintf("Lütfen karşılaştırılacak '%s' dosyasını seçin...\n",title));file_path_obj<-svDialogs::dlg_open(title=title,multiple=FALSE);if(!length(file_path_obj$res)){stop("Dosya seçimi iptal edildi.",call.=FALSE)};file_path<-file_path_obj$res;cat("Seçildi: ",basename(file_path),"\n\n");return(file_path)}

# --- 3. VERİLERİ ÇEK ---
ana_veri_path <- show_file_chooser("Ana Veri Kaynak Dosyası (Excel/CSV)")
source_df_raw <- read_any_format(ana_veri_path)

cat("Veritabanından gerekli tablolar ve durum bilgileri çekiliyor...\n")
db_df <- dbReadTable(db_pool, "gonderiler")
tahmini_teslimat_df <- dbReadTable(db_pool, "tahmini_teslimatlar")

# <<< GÜNCELLENEN KISIM: Hem tarih aralığını hem de en güncel kargoları çek >>>
db_date_range_query <- "SELECT MIN(son_hareket_tarihi) AS min_date, MAX(son_hareket_tarihi) AS max_date FROM gonderiler"
query_latest_shipments <- "SELECT * FROM gonderiler WHERE son_hareket_tarihi = (SELECT MAX(son_hareket_tarihi) FROM gonderiler)"

db_date_range <- tryCatch(dbGetQuery(db_pool, db_date_range_query), error = function(e) data.frame(min_date = NA, max_date = NA))
latest_shipments_df <- tryCatch(dbGetQuery(db_pool, query_latest_shipments), error = function(e) data.frame())
# <<< GÜNCELLENEN KISMIN SONU >>>

cat(sprintf("Veritabanında %s gönderi ve %s teslimat süresi kaydı bulundu.\n\n", format(nrow(db_df), big.mark = ","), nrow(tahmini_teslimat_df)))

# --- 4. AYKIRI DEĞER ANALİZİ (Uygulama Mantığının Simülasyonu) ---
# ... (Bu bölümde değişiklik yok) ...
cat("Kaynak dosyadaki veriler, uygulama mantığına göre aykırı değer tespiti için hazırlanıyor...\n")
source_df_for_analysis<-source_df_raw %>% clean_names() %>% select(kargo_no, kargo_turu, sehir, ilce, kargo_tarihi, teslim_tarihi, yeni_max_teslimat_suresi = tahmini_teslimat_zamani, son_hareket_tarihi) %>% mutate(across(c(kargo_tarihi, teslim_tarihi, son_hareket_tarihi), ~ parse_date_time(., orders = c("d.m.Y H:M","d.m.Y H:M:S","Y-m-d H:M:S"), tz = "Europe/Istanbul", quiet = TRUE)), yeni_max_teslimat_suresi = as.numeric(yeni_max_teslimat_suresi), sehir = str_to_lower(sehir, "tr")) %>% mutate(toplam_teslim_suresi_saat = is_gunu_saati_hesapla_super_vectorized(kargo_tarihi, teslim_tarihi)) %>% left_join(tahmini_teslimat_df %>% select(sehir, db_teslimat_suresi = yeni_max_teslimat_suresi), by = "sehir") %>% mutate(yeni_max_teslimat_suresi = ifelse(is.na(yeni_max_teslimat_suresi), db_teslimat_suresi, yeni_max_teslimat_suresi))
cat("Aykırı değerler tespit ediliyor...\n")
tolerans_orani<-0.25
source_aykiri_degerler<-source_df_for_analysis %>% filter(!is.na(toplam_teslim_suresi_saat)) %>% group_by(ilce,sehir,kargo_turu) %>% mutate(q1=quantile(toplam_teslim_suresi_saat,0.25,na.rm=TRUE),q3=quantile(toplam_teslim_suresi_saat,0.75,na.rm=TRUE),iqr=q3-q1,lower_bound=q1-5.5*iqr,upper_bound=q3+5.5*iqr,is_too_low=toplam_teslim_suresi_saat<lower_bound,is_too_high=toplam_teslim_suresi_saat>upper_bound) %>% ungroup() %>% mutate(is_sla_breach=if_else(!is.na(yeni_max_teslimat_suresi),toplam_teslim_suresi_saat>yeni_max_teslimat_suresi,FALSE),toleransli_ust_sinir=yeni_max_teslimat_suresi*(1+tolerans_orani)) %>% mutate(aykiri_deger_nedeni=case_when(toplam_teslim_suresi_saat==0~"Geçersiz Süre (0 Saat)",is_too_low~"Aşırı Düşük Teslimat Süresi",is_too_high&is_sla_breach&(toplam_teslim_suresi_saat>toleransli_ust_sinir)~"Aşırı Yüksek Teslimat Süresi",TRUE~NA_character_)) %>% filter(!is.na(aykiri_deger_nedeni)) %>% select(kargo_no,sehir,ilce,kargo_turu,teslim_suresi=toplam_teslim_suresi_saat,tahmini_sure=yeni_max_teslimat_suresi,cikarilma_nedeni=aykiri_deger_nedeni)

# --- 5. KARŞILAŞTIRMA VE RAPORLAMA ---
cat("\n======================= TEŞHİS RAPORU (v5 - Tam Analiz) =======================\n\n")

# Adım 5.1: Kaynak dosyayı hazırla
source_df_benzersiz <- source_df_raw %>% clean_names() %>% group_by(kargo_no) %>% slice(1) %>% ungroup()
source_df_temiz <- anti_join(source_df_benzersiz, source_aykiri_degerler, by = "kargo_no")
source_kargos_temiz <- source_df_temiz %>% select(kargo_no)
db_kargos <- db_df %>% select(kargo_no)
kayip_kargolar <- anti_join(source_kargos_temiz, db_kargos, by = "kargo_no")
fazla_kargolar <- anti_join(db_kargos, source_kargos_temiz, by = "kargo_no")

# Adım 5.2: Raporu Yazdır

# <<< GÜNCELLENMİŞ RAPOR BÖLÜMÜ >>>
cat("--- 0. Veritabanı Durum Bilgisi ---\n")
if (!is.na(db_date_range$min_date) && !is.na(db_date_range$max_date)) {
  min_date_formatted <- format(as.Date(db_date_range$min_date), "%d %B %Y")
  max_date_formatted <- format(as.Date(db_date_range$max_date), "%d %B %Y")
  cat(sprintf("-> Platform veritabanı şu anda %s ile %s arasındaki gönderileri içeriyor.\n", min_date_formatted, max_date_formatted))
  
  if(nrow(latest_shipments_df) > 0) {
    cat("-> Veritabanına en son eklenen gönderiler (en güncel tarihli):\n")
    report_df <- latest_shipments_df %>% 
      select(`Kargo No` = kargo_no, `Firma` = kargo_turu, `Durum` = kargo_durumu, `Son Hareket` = son_hareket_tarihi)
    print(head(report_df, 20))
  }
  cat("\n")
  
} else {
  cat("-> Platform veritabanı boş veya tarih aralığı belirlenemedi.\n\n")
}
# <<< GÜNCELLENMİŞ RAPOR BÖLÜMÜ SONU >>>

# ... (Raporun geri kalanında değişiklik yok) ...
cat("--- 1. Genel Özet ---\n")
cat(sprintf("-> Ham Kaynak Dosyadaki Satır Sayısı        : %s\n", format(nrow(source_df_raw), big.mark = ",")))
cat(sprintf("-> Kaynaktaki Benzersiz Kargo Sayısı         : %s\n", format(nrow(source_df_benzersiz), big.mark = ",")))
cat(sprintf("-> AYKIRI DEĞER olarak tespit edilen         : %s\n", format(nrow(source_aykiri_degerler), big.mark = ",")))
cat("----------------------------------------------------------\n")
cat(sprintf("-> Karşılaştırmaya Esas Kargo Sayısı (Kaynak): %s\n", format(nrow(source_kargos_temiz), big.mark = ",")))
cat(sprintf("-> Veritabanındaki Kargo Sayısı (Platform)   : %s\n", format(nrow(db_kargos), big.mark = ",")))
cat(sprintf("-> Nihai EKSİK Kargo Sayısı                  : %s\n\n", format(nrow(kayip_kargolar), big.mark = ",")))

if (nrow(source_aykiri_degerler) > 0) {
  cat("--- 2. Aykırı Değer Olarak İşaretlenen Kargoların Detayları ---\n")
  cat("Aşağıdaki kargolar, teslimat süreleri istatistiksel olarak normalin dışında kaldığı için platform tarafından analizden çıkarılmıştır:\n")
  print(head(source_aykiri_degerler, 20))
  cat("\n")
}

if (nrow(kayip_kargolar) > 0) {
  cat("--- 3. Nihai EKSİK Kargoların Detayları ---\n")
  cat("Aykırı değerler çıkarıldıktan sonra dahi, aşağıdaki kargolar kaynakta olup veritabanında bulunamadı:\n")
  kayip_kargolarin_ham_verisi <- source_df_raw %>% clean_names() %>% filter(kargo_no %in% kayip_kargolar$kargo_no) %>% select(kargo_no, teslim_durumu, marka_adi, kargo_tarihi, teslim_tarihi, son_hareket_tarihi)
  print(head(kayip_kargolarin_ham_verisi, 20))
  cat("\n* Bu durum, genellikle `00_Data_Migration_Tool.R` script'indeki bir sorundan kaynaklanır (Örn: Hatalı sütun adı seçimi).\n\n")
} else if (nrow(source_aykiri_degerler) > 0) {
  cat("--- 3. Nihai EKSİK Kargoların Detayları ---\n")
  cat("Tebrikler! Aykırı değerler dışındaki tüm kargolar veritabanında mevcut. Veri farkı tamamen aykırı değerlerden kaynaklanıyor gibi görünüyor.\n\n")
}

cat("============================= RAPOR SONU =============================\n")

# --- 6. TEMİZLİK ---
try(poolClose(db_pool), silent = TRUE)
cat("Teşhis tamamlandı. Veritabanı bağlantısı kapatıldı.\n")
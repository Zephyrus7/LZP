# ========================================================================
#            CANLI VERİTABANI AYLIK HACİM DENETLEYİCİSİ (v2 - Düzeltilmiş)
# ========================================================================
# Amaç: 'orders' tablosundaki verilerin zaman içindeki dağılımını
#       analiz ederek, tarih aralığının neden yanıltıcı olduğunu
#       anlamak.
#
# v2 Değişikliği: dbplyr'ın SQL'e çeviremediği `floor_date()` fonksiyonu
#                 yerine, evrensel olarak desteklenen `year()` ve `month()`
#                 fonksiyonları kullanılarak yeniden yazıldı.
# ========================================================================

# --- 1. Gerekli Kütüphaneleri ve Bağlantıları Yükle ---
cat("Gerekli dosyalar ve kütüphaneler yükleniyor...\n")
library(dplyr)
library(dbplyr)
library(DBI)
library(pool)
library(stringr) # str_pad fonksiyonu için eklendi

# Veritabanı bağlantı havuzlarını oluşturan betikleri çağır
source("00_Config.R")
source("00_DB_Connector.R")

cat("Canlı veritabanına ('db_pool_live') bağlantı kuruldu.\n\n")


# --- 2. Veritabanından Aylık Özet Verisini Çek ---
cat("Aylık kargo sayıları hesaplanıyor...\n")

tryCatch({
  
  # DEĞİŞİKLİK BURADA: floor_date() yerine year() ve month() kullanıldı
  monthly_summary <- tbl(db_pool_live, "orders") %>%
    filter(is.na(deleted_at)) %>%
    # Önce yıl ve ay bilgilerini ayrı sütunlar olarak çıkar
    mutate(
      order_year = year(created_at),
      order_month = month(created_at)
    ) %>%
    # Sonra bu iki sütuna göre grupla
    group_by(order_year, order_month) %>%
    summarise(kargo_sayisi = n_distinct(id), .groups = "drop") %>%
    arrange(order_year, order_month) %>%
    collect()
  
  
  # --- 3. Sonuçları Konsola Yazdır ---
  cat("\n--- Canlı Veritabanı Aylık Kargo Sayısı Raporu ---\n\n")
  
  if (nrow(monthly_summary) > 0) {
    # DEĞİŞİKLİK BURADA: Sonuçları R içinde formatla
    results_to_print <- monthly_summary %>%
      mutate(
        # Yıl ve ay sütunlarını birleştirerek "YYYY-MM" formatı oluştur
        Donem = paste0(order_year, "-", str_pad(order_month, 2, pad = "0"))
      ) %>%
      select(Donem, `Kargo Sayısı` = kargo_sayisi)
    
    print(results_to_print, n = 200) 
    
    cat("\n-----------------------------------------------------\n")
    cat("RAPOR AÇIKLAMASI:\n")
    cat("Yukarıdaki tablo, 'orders' tablosundaki kargoların aylara göre gerçek dağılımını göstermektedir.\n")
    cat("Giriş ekranında gördüğünüz en eski tarih, muhtemelen o dönemde oluşturulmuş tek bir veya birkaç test kaydından kaynaklanmaktadır.\n")
    cat("Veri yoğunluğunun hangi aylarda toplandığını bu tablodan net bir şekilde görebilirsiniz.\n")
    
  } else {
    cat("UYARI: Canlı veritabanındaki 'orders' tablosunda hiç aktif sipariş bulunamadı!\n")
  }
  
}, error = function(e) {
  
  cat("\n!!!! HATA !!!!\n")
  cat("Veritabanı sorgusu sırasında bir hata oluştu: ", e$message, "\n")
  
}, finally = {
  
  # --- 4. Bağlantı Havuzlarını Güvenle Kapat ---
  cat("\nİşlem tamamlandı. Veritabanı bağlantıları kapatılıyor.\n")
  poolClose(db_pool_static)
  poolClose(db_pool_live)
  
})
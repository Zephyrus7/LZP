# =================================================================
#      CANLI ANALİZ - SORUN GİDERME SCRIPT'İ
# =================================================================
# AMAÇ: Bu script, herhangi bir Shiny arayüzü olmadan, Canlı Analiz
#       için kullanılan verileri, birleştirmeleri ve hesaplamaları
#       adım adım test eder. Sonuçları, analiz edilmesi için R
#       konsoluna yazdırır.
#
# KULLANIM:
# 1. Bu dosyayı projenizin ana dizinine kaydedin.
# 2. RStudio'da açın ve 'Source' butonuna tıklayarak çalıştırın.
# =================================================================

cat("Sorun Giderme Script'i Başlatılıyor...\n\n")

# --- 1. Gerekli Kütüphaneleri ve Bağlantıyı Yükle ---
suppressPackageStartupMessages({
  library(dplyr)
  library(dbplyr)
  library(DBI)
  library(pool)
})

# Veritabanı bağlantı havuzunu oluştur
source("00_DB_Connector.R", encoding = "UTF-8")


# --- 2. Analiz Edilecek Örnek Siparişleri Belirle ---
# Örnek olarak son 1000 siparişi ele alalım.
# Bu siparişler içinden hem tamamlanmış hem de iade edilmiş olanları bulmaya çalışacağız.
cat(">>> ADIM 1: Analiz için son 1000 siparişin ID'leri çekiliyor...\n")

sample_order_ids <- poolWithTransaction(db_pool_live, function(conn) {
  dbGetQuery(conn, "SELECT id FROM orders WHERE deleted_at IS NULL ORDER BY id DESC LIMIT 1000")
})$id

cat(length(sample_order_ids), "adet örnek sipariş ID'si bulundu.\n\n")


# --- 3. Kritik Tarihlerin Varlığını ve İçeriğini Test Et ---
cat(">>> ADIM 2: Her bir kritik süreç adımı için veriler test ediliyor...\n")

poolWithTransaction(db_pool_live, function(conn) {
  
  # A. Ana 'orders' tablosunu kontrol et
  cat("\n--- A. 'orders' Tablosu Kontrolü ---\n")
  orders_sample <- dbGetQuery(conn, "SELECT id, created_at, cargo_date, delivered_at FROM orders WHERE id IN (?)", params = list(sample_order_ids))
  
  veri_tarihi_var_mi <- "created_at" %in% names(orders_sample)
  kargo_tarihi_var_mi <- "cargo_date" %in% names(orders_sample)
  teslim_tarihi_var_mi <- "delivered_at" %in% names(orders_sample)
  
  cat(" 'created_at' (Veri Tarihi) sütunu var mı?:", veri_tarihi_var_mi, "\n")
  cat(" 'cargo_date' sütunu var mı?:", kargo_tarihi_var_mi, "\n")
  cat(" 'delivered_at' sütunu var mı?:", teslim_tarihi_var_mi, "\n")
  
  
  # B. 'order_logs' tablosunu kontrol et
  cat("\n--- B. 'order_logs' Tablosu Kontrolü ---\n")
  logs_sample <- dbGetQuery(conn, "SELECT order_id, proccess, created_at FROM order_logs WHERE order_id IN (?)", params = list(sample_order_ids))
  
  # Her bir kritik adım için kaç adet log bulunduğunu say
  log_counts <- logs_sample %>%
    group_by(proccess) %>%
    summarise(Adet = n(), .groups = "drop") %>%
    arrange(proccess)
  
  cat("Örnek siparişler için bulunan log sayıları:\n")
  print(log_counts)
  
  # İlgilendiğimiz ID'lerin varlığını teyit et
  REQUIRED_IDS <- c(
    `Kargo Kabul (Biz)` = 24, 
    `Order Check (Partner)` = 28, 
    `İade Başlangıç` = 39, 
    `İade Alındı (Partner)` = 47, 
    `İade Edildi (Bize)` = 25, 
    `Teslim Edildi` = 18
  )
  
  for (name in names(REQUIRED_IDS)) {
    id <- REQUIRED_IDS[[name]]
    found <- id %in% log_counts$proccess
    cat(" > '", name, "' (ID=", id, ") için log bulundu mu?: ", found, "\n", sep="")
  }
  
  
  # --- 4. Süre Hesaplamalarını Simüle Et ---
  cat("\n>>> ADIM 3: Süre hesaplamaları simüle ediliyor...\n")
  
  # Her bir sipariş için kritik tarihleri tek bir satırda topla
  event_dates <- logs_sample %>%
    group_by(order_id) %>%
    summarise(
      kargo_kabul_tarihi = min(created_at[proccess == 24], na.rm = TRUE),
      order_check_tarihi = min(created_at[proccess == 28], na.rm = TRUE),
      teslim_tarihi_logs = min(created_at[proccess == 18], na.rm = TRUE),
      iade_baslangic_tarihi = min(created_at[proccess == 39], na.rm = TRUE),
      iade_teslim_alindi_tarihi = min(created_at[proccess == 47], na.rm = TRUE),
      iade_teslim_edildi_tarihi = min(created_at[proccess == 25], na.rm = TRUE)
    ) %>%
    # Inf değerlerini NA yap (min fonksiyonu boş vektörde Inf döndürür)
    mutate(across(where(is.POSIXct), ~if_else(is.infinite(.), NA_POSIXct_, .)))
  
  # 'orders' ve 'order_logs' verisini birleştir
  full_data <- orders_sample %>%
    rename(order_id = id) %>%
    left_join(event_dates, by = "order_id")
  
  # Süreleri hesapla
  calculated_durations <- full_data %>%
    mutate(
      Alim_Suresi_Saat = as.numeric(difftime(kargo_kabul_tarihi, created_at, units = "hours")),
      Partnere_Verme_Suresi_Saat = as.numeric(difftime(order_check_tarihi, kargo_kabul_tarihi, units = "hours")),
      Partner_Teslim_Suresi_Saat = as.numeric(difftime(teslim_tarihi_logs, order_check_tarihi, units = "hours")),
      Musteri_Gonderme_Suresi_Saat = as.numeric(difftime(iade_teslim_alindi_tarihi, iade_baslangic_tarihi, units = "hours")),
      Partner_Iade_Suresi_Saat = as.numeric(difftime(iade_teslim_edildi_tarihi, iade_teslim_alindi_tarihi, units = "hours"))
    ) %>%
    select(order_id, starts_with("Alim"), starts_with("Partnere"), starts_with("Musteri"))
  
  cat("\n--- C. Örnek Süre Hesaplamaları (Saat) ---\n")
  cat("Aşağıdaki özet, hesaplanan sürelerin istatistiklerini gösterir.\n")
  cat("'NA's' sayısı, o sürenin kaç sipariş için HESAPLANAMADIĞINI gösterir.\n\n")
  
  # Her bir süre için özet istatistikleri yazdır
  print(summary(calculated_durations))
  
  cat("\n--- D. '0' veya Negatif Süreye Sahip Sipariş Örnekleri ---\n")
  zero_or_neg_alim <- calculated_durations %>% filter(Alim_Suresi_Saat <= 0)
  cat(" > Alım Süresi <= 0 olan sipariş adedi:", nrow(zero_or_neg_alim), "\n")
  if(nrow(zero_or_neg_alim) > 0) print(head(zero_or_neg_alim))
  
  cat("\n")
})


cat("\nSorun Giderme Script'i Tamamlandı.\n")
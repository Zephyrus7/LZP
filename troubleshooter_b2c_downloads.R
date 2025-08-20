# ========================================================================
#          B2C İNDİRME NİHAİ TEŞHİS BETİĞİ (v3)
# ========================================================================
# Amaç: Hangi spesifik veri grubunun (firma, marka vb.) dplyr
#       hesaplamasını çökerttiğini döngü kullanarak tespit etmek.
# ========================================================================

# --- 1. Proje Ortamını Kurulumu ---
cat(">>> ADIM 1: Proje ortamı kuruluyor...\n")
source("00_Config.R"); source("00_DB_Connector.R"); source("01_B2C_Processor.R"); source("server_b2c.R")
cat("✔ Proje dosyaları başarıyla yüklendi.\n\n")

# --- 2. Ana Veriyi İşleme ---
cat(">>> ADIM 2: Ana veri seti oluşturuluyor...\n")
date_range <- poolWithTransaction(db_pool_static, function(conn) { dbGetQuery(conn, "SELECT MIN(son_hareket_tarihi) AS min_date, MAX(son_hareket_tarihi) AS max_date FROM gonderiler") })
processed_data_list <- analiz_et_ve_skorla_b2c(db_pool_static, date_range$min_date, date_range$max_date, NULL)
cat("✔ Ana veri seti başarıyla oluşturuldu.\n\n")


# --- 3. KRİTİK TEST: Firma Karnesi Çökme Noktası Tespiti ---
cat(">>> ADIM 3: Firma Karnesi raporu için firmalar tek tek test ediliyor...\n")
cat("    (Eğer betik bu adımda bir firmadan sonra durursa, o firma sorunludur.)\n\n")

# Test edilecek tüm firmaların listesini al
all_firms <- unique(processed_data_list$sonuclar$kargo_turu)

# Her bir firma için hesaplamayı tek tek dene
for (current_firm in all_firms) {
  
  cat(paste0("--- Test ediliyor: ", current_firm, "...\n"))
  
  tryCatch({
    # Sadece o anki firma için hesaplama yap
    MOCK_INPUT_KARNE <- list(
      min_hacim_karne = 1, # Tüm veriyi görmek için 1 yapalım
      secilen_firma_karne = current_firm,
      karne_sehir_secimi = "all_cities",
      agirlik_performans = 40, agirlik_hiz = 40, agirlik_sikayet = 20
    )
    
    # Hesaplama mantığını (önceki betikteki test fonksiyonu) çalıştır
    # ÖNEMLİ: Bu fonksiyonun tanımı bir önceki cevabımda mevcut,
    # bu betiğin çalışması için o fonksiyonların da bu dosyada tanımlı olması gerekir.
    # Eğer sildiyseniz, bir önceki cevabımdaki test fonksiyonlarını bu satırdan önceye ekleyin.
    firma_karne_tablo_verisi_test(processed_data_list, MOCK_INPUT_KARNE)
    
    cat(paste0("    ✔ ", current_firm, " BAŞARILI.\n"))
    
  }, error = function(e) {
    cat(paste0("    ███ ", current_firm, " İÇİN HATA TESPİT EDİLDİ ███\n"))
    cat("        Hata Mesajı:", e$message, "\n")
  })
}

cat("\n>>> ADIM 4: Test tamamlandı. Bağlantılar kapatılıyor.\n")
poolClose(db_pool_static)
poolClose(db_pool_live)


# Gerekli Test Fonksiyonları (Bir önceki cevaptan)
# Lütfen bu fonksiyonların betiğinizde olduğundan emin olun.
firma_karne_tablo_verisi_test <- function(processed_data, input) {
  safe_rescale <- function(x, to = c(0, 1)) { unique_vals <- unique(na.omit(x)); if (length(unique_vals) < 2) { return(ifelse(is.na(x), NA_real_, 1)) } ; scales::rescale(x, to = to) }
  ham_veri_temiz <- processed_data$ham_veri_temiz; df <- ham_veri_temiz %>% filter(kargo_turu == input$secilen_firma_karne); if (input$karne_sehir_secimi != "all_cities") { df <- df %>% filter(sehir == input$karne_sehir_secimi) };
  if (nrow(df) == 0) return(tibble())
  firma_karne_filtrelenmis_veri <- df %>% group_by(ilce, sehir, kargo_turu) %>% summarise(toplam_gonderi_sayisi = n(), dinamik_basari_orani = mean(basari_flag, na.rm = TRUE), ortalama_teslim_suresi = mean(toplam_teslim_suresi_saat, na.rm = TRUE), sikayet_orani_yuzde = mean(sikayet_var_mi, na.rm = TRUE) * 100, toplam_sikayet_sayisi = sum(sikayet_var_mi, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(performans_puani = safe_rescale(dinamik_basari_orani), hiz_puani = 1 - safe_rescale(ortalama_teslim_suresi), musteri_deneyimi_puani = 1 - safe_rescale(sikayet_orani_yuzde)) %>% 
    mutate(across(ends_with("_puani"), ~if_else(is.na(.) | is.infinite(.) | is.nan(.), 0, .))) %>% 
    mutate(Ham_EPS = { toplam_agirlik <- input$agirlik_performans + input$agirlik_hiz + input$agirlik_sikayet; if (toplam_agirlik == 0) toplam_agirlik <- 1; agirlik_p <- input$agirlik_performans / toplam_agirlik; agirlik_h <- input$agirlik_hiz / toplam_agirlik; agirlik_s <- input$agirlik_sikayet / toplam_agirlik; (performans_puani * agirlik_p) + (hiz_puani * agirlik_h) + (musteri_deneyimi_puani * agirlik_s) })
  firma_karne_filtrelenmis_veri %>% filter(toplam_gonderi_sayisi >= input$min_hacim_karne) %>% mutate( ortalama_teslim_suresi = round(ortalama_teslim_suresi, 2), sikayet_orani_gosterim = paste0(toplam_sikayet_sayisi, " adet (%", round(sikayet_orani_yuzde, 1), ")") ) %>% arrange(desc(Ham_EPS))
}
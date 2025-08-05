#==============================================================================#
#            B2B ANALİZ MOTORU (Nihai ve Doğru Mantık - v3)                    #
#==============================================================================#

analiz_et_ve_skorla_b2b <- function(db_pool, start_date, end_date, progress_updater = NULL) {
  
  #--- Yardımcı Fonksiyonlar ---
  update_progress <- function(amount, detail) { if (!is.null(progress_updater)) { progress_updater(amount = amount, detail = detail) } }
  
  is_gunu_saati_hesapla_super_vectorized <- function(baslangic_vektoru, bitis_vektoru) {
    brut_saat <- as.numeric(difftime(bitis_vektoru, baslangic_vektoru, units = "hours"))
    start_days <- as.Date(baslangic_vektoru); end_days <- as.Date(bitis_vektoru)
    num_sundays <- (as.integer(end_days) - as.integer(start_days) + wday(start_days, week_start = 1)) %/% 7
    pazar_saati_dusulecek <- num_sundays * 24
    baslangic_pazar_mi <- wday(baslangic_vektoru, week_start = 1) == 7
    baslangic_duzeltme <- if_else(baslangic_pazar_mi, 24 - (hour(baslangic_vektoru) + minute(baslangic_vektoru)/60 + second(baslangic_vektoru)/3600), 0)
    bitis_pazar_mi <- wday(bitis_vektoru, week_start = 1) == 7
    bitis_duzeltme <- if_else(bitis_pazar_mi, hour(bitis_vektoru) + minute(bitis_vektoru)/60 + second(bitis_vektoru)/3600, 0)
    net_saat <- brut_saat - pazar_saati_dusulecek + baslangic_duzeltme + bitis_duzeltme
    net_saat[is.na(baslangic_vektoru) | is.na(bitis_vektoru) | bitis_vektoru < baslangic_vektoru] <- NA_real_
    return(pmax(0, net_saat, na.rm = TRUE))
  }
  
  #--- 1. VERİTABANINDAN SADECE B2B GÖNDERİLERİNİ ÇEK ---
  update_progress(amount = 0.4, detail = "Veritabanından B2B gönderileri çekiliyor...")
  
  query <- "
    SELECT g.* 
    FROM gonderiler g
    INNER JOIN b2b_kargolar b ON g.kargo_no = b.kargo_no
    WHERE g.kargo_tarihi BETWEEN ? AND ?
  "
  analiz_df <- dbGetQuery(db_pool, query, params = list(start_date, end_date))
  
  if (nrow(analiz_df) == 0) {
    showNotification("Seçilen tarih aralığında hiç B2B verisi bulunamadı.", type = "warning")
    return(NULL)
  }
  
  #--- 2. VERİ ZENGİNLEŞTİRME (DOĞRU SIRAYLA) ---
  update_progress(amount = 0.5, detail = "Performans metrikleri hesaplanıyor...")
  
  teslim_edilmis_durumlar <- c("ZT", "Geç")
  
  # ÖNEMLİ: Hesaplamalar artık tüm B2B veri setine uygulanıyor.
  # Filtreleme yapmıyoruz, sadece koşullu olarak yeni sütunlar ekliyoruz.
  feature_rich_df <- analiz_df %>% 
    mutate(
      # is_on_time: Sadece teslim edilenler için hesaplanır, diğerleri NA kalır.
      is_on_time = if_else(
        kargo_durumu %in% teslim_edilmis_durumlar & !is.na(teslim_tarihi) & !is.na(tahmini_teslimat_tarihi), 
        teslim_tarihi <= tahmini_teslimat_tarihi, 
        NA
      ),
      # teslim_suresi: Sadece teslim edilenler için hesaplanır, diğerleri NA kalır.
      toplam_teslim_suresi_saat = if_else(
        kargo_durumu %in% teslim_edilmis_durumlar,
        is_gunu_saati_hesapla_super_vectorized(kargo_tarihi, teslim_tarihi),
        NA_real_
      )
    ) %>% 
    select(
      kargo_no, 
      kargo_firmasi = kargo_turu, 
      gonderici, 
      durum = kargo_durumu, 
      il = sehir, 
      ilce, 
      is_on_time, 
      son_islem_tarihi = son_hareket_tarihi,
      toplam_teslim_suresi_saat
    )
  
  update_progress(amount = 0.1, detail = "Analiz tamamlanıyor...")
  
  #--- 3. SONUCU DÖNDÜR ---
  # Skorlama fonksiyonu (`server_b2b.R` içinde) artık sadece teslim edilmiş gönderileri
  # dikkate alarak (na.rm = TRUE sayesinde) doğru hesaplamayı yapacaktır.
  return(list(main_data = feature_rich_df))
}
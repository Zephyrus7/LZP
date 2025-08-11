#==============================================================================#
#            B2B ANALİZ MOTORU (Firma Bazında Dinamik Hafta Sonu Kuralı)         #
#==============================================================================#

analiz_et_ve_skorla_b2b <- function(db_pool, start_date, end_date, progress_updater = NULL) {
  
  #--- Yardımcı Fonksiyonlar ---
  update_progress <- function(amount, detail) { if (!is.null(progress_updater)) { progress_updater(amount = amount, detail = detail) } }
  
  # =========================================================================
  #         *** YÜKSEK PERFORMANSLI VE FİRMA BAZINDA DİNAMİK FONKSİYON ***
  # =========================================================================
  # === DEĞİŞİKLİK BURADA: Fonksiyon artık kargo firması bilgisini de alıyor ===
  is_gunu_saati_hesapla_super_vectorized <- function(baslangic_vektoru, bitis_vektoru, kargo_turu_vektoru) {
    
    # 1. Brüt süreyi saat olarak hesapla (değişiklik yok)
    brut_saat <- as.numeric(difftime(bitis_vektoru, baslangic_vektoru, units = "hours"))
    
    # 2. Aradaki hafta sonu günlerinin sayısını FİRMAYA ÖZEL olarak bul
    start_days <- as.Date(baslangic_vektoru)
    end_days <- as.Date(bitis_vektoru)
    
    # Herkes için ortak olan Pazar günlerini hesapla
    num_sundays <- (as.integer(end_days) - as.integer(start_days) + wday(start_days, week_start = 1)) %/% 7
    
    # UPS için Cumartesi günlerini de hesapla
    num_saturdays <- (as.integer(end_days) - as.integer(start_days) + wday(start_days, week_start = 7)) %/% 7
    
    # Çıkarılacak saat, firmanın UPS olup olmamasına göre değişir.
    pazar_saati_dusulecek <- if_else(
      kargo_turu_vektoru == "UPS",
      (num_sundays + num_saturdays) * 24, # UPS ise: Cumartesi + Pazar saatlerini düş
      num_sundays * 24                    # Değilse: Sadece Pazar saatlerini düş
    )
    
    # 3. Kenar Durumları Düzelt: Başlangıç gününün hafta sonu olup olmadığını FİRMAYA ÖZEL kontrol et
    baslangic_hafta_sonu_mu <- if_else(
      kargo_turu_vektoru == "UPS",
      wday(baslangic_vektoru, week_start = 1) %in% c(6, 7), # UPS ise: Cumartesi veya Pazar mı?
      wday(baslangic_vektoru, week_start = 1) == 7          # Değilse: Sadece Pazar mı?
    )
    baslangic_duzeltme <- if_else(baslangic_hafta_sonu_mu,
                                  24 - (hour(baslangic_vektoru) + minute(baslangic_vektoru)/60 + second(baslangic_vektoru)/3600),
                                  0)
    
    # 4. Kenar Durumları Düzelt: Bitiş gününün hafta sonu olup olmadığını FİRMAYA ÖZEL kontrol et
    bitis_hafta_sonu_mu <- if_else(
      kargo_turu_vektoru == "UPS",
      wday(bitis_vektoru, week_start = 1) %in% c(6, 7), # UPS ise: Cumartesi veya Pazar mı?
      wday(bitis_vektoru, week_start = 1) == 7          # Değilse: Sadece Pazar mı?
    )
    bitis_duzeltme <- if_else(bitis_hafta_sonu_mu,
                              hour(bitis_vektoru) + minute(bitis_vektoru)/60 + second(bitis_vektoru)/3600,
                              0)
    
    # 5. Net süreyi hesapla (değişiklik yok)
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
  
  feature_rich_df <- analiz_df %>% 
    mutate(
      is_on_time = if_else(
        kargo_durumu %in% teslim_edilmis_durumlar & !is.na(teslim_tarihi) & !is.na(tahmini_teslimat_tarihi), 
        teslim_tarihi <= tahmini_teslimat_tarihi, 
        NA
      ),
      # === DEĞİŞİKLİK BURADA: Fonksiyon çağrısına artık kargo_turu sütunu da gönderiliyor ===
      toplam_teslim_suresi_saat = if_else(
        kargo_durumu %in% teslim_edilmis_durumlar,
        is_gunu_saati_hesapla_super_vectorized(kargo_tarihi, teslim_tarihi, kargo_turu),
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
  return(list(main_data = feature_rich_df))
}
#==============================================================================#
#            B2C ANALİZ MOTORU (Firma Bazında Dinamik Hafta Sonu Kuralı)         #
#==============================================================================#

analiz_et_ve_skorla_b2c <- function(db_pool, start_date, end_date, progress_updater = NULL) {
  
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
    
    # Önce herkes için ortak olan Pazar günlerini hesapla
    num_sundays <- (as.integer(end_days) - as.integer(start_days) + wday(start_days, week_start = 1)) %/% 7
    
    # === DEĞİŞİKLİK BURADA: UPS için Cumartesi günlerini de hesapla ===
    # Cumartesi hesaplaması için haftayı Pazar'dan başlatmak (Pazar=1, ..., Cumartesi=7) en kolay yoldur.
    num_saturdays <- (as.integer(end_days) - as.integer(start_days) + wday(start_days, week_start = 7)) %/% 7
    
    # Çıkarılacak saat, firmanın UPS olup olmamasına göre değişir.
    pazar_saati_dusulecek <- if_else(
      kargo_turu_vektoru == "UPS",
      (num_sundays + num_saturdays) * 24, # UPS ise: Cumartesi + Pazar saatlerini düş
      num_sundays * 24                    # Değilse: Sadece Pazar saatlerini düş
    )
    
    # 3. Kenar Durumları Düzelt: Başlangıç gününün hafta sonu olup olmadığını FİRMAYA ÖZEL kontrol et
    # === DEĞİŞİKLİK BURADA: Hafta sonu tanımı artık dinamik ===
    baslangic_hafta_sonu_mu <- if_else(
      kargo_turu_vektoru == "UPS",
      wday(baslangic_vektoru, week_start = 1) %in% c(6, 7), # UPS ise: Cumartesi veya Pazar mı?
      wday(baslangic_vektoru, week_start = 1) == 7          # Değilse: Sadece Pazar mı?
    )
    baslangic_duzeltme <- if_else(baslangic_hafta_sonu_mu,
                                  24 - (hour(baslangic_vektoru) + minute(baslangic_vektoru)/60 + second(baslangic_vektoru)/3600),
                                  0)
    
    # 4. Kenar Durumları Düzelt: Bitiş gününün hafta sonu olup olmadığını FİRMAYA ÖZEL kontrol et
    # === DEĞİŞİKLİK BURADA: Hafta sonu tanımı artık dinamik ===
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
  
  #--- 1. VERİTABANINDAN VERİ ÇEKME ---
  update_progress(amount = 0.1, detail = "Veritabanından gönderi verileri çekiliyor...")
  query_gonderiler <- "SELECT * FROM gonderiler WHERE son_hareket_tarihi BETWEEN ? AND ?"
  ana_veri_raw <- dbGetQuery(db_pool, query_gonderiler, params = list(start_date, end_date))
  if (nrow(ana_veri_raw) == 0) { showNotification("Seçilen tarih aralığında hiç B2C verisi bulunamadı.", type = "warning"); return(NULL) }
  update_progress(amount = 0.1, detail = "Veritabanından gönderi verileri çekiliyor...")
  tahmini_teslimat_raw <- dbGetQuery(db_pool, "SELECT * FROM tahmini_teslimatlar")
  query_sikayetler <- "SELECT s.* FROM sikayetler s JOIN gonderiler g ON s.kargo_no = g.kargo_no WHERE g.son_hareket_tarihi BETWEEN ? AND ?"
  sikayet_datasi_raw <- dbGetQuery(db_pool, query_sikayetler, params = list(start_date, end_date))
  
  #--- 2. VERİ TEMİZLEME VE BİRLEŞTİRME ---
  update_progress(amount = 0.1, detail = "Veritabanından gönderi verileri çekiliyor...")
  sikayetli_kargolar <- sikayet_datasi_raw %>% distinct(kargo_no) %>% mutate(sikayet_var_mi = TRUE)
  ana_veri_with_complaints <- ana_veri_raw %>% left_join(sikayetli_kargolar, by = "kargo_no") %>% mutate(sikayet_var_mi = if_else(is.na(sikayet_var_mi), FALSE, TRUE))
  
  update_progress(amount = 0.15, detail = "Veriler işleniyor...")
  ana_veri_birlesik_firma <- ana_veri_with_complaints %>% mutate(kargo_turu = case_when(kargo_turu == "Trendyol" ~ "PTT", kargo_turu %in% c("Eve Teslim", "Hediye", "Standart") ~ "Bovo Kargo", TRUE ~ kargo_turu))
  
  update_progress(amount = 0.2, detail = "Veriler işleniyor...")
  ana_veri_birlesik_firma <- ana_veri_birlesik_firma %>%
    mutate(
      # === DEĞİŞİKLİK BURADA: Fonksiyon çağrısına artık kargo_turu sütunu da gönderiliyor ===
      toplam_teslim_suresi_saat = is_gunu_saati_hesapla_super_vectorized(kargo_tarihi, teslim_tarihi, kargo_turu)
    )
  
  update_progress(amount = 0.25, detail = "Veriler işleniyor...")
  tahmini_teslimat <- tahmini_teslimat_raw %>% mutate(sehir = str_to_lower(sehir, "tr"), yeni_max_teslimat_suresi = as.numeric(yeni_max_teslimat_suresi))
  
  ana_veri_zengin <- ana_veri_birlesik_firma %>% 
    mutate(sehir = str_to_lower(sehir, "tr")) %>% 
    left_join(tahmini_teslimat, by = "sehir") %>% 
    mutate(
      basari_flag = if_else(!is.na(toplam_teslim_suresi_saat) & !is.na(yeni_max_teslimat_suresi), if_else(toplam_teslim_suresi_saat <= yeni_max_teslimat_suresi, 1, 0), NA_integer_),
      son_islem_tarihi = as.Date(son_hareket_tarihi)
    )
  
  #--- 3. ANALİZLER ---
  update_progress(amount = 0.25, detail = "Analizler yapılıyor...")
  tolerans_orani <- 0.25
  aykiri_degerler_raporu <- ana_veri_zengin %>% filter(!is.na(toplam_teslim_suresi_saat)) %>% group_by(ilce, sehir, kargo_turu) %>% mutate(q1 = quantile(toplam_teslim_suresi_saat, 0.25, na.rm = TRUE), q3 = quantile(toplam_teslim_suresi_saat, 0.75, na.rm = TRUE), iqr = q3 - q1, lower_bound = q1 - 5.5 * iqr, upper_bound = q3 + 5.5 * iqr, is_too_low = toplam_teslim_suresi_saat < lower_bound, is_too_high = toplam_teslim_suresi_saat > upper_bound) %>% ungroup() %>% mutate(is_sla_breach = if_else(!is.na(yeni_max_teslimat_suresi), toplam_teslim_suresi_saat > yeni_max_teslimat_suresi, FALSE), toleransli_ust_sinir = yeni_max_teslimat_suresi * (1 + tolerans_orani)) %>% mutate(aykiri_deger_nedeni = case_when(toplam_teslim_suresi_saat == 0 ~ "Geçersiz Süre (0 Saat)", is_too_low ~ "Aşırı Düşük Teslimat Süresi", is_too_high & is_sla_breach & (toplam_teslim_suresi_saat > toleransli_ust_sinir) ~ "Aşırı Yüksek Teslimat Süresi", TRUE ~ NA_character_)) %>% filter(!is.na(aykiri_deger_nedeni)) %>% select(sehir, ilce, kargo_turu, teslim_suresi = toplam_teslim_suresi_saat, tahmini_sure = yeni_max_teslimat_suresi, cikarilma_nedeni = aykiri_deger_nedeni, kargo_no)
  
  update_progress(amount = 0.3, detail = "Analizler yapılıyor...")
  temel_metrikler_ham <- ana_veri_zengin %>% filter(!is.na(kargo_turu) & !is.na(ilce)) %>% group_by(ilce, sehir, kargo_turu) %>% summarise(sikayet_orani_yuzde = mean(sikayet_var_mi, na.rm = TRUE) * 100, toplam_gonderi_sayisi = n(), toplam_sikayet_sayisi = sum(sikayet_var_mi, na.rm = TRUE), ortalama_desi = mean(desi, na.rm = TRUE), .groups = 'drop')
  
  update_progress(amount = 0.45, detail = "Analizler yapılıyor...")
  ana_veri_zengin_temizlenmis <- ana_veri_zengin %>% anti_join(aykiri_degerler_raporu, by = "kargo_no")
  
  update_progress(amount = 0.65, detail = "Analizler yapılıyor...")
  performans_metrikleri <- ana_veri_zengin_temizlenmis %>% filter(!is.na(kargo_turu) & !is.na(ilce)) %>% group_by(ilce, sehir, kargo_turu) %>% summarise(dinamik_basari_orani = mean(basari_flag, na.rm = TRUE), ortalama_teslim_suresi = mean(toplam_teslim_suresi_saat, na.rm = TRUE), .groups = 'drop')
  
  update_progress(amount = 0.85, detail = "Analizler yapılıyor...")
  aylik_rapor_verisi <- ana_veri_zengin_temizlenmis %>% filter(!kargo_turu %in% c("Mağazaya Teslim", "Mağazalar Arası Transfer", "21")) %>% filter(!is.na(son_islem_tarihi)) %>% mutate(Donem = floor_date(son_islem_tarihi, "month")) %>% group_by(Donem, kargo_turu) %>% summarise(`Toplam Hacim` = n(), `Ort. Teslimat Süresi (Saat)` = mean(toplam_teslim_suresi_saat, na.rm = TRUE), `Toplam Şikayet Adedi` = sum(sikayet_var_mi, na.rm = TRUE), .groups = 'drop') %>% mutate(`Şikayet Oranı (%)` = if_else(`Toplam Hacim` > 0, (`Toplam Şikayet Adedi` / `Toplam Hacim`) * 100, 0), Donem = format(Donem, "%Y-%m")) %>% select(Donem, `Kargo Firması` = kargo_turu, everything()) %>% arrange(Donem, `Kargo Firması`)
  
  update_progress(amount = 0.925, detail = "Arayüz başlatılıyor...")
  temel_metrikler <- temel_metrikler_ham %>% left_join(performans_metrikleri, by = c("ilce", "sehir", "kargo_turu")) %>% mutate(across(where(is.numeric), ~if_else(is.na(.) | is.infinite(.), 0, .))) %>% group_by(sehir) %>% mutate(performans_puani = scales::rescale(dinamik_basari_orani, to = c(0, 1)), hiz_puani = 1 - scales::rescale(ortalama_teslim_suresi, to = c(0, 1)), musteri_deneyimi_puani = 1 - scales::rescale(sikayet_orani_yuzde, to = c(0, 1))) %>% ungroup() %>% mutate(across(ends_with("_puani"), ~if_else(is.na(.) | is.infinite(.), 0, .)))
  
  #--- 4. SONUÇLARI DÖNDÜR ---
  return(list(sonuclar = temel_metrikler, aykiri_degerler = aykiri_degerler_raporu, ham_veri_temiz = ana_veri_zengin_temizlenmis, aylik_ozet = aylik_rapor_verisi))
}
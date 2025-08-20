# =========================================================================
#            B2C SERVER - ÇEKİRDEK REAKTİF HESAPLAMALAR
# =========================================================================
# NOT: Bu dosya artık bir fonksiyon içermiyor. Buradaki kod,
# `server_b2c.R` tarafından `source` edildiğinde doğrudan onun içinde çalışır.
# =========================================================================

# --- YARDIMCI FONKSİYONLAR ---
safe_rescale <- function(x, to = c(0, 1)) {
  unique_vals <- unique(na.omit(x))
  if (length(unique_vals) < 2) { return(ifelse(is.na(x), NA_real_, 1)) }
  scales::rescale(x, to = to)
}

# --- İNDİRME BUTONU DURUM TAKİPÇİLERİ ---
can_download_ilce <- reactiveVal(FALSE)
can_download_karne <- reactiveVal(FALSE)
can_download_marka <- reactiveVal(FALSE)
can_download_sikayet <- reactiveVal(FALSE)

# --- ANA REAKTİF HESAPLAMALAR ---
dinamik_skorlar <- reactive({
  req(ana_veri_skorlari(), input$agirlik_performans, input$agirlik_hiz, input$agirlik_sikayet)
  toplam_agirlik <- input$agirlik_performans + input$agirlik_hiz + input$agirlik_sikayet
  if (toplam_agirlik == 0) toplam_agirlik <- 1
  agirlik_p <- input$agirlik_performans / toplam_agirlik
  agirlik_h <- input$agirlik_hiz / toplam_agirlik
  agirlik_s <- input$agirlik_sikayet / toplam_agirlik
  ana_veri_skorlari() %>%
    mutate(Ham_EPS = (performans_puani * agirlik_p) + (hiz_puani * agirlik_h) + (musteri_deneyimi_puani * agirlik_s))
})

ilce_karsilastirma_data <- reactive({
  req(input$sehir_secimi_tab1, input$ilce_secimi_tab1, dinamik_skorlar(), input$guvenilirlik_esigi, input$guven_esigi_v, input$taban_puan_c)
  base_data <- dinamik_skorlar() %>% filter(sehir == input$sehir_secimi_tab1)
  if (input$ilce_secimi_tab1 == "all_districts") {
    summary_data <- base_data %>%
      group_by(kargo_turu) %>%
      summarise(
        total_weight = sum(toplam_gonderi_sayisi, na.rm = TRUE),
        Ham_EPS_sum = sum(Ham_EPS * toplam_gonderi_sayisi, na.rm = TRUE),
        hiz_puani_sum = sum(hiz_puani * toplam_gonderi_sayisi, na.rm = TRUE),
        ortalama_desi_sum = sum(ortalama_desi * toplam_gonderi_sayisi, na.rm = TRUE),
        toplam_gonderi_sayisi = sum(toplam_gonderi_sayisi, na.rm = TRUE),
        toplam_sikayet_sayisi = sum(toplam_sikayet_sayisi, na.rm = TRUE)
      ) %>%
      mutate(
        Ham_EPS = if_else(total_weight > 0, Ham_EPS_sum / total_weight, 0),
        hiz_puani = if_else(total_weight > 0, hiz_puani_sum / total_weight, 0),
        ortalama_desi = if_else(total_weight > 0, ortalama_desi_sum / total_weight, 0)
      ) %>% ungroup()
  } else {
    summary_data <- base_data %>% filter(ilce == input$ilce_secimi_tab1)
  }
  safe_sum_prod <- sum(summary_data$Ham_EPS * summary_data$toplam_gonderi_sayisi, na.rm = TRUE)
  safe_sum_weight <- sum(summary_data$toplam_gonderi_sayisi, na.rm = TRUE)
  context_average_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
  m <- input$guvenilirlik_esigi
  v_esik <- input$guven_esigi_v
  c_taban <- input$taban_puan_c / 100
  final_data <- summary_data %>%
    mutate(
      guvenilmez_mi = toplam_gonderi_sayisi < v_esik,
      Hedef_Puan_C = if_else(guvenilmez_mi, c_taban, context_average_C),
      Bayes_EPS = ((toplam_gonderi_sayisi / (toplam_gonderi_sayisi + m)) * Ham_EPS) + ((m / (toplam_gonderi_sayisi + m)) * Hedef_Puan_C),
      hacim_yuzdesi = (toplam_gonderi_sayisi / sum(toplam_gonderi_sayisi)) * 100,
      sikayet_orani_yuzde = if_else(toplam_gonderi_sayisi > 0, (toplam_sikayet_sayisi / toplam_gonderi_sayisi) * 100, 0),
      Bayes_Aciklama = paste0("Ham Skor: %", round(Ham_EPS * 100, 1), "\n", "Gonderi Sayisi: ", toplam_gonderi_sayisi, "\n", "Guven Esigi (v_esik): ", v_esik, "\n\n", if_else(guvenilmez_mi, paste0("Gonderi sayisi guven esiginin altinda oldugu icin 'Guvenilmez' kabul edildi.\nSkoru, Taban Puan olan %", round(c_taban * 100), "'a dogru cekildi."), paste0("Gonderi sayisi guven esigini gectigi icin 'Guvenilir' kabul edildi.\nSkoru, bolge ortalamasi olan %", round(context_average_C * 100), "'a dogru cekildi.")))
    )
  return(final_data)
})

simulator_data <- reactive({
  req(dinamik_skorlar(), input$guvenilirlik_esigi, input$guven_esigi_v, input$taban_puan_c)
  firma_ozeti <- dinamik_skorlar() %>%
    group_by(kargo_turu) %>%
    summarise(
      total_weight = sum(toplam_gonderi_sayisi, na.rm = TRUE),
      Ham_EPS_sum = sum(Ham_EPS * toplam_gonderi_sayisi, na.rm = TRUE),
      Toplam_Gonderi = sum(toplam_gonderi_sayisi, na.rm = TRUE)
    ) %>%
    mutate(Ham_EPS_Ağırlıklı = if_else(total_weight > 0, Ham_EPS_sum / total_weight, 0)) %>% ungroup()
  safe_sum_prod <- sum(firma_ozeti$Ham_EPS_Ağırlıklı * firma_ozeti$Toplam_Gonderi, na.rm = TRUE)
  safe_sum_weight <- sum(firma_ozeti$Toplam_Gonderi, na.rm = TRUE)
  global_ortalama_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
  m <- input$guvenilirlik_esigi
  v_esik <- input$guven_esigi_v
  c_taban <- input$taban_puan_c / 100
  firma_ozeti %>%
    mutate(
      guvenilmez_mi = Toplam_Gonderi < v_esik,
      Hedef_Puan_C = if_else(guvenilmez_mi, c_taban, global_ortalama_C),
      Bayes_EPS = ((Toplam_Gonderi / (Toplam_Gonderi + m)) * Ham_EPS_Ağırlıklı) + ((m / (Toplam_Gonderi + m)) * Hedef_Puan_C),
      Bayes_Aciklama = paste0("Ham Skor: %", round(Ham_EPS_Ağırlıklı * 100, 1), "\n", "Toplam Gonderi: ", Toplam_Gonderi, "\n", "Guven Esigi (v_esik): ", v_esik, "\n\n", if_else(guvenilmez_mi, paste0("Gonderi sayisi guven esiginin altinda oldugu icin 'Guvenilmez' kabul edildi.\nSkoru, Taban Puan olan %", round(c_taban * 100), "'a dogru cekildi."), paste0("Gonderi sayisi guven esigini gectigi icin 'Guvenilir' kabul edildi.\nSkoru, genel ortalama olan %", round(global_ortalama_C * 100), "'a dogru cekildi.")))
    )
})

firma_karne_filtrelenmis_veri <- reactive({
  req(ham_veri_temiz(), input$secilen_firma_karne, input$karne_sehir_secimi)
  df <- ham_veri_temiz() %>% filter(kargo_turu == input$secilen_firma_karne)
  if (input$karne_sehir_secimi != "all_cities") { df <- df %>% filter(sehir == input$karne_sehir_secimi) }
  df %>%
    group_by(ilce, sehir, kargo_turu) %>%
    summarise(
      toplam_gonderi_sayisi = n(),
      dinamik_basari_orani = mean(basari_flag, na.rm = TRUE),
      ortalama_teslim_suresi = mean(toplam_teslim_suresi_saat, na.rm = TRUE),
      sikayet_orani_yuzde = mean(sikayet_var_mi, na.rm = TRUE) * 100,
      toplam_sikayet_sayisi = sum(sikayet_var_mi, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      performans_puani = safe_rescale(dinamik_basari_orani),
      hiz_puani = 1 - safe_rescale(ortalama_teslim_suresi),
      musteri_deneyimi_puani = 1 - safe_rescale(sikayet_orani_yuzde)
    ) %>%
    mutate(across(ends_with("_puani"), ~if_else(is.na(.) | is.infinite(.), 0, .))) %>%
    mutate(Ham_EPS = {
      toplam_agirlik <- input$agirlik_performans + input$agirlik_hiz + input$agirlik_sikayet
      if (toplam_agirlik == 0) toplam_agirlik <- 1
      agirlik_p <- input$agirlik_performans / toplam_agirlik
      agirlik_h <- input$agirlik_hiz / toplam_agirlik
      agirlik_s <- input$agirlik_sikayet / toplam_agirlik
      (performans_puani * agirlik_p) + (hiz_puani * agirlik_h) + (musteri_deneyimi_puani * agirlik_s)
    })
})

firma_karne_tablo_verisi <- reactive({
  req(firma_karne_filtrelenmis_veri(), input$min_hacim_karne)
  firma_karne_filtrelenmis_veri() %>%
    filter(toplam_gonderi_sayisi >= input$min_hacim_karne) %>%
    mutate(
      ortalama_teslim_suresi = round(ortalama_teslim_suresi, 2),
      sikayet_orani_gosterim = paste0(round(sikayet_orani_yuzde, 1), "% (", toplam_sikayet_sayisi, " adet)")
    ) %>%
    arrange(desc(Ham_EPS))
})

marka_analizi_data <- reactive({
  req(ham_veri_temiz(), input$secilen_marka_analizi, input$marka_analizi_secilen_il, input$agirlik_performans, input$agirlik_hiz, input$agirlik_sikayet, input$guvenilirlik_esigi_marka, input$guven_esigi_v_marka, input$taban_puan_c_marka)
  df_marka <- ham_veri_temiz() %>% filter(gonderici == input$secilen_marka_analizi)
  if (input$marka_analizi_secilen_il != "all_cities") {
    df_marka <- df_marka %>% filter(sehir == input$marka_analizi_secilen_il)
    if (!is.null(input$marka_analizi_secilen_ilce) && input$marka_analizi_secilen_ilce != "all_districts") {
      df_marka <- df_marka %>% filter(ilce == input$marka_analizi_secilen_ilce)
    }
  }
  if(nrow(df_marka) == 0) return(NULL)
  df_summary <- df_marka %>%
    group_by(kargo_turu) %>%
    summarise(
      toplam_gonderi_sayisi = n(),
      ortalama_teslim_suresi = mean(toplam_teslim_suresi_saat, na.rm = TRUE),
      dinamik_basari_orani = mean(basari_flag, na.rm = TRUE),
      toplam_sikayet_sayisi = sum(sikayet_var_mi, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(sikayet_orani_yuzde = if_else(toplam_gonderi_sayisi > 0, (toplam_sikayet_sayisi / toplam_gonderi_sayisi) * 100, 0))
  df_scored <- df_summary %>%
    mutate(
      performans_puani = safe_rescale(dinamik_basari_orani),
      hiz_puani = 1 - safe_rescale(ortalama_teslim_suresi),
      musteri_deneyimi_puani = 1 - safe_rescale(sikayet_orani_yuzde)
    ) %>%
    mutate(across(ends_with("_puani"), ~if_else(is.na(.) | is.infinite(.), 0, .)))
  toplam_agirlik <- input$agirlik_performans + input$agirlik_hiz + input$agirlik_sikayet
  if (toplam_agirlik == 0) toplam_agirlik <- 1
  agirlik_p <- input$agirlik_performans / toplam_agirlik
  agirlik_h <- input$agirlik_hiz / toplam_agirlik
  agirlik_s <- input$agirlik_sikayet / toplam_agirlik
  df_ham_skor <- df_scored %>% mutate(Ham_EPS = (performans_puani * agirlik_p) + (hiz_puani * agirlik_h) + (musteri_deneyimi_puani * agirlik_s))
  m <- input$guvenilirlik_esigi_marka
  v_esik <- input$guven_esigi_v_marka
  c_taban <- input$taban_puan_c_marka / 100
  safe_sum_prod <- sum(df_ham_skor$Ham_EPS * df_ham_skor$toplam_gonderi_sayisi, na.rm = TRUE)
  safe_sum_weight <- sum(df_ham_skor$toplam_gonderi_sayisi, na.rm = TRUE)
  context_average_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
  df_final <- df_ham_skor %>%
    mutate(
      guvenilmez_mi = toplam_gonderi_sayisi < v_esik,
      Hedef_Puan_C = if_else(guvenilmez_mi, c_taban, context_average_C),
      Bayes_EPS = ((toplam_gonderi_sayisi / (toplam_gonderi_sayisi + m)) * Ham_EPS) + ((m / (toplam_gonderi_sayisi + m)) * Hedef_Puan_C),
      Bayes_Aciklama = paste0("Ham Skor: %", round(Ham_EPS * 100, 1), "\n", "Toplam Gonderi: ", toplam_gonderi_sayisi, "\n", "Guven Esigi (v_esik): ", v_esik, "\n\n", if_else(guvenilmez_mi, paste0("Gonderi sayisi guven esiginin altinda oldugu icin 'Guvenilmez' kabul edildi.\nSkoru, Taban Puan olan %", round(c_taban * 100), "'a dogru cekildi."), paste0("Gonderi sayisi guven esigini gectigi icin 'Guvenilir' kabul edildi.\nSkoru, bu filtrelenmis bağlamın ortalaması olan %", round(context_average_C * 100), "'a dogru cekildi.")))
    ) %>%
    arrange(desc(Bayes_EPS))
  return(df_final)
})

sikayet_analizi_ozet_verisi <- reactive({
  req(ana_veri_skorlari(), input$sikayet_firma_secimi, input$sikayet_sehir_secimi)
  df <- ana_veri_skorlari()
  if (input$sikayet_firma_secimi != "all_companies") { df <- df %>% filter(kargo_turu == input$sikayet_firma_secimi) }
  if (input$sikayet_sehir_secimi != "all_cities") { df <- df %>% filter(sehir == input$sikayet_sehir_secimi) }
  grouping_key <- if (input$sikayet_firma_secimi == "all_companies") vars(kargo_turu) else vars(sehir, ilce)
  df %>%
    filter(toplam_sikayet_sayisi > 0) %>%
    group_by(!!!grouping_key) %>%
    summarise(
      toplam_sikayet_sayisi = sum(toplam_sikayet_sayisi, na.rm = TRUE),
      toplam_gonderi_sayisi = sum(toplam_gonderi_sayisi, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(sikayet_orani_yuzde = (toplam_sikayet_sayisi / toplam_gonderi_sayisi) * 100) %>%
    arrange(desc(toplam_sikayet_sayisi))
})

aykiri_grafik_verisi <- reactive({
  req(aykiri_veriler(), input$aykiri_analiz_modu)
  df <- aykiri_veriler()
  if (input$aykiri_analiz_modu == "genel") {
    df %>% count(cikarilma_nedeni, name = "sayi") %>% rename(kategori = cikarilma_nedeni)
  } else if (input$aykiri_analiz_modu == "firma") {
    req(input$aykiri_secilen_firma)
    df %>%
      filter(kargo_turu == input$aykiri_secilen_firma) %>%
      mutate(bolge = paste(sehir, ilce, sep=" - ")) %>%
      count(bolge, name = "sayi") %>%
      arrange(desc(sayi)) %>%
      head(10) %>%
      rename(kategori = bolge)
  } else if (input$aykiri_analiz_modu == "bolge") {
    req(input$aykiri_secilen_il)
    df_filtered <- df %>% filter(sehir == input$aykiri_secilen_il)
    if (!is.null(input$aykiri_secilen_ilce) && input$aykiri_secilen_ilce != "all") {
      df_filtered <- df_filtered %>% filter(ilce == input$aykiri_secilen_ilce)
    }
    df_filtered %>% count(kargo_turu, name = "sayi") %>% rename(kategori = kargo_turu)
  }
})

aykiri_firma_ozet_verisi <- reactive({
  req(aykiri_veriler())
  df_summary <- aykiri_veriler() %>%
    count(kargo_turu, cikarilma_nedeni) %>%
    pivot_wider(names_from = cikarilma_nedeni, values_from = n, values_fill = 0)
  olasi_nedenler <- c("Aşırı Yüksek Teslimat Süresi", "Aşırı Düşük Teslimat Süresi", "Geçersiz Süre (0 Saat)")
  for(neden in olasi_nedenler) {
    if (!neden %in% names(df_summary)) {
      df_summary[[neden]] <- 0
    }
  }
  df_summary %>%
    mutate(Toplam = `Aşırı Yüksek Teslimat Süresi` + `Aşırı Düşük Teslimat Süresi` + `Geçersiz Süre (0 Saat)`) %>%
    rename(`Kargo Firması` = kargo_turu, `Aşırı Yüksek Süre` = `Aşırı Yüksek Teslimat Süresi`, `Aşırı Düşük Süre` = `Aşırı Düşük Teslimat Süresi`, `Geçersiz Süre (0 Saat)` = `Geçersiz Süre (0 Saat)`) %>%
    select(`Kargo Firması`, `Aşırı Yüksek Süre`, `Aşırı Düşük Süre`, `Geçersiz Süre (0 Saat)`, `Toplam`) %>%
    arrange(desc(Toplam))
})

can_generate_brand_report <- reactive({
  req(ham_veri_temiz())
  any(!is.na(ham_veri_temiz()$gonderici))
})

# --- DURUM TAKİPÇİSİ OBSERVER'LARI ---
observe({ req(ilce_karsilastirma_data()); can_download_ilce(TRUE) })
observe({ req(firma_karne_tablo_verisi()); can_download_karne(TRUE) })
observe({ req(marka_analizi_data()); can_download_marka(TRUE) })
observe({ req(sikayet_analizi_ozet_verisi()); can_download_sikayet(TRUE) })
# =========================================================================
#                   B2C SERVER - ARAYÜZ ÇIKTILARI
# =========================================================================
# Görevi: Tüm renderUI, renderDataTable, renderPlot, renderText
#         fonksiyonlarını barındırarak arayüz elemanlarını oluşturmak.
# NOT: Bu dosyadaki kod, `server_b2c.R` içinde `source` edilir.
#      Buradaki reaktifler (`simulator_data()`, `ilce_karsilastirma_data()` vb.)
#      `server_b2c_reactives.R` dosyasında tanımlanmıştır.
# =========================================================================

# --- UI RENDER FONKSİYONLARI (Filtreler vb.) ---
output$sehir_secimi_ui <- renderUI({ req(ana_veri_skorlari()); sehir_listesi <- sort(unique(ana_veri_skorlari()$sehir)); selectInput(ns("sehir_secimi_tab1"), "1. Şehir Seçin:", choices = sehir_listesi) })
output$ilce_secimi_ui <- renderUI({ req(input$sehir_secimi_tab1); ilce_secenekleri <- ana_veri_skorlari() %>% filter(sehir == input$sehir_secimi_tab1) %>% pull(ilce) %>% unique() %>% sort(); selectInput(ns("ilce_secimi_tab1"), "2. İlçe Seçin:", choices = c("Tüm İlçeler" = "all_districts", ilce_secenekleri)) })
output$firma_secimi_ui <- renderUI({ req(ana_veri_skorlari()); firma_listesi <- sort(unique(ana_veri_skorlari()$kargo_turu)); selectInput(ns("secilen_firma_karne"), "1. Firma Seçin:", choices = firma_listesi) })
output$firma_karne_sehir_ui <- renderUI({ req(ana_veri_skorlari()); sehir_listesi <- sort(unique(ana_veri_skorlari()$sehir)); selectInput(ns("karne_sehir_secimi"), "2. İl Seçerek Filtrele:", choices = c("Tüm Türkiye" = "all_cities", sehir_listesi)) })
output$marka_analizi_marka_filter_ui <- renderUI({ req(ham_veri_temiz()); marka_listesi <- ham_veri_temiz() %>% filter(!is.na(gonderici)) %>% pull(gonderici) %>% unique() %>% sort(); selectInput(ns("secilen_marka_analizi"), "Marka Seçin:", choices = marka_listesi) })
output$marka_analizi_il_filter_ui <- renderUI({ req(input$secilen_marka_analizi); il_listesi <- ham_veri_temiz() %>% filter(gonderici == input$secilen_marka_analizi, !is.na(sehir)) %>% pull(sehir) %>% unique() %>% sort(); selectInput(ns("marka_analizi_secilen_il"), "İl Seçin (Opsiyonel):", choices = c("Tüm Türkiye" = "all_cities", il_listesi)) })
output$marka_analizi_ilce_filter_ui <- renderUI({ req(input$marka_analizi_secilen_il); if (input$marka_analizi_secilen_il == "all_cities") { return(NULL) }; ilce_listesi <- ham_veri_temiz() %>% filter(gonderici == input$secilen_marka_analizi, sehir == input$marka_analizi_secilen_il, !is.na(ilce)) %>% pull(ilce) %>% unique() %>% sort(); selectInput(ns("marka_analizi_secilen_ilce"), "İlçe Seçin (Opsiyonel):", choices = c("Tüm İlçeler" = "all_districts", ilce_listesi)) })
output$sikayet_analizi_firma_filter_ui <- renderUI({ req(ana_veri_skorlari()); firma_listesi <- sort(unique(ana_veri_skorlari()$kargo_turu)); selectInput(ns("sikayet_firma_secimi"), "1. Firma Seçin:", choices = c("Tüm Firmalar" = "all_companies", firma_listesi)) })
output$sikayet_analizi_sehir_filter_ui <- renderUI({ req(ana_veri_skorlari()); sehir_listesi <- sort(unique(ana_veri_skorlari()$sehir)); selectInput(ns("sikayet_sehir_secimi"), "2. İl Seçin:", choices = c("Tüm Türkiye" = "all_cities", sehir_listesi)) })
output$firma_secim_ui_aykiri <- renderUI({ req(aykiri_veriler()); firma_listesi <- sort(unique(aykiri_veriler()$kargo_turu)); selectInput(ns("aykiri_secilen_firma"), "Firma Seçin:", choices = firma_listesi) })
output$il_secim_ui_aykiri <- renderUI({ req(aykiri_veriler()); il_listesi <- sort(unique(aykiri_veriler()$sehir)); selectInput(ns("aykiri_secilen_il"), "İl Seçin:", choices = il_listesi) })
output$ilce_secim_ui_aykiri <- renderUI({ req(aykiri_veriler(), input$aykiri_secilen_il); ilce_listesi <- aykiri_veriler() %>% filter(sehir == input$aykiri_secilen_il) %>% pull(ilce) %>% unique() %>% sort(); selectInput(ns("aykiri_secilen_ilce"), "İlçe Seçin (Opsiyonel):", choices = c("Tüm İlçeler" = "all", ilce_listesi)) })

# --- SEKME ÇIKTILARI ---

# 1. Ağırlık Simülatörü Sekmesi
output$simulator_total_count_ui <- renderUI({ df <- simulator_data(); req(df); total_count <- sum(df$Toplam_Gonderi, na.rm = TRUE); tags$div(style = "text-align: right;", h5("Toplam Gönderi:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = ","), style = "margin: 0; font-weight: bold;")) })
output$agirlik_toplami <- renderText({ paste0(input$agirlik_performans + input$agirlik_hiz + input$agirlik_sikayet, " %") })
output$simulator_tablosu <- DT::renderDataTable({
  req(simulator_data())
  df_for_display <- simulator_data() %>%
    arrange(desc(Bayes_EPS)) %>%
    mutate(Bayes_EPS_display = paste0('<span title="', Bayes_Aciklama, '">', scales::percent(Bayes_EPS, accuracy = 0.01), '</span>')) %>%
    select( "Kargo Firması" = kargo_turu, "Ham Skor (EPS)" = Ham_EPS_Ağırlıklı, "Hacim Ayarlı Skor" = Bayes_EPS_display, "Toplam Gönderi Sayısı" = Toplam_Gonderi )
  DT::datatable(df_for_display, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, searching = FALSE)) %>%
    formatPercentage('Ham Skor (EPS)', digits = 2)
})

# 2. İlçe Karşılaştırma Sekmesi
output$ilce_karsilastirma_total_count_ui <- renderUI({ df <- ilce_karsilastirma_data(); req(df); total_count <- sum(df$toplam_gonderi_sayisi, na.rm = TRUE); tags$div(style = "text-align: right;", h5("Bölgedeki Toplam Gönderi:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = ","), style = "margin: 0; font-weight: bold;")) })
output$oneri_basligi <- renderText({ req(input$sehir_secimi_tab1, input$ilce_secimi_tab1); bolge <- if(input$ilce_secimi_tab1 == "all_districts") paste(toupper(input$sehir_secimi_tab1), "ŞEHRİ GENELİ") else paste(input$ilce_secimi_tab1, " (",toupper(input$sehir_secimi_tab1),") İlçesi"); paste(bolge, "İçin Anlık Öneriler") })
output$detay_tablo_basligi <- renderText({ req(input$sehir_secimi_tab1, input$ilce_secimi_tab1); bolge <- if(input$ilce_secimi_tab1 == "all_districts") paste(toupper(input$sehir_secimi_tab1), "ŞEHRİ GENELİ") else paste(input$ilce_secimi_tab1, " (",toupper(input$sehir_secimi_tab1),") İlçesi"); paste(bolge, "Anlık Karne") })
output$optimal_firma <- renderText({ df <- ilce_karsilastirma_data(); if (nrow(df) == 0) "Veri Yok" else df %>% arrange(desc(Bayes_EPS)) %>% slice(1) %>% pull(kargo_turu) })
output$hizli_firma <- renderText({ df <- ilce_karsilastirma_data(); if (nrow(df) == 0) "Veri Yok" else df %>% arrange(desc(hiz_puani)) %>% slice(1) %>% pull(kargo_turu) })
output$guvenilir_firma <- renderText({ df <- ilce_karsilastirma_data(); if (nrow(df) == 0) "Veri Yok" else df %>% arrange(sikayet_orani_yuzde) %>% slice(1) %>% pull(kargo_turu) })
output$detay_tablosu <- DT::renderDataTable({
  req(ilce_karsilastirma_data())
  df_for_display <- ilce_karsilastirma_data() %>%
    mutate(
      sikayet_orani_gosterim = paste0(toplam_sikayet_sayisi, " adet (%", round(sikayet_orani_yuzde, 1), ")"),
      ortalama_desi_gosterim = as.character(round(ortalama_desi, 2)),
      gonderi_sayisi_gosterim = paste0(toplam_gonderi_sayisi, " (%", round(hacim_yuzdesi, 0), ")"),
      Bayes_EPS_display = paste0('<span title="', Bayes_Aciklama, '">', scales::percent(Bayes_EPS, accuracy = 0.01), '</span>'),
      Ham_Skor_Gosterim = paste0(round(Ham_EPS * 100, 2), "%")
    ) %>%
    arrange(desc(Bayes_EPS))
  
  df_for_display %>%
    select(
      `Kargo Firması` = kargo_turu,
      `Hacim Ayarlı Skor` = Bayes_EPS_display,
      `Ham Skor` = Ham_Skor_Gosterim,
      `Şikayet Oranı` = sikayet_orani_gosterim,
      `Ortalama Desi` = ortalama_desi_gosterim,
      `Gönderi Sayısı (% Hacim)` = gonderi_sayisi_gosterim
    ) %>%
    DT::datatable(escape = FALSE, rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = FALSE, scrollX = TRUE))
})

# 3. Firma Karnesi Sekmesi
output$firma_karne_total_count_ui <- renderUI({ df <- firma_karne_filtrelenmis_veri(); req(df); total_count <- sum(df$toplam_gonderi_sayisi, na.rm = TRUE); if (total_count == 0) return(NULL); tags$div(style = "text-align: right;", h5("Filtrelenen Gönderi:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = ","), style = "margin: 0; font-weight: bold;")) })
output$firma_karne_basligi <- renderText({ req(input$secilen_firma_karne); paste(input$secilen_firma_karne, "Firmasının Anlık Bölgesel Performans Karnesi") })
output$firma_karne_grafigi <- renderPlot({
  req(firma_karne_filtrelenmis_veri(), input$karne_siralama_tipi, input$min_hacim_karne)
  df_processed <- firma_karne_filtrelenmis_veri() %>%
    filter(toplam_gonderi_sayisi >= input$min_hacim_karne) %>%
    mutate(il_ilce = str_to_upper(paste0(sehir, " - ", ilce), "tr"))
  n_rows_to_show <- min(15, nrow(df_processed))
  if(n_rows_to_show == 0) { return(ggplot() + theme_void()) }
  if (isTRUE(theme_reactive() == "dark")) { plot_theme <- theme_minimal(base_size = 14) + theme(panel.background = element_rect(fill = "#343A40", color = NA), plot.background = element_rect(fill = "#343A40", color = NA), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#495057"), panel.grid.minor = element_blank(), text = element_text(color = "#E9ECEF"), axis.text = element_text(color = "#E9ECEF"), title = element_text(color = "#FFFFFF")) } else { plot_theme <- theme_minimal(base_size = 14) }
  if(input$karne_siralama_tipi == "iyi") {
    df_plot <- df_processed %>% arrange(desc(Ham_EPS)) %>% head(n_rows_to_show)
    plot_aes <- aes(x = reorder(il_ilce, Ham_EPS), y = Ham_EPS, fill = Ham_EPS)
    plot_title <- paste("Anlık Ağırlıklara Göre En İyi Performanslar (Min.", input$min_hacim_karne, "Gönderi)")
    plot_fill_scale <- scale_fill_gradient(low = "#a7d8a5", high = "#2a6f28")
  } else {
    df_plot <- df_processed %>% arrange(Ham_EPS) %>% head(n_rows_to_show)
    plot_aes <- aes(x = reorder(il_ilce, -Ham_EPS), y = Ham_EPS, fill = Ham_EPS)
    plot_title <- paste("Anlık Ağırlıklara Göre En Kötü Performanslar (Min.", input$min_hacim_karne, "Gönderi)")
    plot_fill_scale <- scale_fill_gradient(low = "#f8d7da", high = "#d9534f")
  }
  ggplot(df_plot, plot_aes) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = plot_title, x = "Bölge (İl - İlçe)", y = "Genel Skor (EPS)") +
    plot_fill_scale +
    plot_theme +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))
}, bg = "transparent")
output$firma_karne_tablosu <- DT::renderDataTable({
  req(firma_karne_tablo_verisi())
  df <- firma_karne_tablo_verisi()
  df %>%
    mutate(
      Genel_Skor_Gosterim = paste0(round(Ham_EPS * 100, 1), "%"),
      Basari_Orani_Gosterim = paste0(round(dinamik_basari_orani * 100, 1), "%"),
      Ort_Teslim_Suresi_Gosterim = as.character(round(ortalama_teslim_suresi, 2)),
      Gonderi_Sayisi_Gosterim = as.character(toplam_gonderi_sayisi)
    ) %>%
    select(
      `İlçe` = ilce, `Şehir` = sehir, `Genel Skor (EPS)` = Genel_Skor_Gosterim, `Başarı Oranı` = Basari_Orani_Gosterim,
      `Şikayet Oranı` = sikayet_orani_gosterim, `Ort. Teslim Süresi (Saat)` = Ort_Teslim_Suresi_Gosterim, `Gönderi Sayısı` = Gonderi_Sayisi_Gosterim
    ) %>%
    DT::datatable(rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE))
})

# 4. Marka Analizi Sekmesi
output$marka_analizi_total_count_ui <- renderUI({ df <- marka_analizi_data(); req(df); total_count <- sum(df$toplam_gonderi_sayisi, na.rm = TRUE); if (total_count == 0) return(NULL); tags$div(style = "text-align: right;", h5("Markanın Toplam Gönderi:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = ","), style = "margin: 0; font-weight: bold;")) })
output$marka_analizi_baslik <- renderText({ req(input$secilen_marka_analizi); paste(input$secilen_marka_analizi, "Markasının Kargo Firması Performans Raporu") })
output$marka_analizi_tablosu <- DT::renderDataTable({
  df <- marka_analizi_data(); req(df)
  df_for_display <- df %>%
    mutate(
      Bayes_EPS_display = paste0('<span title="', Bayes_Aciklama, '">', scales::percent(Bayes_EPS, accuracy = 0.01), '</span>'),
      sikayet_orani_gosterim = paste0(toplam_sikayet_sayisi, " adet (%", round(sikayet_orani_yuzde, 1), ")"),
      Ham_Skor_Gosterim = paste0(round(Ham_EPS * 100, 2), "%"),
      Basari_Orani_Gosterim = paste0(round(dinamik_basari_orani * 100, 2), "%"),
      Ort_Teslim_Suresi_Gosterim = as.character(round(ortalama_teslim_suresi, 2)),
      Toplam_Gonderi_Gosterim = as.character(toplam_gonderi_sayisi)
    ) %>%
    select(
      `Kargo Firması` = kargo_turu, `Hacim Ayarlı Skor` = Bayes_EPS_display, `Ham Skor` = Ham_Skor_Gosterim,
      `Ort. Teslim Süresi (Saat)` = Ort_Teslim_Suresi_Gosterim, `Başarı Oranı` = Basari_Orani_Gosterim,
      `Şikayet Oranı` = sikayet_orani_gosterim, `Toplam Gönderi` = Toplam_Gonderi_Gosterim
    )
  DT::datatable(df_for_display, escape = FALSE, rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE))
})

# 5. Şikayet Analizi Sekmesi
output$sikayet_analizi_total_count_ui <- renderUI({ df <- sikayet_analizi_ozet_verisi(); req(df); total_count <- sum(df$toplam_gonderi_sayisi, na.rm = TRUE); if (total_count == 0) return(NULL); tags$div(style = "text-align: right;", h5("Filtrelenen Gönderi:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = ","), style = "margin: 0; font-weight: bold;")) })
output$show_sikayet_panel <- reactive({ req(sikayet_analizi_ozet_verisi()); return(nrow(sikayet_analizi_ozet_verisi()) > 0) }); outputOptions(output, 'show_sikayet_panel', suspendWhenHidden = FALSE)
output$sikayet_analizi_baslik <- renderText({ req(input$sikayet_firma_secimi, input$sikayet_sehir_secimi); firma_adi <- if(input$sikayet_firma_secimi == "all_companies") "Tüm Firmalar" else input$sikayet_firma_secimi; bolge_adi <- if(input$sikayet_sehir_secimi == "all_cities") "Tüm Türkiye" else str_to_upper(input$sikayet_sehir_secimi, "tr"); paste(firma_adi, "için", bolge_adi, "Bölgesindeki Şikayet Dağılımı") })
output$sikayet_analizi_grafigi <- renderPlot({
  df_summary <- sikayet_analizi_ozet_verisi(); req(nrow(df_summary) > 0)
  df_plot <- df_summary %>% head(15)
  if (isTRUE(theme_reactive() == "dark")) { plot_theme <- theme_minimal(base_size = 14) + theme(panel.background = element_rect(fill = "#343A40", color = NA), plot.background = element_rect(fill = "#343A40", color = NA), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(linetype = "dashed", color = "#495057"), text = element_text(color = "#E9ECEF"), axis.text = element_text(color = "#E9ECEF"), title = element_text(color = "#FFFFFF")) } else { plot_theme <- theme_minimal(base_size = 14) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(linetype = "dashed", color = "gray")) }
  if(input$sikayet_firma_secimi == "all_companies") {
    plot_aes <- aes(x = reorder(kargo_turu, toplam_sikayet_sayisi), y = toplam_sikayet_sayisi)
    x_label <- "Kargo Firması"; plot_title <- "Firmalara Göre En Çok Şikayet Alınanlar"
  } else {
    df_plot <- df_plot %>% mutate(il_ilce = str_to_upper(paste0(sehir, " - ", ilce), "tr"))
    plot_aes <- aes(x = reorder(il_ilce, toplam_sikayet_sayisi), y = toplam_sikayet_sayisi)
    x_label <- "Bölge (İl - İlçe)"; plot_title <- "Bölgelere Göre En Çok Şikayet Alınanlar"
  }
  ggplot(df_plot, plot_aes) +
    geom_col(fill = "#d9534f") +
    geom_text(aes(label = toplam_sikayet_sayisi), hjust = -0.2, size = 4, color = if(isTRUE(theme_reactive() == "dark")) "white" else "black") +
    coord_flip() + labs(title = plot_title, x = x_label, y = "Toplam Şikayet Sayısı") + plot_theme
}, bg = "transparent")
output$sikayet_analizi_tablosu <- DT::renderDataTable({
  df_summary <- sikayet_analizi_ozet_verisi(); req(nrow(df_summary) > 0)
  if(input$sikayet_firma_secimi == "all_companies") {
    df_table <- df_summary %>% select(`Kargo Firması` = kargo_turu, `Toplam Şikayet Sayısı` = toplam_sikayet_sayisi, `Toplam Gönderi Sayısı` = toplam_gonderi_sayisi, `Şikayet Oranı (%)` = sikayet_orani_yuzde)
  } else {
    df_table <- df_summary %>% select(`Şehir` = sehir, `İlçe` = ilce, `Toplam Şikayet Sayısı` = toplam_sikayet_sayisi, `Toplam Gönderi Sayısı` = toplam_gonderi_sayisi, `Şikayet Oranı (%)` = sikayet_orani_yuzde)
  }
  DT::datatable(df_table, rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE)) %>%
    formatRound('Şikayet Oranı (%)', digits = 2)
})

# 6. Dinamik Karşılaştırma Sekmesi
output$karsilastirma_tablosu <- DT::renderDataTable({
  req(karsilastirma_verisi()); req(!is.null(input$secilen_metrikler) && length(input$secilen_metrikler) > 0)
  ham_veri <- karsilastirma_verisi()$data
  ana_etiket <- karsilastirma_verisi()$ana_etiket
  karsilastirma_etiket <- karsilastirma_verisi()$karsilastirma_etiket
  final_table <- ham_veri %>% select(`Kargo Firması`)
  metric_names_map <- c("toplam_gonderi_sayisi" = "Hacim", "ortalama_teslim_suresi" = "Ort. Hız", "dinamik_basari_orani" = "Başarı %", "sikayet_orani_yuzde" = "Şikayet %")
  for (metric_value in input$secilen_metrikler) {
    metric_name <- metric_names_map[[metric_value]]
    col_ana_raw <- paste0(metric_value, "_ana")
    col_karsilastirma_raw <- paste0(metric_value, "_karsilastirma")
    col_ana_yeni_ad <- paste0(metric_name, " (", ana_etiket, ")")
    col_karsilastirma_yeni_ad <- paste0(metric_name, " (", karsilastirma_etiket, ")")
    col_degisim_yeni_ad <- paste(metric_name, "Değişim (%)")
    temp_df <- ham_veri %>%
      select(`Kargo Firması`, any_of(c(col_ana_raw, col_karsilastirma_raw))) %>%
      mutate(
        !!col_ana_raw := replace_na(.data[[col_ana_raw]], 0),
        !!col_karsilastirma_raw := replace_na(.data[[col_karsilastirma_raw]], 0),
        `Değişim` = if_else(.data[[col_ana_raw]] == 0, NA_real_, (.data[[col_karsilastirma_raw]] - .data[[col_ana_raw]]) / .data[[col_ana_raw]])
      ) %>%
      rename(
        !!col_ana_yeni_ad := all_of(col_ana_raw),
        !!col_karsilastirma_yeni_ad := all_of(col_karsilastirma_raw),
        !!col_degisim_yeni_ad := `Değişim`
      )
    final_table <- left_join(final_table, temp_df, by = "Kargo Firması")
  }
  dt <- DT::datatable(final_table, rownames = FALSE, options = list(scrollX = TRUE, paging = FALSE, searching = TRUE, info = FALSE))
  yuzde_cols_selector <- str_ends(names(final_table), fixed(" (%)"))
  if (any(yuzde_cols_selector)) { dt <- dt %>% formatPercentage(which(yuzde_cols_selector), digits = 2) }
  basari_cols_selector <- str_detect(names(final_table), "Başarı %")
  if (any(basari_cols_selector)) { dt <- dt %>% formatPercentage(which(basari_cols_selector), digits = 2) }
  ondalikli_cols_selector <- str_detect(names(final_table), "Ort. Hız") & !str_detect(names(final_table), "Değişim")
  if (any(ondalikli_cols_selector)) { dt <- dt %>% formatRound(which(ondalikli_cols_selector), digits = 2) }
  return(dt)
}, server = FALSE)

# 7. Aykırı Değer Raporu Sekmesi
output$aykiri_pie_chart <- renderPlot({
  req(input$aykiri_analiz_modu != "firma_ozet")
  plot_data <- aykiri_grafik_verisi(); req(plot_data, nrow(plot_data) > 0)
  plot_data <- plot_data %>% mutate(yuzde = sayi / sum(sayi))
  grafik_basligi <- case_when(
    input$aykiri_analiz_modu == "genel" ~ "Tüm Firmalar İçin Aykırılık Nedenleri Dağılımı",
    input$aykiri_analiz_modu == "firma" ~ paste(input$aykiri_secilen_firma, "için En Çok Aykırılık Gözlemlenen Bölgeler"),
    input$aykiri_analiz_modu == "bolge" ~ { bolge_adi <- if(!is.null(input$aykiri_secilen_ilce) && input$aykiri_secilen_ilce != "all") { paste(input$aykiri_secilen_il, "-", input$aykiri_secilen_ilce) } else { input$aykiri_secilen_il }; paste(bolge_adi, "Bölgesindeki Aykırı Değerlerin Firmalara Göre Dağılımı") }
  )
  if (isTRUE(theme_reactive() == "dark")) { plot_theme <- theme_void(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "white"), legend.title = element_text(face = "bold", color="white"), legend.text = element_text(color="white")); bar_border_color <- "#212529" } else { plot_theme <- theme_void(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.title = element_text(face = "bold")); bar_border_color <- "white" }
  ggplot(plot_data, aes(x = "", y = yuzde, fill = fct_reorder(kategori, yuzde))) +
    geom_bar(stat = "identity", width = 1, color = bar_border_color) +
    geom_text(aes(label = paste0(round(yuzde * 100), "%\n(", sayi, " adet)")), position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
    labs(title = grafik_basligi, fill = "Kategori") +
    plot_theme
}, bg = "transparent")
output$firma_ozet_basligi <- renderText({ req(aykiri_firma_ozet_verisi()); toplam_aykiri <- sum(aykiri_firma_ozet_verisi()$Toplam); paste0("Firma Bazında Aykırı Değer Özeti (Toplam: ", format(toplam_aykiri, big.mark=","), " Adet)") })
output$aykiri_firma_ozet_tablosu <- DT::renderDataTable({ DT::datatable(aykiri_firma_ozet_verisi(), rownames = FALSE, options = list(searching = FALSE, paging = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all')))) })
output$aykiri_degerler_tablosu <- DT::renderDataTable({ df <- aykiri_veriler(); req(df); DT::datatable(df, colnames = c("Şehir", "İlçe", "Kargo Firması", "Teslim Süresi (Saat)", "Tahmini Süre (Saat)", "Çıkarılma Nedeni", "Kargo No"), options = list(pageLength = 15, scrollX = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE)), filter = 'top') })
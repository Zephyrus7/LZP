# =========================================================================
#                   B2C SERVER - VERİ İNDİRME YÖNETİMİ
# =========================================================================
# Görevi: "Veri İndir" sekmesinin dinamik UI'ını oluşturmak ve
#         tüm downloadHandler fonksiyonlarını yönetmek.
# =========================================================================

register_b2c_downloads <- function(output, input, session, karsilastirma_verisi) {
  
  # Gerekli namespace fonksiyonunu session'dan alıyoruz.
  ns <- session$ns
  
  # --- YARDIMCI RAPOR FONKSİYONLARI ---
  generate_speed_comparison_report <- function(data) {
    b2b_turleri_to_exclude <- c("Mağazaya Teslim", "Mağazalar Arası Transfer", "B2B")
    base_data <- data %>%
      filter(!kargo_turu %in% b2b_turleri_to_exclude) %>%
      mutate(bolge = paste(sehir, ilce, sep=" - ")) %>%
      select(bolge, kargo_turu, ortalama_teslim_suresi, toplam_gonderi_sayisi)
    
    best_worst_performers <- base_data %>%
      group_by(bolge) %>%
      summarise(
        en_iyi_firma = kargo_turu[which.min(ortalama_teslim_suresi)],
        en_kotu_firma = kargo_turu[which.max(ortalama_teslim_suresi)],
        .groups = 'drop'
      )
    
    regional_summary <- base_data %>%
      group_by(bolge) %>%
      summarise(
        toplam_gonderi = sum(toplam_gonderi_sayisi, na.rm = TRUE),
        genel_ortalama_hiz = weighted.mean(ortalama_teslim_suresi, toplam_gonderi_sayisi, na.rm = TRUE),
        .groups = 'drop'
      )
    
    pivoted_data <- base_data %>%
      pivot_wider(
        id_cols = bolge,
        names_from = kargo_turu,
        values_from = c(toplam_gonderi_sayisi, ortalama_teslim_suresi),
        names_sep = "_",
        values_fill = list(toplam_gonderi_sayisi = 0, ortalama_teslim_suresi = NA)
      )
    
    final_report <- pivoted_data %>%
      left_join(regional_summary, by = "bolge") %>%
      left_join(best_worst_performers, by = "bolge")
    
    firmalar <- unique(base_data$kargo_turu)
    firma_sutunlari_sirali <- unlist(lapply(firmalar, function(f) c(paste0("toplam_gonderi_sayisi_", f), paste0("ortalama_teslim_suresi_", f))))
    
    final_report <- final_report %>%
      select(
        `Satır Etiketleri` = bolge,
        any_of(firma_sutunlari_sirali),
        `Say Kargo No Toplamı` = toplam_gonderi,
        `Ortalama Toplam Teslim Süresi (Saat) Toplamı` = genel_ortalama_hiz,
        `EN İYİ TESLİMAT SÜRESİNE SAHİP FİRMA` = en_iyi_firma,
        `EN KÖTÜ TESLİMAT SÜRESİNE SAHİP FİRMA` = en_kotu_firma
      )
    
    return(final_report)
  }
  
  generate_all_brands_on_demand <- function(veri_cercevesi) {
    req(veri_cercevesi, input$agirlik_performans, input$agirlik_hiz, input$agirlik_sikayet, input$guvenilirlik_esigi, input$guven_esigi_v, input$taban_puan_c)
    toplam_agirlik <- input$agirlik_performans + input$agirlik_hiz + input$agirlik_sikayet
    if (toplam_agirlik == 0) toplam_agirlik <- 1
    agirlik_p_val <- input$agirlik_performans / toplam_agirlik
    agirlik_h_val <- input$agirlik_hiz / toplam_agirlik
    agirlik_s_val <- input$agirlik_sikayet / toplam_agirlik
    m_param_val <- input$guvenilirlik_esigi
    v_esik_param_val <- input$guven_esigi_v
    c_taban_param_val <- input$taban_puan_c / 100
    
    df_summary <- veri_cercevesi %>%
      filter(!is.na(gonderici) & !is.na(sehir) & !is.na(ilce)) %>%
      group_by(gonderici, kargo_turu, sehir, ilce) %>%
      summarise(
        toplam_gonderi_sayisi = n(),
        ortalama_teslim_suresi = mean(toplam_teslim_suresi_saat, na.rm = TRUE),
        dinamik_basari_orani = mean(basari_flag, na.rm = TRUE),
        toplam_sikayet_sayisi = sum(sikayet_var_mi, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(sikayet_orani_yuzde = if_else(toplam_gonderi_sayisi > 0, (toplam_sikayet_sayisi / toplam_gonderi_sayisi) * 100, 0))
    
    if (nrow(df_summary) == 0) return(NULL)
    
    df_final <- df_summary %>%
      group_by(gonderici) %>%
      mutate(
        performans_puani = safe_rescale(dinamik_basari_orani),
        hiz_puani = 1 - safe_rescale(ortalama_teslim_suresi),
        musteri_deneyimi_puani = 1 - safe_rescale(sikayet_orani_yuzde),
        across(ends_with("_puani"), ~if_else(is.na(.) | is.infinite(.), 0, .)),
        Ham_EPS = (performans_puani * agirlik_p_val) + (hiz_puani * agirlik_h_val) + (musteri_deneyimi_puani * agirlik_s_val)
      ) %>%
      mutate(
        context_average_C = weighted.mean(Ham_EPS, toplam_gonderi_sayisi, na.rm = TRUE),
        guvenilmez_mi = toplam_gonderi_sayisi < v_esik_param_val,
        Hedef_Puan_C = if_else(guvenilmez_mi, c_taban_param_val, context_average_C),
        Bayes_EPS = ((toplam_gonderi_sayisi / (toplam_gonderi_sayisi + m_param_val)) * Ham_EPS) + ((m_param_val / (toplam_gonderi_sayisi + m_param_val)) * Hedef_Puan_C)
      ) %>%
      ungroup() %>%
      arrange(gonderici, sehir, ilce, desc(Bayes_EPS))
    
    return(df_final)
  }
  
  # --- DİNAMİK İNDİRME PANELİ (UI) ---
  download_ui <- reactive({
    tagList(
      div(style = "margin-bottom: 20px;",
          p(strong("Temel Analiz Raporları")),
          p(tags$small("Analizin ana çıktılarını içeren standart raporlar.")),
          checkboxGroupInput(ns("download_choices_xlsx"), label=NULL, choices = c("Temel Analiz ve Skorlar" = "b2c_sonuclar", "Aykırı Değer Raporu" = "b2c_aykiri", "Bölgesel Hız Karşılaştırma" = "b2c_hiz_raporu"), selected = c("b2c_sonuclar", "b2c_aykiri", "b2c_hiz_raporu")),
          downloadButton(ns("download_data_button_xlsx"), "Seçilen Temel Raporları İndir", class="btn-success btn-block")
      ),
      
      div(style = "margin-bottom: 20px;",
          p(strong("İnteraktif Analiz Raporları")),
          p(tags$small("Aktif sekmelerde gördüğünüz, filtrelenmiş analiz sonuçlarını indirin.")),
          downloadButton(ns("download_simulator_data"), "Ağırlık Simülatörü Sonucunu İndir", class="btn-info btn-block"),
          br(),
          
          if(isTRUE(can_download_ilce())) {
            downloadButton(ns("download_ilce_karsilastirma_data"), "İlçe Karşılaştırma Sonucunu İndir", class="btn-info btn-block")
          } else {
            tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'İlçe Karşılaştırma' sekmesinde bir analiz yapmalısınız.", "İlçe Karşılaştırma Sonucu")
          },
          br(),
          
          if(isTRUE(can_download_karne())) {
            downloadButton(ns("download_firma_karne_data"), "Firma Karnesi Sonucunu İndir", class="btn-info btn-block")
          } else {
            tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'Firma Karnesi' sekmesinde bir analiz yapmalısınız.", "Firma Karnesi Sonucu")
          },
          br(),
          
          if(isTRUE(can_download_marka())) {
            downloadButton(ns("download_marka_analizi_data"), "Marka Analizi Sonucunu İndir", class="btn-info btn-block")
          } else {
            tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'Marka Analizi' sekmesinde bir analiz yapmalısınız.", "Marka Analizi Sonucu")
          },
          br(),
          
          if(isTRUE(can_download_sikayet())) {
            downloadButton(ns("download_sikayet_analizi_data"), "Şikayet Analizi Sonucunu İndir", class="btn-info btn-block")
          } else {
            tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'Şikayet Analizi' sekmesinde bir analiz yapmalısınız.", "Şikayet Analizi Sonucu")
          },
          br(),
          
          downloadButton(ns("download_aykiri_deger_interaktif_data"), "Aykırı Değer Analizi Sonucunu İndir", class="btn-info btn-block"),
          br(),
          
          if(isTruthy(karsilastirma_verisi())) {
            downloadButton(ns("download_comparison_data"), "Dinamik Karşılaştırma Sonucunu İndir", class="btn-info btn-block")
          } else {
            tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce Dinamik Karşılaştırma sekmesinde bir analiz yapmalısınız.", "Dinamik Karşılaştırma Sonucu")
          }
      ),
      
      if(isTruthy(can_generate_brand_report())) {
        div(
          p(strong("Detaylı Kırılımlı Rapor")),
          p(tags$small("Tüm markaların coğrafi kırılımlı performansını tek dosyada alın.")),
          downloadButton(ns("download_data_button_csv"), "Tüm Marka Performansını İndir", class="btn-primary btn-block")
        )
      }
    )
  })
  
  # --- İNDİRME MANTIĞI (DOWNLOAD HANDLERS) ---
  output$download_data_button_xlsx <- downloadHandler(
    filename = function() { paste0("Lojistik_Temel_Raporlar_B2C_", Sys.Date(), ".xlsx") },
    content = function(file) {
      button_id <- ns("download_data_button_xlsx"); original_html <- "Seçilen Temel Raporları İndir"; on.exit({ shinyjs::html(id = button_id, html = original_html); shinyjs::removeClass(id = button_id, class = "btn-loading"); shinyjs::enable(id = button_id) }); shinyjs::html(id = button_id, html = "Raporlar Hazırlanıyor..."); shinyjs::addClass(id = button_id, class = "btn-loading"); shinyjs::disable(id = button_id)
      req(input$download_choices_xlsx)
      list_of_datasets <- list()
      if ("b2c_sonuclar" %in% input$download_choices_xlsx) list_of_datasets[["Analiz_ve_Skorlar"]] <- ana_veri_skorlari()
      if ("b2c_aykiri" %in% input$download_choices_xlsx) list_of_datasets[["Aykırı_Değerler"]] <- aykiri_veriler()
      if ("b2c_hiz_raporu" %in% input$download_choices_xlsx) list_of_datasets[["Bolgesel_Hiz_Karsilastirma"]] <- generate_speed_comparison_report(ana_veri_skorlari())
      if(length(list_of_datasets) > 0) { openxlsx::write.xlsx(list_of_datasets, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center")) } else { showNotification("İndirilecek seçili bir rapor bulunamadı.", type = "warning", duration=4); file.create(file) }
    }
  )
  
  output$download_data_button_csv <- downloadHandler(
    filename = function() { paste0("Lojistik_Marka_Raporu_Detayli_", Sys.Date(), ".csv") },
    content = function(file) {
      button_id <- ns("download_data_button_csv"); original_html <- "Tüm Marka Performansını İndir"; on.exit({ shinyjs::html(id = button_id, html = original_html); shinyjs::removeClass(id = button_id, class = "btn-loading"); shinyjs::enable(id = button_id) }); shinyjs::html(id = button_id, html = "Veriler Hazırlanıyor..."); shinyjs::addClass(id = button_id, class = "btn-loading"); shinyjs::disable(id = button_id)
      req(ham_veri_temiz())
      combined_df <- generate_all_brands_on_demand(ham_veri_temiz())
      if(isTruthy(combined_df) && nrow(combined_df) > 0){
        combined_df_final <- combined_df %>%
          select(`Marka Adı` = gonderici, `Şehir` = sehir, `İlçe` = ilce, `Kargo Firması` = kargo_turu, `Hacim Ayarlı Skor` = Bayes_EPS, `Ham Skor` = Ham_EPS, `Ortalama Teslim Süresi` = ortalama_teslim_suresi, `Başarı Oranı` = dinamik_basari_orani, `Şikayet Oranı` = sikayet_orani_yuzde, `Toplam Gönderi` = toplam_gonderi_sayisi)
        readr::write_csv(combined_df_final, file)
      } else {
        showNotification("İndirilecek marka verisi bulunamadı.", type="warning", duration=4)
        file.create(file)
      }
    }
  )
  
  output$download_simulator_data <- downloadHandler(
    filename = function() { paste0("Lojistik_Agirlik_Simulatoru_Raporu_", Sys.Date(), ".xlsx") },
    content = function(file) {
      button_id <- ns("download_simulator_data"); original_html <- "Ağırlık Simülatörü Sonucunu İndir"; on.exit({ shinyjs::html(id = button_id, html = original_html); shinyjs::removeClass(id = button_id, class = "btn-loading"); shinyjs::enable(id = button_id) }); shinyjs::html(id = button_id, html = "Rapor Hazırlanıyor..."); shinyjs::addClass(id = button_id, class = "btn-loading"); shinyjs::disable(id = button_id)
      req(simulator_data())
      df_to_download <- simulator_data() %>%
        mutate(Bayes_Skor = round(Bayes_EPS * 100, 2), Ham_Skor = round(Ham_EPS_Ağırlıklı * 100, 2)) %>%
        select(`Kargo Firması` = kargo_turu, `Hacim Ayarlı Skor (0-100)` = Bayes_Skor, `Ham Skor (0-100)` = Ham_Skor, `Toplam Gönderi Sayısı` = Toplam_Gonderi, `Açıklama` = Bayes_Aciklama) %>%
        arrange(desc(`Hacim Ayarlı Skor (0-100)`))
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
  
  output$download_comparison_data <- downloadHandler(
    filename = function() { paste0("Lojistik_Dinamik_Karsilastirma_", Sys.Date(), ".xlsx") },
    content = function(file) {
      button_id <- ns("download_comparison_data"); original_html <- "Dinamik Karşılaştırma Sonucunu İndir"; on.exit({ shinyjs::html(id = button_id, html = original_html); shinyjs::removeClass(id = button_id, class = "btn-loading"); shinyjs::enable(id = button_id) }); shinyjs::html(id = button_id, html = "Rapor Hazırlanıyor..."); shinyjs::addClass(id = button_id, class = "btn-loading"); shinyjs::disable(id = button_id)
      req(karsilastirma_verisi(), !is.null(input$secilen_metrikler) && length(input$secilen_metrikler) > 0)
      ham_veri <- karsilastirma_verisi()$data; ana_etiket <- karsilastirma_verisi()$ana_etiket; karsilastirma_etiket <- karsilastirma_verisi()$karsilastirma_etiket
      final_table <- ham_veri %>% select(`Kargo Firması`)
      metric_names_map <- c("toplam_gonderi_sayisi" = "Hacim", "ortalama_teslim_suresi" = "Ort. Hız", "dinamik_basari_orani" = "Başarı Oranı", "sikayet_orani_yuzde" = "Şikayet Oranı")
      for (metric_value in input$secilen_metrikler) {
        metric_name <- metric_names_map[[metric_value]]
        col_ana_raw <- paste0(metric_value, "_ana"); col_karsilastirma_raw <- paste0(metric_value, "_karsilastirma"); col_ana_yeni_ad <- paste0(metric_name, " (", ana_etiket, ")"); col_karsilastirma_yeni_ad <- paste0(metric_name, " (", karsilastirma_etiket, ")"); col_degisim_yeni_ad <- paste(metric_name, "Değişim (%)")
        temp_df <- ham_veri %>%
          mutate(across(everything(), ~replace_na(., 0))) %>%
          mutate(`Değişim` = if_else(.data[[col_ana_raw]] == 0, NA_real_, ((.data[[col_karsilastirma_raw]] - .data[[col_ana_raw]]) / .data[[col_ana_raw]]) * 100)) %>%
          rename(!!col_ana_yeni_ad := all_of(col_ana_raw), !!col_karsilastirma_yeni_ad := all_of(col_karsilastirma_raw), !!col_degisim_yeni_ad := `Değişim`) %>%
          select(`Kargo Firması`, all_of(c(col_ana_yeni_ad, col_karsilastirma_yeni_ad, col_degisim_yeni_ad)))
        final_table <- left_join(final_table, temp_df, by = "Kargo Firması")
      }
      openxlsx::write.xlsx(final_table, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
  
  output$download_ilce_karsilastirma_data <- downloadHandler(
    filename = function() { paste0("Ilce_Karsilastirma_", input$sehir_secimi_tab1, "_", Sys.Date(), ".xlsx") },
    content = function(file) {
      report_data <- ilce_karsilastirma_data()
      if (is.null(report_data) || nrow(report_data) == 0) { showNotification("Bu filtre için indirilecek İlçe Karşılaştırma verisi bulunamadı.", type = "warning", duration = 5); return(NULL) }
      df_to_download <- report_data %>%
        mutate(`Hacim Ayarlı Skor` = round(Bayes_EPS * 100, 2), `Ham Skor` = round(Ham_EPS * 100, 2), `Şikayet Oranı` = round(sikayet_orani_yuzde, 2)) %>%
        select(`Kargo Firması` = kargo_turu, `Hacim Ayarlı Skor`, `Ham Skor`, `Toplam Şikayet` = toplam_sikayet_sayisi, `Şikayet Oranı`, `Ortalama Desi` = ortalama_desi, `Gönderi Sayısı` = toplam_gonderi_sayisi, `Bölge Hacim Yüzdesi` = hacim_yuzdesi)
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE)
    }
  )
  
  output$download_firma_karne_data <- downloadHandler(
    filename = function() { paste0("Firma_Karnesi_", input$secilen_firma_karne, "_", Sys.Date(), ".xlsx") },
    content = function(file) {
      report_data <- firma_karne_tablo_verisi()
      if (is.null(report_data) || nrow(report_data) == 0) { showNotification("Bu filtre için indirilecek Firma Karnesi verisi bulunamadı.", type = "warning", duration = 5); return(NULL) }
      df_to_download <- report_data %>%
        mutate(`Genel Skor (EPS)` = round(Ham_EPS * 100, 1), `Başarı Oranı` = round(dinamik_basari_orani * 100, 1), `Şikayet Oranı` = round(sikayet_orani_yuzde, 1), `Ort. Teslim Süresi (Saat)` = round(ortalama_teslim_suresi, 2)) %>%
        select(`İlçe` = ilce, `Şehir` = sehir, `Genel Skor (EPS)`, `Başarı Oranı`, `Toplam Şikayet` = toplam_sikayet_sayisi, `Şikayet Oranı`, `Ort. Teslim Süresi (Saat)`, `Gönderi Sayısı` = toplam_gonderi_sayisi)
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE)
    }
  )
  
  output$download_marka_analizi_data <- downloadHandler(
    filename = function() { paste0("Marka_Analizi_", input$secilen_marka_analizi, "_", Sys.Date(), ".xlsx") },
    content = function(file) {
      report_data <- marka_analizi_data()
      if (is.null(report_data) || nrow(report_data) == 0) { showNotification("Bu filtre için indirilecek Marka Analizi verisi bulunamadı.", type = "warning", duration = 5); return(NULL) }
      df_to_download <- report_data %>%
        mutate(`Hacim Ayarlı Skor` = round(Bayes_EPS * 100, 2), `Ham Skor` = round(Ham_EPS * 100, 2), `Başarı Oranı` = round(dinamik_basari_orani * 100, 2), `Şikayet Oranı` = round(sikayet_orani_yuzde, 1)) %>%
        select(`Kargo Firması` = kargo_turu, `Hacim Ayarlı Skor`, `Ham Skor`, `Ort. Teslim Süresi (Saat)` = ortalama_teslim_suresi, `Başarı Oranı`, `Toplam Şikayet` = toplam_sikayet_sayisi, `Şikayet Oranı`, `Toplam Gönderi` = toplam_gonderi_sayisi)
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE)
    }
  )
  
  output$download_sikayet_analizi_data <- downloadHandler(
    filename = function() { paste0("Sikayet_Analizi_", Sys.Date(), ".xlsx") },
    content = function(file) {
      report_data <- sikayet_analizi_ozet_verisi()
      if (is.null(report_data) || nrow(report_data) == 0) { showNotification("Bu filtre için indirilecek Şikayet Analizi verisi bulunamadı.", type = "warning", duration = 5); return(NULL) }
      openxlsx::write.xlsx(report_data, file, asTable = TRUE)
    }
  )
  
  output$download_aykiri_deger_interaktif_data <- downloadHandler(
    filename = function() { paste0("Aykiri_Deger_Analizi_", Sys.Date(), ".xlsx") },
    content = function(file) {
      report_data <- aykiri_veriler()
      if (is.null(report_data) || nrow(report_data) == 0) { showNotification("İndirilecek Aykırı Değer verisi bulunamadı.", type = "warning", duration = 5); return(NULL) }
      summary_data <- if(input$aykiri_analiz_modu == "firma_ozet") {
        aykiri_firma_ozet_verisi()
      } else {
        aykiri_grafik_verisi()
      }
      list_of_datasets <- list("Analiz Özeti" = summary_data, "Tüm Aykırı Değer Detayları" = report_data)
      openxlsx::write.xlsx(list_of_datasets, file, asTable = TRUE)
    }
  )
  
  # --- ÇIKTI ---
  return(download_ui)
}
# server_b2b.R - TAM VE BİRLEŞTİRİLMİŞ KOD (Kapsamlı İndirme Yeteneğiyle)

server_b2b <- function(id, data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    
    base_data <- reactive({ data()$main_data })
    
    # Skorlama Fonksiyonu (değişiklik yok)
    generate_b2b_scores <- function(df, m, v_esik, c_taban, custom_context_C = NULL) {
      # ... (Bu fonksiyonun içi aynı kalıyor)
      summary_df <- df %>% 
        filter(!is.na(kargo_firmasi)) %>% 
        group_by(kargo_firmasi) %>% 
        summarise(toplam_kargo = n(), zamaninda_teslim_orani = mean(is_on_time, na.rm = TRUE), .groups = 'drop') %>% 
        mutate(across(where(is.numeric), ~if_else(is.na(.) | is.infinite(.), 0, .)))
      if(nrow(summary_df) == 0) return(tibble(kargo_firmasi=character(), toplam_kargo=integer(), zamaninda_teslim_orani=numeric(), Bayes_Skor=numeric())) # Return empty tibble with correct structure
      
      if (is.null(custom_context_C)) {
        safe_sum_prod <- sum(summary_df$zamaninda_teslim_orani * summary_df$toplam_kargo, na.rm = TRUE)
        safe_sum_weight <- sum(summary_df$toplam_kargo, na.rm = TRUE)
        context_average_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
      } else {
        context_average_C <- custom_context_C
      }
      
      final_df <- summary_df %>% 
        mutate(
          guvenilmez_mi = toplam_kargo < v_esik,
          Hedef_Puan_C = if_else(guvenilmez_mi, c_taban, context_average_C),
          Bayes_Skor = ((toplam_kargo / (toplam_kargo + m)) * zamaninda_teslim_orani) + ((m / (toplam_kargo + m)) * Hedef_Puan_C),
          Bayes_Aciklama = paste0("...")
        ) %>% 
        arrange(desc(Bayes_Skor))
      return(final_df)
    }
    
    # Reaktif bloklar (değişiklik yok, bunlar hala ekranı yönetiyor)
    main_summary <- reactive({ 
      req(base_data(), input$guvenilirlik_esigi, input$guven_esigi_v, input$taban_puan_c)
      generate_b2b_scores(base_data(), m = input$guvenilirlik_esigi, v_esik = input$guven_esigi_v, c_taban = input$taban_puan_c / 100) 
    })
    
    brand_performance <- reactive({ 
      req(base_data(), input$selected_brand, input$guvenilirlik_esigi, input$guven_esigi_v, input$taban_puan_c)
      brand_df <- base_data() %>% filter(gonderici == input$selected_brand)
      safe_sum_prod <- sum(brand_df$is_on_time, na.rm = TRUE)
      safe_sum_weight <- sum(!is.na(brand_df$is_on_time))
      brand_context_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
      generate_b2b_scores(brand_df, m = input$guvenilirlik_esigi, v_esik = input$guven_esigi_v, c_taban = input$taban_puan_c / 100, custom_context_C = brand_context_C)
    })
    
    city_performance <- reactive({ 
      req(base_data(), input$selected_province, input$guvenilirlik_esigi, input$guven_esigi_v, input$taban_puan_c)
      city_df <- base_data() %>% filter(il == input$selected_province)
      if(!is.null(input$selected_district) && input$selected_district != "Tüm İlçeler") { city_df <- city_df %>% filter(ilce == input$selected_district) }
      safe_sum_prod <- sum(city_df$is_on_time, na.rm = TRUE)
      safe_sum_weight <- sum(!is.na(city_df$is_on_time))
      city_context_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
      generate_b2b_scores(city_df, m = input$guvenilirlik_esigi, v_esik = input$guven_esigi_v, c_taban = input$taban_puan_c / 100, custom_context_C = city_context_C)
    })
    
    
    # <<< YENİ FONKSİYON 1: Tüm Markaların Raporunu Üretir >>>
    generate_all_brands_report <- function() {
      all_brands <- unique(base_data()$gonderici)
      
      # purrr::map_dfr kullanarak her bir marka için skor hesapla ve sonuçları birleştir
      purrr::map_dfr(all_brands, function(brand_name) {
        brand_df <- base_data() %>% filter(gonderici == brand_name)
        safe_sum_prod <- sum(brand_df$is_on_time, na.rm = TRUE)
        safe_sum_weight <- sum(!is.na(brand_df$is_on_time))
        brand_context_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
        
        generate_b2b_scores(
          brand_df, 
          m = input$guvenilirlik_esigi, 
          v_esik = input$guven_esigi_v, 
          c_taban = input$taban_puan_c / 100, 
          custom_context_C = brand_context_C
        ) %>%
          mutate(Marka = brand_name) # Hangi markaya ait olduğunu belirtmek için sütun ekle
      })
    }
    
    # <<< YENİ FONKSİYON 2: Tüm Coğrafi Bölgelerin Raporunu Üretir >>>
    generate_all_geo_report <- function() {
      # Bütün il-ilçe kombinasyonlarını bul
      all_geos <- base_data() %>% 
        filter(!is.na(il), !is.na(ilce)) %>%
        distinct(il, ilce)
      
      # Her bir coğrafi bölge için skorları hesapla ve birleştir
      purrr::map2_dfr(all_geos$il, all_geos$ilce, function(current_il, current_ilce) {
        geo_df <- base_data() %>% filter(il == current_il, ilce == current_ilce)
        safe_sum_prod <- sum(geo_df$is_on_time, na.rm = TRUE)
        safe_sum_weight <- sum(!is.na(geo_df$is_on_time))
        geo_context_C <- if(safe_sum_weight > 0) safe_sum_prod / safe_sum_weight else 0
        
        generate_b2b_scores(
          geo_df,
          m = input$guvenilirlik_esigi, 
          v_esik = input$guven_esigi_v, 
          c_taban = input$taban_puan_c / 100, 
          custom_context_C = geo_context_C
        ) %>%
          mutate(İl = current_il, İlçe = current_ilce) # Bölge bilgilerini ekle
      })
    }
    
    
    # Arayüzdeki tablolar, filtreler ve metinleri oluşturan kodlar (değişiklik yok)
    output$main_summary_table <- DT::renderDataTable({
      df_for_display <- main_summary() %>% mutate(Bayes_Skor_display = paste0('<span title="', Bayes_Aciklama, '">', round(Bayes_Skor * 100, 2), '</span>')) %>% select("Kargo Firması" = kargo_firmasi, "Hacim Ayarlı Skor (0-100)" = Bayes_Skor_display, "Zamanında Teslim (Ham %)" = zamaninda_teslim_orani, "Toplam Kargo" = toplam_kargo)
      DT::datatable(df_for_display, escape = FALSE, rownames = FALSE, selection = 'single', options = list(scrollX = TRUE, pageLength = 10)) %>% formatPercentage('Zamanında Teslim (Ham %)', digits = 1)
    })
    # ... (diğer output'lar ve observeEvent'ler burada aynı kalıyor) ...
    output$brand_performance_table <- DT::renderDataTable({
      df_for_display <- brand_performance() %>% select("Kargo Firması" = kargo_firmasi, "Performans Skoru (%)" = Bayes_Skor, "Toplam Kargo" = toplam_kargo)
      DT::datatable(df_for_display, rownames = FALSE, selection = 'single', options = list(scrollX = TRUE, pageLength = 10)) %>% formatPercentage("Performans Skoru (%)", digits = 2)
    })
    output$city_performance_table <- DT::renderDataTable({
      df_for_display <- city_performance() %>% select("Kargo Firması" = kargo_firmasi, "Performans Skoru (%)" = Bayes_Skor, "Toplam Kargo" = toplam_kargo)
      DT::datatable(df_for_display, rownames = FALSE, selection = 'single', options = list(scrollX = TRUE, pageLength = 10)) %>% formatPercentage("Performans Skoru (%)", digits = 2)
    })
    output$brand_filter_ui <- renderUI({ req(base_data()); selectInput(ns("selected_brand"), "Marka Seçin:", choices = sort(unique(base_data()$gonderici))) })
    output$province_filter_ui <- renderUI({ req(base_data()); selectInput(ns("selected_province"), "İl Seçin:", choices = sort(unique(base_data()$il[!is.na(base_data()$il)]))) })
    output$district_filter_ui <- renderUI({ req(base_data(), input$selected_province); districts <- base_data() %>% filter(il == input$selected_province) %>% pull(ilce) %>% unique(); selectInput(ns("selected_district"), "İlçe Seçin:", choices = c("Tüm İlçeler", sort(districts[!is.na(districts)]))) })
    output$recommendation_text <- renderText({ perf_data <- city_performance(); req(nrow(perf_data) > 0); best_performer <- perf_data %>% head(1); paste0("Bu bölgedeki operasyonel kalite önceliğine göre, en yüksek skora (", scales::percent(best_performer$Bayes_Skor, accuracy = 0.01), ") sahip firma: ", best_performer$kargo_firmasi, ".") })
    show_details_modal <- function(df, title) { showModal(modalDialog(title = title, DT::renderDataTable({ df_display <- df %>% select("Kargo No" = kargo_no, "Durum" = durum, "Teslim Süresi (Saat)" = toplam_teslim_suresi_saat, "Zamanında Teslim" = is_on_time); DT::datatable(df_display, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>% formatRound("Teslim Süresi (Saat)", digits = 2) }), footer = modalButton("Kapat"), size = "l", easyClose = TRUE)) }
    observeEvent(input$main_summary_table_rows_selected, { req(input$main_summary_table_rows_selected); selected_firma <- main_summary()[input$main_summary_table_rows_selected, ]$kargo_firmasi; detail_df <- base_data() %>% filter(kargo_firmasi == selected_firma); modal_title <- paste(selected_firma, "- Genel Sipariş Detayları"); show_details_modal(detail_df, modal_title) })
    observeEvent(input$brand_performance_table_rows_selected, { req(input$brand_performance_table_rows_selected, input$selected_brand); selected_firma <- brand_performance()[input$brand_performance_table_rows_selected, ]$kargo_firmasi; detail_df <- base_data() %>% filter(gonderici == input$selected_brand, kargo_firmasi == selected_firma); modal_title <- paste(input$selected_brand, "için", selected_firma, "Sipariş Detayları"); show_details_modal(detail_df, modal_title) })
    observeEvent(input$city_performance_table_rows_selected, { req(input$city_performance_table_rows_selected, input$selected_province); selected_firma <- city_performance()[input$city_performance_table_rows_selected, ]$kargo_firmasi; detail_df <- base_data() %>% filter(il == input$selected_province, kargo_firmasi == selected_firma); if (!is.null(input$selected_district) && input$selected_district != "Tüm İlçeler") { detail_df <- detail_df %>% filter(ilce == input$selected_district); bolge_adi <- paste(input$selected_province, "-", input$selected_district) } else { bolge_adi <- paste(input$selected_province, "Geneli") }; modal_title <- paste(bolge_adi, "için", selected_firma, "Sipariş Detayları"); show_details_modal(detail_df, modal_title) })
    
    
    # <<< DEĞİŞİKLİK: VERİ İNDİRME BÖLÜMÜ GÜNCELLENDİ >>>
    
    download_ui <- reactive({
      tagList(
        p(strong("B2B Kapsamlı Raporlar")),
        p(tags$small("Analizlerin kapsamlı dökümlerini indirin.")),
        hr(),
        downloadButton(ns("download_main_b2b"), "Genel Firma Karnesi Raporu", class="btn-primary btn-block"),
        br(),
        # Buton metinleri güncellendi
        downloadButton(ns("download_brand_b2b"), "Tüm Markaların Performans Raporu", class="btn-primary btn-block"),
        br(),
        downloadButton(ns("download_geo_b2b"), "Tüm Coğrafi Bölgelerin Raporu", class="btn-primary btn-block")
      )
    })
    
    # Ana karne indiricisi (değişiklik yok)
    output$download_main_b2b <- downloadHandler(
      filename = function() { paste0("B2B_Genel_Firma_Karnesi_", Sys.Date(), ".xlsx") },
      content = function(file) {
        req(main_summary())
        df_to_download <- main_summary() %>%
          mutate(`Hacim Ayarlı Skor` = round(Bayes_Skor * 100, 2)) %>%
          select(`Kargo Firması` = kargo_firmasi, `Hacim Ayarlı Skor`, `Zamanında Teslim Oranı` = zamaninda_teslim_orani, `Toplam Kargo` = toplam_kargo)
        openxlsx::write.xlsx(df_to_download, file, asTable = TRUE)
      }
    )
    
    # Marka performans indiricisi (yeni fonksiyonu kullanacak şekilde güncellendi)
    output$download_brand_b2b <- downloadHandler(
      filename = function() { paste0("B2B_Tum_Markalar_Raporu_", Sys.Date(), ".xlsx") },
      content = function(file) {
        # Artık ekrandaki reaktif veriyi değil, kapsamlı rapor fonksiyonunu çağırıyoruz
        report_data <- generate_all_brands_report()
        req(report_data)
        df_to_download <- report_data %>%
          mutate(Skor = round(Bayes_Skor * 100, 2)) %>%
          select(Marka, `Kargo Firması` = kargo_firmasi, `Performans Skoru` = Skor, `Toplam Kargo` = toplam_kargo, `Zamanında Teslim Oranı` = zamaninda_teslim_orani) %>%
          arrange(Marka, desc(`Performans Skoru`))
        openxlsx::write.xlsx(df_to_download, file, asTable = TRUE)
      }
    )
    
    # Coğrafi analiz indiricisi (yeni fonksiyonu kullanacak şekilde güncellendi)
    output$download_geo_b2b <- downloadHandler(
      filename = function() { paste0("B2B_Tum_Cografi_Rapor_", Sys.Date(), ".xlsx") },
      content = function(file) {
        # Kapsamlı coğrafi rapor fonksiyonunu çağırıyoruz
        report_data <- generate_all_geo_report()
        req(report_data)
        df_to_download <- report_data %>%
          mutate(Skor = round(Bayes_Skor * 100, 2)) %>%
          select(İl, İlçe, `Kargo Firması` = kargo_firmasi, `Performans Skoru` = Skor, `Toplam Kargo` = toplam_kargo, `Zamanında Teslim Oranı` = zamaninda_teslim_orani) %>%
          arrange(İl, İlçe, desc(`Performans Skoru`))
        openxlsx::write.xlsx(df_to_download, file, asTable = TRUE)
      }
    )
    
    return(
      list(
        download_ui = download_ui
      )
    )
    
  }) # moduleServer sonu
}
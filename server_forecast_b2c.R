# =========================================================================
#      B2C TAHMİNLEME MODÜLÜ - SUNUCU MANTIĞI (SERVER)
# =========================================================================
# Görevi: ui_forecast_b2c.R'da tanımlanan arayüzü yönetir.
#       - Kullanıcı seçimlerini alır.
#       - 'aylik_ozet' verisini kullanarak prophet ile tahminleme yapar.
#       - Grafik ve tablo çıktılarını oluşturur.
#       - YENİ: Başlangıçta bilgilendirme paneli gösterir.
# =========================================================================

server_forecast_b2c <- function(id, processed_data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. Temel Reaktif Veri ---
    aylik_veri <- reactive({
      req(processed_data())
      processed_data()$aylik_ozet
    })
    
    # --- 2. Dinamik UI Elemanları ---
    output$forecast_firma_secimi_ui <- renderUI({
      req(aylik_veri())
      firma_listesi <- sort(unique(aylik_veri()$`Kargo Firması`))
      selectInput(
        ns("forecast_firma_secimi"),
        "1. Kargo Firması Seçin:",
        choices = c("Tüm Firmalar (Genel Trend)" = "all_companies", firma_listesi)
      )
    })
    
    # --- 3. Tahminleme Motoru ---
    # `eventReactive` yerine `reactiveVal` ve `observeEvent` kullanalım.
    # Bu, `conditionalPanel` ile daha stabil çalışır.
    forecast_results <- reactiveVal(NULL)
    
    observeEvent(input$forecast_baslat_button, {
      
      req(aylik_veri(), input$forecast_firma_secimi, input$forecast_metrik_secimi)
      
      # Önceki sonuçları temizle
      forecast_results(NULL)
      
      show_notification <- showNotification("Tahminleme modeli oluşturuluyor...", duration = NULL, type = "message")
      on.exit(removeNotification(show_notification), add = TRUE)
      
      df <- aylik_veri()
      
      if (input$forecast_firma_secimi != "all_companies") {
        df <- df %>% filter(`Kargo Firması` == input$forecast_firma_secimi)
      } else {
        nested_data <- df %>%
          group_by(Donem) %>%
          nest()
        
        summarized_data <- nested_data %>%
          mutate(summary = map(data, function(monthly_df) {
            valid_rows <- monthly_df %>%
              filter(!is.na(`Ort. Teslimat Süresi (Saat)`) & !is.na(`Toplam Hacim`) & `Toplam Hacim` > 0)
            
            avg_delivery_time <- if(nrow(valid_rows) > 0) {
              weighted.mean(valid_rows$`Ort. Teslimat Süresi (Saat)`, valid_rows$`Toplam Hacim`, na.rm = TRUE)
            } else {
              NA_real_
            }
            
            tibble(
              `Toplam Hacim` = sum(monthly_df$`Toplam Hacim`, na.rm = TRUE),
              `Toplam Şikayet Adedi` = sum(monthly_df$`Toplam Şikayet Adedi`, na.rm = TRUE),
              `Ort. Teslimat Süresi (Saat)` = avg_delivery_time
            )
          }))
        
        df <- summarized_data %>%
          select(-data) %>%
          unnest(summary) %>%
          mutate(`Şikayet Oranı (%)` = if_else(`Toplam Hacim` > 0, (`Toplam Şikayet Adedi` / `Toplam Hacim`) * 100, 0))
      }
      
      prophet_df <- df %>%
        rename(y = !!input$forecast_metrik_secimi, ds = Donem) %>%
        mutate(ds = as.Date(paste0(ds, "-01"))) %>%
        filter(!is.na(y)) %>%
        select(ds, y)
      
      if(nrow(prophet_df) < 2) {
        showNotification("Seçilen filtre için tahmin yapılacak yeterli geçmiş veri (en az 2 ay) bulunamadı.", type="warning", duration=5)
        return() # reactiveVal'i NULL olarak bırak ve çık
      }
      
      model <- prophet(prophet_df)
      future <- make_future_dataframe(model, periods = as.numeric(input$forecast_donem_sayisi), freq = 'month')
      forecast <- predict(model, future)
      
      # Sonuçları reactiveVal'e ata
      forecast_results(
        list(model = model, forecast_data = forecast, df = prophet_df)
      )
    })
    
    # --- 4. Çıktıların Oluşturulması ---
    
    output$forecast_grafik_basligi <- renderText({
      req(forecast_results())
      firma_adi <- if(input$forecast_firma_secimi == "all_companies") "Tüm Firmalar" else input$forecast_firma_secimi
      paste0(firma_adi, " için '", input$forecast_metrik_secimi, "' Tahmini (Sonraki ", input$forecast_donem_sayisi, " Ay)")
    })
    
    output$forecast_grafigi <- renderPlot({
      res <- forecast_results()
      req(res)
      
      p <- plot(res$model, res$forecast_data) +
        labs(x = "Tarih", y = input$forecast_metrik_secimi)
      
      if (isTRUE(theme_reactive() == "dark")) {
        p <- p + theme(
          panel.background = element_rect(fill = "#343A40", color = NA),
          plot.background = element_rect(fill = "#343A40", color = NA),
          panel.grid.major = element_line(color = "#495057"),
          panel.grid.minor = element_line(color = "#495057"),
          text = element_text(color = "#E9ECEF"),
          axis.text = element_text(color = "#E9ECEF"),
          title = element_text(color = "#FFFFFF")
        )
      } else {
        p <- p + theme_minimal(base_size = 14)
      }
      
      p
      
    }, bg = "transparent")
    
    output$forecast_tablosu <- DT::renderDataTable({
      res <- forecast_results()
      req(res)
      
      res$forecast_data %>%
        mutate(ds = format(ds, "%Y-%m-%d")) %>%
        select(
          `Tarih` = ds,
          `Tahmin (yhat)` = yhat,
          `En Düşük Tahmin (yhat_lower)` = yhat_lower,
          `En Yüksek Tahmin (yhat_upper)` = yhat_upper,
          `Trend` = trend
        ) %>%
        filter(as.Date(Tarih) > max(res$df$ds)) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 12, scrollX = TRUE, searching = FALSE)
        ) %>%
        formatRound(columns = c("Tahmin (yhat)", "En Düşük Tahmin (yhat_lower)", "En Yüksek Tahmin (yhat_upper)", "Trend"), digits = 2)
    })
    
    # --- 5. KOŞULLU PANEL İÇİN GEREKLİ ÇIKTI ---
    # Bu reaktif, `forecast_results()`'ın dolu olup olmadığını kontrol eder.
    # `conditionalPanel` bu çıktının durumuna göre panelleri değiştirir.
    output$forecast_results_exist <- reactive({
      isTruthy(forecast_results())
    })
    
    # Bu seçenek, Shiny'nin gizli olan bir çıktıya ait reaktifleri
    # "askıya almasını" engeller. Bu, conditionalPanel'in düzgün çalışması için kritiktir.
    outputOptions(output, 'forecast_results_exist', suspendWhenHidden = FALSE)
    
  }) # moduleServer sonu
}
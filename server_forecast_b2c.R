# =========================================================================
#      B2C TAHMİNLEME MODÜLÜ - SUNUCU MANTIĞI (SERVER)
# =========================================================================
# YENİLİK: Durum takibi artık Kargo Firması, Metrik ve Periyot olmak üzere
#          üç input'u da kapsamaktadır. Bu girdilerden herhangi biri
#          değiştirildiğinde, "Ayarlar Değiştirildi" uyarısı gösterilir.
# =========================================================================

library(promises)
library(future)

plan(multisession)

server_forecast_b2c <- function(id, processed_data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. Temel Reaktif Veri ve Durum Takipçileri ---
    aylik_veri <- reactive({
      req(processed_data())
      processed_data()$aylik_ozet
    })
    
    forecast_results <- reactiveVal(NULL)
    # DEĞİŞİKLİK: Artık tek bir değer yerine bir parametre listesi saklıyoruz.
    last_calculated_params <- reactiveVal(NULL) 
    
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
    observeEvent(input$forecast_baslat_button, {
      req(aylik_veri(), input$forecast_firma_secimi, input$forecast_metrik_secimi)
      
      on.exit({
        shinyjs::html(id = "forecast_progress_text", html = 'Tahmini Oluştur <i class="fa fa-cogs" role="presentation" aria-label="cogs icon"></i>')
        shinyjs::removeClass(id = "forecast_container", class = "btn-loading")
        shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'auto');", ns("forecast_container")))
      })
      
      forecast_results(NULL)
      shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'none');", ns("forecast_container")))
      shinyjs::addClass(id = "forecast_container", class = "btn-loading")
      shinyjs::html(id = "forecast_progress_text", html = "Model Başlatılıyor...")
      shinyjs::runjs(sprintf("$('#%s').css('width', '60%%');", ns("forecast_progress_fill")))
      
      aylik_veri_snap <- aylik_veri()
      firma_secimi_snap <- input$forecast_firma_secimi
      metrik_secimi_snap <- input$forecast_metrik_secimi
      donem_sayisi_snap <- as.numeric(input$forecast_donem_sayisi)
      
      # DEĞİŞİKLİK: Hesaplama anındaki tüm parametreleri bir liste olarak sakla.
      last_calculated_params(list(
        company = firma_secimi_snap,
        metric = metrik_secimi_snap,
        period = donem_sayisi_snap
      ))
      
      future({
        # ... (future bloğunun içi aynı) ...
        df <- aylik_veri_snap
        if (firma_secimi_snap != "all_companies") { df <- df %>% filter(`Kargo Firması` == firma_secimi_snap) } else {
          nested_data <- df %>% group_by(Donem) %>% nest()
          summarized_data <- nested_data %>% mutate(summary = map(data, function(monthly_df) {
            valid_rows <- monthly_df %>% filter(!is.na(`Ort. Teslimat Süresi (Saat)`) & !is.na(`Toplam Hacim`) & `Toplam Hacim` > 0)
            avg_delivery_time <- if(nrow(valid_rows) > 0) { weighted.mean(valid_rows$`Ort. Teslimat Süresi (Saat)`, valid_rows$`Toplam Hacim`, na.rm = TRUE) } else { NA_real_ }
            tibble(`Toplam Hacim` = sum(monthly_df$`Toplam Hacim`, na.rm = TRUE), `Toplam Şikayet Adedi` = sum(monthly_df$`Toplam Şikayet Adedi`, na.rm = TRUE), `Ort. Teslimat Süresi (Saat)` = avg_delivery_time)
          }))
          df <- summarized_data %>% select(-data) %>% unnest(summary) %>% mutate(`Şikayet Oranı (%)` = if_else(`Toplam Hacim` > 0, (`Toplam Şikayet Adedi` / `Toplam Hacim`) * 100, 0))
        }
        prophet_df <- df %>% rename(y = !!metrik_secimi_snap, ds = Donem) %>% mutate(ds = as.Date(paste0(ds, "-01"))) %>% filter(!is.na(y)) %>% select(ds, y)
        if(nrow(prophet_df) < 2) { return(list(error = "Yetersiz veri (en az 2 ay gerekli).")) }
        model <- prophet(prophet_df); future_df <- make_future_dataframe(model, periods = donem_sayisi_snap, freq = 'month'); forecast <- predict(model, future_df)
        list(model = model, forecast_data = forecast, df = prophet_df, error = NULL)
        
      }, seed = TRUE) %...>% (function(result) {
        if (!is.null(result$error)) { showNotification(result$error, type="warning", duration=5); forecast_results(NULL) } else { forecast_results(result) }
        shinyjs::runjs(sprintf("$('#%s').css('width', '100%%');", ns("forecast_progress_fill")))
        shinyjs::delay(500, { shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("forecast_progress_fill"))) })
      })
      
      return(NULL)
    })
    
    # --- 4. Dinamik Ana Panel Arayüzü ---
    output$forecast_main_panel_ui <- renderUI({
      
      if (!isTruthy(forecast_results())) {
        return( div( style = "text-align: center; padding: 50px; border-radius: 10px; background-color: var(--panel-bg-light); border: 2px dashed var(--border-color-light);", icon("info-circle", "fa-3x", style="color: var(--text-color-light); opacity: 0.7;"), h4("Tahminlemeye Hazır", style="margin-top: 20px;"), p("Lütfen sol taraftaki menüden istediğiniz kargo firmasını, metriği ve tahmin periyodunu seçin."), p(strong("Ardından 'Tahmini Oluştur' butonuna basarak sonuçları bu alanda görün.")) ) )
      }
      
      # DEĞİŞİKLİK: Senkronizasyon kontrolü artık 3 parametreyi de denetliyor.
      last_params <- last_calculated_params()
      is_synced <- isTRUE(
        last_params$company == input$forecast_firma_secimi &&
          last_params$metric  == input$forecast_metrik_secimi &&
          last_params$period  == as.numeric(input$forecast_donem_sayisi)
      )
      
      if (!is_synced) {
        return( div( class = "warning-panel", icon("exclamation-triangle", "fa-3x"), h4("Ayarlar Değiştirildi", style="margin-top: 20px;"), p("Tahmin parametreleri değiştirildi. Güncel ayarları yansıtan bir sonuç görmek için lütfen tekrar hesaplama yapın."), p(strong("Lütfen 'Tahmini Oluştur' butonuna yeniden basın.")) ) )
      }
      
      if (is_synced) {
        return( tagList( h3(textOutput(ns("forecast_grafik_basligi"))), plotOutput(ns("forecast_grafigi"), height = "500px"), hr(), h4("Tahmin Veri Tablosu"), DT::dataTableOutput(ns("forecast_tablosu")) ) )
      }
    })
    
    # --- 5. Grafik ve Tablo Çıktıları ---
    output$forecast_grafik_basligi <- renderText({
      req(forecast_results())
      # DEĞİŞİKLİK: Başlık metni de artık saklanan parametrelerden okunuyor.
      last_params <- last_calculated_params()
      firma_adi <- if(last_params$company == "all_companies") "Tüm Firmalar" else last_params$company
      paste0(firma_adi, " için '", last_params$metric, "' Tahmini (Sonraki ", last_params$period, " Ay)")
    })
    
    output$forecast_grafigi <- renderPlot({ res <- forecast_results(); req(res); p <- plot(res$model, res$forecast_data) + labs(x = "Tarih", y = input$forecast_metrik_secimi); if (isTRUE(theme_reactive() == "dark")) { p <- p + theme(panel.background = element_rect(fill = "#343A40", color = NA), plot.background = element_rect(fill = "#343A40", color = NA), panel.grid.major = element_line(color = "#495057"), panel.grid.minor = element_line(color = "#495057"), text = element_text(color = "#E9ECEF"), axis.text = element_text(color = "#E9ECEF"), title = element_text(color = "#FFFFFF")) } else { p <- p + theme_minimal(base_size = 14) }; p }, bg = "transparent")
    
    output$forecast_tablosu <- DT::renderDataTable({
      res <- forecast_results(); req(res);
      res$forecast_data %>%
        mutate(ds = format(ds, "%Y-%m-%d")) %>%
        select(`Tarih` = ds, `Tahmin (yhat)` = yhat, `En Düşük Tahmin (yhat_lower)` = yhat_lower, `En Yüksek Tahmin (yhat_upper)` = yhat_upper, `Trend` = trend) %>%
        filter(as.Date(Tarih) > max(res$df$ds)) %>%
        DT::datatable(rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, searching = FALSE)) %>%
        formatRound(columns = c("Tahmin (yhat)", "En Düşük Tahmin (yhat_lower)", "En Yüksek Tahmin (yhat_upper)", "Trend"), digits = 2)
    })
    
  }) # moduleServer sonu
}
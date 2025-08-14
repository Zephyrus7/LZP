# =========================================================================
#      B2C TAHMİNLEME MODÜLÜ - SUNUCU MANTIĞI (SERVER)
# =========================================================================
# Amaç: ui_forecast_b2c.R'da tanımlanan arayüzü yönetir.
#       - Kullanıcı seçimlerini alır.
#       - 'aylik_ozet' verisini kullanarak prophet ile tahminleme yapar.
#       - Grafik ve tablo çıktılarını oluşturur.
# =========================================================================

server_forecast_b2c <- function(id, processed_data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. Temel Reaktif Veri ---
    # Processor'dan gelen 'aylik_ozet' verisini reaktif olarak al
    aylik_veri <- reactive({
      req(processed_data())
      processed_data()$aylik_ozet
    })
    
    # --- 2. Dinamik UI Elemanları ---
    # Aylık verideki kargo firmalarını al ve filtreye seçenek olarak ekle
    output$forecast_firma_secimi_ui <- renderUI({
      req(aylik_veri())
      firma_listesi <- sort(unique(aylik_veri()$`Kargo Firması`))
      selectInput(
        ns("forecast_firma_secimi"),
        "1. Kargo Firması Seçin:",
        # Seçeneklerin başına genel trend analizi için bir opsiyon ekle
        choices = c("Tüm Firmalar (Genel Trend)" = "all_companies", firma_listesi)
      )
    })
    
    # --- 3. Tahminleme Motoru ---
    # Sadece "Tahmini Oluştur" butonuna basıldığında tetiklenecek reaktif blok
    forecast_results <- eventReactive(input$forecast_baslat_button, {
      
      # Gerekli girdilerin mevcut olduğundan emin ol
      req(aylik_veri(), input$forecast_firma_secimi, input$forecast_metrik_secimi)
      
      # Kullanıcıya işlem başladığına dair bilgi ver
      show_notification <- showNotification("Tahminleme modeli oluşturuluyor...", duration = NULL, type = "message")
      on.exit(removeNotification(show_notification), add = TRUE)
      
      # Seçilen firmaya göre veriyi filtrele
      df <- aylik_veri()
      if (input$forecast_firma_secimi != "all_companies") {
        df <- df %>% filter(`Kargo Firması` == input$forecast_firma_secimi)
      } else {
        # "Tüm Firmalar" seçildiyse, veriyi döneme göre topla/ortalamasını al
        df <- df %>%
          group_by(Donem) %>%
          summarise(
            `Toplam Hacim` = sum(`Toplam Hacim`, na.rm = TRUE),
            `Ort. Teslimat Süresi (Saat)` = weighted.mean(`Ort. Teslimat Süresi (Saat)`, `Toplam Hacim`, na.rm = TRUE),
            `Toplam Şikayet Adedi` = sum(`Toplam Şikayet Adedi`, na.rm = TRUE),
            .groups = 'drop'
          ) %>% 
          mutate(`Şikayet Oranı (%)` = if_else(`Toplam Hacim` > 0, (`Toplam Şikayet Adedi` / `Toplam Hacim`) * 100, 0))
      }
      
      # Prophet'in istediği formata (ds, y) veriyi hazırla
      prophet_df <- df %>%
        rename(y = !!input$forecast_metrik_secimi, ds = Donem) %>%
        mutate(ds = as.Date(paste0(ds, "-01"))) %>% # YYYY-AA formatını tarihe çevir
        select(ds, y)
      
      # Eğer yeterli veri yoksa (prophet en az 2 nokta ister), işlemi durdur
      if(nrow(prophet_df) < 2) {
        showNotification("Seçilen filtre için tahmin yapılacak yeterli geçmiş veri (en az 2 ay) bulunamadı.", type="warning", duration=5)
        return(NULL)
      }
      
      # Prophet modelini kur ve eğit
      model <- prophet(prophet_df)
      
      # Geleceğe yönelik tahmin yapılacak tarih çerçevesini oluştur
      future <- make_future_dataframe(model, periods = as.numeric(input$forecast_donem_sayisi), freq = 'month')
      
      # Tahmini yap
      forecast <- predict(model, future)
      
      # Sonuçları (model, tahmin verisi) bir liste olarak döndür
      return(list(model = model, forecast_data = forecast, df = prophet_df))
    })
    
    # --- 4. Çıktıların Oluşturulması ---
    
    # Grafik başlığını dinamik olarak oluştur
    output$forecast_grafik_basligi <- renderText({
      req(forecast_results())
      firma_adi <- if(input$forecast_firma_secimi == "all_companies") "Tüm Firmalar" else input$forecast_firma_secimi
      paste0(firma_adi, " için '", input$forecast_metrik_secimi, "' Tahmini (Sonraki ", input$forecast_donem_sayisi, " Ay)")
    })
    
    # Tahmin grafiğini çiz
    output$forecast_grafigi <- renderPlot({
      res <- forecast_results()
      req(res)
      
      # Prophet'in kendi plot fonksiyonunu kullan
      p <- plot(res$model, res$forecast_data) +
        labs(x = "Tarih", y = input$forecast_metrik_secimi)
      
      # Dark Mode uyumu için grafik temasını ayarla
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
      
      p # Grafiği döndür
      
    }, bg = "transparent") # Arka planı şeffaf yap
    
    # Tahmin verilerini tablo olarak göster
    output$forecast_tablosu <- DT::renderDataTable({
      res <- forecast_results()
      req(res)
      
      # Gösterilecek veriyi seç ve formatla
      res$forecast_data %>%
        mutate(ds = format(ds, "%Y-%m-%d")) %>%
        select(
          `Tarih` = ds,
          `Tahmin (yhat)` = yhat,
          `En Düşük Tahmin (yhat_lower)` = yhat_lower,
          `En Yüksek Tahmin (yhat_upper)` = yhat_upper,
          `Trend` = trend
        ) %>%
        # Sadece gelecek tahminlerini göster
        filter(as.Date(Tarih) > max(res$df$ds)) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 12, scrollX = TRUE, searching = FALSE)
        ) %>%
        # Sayısal sütunları yuvarla
        formatRound(columns = c("Tahmin (yhat)", "En Düşük Tahmin (yhat_lower)", "En Yüksek Tahmin (yhat_upper)", "Trend"), digits = 2)
    })
    
  })
}
ui_forecast_b2c <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Gelecek Tahmini",
    icon = icon("chart-line"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Tahminleme Ayarları"),
        p(tags$small(em("Geçmiş aylık verileri kullanarak seçtiğiniz metriğin gelecekteki trendini ve mevsimselliğini tahmin edin."))),
        hr(),
        uiOutput(ns("forecast_firma_secimi_ui")),
        selectInput(
          ns("forecast_metrik_secimi"), 
          "2. Tahmin Edilecek Metrik:",
          choices = c(
            "Toplam Gönderi Hacmi" = "Toplam Hacim",
            "Ortalama Teslimat Süresi (Saat)" = "Ort. Teslimat Süresi (Saat)",
            "Şikayet Oranı (%)" = "Şikayet Oranı (%)"
          )
        ),
        sliderInput(
          ns("forecast_donem_sayisi"), 
          "3. Tahmin Periyodu (Ay):", 
          min = 3, max = 24, value = 12, step = 1
        ),
        hr(),
        
        div(id = ns("forecast_container"), 
            onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("forecast_baslat_button")),
            class = "btn btn-primary btn-block btn-progress-container",
            div(class = "btn-progress-fill", id = ns("forecast_progress_fill")),
            span(class = "btn-progress-text", id = ns("forecast_progress_text"), "Tahmini Oluştur", icon = icon("cogs"))
        )
        
      ),
      
      mainPanel(
        width = 9,
        # mainPanel artık sunucu tarafından dinamik olarak doldurulacak
        uiOutput(ns("forecast_main_panel_ui"))
      )
    )
  )
}
# =========================================================================
#      B2C TAHMİNLEME MODÜLÜ - KULLANICI ARAYÜZÜ (UI)
# =========================================================================
# YENİLİK: Bilgilendirme paneli artık Karanlık Mod ile uyumlu CSS
#          değişkenlerini kullanıyor.
# =========================================================================

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
        actionButton(
          ns("forecast_baslat_button"), 
          "Tahmini Oluştur ve Görüntüle", 
          icon = icon("cogs"), 
          class = "btn-primary btn-block"
        )
      ),
      
      mainPanel(
        width = 9,
        
        # KOŞUL 1: Henüz sonuç yoksa bu bilgilendirme panelini göster.
        conditionalPanel(
          condition = "!output.forecast_results_exist",
          ns = ns,
          
          # <<< DEĞİŞİKLİK BURADA: Sabit renkler yerine CSS değişkenleri kullanıldı >>>
          div(
            style = paste0(
              "text-align: center; padding: 50px; border-radius: 10px;",
              "background-color: var(--panel-bg-light);", # Temaya göre değişen arkaplan
              "border: 2px dashed var(--border-color-light);" # Temaya göre değişen çerçeve
            ),
            icon("info-circle", "fa-3x", style="color: var(--text-color-light); opacity: 0.7;"), # Temaya göre değişen ikon rengi
            h4("Tahminlemeye Hazır", style="margin-top: 20px;"),
            p("Lütfen sol taraftaki menüden istediğiniz kargo firmasını, metriği ve tahmin periyodunu seçin."),
            p(strong("Ardından 'Tahmini Oluştur ve Görüntüle' butonuna basarak sonuçları bu alanda görün."))
          )
        ),
        
        # KOŞUL 2: Sonuçlar mevcut olduğunda ise normal çıktıları göster.
        conditionalPanel(
          condition = "output.forecast_results_exist",
          ns = ns,
          h3(textOutput(ns("forecast_grafik_basligi"))),
          plotOutput(ns("forecast_grafigi"), height = "500px"),
          hr(),
          h4("Tahmin Veri Tablosu"),
          DT::dataTableOutput(ns("forecast_tablosu"))
        )
      )
    )
  )
}
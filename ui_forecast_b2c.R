# =========================================================================
#      B2C TAHMİNLEME MODÜLÜ - KULLANICI ARAYÜZÜ (UI)
# =========================================================================
# Amaç: Kullanıcının, B2C verileri üzerinden geleceğe yönelik
#       tahminler oluşturmak için gerekli parametreleri seçmesini
#       sağlayan arayüzü tanımlar.
# =========================================================================

ui_forecast_b2c <- function(id) {
  ns <- NS(id)
  
  # Bu arayüz, B2C analizi için oluşturulan sekmelerin yanına eklenecek
  # yeni bir "Gelecek Tahmini" sekmesinin içeriğini oluşturur.
  tabPanel(
    "Gelecek Tahmini", # Sekmenin başlığı
    icon = icon("chart-line"), # Sekmenin ikonu
    
    sidebarLayout(
      # -----------------------------------------------------------------
      # Sol Panel: Kullanıcının ayar yapacağı kontroller
      # -----------------------------------------------------------------
      sidebarPanel(
        width = 3,
        h4("Tahminleme Ayarları"),
        
        p(tags$small(em("Geçmiş aylık verileri kullanarak seçtiğiniz metriğin gelecekteki trendini ve mevsimselliğini tahmin edin."))),
        
        hr(),
        
        # 1. Kargo Firması Seçimi (Bu, sunucu tarafından doldurulacak)
        uiOutput(ns("forecast_firma_secimi_ui")),
        
        # 2. Tahmin Edilecek Metrik Seçimi
        selectInput(
          ns("forecast_metrik_secimi"), 
          "2. Tahmin Edilecek Metrik:",
          choices = c(
            "Toplam Gönderi Hacmi" = "Toplam Hacim",
            "Ortalama Teslimat Süresi (Saat)" = "Ort. Teslimat Süresi (Saat)",
            "Şikayet Oranı (%)" = "Şikayet Oranı (%)"
          )
        ),
        
        # 3. Tahmin Periyodu Seçimi
        sliderInput(
          ns("forecast_donem_sayisi"), 
          "3. Tahmin Periyodu (Ay):", 
          min = 3, max = 24, value = 12, step = 1
        ),
        
        hr(),
        
        # 4. Tahmini Başlatma Butonu
        actionButton(
          ns("forecast_baslat_button"), 
          "Tahmini Oluştur ve Görüntüle", 
          icon = icon("cogs"), 
          class = "btn-primary btn-block"
        )
      ),
      
      # -----------------------------------------------------------------
      # Ana Panel: Tahmin sonuçlarının gösterileceği alan
      # -----------------------------------------------------------------
      mainPanel(
        width = 9,
        
        # Grafik için dinamik bir başlık
        h3(textOutput(ns("forecast_grafik_basligi"))),
        
        # Prophet modelinin üreteceği grafik için bir yer tutucu
        plotOutput(ns("forecast_grafigi"), height = "500px"),
        
        hr(),
        
        # Tahmin verilerini detaylı gösterecek tablo için bir başlık
        h4("Tahmin Veri Tablosu"),
        
        # Tahmin verilerini gösterecek tablo için bir yer tutucu
        DT::dataTableOutput(ns("forecast_tablosu"))
      )
    )
  )
}
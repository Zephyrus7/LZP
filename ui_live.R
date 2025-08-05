#==============================================================================#
#       CANLI ANALİZ KULLANICI ARAYÜZÜ (Dashboard Arayüzü v1.0)                #
#==============================================================================#
# AMAÇ: Bu UI modülü, server_live.R tarafından üretilen tüm canlı analiz
#       çıktılarını (grafikler, tablolar) tek bir panelde düzenli bir
#       şekilde kullanıcıya sunar.
#==============================================================================#

ui_live <- function(id) {
  # Namespace (ns) fonksiyonunu oluştur
  ns <- NS(id)
  
  # Arayüzü, app.R'a eklenecek bir tabPanel listesi olarak döndür
  list(
    tabPanel(
      title = "Canlı Operasyon Paneli",
      icon = icon("satellite-dish"), # Canlı veriyi simgeleyen bir ikon
      
      # Sayfa içeriğini düzenlemek için fluidPage kullan
      fluidPage(
        
        #--------------- ÜST SATIR: EN KRİTİK ALARM VE KONTROLLER ---------------#
        fluidRow(
          
          # Hareketsiz Gönderiler Alarm Listesi
          column(
            width = 12,
            h3("Hareketsiz Gönderi Alarm Listesi"),
            p("Aşağıdaki listede, belirlenen süreden daha uzun süredir durumu değişmeyen 'unutulmuş' kargolar gösterilmektedir."),
            
            wellPanel(
              style = "background-color: #f8d7da; border-color: #f5c6cb;", # Hafif kırmızı bir uyarı paneli
              
              # Kullanıcının gün eşiğini gireceği numericInput
              numericInput(
                inputId = ns("hareketsiz_gun_esigi"),
                label = "Kaç günden daha uzun süredir hareketsiz olanları göster?",
                value = 3, # Varsayılan değer 3 gün
                min = 1,
                max = 30,
                step = 1,
                width = "400px"
              ),
              
              hr(),
              
              # server_live.R'daki 'hareketsiz_gonderi_table' çıktısını göster
              DT::dataTableOutput(ns("hareketsiz_gonderi_table"))
            )
          )
        ),
        
        hr(), # Satırları ayırmak için yatay bir çizgi
        
        #--------------- ALT SATIR: GENEL DURUM ANALİZLERİ ---------------#
        fluidRow(
          
          # SOL SÜTUN: Açık Gönderi Durum Dağılımı
          column(
            width = 5,
            h3("Açık Gönderilerin Anlık Dağılımı"),
            p("Henüz teslimat veya iade süreci tamamlanmamış kargoların mevcut durumlarına göre dağılımı."),
            
            # server_live.R'daki 'acik_gonderi_plot' çıktısını göster
            plotOutput(ns("acik_gonderi_plot"), height = "500px")
          ),
          
          # SAĞ SÜTUN: "Altın Süreç" Performansı
          column(
            width = 7,
            h3('"Altın Süreç" Adımlarının Performansı'),
            p("Tamamlanmış süreç adımlarının ne kadar iş saatinde tamamlandığını gösteren dağılım ve özet istatistikler."),
            
            # server_live.R'daki 'altin_surec_plot' çıktısını göster
            plotOutput(ns("altin_surec_plot"), height = "300px"),
            
            hr(),
            
            # server_live.R'daki 'altin_surec_table' çıktısını göster
            DT::dataTableOutput(ns("altin_surec_table"))
          )
        )
      )
    )
  )
}
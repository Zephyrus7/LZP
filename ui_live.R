# =================================================================
#           CANLI ANALİZ ARAYÜZ (UI) MODÜLÜ (TÜM SEKMELER)
# =================================================================
# AMAÇ: Bu UI modülü, tüm canlı analiz çıktılarını dört ayrı
#       ve odaklanmış sekmede sunar.
# =================================================================

ui_live <- function(id) {
  # Namespace (ns) fonksiyonunu oluştur
  ns <- NS(id)
  
  # Arayüzü, app.R'a eklenecek bir tabPanel listesi olarak döndür
  list(
    
    # --- Sekme 1: Hareketsiz Gönderiler (Mevcut) ---
    tabPanel(
      title = "Hareketsiz Gönderiler",
      icon = icon("bell"),
      fluidPage(
        fluidRow(
          column(width = 12, h3("Hareketsiz Gönderi Alarm Listesi"),
                 p("Aşağıdaki listede, son hareket tarihinden bu yana belirlenen süreden daha uzun süredir durumu değişmeyen 'açık' (teslim edilmemiş) kargolar gösterilmektedir."),
                 wellPanel(
                   style = "background-color: #f8d7da; border-color: #f5c6cb;",
                   numericInput(inputId = ns("hareketsiz_gun_esigi"), label = "Kaç günden daha uzun süredir hareketsiz olanları göster?", value = 3, min = 1, max = 30, step = 1, width = "400px"),
                   hr(),
                   DT::dataTableOutput(ns("hareketsiz_gonderi_table"))
                 )
          )
        )
      )
    ),
    
    # --- Sekme 2: Açık Gönderi Durumu (Mevcut) ---
    tabPanel(
      title = "Açık Gönderi Durumu",
      icon = icon("chart-pie"),
      fluidPage(
        fluidRow(
          column(width = 12, h3("Açık Gönderilerin Anlık Durum Dağılımı"),
                 p("Henüz teslimatı tamamlanmamış kargoların, bilinen en son durumlarına göre dağılımı."),
                 plotOutput(ns("acik_gonderi_plot"), height = "500px"),
                 hr(),
                 DT::dataTableOutput(ns("acik_gonderi_table"))
          )
        )
      )
    ),
    
    # ====================================================================
    # === DEĞİŞİKLİK BURADA: Yeni Sekmeler Eklendi ========================
    # ====================================================================
    
    # --- Sekme 3: Altın Süreç Analizi (YENİ) ---
    tabPanel(
      title = "Altın Süreç Analizi",
      icon = icon("project-diagram"),
      fluidPage(
        fluidRow(
          column(
            width = 12,
            h3('"Altın Süreç" Adımlarının Performansı'),
            p("Tamamlanmış süreç adımlarının ne kadar iş saatinde tamamlandığını gösteren dağılım ve özet istatistikler."),
            
            # Sonuçların gösterileceği grafik ve tablo için yer tutucular
            plotOutput(ns("altin_surec_plot"), height = "400px"),
            hr(),
            DT::dataTableOutput(ns("altin_surec_table"))
          )
        )
      )
    ),
    
    # --- Sekme 4: Canlı Firma Karnesi (YENİ) ---
    tabPanel(
      title = "Canlı Firma Karnesi",
      icon = icon("tachometer-alt"),
      fluidPage(
        fluidRow(
          column(
            width = 12,
            h3("Kargo Firması Anlık Performans Karnesi"),
            p("Seçilen tarih aralığındaki tamamlanmış gönderilere göre firmaların anlık teslimat ve başarı performansları."),
            
            # Sonuçların gösterileceği tablo için yer tutucu
            DT::dataTableOutput(ns("canli_karne_table"))
          )
        )
      )
    )
    # ====================================================================
    
  ) # list sonu
}
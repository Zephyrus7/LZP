# =...
# YENİLİK: "Teslimat Performansı" sekmesi, iki ayrı tablo yerine
#          tüm bilgileri birleştiren tek ve minimalist bir
#          "Genel Performans Karnesi" tablosunu gösterecek şekilde
#          yeniden tasarlandı.
# =========================================================================

ui_live <- function(id) {
  ns <- NS(id)
  
  list(
    # --- SEKME 1: Teslimat Performansı (TAMAMEN YENİLENDİ) ---
    tabPanel(
      title = "Teslimat Performansı",
      icon = icon("truck-fast"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     h4("Performans Filtreleri"),
                     uiOutput(ns("firma_filtre_ui_teslimat")),
                     hr(),
                     p(tags$small(em("Yukarıdaki filtrelere göre taşıyıcıların genel performans karnesini analiz edin.")))
        ),
        mainPanel(width = 9,
                  # YENİ YAPI: Tek, birleştirilmiş tablo
                  h4("Genel Performans Karnesi"),
                  p(tags$small("Tüm temel performans göstergelerinin (KPI) birleştirildiği özet rapor. 'Teslimat Hızı Dağılımı' sütunu, sırasıyla 0-24, 24-48, 48-72 ve 72+ saat aralıklarındaki gönderi oranını görsel olarak temsil eder.")),
                  DT::dataTableOutput(ns("unified_performance_table"))
        )
      )
    ),
    
    # --- SEKME 2: Operasyonel Alarmlar (Değişiklik Yok) ---
    tabPanel(
      title = "Operasyonel Alarmlar",
      icon = icon("bell"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     h4("Filtreler"),
                     uiOutput(ns("firma_filtre_ui_operasyonel")),
                     hr(),
                     h4("Hareketsiz Gönderi Ayarları"),
                     numericInput(inputId = ns("hareketsiz_gun_esigi"), 
                                  label = "Kaç günden daha uzun süredir hareketsiz olanları göster?", 
                                  value = 3, min = 1, max = 30, step = 1)
        ),
        mainPanel(width = 9,
                  div(style = "display: flex; justify-content: space-between; align-items: center;",
                      h3("Açık Gönderilerin Anlık Durum Dağılımı"),
                      uiOutput(ns("operasyonel_total_count_ui"))
                  ),
                  plotOutput(ns("acik_gonderi_plot"), height = "450px"),
                  hr(),
                  h3("Hareketsiz Gönderi Alarm Listesi"),
                  p("Aşağıdaki listede, belirlenen eşikten daha uzun süredir son durumu değişmeyen 'açık' kargolar gösterilmektedir."),
                  DT::dataTableOutput(ns("hareketsiz_gonderi_table"))
        )
      )
    ),
    
    # --- SEKME 3: İade Süreçleri (Değişiklik Yok) ---
    tabPanel(
      title = "İade Süreçleri",
      icon = icon("undo"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     h4("İade Filtreleri"),
                     uiOutput(ns("firma_filtre_ui_iade")),
                     hr(),
                     p(tags$small(em("Yukarıdaki filtrelere göre iade sürecinin adımlarını analiz edin.")))
        ),
        mainPanel(width = 9,
                  div(style = "display: flex; justify-content: space-between; align-items: center;",
                      h3("İade Süreç Adımlarının Performansı"),
                      uiOutput(ns("iade_total_count_ui"))
                  ),
                  fluidRow(
                    column(4, wellPanel(style="background-color: #ffeaea", h5("Ort. Müşterinin Gönderme Süresi (Saat)"), h3(textOutput(ns("musteri_iade_suresi_val"))))),
                    column(4, wellPanel(style="background-color: #fff4e2", h5("Ort. Partner İade Süresi (Saat)"), h3(textOutput(ns("partner_iade_suresi_val"))))),
                    column(4, wellPanel(style="background-color: #eaf5ff", h5("Ort. Bovo İade Teslim Süresi (Saat)"), h3(textOutput(ns("bovo_iade_suresi_val")))))
                  ),
                  hr(),
                  h4("Firma Bazında İade Süre Dağılımı"),
                  DT::dataTableOutput(ns("iade_performans_table"))
        )
      )
    ),
    
    # --- SEKME 4: Tüm Siparişler Özeti (Değişiklik Yok) ---
    tabPanel(
      title = "Tüm Siparişler Özeti",
      icon = icon("archive"),
      fluidPage(
        wellPanel(
          h3("Tüm Siparişlerin Durum Dağılım Raporu"),
          p("Bu rapor, seçilen tarih aralığındaki **tüm siparişleri (açık, kapalı, iptal vb.)** analiz eder ve durumlarına göre özetler. Veri tabanının tamamını sorgulayabileceği için, bu analiz sadece butona tıklandığında başlar."),
          actionButton(ns("run_full_analysis_button"), "Raporu Oluştur ve Görüntüle", icon = icon("cogs"), class = "btn-primary btn-lg")
        ),
        hr(),
        shinyjs::hidden(
          div(id = ns("full_analysis_results_panel"),
              h3(textOutput(ns("full_analysis_title"))),
              DT::dataTableOutput(ns("full_analysis_table"))
          )
        )
      )
    )
    
  ) # list sonu
}
ui_live <- function(id) {
  ns <- NS(id)
  
  list(
    # --- SEKME 1: Teslimat Performansı (Değişiklik Yok) ---
    tabPanel(
      title = "Teslimat Performansı",
      icon = icon("truck-fast"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     h4("Performans Filtreleri"),
                     uiOutput(ns("firma_filtre_ui_teslimat")),
                     uiOutput(ns("marka_filtre_ui_teslimat")),
                     hr(),
                     p(tags$small(em("Yukarıdaki filtrelere göre teslimat sürecinin adımlarını (Alım, Partnere Verme, Teslimat) analiz edin.")))
        ),
        mainPanel(width = 9,
                  h3("Teslimat Süreç Adımlarının Performansı"),
                  fluidRow(
                    column(4, wellPanel(style="background-color: #eaf5ff", h5("Ort. Alım Süresi (Saat)"), h3(textOutput(ns("alim_suresi_val"))))),
                    column(4, wellPanel(style="background-color: #fff4e2", h5("Ort. Partnere Verme Süresi (Saat)"), h3(textOutput(ns("partnere_verme_suresi_val"))))),
                    column(4, wellPanel(style="background-color: #eaffee", h5("Ort. Partner Teslim Süresi (Saat)"), h3(textOutput(ns("partner_teslim_suresi_val")))))
                  ),
                  hr(),
                  h4("Firma Bazında Süre Dağılımı"),
                  DT::dataTableOutput(ns("teslimat_performans_table"))
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
                  h3("Açık Gönderilerin Anlık Durum Dağılımı"),
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
                     uiOutput(ns("marka_filtre_ui_iade")),
                     hr(),
                     p(tags$small(em("Yukarıdaki filtrelere göre iade sürecinin adımlarını analiz edin.")))
        ),
        mainPanel(width = 9,
                  h3("İade Süreç Adımlarının Performansı"),
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
    
    # ========================================================================
    #               YENİ SEKME 4: TÜM SİPARİŞLER ÖZETİ
    # ========================================================================
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
        # Sonuçlar başlangıçta gizli olacak
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
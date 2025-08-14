# =================================================================
#           CANLI ANALİZ ARAYÜZ (UI) MODÜLÜ (SIDEBAR EKLENMİŞ)
# =================================================================
# GÜNCELLEME: İki analiz sekmesi de (Açık Gönderi ve Sipariş Dağılımı)
#             istek üzerine sidebarLayout yapısına geçirildi.
#             Her iki sekme için de firma filtresi yer tutucuları
#             kenar çubuklarına eklendi.
# =================================================================

ui_live <- function(id) {
  ns <- NS(id)
  
  list(
    # --- Sekme 1: Hareketsiz Gönderiler (Değişiklik Yok) ---
    tabPanel(
      title = "Hareketsiz Gönderiler",
      icon = icon("bell"),
      fluidPage(
        fluidRow(
          column(width = 12, h3("Hareketsiz Gönderi Alarm Listesi"),
                 p("Aşağıdaki listede, son hareket tarihinden bu yana belirlenen süreden daha uzun süredir durumu değişmeyen 'açık' (teslim edilmemiş) kargolar gösterilmektedir."),
                 wellPanel(style = "background-color: #f8d7da; border-color: #f5c6cb;",
                           numericInput(inputId = ns("hareketsiz_gun_esigi"), label = "Kaç günden daha uzun süredir hareketsiz olanları göster?", value = 3, min = 1, max = 30, step = 1, width = "400px"),
                           hr(),
                           DT::dataTableOutput(ns("hareketsiz_gonderi_table"))
                 )
          )
        )
      )
    ),
    
    # --- Sekme 2: Açık Gönderi Durumu (SidebarLayout ile Güncellendi) ---
    tabPanel(
      title = "Açık Gönderi Durumu",
      icon = icon("chart-pie"),
      sidebarLayout(
        # === DEĞİŞİKLİK BURADA: Sidebar Paneli Eklendi ===
        sidebarPanel(width = 3,
                     h4("Filtreler"),
                     p(tags$small(em("Grafiği ve tabloyu seçilen kargo firmasına göre filtreleyin."))),
                     # Bu sekmenin filtresi için UI yer tutucusu
                     uiOutput(ns("firma_filtre_ui_acik"))
        ),
        # === DEĞİŞİKLİK BURADA: Ana Panel Eklendi ===
        mainPanel(width = 9,
                  h3("Açık Gönderilerin Anlık Durum Dağılımı"),
                  plotOutput(ns("acik_gonderi_plot"), height = "500px"),
                  hr(),
                  DT::dataTableOutput(ns("acik_gonderi_table"))
        )
      )
    ),
    
    # --- Sekme 3: Altın Süreç Analizi (Değişiklik Yok) ---
    tabPanel(
      title = "Altın Süreç Analizi",
      icon = icon("project-diagram"),
      fluidPage(
        fluidRow(
          column(width = 12, h3('"Altın Süreç" Adımlarının Performansı'),
                 p("Tamamlanmış süreç adımlarının ne kadar iş saatinde tamamlandığını gösteren dağılım ve özet istatistikler."),
                 plotOutput(ns("altin_surec_plot"), height = "400px"),
                 hr(),
                 DT::dataTableOutput(ns("altin_surec_table"))
          )
        )
      )
    ),
    
    # --- Sekme 4: Canlı Firma Karnesi (Değişiklik Yok) ---
    tabPanel(
      title = "Canlı Firma Karnesi",
      icon = icon("tachometer-alt"),
      fluidPage(
        fluidRow(
          column(width = 12, h3("Kargo Firması Anlık Performans Karnesi"),
                 p("Seçilen tarih aralığındaki tamamlanmış gönderilere göre firmaların anlık teslimat ve başarı performansları."),
                 DT::dataTableOutput(ns("canli_karne_table"))
          )
        )
      )
    ),
    
    # --- Sekme 5: Anlık Sipariş Durum Dağılımı (SidebarLayout ile Güncellendi) ---
    tabPanel(
      title = "Sipariş Durum Dağılımı",
      icon = icon("tasks"),
      sidebarLayout(
        # === DEĞİŞİKLİK BURADA: Sidebar Paneli Eklendi ===
        sidebarPanel(width = 3,
                     h4("Filtreler"),
                     p(tags$small(em("Aşağıdaki tabloyu seçilen kargo firmasına göre filtreleyin."))),
                     # Bu sekmenin filtresi için UI yer tutucusu
                     uiOutput(ns("firma_filtre_ui_dagilim"))
        ),
        # === DEĞİŞİKLİK BURADA: Ana Panel Eklendi ===
        mainPanel(width = 9,
                  h3("Tüm Siparişlerin Anlık Durum Dağılımı"),
                  p("Platformdaki tüm siparişlerin (açık, kapalı, iptal vb.) genel durum dağılımını gösterir."),
                  DT::dataTableOutput(ns("surec_performans_table"))
        )
      )
    )
    
  ) # list sonu
}
# ui_b2b.R - GÜNCELLENMİŞ VE TAM HALİ (Jargon Kaldırıldı)

ui_b2b <- function(id) {
  ns <- NS(id)
  list(
    tabPanel("Kargo Firması Karnesi",
             sidebarLayout(
               sidebarPanel(width = 3,
                            
                            # === DEĞİŞİKLİK BURADA ===
                            h4("Hacim Ayarlı Skorlama Ayarları"),
                            # ========================
                            
                            p(tags$small(em("Bu ayarlar, düşük gönderi hacmine sahip firmaların skorlarını daha adil ve istatistiksel olarak anlamlı hale getirmek için kullanılır."))),
                            sliderInput(ns("guven_esigi_v"), label = "1. Minimum Güven Eşiği (v_eşik):", min = 0, max = 5000, value = 1500, step = 50),
                            p(tags$small("Bir firmanın 'Güvenilir' kabul edilmesi için gereken minimum gönderi sayısı. Bu sayının altındaki firmalar 'Güvenilmez Bölge'de kabul edilir ve skorları Taban Puana doğru çekilir.")),
                            sliderInput(ns("taban_puan_c"), label = "2. Taban Puan (C_taban):", min = 0, max = 100, value = 50, post = " Puan"),
                            p(tags$small("'Güvenilmez Bölge'deki firmaların skorlarının çekileceği varsayılan düşük puandır.")),
                            sliderInput(ns("guvenilirlik_esigi"), label = "3. İstatistiksel Güvenilirlik Eşiği (m):", min = 0, max = 2500, value = 750, step = 50),
                            p(tags$small("Skorların hedef puana ne kadar şiddetle çekileceğini belirler. Yüksek değerler, 'çekim gücünü' artırır."))
               ),
               mainPanel(width = 9,
                         h3("Hacme Göre Ayarlanmış Kargo Firması Performans Skoru"),
                         p("Performans, zamanında teslimat oranına göre hesaplanmış ve düşük hacimli firmaların skorları hacimlerine göre adil bir şekilde ayarlanmıştır."),
                         hr(),
                         DT::dataTableOutput(ns("main_summary_table"))
               )
             )
    ),
    # Diğer sekmelerde değişiklik yok
    tabPanel("Marka Performansı", sidebarLayout(sidebarPanel(h4("Filtrele"), uiOutput(ns("brand_filter_ui"))), mainPanel(h3("Seçilen Markanın Kargo Firmalarıyla Performansı"), DT::dataTableOutput(ns("brand_performance_table"))))),
    tabPanel("Coğrafi Analiz", sidebarLayout(sidebarPanel(uiOutput(ns("province_filter_ui")), uiOutput(ns("district_filter_ui"))), mainPanel(h4("Performans Önerisi"), verbatimTextOutput(ns("recommendation_text")), hr(), DT::dataTableOutput(ns("city_performance_table")))))
  )
}
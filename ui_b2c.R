# ui_b2c.R - GÜNCELLENMİŞ HAL (Karşılaştırma Paneli Basitleştirildi)

# =========================================================================
#                   >>> YARDIMCI FONKSİYON <<<
# =========================================================================
shimmer_placeholder <- function(height = "100px", width = "100%") {
  div(class = "shimmer-placeholder", style = paste0("height: ", height, "; width: ", width, ";"))
}
# =========================================================================


ui_b2c <- function(id) {
  ns <- NS(id)
  list(
    tabPanel("Ağırlık Simülatörü", icon = icon("sliders-h"),
             sidebarLayout(
               sidebarPanel(width=3, 
                            h4("Stratejik Öncelikleriniz"), 
                            sliderInput(ns("agirlik_performans"), "Başarı Oranı Ağırlığı:", min=0, max=100, value=40, post=" %"), 
                            sliderInput(ns("agirlik_hiz"), "Hız Ağırlığı:", min=0, max=100, value=40, post=" %"), 
                            sliderInput(ns("agirlik_sikayet"), "Müşteri Deneyimi Ağırlığı:", min=0, max=100, value=20, post=" %"), 
                            h4("Ağırlık Toplamı:"), 
                            h3(textOutput(ns("agirlik_toplami"))),
                            hr(),
                            h4("Hacim Ayarlı Skorlama Ayarları"),
                            p(tags$small(em("Bu ayarlar, düşük gönderi hacmine sahip firmaların skorlarını daha adil ve istatistiksel olarak anlamlı hale getirmek için kullanılır."))),
                            sliderInput(ns("guven_esigi_v"), label = "1. Minimum Güven Eşiği (v_eşik):", min = 0, max = 5000, value = 1500, step = 50),
                            p(tags$small("Bir firmanın 'Güvenilir' kabul edilmesi için gereken minimum gönderi sayısı. Bu sayının altındaki firmalar 'Güvenilmez Bölge'de kabul edilir ve skorları Taban Puana doğru çekilir.")),
                            sliderInput(ns("taban_puan_c"), label = "2. Taban Puan (C_taban):", min = 0, max = 100, value = 25, post = " Puan"),
                            p(tags$small("'Güvenilmez Bölge'deki firmaların skorlarının çekileceği varsayılan düşük puandır. Bu, düşük hacimli firmaların haksız yere ödüllendirilmesini engeller.")),
                            sliderInput(ns("guvenilirlik_esigi"), label = "3. İstatistiksel Güvenilirlik Eşiği (m):", min = 0, max = 2500, value = 750, step = 50),
                            p(tags$small("Skorların hedef puana (Taban Puan veya Genel Ortalama) ne kadar şiddetle çekileceğini belirler. Yüksek değerler, 'çekim gücünü' artırır."))
               ),
               mainPanel(width=9, 
                         h3("Ağırlıklara ve Hacme Göre Ayarlanmış Firma Sıralaması"), 
                         DT::dataTableOutput(ns("simulator_tablosu")))
             )
    ),
    tabPanel("İlçe Karşılaştırma", icon = icon("map-marked-alt"),
             fluidRow(
               column(4, wellPanel(h4("Analiz Yapılacak Bölge"), uiOutput(ns("sehir_secimi_ui")), uiOutput(ns("ilce_secimi_ui")))),
               column(8, h3(textOutput(ns("oneri_basligi"))), fluidRow(column(4, wellPanel(h5("Genel En İyi"), h4(textOutput(ns("optimal_firma"))))), column(4, wellPanel(h5("En Hızlı"), h4(textOutput(ns("hizli_firma"))))), column(4, wellPanel(h5("En Sorunsuz"), h4(textOutput(ns("guvenilir_firma")))))))
             ), hr(), h3(textOutput(ns("detay_tablo_basligi"))), DT::dataTableOutput(ns("detay_tablosu"))
    ),
    tabPanel("Firma Karnesi", icon = icon("book"),
             sidebarLayout(
               sidebarPanel(width=3,
                            h4("Filtreleme Seçenekleri"),
                            uiOutput(ns("firma_secimi_ui")),
                            uiOutput(ns("firma_karne_sehir_ui")),
                            numericInput(ns("min_hacim_karne"), "Grafik için Min. Gönderi Sayısı:", value = 50, min = 1, step = 10),
                            p(tags$small("Grafikte sadece burada belirtilen sayıdan daha fazla gönderi hacmine sahip bölgeler gösterilir."))
               ),
               mainPanel(width=9,
                         h3(textOutput(ns("firma_karne_basligi"))),
                         div(id = ns("placeholder_karne_grafik"), br(), shimmer_placeholder(height = "450px")),
                         div(id = ns("placeholder_karne_tablo"), br(), shimmer_placeholder(height = "300px")),
                         shinyjs::hidden(
                           div(id = ns("content_karne_grafik"),
                               radioButtons(ns("karne_siralama_tipi"), "Grafik Gösterimi:", choices = c("En İyi Performans" = "iyi", "En Kötü Performans" = "kotu"), selected = "iyi", inline = TRUE),
                               plotOutput(ns("firma_karne_grafigi"), height = "450px")
                           )
                         ),
                         shinyjs::hidden(
                           div(id = ns("content_karne_tablo"),
                               hr(),
                               DT::dataTableOutput(ns("firma_karne_tablosu"))
                           )
                         ),
                         shinyjs::hidden(
                           div(id = ns("panel_karne_veri_yok"),
                               style = "text-align: center; padding-top: 50px; padding-bottom: 50px; border: 1px dashed #ccc; background-color: #f9f9f9;",
                               h4("Bu bölge için seçilmiş kargo şirketine ait veri bulunmamaktadır.")
                           )
                         )
               )
             )
    ),
    tabPanel("Marka Analizi", icon = icon("tags"),
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4("Filtreleme Seçenekleri"),
                            uiOutput(ns("marka_analizi_marka_filter_ui")),
                            uiOutput(ns("marka_analizi_il_filter_ui")),
                            uiOutput(ns("marka_analizi_ilce_filter_ui")),
                            p(tags$small(em("Burada seçeceğiniz markanın B2C gönderilerinin, dilerseniz belirli bir coğrafyaya göre filtrelenmiş, kargo firmalarıyla olan performansını karşılaştırabilirsiniz.")))
               ),
               mainPanel(width = 9,
                         h3(textOutput(ns("marka_analizi_baslik"))),
                         hr(),
                         DT::dataTableOutput(ns("marka_analizi_tablosu"))
               )
             )
    ),
    tabPanel("Şikayet Analizi", icon = icon("exclamation-triangle"),
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4("Filtreleme Seçenekleri"),
                            uiOutput(ns("sikayet_analizi_firma_filter_ui")),
                            uiOutput(ns("sikayet_analizi_sehir_filter_ui"))
               ),
               mainPanel(width = 9,
                         h3(textOutput(ns("sikayet_analizi_baslik"))),
                         conditionalPanel(condition = "output.show_sikayet_panel", ns = ns,
                                          plotOutput(ns("sikayet_analizi_grafigi"), height = "450px"),
                                          hr(),
                                          DT::dataTableOutput(ns("sikayet_analizi_tablosu"))
                         ),
                         conditionalPanel(condition = "!output.show_sikayet_panel", ns = ns,
                                          div(style = "text-align: center; padding-top: 50px; padding-bottom: 50px; border: 1px dashed #ccc; background-color: #f9f9f9;",
                                              h4("Bu filtreler için görüntülenecek şikayet verisi bulunmamaktadır."))
                         )
               )
             )
    ),
    tabPanel("Dinamik Karşılaştırma", icon = icon("exchange-alt"),
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4("1. Ana Dönemi Seçin (Eski Dönem)"),
                            dateRangeInput(ns("ana_donem_secimi"), label = NULL, start = floor_date(Sys.Date() %m-% months(2), "month"), end = ceiling_date(Sys.Date() %m-% months(2), "month") - 1, format = "dd-mm-yyyy", separator = " - ", language = "tr"),
                            hr(),
                            h4("2. Karşılaştırma Dönemini Seçin (Yeni Dönem)"),
                            dateRangeInput(ns("karsilastirma_donem_secimi"), label = NULL, start = floor_date(Sys.Date() %m-% months(1), "month"), end = ceiling_date(Sys.Date() %m-% months(1), "month") - 1, format = "dd-mm-yyyy", separator = " - ", language = "tr"),
                            hr(),
                            h4("3. Tabloya Eklenecek Metrikleri Seçin"),
                            selectInput(ns("secilen_metrikler"), label = NULL, choices = c("Hacim" = "toplam_gonderi_sayisi", "Ortalama Teslim Süresi (Saat)" = "ortalama_teslim_suresi", "Başarı Oranı" = "dinamik_basari_orani", "Şikayet Oranı (%)" = "sikayet_orani_yuzde"), multiple = TRUE, selected = "toplam_gonderi_sayisi"),
                            hr(), 
                            actionButton(ns("karsilastir_button"), "VERİLERİ KARŞILAŞTIR", icon = icon("exchange-alt"), class = "btn-success btn-lg btn-block")
               ),
               # === DEĞİŞİKLİK BURADA: ConditionalPanel'lar kaldırıldı. ===
               mainPanel(width = 9, 
                         h3("Firma Bazında Metrik Karşılaştırma Raporu"),
                         hr(),
                         # Sadece dataTableOutput bırakıldı.
                         DT::dataTableOutput(ns("karsilastirma_tablosu"))
               )
             )
    ),
    tabPanel("Aykırı Değer Raporu", icon = icon("chart-pie"),
             sidebarLayout(
               sidebarPanel(width = 4,
                            wellPanel(
                              h4("Aykırı Değer Analizi"),
                              radioButtons(ns("aykiri_analiz_modu"), "Analiz Modunu Seçin:", choices = c("Genel Özet" = "genel", "Firmaya Göre" = "firma", "Bölgeye Göre" = "bolge", "Firma Bazında Özet" = "firma_ozet"), selected = "genel"),
                              hr(),
                              conditionalPanel(condition = paste0("input['", ns("aykiri_analiz_modu"), "'] == 'firma'"), uiOutput(ns("firma_secim_ui_aykiri"))),
                              conditionalPanel(condition = paste0("input['", ns("aykiri_analiz_modu"), "'] == 'bolge'"), uiOutput(ns("il_secim_ui_aykiri")), uiOutput(ns("ilce_secim_ui_aykiri")))
                            )
               ),
               mainPanel(width = 8,
                         conditionalPanel(condition = paste0("input['", ns("aykiri_analiz_modu"), "'] != 'firma_ozet'"), plotOutput(ns("aykiri_pie_chart"), height = "500px")),
                         conditionalPanel(condition = paste0("input['", ns("aykiri_analiz_modu"), "'] == 'firma_ozet'"), h4(textOutput(ns("firma_ozet_basligi"))), hr(), DT::dataTableOutput(ns("aykiri_firma_ozet_tablosu")))
               )
             ),
             hr(), h3("Aykırı Değer Detay Tablosu"), p("Aşağıdaki tabloda, istatistiksel normların (teslimat süresi) dışında kaldığı için analizden çıkarılan gönderiler listelenmektedir."),
             DT::dataTableOutput(ns("aykiri_degerler_tablosu"))
    )
  )
}
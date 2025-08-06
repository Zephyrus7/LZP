# app.R - TAM HALİ (Buton Görünümü İyileştirildi ve Hatalar Düzeltildi)

#--- 1. MODÜLLERİ VE KONFİGÜRASYONU YÜKLE ---
source("00_Config.R")
source("00_DB_Connector.R")
source("01_B2C_Processor.R")
source("02_B2B_Processor.R")
source("ui_b2c.R")
source("server_b2c.R")
source("ui_b2b.R") 
source("server_b2b.R")

options(shiny.maxRequestSize = 500*1024^2)

#--- 2. KULLANICI ARAYÜZÜ (UI) ---
ui <- fluidPage(
  uiOutput("main_ui_placeholder")
)

#--- 3. SUNUCU MANTIĞI (SERVER) ---
server <- function(input, output, session) {
  
  # Session namespace'i alıyoruz, bu en iyi pratiktir.
  ns <- session$ns
  
  # ... (Uygulama başlangıç, login, db_date_range kodları aynı kalır) ...
  try({
    user_count <- dbGetQuery(db_pool, "SELECT COUNT(*) AS n FROM kullanicilar")
    if (user_count$n == 0) {
      cat("'kullanicilar' tablosu boş. Varsayılan admin kullanıcısı oluşturuluyor...\n")
      hashed_password <- bcrypt::hashpw("admin")
      insert_query <- "INSERT INTO kullanicilar (kullanici_adi, parola_hash, ad_soyad) VALUES (?, ?, ?)"
      dbExecute(db_pool, insert_query, params = list("admin", hashed_password, "Varsayılan Yönetici"))
      cat("Varsayılan Kullanıcı:\n  Kullanıcı Adı: admin\n  Şifre: admin\n")
    }
  })
  
  db_date_range <- tryCatch({
    query <- "SELECT MIN(son_hareket_tarihi) AS min_date, MAX(son_hareket_tarihi) AS max_date FROM gonderiler"
    range_df <- dbGetQuery(db_pool, query)
    if (is.na(range_df$min_date) || is.na(range_df$max_date)) {
      list(min_date = Sys.Date(), max_date = Sys.Date())
    } else {
      cat("Veritabanındaki mevcut veri aralığı:", format(as.Date(range_df$min_date), "%d-%m-%Y"), "-", format(as.Date(range_df$max_date), "%d-%m-%Y"), "\n")
      range_df
    }
  }, error = function(e) {
    list(min_date = Sys.Date(), max_date = Sys.Date())
  })
  
  rv <- reactiveValues(data = NULL, tip = NULL, active_tabs = character(0), user_authenticated = FALSE)
  
  login_dialog <- modalDialog(
    title = "Lojistik Zeka Platformu - Giriş",
    textInput(ns("login_username"), "Kullanıcı Adı"),
    passwordInput(ns("login_password"), "Şifre"),
    tags$script(HTML(sprintf("
      $(document).on('keyup', function(e) {
        if ($('#shiny-modal').is(':visible') && (e.which == 13)) {
          $('#%s').click();
        }
      });
    ", ns("login_button")))),
    footer = tagList(actionButton(ns("login_button"), "Giriş Yap", class = "btn-primary")),
    easyClose = FALSE
  )
  showModal(login_dialog)
  observeEvent(input$login_button, {
    req(rv$user_authenticated == FALSE)
    req(input$login_username, input$login_password)
    query <- "SELECT parola_hash FROM kullanicilar WHERE kullanici_adi = ?"
    user_data <- dbGetQuery(db_pool, query, params = list(input$login_username))
    if (nrow(user_data) == 1 && bcrypt::checkpw(input$login_password, user_data$parola_hash[1])) {
      rv$user_authenticated <- TRUE
      removeModal()
    } else {
      showNotification("Hatalı kullanıcı adı veya şifre.", type = "error", duration = 5)
    }
  })
  
  output$db_date_range_display <- renderText({
    req(db_date_range$min_date)
    min_date_formatted <- format(as.Date(db_date_range$min_date), "%d-%m-%Y")
    max_date_formatted <- format(as.Date(db_date_range$max_date), "%d-%m-%Y")
    paste("Mevcut veri", min_date_formatted, "ile", max_date_formatted, "arasını kapsamaktadır.")
  })
  
  output$main_ui_placeholder <- renderUI({
    req(rv$user_authenticated)
    navbarPage(
      id = "main_navbar",
      title = "Lojistik Zeka Platformu",
      theme = shinytheme("sandstone"),
      header = tagList(
        shinyjs::useShinyjs(),
        tags$head(tags$style(HTML("
          /* Mevcut stiller... */
          .navbar-default { background-color: #4A545C !important; border-color: #3E464D !important; } .navbar-default .navbar-brand { color: #ffffff; } .navbar-default .navbar-brand:hover, .navbar-default .navbar-brand:focus { color: #f1f1f1; } .navbar-default .navbar-nav > li > a { color: #d1d1d1; } .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus { color: #ffffff; background-color: #3E464D; } .navbar-default .navbar-nav > li > a:hover, .navbar-default .navbar-nav > li > a:focus { color: #ffffff; background-color: #5a626a; }
          .btn-loading { position: relative; opacity: 0.85; cursor: not-allowed !important; }
          .btn-loading::after { content: ''; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-image: linear-gradient(90deg, rgba(255,255,255,0) 0%, rgba(255,255,255,0.25) 25%, rgba(255,255,255,0.5) 50%, rgba(255,255,255,0.75) 75%, rgba(255,255,255,0.875) 100%); animation: shimmer 1.5s infinite; border-radius: inherit; }
          .shimmer-placeholder { background-color: #e9ecef; position: relative; overflow: hidden; border-radius: 4px; }
          .shimmer-placeholder::after { content: ''; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-image: linear-gradient(90deg, rgba(255,255,255,0) 0%, rgba(255,255,255,0.25) 25%, rgba(255,255,255,0.75) 50%, rgba(255,255,255,0.25) 70%, rgba(255,255,255,0) 100%); animation: shimmer 1.25s infinite; }
          @keyframes shimmer { 0% { transform: translateX(-100%); } 100% { transform: translateX(100%); } }
          
          /* =================================================================== */
          /*            >>> İLERLEME ÇUBUĞU BUTONU İÇİN STİLLER <<<              */
          /* =================================================================== */
          .btn-progress-container { 
            position: relative; 
            text-align: center; 
            color: white !important; 
            overflow: hidden; 
            text-transform: none !important; /* YENİ: Büyük harf dönüşümünü engeller */
          }
          .btn-progress-fill { 
            position: absolute; 
            left: 0; 
            top: 0; 
            height: 100%; 
            width: 0%; 
            background-color: rgba(0, 0, 0, 0.2); 
            transition: width 0.25s ease-in-out; 
          }
          .btn-progress-text { 
            position: relative; 
            z-index: 1; 
          }
        ")))
      ),
      tabPanel("Giriş ve Ayarlar", icon = icon("cog"),
               fluidRow(
                 column(4,
                        h3("Analiz Ayarları"),
                        wellPanel(
                          h4("1. Analiz Modunu Seçin"),
                          radioButtons(ns("analiz_modu"), label = NULL, choices = c("Statik Analiz" = "statik", "Canlı Analiz" = "canli"), selected = "statik", inline = TRUE),
                          hr(),
                          conditionalPanel( "input.analiz_modu == 'statik'", h4("2. Analiz Veri Kapsamını Seçin"), radioButtons(ns("statik_veri_secimi"), label = NULL, choices = c("Tüm Veri" = "tumu", "Tarih Aralığı Seç" = "tarih_sec"), selected = "tumu", inline = TRUE), p(tags$small(em(textOutput(ns("db_date_range_display"))))), conditionalPanel( condition = "input.statik_veri_secimi == 'tarih_sec'", dateRangeInput(ns("tarih_araligi"), label = "Başlangıç - Bitiş Tarihi", start = floor_date(Sys.Date(), "year"), end = Sys.Date(), format = "dd-mm-yyyy", language = "tr") ), hr(), h4("3. Analiz Tipini Seçin"), radioButtons(ns("analiz_tipi_statik"), label = NULL, choices = c("Bireysel (B2C)" = "B2C", "Kurumsal (B2B)" = "B2B"), inline = TRUE) ),
                          conditionalPanel( "input.analiz_modu == 'canli'", h4("2. Analiz Tipini Seçin"), radioButtons(ns("analiz_tipi_canli"), label = NULL, choices = c("Bireysel (B2C)" = "B2C", "Kurumsal (B2B)" = "B2B"), inline = TRUE), p(tags$small("Bu modül, veritabanındaki en güncel verileri kullanarak anlık bir analiz sunar.")) ),
                          hr(),
                          
                          # >>> DEĞİŞİKLİK: btn-lg kaldırıldı, style etiketi güncellendi
                          div(
                            id = ns("analiz_baslat_container"),
                            onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("analiz_baslat")),
                            class = "btn btn-primary btn-block btn-progress-container", 
                            style = "padding: 8px 12px; font-size: 15px; line-height: 3; border-radius: 6px;", 
                            div(class = "btn-progress-fill", id=ns("progress_fill")),
                            span(class = "btn-progress-text", id=ns("progress_text"), "Analizi Başlat")
                          )
                        )
                 ),
                 column(8, h3("Platforma Hoş Geldiniz!"), p("Bu platform, B2C ve B2B kargo operasyonlarınızı merkezi veritabanından analiz etmenizi sağlar."), tags$ol( tags$li("Sol taraftan bir analiz modu (Statik veya Canlı) seçin."), tags$li("Seçiminize göre beliren ayarları (veri kapsamı, tarih aralığı gibi) yapın ve analiz tipini belirleyin."), tags$li("'Analizi Başlat' butonuna tıklayarak seçtiğiniz modda analizi başlatın."), tags$li("Analiz tamamlandığında, sonuçları inceleyebileceğiniz yeni sekmeler eklenecektir.") ) )
               )
      )
    )
  })
  
  server_b2c("b2c_modul", reactive(if(req(rv$tip) == "B2C") rv$data))
  server_b2b("b2b_modul", reactive(if(req(rv$tip) == "B2B") rv$data))
  
  observeEvent(input$analiz_baslat, {
    req(rv$user_authenticated)
    
    # --- 1. BUTONU "ÇALIŞIYOR" MODUNA AL ---
    shinyjs::addClass(id = "analiz_baslat_container", class = "btn-loading") 
    shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'none');", ns("analiz_baslat_container"))) 
    shinyjs::html(id = "progress_text", html = "Başlatılıyor...") 
    shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("progress_fill")))
    
    # --- 2. İŞLEM BİTİNCE BUTONU ESKİ HALİNE GETİR ---
    on.exit({
      shinyjs::html(id = "progress_text", html = "Analizi Başlat")
      shinyjs::removeClass(id = "analiz_baslat_container", class = "btn-loading")
      shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'auto');", ns("analiz_baslat_container")))
      
      shinyjs::runjs(sprintf("$('#%s').css('width', '100%%');", ns("progress_fill")))
      Sys.sleep(0.5)
      shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("progress_fill")))
    })
    
    # --- 3. ÖNCEKİ SONUÇLARI TEMİZLE ---
    if(length(rv$active_tabs) > 0) { lapply(rv$active_tabs, function(tab_val) removeTab(inputId = "main_navbar", target = tab_val)); rv$active_tabs <- character(0) }
    rv$data <- NULL
    
    # --- 4. YENİ İLERLEME FONKSİYONUNU TANIMLA ---
    custom_progress_updater <- function(amount, detail = NULL) {
      if(!is.null(detail)){
        shinyjs::html(selector = paste0("#", ns("progress_text")), html = detail)
      }
      shinyjs::runjs(sprintf("$('#%s').css('width', '%f%%');", ns("progress_fill"), amount * 100))
    }
    
    # --- 5. ANALİZ SÜRECİNİ YENİ FONKSİYONLA ÇALIŞTIR ---
    if (input$analiz_modu == "statik") {
      local_start_date <- NULL
      local_end_date <- NULL
      if (input$statik_veri_secimi == 'tarih_sec') {
        req(input$tarih_araligi)
        local_start_date <- as.Date(input$tarih_araligi[1])
        local_end_date <- as.Date(input$tarih_araligi[2])
        db_min_date <- as.Date(db_date_range$min_date); db_max_date <- as.Date(db_date_range$max_date)
        if (local_start_date > db_max_date || local_end_date < db_min_date) {
          showModal(modalDialog(title = "Geçersiz Tarih Aralığı", p("Seçtiğiniz tarih aralığında veritabanında hiç veri bulunmamaktadır."), p(strong("Veritabanındaki mevcut veri aralığı:")), p(paste(format(db_min_date, "%d %B %Y"), "-", format(db_max_date, "%d %B %Y"))), footer = modalButton("Anladım")))
          return() 
        }
      } else { 
        local_start_date <- as.Date(db_date_range$min_date)
        local_end_date <- as.Date(db_date_range$max_date)
      }
      
      analiz_tipi <- input$analiz_tipi_statik
      analysis_result <- NULL
      if(analiz_tipi == "B2C") {
        rv$tip <- "B2C"
        analysis_result <- analiz_et_ve_skorla_b2c(db_pool = db_pool, start_date = local_start_date, end_date = local_end_date, progress_updater = custom_progress_updater)
      } else if (analiz_tipi == "B2B") {
        rv$tip <- "B2B"
        analysis_result <- analiz_et_ve_skorla_b2b(db_pool = db_pool, start_date = local_start_date, end_date = local_end_date, progress_updater = custom_progress_updater)
      }
      if (!is.null(analysis_result)) { 
        analysis_result$tarih_araligi <- c(local_start_date, local_end_date)
      }
      rv$data <- analysis_result
      
    } else if (input$analiz_modu == "canli") {
      showNotification("Canlı Analiz modu henüz geliştirme aşamasındadır.", type = "warning", duration = 8); return()
    }
    
    # --- 6. SONUÇLARI GÖSTER ---
    if (!is.null(rv$data)) {
      tab_list <- list()
      if (rv$tip == "B2C") { tab_list <- ui_b2c("b2c_modul") } else if (rv$tip == "B2B") { tab_list <- ui_b2b("b2b_modul") }
      lapply(tab_list, function(tab) appendTab(inputId = "main_navbar", tab, select = FALSE))
      rv$active_tabs <- c(rv$active_tabs, sapply(tab_list, function(t) t$attribs$title))
      if (rv$tip == "B2C") { updateNavbarPage(session, "main_navbar", selected = "Ağırlık Simülatörü") } else if (rv$tip == "B2B") { updateNavbarPage(session, "main_navbar", selected = "Kargo Firması Karnesi") }
      download_tab_value <- "download_tab"
      appendTab(inputId = "main_navbar", tabPanel(title = "Veri İndir", value = download_tab_value, icon = icon("download"), sidebarLayout(sidebarPanel(h4("İndirme Seçenekleri"), uiOutput(ns("download_options_ui")), hr(), downloadButton(ns("download_data_button"), "Seçilen Verileri İndir (.xlsx)", class="btn-success btn-block")), mainPanel(h3("Veri Raporlarını İndirin"), p("Sol taraftaki menüden indirmek istediğiniz raporları seçin.")))))
      rv$active_tabs <- c(rv$active_tabs, download_tab_value)
    }
  })
  
  # ... (Kodun geri kalan tüm kısımları, indirme fonksiyonları vb. aynıdır) ...
  generate_speed_comparison_report <- function(data) {
    b2b_turleri_to_exclude <- c("Mağazaya Teslim", "Mağazalar Arası Transfer", "B2B")
    base_data <- data %>% filter(!kargo_turu %in% b2b_turleri_to_exclude) %>% mutate(bolge = paste(sehir, ilce, sep=" - ")) %>% select(bolge, kargo_turu, ortalama_teslim_suresi, toplam_gonderi_sayisi)
    best_worst_performers <- base_data %>% group_by(bolge) %>% summarise(en_iyi_firma = kargo_turu[which.min(ortalama_teslim_suresi)], en_kotu_firma = kargo_turu[which.max(ortalama_teslim_suresi)], .groups = 'drop')
    regional_summary <- base_data %>% group_by(bolge) %>% summarise(toplam_gonderi = sum(toplam_gonderi_sayisi, na.rm = TRUE), genel_ortalama_hiz = weighted.mean(ortalama_teslim_suresi, toplam_gonderi_sayisi, na.rm = TRUE), .groups = 'drop')
    pivoted_data <- base_data %>% pivot_wider(id_cols = bolge, names_from = kargo_turu, values_from = c(toplam_gonderi_sayisi, ortalama_teslim_suresi), names_sep = "_", values_fill = list(toplam_gonderi_sayisi = 0, ortalama_teslim_suresi = NA))
    final_report <- pivoted_data %>% left_join(regional_summary, by = "bolge") %>% left_join(best_worst_performers, by = "bolge")
    firmalar <- unique(base_data$kargo_turu)
    firma_sutunlari_sirali <- unlist(lapply(firmalar, function(f) c(paste0("toplam_gonderi_sayisi_", f), paste0("ortalama_teslim_suresi_", f))))
    final_report <- final_report %>% select(`Satır Etiketleri` = bolge, all_of(firma_sutunlari_sirali), `Say Kargo No Toplamı` = toplam_gonderi, `Ortalama Toplam Teslim Süresi (Saat) Toplamı` = genel_ortalama_hiz, `EN İYİ TESLİMAT SÜRESİNE SAHİP FİRMA` = en_iyi_firma, `EN KÖTÜ TESLİMAT SÜRESİNE SAHİP FİRMA` = en_kotu_firma)
    return(final_report)
  }
  output$download_options_ui <- renderUI({
    req(rv$user_authenticated, rv$tip)
    choices_list <- if(rv$tip == "B2C") { c("Temel Analiz Sonuçları ve Skorlar" = "b2c_sonuclar", "Aykırı Değer Raporu" = "b2c_aykiri", "Bölgesel Hız Karşılaştırma Raporu" = "b2c_hiz_raporu") } else { c("B2B Ana Analiz Verisi" = "b2b_main") }
    checkboxGroupInput(ns("download_choices"), "İndirilecek Raporları Seçin:", choices = choices_list, selected = choices_list)
  })
  output$download_data_button <- downloadHandler(
    filename = function() { req(rv$user_authenticated); paste0("Lojistik_Raporu_", rv$tip, "_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(rv$user_authenticated, input$download_choices)
      list_of_datasets <- list()
      if(rv$tip == "B2C") {
        if ("b2c_sonuclar" %in% input$download_choices) list_of_datasets[["Analiz_ve_Skorlar"]] <- rv$data$sonuclar
        if ("b2c_aykiri" %in% input$download_choices) list_of_datasets[["Aykırı_Değerler"]] <- rv$data$aykiri_degerler
        if ("b2c_hiz_raporu" %in% input$download_choices) list_of_datasets[["Bolgesel_Hiz_Karsilastirma"]] <- generate_speed_comparison_report(rv$data$sonuclar)
      } else {
        if ("b2b_main" %in% input$download_choices) list_of_datasets[["B2B_Ana_Veri"]] <- rv$data$main_data
      }
      write.xlsx(list_of_datasets, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
}

#--- 4. UYGULAMAYI BAŞLAT ---
shinyApp(ui = ui, server = server)
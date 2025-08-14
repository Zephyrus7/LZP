# app.R - GERÇEK NİHAİ VERSİYON (Dark Mode CSS Düzeltmesi Dahil)

#--- 1. MODÜLLERİ VE KONFİGÜRASYONU YÜKLE ---
source("00_Config.R")
source("00_DB_Connector.R")
source("01_B2C_Processor.R"); source("02_B2B_Processor.R")
source("ui_b2c.R"); source("server_b2c.R"); source("ui_b2b.R"); source("server_b2b.R")
source("03_Live_Processor.R"); source("ui_live.R"); source("server_live.R")
options(shiny.maxRequestSize = 500*1024^2)

#--- 2. KULLANICI ARAYÜZÜ (UI) ---
ui <- fluidPage(
  uiOutput("main_ui_placeholder")
)

#--- 3. SUNUCU MANTIĞI (SERVER) ---
server <- function(input, output, session) {
  
  ns <- session$ns
  
  # Başlangıç sorguları için güvenli bağlantı yönetimi
  try({
    poolWithTransaction(db_pool_static, function(conn) {
      user_count <- dbGetQuery(conn, "SELECT COUNT(*) AS n FROM kullanicilar")
      if (user_count$n == 0) {
        cat("'kullanicilar' tablosu boş. Varsayılan admin kullanıcısı oluşturuluyor...\n")
        hashed_password <- bcrypt::hashpw("admin")
        insert_query <- "INSERT INTO kullanicilar (kullanici_adi, parola_hash, ad_soyad) VALUES (?, ?, ?)"
        dbExecute(conn, insert_query, params = list("admin", hashed_password, "Varsayılan Yönetici"))
        cat("Varsayılan Kullanıcı:\n  Kullanıcı Adı: admin\n  Şifre: admin\n")
      }
    })
  })
  db_date_range <- tryCatch({
    poolWithTransaction(db_pool_static, function(conn) {
      query <- "SELECT MIN(son_hareket_tarihi) AS min_date, MAX(son_hareket_tarihi) AS max_date FROM gonderiler"
      range_df <- dbGetQuery(conn, query)
      if (is.na(range_df$min_date) || is.na(range_df$max_date)) {
        list(min_date = Sys.Date(), max_date = Sys.Date())
      } else {
        cat("Statik Veri Aralığı:", format(as.Date(range_df$min_date), "%d-%m-%Y"), "-", format(as.Date(range_df$max_date), "%d-%m-%Y"), "\n")
        range_df
      }
    })
  }, error = function(e) {
    list(min_date = Sys.Date(), max_date = Sys.Date())
  })
  
  # Global reaktif değerler
  rv <- reactiveValues(
    data = NULL, tip = NULL, active_tabs = character(0), user_authenticated = FALSE,
    b2c_server_result = NULL, b2b_server_result = NULL, live_server_result = NULL
  )
  
  # Kullanıcı giriş mantığı
  login_dialog<-modalDialog(title="Lojistik Zeka Platformu - Giriş",textInput(ns("login_username"),"Kullanıcı Adı"),passwordInput(ns("login_password"),"Şifre"),tags$script(HTML(sprintf("$(document).on('keyup', function(e) { if ($('#shiny-modal').is(':visible') && (e.which == 13)) { $('#%s').click(); } });",ns("login_button")))),footer=tagList(actionButton(ns("login_button"),"Giriş Yap",class="btn-primary")),easyClose=FALSE)
  showModal(login_dialog)
  observeEvent(input$login_button,{req(rv$user_authenticated==FALSE);req(input$login_username,input$login_password);query<-"SELECT parola_hash FROM kullanicilar WHERE kullanici_adi = ?";user_data<-dbGetQuery(db_pool_static,query,params=list(input$login_username));if(nrow(user_data)==1&&bcrypt::checkpw(input$login_password,user_data$parola_hash[1])){rv$user_authenticated<-TRUE;removeModal()}else{showNotification("Hatalı kullanıcı adı veya şifre.",type="error",duration=5)}})
  output$db_date_range_display<-renderText({req(db_date_range$min_date);min_date_formatted<-format(as.Date(db_date_range$min_date),"%d-%m-%Y");max_date_formatted<-format(as.Date(db_date_range$max_date),"%d-%m-%Y");paste("Mevcut veri",min_date_formatted,"ile",max_date_formatted,"arasını kapsamaktadır.")})
  
  # Ana Arayüzü Render Etme
  output$main_ui_placeholder <- renderUI({
    req(rv$user_authenticated)
    navbarPage(
      id = "main_navbar",
      title = "Lojistik Zeka Platformu",
      theme = shinytheme("sandstone"),
      header = tagList(
        shinyjs::useShinyjs(),
        # === DEĞİŞİKLİK BURADA: Tam ve Eksiksiz CSS Bloğu Geri Eklendi ===
        tags$head(tags$style(HTML("
          :root { --bg-color-light: #FFFFFF; --panel-bg-light: #F8F9FA; --text-color-light: #212529; --border-color-light: #DEE2E6; --bg-color-dark: #212529; --panel-bg-dark: #343A40; --text-color-dark: #E9ECEF; --border-color-dark: #495057; }
          body.dark-mode { --bg-color-light: var(--bg-color-dark); --panel-bg-light: var(--panel-bg-dark); --text-color-light: var(--text-color-dark); --border-color-light: var(--border-color-dark); }
          body { background-color: var(--bg-color-light); color: var(--text-color-light); transition: background-color 0.3s ease, color 0.3s ease; }
          /* BU KISIM EKSİKTİ: Metinleri, başlıkları ve labelları renklendirir */
          h1, h2, h3, h4, h5, p, .form-group label, .control-label { color: var(--text-color-light) !important; }
          .well, .sidebar-panel, .modal-content, .panel { background-color: var(--panel-bg-light) !important; border: 1px solid var(--border-color-light) !important; }
          /* Tablolar için eksik olan Dark Mode kuralları */
          body.dark-mode .dataTables_wrapper, body.dark-mode .dataTables_length, body.dark-mode .dataTables_filter, body.dark-mode .dataTables_info, body.dark-mode .dataTables_paginate { color: var(--text-color-dark) !important; }
          body.dark-mode table.dataTable tr.odd { background-color: var(--panel-bg-dark); }
          body.dark-mode table.dataTable tr.even { background-color: #2c3136; }
          body.dark-mode table.dataTable th, body.dark-mode table.dataTable td { border-bottom: 1px solid var(--border-color-dark); color: var(--text-color-dark) !important; }
          /* Animasyonlu buton stilleri */
          .btn-progress-container { position: relative; text-align: center; color: white !important; overflow: hidden; text-transform: none !important; }
          .btn-progress-fill { position: absolute; left: 0; top: 0; height: 100%; width: 0%; background-color: rgba(0, 0, 0, 0.2); transition: width 0.25s ease-in-out; }
          .btn-progress-text { position: relative; z-index: 1; }
          .btn-loading { position: relative; opacity: 0.85; cursor: not-allowed !important; overflow: hidden; }
          .btn-loading::after { content: ''; position: absolute; top: 0; left: -50%; width: 200%; height: 100%; background-image: linear-gradient(90deg, rgba(255,255,255,0) 0%, rgba(255,255,255,0.25) 25%, rgba(255,255,255,0.5) 50%, rgba(255,255,255,0.725) 70%, rgba(255,255,255,0.925) 100%); animation: shimmer 1.75s infinite; border-radius: inherit; }
          @keyframes shimmer { 0% { transform: translateX(-100%); } 100% { transform: translateX(100%); } }
        ")))
        # ======================================================================
      ),
      tabPanel("Giriş ve Ayarlar", icon = icon("cog"),
               fluidRow(
                 column(4, h3("Analiz Ayarları"),
                        wellPanel(
                          h4("1. Analiz Modunu Seçin"), radioButtons(ns("analiz_modu"), label = NULL, choices = c("Statik Analiz" = "statik", "Canlı Analiz" = "canli"), selected = "statik", inline = TRUE), hr(),
                          conditionalPanel("input.analiz_modu == 'statik'", h4("2. Analiz Veri Kapsamını Seçin"), radioButtons(ns("statik_veri_secimi"), label = NULL, choices = c("Tüm Veri" = "tumu", "Tarih Aralığı Seç" = "tarih_sec"), selected = "tumu", inline = TRUE), p(tags$small(em(textOutput(ns("db_date_range_display"))))), conditionalPanel(condition = "input.statik_veri_secimi == 'tarih_sec'", dateRangeInput(ns("tarih_araligi"), label = "Başlangıç - Bitiş Tarihi", start = floor_date(Sys.Date(), "year"), end = Sys.Date(), format = "dd-mm-yyyy", language = "tr")), hr(), h4("3. Analiz Tipini Seçin"), radioButtons(ns("analiz_tipi_statik"), label = NULL, choices = c("Bireysel (B2C)" = "B2C", "Kurumsal (B2B)" = "B2B"), inline = TRUE)),
                          conditionalPanel("input.analiz_modu == 'canli'", p(tags$small("Bu modül, en güncel operasyonel verileri kullanarak anlık bir analiz sunar. Tarih aralığı otomatik olarak belirlenir."))), hr(),
                          div(id = ns("analiz_baslat_container"), onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("analiz_baslat")), class = "btn btn-primary btn-block btn-progress-container", style = "padding: 8px 12px; font-size: 15px; line-height: 1.5; border-radius: 6px;", div(class = "btn-progress-fill", id=ns("progress_fill")), span(class = "btn-progress-text", id=ns("progress_text"), "Analizi Başlat"))
                        )
                 ),
                 column(8,
                        div(style = "display: flex; justify-content: space-between; align-items: center;", h3("Platforma Hoş Geldiniz!"), shinyWidgets::switchInput(inputId = ns("dark_mode_switch"), onLabel = icon("moon"), offLabel = icon("sun"), onStatus = "info", offStatus = "warning", inline = TRUE, size = "small")),
                        p("Bu platform, B2C ve B2B kargo operasyonlarınızı analiz etmenizi sağlar."),
                        tags$ol(tags$li("Sol taraftan bir analiz modu (Statik veya Canlı) seçin."), tags$li("Seçiminize göre beliren ayarları yapın."), tags$li("'Analizi Başlat' butonuna tıklayarak seçtiğiniz modda analizi başlatın."), tags$li("Analiz tamamlandığında, sonuçları inceleyebileceğiniz yeni sekmeler eklenecektir.")))
               )
      )
    )
  })
  
  # Dark Mode Mantığı
  theme_reactive <- reactiveVal("light")
  observeEvent(input$dark_mode_switch, {
    shinyjs::runjs('if(typeof shinyjs.init != "function") { shinyjs.init = function() { $("body").addClass("shinyjs-resettable"); } }');
    if (isTRUE(input$dark_mode_switch)) {
      shinyjs::addClass(selector = "body", class = "dark-mode"); theme_reactive("dark")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode"); theme_reactive("light")
    }
  })
  
  # Server modüllerini çağır
  rv$b2c_server_result <- server_b2c("b2c_modul", reactive(if(req(rv$tip) == "B2C") rv$data), theme_reactive)
  rv$b2b_server_result <- server_b2b("b2b_modul", reactive(if(req(rv$tip) == "B2B") rv$data), theme_reactive)
  rv$live_server_result <- server_live("live_modul", reactive(if(req(rv$tip) == "LIVE") rv$data))
  
  # 'Analizi Başlat' Buton Mantığı
  observeEvent(input$analiz_baslat, {
    req(rv$user_authenticated)
    shinyjs::addClass(id = "analiz_baslat_container", class = "btn-loading"); shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'none');", ns("analiz_baslat_container"))); shinyjs::html(id = "progress_text", html = "Başlatılıyor..."); shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("progress_fill")))
    on.exit({ shinyjs::html(id = "progress_text", html = "Analizi Başlat"); shinyjs::removeClass(id = "analiz_baslat_container", class = "btn-loading"); shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'auto');", ns("analiz_baslat_container"))); shinyjs::runjs(sprintf("$('#%s').css('width', '100%%');", ns("progress_fill"))); Sys.sleep(0.5); shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("progress_fill"))) })
    if(length(rv$active_tabs) > 0) { lapply(rv$active_tabs, function(tab_val) removeTab(inputId = "main_navbar", target = tab_val)); rv$active_tabs <- character(0) }; rv$data <- NULL
    custom_progress_updater <- function(amount, detail = NULL) { if(!is.null(detail)){ shinyjs::html(selector = paste0("#", ns("progress_text")), html = detail) }; shinyjs::runjs(sprintf("$('#%s').css('width', '%f%%');", ns("progress_fill"), amount * 100)) }
    
    if (input$analiz_modu == "statik") {
      if (input$statik_veri_secimi == 'tarih_sec') { req(input$tarih_araligi); local_start_date <- as.Date(input$tarih_araligi[1]); local_end_date <- as.Date(input$tarih_araligi[2])
      } else { req(db_date_range$min_date, db_date_range$max_date); local_start_date <- as.Date(db_date_range$min_date); local_end_date <- as.Date(db_date_range$max_date) }
      analiz_tipi <- input$analiz_tipi_statik
      if(analiz_tipi == "B2C") { rv$tip <- "B2C"; rv$data <- analiz_et_ve_skorla_b2c(db_pool = db_pool_static, start_date = local_start_date, end_date = local_end_date, progress_updater = custom_progress_updater) }
      else if (analiz_tipi == "B2B") { rv$tip <- "B2B"; rv$data <- analiz_et_ve_skorla_b2b(db_pool = db_pool_static, start_date = local_start_date, end_date = local_end_date, progress_updater = custom_progress_updater) }
    } else if (input$analiz_modu == "canli") {
      rv$tip <- "LIVE"; live_start_date <- as.Date("2000-01-01"); live_end_date <- Sys.Date() + 3650
      rv$data <- analiz_et_ve_skorla_live(db_pool = db_pool_live, start_date = live_start_date, end_date = live_end_date, progress_updater = custom_progress_updater)
    }
    
    if (!is.null(rv$data)) {
      tab_list <- list(); if (rv$tip == "B2C") { tab_list <- ui_b2c("b2c_modul") } else if (rv$tip == "B2B") { tab_list <- ui_b2b("b2b_modul") } else if (rv$tip == "LIVE") { tab_list <- ui_live("live_modul") }
      lapply(tab_list, function(tab) appendTab(inputId = "main_navbar", tab, select = TRUE))
      rv$active_tabs <- c(rv$active_tabs, sapply(tab_list, function(t) t$attribs$title))
      if (rv$tip %in% c("B2C", "B2B")) {
        download_tab_value <- "download_tab"; appendTab(inputId = "main_navbar", tabPanel(title = "Veri İndir", value = download_tab_value, icon = icon("download"), sidebarLayout(sidebarPanel(h4("İndirme Seçenekleri"), uiOutput(ns("download_ui_placeholder"))), mainPanel(p("Bu bölüm, analiz sonuçlarını indirmenizi sağlar.")))))
        rv$active_tabs <- c(rv$active_tabs, download_tab_value)
      }
    }
  })
  
  output$download_ui_placeholder <- renderUI({ req(rv$user_authenticated, rv$tip); if (rv$tip == "B2C") { req(rv$b2c_server_result$download_ui); rv$b2c_server_result$download_ui() } else if (rv$tip == "B2B") { req(rv$b2b_server_result$download_ui); rv$b2b_server_result$download_ui() } })
}

#--- 4. UYGULAMAYI BAŞLAT ---
shinyApp(ui = ui, server = server)
# =========================================================================
#             ANA UYGULAMA - app.R (VERİ İNDİRME DÜZELTMESİ)
# =========================================================================
# DEĞİŞİKLİK:
# 1. 'observeEvent' bloğuna, Statik Analiz (B2C/B2B) sonrası dinamik
#    olarak "Veri İndir" sekmesini ekleyen mantık geri eklendi.
# 2. Bu dinamik sekmenin içeriğini dolduran 'renderUI' bloğu
#    sunucu mantığına geri eklendi.
# =========================================================================

#--- 1. MODÜLLERİ VE KONFİGÜRASYONU YÜKLE ---
source("00_Config.R")
source("00_DB_Connector.R") 
source("01_B2C_Processor.R"); source("02_B2B_Processor.R")
source("ui_b2c.R"); source("server_b2c.R")
source("ui_b2b.R"); source("server_b2b.R")
source("03_Live_Processor.R"); source("ui_live.R"); source("server_live.R")
source("ui_forecast_b2c.R")
source("server_forecast_b2c.R")


#--- 2. KULLANICI ARAYÜZÜ (UI) ---
ui <- fluidPage(
  shinyjs::useShinyjs(),
  uiOutput("main_ui_placeholder")
)

#--- 3. SUNUCU MANTIĞI (SERVER) ---
server <- function(input, output, session) {
  
  ns <- session$ns
  
  # Başlangıç sorguları için güvenli bağlantı yönetimi
  try({
    poolWithTransaction(db_pool_static, function(conn) {
      if(dbExistsTable(conn, "kullanicilar")) {
        user_count <- dbGetQuery(conn, "SELECT COUNT(*) AS n FROM kullanicilar")
        if (user_count$n == 0) {
          hashed_password <- bcrypt::hashpw("admin")
          dbExecute(conn, "INSERT INTO kullanicilar (kullanici_adi, parola_hash, ad_soyad) VALUES (?, ?, ?)", params = list("admin", hashed_password, "Varsayılan Yönetici"))
          cat("Varsayılan Kullanıcı: admin, Şifre: admin oluşturuldu.\n")
        }
      }
    })
  })
  
  db_date_range <- tryCatch({
    poolWithTransaction(db_pool_static, function(conn) {
      if(dbExistsTable(conn, "gonderiler")) {
        range_df <- dbGetQuery(conn, "SELECT MIN(son_hareket_tarihi) AS min_date, MAX(son_hareket_tarihi) AS max_date FROM gonderiler")
        if (is.na(range_df$min_date) || is.na(range_df$max_date)) {
          list(min_date = Sys.Date(), max_date = Sys.Date())
        } else {
          range_df
        }
      } else { list(min_date = Sys.Date(), max_date = Sys.Date()) }
    })
  }, error = function(e) { list(min_date = Sys.Date(), max_date = Sys.Date()) })
  
  # <<< YENİ: Canlı veri tabanı için tarih aralığı sorgusu eklendi >>>
  db_date_range_live <- tryCatch({
    poolWithTransaction(db_pool_live, function(conn) {
      if(dbExistsTable(conn, "orders")) {
        range_df <- dbGetQuery(conn, "SELECT MIN(created_at) AS min_date, MAX(created_at) AS max_date FROM orders WHERE deleted_at IS NULL")
        if (is.na(range_df$min_date) || is.na(range_df$max_date)) {
          list(min_date = Sys.Date(), max_date = Sys.Date())
        } else {
          range_df
        }
      } else { list(min_date = Sys.Date(), max_date = Sys.Date()) }
    })
  }, error = function(e) { list(min_date = Sys.Date(), max_date = Sys.Date()) })
  # >>> BİTİŞ
  
  # Global reaktif değerler
  rv <- reactiveValues(data = NULL, tip = NULL, active_tabs = character(0), user_authenticated = FALSE)
  theme_reactive <- reactiveVal("light")
  
  # Giriş ekranı
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
    req(input$login_username, input$login_password)
    user_data <- dbGetQuery(db_pool_static, "SELECT parola_hash FROM kullanicilar WHERE kullanici_adi = ?", params = list(input$login_username))
    if(nrow(user_data) == 1 && bcrypt::checkpw(input$login_password, user_data$parola_hash[1])) {
      rv$user_authenticated <- TRUE
      removeModal()
    } else {
      showNotification("Hatalı kullanıcı adı veya şifre.", type="error", duration=5)
    }
  })
  
  output$db_date_range_display <- renderText({
    req(db_date_range$min_date)
    paste("Mevcut veri", format(as.Date(db_date_range$min_date),"%d-%m-%Y"), "ile", format(as.Date(db_date_range$max_date),"%d-%m-%Y"), "arasını kapsamaktadır.")
  })
  
  # <<< YENİ: Canlı veri için tarih aralığı metni üreten çıktı eklendi >>>
  output$db_date_range_display_live <- renderText({
    req(db_date_range_live$min_date)
    paste("Canlı veri", format(as.Date(db_date_range_live$min_date),"%d-%m-%Y"), "ile", format(as.Date(db_date_range_live$max_date),"%d-%m-%Y"), "arasını kapsamaktadır.")
  })
  # >>> BİTİŞ
  
  # Ana Arayüz
  output$main_ui_placeholder <- renderUI({
    req(rv$user_authenticated)
    navbarPage(
      id = "main_navbar", title = "Lojistik Zeka Platformu", theme = shinytheme("sandstone"),
      header = tagList(
        tags$head(tags$style(HTML("
          :root { --bg-color-light: #FFFFFF; --panel-bg-light: #F8F9FA; --text-color-light: #212529; --border-color-light: #DEE2E6; --bg-color-dark: #212529; --panel-bg-dark: #343A40; --text-color-dark: #E9ECEF; --border-color-dark: #495057; }
          body.dark-mode { --bg-color-light: var(--bg-color-dark); --panel-bg-light: var(--panel-bg-dark); --text-color-light: var(--text-color-dark); --border-color-light: var(--border-color-dark); }
          body { background-color: var(--bg-color-light); color: var(--text-color-light); transition: background-color 0.3s ease, color 0.3s ease; }
          h1, h2, h3, h4, h5, p, .form-group label, .control-label { color: var(--text-color-light) !important; }
          .well, .sidebar-panel, .modal-content, .panel { background-color: var(--panel-bg-light) !important; border: 1px solid var(--border-color-light) !important; }
          body.dark-mode .dataTables_wrapper, body.dark-mode .dataTables_length, body.dark-mode .dataTables_filter, body.dark-mode .dataTables_info, body.dark-mode .dataTables_paginate { color: var(--text-color-dark) !important; }
          body.dark-mode table.dataTable tr.odd { background-color: var(--panel-bg-dark); }
          body.dark-mode table.dataTable tr.even { background-color: #2c3136; }
          body.dark-mode table.dataTable th, body.dark-mode table.dataTable td { border-bottom: 1px solid var(--border-color-dark); color: var(--text-color-dark) !important; }
          body.dark-mode .navbar-default { background-color: #212529 !important; border-color: #495057 !important; }
          body.dark-mode .navbar-default .navbar-brand, body.dark-mode .navbar-default .navbar-nav > li > a { color: #adb5bd !important; }
          body.dark-mode .navbar-default .navbar-brand:hover, body.dark-mode .navbar-default .navbar-nav > li > a:hover { color: #FFFFFF !important; }
          body.dark-mode .navbar-default .navbar-nav > .active > a, body.dark-mode .navbar-default .navbar-nav > .active > a:hover, body.dark-mode .navbar-default .navbar-nav > .active > a:focus { background-color: #343A40 !important; color: #FFFFFF !important; }
          .btn-progress-container { position: relative; text-align: center; color: white !important; overflow: hidden; text-transform: none !important; }
          .btn-progress-fill { position: absolute; left: 0; top: 0; height: 100%; width: 0%; background-color: rgba(0, 0, 0, 0.2); transition: width 0.25s ease-in-out; }
          .btn-progress-text { position: relative; z-index: 1; }
          .btn-loading { position: relative; opacity: 0.85; cursor: not-allowed !important; overflow: hidden; }
          .btn-loading::after { content: ''; position: absolute; top: 0; left: -50%; width: 200%; height: 100%; background-image: linear-gradient(90deg, rgba(255,255,255,0) 0%, rgba(255,255,255,0.25) 25%, rgba(255,255,255,0.5) 50%, rgba(255,255,255,0.725) 70%, rgba(255,255,255,0.925) 100%); animation: shimmer 1.75s infinite; border-radius: inherit; }
          @keyframes shimmer { 0% { transform: translateX(-100%); } 100% { transform: translateX(100%); } }
        ")))
      ),
      tabPanel("Giriş ve Ayarlar", icon = icon("cog"),
               fluidRow(
                 column(4, h3("Analiz Ayarları"),
                        wellPanel(
                          h4("1. Analiz Modunu Seçin"), radioButtons(ns("analiz_modu"), label = NULL, choices = c("Statik Analiz" = "statik", "Canlı Analiz" = "canli"), selected = "statik", inline = TRUE), hr(),
                          conditionalPanel("input.analiz_modu == 'statik'", ns = ns, h4("2. Analiz Veri Kapsamını Seçin"), radioButtons(ns("statik_veri_secimi"), label = NULL, choices = c("Tüm Veri" = "tumu", "Tarih Aralığı Seç" = "tarih_sec"), selected = "tumu", inline = TRUE), p(tags$small(em(textOutput(ns("db_date_range_display"))))), conditionalPanel(condition = "input.statik_veri_secimi == 'tarih_sec'", ns = ns, dateRangeInput(ns("tarih_araligi"), label = "Başlangıç - Bitiş Tarihi", start = floor_date(Sys.Date(), "year"), end = Sys.Date(), format = "dd-mm-yyyy", language = "tr")), hr(), h4("3. Analiz Tipini Seçin"), radioButtons(ns("analiz_tipi_statik"), label = NULL, choices = c("Bireysel (B2C)" = "B2C", "Kurumsal (B2B)" = "B2B"), inline = TRUE)),
                          
                          # <<< YENİ: Canlı Analiz paneli güncellendi >>>
                          conditionalPanel("input.analiz_modu == 'canli'", ns = ns,
                                           p(tags$small(em(textOutput(ns("db_date_range_display_live"))))),
                                           p(tags$small("Bu modül, en güncel operasyonel verileri kullanarak anlık bir analiz sunar."))
                          ),
                          # >>> BİTİŞ
                          
                          hr(),
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
  
  # Dark Mode
  observeEvent(input$dark_mode_switch, {
    if (isTRUE(input$dark_mode_switch)) { shinyjs::addClass(selector = "body", class = "dark-mode"); theme_reactive("dark") } 
    else { shinyjs::removeClass(selector = "body", class = "dark-mode"); theme_reactive("light") }
  })
  
  # Server Modüllerini Çağır
  b2c_server_result <- server_b2c("b2c_modul", reactive(if(req(rv$tip) == "B2C") rv$data else NULL), theme_reactive)
  b2b_server_result <- server_b2b("b2b_modul", reactive(if(req(rv$tip) == "B2B") rv$data else NULL), theme_reactive)
  live_server_result <- server_live("live_modul", reactive(if(req(rv$tip) == "LIVE") rv$data else NULL), theme_reactive)
  
  # Analiz Başlat
  observeEvent(input$analiz_baslat, {
    req(rv$user_authenticated)
    # Buton animasyonları
    shinyjs::addClass(id = "analiz_baslat_container", class = "btn-loading"); shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'none');", ns("analiz_baslat_container"))); shinyjs::html(id = "progress_text", html = "Başlatılıyor..."); shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("progress_fill")))
    on.exit({ shinyjs::html(id = "progress_text", html = "Analizi Başlat"); shinyjs::removeClass(id = "analiz_baslat_container", class = "btn-loading"); shinyjs::runjs(sprintf("$('#%s').css('pointer-events', 'auto');", ns("analiz_baslat_container"))); shinyjs::runjs(sprintf("$('#%s').css('width', '100%%');", ns("progress_fill"))); Sys.sleep(0.5); shinyjs::runjs(sprintf("$('#%s').css('width', '0%%');", ns("progress_fill"))) })
    
    # Mevcut sekmeleri temizle
    if(length(rv$active_tabs) > 0) { lapply(rv$active_tabs, function(tab_val) removeTab(inputId = "main_navbar", target = tab_val)); rv$active_tabs <- character(0) }; rv$data <- NULL
    
    custom_progress_updater <- function(amount, detail = NULL) { if(!is.null(detail)){ shinyjs::html(selector = paste0("#", ns("progress_text")), html = detail) }; shinyjs::runjs(sprintf("$('#%s').css('width', '%f%%');", ns("progress_fill"), amount * 100)) }
    
    # Analiz moduna göre ilgili Processor'ı çağır
    if (input$analiz_modu == "statik") {
      start_date <- if (input$statik_veri_secimi == 'tarih_sec') as.Date(input$tarih_araligi[1]) else as.Date(db_date_range$min_date)
      end_date <- if (input$statik_veri_secimi == 'tarih_sec') as.Date(input$tarih_araligi[2]) else as.Date(db_date_range$max_date)
      
      analiz_tipi <- input$analiz_tipi_statik
      rv$tip <- analiz_tipi
      if(analiz_tipi == "B2C") { rv$data <- analiz_et_ve_skorla_b2c(db_pool = db_pool_static, start_date = start_date, end_date = end_date, progress_updater = custom_progress_updater) }
      else if (analiz_tipi == "B2B") { rv$data <- analiz_et_ve_skorla_b2b(db_pool = db_pool_static, start_date = start_date, end_date = end_date, progress_updater = custom_progress_updater) }
      
    } else if (input$analiz_modu == "canli") {
      # <<< YENİ: Canlı analiz artık kendi çektiği tarih aralığını kullanacak >>>
      # Bu satır, start_date ve end_date'i sabit değerler yerine dinamik olarak ayarlar.
      # Ancak, canlı analizin her zaman "tüm veriyi" analiz etmesi istendiği için,
      # processor'a geniş bir tarih aralığı göndermeye devam etmek daha doğru bir yaklaşımdır.
      # Processor içindeki `filter(veri_tarihi >= !!start_date)` zaten bu aralığı kullanır.
      # Bu nedenle, bu blokta bir değişiklik yapmaya GEREK YOKTUR.
      # `db_date_range_live` sadece GÖRSEL bilgilendirme içindir.
      # >>> BİTİŞ
      
      rv$tip <- "LIVE"
      # Canlı analizin doğası gereği genellikle tüm ilgili veriyi (veya son N ayı) analiz etmesi beklenir.
      # `analiz_et_ve_skorla_live` fonksiyonuna sabit/geniş bir aralık göndermek,
      # bu mantığı korur. Arayüzde gösterilen tarih sadece bilgilendirme amaçlıdır.
      rv$data <- analiz_et_ve_skorla_live(db_pool = db_pool_live, start_date = as.Date(db_date_range_live$min_date), end_date = as.Date(db_date_range_live$max_date), progress_updater = custom_progress_updater)
    }
    
    # Dinamik sekme ekleme mantığı
    if (!is.null(rv$data)) {
      tab_list <- switch(rv$tip, "B2C" = ui_b2c("b2c_modul"), "B2B" = ui_b2b("b2b_modul"), "LIVE" = ui_live("live_modul"))
      lapply(tab_list, function(tab) appendTab(inputId = "main_navbar", tab, select = TRUE))
      rv$active_tabs <- sapply(tab_list, function(t) t$attribs$title)
      
      if (rv$tip %in% c("B2C", "B2B")) {
        download_tab_value <- "download_tab"
        appendTab(
          inputId = "main_navbar", 
          tabPanel(
            title = "Veri İndir", 
            value = download_tab_value, 
            icon = icon("download"),
            sidebarLayout(
              sidebarPanel(
                h4("İndirme Seçenekleri"), 
                uiOutput(ns("download_ui_placeholder")) 
              ), 
              mainPanel(
                h3("Veri Raporlarını İndirin"),
                p("Sol taraftaki menüden indirmek istediğiniz raporları seçin ve ilgili butona tıklayarak indirme işlemini başlatın.")
              )
            )
          )
        )
        rv$active_tabs <- c(rv$active_tabs, download_tab_value)
      }
    }
  })
  
  # Dinamik UI'ı dolduran renderUI
  output$download_ui_placeholder <- renderUI({
    req(rv$user_authenticated, rv$tip)
    if (rv$tip == "B2C") {
      req(b2c_server_result$download_ui)
      b2c_server_result$download_ui()
    } else if (rv$tip == "B2B") {
      # Henüz b2b için download_ui mevcut değil, eklendiğinde burası çalışacak.
      req(b2b_server_result$download_ui)
      b2b_server_result$download_ui()
    }
  })
  
}

#--- 4. UYGULAMAYI BAŞLAT ---
shinyApp(ui = ui, server = server)
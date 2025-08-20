# =========================================================================
#             B2C SERVER - ANA ORKESTRA ŞEFİ (NİHAİ DOĞRU YAPI)
# =========================================================================

server_b2c <- function(id, data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. TEMEL REAKTİF VERİLERİ ve DEĞİŞKENLERİ OLUŞTUR
    ana_veri <- reactive({ req(data()); data() })
    ana_veri_skorlari <- reactive({ 
      req(ana_veri())
      ana_veri()$sonuclar %>% 
        filter(!kargo_turu %in% c("Mağazaya Teslim", "Mağazalar Arası Transfer", "21")) 
    })
    aykiri_veriler <- reactive({ req(ana_veri()); ana_veri()$aykiri_degerler })
    ham_veri_temiz <- reactive({ req(ana_veri()); ana_veri()$ham_veri_temiz })
    karsilastirma_verisi <- reactiveVal(NULL)
    b2b_turleri <- c("Mağazaya Teslim", "Mağazalar Arası Transfer", "21")
    
    # 2. TÜM ALT MODÜLLERİ YEREL ORTAMA YÜKLE
    # `local = TRUE` sayesinde bu dosyaların içindeki kod, sanki buraya
    # kopyalanmış gibi çalışır ve tüm değişken/reaktifler aynı ortamda olur.
    source("server_b2c_reactives.R", local = TRUE)
    source("server_b2c_outputs.R", local = TRUE)
    source("server_b2c_observers.R", local = TRUE)
    
    # `server_b2c_downloads.R` dosyası bir fonksiyon (`register_b2c_downloads`) 
    # tanımladığı ve bir değer (`download_ui`) döndürdüğü için onu `source`
    # edip, dönen `value`'sunu (yani fonksiyonun kendisini) çağırıyoruz.
    download_ui_reactive <- source("server_b2c_downloads.R", local = TRUE)$value(
      output = output, 
      input = input, 
      session = session, 
      karsilastirma_verisi = karsilastirma_verisi
    )
    
    # 3. TAHMİNLEME MODÜLÜNÜ ÇAĞIR
    server_forecast_b2c(
      id = "forecast_b2c_modul", 
      processed_data = ana_veri,
      theme_reactive = theme_reactive
    )
    
    # 4. app.R'a GEREKLİ DEĞERİ DÖNDÜR
    return(
      list(
        download_ui = download_ui_reactive
      )
    )
    
  }) # moduleServer sonu
}
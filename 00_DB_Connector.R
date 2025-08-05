#==============================================================================#
#                 VERİTABANI BAĞLANTI YÖNETİCİSİ (Basit ve Stabil)             #
#==============================================================================#
# BU SCRIPT'İN TEK GÖREVİ, 'db_pool' ADINDA GEÇERLİ BİR BAĞLANTI HAVUZU
# OLUŞTURMAKTIR. UYGULAMAYA ÖZEL HİÇBİR MANTIK İÇERMEZ.
#==============================================================================#

#--- GEREKLİ KÜTÜPHANELERİ ÇAĞIR ---
library(pool)
library(config)
library(DBI)
library(RMariaDB)

#--- config.yml DOSYASINDAN AYARLARI OKU ---
tryCatch({
  db_config <- config::get(file = "config.yml", config = "default")$database
}, error = function(e) {
  stop("config.yml dosyası bulunamadı veya okunamadı! Hata: ", e$message)
})

#--- VERİTABANI BAĞLANTI HAVUZUNU (POOL) OLUŞTUR ---
db_pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  user = db_config$user,
  password = db_config$password,
  host = db_config$host,
  port = db_config$port,
  dbname = db_config$dbname
)

#--- UYGULAMA KAPANDIĞINDA BAĞLANTIYI GÜVENLE KAPAT ---
# Bu, sadece bir Shiny uygulaması çalışırken çalışır ve hata vermez.
if (shiny::isRunning()) {
  shiny::onStop(function() {
    cat("Shiny uygulaması durduruluyor. Veritabanı havuzu kapatılıyor...\n")
    poolClose(db_pool)
  })
}

cat("Veritabanı bağlantı havuzu 'db_pool' başarıyla oluşturuldu.\n")
# =================================================================
#        VERİTABANI BAĞLANTI YÖNETİCİSİ (ÇİFT VERİTABANI)
# =================================================================
# YENİ: Bu script artık 'config.yml' dosyasındaki 'default' ve
#       'live' profillerini okuyarak, sırasıyla 'db_pool_static'
#       ve 'db_pool_live' adında İKİ AYRI bağlantı havuzu oluşturur.
# =================================================================

# Gerekli kütüphaneleri çağır
library(pool)
library(config)
library(DBI)
library(RMariaDB)

# --- 1. STATİK ANALİZ için Bağlantı Havuzu Oluşturma ---

# 'config.yml' dosyasından 'default' profilindeki ayarları oku
tryCatch({
  static_db_config <- config::get(file = "config.yml", config = "default")$database
}, error = function(e) {
  stop("HATA: config.yml dosyasında 'default' profili bulunamadı! Hata: ", e$message)
})

# STATİK veritabanı bağlantı havuzunu (pool) oluştur
db_pool_static <- dbPool(
  drv = RMariaDB::MariaDB(),
  user = static_db_config$user,
  password = static_db_config$password,
  host = static_db_config$host,
  port = static_db_config$port,
  dbname = static_db_config$dbname
)

cat("Statik analiz veritabanı havuzu ('db_pool_static') başarıyla oluşturuldu.\n")


# --- 2. CANLI ANALİZ için Bağlantı Havuzu Oluşturma ---

# 'config.yml' dosyasından 'live' profilindeki ayarları oku
tryCatch({
  live_db_config <- config::get(file = "config.yml", config = "live")$database
}, error = function(e) {
  stop("HATA: config.yml dosyasında 'live' profili bulunamadı! Hata: ", e$message)
})

# CANLI veritabanı bağlantı havuzunu (pool) oluştur
db_pool_live <- dbPool(
  drv = RMariaDB::MariaDB(),
  user = live_db_config$user,
  password = live_db_config$password,
  host = live_db_config$host,
  port = live_db_config$port,
  dbname = live_db_config$dbname
)

cat("Canlı analiz veritabanı havuzu ('db_pool_live') başarıyla oluşturuldu.\n\n")


# --- 3. Uygulama Kapatıldığında Tüm Bağlantıları Güvenle Kapat ---
if (shiny::isRunning()) {
  shiny::onStop(function() {
    cat("Shiny uygulaması durduruluyor. Tüm veritabanı havuzları kapatılıyor...\n")
    poolClose(db_pool_static)
    poolClose(db_pool_live)
  })
}
# .Rprofile - GÜNCELLENMİŞ HAL (Asenkron Çalışma için Future Planı Eklendi)

# renv paket yöneticisini etkinleştir
source("renv/activate.R")

# === DEĞİŞİKLİK BURADA: ASENKRON (ARKA PLAN) İŞLEMLERİ ETKİNLEŞTİRME ===
# Proje her açıldığında, 'future' paketine birden fazla R oturumu
# (çekirdek sayısı kadar) kullanabileceğini söylüyoruz. Bu, 'future()' komutuyla
# başlatılan işlemlerin ana R oturumunu kilitlemeden arka planda
# çalışmasını sağlar.

# if (require("future")) {
#   future::plan(future::multisession)
#   cat("Asenkron çalışma modu etkinleştirildi (multisession plan).\n")
# }
# ======================================================================

# Windows/Linux uyumluluğu için Türkçe locale ayarları
# tryCatch({
#   Sys.setlocale("LC_TIME", "Turkish")
# }, warning = function(w) {
#   Sys.setlocale("LC_TIME", "tr_TR.UTF-8")
# })

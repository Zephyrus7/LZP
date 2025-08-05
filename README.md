# Lojistik Zeka Platformu

Bu proje, R Shiny üzerinde geliştirilmiş modüler bir iş zekası (BI) platformudur. Ham kargo verilerini merkezi bir MySQL veritabanından işleyerek, B2C ve B2B operasyonları için interaktif raporlar, skorlar ve stratejik içgörüler sunar.

## Proje Amacı

Platformun temel amacı, sadece veriyi görselleştirmek değil, aynı zamanda operasyonel mükemmelliği artıracak eyleme dönüştürülebilir bilgiler sağlamaktır

## Temel Yetenekler

*   **B2C ve B2B Analiz Modülleri:** Bireysel ve kurumsal operasyonlar için özelleştirilmiş analiz ve raporlama sekmeleri.
*   **Ağırlık Simülatörü:** Kullanıcıların kendi stratejik önceliklerine (hız, başarı oranı, müşteri deneyimi) göre firma performansını anlık olarak sıralamasına olanak tanır.
*   **Bağlama Özel Skorlama:** Her analizin (genel, marka bazlı, coğrafi) kendi bağlamı içinde adil ve doğru skorlar üretmesini sağlar.
*   **Dinamik Karşılaştırma:** İki farklı tarih aralığının performansını veritabanından anlık veri çekerek karşılaştırır.
*   **Veri Aktarım Aracı:** Farklı formatlardaki (.csv, .xlsx) ham verileri temizleyen, doğrulayan ve veritabanına aktaran merkezi bir script.

## Teknik Yapı

*   **Arayüz (Frontend):** R Shiny & `shinythemes`
*   **Arka Plan (Backend):** R
*   **Veri Manipülasyonu:** `dplyr`, `tidyr`
*   **Veritabanı:** MySQL / MariaDB (Bağlantı için `pool` ve `DBI` paketleri)
*   **Bağımlılık Yönetimi:** `renv`
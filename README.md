# Lojistik Zeka Platformu

Bu proje, R Shiny üzerinde geliştirilmiş modüler bir iş zekası (BI) platformudur. Ham kargo verilerini merkezi bir MySQL veritabanından işleyerek, B2C ve B2B operasyonları için interaktif raporlar, skorlar ve stratejik içgörüler sunar.

## Projenin Felsefesi

Platformun temel amacı, sadece veriyi görselleştirmek değil, aynı zamanda operasyonel mükemmelliği artıracak eyleme dönüştürülebilir bilgiler sağlamaktır. En değerli ve özgün özelliği, düşük gönderi hacmine sahip birimlerin (firma/bölge) istatistiksel olarak yanıltıcı skorlar almasını engelleyen, bağlama duyarlı **Hacim Ayarlı Skorlama** mekanizmasıdır.

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

## Nasıl Çalıştırılır?

1.  **Depoyu Klonlayın:**
    ```bash
    git clone https://github.com/Zephyrus7/LZP.git
    ```
2.  **Bağımlılıkları Yükleyin:** Projeyi RStudio'da açın ve konsola `renv::restore()` yazarak projeye özel kütüphanelerin kurulmasını sağlayın.
3.  **Veritabanını Kurun:** Merkezi bir MySQL sunucusunda `lojistik_db` adında bir veritabanı oluşturun. Gerekli tabloların şeması ileride eklenecektir.
4.  **Konfigürasyonu Ayarlayın:** `config.yml` dosyasını kendi veritabanı bilgilerinizle güncelleyin.
5.  **Veriyi Aktarın:** `00_Data_Migration_Tool.R` script'ini çalıştırarak ham veri dosyalarınızı seçin ve veritabanını doldurun.
6.  **Uygulamayı Başlatın:** `app.R` dosyasını açın ve RStudio'da `Run App` butonuna tıklayın.
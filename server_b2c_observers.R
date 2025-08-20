# =========================================================================
#                B2C SERVER - KULLANICI ETKİLEŞİMLERİ
# =========================================================================
# Görevi: Tüm observeEvent bloklarını barındırarak kullanıcı
#         aksiyonlarına (tıklama vb.) tepki vermek.
# NOT: Bu dosyadaki kod, `server_b2c.R` içinde `source` edilir.
# =========================================================================

# --- Firma Karnesi Sekmesi UI Yönetimi ---
# Kullanıcı firma veya şehir filtresini değiştirdiğinde, mevcut içeriği gizle
# ve yükleme animasyonunu göster.
observeEvent(list(input$secilen_firma_karne, input$karne_sehir_secimi), {
  req(input$secilen_firma_karne)
  shinyjs::hide("content_karne_grafik")
  shinyjs::hide("content_karne_tablo")
  shinyjs::hide("panel_karne_veri_yok")
  shinyjs::show("placeholder_karne_grafik")
  shinyjs::show("placeholder_karne_tablo")
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Firma karne verisi (yeniden) hesaplandığında tetiklenir.
observeEvent(firma_karne_filtrelenmis_veri(), {
  # min_hacim_karne input'u NULL ise 0 olarak kabul et
  min_hacim <- input$min_hacim_karne %||% 0
  
  data_for_plot <- firma_karne_filtrelenmis_veri() %>%
    filter(toplam_gonderi_sayisi >= min_hacim)
  
  if (nrow(data_for_plot) > 0) {
    shinyjs::hide("placeholder_karne_grafik", anim = TRUE, animType = "fade")
    shinyjs::show("content_karne_grafik", anim = TRUE, animType = "fade")
    shinyjs::hide("placeholder_karne_tablo", anim = TRUE, animType = "fade")
    shinyjs::show("content_karne_tablo", anim = TRUE, animType = "fade")
    shinyjs::hide("panel_karne_veri_yok")
  } else {
    shinyjs::hide("placeholder_karne_grafik")
    shinyjs::hide("placeholder_karne_tablo")
    shinyjs::hide("content_karne_grafik")
    shinyjs::hide("content_karne_tablo")
    shinyjs::show("panel_karne_veri_yok", anim = TRUE, animType = "fade")
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)


# --- Dinamik Karşılaştırma Butonu ---
observeEvent(input$karsilastir_button, {
  req(input$ana_donem_secimi, input$karsilastirma_donem_secimi)
  
  # Butonun ID'sini namespace ile al
  button_id <- ns("karsilastir_button")
  original_html <- "VERİLERİ KARŞILAŞTIR"
  
  # Buton animasyonunu başlat
  shinyjs::html(button_id, "Hesaplanıyor...")
  shinyjs::disable(button_id)
  shinyjs::addClass(button_id, "btn-loading")
  on.exit({
    shinyjs::html(button_id, original_html)
    shinyjs::removeClass(button_id, "btn-loading")
    shinyjs::enable(button_id)
  })
  
  # Veritabanından belirli bir dönem için özet veri çeken fonksiyon
  # NOT: Bu fonksiyon, `analiz_et_ve_skorla_b2c`'yi çağırdığı için, bu fonksiyonun
  # global ortamda (app.R tarafından source edilmiş) olması gerekir.
  get_summary_for_period <- function(start_date, end_date) {
    period_data_list <- analiz_et_ve_skorla_b2c(db_pool = db_pool_static, start_date = start_date, end_date = end_date, progress_updater = NULL)
    if(is.null(period_data_list)) { return(NULL) }
    
    summary <- period_data_list$ham_veri_temiz %>%
      filter(!kargo_turu %in% b2b_turleri, !is.na(kargo_turu)) %>%
      group_by(`Kargo Firması` = kargo_turu) %>%
      summarise(
        toplam_gonderi_sayisi = n(),
        ortalama_teslim_suresi = mean(toplam_teslim_suresi_saat, na.rm = TRUE),
        dinamik_basari_orani = mean(basari_flag, na.rm = TRUE),
        sikayet_orani_yuzde = mean(sikayet_var_mi, na.rm = TRUE) * 100
      )
    return(summary)
  }
  
  ana_ozet <- get_summary_for_period(input$ana_donem_secimi[1], input$ana_donem_secimi[2])
  karsilastirma_ozet <- get_summary_for_period(input$karsilastirma_donem_secimi[1], input$karsilastirma_donem_secimi[2])
  
  if (is.null(ana_ozet) || is.null(karsilastirma_ozet)) {
    showNotification("Seçilen tarih aralıklarından birinde veya her ikisinde veri bulunamadı.", type = "warning", duration = 7)
    karsilastirma_verisi(NULL)
    return()
  }
  
  format_date_range <- function(dates) {
    if (is.null(dates)) return("")
    paste(format(as.Date(dates[1]), "%d %b %Y"), "-", format(as.Date(dates[2]), "%d %b %Y"))
  }
  
  ana_etiket <- format_date_range(input$ana_donem_secimi)
  karsilastirma_etiket <- format_date_range(input$karsilastirma_donem_secimi)
  
  birlesik_veri <- full_join(ana_ozet, karsilastirma_ozet, by = "Kargo Firması", suffix = c("_ana", "_karsilastirma"))
  
  # Sonucu reactiveVal'e yaz
  karsilastirma_verisi(list(data = birlesik_veri, ana_etiket = ana_etiket, karsilastirma_etiket = karsilastirma_etiket))
})


# --- Tablo Satırlarına Tıklama Olayları (Modal Pencere Açanlar) ---

# 1. İlçe Karşılaştırma Tablosu
observeEvent(input$detay_tablosu_rows_selected, {
  req(input$detay_tablosu_rows_selected)
  selected_row_index <- input$detay_tablosu_rows_selected
  
  # Tablonun o anki sıralamasını ve verisini al
  karsilastirma_verisi_orijinal <- ilce_karsilastirma_data() %>% arrange(desc(Bayes_EPS))
  
  secilen_firma <- karsilastirma_verisi_orijinal %>% slice(selected_row_index) %>% pull(kargo_turu)
  secilen_sehir <- input$sehir_secimi_tab1
  secilen_ilce <- input$ilce_secimi_tab1
  
  detay_verisi <- ham_veri_temiz() %>% filter(kargo_turu == secilen_firma)
  if (secilen_ilce == "all_districts") {
    detay_verisi <- detay_verisi %>% filter(sehir == secilen_sehir)
    bolge_adi <- str_to_upper(secilen_sehir, "tr")
  } else {
    detay_verisi <- detay_verisi %>% filter(sehir == secilen_sehir, ilce == secilen_ilce)
    bolge_adi <- paste(str_to_upper(secilen_sehir, "tr"), "-", str_to_upper(secilen_ilce, "tr"))
  }
  
  detay_verisi_final <- detay_verisi %>%
    select(`Kargo No` = kargo_no, `Durum` = kargo_durumu, `Teslim Süresi (Saat)` = toplam_teslim_suresi_saat, `Tahmini Süre (Saat)` = yeni_max_teslimat_suresi, `Şikayet Var Mı?` = sikayet_var_mi, `Son Hareket Tarihi` = son_islem_tarihi) %>%
    arrange(desc(`Teslim Süresi (Saat)`))
  
  showModal(modalDialog(
    title = paste0(secilen_firma, " - ", bolge_adi, " Bölgesi Sipariş Detayları"),
    DT::dataTableOutput(ns("siparis_detay_tablosu")),
    footer = modalButton("Kapat"), size = "l", easyClose = TRUE
  ))
  
  output$siparis_detay_tablosu <- DT::renderDataTable({
    DT::datatable(detay_verisi_final, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
})

# 2. Firma Karnesi Tablosu
observeEvent(input$firma_karne_tablosu_rows_selected, {
  req(input$firma_karne_tablosu_rows_selected)
  selected_row_index <- input$firma_karne_tablosu_rows_selected
  secilen_firma <- input$secilen_firma_karne
  
  karne_data_guncel <- firma_karne_tablo_verisi()
  secilen_sehir <- karne_data_guncel %>% slice(selected_row_index) %>% pull(sehir)
  secilen_ilce <- karne_data_guncel %>% slice(selected_row_index) %>% pull(ilce)
  
  detay_verisi <- ham_veri_temiz() %>% filter(kargo_turu == secilen_firma, sehir == secilen_sehir, ilce == secilen_ilce)
  detay_verisi_final <- detay_verisi %>%
    select(`Kargo No` = kargo_no, `Durum` = kargo_durumu, `Teslim Süresi (Saat)` = toplam_teslim_suresi_saat, `Tahmini Süre (Saat)` = yeni_max_teslimat_suresi, `Şikayet Var Mı?` = sikayet_var_mi, `Son Hareket Tarihi` = son_islem_tarihi) %>%
    arrange(desc(`Teslim Süresi (Saat)`))
  
  bolge_adi <- paste(str_to_upper(secilen_sehir, "tr"), "-", str_to_upper(secilen_ilce, "tr"))
  
  showModal(modalDialog(
    title = paste0(secilen_firma, " - ", bolge_adi, " Bölgesi Sipariş Detayları"),
    DT::dataTableOutput(ns("siparis_detay_tablosu_karne")),
    footer = modalButton("Kapat"), size = "l", easyClose = TRUE
  ))
  
  output$siparis_detay_tablosu_karne <- DT::renderDataTable({
    DT::datatable(detay_verisi_final, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
})

# 3. Marka Analizi Tablosu
observeEvent(input$marka_analizi_tablosu_rows_selected, {
  req(input$marka_analizi_tablosu_rows_selected, input$secilen_marka_analizi, input$marka_analizi_secilen_il)
  
  selected_row_data <- marka_analizi_data()[input$marka_analizi_tablosu_rows_selected, ]
  secilen_firma <- selected_row_data$kargo_turu
  secilen_marka <- input$secilen_marka_analizi
  
  detay_verisi <- ham_veri_temiz() %>% filter(gonderici == secilen_marka, kargo_turu == secilen_firma)
  
  bolge_adi <- if (input$marka_analizi_secilen_il != "all_cities") {
    if (!is.null(input$marka_analizi_secilen_ilce) && input$marka_analizi_secilen_ilce != "all_districts") {
      paste(str_to_upper(input$marka_analizi_secilen_il, "tr"), "-", str_to_upper(input$marka_analizi_secilen_ilce, "tr"))
    } else {
      str_to_upper(input$marka_analizi_secilen_il, "tr")
    }
  } else {
    "Tüm Türkiye"
  }
  
  if (input$marka_analizi_secilen_il != "all_cities") {
    detay_verisi <- detay_verisi %>% filter(sehir == input$marka_analizi_secilen_il)
    if (!is.null(input$marka_analizi_secilen_ilce) && input$marka_analizi_secilen_ilce != "all_districts") {
      detay_verisi <- detay_verisi %>% filter(ilce == input$marka_analizi_secilen_ilce)
    }
  }
  
  detay_verisi_final <- detay_verisi %>%
    select(`Kargo No` = kargo_no, `Durum` = kargo_durumu, `Teslim Süresi (Saat)` = toplam_teslim_suresi_saat, `Tahmini Süre (Saat)` = yeni_max_teslimat_suresi, `Şikayet Var Mı?` = sikayet_var_mi, `Son Hareket Tarihi` = son_islem_tarihi) %>%
    arrange(desc(`Teslim Süresi (Saat)`))
  
  modal_title <- paste0(secilen_marka, " | ", secilen_firma, " | ", bolge_adi, " Gönderi Detayları")
  
  showModal(modalDialog(
    title = modal_title,
    DT::dataTableOutput(ns("siparis_detay_tablosu_marka")),
    footer = modalButton("Kapat"), size = "l", easyClose = TRUE
  ))
  
  output$siparis_detay_tablosu_marka <- DT::renderDataTable({
    DT::datatable(detay_verisi_final, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
})

# 4. Şikayet Analizi Tablosu
observeEvent(input$sikayet_analizi_tablosu_rows_selected, {
  req(input$sikayet_analizi_tablosu_rows_selected, ham_veri_temiz())
  selected_row_index <- input$sikayet_analizi_tablosu_rows_selected
  
  summary_data <- sikayet_analizi_ozet_verisi()
  req(nrow(summary_data) >= selected_row_index)
  
  selected_row <- summary_data %>% slice(selected_row_index)
  
  if (input$sikayet_firma_secimi == "all_companies") {
    secilen_firma <- selected_row %>% pull(kargo_turu)
    detay_verisi <- ham_veri_temiz() %>% filter(kargo_turu == secilen_firma, sikayet_var_mi == TRUE)
    modal_title <- paste(secilen_firma, "için Şikayetli Gönderi Detayları")
  } else {
    secilen_sehir <- selected_row %>% pull(sehir)
    secilen_ilce <- selected_row %>% pull(ilce)
    detay_verisi <- ham_veri_temiz() %>% filter(sehir == secilen_sehir, ilce == secilen_ilce, kargo_turu == input$sikayet_firma_secimi, sikayet_var_mi == TRUE)
    modal_title <- paste(input$sikayet_firma_secimi, "-", str_to_upper(secilen_sehir, "tr"), str_to_upper(secilen_ilce, "tr"), "Bölgesi Şikayet Detayları")
  }
  
  if (input$sikayet_sehir_secimi != "all_cities") {
    detay_verisi <- detay_verisi %>% filter(sehir == input$sikayet_sehir_secimi)
  }
  
  detay_verisi_final <- detay_verisi %>%
    select(`Kargo No` = kargo_no, `Marka`=gonderici, `Durum` = kargo_durumu, `Teslim Süresi (Saat)` = toplam_teslim_suresi_saat, `Tahmini Süre (Saat)` = yeni_max_teslimat_suresi, `Son Hareket Tarihi` = son_islem_tarihi) %>%
    arrange(desc(`Son Hareket Tarihi`))
  
  showModal(modalDialog(
    title = modal_title,
    DT::dataTableOutput(ns("siparis_detay_tablosu_sikayet")),
    footer = modalButton("Kapat"), size = "l", easyClose = TRUE
  ))
  
  output$siparis_detay_tablosu_sikayet <- DT::renderDataTable({
    DT::datatable(detay_verisi_final, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
})
# =========================================================================
#             CANLI ANALİZ MODÜLÜ - VERİ İNDİRME YÖNETİMİ (DÜZELTİLDİ)
# =========================================================================
# YENİLİK: Hareketsiz gönderi raporu artık hesaplama yapmıyor, sadece
#          server_live.R'dan gelen hazır veriyi yazdırıyor. Bu,
#          'gizli sekme' sorununu ve indirme hatalarını çözer.
# =========================================================================

register_live_downloads <- function(output, session, kpi_data, inactive_data, return_data, full_data) {
  
  ns <- session$ns
  
  # --- 1. DİNAMİK İNDİRME PANELİ (UI) ---
  download_ui <- reactive({
    tagList(
      div(style = "margin-bottom: 20px;",
          p(strong("Genel Raporlar")),
          p(tags$small("Aktif sekmelerde gördüğünüz, filtrelenmiş analiz sonuçlarını indirin.")),
          
          if (isTruthy(kpi_data())) { downloadButton(ns("download_kpi_report"), "Teslimat Performans Karnesi", class="btn-info btn-block") } 
          else { tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'Teslimat Performansı' sekmesinde bir analiz yapılmış olmalıdır.", "Teslimat Performans Karnesi") },
          br(),
          
          # <<< DEĞİŞİKLİK: Koşul artık inactive_data reaktifini ve satır sayısını kontrol ediyor >>>
          if (isTruthy(inactive_data()) && nrow(inactive_data()) > 0) { downloadButton(ns("download_inactive_report"), "Hareketsiz Gönderi Alarm Raporu", class="btn-info btn-block") } 
          else { tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'Operasyonel Alarmlar' sekmesinde görüntülenecek hareketsiz gönderi olmalıdır.", "Hareketsiz Gönderi Alarm Raporu") },
          br(),
          
          if (isTruthy(return_data()) && nrow(return_data()) > 0) { downloadButton(ns("download_return_report"), "İade Süreçleri Raporu", class="btn-info btn-block") } 
          else { tags$div(class = "btn btn-info btn-block disabled", title = "Bu raporu indirmek için önce 'İade Süreçleri' sekmesinde bir analiz yapılmış olmalıdır.", "İade Süreçleri Raporu") }
      ),
      
      div(
        p(strong("Kapsamlı Rapor")),
        p(tags$small("Analizdeki tüm siparişlerin durum dökümünü indirin.")),
        
        if (isTruthy(full_data())) { downloadButton(ns("download_full_report"), "Tüm Siparişlerin Durum Özeti", class="btn-primary btn-block") } 
        else { tags$div(class = "btn btn-primary btn-block disabled", title = "Bu raporu indirmek için önce 'Tüm Siparişler Özeti' sekmesindeki butona basarak raporu oluşturmalısınız.", "Tüm Siparişlerin Durum Özeti") }
      )
    )
  })
  
  # --- 2. İNDİRME MANTIĞI (DOWNLOAD HANDLERS) ---
  
  output$download_kpi_report <- downloadHandler(
    filename = function() { paste0("Canli_Teslimat_Performans_Karnesi_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(kpi_data())
      df_to_download <- kpi_data() %>%
        select( `Firma`, `Toplam Gönderi` = Toplam_Gonderi, `Gönderi Oranı (%)` = Gonderi_Oran, `Partner Alım (Saat)` = Ort_Partner_Alim_Saat, `Partner Teslim (Saat)` = Ort_Partner_Teslim_Saat, `Zamanında Teslim Adet` = Zamaninda_Teslim_Adet, `Zamanında Teslim Oran (%)` = Zamaninda_Teslim_Oran, `Teslim Edilen Adet` = Teslim_Edilen_Adet, `Kayıp Adet` = Kayip_Adet, `Kayıp Oran (%)` = Kayip_Oran, `Teslim 0-24 Saat Oran (%)` = Prop_0_24, `Teslim 24-48 Saat Oran (%)` = Prop_24_48, `Teslim 48-72 Saat Oran (%)` = Prop_48_72, `Teslim 72+ Saat Oran (%)` = Prop_72_Plus ) %>%
        mutate(across(ends_with("(%)"), ~ . * 100))
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
  
  # <<< DEĞİŞİKLİK: BU BLOK ARTIK ÇOK DAHA BASİT VE GÜVENLİ >>>
  output$download_inactive_report <- downloadHandler(
    filename = function() { paste0("Canli_Hareketsiz_Gonderi_Alarmlari_", Sys.Date(), ".xlsx") },
    content = function(file) {
      # Artık hesaplama yapmıyoruz, sadece hazır veriyi istiyoruz.
      req(inactive_data())
      # Gelen veriyi doğrudan dosyaya yazıyoruz.
      openxlsx::write.xlsx(inactive_data(), file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
  
  output$download_return_report <- downloadHandler(
    filename = function() { paste0("Canli_Iade_Surecleri_Raporu_", Sys.Date(), ".xlsx") },
    content = function(file) {
      df_to_download <- return_data() %>%
        group_by(`Kargo Firması` = Kargo_Turu) %>%
        summarise( `Ort. Müşterinin Gönderme Süresi (Saat)` = round(mean(Musteri_Gonderme_Suresi_Saat, na.rm = TRUE), 2), `Ort. Partner İade Süresi (Saat)` = round(mean(Partner_Iade_Suresi_Saat, na.rm = TRUE), 2), `Toplam İade Adedi` = n() ) %>%
        arrange(desc(`Toplam İade Adedi`))
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
  
  output$download_full_report <- downloadHandler(
    filename = function() { paste0("Canli_Tum_Siparisler_Durum_Ozeti_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(full_data())
      df_to_download <- full_data() %>%
        rename(`Sipariş Durumu` = Durum_Adi, `Toplam Adet` = Adet)
      openxlsx::write.xlsx(df_to_download, file, asTable = TRUE, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", halign = "center"))
    }
  )
  
  # --- 3. ÇIKTI ---
  return(download_ui)
}
server_live <- function(id, data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #--- YARDIMCI FONKSİYON: ÖZEL GRAFİK TEMASI ---
    get_custom_theme <- function(current_theme) {
      if (current_theme == "dark") {
        theme(
          plot.background = element_rect(fill = "#343A40", color = NA),
          panel.background = element_rect(fill = "#343A40", color = NA),
          panel.grid.major = element_line(color = "#495057"),
          panel.grid.minor = element_line(color = "#495057"),
          text = element_text(color = "#E9ECEF", family = "sans"),
          axis.text = element_text(color = "#E9ECEF"),
          title = element_text(color = "#E9ECEF", size = 15)
        )
      } else {
        theme_minimal(base_size = 15)
      }
    }
    
    #--- 1. TEMEL REAKTİF VERİ ve SÖZLÜKLER ---
    live_data_enriched <- reactive({
      req(data())
      data()$canli_veri_zenginlesmis
    })
    
    work_steps_dict <- reactive({
      poolWithTransaction(db_pool_live, function(conn) {
        dbGetQuery(conn, "SELECT id, name FROM work_steps")
      })
    })
    
    #--- 2. FİLTRELER İÇİN DİNAMİK UI ÜRETİMİ ---
    # ... (Bu bölüm değişmedi)
    output$firma_filtre_ui_teslimat <- renderUI({ req(live_data_enriched()); firma_listesi <- sort(unique(live_data_enriched()$Kargo_Turu)); selectInput(ns("firma_secimi_teslimat"), "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi)) })
    output$marka_filtre_ui_teslimat <- renderUI({ return(NULL) })
    output$firma_filtre_ui_operasyonel <- renderUI({ req(live_data_enriched()); firma_listesi <- live_data_enriched() %>% filter(is.na(teslim_tarihi)) %>% pull(Kargo_Turu) %>% unique() %>% sort(); selectInput(ns("firma_secimi_operasyonel"), "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi)) })
    output$firma_filtre_ui_iade <- renderUI({ req(live_data_enriched()); firma_listesi <- live_data_enriched() %>% filter(!is.na(iade_baslangic_tarihi)) %>% pull(Kargo_Turu) %>% unique() %>% sort(); selectInput(ns("firma_secimi_iade"), "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi)) })
    output$marka_filtre_ui_iade <- renderUI({ return(NULL) })
    
    #--- 3. BÖLÜM: TESLİMAT PERFORMANSI ---
    # ... (Bu bölüm değişmedi)
    teslimat_performans_data <- reactive({ req(live_data_enriched(), input$firma_secimi_teslimat); df <- live_data_enriched(); if (input$firma_secimi_teslimat != "TUMU") { df <- df %>% filter(Kargo_Turu == input$firma_secimi_teslimat) }; df %>% mutate(Alim_Suresi_Saat = as.numeric(difftime(kargo_kabul_tarihi, veri_tarihi, units = "hours")), Partnere_Verme_Suresi_Saat = as.numeric(difftime(order_check_tarihi, kargo_kabul_tarihi, units = "hours")), Partner_Teslim_Suresi_Saat = as.numeric(difftime(teslim_tarihi, order_check_tarihi, units = "hours"))) })
    output$alim_suresi_val <- renderText({ round(mean(teslimat_performans_data()$Alim_Suresi_Saat, na.rm = TRUE), 2) })
    output$partnere_verme_suresi_val <- renderText({ round(mean(teslimat_performans_data()$Partnere_Verme_Suresi_Saat, na.rm = TRUE), 2) })
    output$partner_teslim_suresi_val <- renderText({ round(mean(teslimat_performans_data()$Partner_Teslim_Suresi_Saat, na.rm = TRUE), 2) })
    output$teslimat_performans_table <- DT::renderDataTable({ teslimat_performans_data() %>% group_by(`Kargo Firması` = Kargo_Turu) %>% summarise(`Ort. Alım Süresi (Saat)` = round(mean(Alim_Suresi_Saat, na.rm = TRUE), 2), `Ort. Partnere Verme Süresi (Saat)` = round(mean(Partnere_Verme_Suresi_Saat, na.rm = TRUE), 2), `Ort. Partner Teslim Süresi (Saat)` = round(mean(Partner_Teslim_Suresi_Saat, na.rm = TRUE), 2), `İşlem Adedi` = n()) %>% arrange(desc(`İşlem Adedi`)) %>% DT::datatable(rownames = FALSE, options = list(pageLength = 10, searching = TRUE)) })
    
    #--- 4. BÖLÜM: OPERASYONEL ALARMLAR ---
    # ... (Bu bölüm değişmedi)
    operasyonel_data_filtrelenmis <- reactive({ req(live_data_enriched(), input$firma_secimi_operasyonel); df <- live_data_enriched() %>% filter(is.na(teslim_tarihi)); if (input$firma_secimi_operasyonel != "TUMU") { df <- df %>% filter(Kargo_Turu == input$firma_secimi_operasyonel) }; df })
    acik_gonderi_summary <- reactive({ operasyonel_data_filtrelenmis() %>% left_join(work_steps_dict(), by = c("status" = "id")) %>% mutate(Durum_Adi = case_when(status == 0 ~ "Tanımsız Durum", !is.na(name) ~ name, TRUE ~ as.character(status))) %>% count(Durum_Adi, name = "Adet", sort = TRUE) })
    output$acik_gonderi_plot <- renderPlot({ df_summary <- acik_gonderi_summary(); validate(need(nrow(df_summary) > 0, "Seçilen filtre için görüntülenecek açık gönderi verisi bulunamadı.")); p <- ggplot(df_summary, aes(x = reorder(Durum_Adi, Adet), y = Adet, fill = Durum_Adi)) + geom_col(show.legend = FALSE) + geom_text(aes(label = Adet), hjust = -0.2, size = 4.5, color = if(theme_reactive()=="dark") "white" else "black") + coord_flip() + labs(x = NULL, y = "Açık Gönderi Adedi") + scale_y_continuous(limits = c(0, max(df_summary$Adet) * 1.15)); p + get_custom_theme(theme_reactive()) }, bg="transparent")
    output$hareketsiz_gonderi_table <- DT::renderDataTable({ req(input$hareketsiz_gun_esigi); threshold_days <- as.numeric(input$hareketsiz_gun_esigi); operasyonel_data_filtrelenmis() %>% mutate(Hareketsiz_Kalma_Suresi_Gun = difftime(Sys.time(), son_islem_tarihi, units = "days")) %>% filter(Hareketsiz_Kalma_Suresi_Gun > threshold_days) %>% left_join(work_steps_dict(), by = c("status" = "id")) %>% mutate(`Son Durum` = case_when(status == 0 ~ "Tanımsız Durum", !is.na(name) ~ name, TRUE ~ as.character(status)), `Hareketsiz Kalma Süresi (Gün)` = round(as.numeric(Hareketsiz_Kalma_Suresi_Gun), 1), `Son Hareket Tarihi` = format(as.POSIXct(son_islem_tarihi), "%Y-%m-%d %H:%M")) %>% select(`Kargo Takip No` = kargo_no, `Kargo Firması` = Kargo_Turu, `Son Durum`, `Son Hareket Tarihi`, `Hareketsiz Kalma Süresi (Gün)`) %>% arrange(desc(`Hareketsiz Kalma Süresi (Gün)`)) %>% DT::datatable(rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE)) })
    
    #--- 5. BÖLÜM: İADE SÜREÇLERİ ---
    # ... (Bu bölüm değişmedi)
    iade_performans_data <- reactive({ req(live_data_enriched(), input$firma_secimi_iade); df <- live_data_enriched(); if (input$firma_secimi_iade != "TUMU") { df <- df %>% filter(Kargo_Turu == input$firma_secimi_iade) }; df %>% filter(!is.na(iade_baslangic_tarihi)) %>% mutate(Musteri_Gonderme_Suresi_Saat = as.numeric(difftime(iade_teslim_alindi_tarihi, iade_baslangic_tarihi, units = "hours")), Partner_Iade_Suresi_Saat = as.numeric(difftime(iade_teslim_edildi_tarihi, iade_teslim_alindi_tarihi, units = "hours")), Bovo_Iade_Teslim_Suresi_Saat = as.numeric(difftime(iade_teslim_edildi_tarihi, iade_teslim_alindi_tarihi, units = "hours"))) })
    output$musteri_iade_suresi_val <- renderText({ round(mean(iade_performans_data()$Musteri_Gonderme_Suresi_Saat, na.rm = TRUE), 2) })
    output$partner_iade_suresi_val <- renderText({ round(mean(iade_performans_data()$Partner_Iade_Suresi_Saat, na.rm = TRUE), 2) })
    output$bovo_iade_suresi_val <- renderText({ round(mean(iade_performans_data()$Bovo_Iade_Teslim_Suresi_Saat, na.rm = TRUE), 2) })
    output$iade_performans_table <- DT::renderDataTable({ iade_performans_data() %>% group_by(`Kargo Firması` = Kargo_Turu) %>% summarise(`Ort. Müşterinin Gönderme Süresi (Saat)` = round(mean(Musteri_Gonderme_Suresi_Saat, na.rm = TRUE), 2), `Ort. Partner İade Süresi (Saat)` = round(mean(Partner_Iade_Suresi_Saat, na.rm = TRUE), 2), `İade Adedi` = n()) %>% arrange(desc(`İade Adedi`)) %>% DT::datatable(rownames = FALSE, options = list(pageLength = 10, searching = TRUE)) })
    
    # ========================================================================
    #               YENİ BÖLÜM 6: TÜM SİPARİŞLER ÖZETİ
    # ========================================================================
    
    # Hesaplama sonucunu saklamak için bir reactiveVal oluştur
    full_analysis_data <- reactiveVal(NULL)
    
    # Butona basılmasını dinle
    observeEvent(input$run_full_analysis_button, {
      
      showNotification("Tüm siparişler analiz ediliyor, lütfen bekleyin...", duration = NULL, type = "message", id="full_analysis_notification")
      
      # Hesaplamayı yap
      summary_df <- live_data_enriched() %>%
        left_join(work_steps_dict(), by = c("status" = "id")) %>%
        mutate(
          Durum_Adi = case_when(
            status == 0 ~ "Tanımsız Durum",
            !is.na(name) ~ name,
            TRUE ~ as.character(status)
          )
        ) %>%
        count(Durum_Adi, name = "Adet", sort = TRUE)
      
      # Sonucu reactiveVal'e ata
      full_analysis_data(summary_df)
      
      # Sonuç panelini görünür yap
      shinyjs::show("full_analysis_results_panel", anim = TRUE, animType = "fade")
      
      removeNotification("full_analysis_notification")
    })
    
    # Tablo başlığını oluştur
    output$full_analysis_title <- renderText({
      req(full_analysis_data())
      total_orders <- sum(full_analysis_data()$Adet)
      paste0("Tüm Siparişlerin Durum Dağılımı (Toplam: ", format(total_orders, big.mark = ".", decimal.mark = ","), " Adet)")
    })
    
    # Tabloyu render et
    output$full_analysis_table <- DT::renderDataTable({
      req(full_analysis_data())
      DT::datatable(
        full_analysis_data(),
        colnames = c("Sipariş Durumu", "Toplam Adet"),
        rownames = FALSE,
        options = list(pageLength = 15, searching = TRUE)
      )
    })
    
  }) # moduleServer sonu
}
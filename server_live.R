# =========================================================================
#             CANLI ANALİZ MODÜLÜ - SUNUCU MANTIĞI (SERVER)
# =========================================================================
# YENİLİK: "Teslimat Performansı" sekmesi, tüm KPI'ları tek bir
#          minimalist tabloda birleştirecek şekilde tamamen yeniden yazıldı.
#          - Zaman aralığı dağılımlarını gösteren bir mini grafik (sparkline)
#            üreten yeni bir yardımcı fonksiyon eklendi.
#          - Tüm hesaplamalar tek bir merkezi reaktif ifadede birleştirildi.
# =========================================================================

server_live <- function(id, data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #--- YARDIMCI FONKSİYONLAR ---
    get_custom_theme <- function(current_theme) {
      # ... (Bu fonksiyon değişmedi)
      if (current_theme == "dark") {
        theme( plot.background = element_rect(fill = "#343A40", color = NA), panel.background = element_rect(fill = "#343A40", color = NA), panel.grid.major = element_line(color = "#495057"), panel.grid.minor = element_line(color = "#495057"), text = element_text(color = "#E9ECEF", family = "sans"), axis.text = element_text(color = "#E9ECEF"), title = element_text(color = "#E9ECEF", size = 15) )
      } else { theme_minimal(base_size = 15) }
    }
    
    # YENİ YARDIMCI FONKSİYON: Zaman aralığı oranlarına göre HTML bar grafiği oluşturur
    create_sparkline_bar <- function(p1, p2, p3, p4) {
      # p1: 0-24h, p2: 24-48h, p3: 48-72h, p4: 72h+ oranları
      title_text <- sprintf("Dağılım: 0-24h: %.1f%% | 24-48h: %.1f%% | 48-72h: %.1f%% | 72h+: %.1f%%", p1*100, p2*100, p3*100, p4*100)
      
      bar_html <- paste0(
        sprintf('<div class="spark-bar-segment spark-bar-green" style="width: %.1f%%;"></div>', p1 * 100),
        sprintf('<div class="spark-bar-segment spark-bar-blue" style="width: %.1f%%;"></div>', p2 * 100),
        sprintf('<div class="spark-bar-segment spark-bar-yellow" style="width: %.1f%%;"></div>', p3 * 100),
        sprintf('<div class="spark-bar-segment spark-bar-red" style="width: %.1f%%;"></div>', p4 * 100)
      )
      
      sprintf('<div class="spark-bar-container" title="%s">%s</div>', title_text, bar_html)
    }
    
    #--- 1. TEMEL REAKTİF VERİ ve SÖZLÜKLER ---
    live_data_enriched <- reactive({ req(data()); data()$canli_veri_zenginlesmis })
    work_steps_dict <- reactive({ poolWithTransaction(db_pool_live, function(conn) { dbGetQuery(conn, "SELECT id, name FROM work_steps") }) })
    
    #--- 2. FİLTRELER İÇİN DİNAMİK UI ÜRETİMİ (Değişiklik Yok) ---
    output$firma_filtre_ui_teslimat <- renderUI({ req(live_data_enriched()); firma_listesi <- sort(unique(live_data_enriched()$Kargo_Turu)); selectInput(ns("firma_secimi_teslimat"), "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi)) })
    output$firma_filtre_ui_operasyonel <- renderUI({ req(live_data_enriched()); firma_listesi <- live_data_enriched() %>% filter(is.na(teslim_tarihi)) %>% pull(Kargo_Turu) %>% unique() %>% sort(); selectInput(ns("firma_secimi_operasyonel"), "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi)) })
    output$firma_filtre_ui_iade <- renderUI({ req(live_data_enriched()); firma_listesi <- live_data_enriched() %>% filter(!is.na(iade_baslangic_tarihi)) %>% pull(Kargo_Turu) %>% unique() %>% sort(); selectInput(ns("firma_secimi_iade"), "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi)) })
    
    # ========================================================================
    #         BÖLÜM 3: TESLİMAT PERFORMANSI (TAMAMEN YENİLENDİ)
    # ========================================================================
    
    # --- 3a. Merkezi KPI Hesaplama Reaktifi ---
    unified_kpi_data <- reactive({
      req(live_data_enriched(), input$firma_secimi_teslimat)
      
      df_filtered <- live_data_enriched()
      if (input$firma_secimi_teslimat != "TUMU") {
        df_filtered <- df_filtered %>% filter(Kargo_Turu == input$firma_secimi_teslimat)
      }
      
      if(nrow(df_filtered) == 0) return(NULL)
      
      df_filtered %>%
        mutate(
          Partner_Alim_Suresi_Saat = as.numeric(difftime(kargo_kabul_tarihi, veri_tarihi, units = "hours")),
          Partner_Teslim_Suresi_Saat = as.numeric(difftime(teslim_tarihi, order_check_tarihi, units = "hours")),
          is_on_time = !is.na(teslim_tarihi) & !is.na(tahmini_teslimat_tarihi) & (teslim_tarihi <= tahmini_teslimat_tarihi),
          is_delivered = !is.na(teslim_tarihi),
          is_lost = is.na(teslim_tarihi) & (difftime(Sys.time(), son_islem_tarihi, units = "days") > 30),
          time_bucket = case_when(
            is_delivered ~ as.numeric(difftime(teslim_tarihi, veri_tarihi, units = "hours")),
            TRUE ~ NA_real_
          )
        ) %>%
        group_by(Firma = Kargo_Turu) %>%
        summarise(
          Toplam_Gonderi = n(),
          Ort_Partner_Alim_Saat = mean(Partner_Alim_Suresi_Saat, na.rm = TRUE),
          Ort_Partner_Teslim_Saat = mean(Partner_Teslim_Suresi_Saat, na.rm = TRUE),
          Zamaninda_Teslim_Adet = sum(is_on_time, na.rm = TRUE),
          Teslim_Edilen_Adet = sum(is_delivered, na.rm = TRUE),
          Kayip_Adet = sum(is_lost, na.rm = TRUE),
          
          # Zaman aralıkları için adetler
          Saat_0_24_Adet = sum(time_bucket <= 24, na.rm = TRUE),
          Saat_24_48_Adet = sum(time_bucket > 24 & time_bucket <= 48, na.rm = TRUE),
          Saat_48_72_Adet = sum(time_bucket > 48 & time_bucket <= 72, na.rm = TRUE),
          Saat_72_Plus_Adet = sum(time_bucket > 72, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # Oranları ve son hesaplamaları yap
        mutate(
          Zamaninda_Teslim_Oran = if_else(Teslim_Edilen_Adet > 0, Zamaninda_Teslim_Adet / Teslim_Edilen_Adet, 0),
          Kayip_Oran = if_else(Toplam_Gonderi > 0, Kayip_Adet / Toplam_Gonderi, 0),
          
          # Sparkline için oranlar
          Prop_0_24 = if_else(Teslim_Edilen_Adet > 0, Saat_0_24_Adet / Teslim_Edilen_Adet, 0),
          Prop_24_48 = if_else(Teslim_Edilen_Adet > 0, Saat_24_48_Adet / Teslim_Edilen_Adet, 0),
          Prop_48_72 = if_else(Teslim_Edilen_Adet > 0, Saat_48_72_Adet / Teslim_Edilen_Adet, 0),
          Prop_72_Plus = if_else(Teslim_Edilen_Adet > 0, Saat_72_Plus_Adet / Teslim_Edilen_Adet, 0)
        )
    })
    
    # --- 3b. Yeni Birleşik Performans Karnesi Tablosu ---
    output$unified_performance_table <- DT::renderDataTable({
      kpi_data <- unified_kpi_data()
      validate(need(is.data.frame(kpi_data) && nrow(kpi_data) > 0, "Seçilen filtre için görüntülenecek teslimat verisi bulunamadı."))
      
      kpi_data %>%
        # Gösterim için yeni birleştirilmiş sütunları oluştur
        mutate(
          Teslimat_Hizi_Dagilimi = create_sparkline_bar(Prop_0_24, Prop_24_48, Prop_48_72, Prop_72_Plus),
          Zamaninda_Teslim_Gosterim = paste0(Zamaninda_Teslim_Adet, " (", scales::percent(Zamaninda_Teslim_Oran, accuracy = 0.1), ")"),
          Kayip_Gosterim = paste0(Kayip_Adet, " (", scales::percent(Kayip_Oran, accuracy = 0.1), ")")
        ) %>%
        select(
          Firma,
          `Toplam Gönderi` = Toplam_Gonderi,
          `Teslimat Hızı Dağılımı` = Teslimat_Hizi_Dagilimi,
          `Partner Alım (Saat)` = Ort_Partner_Alim_Saat,
          `Partner Teslim (Saat)` = Ort_Partner_Teslim_Saat,
          `Zamanında Teslim` = Zamaninda_Teslim_Gosterim,
          `Kayıp` = Kayip_Gosterim
        ) %>%
        DT::datatable(
          escape = FALSE, # HTML sparkline'ın render edilmesi için kritik
          rownames = FALSE,
          options = list(
            pageLength = 10, 
            searching = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = '_all'))
          )
        ) %>%
        formatRound(columns = c("Partner Alım (Saat)", "Partner Teslim (Saat)"), digits = 1)
    })
    
    # ========================================================================
    #         BÖLÜM 4, 5, 6 (Değişiklik Yok)
    # ========================================================================
    
    #--- 4. BÖLÜM: OPERASYONEL ALARMLAR ---
    operasyonel_data_filtrelenmis <- reactive({ req(live_data_enriched(), input$firma_secimi_operasyonel); df <- live_data_enriched() %>% filter(is.na(teslim_tarihi)); if (input$firma_secimi_operasyonel != "TUMU") { df <- df %>% filter(Kargo_Turu == input$firma_secimi_operasyonel) }; df })
    acik_gonderi_summary <- reactive({ operasyonel_data_filtrelenmis() %>% left_join(work_steps_dict(), by = c("status" = "id")) %>% mutate(Durum_Adi = case_when(status == 0 ~ "Tanımsız Durum", !is.na(name) ~ name, TRUE ~ as.character(status))) %>% count(Durum_Adi, name = "Adet", sort = TRUE) })
    output$acik_gonderi_plot <- renderPlot({ df_summary <- acik_gonderi_summary(); validate(need(nrow(df_summary) > 0, "Seçilen filtre için görüntülenecek açık gönderi verisi bulunamadı.")); p <- ggplot(df_summary, aes(x = reorder(Durum_Adi, Adet), y = Adet, fill = Durum_Adi)) + geom_col(show.legend = FALSE) + geom_text(aes(label = Adet), hjust = -0.2, size = 4.5, color = if(theme_reactive()=="dark") "white" else "black") + coord_flip() + labs(x = NULL, y = "Açık Gönderi Adedi") + scale_y_continuous(limits = c(0, max(df_summary$Adet) * 1.15)); p + get_custom_theme(theme_reactive()) }, bg="transparent")
    output$hareketsiz_gonderi_table <- DT::renderDataTable({ req(input$hareketsiz_gun_esigi); threshold_days <- as.numeric(input$hareketsiz_gun_esigi); operasyonel_data_filtrelenmis() %>% mutate(Hareketsiz_Kalma_Suresi_Gun = difftime(Sys.time(), son_islem_tarihi, units = "days")) %>% filter(Hareketsiz_Kalma_Suresi_Gun > threshold_days) %>% left_join(work_steps_dict(), by = c("status" = "id")) %>% mutate(`Son Durum` = case_when(status == 0 ~ "Tanımsız Durum", !is.na(name) ~ name, TRUE ~ as.character(status)), `Hareketsiz Kalma Süresi (Gün)` = round(as.numeric(Hareketsiz_Kalma_Suresi_Gun), 1), `Son Hareket Tarihi` = format(as.POSIXct(son_islem_tarihi), "%Y-%m-%d %H:%M")) %>% select(`Kargo Takip No` = kargo_no, `Kargo Firması` = Kargo_Turu, `Son Durum`, `Son Hareket Tarihi`, `Hareketsiz Kalma Süresi (Gün)`) %>% arrange(desc(`Hareketsiz Kalma Süresi (Gün)`)) %>% DT::datatable(rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE)) })
    output$operasyonel_total_count_ui <- renderUI({ df <- operasyonel_data_filtrelenmis(); req(df); total_count <- nrow(df); if (total_count == 0) return(NULL); tags$div( style = "text-align: right;", h5("Açık Gönderi Sayısı:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = "."), style = "margin: 0; font-weight: bold;") ) })
    
    #--- 5. BÖLÜM: İADE SÜREÇLERİ ---
    iade_performans_data <- reactive({ req(live_data_enriched(), input$firma_secimi_iade); df <- live_data_enriched(); if (input$firma_secimi_iade != "TUMU") { df <- df %>% filter(Kargo_Turu == input$firma_secimi_iade) }; df %>% filter(!is.na(iade_baslangic_tarihi)) %>% mutate(Musteri_Gonderme_Suresi_Saat = as.numeric(difftime(iade_teslim_alindi_tarihi, iade_baslangic_tarihi, units = "hours")), Partner_Iade_Suresi_Saat = as.numeric(difftime(iade_teslim_edildi_tarihi, iade_teslim_alindi_tarihi, units = "hours")), Bovo_Iade_Teslim_Suresi_Saat = as.numeric(difftime(iade_teslim_edildi_tarihi, iade_teslim_alindi_tarihi, units = "hours"))) })
    output$musteri_iade_suresi_val <- renderText({ round(mean(iade_performans_data()$Musteri_Gonderme_Suresi_Saat, na.rm = TRUE), 2) })
    output$partner_iade_suresi_val <- renderText({ round(mean(iade_performans_data()$Partner_Iade_Suresi_Saat, na.rm = TRUE), 2) })
    output$bovo_iade_suresi_val <- renderText({ round(mean(iade_performans_data()$Bovo_Iade_Teslim_Suresi_Saat, na.rm = TRUE), 2) })
    output$iade_performans_table <- DT::renderDataTable({ iade_performans_data() %>% group_by(`Kargo Firması` = Kargo_Turu) %>% summarise(`Ort. Müşterinin Gönderme Süresi (Saat)` = round(mean(Musteri_Gonderme_Suresi_Saat, na.rm = TRUE), 2), `Ort. Partner İade Süresi (Saat)` = round(mean(Partner_Iade_Suresi_Saat, na.rm = TRUE), 2), `İade Adedi` = n()) %>% arrange(desc(`İade Adedi`)) %>% DT::datatable(rownames = FALSE, options = list(pageLength = 10, searching = TRUE)) })
    output$iade_total_count_ui <- renderUI({ df <- iade_performans_data(); req(df); total_count <- nrow(df); if (total_count == 0) return(NULL); tags$div( style = "text-align: right;", h5("Filtrelenen İade Sayısı:", style = "margin: 0; color: #7f8c8d; font-weight: normal;"), h4(format(total_count, big.mark = "."), style = "margin: 0; font-weight: bold;") ) })
    
    #--- 6. BÖLÜM: TÜM SİPARİŞLER ÖZETİ ---
    full_analysis_data <- reactiveVal(NULL)
    observeEvent(input$run_full_analysis_button, { showNotification("Tüm siparişler analiz ediliyor, lütfen bekleyin...", duration = NULL, type = "message", id="full_analysis_notification"); summary_df <- live_data_enriched() %>% left_join(work_steps_dict(), by = c("status" = "id")) %>% mutate( Durum_Adi = case_when( status == 0 ~ "Tanımsız Durum", !is.na(name) ~ name, TRUE ~ as.character(status) ) ) %>% count(Durum_Adi, name = "Adet", sort = TRUE); full_analysis_data(summary_df); shinyjs::show("full_analysis_results_panel", anim = TRUE, animType = "fade"); removeNotification("full_analysis_notification") })
    output$full_analysis_title <- renderText({ req(full_analysis_data()); total_orders <- sum(full_analysis_data()$Adet); paste0("Tüm Siparişlerin Durum Dağılımı (Toplam: ", format(total_orders, big.mark = ".", decimal.mark = ","), " Adet)") })
    output$full_analysis_table <- DT::renderDataTable({ req(full_analysis_data()); DT::datatable( full_analysis_data(), colnames = c("Sipariş Durumu", "Toplam Adet"), rownames = FALSE, options = list(pageLength = 15, searching = TRUE) ) })
    
  }) # moduleServer sonu
}
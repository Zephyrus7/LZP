# =================================================================
#           CANLI ANALİZ SUNUCU (SERVER) MODÜLÜ (BASİTLEŞTİRİLMİŞ SÜREÇ)
# =================================================================
# FİNAL DÜZELTME: "Altın Süreç" analizi, 'order_logs' yerine doğrudan
# 'orders' tablosundaki güvenilir tarihleri kullanarak basitleştirildi
# ve çalışır hale getirildi.
# =================================================================

server_live <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #--- 1. REAKTİF VERİ KURULUMU ---
    live_data <- reactive({ req(data()); data()$canli_veri_ham })
    
    #--- 2. HAREKETSİZ GÖNDERİLER (Sekme 1) ---
    hareketsiz_gonderiler_data <- reactive({
      req(input$hareketsiz_gun_esigi, live_data())
      threshold_days <- as.numeric(input$hareketsiz_gun_esigi)
      idle_shipments <- live_data() %>%
        filter(is.na(teslim_tarihi)) %>%
        mutate(Hareketsiz_Kalma_Suresi_Gun = difftime(Sys.time(), son_islem_tarihi, units = "days")) %>%
        filter(Hareketsiz_Kalma_Suresi_Gun > threshold_days) %>%
        arrange(desc(Hareketsiz_Kalma_Suresi_Gun)) %>%
        select(`Kargo Takip No`=kargo_no, `Kargo Firması`=kargo_firmasi, `Son Hareket Tarihi`=son_islem_tarihi, `Hareketsiz Kalma Süresi (Gün)`=Hareketsiz_Kalma_Suresi_Gun) %>%
        mutate(`Hareketsiz Kalma Süresi (Gün)` = round(as.numeric(`Hareketsiz Kalma Süresi (Gün)`), 1), `Son Hareket Tarihi` = format(as.POSIXct(`Son Hareket Tarihi`), "%Y-%m-%d %H:%M"))
      return(idle_shipments)
    })
    output$hareketsiz_gonderi_table <- DT::renderDataTable({
      df <- hareketsiz_gonderiler_data(); DT::datatable(df, rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, language = list(zeroRecords = "Bu filtreye uygun hareketsiz gönderi bulunamadı.")))
    })
    
    #--- 3. AÇIK GÖNDERİ DURUMU (Sekme 2) ---
    acik_gonderiler_summary <- reactive({
      # Bu bölümün kodu doğru ve çalışıyor, değişiklik yok.
      req(live_data()); acik_gonderi_idler <- live_data() %>% filter(is.na(teslim_tarihi)) %>% pull(order_id)
      if (length(acik_gonderi_idler) == 0) { return(tibble("Son Durum" = character(), Adet = integer())) }
      acik_gonderi_loglari <- dbGetQuery(db_pool_live, "SELECT order_id, created_at, proccess FROM order_logs WHERE order_id IN (?)", params = list(acik_gonderi_idler))
      work_steps_dict <- dbGetQuery(db_pool_live, "SELECT id, name FROM work_steps")
      if(nrow(acik_gonderi_loglari) == 0) return(tibble())
      son_durumlar_raw <- acik_gonderi_loglari %>% group_by(order_id) %>% slice_max(order_by = created_at, n = 1, with_ties = FALSE) %>% ungroup() %>% mutate(proccess_numeric = suppressWarnings(as.integer(proccess)))
      son_durumlar_tercume <- son_durumlar_raw %>% left_join(work_steps_dict, by = c("proccess_numeric" = "id")) %>% mutate(Son_Durum_Adi = coalesce(name, proccess), Son_Durum_Adi = if_else(is.na(Son_Durum_Adi) | Son_Durum_Adi == "", "Bilinmiyor", Son_Durum_Adi)) %>% count("Son Durum" = Son_Durum_Adi, name = "Adet", sort = TRUE)
      return(son_durumlar_tercume)
    })
    output$acik_gonderi_plot <- renderPlot({
      df <- acik_gonderiler_summary(); validate(need(nrow(df) > 0, "Görüntülenecek açık gönderi verisi bulunamadı."))
      ggplot(df, aes(x = reorder(`Son Durum`, Adet), y = Adet, fill = `Son Durum`)) + geom_col(show.legend = FALSE) + geom_text(aes(label = Adet), hjust = -0.2, size = 4.5) + coord_flip() + labs(x = "Mevcut Son Durum", y = "Açık Gönderi Adedi") + theme_minimal(base_size = 15) + scale_y_continuous(limits = c(0, max(df$Adet) * 1.15))
    })
    output$acik_gonderi_table <- DT::renderDataTable({
      df <- acik_gonderiler_summary(); DT::datatable(df, rownames = FALSE, options = list(dom = 't', paging = FALSE, searching = FALSE))
    })
    
    # === DEĞİŞİKLİK BURADA: Altın Süreç ve Karne Analizleri Güncellendi ===
    
    #--- 4. ALTIN SÜREÇ ANALİZİ (Sekme 3 - BASİTLEŞTİRİLDİ) ---
    altin_surec_data <- reactive({
      live_data() %>%
        filter(!is.na(teslim_tarihi) & !is.na(kargo_tarihi)) %>%
        mutate(
          Toplam_Sure_Saat = as.numeric(difftime(teslim_tarihi, kargo_tarihi, units = "hours"))
        ) %>%
        select(Toplam_Sure_Saat)
    })
    
    output$altin_surec_plot <- renderPlot({
      df <- altin_surec_data()
      validate(need(nrow(df) > 0 && sum(!is.na(df$Toplam_Sure_Saat)) > 0, "Hesaplanacak 'Toplam Süre' verisi bulunamadı."))
      
      ggplot(df, aes(x = "Toplam Teslim Süresi", y = Toplam_Sure_Saat)) +
        geom_boxplot(fill = "steelblue") +
        coord_flip() +
        labs(title = "Tamamlanmış Gönderilerin Toplam Teslim Süreleri (Saat)", x = "", y = "Süre (Saat)") +
        theme_minimal(base_size = 15)
    })
    
    output$altin_surec_table <- DT::renderDataTable({
      df <- altin_surec_data()
      df %>%
        summarise(
          "Süreç" = "Toplam Teslim Süresi (Saat)",
          Ortalama = round(mean(Toplam_Sure_Saat, na.rm = TRUE), 2),
          Medyan = round(median(Toplam_Sure_Saat, na.rm = TRUE), 2),
          "Max" = round(max(Toplam_Sure_Saat, na.rm = TRUE), 2),
          "İşlem Adedi" = n()
        ) %>%
        DT::datatable(., rownames = FALSE, options = list(dom = 't', paging = FALSE, searching = FALSE))
    })
    
    #--- 5. CANLI FİRMA KARNESİ (Sekme 4 - GÜNCELLENDİ) ---
    canli_karne_data <- reactive({
      live_data() %>%
        filter(!is.na(teslim_tarihi)) %>%
        mutate(
          # Başarı bayrağı için tahmini teslimat tarihinin de dolu olması gerekir
          basari_flag = if_else(
            !is.na(tahmini_teslimat_tarihi),
            as.integer(teslim_tarihi <= tahmini_teslimat_tarihi),
            NA_integer_
          ),
          teslim_suresi_saat = as.numeric(difftime(teslim_tarihi, kargo_tarihi, units = "hours"))
        ) %>%
        group_by(`Kargo Firması` = kargo_firmasi) %>%
        summarise(
          `Ortalama Teslim Süresi (Saat)` = round(mean(teslim_suresi_saat, na.rm = TRUE), 2),
          `Zamanında Teslim Oranı` = scales::percent(mean(basari_flag, na.rm = TRUE), accuracy = 0.1),
          `Tamamlanan Gönderi` = n()
        ) %>%
        arrange(desc(`Tamamlanan Gönderi`))
    })
    
    output$canli_karne_table <- DT::renderDataTable({
      DT::datatable(canli_karne_data(), rownames = FALSE, filter = 'top', options = list(pageLength = 10, scrollX = TRUE))
    })
    
    #--- 6. TEKİL KARGO TAKİP (Drill-Down) ---
    observeEvent(input$hareketsiz_gonderi_table_rows_selected, {
      secilen_kargo_no <- hareketsiz_gonderiler_data()[input$hareketsiz_gonderi_table_rows_selected, ]$`Kargo Takip No`
      show_log_modal(secilen_kargo_no)
    })
    
    show_log_modal <- function(takip_no) {
      loglar <- dbGetQuery(db_pool_live, "SELECT ol.created_at, ol.proccess FROM order_logs ol WHERE ol.order_id = (SELECT id FROM orders WHERE special_tracking_number = ?) ORDER BY ol.created_at DESC", params = list(takip_no))
      showModal(modalDialog(title = paste("Kargo Geçmişi:", takip_no), DT::renderDataTable(DT::datatable(loglar, colnames = c("Zaman", "Durum Adı"), rownames = FALSE)), footer = modalButton("Kapat"), size = "l"))
    }
  })
}
# server_live.R - HESAPLAMA MANTIĞI DÜZELTİLMİŞ VE EKSİKSİZ TAM HALİ

server_live <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    #--- 1. REAKTİF VERİ KURULUMU ---
    live_data <- reactive({
      req(data())
      data()
    })
    
    process_times_data <- reactive({ live_data()$process_times })
    raw_events_data <- reactive({ live_data()$raw_events })
    
    #--- 2. "ALTIN SÜREÇ" PERFORMANS ANALİZİ ---
    long_process_data <- reactive({
      req(process_times_data())
      process_times_data() %>%
        select(-`Kargo Takip No`) %>%
        pivot_longer(
          cols = everything(),
          names_to = "Surec_Adi",
          values_to = "Sure_Saat"
        ) %>%
        filter(!is.na(Sure_Saat))
    })
    
    altin_surec_summary <- reactive({
      req(long_process_data())
      long_process_data() %>%
        group_by("Süreç Adı" = Surec_Adi) %>%
        summarise(
          "Ortalama (Saat)" = mean(Sure_Saat, na.rm = TRUE),
          "Medyan (Saat)" = median(Sure_Saat, na.rm = TRUE),
          "75. Persentil" = quantile(Sure_Saat, 0.75, na.rm = TRUE),
          "95. Persentil" = quantile(Sure_Saat, 0.95, na.rm = TRUE),
          "İşlem Adedi" = n()
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
    })
    
    output$altin_surec_plot <- renderPlot({
      req(long_process_data())
      ggplot(long_process_data(), aes(x = reorder(Surec_Adi, Sure_Saat, median), y = Sure_Saat, fill = Surec_Adi)) +
        geom_boxplot(show.legend = FALSE) +
        coord_flip() +
        labs(
          title = '"Altın Süreç" Adımlarının Süre Dağılımları',
          x = "Süreç Adımı",
          y = "Tamamlanma Süresi (İş Saati)"
        ) +
        theme_minimal(base_size = 14)
    })
    
    output$altin_surec_table <- DT::renderDataTable({
      req(altin_surec_summary())
      DT::datatable(
        altin_surec_summary(),
        rownames = FALSE,
        options = list(dom = 't', paging = FALSE)
      )
    })
    
    #--- 3. AÇIK GÖNDERİ DURUM DAĞILIMI ---
    acik_gonderiler_summary <- reactive({
      req(raw_events_data())
      
      terminal_statuses <- c("complete", "refund", "lost", "cancelled")
      
      closed_shipments <- raw_events_data() %>%
        filter(status_value %in% terminal_statuses) %>%
        distinct(order_tracking_no)
      
      open_shipments_last_status <- raw_events_data() %>%
        anti_join(closed_shipments, by = "order_tracking_no") %>%
        group_by(order_tracking_no) %>%
        slice_max(order_by = event_timestamp, n = 1) %>%
        ungroup() %>%
        count("Son Durum" = status_name, name = "Adet", sort = TRUE)
      
      return(open_shipments_last_status)
    })
    
    output$acik_gonderi_plot <- renderPlot({
      df <- acik_gonderiler_summary()
      req(nrow(df) > 0)
      ggplot(df, aes(x = reorder(`Son Durum`, Adet), y = Adet, fill = `Son Durum`)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = Adet), hjust = -0.2, size = 4) +
        coord_flip() +
        labs(
          title = "Açık Gönderilerin Anlık Durum Dağılımı",
          x = "Mevcut Durum",
          y = "Gönderi Adedi"
        ) +
        theme_minimal(base_size = 14) +
        scale_y_continuous(limits = c(0, max(df$Adet) * 1.1))
    })
    
    #--- 4. HAREKETSİZ GÖNDERİLER (ALARM LİSTESİ) ---
    hareketsiz_gonderiler_data <- reactive({
      req(input$hareketsiz_gun_esigi, raw_events_data())
      
      threshold_hours <- as.numeric(input$hareketsiz_gun_esigi) * 24
      
      terminal_statuses <- c("complete", "refund", "lost", "cancelled")
      
      closed_shipments <- raw_events_data() %>%
        filter(status_value %in% terminal_statuses) %>%
        distinct(order_tracking_no)
      
      # DÜZELTİLMİŞ VE DAHA SAĞLAM MANTIK
      idle_shipments <- raw_events_data() %>%
        # 1. Bitmiş kargoları veri setinden çıkar
        anti_join(closed_shipments, by = "order_tracking_no") %>%
        # 2. Kalan her bir kargo için, SADECE en son olayı al
        group_by("Kargo Takip No" = order_tracking_no) %>%
        slice_max(order_by = event_timestamp, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        # 3. Şimdi hareketsiz kalma süresini hesapla
        mutate(
          "Hareketsiz Kalma Süresi (Saat)" = difftime(Sys.time(), event_timestamp, units = "hours")
        ) %>%
        # 4. Eşikten büyük olanları filtrele
        filter(`Hareketsiz Kalma Süresi (Saat)` > threshold_hours) %>%
        # 5. Sonuçları düzenle ve sırala
        arrange(desc(`Hareketsiz Kalma Süresi (Saat)`)) %>%
        select(
          `Kargo Takip No`,
          "Son Durum" = status_name,
          "Son Hareket Tarihi" = event_timestamp,
          `Hareketsiz Kalma Süresi (Saat)`
        ) %>%
        mutate(
          `Hareketsiz Kalma Süresi (Saat)` = round(as.numeric(`Hareketsiz Kalma Süresi (Saat)`), 1),
          `Son Hareket Tarihi` = format(as.POSIXct(`Son Hareket Tarihi`), "%Y-%m-%d %H:%M:%S")
        )
      
      return(idle_shipments)
    })
    
    output$hareketsiz_gonderi_table <- DT::renderDataTable({
      df <- hareketsiz_gonderiler_data()
      # Veri olmasa bile boş bir tablo göstermek, bir hatadan daha iyidir.
      # Bu yüzden req() kullanmıyoruz.
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 10, 
          searching = TRUE, 
          scrollX = TRUE,
          # Veri yoksa gösterilecek mesaj
          language = list(zeroRecords = "Bu filtreye uygun hareketsiz gönderi bulunamadı.")
        )
      )
    })
    
  })
}
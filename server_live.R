# =================================================================
#           CANLI ANALİZ SUNUCU (SERVER) MODÜLÜ (FİNAL DÜZELTMELER)
# =================================================================
# GÜNCELLEME 1: Havuz (Pool) uyarısını gidermek için `work_steps_dict`
#               içindeki sorgu `poolWithTransaction` içine alındı.
# GÜNCELLEME 2: Grafiklerin Dark Mode'da doğru görünmesi için,
#               uygulamanın CSS'i ile uyumlu özel bir ggplot tema
#               fonksiyonu (`get_custom_theme`) oluşturuldu.
#               `ggdark` kütüphanesi kullanımdan kaldırıldı.
# =================================================================

server_live <- function(id, data, theme_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #--- YARDIMCI FONKSİYON: ÖZEL GRAFİK TEMASI ---
    get_custom_theme <- function(current_theme) {
      if (current_theme == "dark") {
        # Statik analizdeki profesyonel görünümü taklit eden özel koyu tema
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
        # Standart açık tema
        theme_minimal(base_size = 15)
      }
    }
    
    #--- 1. REAKTİF VERİ KURULUMU ve SÖZLÜKLER ---
    live_data <- reactive({ req(data()); data()$canli_veri_ham })
    
    # DÜZELTME: Sorgu, havuz sızıntısını önlemek için poolWithTransaction içine alındı.
    work_steps_dict <- reactive({
      poolWithTransaction(db_pool_live, function(conn) {
        dbGetQuery(conn, "SELECT id, name, value FROM work_steps")
      })
    })
    
    
    #--- 2. HAREKETSİZ GÖNDERİLER (Sekme 1) ---
    hareketsiz_gonderiler_data <- reactive({
      req(input$hareketsiz_gun_esigi, live_data())
      threshold_days <- as.numeric(input$hareketsiz_gun_esigi)
      live_data() %>%
        filter(is.na(teslim_tarihi)) %>%
        mutate(Hareketsiz_Kalma_Suresi_Gun = difftime(Sys.time(), son_islem_tarihi, units = "days")) %>%
        filter(Hareketsiz_Kalma_Suresi_Gun > threshold_days) %>%
        arrange(desc(Hareketsiz_Kalma_Suresi_Gun)) %>%
        select(`Kargo Takip No`=kargo_no, `Kargo Firması`=kargo_firmasi, `Son Hareket Tarihi`=son_islem_tarihi, `Hareketsiz Kalma Süresi (Gün)`=Hareketsiz_Kalma_Suresi_Gun) %>%
        mutate(`Hareketsiz Kalma Süresi (Gün)` = round(as.numeric(`Hareketsiz Kalma Süresi (Gün)`), 1), `Son Hareket Tarihi` = format(as.POSIXct(`Son Hareket Tarihi`), "%Y-%m-%d %H:%M"))
    })
    output$hareketsiz_gonderi_table <- DT::renderDataTable({
      df <- hareketsiz_gonderiler_data(); DT::datatable(df, rownames = FALSE, selection = 'single', options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, language = list(zeroRecords = "Bu filtreye uygun hareketsiz gönderi bulunamadı.")))
    })
    
    
    #--- 3. AÇIK GÖNDERİ DURUMU (Sekme 2 - Bağımsız Filtre ile) ---
    output$firma_filtre_ui_acik <- renderUI({
      req(live_data())
      firma_listesi <- live_data() %>% filter(is.na(teslim_tarihi)) %>% pull(kargo_firmasi) %>% unique() %>% sort()
      selectInput(inputId = ns("firma_secimi_acik"), label = "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi), selected = "TUMU")
    })
    
    acik_gonderiler_summary <- reactive({
      req(live_data(), work_steps_dict(), input$firma_secimi_acik)
      validate(need("status" %in% names(live_data()), "Gerekli 'status' sütunu veri setinde bulunamadı."))
      
      ham_veri <- live_data()
      if (input$firma_secimi_acik != "TUMU") { ham_veri <- ham_veri %>% filter(kargo_firmasi == input$firma_secimi_acik) }
      
      ham_veri %>%
        filter(is.na(teslim_tarihi)) %>%
        left_join(work_steps_dict(), by = c("status" = "id")) %>%
        mutate(Durum_Adi = coalesce(name, as.character(status), "Bilinmiyor")) %>%
        count("Son Durum" = Durum_Adi, name = "Adet", sort = TRUE)
    })
    
    # DÜZELTME: Grafik artık özel temayı kullanıyor
    output$acik_gonderi_plot <- renderPlot({
      df <- acik_gonderiler_summary(); validate(need(nrow(df) > 0, "Seçilen firma için görüntülenecek açık gönderi verisi bulunamadı."))
      
      p <- ggplot(df, aes(x = reorder(`Son Durum`, Adet), y = Adet, fill = `Son Durum`)) + 
        geom_col(show.legend = FALSE) + 
        geom_text(aes(label = Adet), hjust = -0.2, size = 4.5, color = if(theme_reactive()=="dark") "white" else "black") + 
        coord_flip() + 
        labs(x = NULL, y = "Açık Gönderi Adedi") + 
        scale_y_continuous(limits = c(0, max(df$Adet) * 1.15))
      
      p + get_custom_theme(theme_reactive())
    }, bg="transparent") # Arka planı transparan yaparak CSS'in rengini almasını sağla
    
    output$acik_gonderi_table <- DT::renderDataTable({ df <- acik_gonderiler_summary(); DT::datatable(df, rownames = FALSE, options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE)) })
    
    
    #--- 4. ALTIN SÜREÇ ANALİZİ (Sekme 3) ---
    altin_surec_data <- reactive({ live_data() %>% filter(!is.na(teslim_tarihi) & !is.na(kargo_tarihi)) %>% mutate(Toplam_Sure_Saat = as.numeric(difftime(teslim_tarihi, kargo_tarihi, units = "hours"))) %>% select(Toplam_Sure_Saat) })
    
    # DÜZELTME: Grafik artık özel temayı kullanıyor
    output$altin_surec_plot <- renderPlot({
      df <- altin_surec_data(); validate(need(nrow(df) > 0 && sum(!is.na(df$Toplam_Sure_Saat)) > 0, "Hesaplanacak 'Toplam Süre' verisi bulunamadı."))
      
      p <- ggplot(df, aes(x = "Toplam Teslim Süresi", y = Toplam_Sure_Saat)) + 
        geom_boxplot(fill = "steelblue") + 
        coord_flip() + 
        labs(y = "Süre (Saat)", x = NULL, title = "Tamamlanmış Gönderilerin Toplam Teslim Süreleri (Saat)")
      
      p + get_custom_theme(theme_reactive())
    }, bg="transparent")
    
    output$altin_surec_table <- DT::renderDataTable({ df <- altin_surec_data(); df %>% summarise("Süreç" = "Toplam Teslim Süresi (Saat)", Ortalama = round(mean(Toplam_Sure_Saat, na.rm = TRUE), 2), Medyan = round(median(Toplam_Sure_Saat, na.rm = TRUE), 2), "Max" = round(max(Toplam_Sure_Saat, na.rm = TRUE), 2), "İşlem Adedi" = n()) %>% DT::datatable(., rownames = FALSE, options = list(dom = 't', paging = FALSE, searching = FALSE)) })
    
    
    #--- 5. CANLI FİRMA KARNESİ (Sekme 4) ---
    canli_karne_data <- reactive({ live_data() %>% filter(!is.na(teslim_tarihi)) %>% mutate(basari_flag = if_else(!is.na(tahmini_teslimat_tarihi), as.integer(teslim_tarihi <= tahmini_teslimat_tarihi), NA_integer_), teslim_suresi_saat = as.numeric(difftime(teslim_tarihi, kargo_tarihi, units = "hours"))) %>% group_by(`Kargo Firması` = kargo_firmasi) %>% summarise(`Ortalama Teslim Süresi (Saat)` = round(mean(teslim_suresi_saat, na.rm = TRUE), 2), `Zamanında Teslim Oranı` = scales::percent(mean(basari_flag, na.rm = TRUE), accuracy = 0.1), `Tamamlanan Gönderi` = n()) %>% arrange(desc(`Tamamlanan Gönderi`)) })
    output$canli_karne_table <- DT::renderDataTable({ DT::datatable(canli_karne_data(), rownames = FALSE, filter = 'top', options = list(pageLength = 10, scrollX = TRUE)) })
    
    
    #--- 6. ANLIK SİPARİŞ DURUM DAĞILIMI (Sekme 5) ---
    output$firma_filtre_ui_dagilim <- renderUI({
      req(live_data())
      firma_listesi <- live_data() %>% pull(kargo_firmasi) %>% unique() %>% sort()
      selectInput(inputId = ns("firma_secimi_dagilim"), label = "Kargo Firması:", choices = c("Tüm Firmalar" = "TUMU", firma_listesi), selected = "TUMU")
    })
    
    siparis_durum_dagilimi_data <- reactive({
      req(live_data(), work_steps_dict(), input$firma_secimi_dagilim)
      validate(need("status" %in% names(live_data()), "Gerekli 'status' sütunu veri setinde bulunamadı."))
      
      ham_veri <- live_data()
      if (input$firma_secimi_dagilim != "TUMU") { ham_veri <- ham_veri %>% filter(kargo_firmasi == input$firma_secimi_dagilim) }
      
      ham_veri %>%
        left_join(work_steps_dict(), by = c("status" = "id")) %>%
        mutate(Durum_Adi = coalesce(name, as.character(status), "Bilinmiyor")) %>%
        count("Sipariş Durumu" = Durum_Adi, name = "Adet", sort = TRUE)
    })
    output$surec_performans_table <- DT::renderDataTable({ df <- siparis_durum_dagilimi_data(); validate(need(nrow(df) > 0, "Görüntülenecek sipariş durum verisi bulunamadı.")); DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, searching = TRUE)) })
    
    
    #--- 7. TEKİL KARGO TAKİP (Drill-Down) ---
    observeEvent(input$hareketsiz_gonderi_table_rows_selected, {
      req(input$hareketsiz_gonderi_table_rows_selected)
      secilen_kargo_no <- hareketsiz_gonderiler_data()[input$hareketsiz_gonderi_table_rows_selected, ]$`Kargo Takip No`
      show_log_modal(secilen_kargo_no)
    })
    
    show_log_modal <- function(takip_no) {
      loglar_raw <- dbGetQuery(db_pool_live, "SELECT ol.created_at, ol.proccess, ol.proccess_time FROM order_logs ol JOIN orders o ON ol.order_id = o.id WHERE o.special_tracking_number = ? ORDER BY ol.created_at DESC", params = list(takip_no))
      s_dict <- work_steps_dict()
      loglar_tercume <- loglar_raw %>%
        mutate(proccess_time_int = suppressWarnings(as.integer(proccess_time))) %>%
        left_join(s_dict, by = c("proccess" = "value")) %>%
        left_join(s_dict, by = c("proccess_time_int" = "id"), suffix = c("_from_value", "_from_id")) %>%
        mutate(Durum = coalesce(name_from_value, name_from_id, proccess, "Bilinmiyor")) %>%
        select(Zaman = created_at, Durum) %>%
        mutate(Zaman = format(Zaman, "%Y-%m-%d %H:%M:%S"))
      showModal(modalDialog( title = paste("Kargo Geçmişi:", takip_no), DT::renderDataTable(DT::datatable(loglar_tercume, rownames = FALSE, options = list(pageLength = 10, searching = FALSE, scrollX = TRUE))), footer = modalButton("Kapat"), size = "l" ))
    }
    
  }) # moduleServer sonu
}
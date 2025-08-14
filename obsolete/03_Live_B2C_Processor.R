# 03_Live_B2C_Processor.R - NULL TAKİP NO FİLTRESİ (v1.2 - Nihai)

#==============================================================================#
#       CANLI B2C ANALİZ MOTORU (NULL Filtresi ve Kontroller v1.2)             #
#==============================================================================#
# GÜNCELLEME: Veritabanı sorgusuna, order_tracking_no'su NULL veya boş olan
#             "sahipsiz" olay kayıtlarını en başından temizleyen bir WHERE
#             koşulu eklendi. Bu, veri bütünlüğünü sağlar ve sonraki
#             hesaplamalarda hataları önler.
#==============================================================================#

hesapla_canli_surecler_b2c <- function(db_pool, progress_updater = NULL) {
  
  #--- 1. YARDIMCI FONKSİYONLAR ---
  
  update_progress <- function(amount, detail) {
    if (!is.null(progress_updater)) {
      progress_updater(amount = amount, detail = detail)
    }
  }
  
  calculate_business_hours <- function(start_vector, end_vector, holidays_df) {
    brut_saat <- as.numeric(difftime(end_vector, start_vector, units = "hours"))
    if (any(is.na(start_vector)) || any(is.na(end_vector))) return(NA_real_)
    start_days <- as.Date(start_vector)
    end_days <- as.Date(end_vector)
    num_sundays <- (as.integer(end_days) - as.integer(start_days) + wday(start_days, week_start = 1)) %/% 7
    pazar_saati_dusulecek <- num_sundays * 24
    num_holidays <- 0
    if (nrow(holidays_df) > 0) {
      count_holidays_in_range <- function(start_d, end_d) {
        relevant_holidays <- holidays_df$holiday_date[holidays_df$holiday_date >= start_d & holidays_df$holiday_date <= end_d]
        sum(wday(relevant_holidays, week_start = 1) != 7)
      }
      num_holidays <- mapply(count_holidays_in_range, start_days, end_days)
    }
    tatil_saati_dusulecek <- num_holidays * 24
    net_saat <- brut_saat - pazar_saati_dusulecek - tatil_saati_dusulecek
    return(pmax(0, net_saat, na.rm = TRUE))
  }
  
  
  #--- 2. GEREKLİ VERİLERİ VERİTABANINDAN ÇEKME (GÜNCELLENMİŞ) ---
  cat("--- HATA AYIKLAMA (Adım 1): Veritabanı Sorgusu Başlatılıyor ---\n")
  update_progress(amount = 0.1, detail = "Canlı olay verileri çekiliyor...")
  
  # GÜNCELLEME: order_tracking_no'su NULL veya boş olanları filtrele ve status_name'i ekle
  query_events <- "
    SELECT o.order_tracking_no, o.event_timestamp, ws.status_value, ws.status_name
    FROM live_orders o
    INNER JOIN live_work_steps ws ON o.status_id = ws.status_id
    WHERE o.order_tracking_no IS NOT NULL AND o.order_tracking_no != ''
  "
  events_long <- dbGetQuery(db_pool, query_events)
  
  if (nrow(events_long) == 0) {
    cat("--- HATA AYIKLAMA (Adım 1) BAŞARISIZ: Veritabanından geçerli (NULL olmayan) olay verisi çekilemedi! ---\n")
    showNotification("Analiz edilecek geçerli canlı B2C verisi bulunamadı.", type = "warning", duration = 8)
    return(NULL)
  }
  cat(sprintf("--- HATA AYIKLAMA (Adım 1) BAŞARILI: Veritabanından %d adet geçerli olay kaydı çekildi. ---\n", nrow(events_long)))
  
  edd_days_rules <- dbGetQuery(db_pool, "SELECT * FROM live_edd_days")
  holidays_df <- if (dbExistsTable(db_pool, "live_holidays")) {
    dbGetQuery(db_pool, "SELECT holiday_date FROM live_holidays") %>%
      mutate(holiday_date = as.Date(holiday_date))
  } else {
    data.frame(holiday_date = as.Date(character(0)))
  }
  
  #--- 3. ETKİN BAŞLANGIÇ ZAMANINI (EFFECTIVE START TIME) HESAPLAMA ---
  cat("--- HATA AYIKLAMA (Adım 2): Etkin Başlangıç Zamanı Hesaplanıyor ---\n")
  update_progress(amount = 0.2, detail = "Siparişlerin başlangıç zamanları ayarlanıyor...")
  
  initial_start_times <- events_long %>%
    group_by(order_tracking_no) %>%
    summarise(initial_event_time = min(as.POSIXct(event_timestamp), na.rm = TRUE), .groups = 'drop') %>%
    filter(is.finite(initial_event_time))
  
  calculate_effective_start <- function(timestamp, rules) {
    if(is.na(timestamp)) return(NA_POSIXct_)
    event_day_name <- weekdays(timestamp)
    event_hour <- hour(timestamp)
    rule <- rules %>% 
      filter(day_name == event_day_name, start_hour <= event_hour, end_hour > event_hour)
    if (nrow(rule) == 1) {
      return(timestamp + days(rule$added_day[1]))
    } else {
      return(timestamp)
    }
  }
  
  effective_start_times <- initial_start_times %>%
    mutate(
      effective_start_time = purrr::map_vec(initial_event_time, ~calculate_effective_start(., edd_days_rules))
    ) %>%
    select(order_tracking_no, effective_start_time)
  
  if (nrow(effective_start_times) == 0) {
    cat("--- HATA AYIKLAMA (Adım 2) BAŞARISIZ: Etkin başlangıç zamanı hesaplandıktan sonra hiç kargo kalmadı. ---\n")
    return(NULL)
  }
  cat(sprintf("--- HATA AYIKLAMA (Adım 2) BAŞARILI: %d kargo için başlangıç zamanı hesaplandı. ---\n", nrow(effective_start_times)))
  
  #--- 4. OLAY VERİSİNİ "GENİŞ FORMATA" ÇEVİRME (PIVOTING) ---
  cat("--- HATA AYIKLAMA (Adım 3): Veri Geniş Formata Çevriliyor ---\n")
  update_progress(amount = 0.2, detail = "Süreç adımları analiz için hazırlanıyor...")
  
  key_events <- c("accept", "up_transfer", "down_station", "complete", "start_refund")
  
  events_in_data <- unique(events_long$status_value)
  found_key_events <- intersect(key_events, events_in_data)
  if(length(found_key_events) == 0) {
    cat("--- HATA AYIKLAMA (Adım 3) KRİTİK HATA: Tanımlanan anahtar olaylardan ('key_events') HİÇBİRİ veritabanından gelen veride bulunamadı! ---\n")
    cat("Veride bulunan olay türleri:", paste(events_in_data, collapse=", "), "\n")
    return(NULL)
  }
  cat(sprintf("--- HATA AYIKLAMA (Adım 3) BİLGİ: Anahtar olaylardan %d tanesi veride bulundu: %s\n", length(found_key_events), paste(found_key_events, collapse=", ")))
  
  events_wide <- events_long %>%
    filter(status_value %in% key_events) %>%
    group_by(order_tracking_no, status_value) %>%
    summarise(event_time = min(as.POSIXct(event_timestamp), na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(
      names_from = status_value,
      values_from = event_time
    )
  
  if (nrow(events_wide) == 0) {
    cat("--- HATA AYIKLAMA (Adım 3) BAŞARISIZ: Veri geniş formata çevrildikten sonra hiç satır kalmadı. ---\n")
    return(NULL)
  }
  cat(sprintf("--- HATA AYIKLAMA (Adım 3) BAŞARILI: Veri geniş formata çevrildi, %d satır oluştu. ---\n", nrow(events_wide)))
  
  #--- 5. "ALTIN SÜREÇ" SÜRELERİNİ HESAPLAMA ---
  cat("--- HATA AYIKLAMA (Adım 4): Altın Süreç Süreleri Hesaplanıyor ---\n")
  update_progress(amount = 0.3, detail = "Altın Süreç metrikleri hesaplanıyor...")
  
  process_times <- events_wide %>%
    left_join(effective_start_times, by = "order_tracking_no") %>%
    rowwise() %>%
    mutate(
      Alim_Suresi_Saat = calculate_business_hours(effective_start_time, if("accept" %in% names(.)) accept else NA, holidays_df),
      Transfer_Suresi_Saat = calculate_business_hours(if("accept" %in% names(.)) accept else NA, if("down_station" %in% names(.)) down_station else NA, holidays_df),
      Teslimat_Operasyon_Suresi_Saat = calculate_business_hours(if("down_station" %in% names(.)) down_station else NA, if("complete" %in% names(.)) complete else NA, holidays_df),
      Toplam_Teslim_Suresi_Saat = calculate_business_hours(effective_start_time, if("complete" %in% names(.)) complete else NA, holidays_df)
    ) %>%
    ungroup()
  
  #--- 6. SONUCU DÖNDÜRME ---
  cat("--- HATA AYIKLAMA (Adım 5): Analiz Tamamlandı, Sonuçlar Döndürülüyor ---\n")
  
  final_data <- process_times %>%
    select(
      "Kargo Takip No" = order_tracking_no,
      Alim_Suresi_Saat,
      Transfer_Suresi_Saat,
      Teslimat_Operasyon_Suresi_Saat,
      Toplam_Teslim_Suresi_Saat
    )
  
  # YENİ: server_live'a ham veriyi de göndermek için sonucu list olarak döndür
  return(
    list(
      process_times = final_data,
      raw_events = events_long # server_live bu veriyi de kullanacak
    )
  )
}
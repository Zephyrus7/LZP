library(dplyr)
library(dbplyr)
library(DBI)
library(lubridate)

analiz_et_ve_skorla_live <- function(db_pool, start_date, end_date, progress_updater = NULL) {
  
  update_progress <- function(amount, detail) {
    if (!is.null(progress_updater)) {
      progress_updater(amount = amount, detail = detail)
    }
  }
  
  update_progress(amount = 0.1, detail = "Canlı veritabanı tablolarına bağlanılıyor...")
  orders_tbl <- tbl(db_pool, "orders")
  order_logs_tbl <- tbl(db_pool, "order_logs")
  work_types_tbl <- tbl(db_pool, "work_types")
  
  update_progress(amount = 0.2, detail = "Süreç adımlarının tarihleri çıkarılıyor...")
  
  process_dates_tbl <- order_logs_tbl %>%
    filter(proccess %in% c(24, 28, 39, 47, 25, 18)) %>%
    group_by(order_id, proccess) %>%
    summarise(event_date = min(created_at, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = proccess,
      values_from = event_date,
      names_prefix = "proccess_date_"
    )
  
  update_progress(amount = 0.5, detail = "Ana veriler birleştiriliyor...")
  
  # === DEĞİŞİKLİK BURADA: Sütun adları tam olarak belirtildi ===
  
  # 1. 'orders' tablosunu, ihtiyaç duyacağımız sütunları AÇIKÇA seçerek ve
  #    'created_at' sütununu en başından yeniden adlandırarak hazırla.
  base_orders_tbl <- orders_tbl %>%
    select(
      order_id = id,
      veri_tarihi = created_at, # <- EN KRİTİK DEĞİŞİKLİK
      status,
      deleted_at,
      work_type,
      special_tracking_number,
      delivered_at,
      estimated_delivery_date,
      last_move_date
    )
  
  # 2. Filtrelemeyi bu temiz ve belirsiz olmayan tablo üzerinden yap.
  filtered_orders_tbl <- base_orders_tbl %>%
    filter(
      !is.na(veri_tarihi),
      veri_tarihi >= !!start_date,
      veri_tarihi <= !!end_date,
      is.na(deleted_at)
    )
  
  # 3. Birleştirmeleri ve son sütun seçimini yap.
  ana_veri_sorgusu <- filtered_orders_tbl %>%
    left_join(process_dates_tbl, by = "order_id") %>%
    left_join(work_types_tbl, by = c("work_type" = "id")) %>%
    select(
      order_id,
      kargo_no = special_tracking_number,
      kargo_firmasi_orjinal = name,
      status,
      veri_tarihi,
      teslim_tarihi_orders = delivered_at,
      tahmini_teslimat_tarihi = estimated_delivery_date,
      son_islem_tarihi = last_move_date,
      kargo_kabul_tarihi = proccess_date_24,
      order_check_tarihi = proccess_date_28,
      iade_baslangic_tarihi = proccess_date_39,
      iade_teslim_alindi_tarihi = proccess_date_47,
      iade_teslim_edildi_tarihi = proccess_date_25,
      teslim_tarihi_logs = proccess_date_18
    )
  # =================================================================
  
  update_progress(amount = 0.8, detail = "Hesaplanmış canlı veriler R ortamına çekiliyor...")
  ana_veri_df <- ana_veri_sorgusu %>% collect()
  
  if (nrow(ana_veri_df) == 0) {
    showNotification("Seçilen tarih aralığında hiç Canlı Veri bulunamadı.", type = "warning")
    return(NULL)
  }
  
  update_progress(amount = 0.95, detail = "Son temizlik ve gruplama işlemleri yapılıyor...")
  
  canli_veri_zenginlesmis <- ana_veri_df %>%
    mutate(
      teslim_tarihi = coalesce(teslim_tarihi_logs, teslim_tarihi_orders),
      Kargo_Turu = case_when(
        kargo_firmasi_orjinal %in% c("Standart", "Alım Talebi", "Mağazaya Teslim", "Toplama Talebi", "Beymen",
                                     "Kolay İade", "Hepsi Burada", "Müşteriden İade", "Eve Teslim",
                                     "Aynı Gün Teslim", "Hediye") ~ "Standart Hizmetler",
        grepl("Trendyol", kargo_firmasi_orjinal, ignore.case = TRUE) ~ "Trendyol",
        TRUE ~ kargo_firmasi_orjinal
      )
    )
  
  return(
    list(
      canli_veri_zenginlesmis = canli_veri_zenginlesmis
    )
  )
}
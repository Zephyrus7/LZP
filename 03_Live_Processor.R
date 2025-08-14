# =================================================================
#      CANLI ANALİZ VERİ İŞLEME MOTORU (NİHAİ DÜZELTME)
# =================================================================
# DÜZELTME: Yeni stratejinin çalışması için gerekli olan 'status'
#           sütunu sorguya eklendi.
# =================================================================

# Gerekli kütüphaneler
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
  work_types_tbl <- tbl(db_pool, "work_types")
  
  update_progress(amount = 0.3, detail = "Canlı veriler birleştiriliyor...")
  
  ana_veri_sorgusu <- orders_tbl %>%
    filter(
      !is.na(cargo_date),
      cargo_date >= !!start_date,
      cargo_date <= !!end_date,
      is.na(deleted_at)
    ) %>%
    left_join(
      work_types_tbl,
      by = c("work_type" = "id"),
      suffix = c("_order", "_work_type")
    ) %>%
    select(
      order_id = id,
      kargo_no = special_tracking_number,
      kargo_firmasi = name_work_type,
      status = status, # <-- GÜNCELLENEN SATIR
      kargo_tarihi = cargo_date,
      teslim_tarihi = delivered_at,
      tahmini_teslimat_tarihi = estimated_delivery_date,
      son_islem_tarihi = last_move_date
    )
  
  update_progress(amount = 0.6, detail = "Hesaplanmış canlı veriler R ortamına çekiliyor...")
  ana_veri_df <- ana_veri_sorgusu %>% collect()
  
  if (nrow(ana_veri_df) == 0) {
    showNotification("Seçilen tarih aralığında hiç Canlı Veri bulunamadı.", type = "warning")
    return(NULL)
  }
  
  update_progress(amount = 0.95, detail = "Arayüz için hazırlanıyor...")
  
  return(
    list(
      canli_veri_ham = ana_veri_df
    )
  )
}
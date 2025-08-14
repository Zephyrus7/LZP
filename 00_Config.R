# 00_Config.R - GÜNCELLENMİŞ HAL (Asenkron İşlemler için Paketler Eklendi)

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

library(readxl)
library(openxlsx)
library(readr)
library(janitor)
library(ggplot2)
library(DT)
library(scales)
library(forcats)
library(DBI)      
library(pool)     
library(config)   
library(bcrypt)
library(RMariaDB)
library(config)
library(DBI)
library(prophet)

# === DEĞİŞİKLİK BURADA: YENİ PAKETLER EKLENDİ ===
library(future)          # Arka planda R kodunu çalıştırmak için temel motor
library(promises)        # 'future' paketini Shiny ile uyumlu hale getiren köprü
library(shinycssloaders) # Çıktılar hesaplanırken otomatik yükleme animasyonları (spinner) gösterir
# =================================================
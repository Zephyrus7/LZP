# 00_Config.R - Sadeleştirilmiş ve Nihai Hali

# Temel Shiny ve Veri Manipülasyon Kütüphaneleri
library(shiny)
library(shinythemes)
library(shinyjs)      # <-- Sadece bu gerekli
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Diğer Gerekli Kütüphaneler
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

# KALDIRILANLAR: promises, future, shinyLottie, lottieR ve plan() komutu kaldırıldı.
# Creacion de archivos en formato ORYZA
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022



### cargar requerimientos
library(tidyverse)
library(lubridate)
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/write_files/write_exp_oryza.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/write_files/write_soil_oryza.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/write_files/write_wth_oryza.R", encoding = "UTF-8")


##Crear Archivos climaticos
dir_wth <- "data/OUTPUTS/WTH/"
dir.create(dir_wth)
wth_data_exp %>% mutate(wth_data = map(wth_data, ~.x %>% impute_mean_wth)) %>%
  mutate(pwalk(., write_wth_oryza, multiyear = F, tag = T))

#crear archivos experimentales
dir_exp <- paste0("data/OUTPUTS/EXP/")
dir.create(dir_exp, showWarnings = FALSE)
map(data$data, ~write_exp_oryza(.x, dir_exp))

#crear archivos suelo
dir_soil <- paste0("data/OUTPUTS/SOIL/")
dir.create(dir_soil, showWarnings = FALSE)
map2(soil_by_loc, names(soil_by_loc),  ~write_soil_oryza(dir_soil, .y, .x, SATAV = 25, RIWCLI = 'NO'))










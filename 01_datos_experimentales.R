# Revision datos experimentales Fedearroz
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022

### cargar requerimientos
library(tidyverse)
library(readxl)
library(naniar)
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/utils_crop_model.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/eval_models.R", encoding = "UTF-8")


###  importar archivos one drive
path_data  <- "D:/OneDrive - CGIAR/Projects/ORYZA_2022/00_DATA/"
path_proj <- "data/INPUTS/"

files <- list.files(path_data, recursive = T, full.names = T, pattern = ".xlsx$")
files %>% map(~file.copy(.x, to = path_proj))

file.rename(list.files("data/INPUTS/", full.names = T),
            list.files("data/INPUTS/", full.names = T) %>%
              str_remove("_EPOCAS") %>% str_replace_all("FED", "F"))


cultivar <- "F67"

files_cultivar <- list.files(path_proj, pattern = fixed(cultivar))

sites <- str_sub(files_cultivar, 1,4)

data <- files_cultivar %>% 
  enframe(name = NULL, value = "file") %>%
    mutate(loc_cul = str_sub(file, 1,-6)) %>% 
  separate(col = loc_cul, into = c("localidad", "cultivar"), sep = "_") %>%
  mutate(data =  map(file, ~read_INPUT_data(paste0(path_proj, .))))


###### req load _eval oryza


###extract by component 


# WTH DATA

wth_data_exp <- data %>% mutate(wth_data = map(data, ~.x$WTH_obs)) %>% 
  select(localidad, wth_data) %>% 
#  group_by(localidad) %>% slice(1) %>% 
  unnest(wth_data) %>% mutate(DATE = as.Date(DATE), wspd = as.numeric(WVEL)) %>% 
  set_names(tolower(names(.))) %>% dplyr::select(-wvel) %>%
  dplyr::distinct() %>% 
  nest(data = -c(localidad:ws_id)) 
#%>%  mutate(plot_na = map2(loc_id, data, plot_na_wth))



### plot NA

map2(wth_data_exp$loc_id, wth_data_exp$data, 
     ~vis_miss(dplyr::select(.y, -date), warn_large_data = F) +
  #  facet_wrap(id ~.)
  labs(title = .x) +
  scale_y_continuous(breaks = seq(0, length(.y$date), by = 366), 
                     labels = cut.Date(.y$date, breaks = "1 years") %>% 
                       unique() %>% year()) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 0)))



##Crear Archivos climaticos


dir.create("data")
left_join(loc, wth_data) %>% mutate(path = "data/", stn = 1) %>%
  rename(lat = LATITUD, lon =  LONGITUD, elev = ASNM, id_name = loc_id) %>%
  select(path, id_name, wth_data, lat, lon, elev, stn) %>%
  mutate(pwalk(., write_wth_oryza))


#crear archivos experimentales
out_path <- paste0(getwd(), "/data/OUTPUTS/EXP/")
dir.create(out_path, showWarnings = FALSE)

map(data$data, ~write_exp_oryza(.x, out_path))

exp_files <- list.files(out_path)


for(y in 1:length(data$data)){
  
  data$data[[y]]$PLANT_gro = data$data[[y]]$PLANT_gro %>% 
    set_names(colnames(data$data[[y]]$PLANT_gro) %>% 
                str_replace_all(pattern = "_S_SD", replacement = "_SE") %>% 
                str_replace_all(pattern = "_SD", replacement = "_SE"))}



#Extract data by component

# Crop Phenology
phen <- extract_obs_var(data$data, "phen")

#Leaf Area Index
lai <- extract_obs_var(data$data, "lai")

#Shoot Dry Matter
dry_matter <- extract_obs_var(data$data, "dry_matter")

#Yield dry matter
yield <- extract_obs_var(data$data, "yield")




set <- exp_files%>%
  str_sub(1,-5) %>% enframe(name = NULL, value = "exp_file") %>%
  separate(exp_file, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_", remove = F) %>%
  mutate(ID = paste0(LOC_ID, TR_N, PROJECT))


obs_data <- obs_data %>%
  map(., ~.[[vars]]) %>%
  bind_rows() %>% 
  dplyr::select(-LOC_ID, -CULTIVAR) %>%
  nest(-c(ID)) %>% right_join(set, by= "ID") %>% unnest(data) %>%
  select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N)) 



    
    
    



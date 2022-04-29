# Revision/Importar datos experimentales Fedearroz 2022
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022

### cargar requerimientos
#library(tidyverse)
#library(lubridate)
#library(readxl)
#library(naniar)
#library(plotly)
#library(Hmisc)

source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/utils_crop_model.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/eval_models.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/graphics/INPUT_data_graphics.R", encoding = "UTF-8")

inpack(c("tidyverse", "lubridate", "readxl", "naniar", "plotly", "Hmisc"))

###  importar archivos one drive ##### 
path_data_raw  <- "D:/OneDrive - CGIAR/Projects/ORYZA_2022/00_DATA/" # directorio de Onedrive - datos crudos en INPUT_data.xlsx
path_proj <- "data/INPUTS/" # direcorio para copiar archivos excel de onedrive 


# esa parte solo se corre si voy a importar datos crudos --- if_not saltar a cultivar
files <- list.files(path_data_raw, recursive = T, full.names = T, pattern = ".xlsx$")
files %>% map(~file.copy(.x, to = path_proj))

file.rename(list.files(path_proj, full.names = T),
            list.files(path_proj, full.names = T) %>%
              str_remove("_EPOCAS") %>% str_replace_all("FED", "F"))

######

############################ ------


## Seleccionar el cultivar
cultivar <- "F67"

## seleciconar archivos por cultivar
files_cultivar <- list.files(path_proj, pattern = fixed(cultivar))

#Extract sites 
sites <- str_sub(files_cultivar, 1,4)

## Inporta datos de trabajo
data <- files_cultivar %>% 
  enframe(name = NULL, value = "file") %>%
    mutate(loc_cul = str_sub(file, 1,-6)) %>% 
  separate(col = loc_cul, into = c("localidad", "cultivar"), sep = "_") %>%
  mutate(data =  map(file, ~read_INPUT_data(paste0(path_proj, .))))

## Revisa columnas plant_gro -- INPUT data cambios
for(y in 1:length(data$data)){
  
  data$data[[y]]$PLANT_gro = data$data[[y]]$PLANT_gro %>% 
    set_names(colnames(data$data[[y]]$PLANT_gro) %>% 
                str_replace_all(pattern = "_S_S", replacement = "delete") %>% 
                str_replace_all(pattern = "_SD", replacement = "_SE")) %>% 
    dplyr::select(-contains("delete"))}



###extract data by component 

## Soil data ' todas localidades y experimentos MADR
soil_data <- read_csv(paste0("data/soil_data_final.csv" )) %>%
  mutate(SAMPLING_DATE = mdy(SAMPLING_DATE)) %>% rename(SC = SCARBON)


# perfil de suelo agrupado por localidad y profundidad - promedio por bootstraping - ver mean_boot() function 100 reps
soil_by_loc <- soil_data %>% group_by(LOC_ID, DEPTH_range) %>%
  summarize_if(is.numeric, .funs = mean_boot) %>%
  mutate(ID=LOC_ID, STC=get_STC(SAND, CLAY)) %>% ungroup() %>% split(.$ID)
  


## Extraer Datos climaticos

wth_data_exp <- data %>% mutate(wth_data = map(data, ~.x$WTH_obs)) %>% 
  select(localidad, wth_data) %>% 
#  group_by(localidad) %>% slice(1) %>% 
  unnest(wth_data) %>% 
  mutate(DATE = as.Date(DATE), wspd = suppressWarnings(as.numeric(WVEL))) %>% 
  set_names(tolower(names(.))) %>% dplyr::select(-wvel) %>%
  dplyr::distinct() %>% 
  nest(wth_data = -c(localidad:ws_id)) %>% 
  left_join(
    
    data$data %>% map("AGRO_man") %>% bind_rows() %>% 
      dplyr::select(LOC_ID, LATITUD, LONGITUD, ASNM) %>% 
      distinct() %>% set_names(c("localidad", "lat", "lon", "elev")) %>%
      group_by(localidad) %>% slice(1) %>% 
      ungroup(),
    by = "localidad") %>% rename(id_name = loc_id, stn = ws_id) %>% 
  mutate(path = "data/OUTPUTS/WTH/") %>%
  select(path, id_name, wth_data, lat, lon, elev, stn)


# plot NA - WTH (1 plot por estacion)

map2(wth_data_exp$id_name, wth_data_exp$wth_data, 
     ~vis_miss(dplyr::select(.y, -date), warn_large_data = F) +
  #  facet_wrap(id ~.)
  labs(title = .x) +
  scale_y_continuous(breaks = seq(0, length(.y$date), by = 366), 
                     labels = cut.Date(.y$date, breaks = "1 years") %>% 
                       unique() %>% year()) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 0)))




# Extract and plot - Growth and Development observed data by component

# Crop Phenology
phen <- extract_obs_var(data$data, "phen")
plot_phen_obs(phen) %>% ggplotly()

#Leaf Area Index
lai <- extract_obs_var(data$data, "lai")
plot_lai_obs(lai) %>% ggplotly() 

#Shoot Dry Matter
dry_matter <- extract_obs_var(data$data, "dry_matter")
plot_drymatter_obs(dry_matter) %>% ggplotly()

#Yield dry matter
yield <- extract_obs_var(data$data, "yield")
plot_yield_obs(yield) %>% ggplotly()


    
    



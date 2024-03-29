# Parametrizacion de variedades 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022


source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/run_tools/extract_drates_param.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/run_tools/run_drates_param.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/param_models/get_params_oryza.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/master/R_package/graphics/param_oryza_graphics.R", encoding = "UTF-8")




### Inputs sample data ### espeficificar nombre de cultivar - el mismo de libtos IMPUT_data.xlsx
cult <- str_replace(cultivar, fixed("F"), "FED")
exp_files <- str_subset(list.files("data/OUTPUTS/EXP/", pattern = "\\.exp$"), cult)
exp_files <- exp_files[str_detect(exp_files, c("YOCS_FED2000_MADRI_S1.exp" , "VVME_FED2000_COL_S2.exp", "YOCS_FED2000_MADRI_S3.exp", "VVME_FED2000_COL_S4.exp"), negate = T)]

### split data into calibration set (cal_set) and evaluation set (eval_set), proportion=0.7
set.seed(1234)
cal_set <- sample(exp_files, length(exp_files)*0.60)    
eval_set <- setdiff(exp_files, cal_set)

path_cal = 'data/OUTPUTS/'
download_ORYZA_Tools(path_cal)
run_drates_param(cal_set, path_cal)

### Extract and import params from PARAM.out
raw_params <- extract_drates_param(cal_set, path_cal)
tidy_params <- tidy_params_oryza(raw_params, method =  2)
yield_params <-  map(data$data, ~.x$YIELD_obs) %>% bind_rows()



###############################################################
### PARAM PLOTS

###  Development Rates 
DVR_plot1(tidy_params$DVR_tb, save_plot = "N")   #point
DVR_plot1(tidy_params$DVR_tb, save_plot = "N") %>% ggplotly() 

##  Biomass Partition 
BPART_plot1(tidy_params$PF_tb, save_plot = "N") %>% ggplotly()#all data
BPART_plot3(tidy_params$PF_tb, save_plot = "N") %>% ggplotly()#facet by pf

##  Fraction of carbohydrates allocated to stems that is stored as reserves (FSTR)
FSTR_plot(raw_params$FSTR_df, save_plot = "N")  # by site

##  Leaf death coefficient
DRLV_plot (raw_params$DRLV_df, llim = 1.1, save_plot = "N")# by 


## Spikelet growth factor - yield parameters
SPGF_tb <- map(data$data, SPGF_extract) %>% bind_rows
SPGF_plot(SPGF_tb, save_plot = "N") %>% ggplotly()
#SPGF <- SPGF_cal(SPGF_tb)


##################################################
# Crop Parametrization

## DVS is a numeric vector with lenght>3, Contain the proposed Development stage
#  DVS = 0.00 0.25 0.50 0.75 1.00 1.50 2.00 2.50
DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))
span = 0.75 ; nse = 4

## Compute mean, max and minimun value. Require set span and nse arguments, 
BPF_tb <- loess_crp_oryza(tidy_params$BPF_tb, DVS, span, nse)
plot_pf_loess(tidy_params$BPF_tb, BPF_tb, span, nse) #%>% ggplotly()

#DVS[2] <- 0.35
SLA_tb <- loess_crp_oryza(tidy_params$SLA_tb, DVS, span = 0.75)#[[1]] # 1 = mean, 2 = min, 3 = max
plot_sla_loess(tidy_params$SLA_tb, SLA_tb, span = 0.75, nse = nse) %>% ggplotly()






DVR_data <- 
BPF_data <-  
SLA_data <- 
yield_data <- 


















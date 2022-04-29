# Parametrizacion de variedades 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022


source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/run_tools/extract_drates_param.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/run_tools/run_drates_param.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/get_model_params/get_params_oryza.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/graphics/param_oryza_graphics.R", encoding = "UTF-8")




### Inputs sample data ### espeficificar nombre de cultivar - el mismo de libtos IMPUT_data.xlsx
cult <- str_replace(cultivar, fixed("F"), "FED")
exp_files <- str_subset(list.files("data/OUTPUTS/EXP/", pattern = "\\.exp$"), cult)

### split data into calibration set (cal_set) and evaluation set (eval_set), proportion=0.7
set.seed(1234)
cal_set <- sample(exp_files, length(exp_files)*0.60)    
eval_set <- setdiff(exp_files, cal_set)

path_cal = 'data/OUTPUTS/'
download_ORYZA_Tools(path_cal)
run_drates_param(cal_set, path_cal)

### Extract and import params from PARAM.out
raw_params <- extract_drates_param(cal_set, path_cal)
tidy_params <- tidy_params_oryza(raw_params, 2)



###############################################################
### PARAM PLOTS

###  Development Rates 
DVR_plot1(tidy_params$DVR_tb, save_plot = "N")   #point
DVR_plot1(tidy_params$DVR_tb, save_plot = "N") %>% ggplotly() 

##  Biomass Partition 
BPART_plot1(tidy_params$PF_tb, save_plot = "N") %>% ggplotly()#all data
BPART_plot3(tidy_params$PF_tb, save_plot = "N") #facet by pf

##  Fraction of carbohydrates allocated to stems that is stored as reserves (FSTR)
FSTR_plot(raw_params$FSTR_df, save_plot = "N")  # by site

##  Leaf death coefficient
DRLV_plot (raw_params$DRLV_df, llim = 1.1, save_plot = "Y")# by 


## Spikelet growth factor
SPGF_df <- map(data$data, SPGF_extract) %>% bind_rows
SPGF_plot(SPGF_df, save_plot = "N")

##################################################

## DVS is a numeric vector with lenght>3, Contain the proposed Development stage
#  DVS = 0.00 0.25 0.50 0.75 1.00 1.50 2.00 2.50
DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))
span = 0.75 ; nse = 4

## Compute mean, max and minimun value. Require set span and nse arguments, 
PF_tb <- loess_crp_oryza(tidy_params$PF_tb, DVS, span, nse)
plot_pf_loess(tidy_params$PF_tb, PF_tb, span, nse) %>% ggplotly()

DVS[2] <- 0.35
SLA_tb <- loess_crp_oryza(tidy_params$SLA_tb, DVS, span = 0.75, nse = 2)
plot_sla_loess(tidy_params$SLA_tb, SLA_tb, span = 0.75, 0.5, 2) %>% ggplotly()


SPGF <- SPGF_cal(SPGF_df)

















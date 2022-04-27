# Parametrizacion de variedades 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022


library(data.table)
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/run_tools/extract_drates_param.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/run_tools/run_drates_param.R", encoding = "UTF-8")




### Inputs sample data 
exp_files <- str_subset(list.files("data/OUTPUTS/EXP/", pattern = "\\.exp$"), "FED67")

### split data into calibration set (cal_set) and evaluation set (eval_set), proportion=0.7
set.seed(1234)
cal_set <- sample(exp_files, length(exp_files)*0.60)    
eval_set <- setdiff(exp_files, cal_set)

setwd(dir = 'data/OUTPUTS/')
download_ORYZA_Tools()
run_drates_param(cal_set)
extract_drates_param(cal_set)





## Plots for Development Rates 
DVR_plot1(DVR_df, save_plot = "N")  #point
DVR_plot2(DVR_df, save_plot = "N")  #density
DVR_plot3(DVR_df, save_plot = "N")  #stat summary

# Plots for LAI, SLA and CC
## Plots for Biomass Partition ('PF_m1': Oryza_manual,
#                               'PF_m2: Samurai)

BPART_plot1(PF_m1, save_plot = "N") #all data
BPART_plot2(PF_m1, save_plot = "N") #facet by site & pf
BPART_plot3(PF_m1, save_plot = "N") #facet by pf
# Calulate ORYZA CRP params to write CRP file
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2022



default_list <- tibble(
  
  #strsplit(clipboard(), "\n") %>% unlist() %>% paste(collapse = "', '")
  Model = c(rep("ORYZA_v3", 22), rep("DSSAT_CERES", 9), rep("AQUACROP", 16)),
  
  Component = c('Phenology', 'Phenology', 'Phenology', 'Phenology', 
                'Leaf and stem growth', 'Leaf and stem growth', 'Leaf and stem growth', 'Leaf and stem growth', 
                'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ',
                'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress', 
                'Phenology', 'Phenology', 'Phenology', 'Phenology', 
                'Leaf and stem growth', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Temperature and drought stress',
                'Phenology', 'Phenology', 'Phenology', 'Phenology', 'Leaf and stem growth', 'Leaf and stem growth', 'Leaf and stem growth',
                'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 
                'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress', 'Temperature and drought stress'),

  
  Parameter = c('DVRJ', 'DVRI', 'DVRP', 'DVRR', 
                'RGRLMX ', 'RGRLMN', 'SLAMAX', 'SLATB', 'FSTR', 'SPGF', 'WGRMX ', 'FSHTB', 'FLVTB', 'FSTTB ', 'FSOTB', 'DRLVT', 'ZRTMCD', 
                'ULLE', 'LLLE', 'FSWTD', 'COLDREP', 'CTSTER', 
                'P1', 'P2O', 'P2R', 'P5', 'PHINT', 'G1', 'G2', 'G3', 'G4', 
                'GDD_CCmax', 'GDD_FL', 'GDD_FLL', 'GDD_M', 'CGC', 'CC_senecence', 'CC_max', 'WP', 'HIo', 'GDD_HI', 'GDC', 'Zr', 'Kc', 'Ks_exp', 'Ks_polc', 'Ks_polh'),
  Unit = c('1/°Cd', '1/°Cd', '1/°Cd', '1/°Cd', '1/°Cd', '1/°Cd',
           'ha/kg', 'DVS - value (ha/kg)', 'fraction', 'No/kg', 'kg/grain', 'DVS - fraction', 'DVS - fraction', 'DVS - fraction', 'DVS - fraction', 'DVS - coeff', 'm', 
           'kPa', 'kPa', 'fraction', '°C', '°C',
           'GDD', 'h', 'GDD', 'GDD', 'GDD', 'No/g', 'g', 'scaler value', 'scaler value',
           'GDD', 'GDD', 'GDD', 'GDD', '%/d', 'GDD', '%', 'g/m²', '%', 'GDD', '%/GDD', 'm', 'KcTr', 'p-exp', '°C', '°C'),
  Base = list('0.000656', '0.0007576', '0.000681', '0.002351', '0.0085', '0.004', '0.0045',
              tibble(DVS = c(0.00,	0.16,	0.33,	0.65,	0.79,	2.10,	2.50),
              Value = c(0.0045,	0.0045,	0.0033,	0.0028,	0.0024,	0.0023,	0.0023)), 
           '0.2', '64900', '0.0000249', 
           tibble(DVS = c(0,0.43,1,2.5),
                  FSH = c(0.5,0.75,1,1)), 
           'x', 'x', 'x', 'x', '0.4', 
           '1.45', '1404', '0.4', '21', '36.5', 
           '500', '12', '160', '450', '83', '60', '0.025', '1', '1', 
           '370', '1150', '350', '1900', '0.12257', '1300', '0.95', '19', '43', '680', '0.0933', '0.5', '1.1', '0.4', '8', '35'),
  Min = c('0.0004592', '0.00053032', '0.0004767', '0.0016457', '0.00595', '0.0028', '0.00315', 
          'x',
          '0.14', '45430', '0.00001743', 
          'x', 'x', 'x', 'x', 'x', '0.28', 
          '1.015', '982.8', '0.28', '14.7', '25.55', 
          '350', '8.4', '112', '315', '58.1', '42', '0.0175', '0.7', '0.7', 
          '259', '805', '245', '1330', '0.085799', '910', '0.665', '13.3', '30.1', '476', '0.06531', '0.35', '0.77', '0.28', '5.6', '24.5'),
  Max = c('0.0008528', '0.00098488', '0.0008853', '0.0030563', '0.01105', '0.0052', '0.00585', 
          'x', 
          '0.26', '84370', '0.00003237',
          'x', 'x', 'x', 'x', 'x', '0.52', 
          '1.885', '1825.2', '0.52', '27.3', '47.45', 
          '650', '15.6', '208', '585', '107.9', '78', '0.0325', '1.3', '1.3', 
          '481', '1495', '455', '2470', '0.159341', '1690', '1.235', '24.7', '55.9', '884', '0.12129', '0.65', '1.43', '0.52', '10.4', '45.5'),
  
  Description = c('Development rate in juvenile phase', 'Development rate in photoperiod-sensitive phase', 'Development rate in panicle development', 'Development rate in reproductive phase', 
                  'Maximum relative growth rate of leaf area', 'Minimum relative growth rate of leaf area', 'Maximum value of SLA', 
                  'Table of specific leaf area', 'Fraction of carbohydrates allocated to stems that is stored as reserves', 'Spikelet growth factor', 'Maximum individual grain weight', 
                  'Table of fraction total dry matter partitioned to the shoot', 'Table of fraction shoot dry matter partitioned to the leaves', 'Table of fraction shoot dry matter partitioned to the stems', 'Table of fraction shoot dry matter partitioned to the panicles', 'Table of leaf death coefficient', 
                  'Maximum depth of roots if drought', 'Upper limit leaf expansion', 'Lower limit leaf expansion', 'Ratio of remaining available water to total water supply capability - transpiration eq', 'The threshold temperature for cold caused sterility', 'The threshold temperature for heat caused sterility', 
                  'Basic vegetative phase', 'Critical photoperiod or the longest day length', 'Extent to which phasic development leading to panicle initiation is delayed', 'Time period from beginning of grain filling', 'Phylochron interval', 'Potential spikelet number coefficient', 'Single grain weight', 'Tillering coefficient', 'Temperature tolerance coefficient', 
                  'from emergence to maximum rooting depth', 'from emergence to flowering', 'Length of the flowering stage', 'from emergence to maturity', 'Canopy Growth Coefficient', 'GDD from emergence to start senescence', 'Maximun canopy cover', 'Crop Water Productivity', 'Reference Harvest Index', 'Building-up of Harvest Index during yield formation', 'Canopy Decline Coefficient', 'Maximum effective rooting depth', 
                  'Crop coefficient when canopy is complete', 'Soil water depletion factor for canopy expansion- Lower threshold', 'Minimum air temperature below which pollination starts to fail', 'Maximum air temperature above which pollination starts to fail')
  
  
  
)


default_list$Base

  
tidy_params_oryza <- function(raw_params, method = 1){
  
  if(method == 1){
    
    
    ### Method 1. According to ORYZA_V3_User_Manual_2014
    ### Apply filters by DVSM 
    BPF_tb <- raw_params$BPART_df %>% 
      pivot_longer(cols = FLV:FSO, names_to = "Partition_Parameter", values_to = "Value") %>%
      mutate(
        #        Growth_phase = case_when(
        #            DVSM<=0.65 ~ "Vegetative",
        #            DVSM>0.65&DVSM<=1 ~"Reproductive",
        #            DVSM>1 ~"Ripening"),
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
          DVSM>1.1 &  Partition_Parameter == "FLV" ~ 0,
          DVSM>1.1 &  Partition_Parameter == "FST" ~ 0,
          DVSM>1.1 &  Partition_Parameter == "FSO" ~ 1,
          Value<0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    
    
  } else {
    
    ### Method 2. Grafical analysis, logical-physiological filters
    BPF_tb <- raw_params$BPART_df %>%
      filter(FLV>=-0.2, FLV<=1.2,
             FST>=-0.2, FST<=1.2,
             FST>=-0.2, FSO<=1.2) %>%
      pivot_longer(cols = FLV:FSO, names_to = "Partition_Parameter", values_to = "Value") %>%
      mutate(
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
          Partition_Parameter == "FSO" & DVSM>1 & Value<0.30   ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM>1 & Value>0.25   ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<1 & Value>0.75   ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<0.75 & Value<0.4 ~ NA_real_,
          Partition_Parameter == "FST" & DVSM>1 & Value>0.5    ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<1 & Value>0.80   ~ NA_real_,
          Value<0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    
    
  }
  
  
##### tidy Specific LEaf Area
SLA_df <- raw_params$LAI_df %>%
    select(exp_file, DVS, LAI) %>% na.omit() %>%
    left_join(raw_params$BMASS_df) %>%
    mutate(SLA = LAI/WLVG, Value = SLA) %>%
    filter(SLA < 0.0046) %>%
    dplyr::rename(DVSM=DVS)


# DVR_plots
DVR_tb <- raw_params$DVR_df %>%
  pivot_longer(cols = -exp_file, names_to = "DVR", values_to = "Value") %>%
  filter(Value<0.0046) %>% #DVR!= "DVRI", 
  mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR"))) 


 return(list(DVR_tb = DVR_tb, BPF_tb = BPF_tb, SLA_tb = SLA_df))

}

#tidy_params_oryza(params)


### SLA_df
SLA_max <- function(SLA_df, default = 0.0045, fr=0.50) {
  
  sla_max <- SLA_df %>%
    filter(DVSM<fr) %>%
    lm(SLA~DVSM, data = .) %>%
    summary() %>%
    .$coefficients %>% as_tibble() %>%
    mutate(par=Estimate+1.96*`Std. Error`) %>%
    .$par %>%
    .[1]
  
  
  if (sla_max < 0.0039) {
    
    message(paste("SLA_max calculated:", sla_max, "- is a low value.", "- Use default"))
    return(default)
    
  } else if (sla_max > 0.0051){
    
    message(paste("SLA_max
                  calculated:", sla_max, "- is a high value.", "- Use default"))
    return(default)
    
  } else {
    
    message(paste("SLA_max calculated:", sla_max, "- is a normal value.", "- Use data"))
    return(sla_max)
    
  }
}


## Function to Calculate Fraction of carbohydrates allocated to stems that is stored as reserves
FSTR_cal <- function(FSTR_df, default = 0.2){
  
  FSTR <- mean_boot(FSTR_df$FSTR)[[1]]
  
  
  if (FSTR < 0.14) {
    
    message(paste("FSTR calculated:", FSTR, "- is a low value.", "- Use default"))
    return(default)
    
  } else if (FSTR > 0.26){
    
    message(paste("FSTR calculated:", FSTR, "- is a high value.", "- Use default"))
    return(default)
    
  } else {
    
    message(paste("FSTR calculated:", FSTR, "- is a normal value.", "- Use data"))
    return(FSTR)

  }
  
}


## Function to plot Fraction of carbohydrates allocated to stems that is stored as reserves
FSTR_plot <- function(FSTR_df, save_plot = "N") {
  
  plot <- FSTR_df %>% filter(FSTR>0) %>% mutate(LOC_ID =  str_sub(exp_file, 1,4)) %>%
    ggplot(aes(LOC_ID, FSTR, label=exp_file)) +
    geom_jitter(width = 0.1) +
    stat_summary(fun.data = mean_se, color="red") +
    geom_hline(yintercept = mean(FSTR_df$FSTR), color="blue", linetype="twodash") +
    annotate("text", x = 0.65, y = mean(FSTR_df$FSTR), 
             label =  paste0("mean =\n", round(mean(FSTR_df$FSTR),3))) + 
    labs(title = paste0("Fraction of carbohydrates allocated to stems (stored as reserves) - ", cultivar),
         x="Locality") +
    theme_bw()
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("FSTR for ", cultivar, ".png"), width=7, height=3))
  
  plot
  
}


#WGRMX  = 0.0000249 ! Maximum individual grain weight (kg grain-1)
WGRMX_cal <- function(GW1000, default = 0.000025){
  
  wgrmx <- bootstrap_param(GW1000)
  
  #(kg grain-1)
  WGRMX <-  wgrmx[[1]]/(1000000)
  
  if (WGRMX < 0.000019) {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a low value.", "- Use default"))
    return(default)
    
  } else if (WGRMX > 0.000040){
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a high value.", "- Use default"))
    return(default)
    
  } else {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a normal value.", "- Use data"))
    return(WGRMX)
    
  }
  
  
  
  
  
  
}


# SPGF: The spikelet growth formation factor (SPGF; number kg−1) was derived as
#       the slope of the relationship between spikelet number m−2 and growth of the
#       crop between panicle initiation and flowering (g m-2)

## SPGF_cal function compute SPGF from experimental data, It take yield traits (spikelet number), 
##          growth data (biomass) and phenology observation to find the relationship between spikelet
##          number and and crop growth between panicle initiation and flowering.

## Function to extract variables (from INPUT_data.xls template) requiere to SPGF calculation
SPGF_extract <- function(INPUT_data, max_diff = 5) {
  
  
  ##Load data for each workbook (XLSX)   
  
  ##Extract Spikelets number from YIELD_obs and join with Phenology observations (PHEN_bs)   
  SPIK_by_EXP <- INPUT_data$YIELD_obs %>%
    mutate(SPIK_M2_avg=PAN_M2*GT_PAN,
           SPIK_M2_min=(PAN_M2-PAN_M2_SE)*(GT_PAN-GT_PAN_SE),
           SPIK_M2_max=(PAN_M2+PAN_M2_SE)*(GT_PAN+GT_PAN_SE))%>%
    left_join(INPUT_data$PHEN_obs, by="ID")%>%
    select(ID, contains("SPIK_M2"), IDAT, FDAT)
  
  ##Extract  Total dry weight at panicle initiation or closest sampling date
  WAGT_PI <- INPUT_data$PLANT_gro %>%
    inner_join(SPIK_by_EXP, by="ID") %>%
    mutate(diff_pi=abs(as.integer(difftime(SAMPLING_DATE, IDAT, units = "days"))),
           WAGT_PI=WAGT_OBS, 
           WAGT_PI_SE=WAGT_SE)%>%
    group_by(ID) %>%
    filter(diff_pi==min(diff_pi))%>%
    select(ID, diff_pi, contains("WAGT_PI"))
  
  ##Extract  Total dry weight at flowering initiation or closest sampling date    
  WAGT_FL <- INPUT_data$PLANT_gro %>%
    inner_join(SPIK_by_EXP, by="ID") %>%
    mutate(diff_fl=abs(as.integer(difftime(SAMPLING_DATE, FDAT, units = "days"))),
           WAGT_FL=WAGT_OBS, 
           WAGT_FL_SE=WAGT_SE)%>%
    group_by(ID) %>%
    filter(diff_fl==min(diff_fl))%>%
    select(ID, diff_fl, contains("WAGT_FL"))
  
  ##Join data and compute variables to SPGF calculation  
  SPIK_by_EXP %>%
    left_join(WAGT_PI, by = "ID")%>%left_join(WAGT_FL, by = "ID") %>%
    mutate(diff_pi_fl=(WAGT_FL-WAGT_PI)/10) %>%#(g/m²)
    filter(diff_fl<max_diff, diff_pi<max_diff) %>%
    mutate(LOC_ID=substr(ID, 1,4))
  
}


## Function to calculate SPGF by lm 

SPGF_cal <- function(SPGF_df, out="", default = 45000) {
  
  
  SPGF_df <- SPGF_df %>% filter(ID != out)
  
  
  ## Linear model between Spikelet number (number/ m²) and crop growth between panicle initiation and flowering (g/m²)
  lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
  
  ## Print SPGF compute from lm-slope
  spgf <-  sprintf("%.1f", coef(lm_spgf)[[2]]*1000) # Spikelet growth factor (no kg-1)"))
  

  
  if (as.numeric(spgf)<35000) {
    
    message(paste("SPGF calculated:", spgf, "- is a low value.", "- Use default"))
    return(default)
    
  } else if (as.numeric(spgf)> 65000){
    
    message(paste("SPGF calculated:", spgf, "- is a high value.", "- Use default"))
    return(default)
    
  } else {
    
    message(paste("SPGF calculated:", spgf, "- is a normal value.", "- Use data"))
    return(as.numeric(spgf))
    
  }
  
}


# Function to get mean, minimun and maximun partition factor tables
loess_crp_oryza <- function(data, DVS, span=0.5, nse=4) {
  
  #data convert to list
  if(any(str_detect(names(data), "SLA"))){
    SLA_max = SLA_max(data)
    data = list(SLA=data)
  } else {data=split(data, data$Partition_Parameter)}
  
  
  
  # Function to create crp tb
  crp_pf_tb <- function(data) {
    
    data %>% bind_rows(.id = "Partition_Parameter") %>%
      mutate(
        Value=case_when(
          Partition_Parameter != "FSO" & DVS == 0 ~ 0.5,
          Partition_Parameter == "FSO" & DVS > 1.5 ~ 1,
          Partition_Parameter == "FSO" & DVS < 0.75 ~ 0,
          Value<0 | is.na(Value) ~ 0,
          TRUE ~ Value)) %>%
      spread(Partition_Parameter, Value) %>%
      mutate(PF_sum=FLV+FSO+FST,
             PF_diff=1-PF_sum,
             FLV = case_when(
               DVS<1 ~ FLV+(PF_diff/2),
               TRUE ~ FLV),
             FST = case_when(
               DVS<1 ~ FST+(PF_diff/2),
               TRUE ~ FST),
             FSO = case_when(
               DVS>=1 ~ FSO+PF_diff,
               TRUE ~ FSO),
             PF_sum2= FLV+FSO+FST,
             Test_log = (FLV+FSO+FST)==1) %>%
      #            rename(DVSM=DVS) %>%
      select(DVS, FLV, FST, FSO) %>%
      gather(Partition_Parameter, Value, -1) %>%
      mutate(Partition_Parameter = factor(Partition_Parameter,
                                          c("FLV", "FST", "FSO"))) %>%
      select(Partition_Parameter, DVS, Value)
    
  }
  
  crp_sla_tb <- function(data, SLA_max) {
    
    data %>% bind_rows() %>%
      mutate(
        Value=case_when(
          DVS == 0 ~ SLA_max,
          Value < 0 | is.na(Value) ~ min(Value, na.rm = T),
          TRUE ~ Value))
    
  }
  
  
  
  
  #Loess model by Partition factor    
  mod1 <- lapply(data, function(x){loess(Value~DVSM, data = x, span = span)}) %>%
    #predicted_list
    lapply(., function(x){predict(x, newdata = DVS, se=T)})
  
  #Cal mean
  PTB_crp_mean <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                      Value=x$fit)})
  #Cal min
  PTB_crp_min <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                     Value=x$fit+nse*x$se.fit)})
  #Cal_max
  PTB_crp_max <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                     Value=x$fit-nse*x$se.fit)})
  
  
  if(any(str_detect(names(data), "SLA"))){
    crp_list <- list(
      mean = crp_sla_tb(PTB_crp_mean, SLA_max),
      min =  crp_sla_tb(PTB_crp_max, SLA_max),
      max =  crp_sla_tb(PTB_crp_min, SLA_max))
  } else {
    crp_list <- list(
      mean = crp_pf_tb(PTB_crp_mean),
      min = crp_pf_tb(PTB_crp_min),
      max = crp_pf_tb(PTB_crp_max))}
  
  
  return(crp_list)
  
}
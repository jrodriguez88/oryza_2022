
bootstrap_param <- function(param_data, reps = 2000, ci = 0.95, stat = "mean"){
  
  require(Hmisc)
  
  stat <- switch (stat,
                  "mean" = 1,
                  "min" = 2,
                  "max" = 3
  )
  
  smean.cl.boot(param_data, conf.int=ci, B=reps, na.rm=TRUE, reps=T)[stat]
  
  
}


tidy_crop_oryza <- function(raw_params, tidy_params, yield_params, BPF_tb, SLA_tb, SPGF_tb){
  
    DVR_data <- raw_params$DVR_df
    BPF_data <- tidy_params$BPF_tb 
    SLA_data <- tidy_params$SLA_tb
    yield_data <- yield_params
  
  
  paste_crp <- function(BPF_tb, param, metric){
    
    BPF_tb[[metric]] %>%
      spread(Partition_Parameter, Value) %>%
      select(DVS, all_of(param))
    
  }
  
  data <- list(
    
    
    # 1. Phenological development parameters  
    DVRJ = bootstrap_param(DVR_data$DVRJ),
    DVRI = bootstrap_param(DVR_data$DVRI),
    DVRP = bootstrap_param(DVR_data$DVRP),
    DVRR = bootstrap_param(DVR_data$DVRR),
    
    # 2. Leaf and stem growth parameters
    RGRLMX = 0.0085, 
    RGRLMN = 0.0040, 
    
    SLAMAX = SLA_max(SLA_data),
    SLATB = SLA_tb$mean,
    
    # 6. Growth parameters
    FSTR = bootstrap_param(raw_params$FSTR_df$FSTR),
    SPGF = SPGF_cal(SPGF_tb),
    WGRMX = WGRMX_cal(yield_params), 
    FSHTB = tibble(DVS = c(0,0.43,1,2.5),
                   FSH = c(0.5,0.75,1,1)),
    FLVTB = paste_crp(BPF_tb, "FLV", "mean"),
    FSTTB = paste_crp(BPF_tb, "FST", "mean"),
    FSOTB = paste_crp(BPF_tb, "FSO", "mean"),
    DRLVT = tibble(DVS = c(0, 0.6, 1, 1.6, 2.1, 2.5),
                   DRLV = c(0, 0, 0.015, 0.025, 0.05, 0.05)),
    # 8. Root parameters
    GZRT   = 0.01,
    ZRTMCW = 0.25,
    ZRTMCD = 0.45,
    
    # 9. Temperature and drought stress parameters
    COLDREP = 20., 
    CTSTER = 36.5, 
    ULLE = 1.45, 
    LLLE = 1404.,   
    FSWTD = 0.40
  )
  
  return(data)    
  
}


crop_params_oryza <- tidy_crop_oryza(raw_params, tidy_params, yield_params, BPF_tb, SLA_tb, SPGF_tb)



write_crop_oryza(path_cal, "F2000", crop_params_oryza)

enframe(crop_params_oryza, name = "Parameter") %>% unnest(value)

run_ORYZA(path_cal, cultivar, cal_set, "cal")
run_ORYZA(path_cal, cultivar, eval_set, "val")


#source('D:/03_DEVELOPER/ORYZA_Model_RTOOLS/RES_analyzer.R', encoding = 'UTF-8')
source('https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/RES_analyzer.R', encoding = 'UTF-8')
res_file <- str_subset(list.files(path, full.names = T), "_res.dat") %>% 
  str_subset(str_to_lower(cultivar))

sim_data_cal <- read_res_exp(res_file[1])
sim_data_eval <- read_res_exp(res_file[2])

#sim_data <- map(res_file, read_res_exp) %>% #set_names(res_file) %>%
#    bind_rows(.id = "res_file")







metrics_cal <- map(c("phen", "dry_matter", "lai",  "yield"), 
                   ~eval_sim_oryza(data$data, sim_data_cal, cal_set, .x, T)) %>% bind_rows()

metrics_eval <- map(c("phen", "dry_matter", "lai",  "yield"), 
                    ~eval_sim_oryza(data$data, sim_data_eval, eval_set, .x, T)) %>% bind_rows()


metrics_cal
metrics_eval

metrics_cal %>% unnest(data) %>% 
  mutate(locality = str_sub(exp_file, 1, 4)) %>%
  ggplot(aes(obs, sim, color = locality, label = exp_file)) + geom_point() +
  expand_limits(x = 0, y = 0) + 
  geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
  geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
  geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
  facet_wrap(~var, scales = "free") + 
  theme_bw() + labs(title = "Calibration set")


metrics_eval %>% unnest(data) %>% 
  mutate(locality = str_sub(exp_file, 1, 4)) %>%
  ggplot(aes(obs, sim, color = locality, label = exp_file)) + geom_point() +
  expand_limits(x = 0, y = 0) + 
  geom_abline(intercept = 0, slope = 1, linetype = "twodash", size=1)+
  geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", size=0.5, color = "red") +
  geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", size=0.5, color = "red") + 
  facet_wrap(~var, scales = "free") + 
  theme_bw() + labs(title = "Evaluation set")



metrics_eval$data[[10]] %>% mutate(locality = str_sub(exp_file, 1, 4)) %>% mutate(obs = obs/0.86) %>% 
  ggplot(aes(exp_file, obs, label = exp_file)) + 
  geom_point() +
  geom_errorbar(aes(ymin = obs - se, ymax = obs + se), width = .2, position = position_dodge(0.2)) +
  geom_point(aes(y = sim), color = "red") + 
  #    geom_vline(xintercept = mean(obs)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45),
    legend.position = "bottom",
    #        legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold")) +
  expand_limits(y = 0)








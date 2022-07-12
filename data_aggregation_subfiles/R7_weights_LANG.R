#########################
### R7_weights_LANG.R ###
#########################

getwd()
t_start <-Sys.time()

# run_lang_weights_generation = T/F    ------------------------------------------
if(run_lang_weights_generation == F){
  load("data/0_R_data/R7_weights.Rdata")
}else if(run_lang_weights_generation == T){
  
  # preprocessing cepii_dist again
  df_cepii_dist <- add_column(df_cepii_dist, comlang_both = 0, .after = "comlang_ethno")
  id_comlang_off_OR_ethno <- (df_cepii_dist$comlang_off == 1 | df_cepii_dist$comlang_ethno == 1)
  df_cepii_dist$comlang_both[id_comlang_off_OR_ethno] <- 1
  ifrm(id_comlang_off_OR_ethno)
  
  # check if OR operator worked properly
  x <- subset(df_cepii_dist, comlang_both ==1)
  df_cepii_dist$comlang_off %>% table
  x$comlang_off %>% table
  x$comlang_ethno %>% table
  x$comlang_both %>% table 
  F %in% ( (df_cepii_dist$comlang_off == 1 | df_cepii_dist$comlang_ethno == 1) == (df_cepii_dist$comlang_both ==1) )
  ifrm(x)

  
  # # LANG WEIGHTS > set weights for LANG and add computed values into df_MAIN    ---------------------------
  get_DEGROOT_weights("ld_w_lang_both", df_cepii_dist$iso2_o, df_cepii_dist$iso2_d, df_cepii_dist$comlang_both)

  # save_data_R7_weights = T/F  ----------------------------------------
  if(save_data_R7_weights == T){
    save(list =  ls(.GlobalEnv),file = "data/0_R_data/R7_weights.Rdata")
  } # end save_data_R7_weights
  
}# end run_grav_weights_generation


# end  ----------------------------------------
t_end <-Sys.time()
t_R7<-t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R7_weights_LANG.R", ";  runtime ", round(t_R7[[1]],2)," ", units(t_R7), sep=""))


ifrm(run_lang_weights_generation) 
ifrm(save_data_R7_weights)

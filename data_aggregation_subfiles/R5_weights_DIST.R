#########################
### R5_weights_DIST.R ###
#########################

getwd()
t_start <-Sys.time()

# run_dist_weights_generation = T/F    ------------------------------------------
if(run_dist_weights_generation == F){
  load("data/0_R_data/R5_weights.Rdata")
}else if(run_dist_weights_generation == T){

  # preprocessing cepii_dist, adding iso2 names
  df_cepii_dist <- add_column(df_cepii_dist, iso2_o = NA, .after = "iso_o")
  df_cepii_dist$iso2_o <- df_iso_names$iso2[match(df_cepii_dist$iso_o, df_iso_names$iso3)]
  df_cepii_dist <- add_column(df_cepii_dist, iso2_d = NA, .after = "iso_d")
  df_cepii_dist$iso2_d <- df_iso_names$iso2[match(df_cepii_dist$iso_d, df_iso_names$iso3)]
  
  # # DIST WEIGHTS > set weights for dist and add computed values into df_MAIN    ---------------------------
  get_DEGROOT_weights("ld_w_dist", df_cepii_dist$iso2_o, df_cepii_dist$iso2_d, df_cepii_dist$dist)
  
  # df_cepii_dist$dist_inv <- 1/df_cepii_dist$dist
  # get_DEGROOT_weights("ld_w_dist_inv", df_cepii_dist$iso2_o, df_cepii_dist$iso2_d, df_cepii_dist$dist_inv)


  # save_data_R5_weights = T/F  ----------------------------------------
  if(save_data_R5_weights == T){
    save(list =  ls(.GlobalEnv),file = "data/0_R_data/R5_weights.Rdata")
  } # end save_data_R5_weights
  
}# end run_dist_weights_generation


# end  ----------------------------------------
t_end <-Sys.time()
t_R5<-t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R5_weights_DIST.R", ";  runtime ", round(t_R5[[1]],2)," ", units(t_R5), sep=""))


ifrm(run_dist_weights_generation)
ifrm(save_data_R5_weights)  

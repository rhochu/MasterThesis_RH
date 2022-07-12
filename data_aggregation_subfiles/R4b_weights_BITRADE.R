#############################
### R4b_weights_BITRADE.R ###
#############################

getwd()
t_start <-Sys.time()

# sanity_check_bitrade = T/F    ------------------------------------------
if(sanity_check_bitrade == T){
  
  check_bitrade <- c(rep(NA, nrow(df_cepii_bitrade)))
  for(i in 1:length(check_bitrade)){
    df_cepii_bitrade$iso_o %in% df_cepii_bitrade$iso_d[i] %>% table
    df_cepii_bitrade$iso_d %in% df_cepii_bitrade$iso_o[i] %>% table
    
    x<-df_cepii_bitrade$iso_d %in% df_cepii_bitrade$iso_o[i] &
      df_cepii_bitrade$iso_o %in% df_cepii_bitrade$iso_d[i] 
    check_bitrade[i] <- as.vector(table(x))[2]
  }
  check_bitrade %>% table
  
  ifrm(check_bitrade)
  ifrm(x)
  ifrm(i)
}


# run_cepii_bitrade_weights_generation = T/F  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
if(run_cepii_bitrade_weights_generation == F){
  load("data/0_R_data/R4b_weights.Rdata")
}else if(run_cepii_bitrade_weights_generation ==T ){
  
  #  PRE-PROCESS BITRADE data    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # adding iso2 codes to the iso3 codes
  df_cepii_bitrade<- add_column(df_cepii_bitrade, iso2_o = NA, .after = "iso_o")
  df_cepii_bitrade$iso2_o <- df_iso_names$iso2[match(df_cepii_bitrade$iso_o, df_iso_names$iso3)]
  df_cepii_bitrade <- add_column(df_cepii_bitrade, iso2_d = NA, .after = "iso_d")
  df_cepii_bitrade$iso2_d <- df_iso_names$iso2[match(df_cepii_bitrade$iso_d, df_iso_names$iso3)]
  
  df_cepii_bitrade <- subset(df_cepii_bitrade, year == "2014")   # only take the most recent measurement and drop the rest of bitrade observations 
  id_FLOW_0 <- df_cepii_bitrade$FLOW %in% NA
  df_cepii_bitrade$FLOW[id_FLOW_0] <- df_cepii_bitrade$FLOW_0[id_FLOW_0]  # adding 0's to "FLOW" in case "FLOW" is NA and "FLOW_0" has an acutal 0 => meaning no observed trade, so zero trade
  NA %in% df_cepii_bitrade$FLOW  # no more NA's in FLOW column
  
  exrate_USDtoGBR_2014 <- subset(df_cepii_exrates, year == "2014" & iso == "USA")[,4]  # extracting exchange rate for most recent observation because everything is denoted in GBR pounds
  df_cepii_bitrade$FLOW_USD <- df_cepii_bitrade$FLOW / exrate_USDtoGBR_2014    # converted to USD
  ifrm(exrate_USDtoGBR_2014)
  
  # # BITRADE (cepii) WEIGHTS > set weights for BITRADE (cepii) and add computed values into df_MAIN    ---------------------------
  get_DEGROOT_weights("ld_w_bitrade_cepii", df_cepii_bitrade$iso2_o, df_cepii_bitrade$iso2_d, df_cepii_bitrade$FLOW_USD)
  
  # save_data_R4b_weights = T/F  ----------------------------------------
  if(save_data_R4b_weights == T){
    save(list =  ls(.GlobalEnv),file = "data/0_R_data/R4b_weights.Rdata")
  } # end save_data_R4b_weights
  
 ifrm(id_FLOW_0)
  
} # end run_cepii_bitrade_weights_generation


# end  ----------------------------------------
t_end <-Sys.time()
t_R4b<-t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R4b_weights_BITRADE.R", ";  runtime ", round(t_R4b[[1]],2)," ", units(t_R4b), sep=""))


ifrm(sanity_check_bitrade)
ifrm(run_cepii_bitrade_weights_generation)
ifrm(save_data_R4b_weights)  

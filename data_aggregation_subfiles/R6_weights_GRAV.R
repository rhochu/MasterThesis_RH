#########################
### R6_weights_GRAV.R ###
#########################

getwd()
t_start <-Sys.time()

# run_grav_weights_generation = T/F    ------------------------------------------
if(run_grav_weights_generation == F){
  load("data/0_R_data/R6_weights.Rdata")
}else if(run_grav_weights_generation == T){
  
  # preprocessing cepii_gdp_pop, adding iso2 names
  df_cepii_gdp_pop <- subset(df_cepii_gdp_pop, year == "2014")   # only take the most recent measurement and drop the rest of observations 
  df_cepii_gdp_pop <- add_column(df_cepii_gdp_pop, iso2 = NA, .after = "iso")
  df_cepii_gdp_pop$iso2 <- df_iso_names$iso2[match(df_cepii_gdp_pop$iso, df_iso_names$iso3)]

  # check if iso2 in bitrade also in gdp_pop
  id_notmactch_o <- unique(df_cepii_bitrade$iso2_o) %in% unique(df_cepii_gdp_pop$iso2)
  unique(df_cepii_bitrade$iso2_o)[!id_notmactch_o]

  id_notmactch_d <- unique(df_cepii_bitrade$iso2_d) %in% unique(df_cepii_gdp_pop$iso2)
  unique(df_cepii_bitrade$iso2_d)[!id_notmactch_d]
  
  id_notmactch_gdp2bitrade <- unique(df_cepii_gdp_pop$iso2) %in% unique(df_cepii_bitrade$iso2_d) 
  unique(df_cepii_gdp_pop$iso2) [!id_notmactch_gdp2bitrade]
  # STILL IN PROGRSS => quite a lot of countries are still missing
  
  ifrm(id_notmactch_o)
  ifrm(id_notmactch_d)
  ifrm(id_notmactch_gdp2bitrade)
  
  # match DIST data to bitrade
  df_cepii_dist$iso2_o_to_d <-  paste(df_cepii_dist$iso2_o, df_cepii_dist$iso2_d, sep="_to_")  # prep extra column in df_cepii_dist to later have unique identifiers
  df_cepii_bitrade <- add_column(df_cepii_bitrade, iso2_o_to_d = NA, .before = "year") # same for bitrae
  df_cepii_bitrade$iso2_o_to_d <- paste(df_cepii_bitrade$iso2_o, df_cepii_bitrade$iso2_d, sep = "_to_")
  df_cepii_bitrade <- add_column(df_cepii_bitrade, match_cepii_dist = NA, .before = "year") 
  df_cepii_bitrade$match_cepii_dist <- df_cepii_dist$dist[match(df_cepii_bitrade$iso2_o_to_d, df_cepii_dist$iso2_o_to_d)]
  
  # match GDP data to bitrade, generate gravity value
  exrate_USDtoGBR_2014 <- subset(df_cepii_exrates, year == "2014" & iso == "USA")[,4]  # extracting exchange rate for most recent observation because everything is denoted in GBR pounds
  df_cepii_bitrade$GDP_USD_o <- df_cepii_gdp_pop$GDP[match(df_cepii_bitrade$iso2_o, df_cepii_gdp_pop$iso2)] / exrate_USDtoGBR_2014
  df_cepii_bitrade$GDP_USD_d <- df_cepii_gdp_pop$GDP[match(df_cepii_bitrade$iso2_d, df_cepii_gdp_pop$iso2)] / exrate_USDtoGBR_2014
  ifrm(exrate_USDtoGBR_2014)
  
  # create trade gravity
  df_cepii_bitrade$gravity_USD <- (df_cepii_bitrade$GDP_USD_o * df_cepii_bitrade$GDP_USD_d) /df_cepii_bitrade$match_cepii_dist
  
  
  
  # # GRAV WEIGHTS > set weights for GRAV and add computed values into df_MAIN    ---------------------------
  get_DEGROOT_weights("ld_w_trade_grav", df_cepii_bitrade$iso2_o, df_cepii_bitrade$iso2_d, df_cepii_bitrade$gravity_USD)

  # save_data_R6_weights = T/F  ----------------------------------------
  if(save_data_R6_weights == T){
    save(list =  ls(.GlobalEnv),file = "data/0_R_data/R6_weights.Rdata")
  } # end save_data_R6_weights
  
}# end run_grav_weights_generation


# end  ----------------------------------------
t_end <-Sys.time()
t_R6<-t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R6_weights_DIST.R", ";  runtime ", round(t_R6[[1]],2)," ", units(t_R6), sep=""))

ifrm(run_grav_weights_generation) 
ifrm(save_data_R6_weights)

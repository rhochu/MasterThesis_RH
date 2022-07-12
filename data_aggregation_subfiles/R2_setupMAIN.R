######################
### R2_setupMAIN.R ###
######################

getwd()
t_start <-Sys.time()

# reduce_sample_for_notebook_test = T/F     ---------------------------------------
if(reduce_sample_for_test == T){
  if(dim(df_mob)[1] > 1845218){ 
    seq(1,length(unique(df_mob$country_region_code)),by =34)
    cntry_keep <- unique(df_mob$country_region_code)[c(1,2,3,4,5,6,7,8,9,10,35,69,103)]
    id_cntry_keep <- df_mob$country_region_code %in% cntry_keep
    df_mob <- df_mob[id_cntry_keep,]
    rm(cntry_keep, id_cntry_keep)
    
  }else if(dim(df_mob)[1] < 1845218){ 
    print("df_mob ALREADY REDUCED!")
    }
  print("reduce_sample_for_test_on_notebook > done")
}else if(reduce_sample_for_test == F){
  print("reduce_sample_for_test_on_notebook > NOT reduced, FULL SAMPLE")
}


# run_setupmain = T/F     ---------------------------------------
if(run_setupmain == F){
  load("data/0_R_data/R2_setupmain.Rdata")
}else if(run_setupmain == T){

  # create empty data set setupMAIN  -------------------------------------------------
  # # df_mob defines which countries appear in the main data set. no point in taking the full ISO-list => if mobility not included, no regression. 
  
  df_MAIN.colnames <- c("cntry", "iso2", "iso3", "date_day", 
                        "retail_and_recreation_percent_change_from_baseline",
                        "grocery_and_pharmacy_percent_change_from_baseline", 
                        "parks_percent_change_from_baseline",
                        "transit_stations_percent_change_from_baseline", 
                        "workplaces_percent_change_from_baseline", 
                        "residential_percent_change_from_baseline")
  df_MAIN <- data.frame(matrix(ncol = length(df_MAIN.colnames), nrow = 0))
  colnames(df_MAIN) <- df_MAIN.colnames
  ifrm(df_MAIN.colnames)


# > mob to MAIN; subset mob by taking only country observations, no subareas      -----------------------------------------------------------
  df_mob.cntry <-  subset(df_mob,
                              is.character(country_region_code) == T & 
                                nchar(country_region_code) == 2 &
                                sub_region_1 =="" & 
                                sub_region_2 =="" & 
                                metro_area=="" &
                                iso_3166_2_code=="")                      # create subset of countries only
  df_mob.cntry.iso2 <- unique(df_mob.cntry$country_region_code)
  
  df_MAIN[1:dim(df_mob.cntry)[1],] <- NA                                   # add enough rows so the data-frame from mob can be copied to MAIN
  df_MAIN$cntry <-    df_mob.cntry$country_region                          # add country name values from mob to MAIN
  df_MAIN$iso2 <-     df_mob.cntry$country_region_code                    # add country iso2 values from mob to MAIN
  for(i in 1:length(df_mob.cntry.iso2)){                                   # add country iso3 values from mob to MAIN
    id_iso2 <- df_mob.cntry$country_region_code %in% df_mob.cntry.iso2[i]
    df_MAIN$iso3[id_iso2] <- df_iso_names$iso3[df_iso_names$iso2 %in% df_mob.cntry.iso2[i] ]
  }
  ifrm(i)
  ifrm(id_iso2)
  ifrm(df_mob.cntry.iso2)
  
  class(df_mob.cntry$date)
  class(df_MAIN$date_day)
  df_MAIN$date_day <- as.Date(df_mob.cntry$date)                            # add date values to MAIN
  
  mob.value_names <- c("retail_and_recreation_percent_change_from_baseline",  # add mobility values
                  "grocery_and_pharmacy_percent_change_from_baseline", 
                  "parks_percent_change_from_baseline",
                  "transit_stations_percent_change_from_baseline", 
                  "workplaces_percent_change_from_baseline", 
                  "residential_percent_change_from_baseline")
  id_subdf_mob.values <- names(df_mob.cntry) %in% mob.value_names
  id_df_MAIN_mob.values <- names(df_MAIN) %in% mob.value_names    
  
  df_MAIN[,id_df_MAIN_mob.values] <- df_mob.cntry[,id_subdf_mob.values]
  ifrm(id_subdf_mob.values)
  ifrm(id_df_MAIN_mob.values)
  ifrm(mob.value_names)
  
  
  # > cornet, subset cornet by taking only full pairs observations, no subareas      -----------------------------------------------------------
  names(df_cornet)
  df_cornet$init_country_level
  
  df_cornet$date_end <- as.Date(df_cornet$date_end, format = "%d.%m.%y")            # transform date values to actual dates
  df_cornet$date_start <- as.Date(df_cornet$date_start, format = "%d.%m.%y")
  
  df_cornet.ld_national <- subset(df_cornet, 
                                        type == "Lockdown" & 
                                        init_country_level == "National" &
                                        complete.cases(c(df_cornet["date_start"],df_cornet["date_end"]))) 
  
  # > cornet to MAIN ------------------------------------------------------------------
  # use pairs of lockdown period in df_cornet to set a lockdown dummy 0/1 for each country in each observation in t
  df_MAIN <- add_column(df_MAIN, ld_cornet  = NA, .after = "date_day") # create column of NA after date_day for sci weights
  df_MAIN.cntry_names <- unique(df_MAIN$iso2)
  
  for(i in 1:length(df_MAIN.cntry_names)){
    id_df_MAIN.cntry.i <- df_MAIN$iso2 %in% df_MAIN.cntry_names[i]
    df_MAIN.cntry.i <-  df_MAIN[id_df_MAIN.cntry.i,]
    df_cornet.ld_national.cntry.i <- subset(df_cornet.ld_national, ISO_A2 == df_MAIN.cntry_names[i])
     
     if(df_MAIN.cntry_names[i] %in% df_cornet.ld_national$ISO_A2 ==T){
    
      for(ii in 1:dim(df_cornet.ld_national.cntry.i)[1]){
        
        if(complete.cases(df_cornet.ld_national.cntry.i[ii,9:10]) == T & 
           df_cornet.ld_national.cntry.i[ii,"date_start"] < df_cornet.ld_national.cntry.i[ii,"date_end"]){
          date_rng_ld.ii<-seq(from = df_cornet.ld_national.cntry.i[ii,"date_start"],
                              to =  df_cornet.ld_national.cntry.i[ii,"date_end"], 
                              by = "day") 
      
          if(ii == 1){  #!exists(date_rng_ld.all.i)){
            date_rng_ld.all.i <- date_rng_ld.ii
          }else{
            date_rng_ld.all.i <- append(date_rng_ld.all.i, date_rng_ld.ii)
            }
        }
        date_rng_ld.all.i
      }
      length(df_MAIN.cntry.i$date_day)
      id_cntry.i_ld <- df_MAIN.cntry.i$date_day %in% date_rng_ld.all.i
      df_MAIN.cntry.i$ld_cornet[id_cntry.i_ld] <- 1
      df_MAIN.cntry.i$ld_cornet[!id_cntry.i_ld] <- 0
      df_MAIN$ld_cornet[id_df_MAIN.cntry.i] <- df_MAIN.cntry.i$ld_cornet
      
    }else if(df_MAIN.cntry_names[i] %in% df_cornet.ld_national$ISO_A2 == F){
      if(!exists("count_obs_skipped_because_missing") ){
        count_obs_skipped_because_missing <- 1 
      }else if(exists("count_obs_skipped_because_missing") ){
        count_obs_skipped_because_missing <- count_obs_skipped_because_missing+1 }
      # print("do nothing for now")

      }
    
  }
ifrm(i)
ifrm(ii)
ifrm(df_MAIN.cntry_names)
ifrm(date_rng_ld.all.i)
ifrm(date_rng_ld.ii)
ifrm(id_df_MAIN.cntry.i) 
ifrm(id_cntry.i_ld)
  
# adding a column that specifies WEEKDAY, for dummy variable later
df_MAIN <- add_column(df_MAIN, weekday = NA, d_weekend = 0, .after = "date_day")
df_MAIN$weekday <- weekdays(df_MAIN$date_day)
df_MAIN$d_weekend[df_MAIN$weekday == "Saturday" | df_MAIN$weekday == "Sunday" ] <- 1


  # save_data_R2_setupmain = T/F  ----------------------------------------
  if(save_data_R2_setupmain == T){
    save(list =  ls(.GlobalEnv),file = "data/0_R_data/R2_setupmain.Rdata")
  }

}

# end  ----------------------------------------
t_end <-Sys.time()
t_R2<-t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R2_setupMAIN.R", ";  runtime ", round(t_R2[[1]],2)," ", units(t_R2), sep=""))
print(paste(count_obs_skipped_because_missing, " observations lost because no end date specified in df_cornet"))


ifrm(count_obs_skipped_because_missing)

ifrm(reduce_sample_for_test)
ifrm(run_setupmain)
ifrm(save_data_R2_setupmain)

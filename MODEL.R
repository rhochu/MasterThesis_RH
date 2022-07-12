#################################0
### Master Thesis -  READ_ME.R ###
#################################0
# author:     Raul Hochuli (raul.hochuli@uzh.ch)
# University of Zurich, Department of Economics
# supervisor: Prof. Dr. David Yanagizawa-Drott (david.yanagizawa-drott@econ.uzh.ch), 
#             Dr. Alexandra Schubert (lexi.schubert@econ.uzh.ch)


# Adjust directory HERE and set control if packages are reinstalled (see READ_ME-file)
MAIN_PATH <- "/Users/raulhochuli/Dropbox/0_raulhochuli/Dokumente_DB/2_Ausbildung/UZH/21_FS/MasterThesis_RaulHochuli_2013_713_110"  
install_packages_new <- F

# INTRO  =======================================================================================================================================
#   > clean environment, load packages & functions, load data --------------------------------------
rm(list=setdiff(ls(), c("MAIN_PATH", "install_packages_new")))
setwd(MAIN_PATH)
getwd()
list.files() # should show whole content of the master thesis folder
graphics.off()
cat("\014")

load("data/0_R_data/MASTER_Envir_final.Rdata")
# rm(export_fe_acf_plots, export_fe_qqplots, get_DEGROOT_weights, obs_fe_tbarplots, ifrm)
source("stat/packages_functions.R") # get all packages & functions



#   > Last data transformations  --------------------------------------
# shift all weight covriates by 1 day (t=1) to fit DeGroot model where action at t=0 is determined by t=-1
degroot_weights <- colnames(df_MAIN)[8:13]
degroot_weights
id_degroot_weights <-  colnames(df_MAIN)  %in%   degroot_weights 
id_degroot_weights
df_MAIN_iso_names <- unique(df_MAIN$iso2)
df_MAIN_iso_names

for(i in 1: length(df_MAIN_iso_names) ){ 
  id_iso_i <- df_MAIN$iso2 %in% df_MAIN_iso_names[i]
  df_MAIN[id_iso_i, id_degroot_weights] <- 
    rbind(df_MAIN[id_iso_i, id_degroot_weights][ 2: sum(id_iso_i, na.rm = T), ], 
          rep(NA, sum(id_degroot_weights, na.rm = T)) )
  
  ifrm(id_iso_i)
  }
length(unique(df_MAIN$iso2))
colSums(is.na(df_MAIN[,id_degroot_weights]))

df_MAIN[df_MAIN$date_day %in% as.Date("2020-12-29"),] %>% head
df_MAIN <- df_MAIN[ !( df_MAIN$date_day %in% as.Date("2020-12-29") ),] 
df_MAIN[df_MAIN$date_day %in% as.Date("2020-12-29"),] %>% head


ifrm(degroot_weights)
ifrm(id_degroot_weights)
ifrm(df_MAIN_iso_names)

# turn sign of ld_w_dist negative so it matches direction
df_MAIN$ld_w_dist <- -1*df_MAIN$ld_w_dist

# shorten Y-variable names to later better fit into the latex table
y_names_old <- df_MAIN[,(ncol(df_MAIN)-5):ncol(df_MAIN)] %>% names
y_names_new <- gsub("_percent_change_from_baseline", "", y_names_old)
colnames(df_MAIN)[(ncol(df_MAIN)-5):ncol(df_MAIN)] <- y_names_new
colnames(df_MAIN)
ifrm(y_names_old)
ifrm(y_names_new)

# create and add factor variables to later use as index in panel regression
names(df_MAIN)
df_MAIN$iso2 %>% class
df_MAIN <- add_column(df_MAIN, iso2_asfactor = NA, .after = "iso2")
df_MAIN$iso2_asfactor <- as.factor(df_MAIN$iso2)
df_MAIN$iso2_asfactor %>% class

df_MAIN$date_day %>% class
df_MAIN <- add_column(df_MAIN, date_day_asfactor = NA, .after = "date_day")
df_MAIN$date_day_asfactor <- as.factor(df_MAIN$date_day)
df_MAIN$date_day_asfactor %>% class

# add week numbers
df_MAIN$date_day %>% class
df_MAIN <- add_column(df_MAIN, week_number = NA, .after = "date_day_asfactor")
df_MAIN$week_number <- strftime( df_MAIN$date_day, format ="%V")
df_MAIN[c("date_day", "weekday", "week_number")]
df_MAIN$week_number <- as.factor(df_MAIN$week_number)



# REGRESSION SETUPS  =======================================================================================================================================
#   > reg I fe0-fe7 , increasing variables, FE iso2 & date_day --------------------------------------
{
  fe1a_standardSE <- feols(retail_and_recreation ~ ld_cornet ,
                             data = df_MAIN, se = "standard")
  fe2a_standardSE <- feols(retail_and_recreation ~ ld_cornet |
                  iso2_asfactor  , data = df_MAIN, se = "standard" )
  fe3a_standardSE <- feols(retail_and_recreation ~ ld_cornet |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "standard" )
  fe4a_standardSE <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "standard" )
  fe5a_standardSE <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "standard" )
  fe6a_standardSE <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "standard" )
  fe7a_standardSE <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "standard" )
  fe7a_standardSE_no_day_FE <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                            iso2_asfactor   , data = df_MAIN, se = "standard" )

  fe1a <- feols(retail_and_recreation ~ ld_cornet ,
                           data = df_MAIN, se = "hetero" )
  fe2a <- feols(retail_and_recreation ~ ld_cornet |
                             iso2_asfactor  , data = df_MAIN, se = "hetero" )
  fe3a <- feols(retail_and_recreation ~ ld_cornet |
                             iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe4a <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci |
                             iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe5a <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist |
                             iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe6a <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii |
                             iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7a <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                             iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7a_no_day_FE <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                             iso2_asfactor   , data = df_MAIN, se = "hetero" )
}

# fe3 
{
  # fe3a <- feols(retail_and_recreation ~ ld_cornet |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe3b <- feols(grocery_and_pharmacy ~ ld_cornet |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe3c <- feols(parks ~ ld_cornet |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe3d <- feols(transit_stations ~ ld_cornet |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe3e <- feols(workplaces ~ ld_cornet |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe3f <- feols(residential ~ ld_cornet |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
}

# fe5
{
  # fe5a <- feols(retail_and_recreation ~ ld_cornet  + ld_w_sci  + ld_w_dist |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe5b <- feols(grocery_and_pharmacy ~ ld_cornet + ld_w_sci  + ld_w_dist |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe5c <- feols(parks ~ ld_cornet  + ld_w_sci  + ld_w_dist |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe5d <- feols(transit_stations ~ ld_cornet  + ld_w_sci  + ld_w_dist |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe5e <- feols(workplaces ~ ld_cornet  + ld_w_sci  + ld_w_dist |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  # fe5f <- feols(residential ~ ld_cornet  + ld_w_sci  + ld_w_dist |
  #                 iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
}
etable( fe2a_standardSE, fe3a_standardSE, fe4a_standardSE, fe5a_standardSE, fe6a_standardSE, fe7a_standardSE, fe7a_standardSE_no_day_FE )
etable( fe2a, fe3a, fe4a, fe5a, fe6a, fe7a, fe7a_no_day_FE, tex = F)

etable( fe3a, fe3b, fe3c, fe3d, fe3e, fe3f, tex = F ) 
etable( fe5a, fe5b, fe5c, fe5d, fe5e, fe5f, tex = F ) 

#   > reg II  fe7,  all variables, FE iso2 & date_day, HCSE  --------------------------------------
{
  fe7a <- feols(retail_and_recreation ~ 
                  ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7b <- feols(grocery_and_pharmacy ~ 
                  ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7c <- feols(parks ~ 
                  ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7d <- feols(transit_stations ~ 
                  ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7e <- feols(workplaces ~ 
                  ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
  fe7f <- feols(residential ~ 
                  ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                  iso2_asfactor + date_day_asfactor  , data = df_MAIN, se = "hetero" )
}
# > Residual Extraction to df_res_fe7
{
  df_res_fe7 <- data.frame(  
    cntry_name = df_MAIN$cntry, 
    iso2 = df_MAIN$iso2,
    date_day = df_MAIN$date_day,
    res_fe7a = NA, 
    res_fe7b = NA, 
    res_fe7c = NA,
    res_fe7d = NA,
    res_fe7e = NA,
    res_fe7f = NA
  )
  df_res_fe7$res_fe7a[fe7a$obs_selection$obsRemoved] <- fe7a$residuals 
  df_res_fe7$res_fe7b[fe7b$obs_selection$obsRemoved] <- fe7b$residuals
  df_res_fe7$res_fe7c[fe7c$obs_selection$obsRemoved] <- fe7c$residuals 
  df_res_fe7$res_fe7d[fe7d$obs_selection$obsRemoved] <- fe7d$residuals 
  df_res_fe7$res_fe7e[fe7e$obs_selection$obsRemoved] <- fe7e$residuals 
  df_res_fe7$res_fe7f[fe7f$obs_selection$obsRemoved] <- fe7f$residuals
}
etable(fe7a, fe7b, fe7c, fe7d, fe7e, fe7f, tex = F)



#   > reg III fe8,  all variables, FE iso2 & date_day, HCSE clustered by iso  --------------------------------------
{
fe8a <- feols(retail_and_recreation ~ 
                ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor  , data = df_MAIN, cluster = c("iso2") )
fe8b <- feols(grocery_and_pharmacy ~ 
                ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor  , data = df_MAIN, cluster = c("iso2") )
fe8c <- feols(parks ~ 
                ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor  , data = df_MAIN, cluster = c("iso2") )
fe8d <- feols(transit_stations ~ 
                ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor  , data = df_MAIN, cluster = c("iso2") )
fe8e <- feols(workplaces ~ 
                ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor  , data = df_MAIN, cluster = c("iso2") )
fe8f <- feols(residential ~ 
                ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor  , data = df_MAIN, cluster = c("iso2") )
}
# > Residual Extraction to df_res_fe8
{
  df_res_fe8 <- data.frame(  
    cntry_name = df_MAIN$cntry,
    iso2 = df_MAIN$iso2,
    date_day = df_MAIN$date_day,
    res_fe8a = NA, 
    res_fe8b = NA, 
    res_fe8c = NA,
    res_fe8d = NA,
    res_fe8e = NA,
    res_fe8f = NA
  )
  df_res_fe8$res_fe8a[fe8a$obs_selection$obsRemoved] <- fe8a$residuals 
  df_res_fe8$res_fe8b[fe8b$obs_selection$obsRemoved] <- fe8b$residuals
  df_res_fe8$res_fe8c[fe8c$obs_selection$obsRemoved] <- fe8c$residuals 
  df_res_fe8$res_fe8d[fe8d$obs_selection$obsRemoved] <- fe8d$residuals 
  df_res_fe8$res_fe8e[fe8e$obs_selection$obsRemoved] <- fe8e$residuals 
  df_res_fe8$res_fe8f[fe8f$obs_selection$obsRemoved] <- fe8f$residuals
}
etable(fe8a, fe8b, fe8c, fe8d, fe8e, fe8f, tex = F)



#   > reg IV  fe9,  all variables, FE iso2 & date_day, HCSE clustered by iso + day --------------------------------------
{
fe9a <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day") )
fe9b <- feols(grocery_and_pharmacy ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day" ))
fe9c <- feols(parks ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day" ))
fe9d <- feols(transit_stations ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day" ))
fe9e <- feols(workplaces ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day" ))
fe9f <- feols(residential ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day" ))

}
# > Residual Extraction to df_res_fe9
{
  df_res_fe9 <- data.frame(
    cntry_name = df_MAIN$cntry,
    iso2 = df_MAIN$iso2,
    date_day = df_MAIN$date_day,
    res_fe9a = NA, 
    res_fe9b = NA, 
    res_fe9c = NA,
    res_fe9d = NA,
    res_fe9e = NA,
    res_fe9f = NA
  )
  df_res_fe9$res_fe9a[fe9a$obs_selection$obsRemoved] <- fe9a$residuals 
  df_res_fe9$res_fe9b[fe9b$obs_selection$obsRemoved] <- fe9b$residuals
  df_res_fe9$res_fe9c[fe9c$obs_selection$obsRemoved] <- fe9c$residuals 
  df_res_fe9$res_fe9d[fe9d$obs_selection$obsRemoved] <- fe9d$residuals 
  df_res_fe9$res_fe9e[fe9e$obs_selection$obsRemoved] <- fe9e$residuals 
  df_res_fe9$res_fe9f[fe9f$obs_selection$obsRemoved] <- fe9f$residuals
  
}
etable(fe9a, fe9b, fe9c, fe9d, fe9e, fe9f, tex = F)



#   > time effects: which time FE is best suited, resp cancels the others out.  --------------------------------------
{
# create weekday dummies
df_MAIN_week_specific_dummies <- dummy_cols(df_MAIN, select_columns =c("weekday"), remove_first_dummy = F)
          # weekday_Tuesday + weekday_Wednesday + weekday_Thursday + weekday_Friday + weekday_Saturday + weekday_Sunday 

# FE iso2+day + weekday dummies
fe9a2 <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both +
                 weekday_Tuesday + weekday_Wednesday + weekday_Thursday + weekday_Friday + weekday_Saturday + weekday_Sunday |
                iso2_asfactor + date_day_asfactor , data = df_MAIN_week_specific_dummies, cluster = c("iso2", "date_day") )
fe9b2 <- feols(grocery_and_pharmacy ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both +
                 weekday_Tuesday + weekday_Wednesday + weekday_Thursday + weekday_Friday + weekday_Saturday + weekday_Sunday |
                 iso2_asfactor + date_day_asfactor , data = df_MAIN_week_specific_dummies, cluster = c("iso2", "date_day" ))
          # dummies drop out in multicollinearity


# create week number as FE
df_MAIN_week_specific_dummies <- dummy_cols(df_MAIN, select_columns =c("week_number"), remove_first_dummy = F)
df_MAIN_week_specific_dummies$week_number <- as.factor(df_MAIN_week_specific_dummies$week_number)

# FE iso2+ weeknumber
fe9a3 <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                 iso2_asfactor + week_number , data = df_MAIN_week_specific_dummies, cluster = c("iso2", "date_day") )
fe9b3 <- feols(grocery_and_pharmacy ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                 iso2_asfactor + week_number , data = df_MAIN_week_specific_dummies, cluster = c("iso2", "date_day" ))

# FE iso2  + weekday + day
fe9a4 <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                 iso2_asfactor + as.factor(weekday) + date_day_asfactor, data = df_MAIN_week_specific_dummies, cluster = c("iso2", "date_day") )

# FE iso2 + weeknumber + weekday + day
fe9a5 <- feols(retail_and_recreation ~ ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                 iso2_asfactor + week_number + as.factor(weekday) +date_day_asfactor, data = df_MAIN_week_specific_dummies, cluster = c("iso2", "date_day") )
}
etable(fe9a2, fe9b2, fe9a3, fe9b3, fe9a4, tex = F)
etable(fe9a, fe9a2, fe9a3, fe9a4, fe9a5)
# facit: Day FE render all other additional FE unnecessary as they already account for this variation. Given the high variation 
# at presumably a weekday seasonality, it is not sensible to take week number FE. And weekday specific variation already includced, see fe9a2
rm(fe9a2, fe9b2, fe9a3, fe9b3, fe9a4, fe9a5)



# ROBUSTNESS SETUPS  =======================================================================================================================================
#   > robustness I   fe21, SPLIT at gap-period, all variables, HCSE clustered by iso + week   --------------------------------------
# generate split dummy for splitting date into two parts; finding the time window with vast missing values
{
  # find gap in observations over time
  df_res_fe9$res_fe9a
  table(df_res_fe9$date_day)
  
  with(df_res_fe9, 
       date_res_fe9all <<- c( date_day[ !is.na(res_fe9a) ], 
                               date_day[ !is.na(res_fe9b) ], 
                               date_day[ !is.na(res_fe9c) ], 
                               date_day[ !is.na(res_fe9d) ], 
                               date_day[ !is.na(res_fe9e) ], 
                               date_day[ !is.na(res_fe9f) ] )
  )
  date_res_fe9all %>% class
  table(date_res_fe9all )
  graphics.off()
  
  # hist_nobs_over_time_fe9 <- ggplot(
  #   mapping = aes(date_res_fe9all)) + 
  #   geom_histogram( binwidth = 1, color = "grey80" ) +

  df_date_res_fe9all <- data.frame(
    date_day=as.Date(names(table(date_res_fe9all))) ,
    Frequency=as.vector(table(date_res_fe9all)))
  df_date_res_fe9all$date_day %>% class

  
  hist_nobs_over_time_fe9 <- ggplot(
    data = df_date_res_fe9all, aes(x = date_day, y = Frequency) ) + 
    geom_line(color = "red", alpha = 0.75) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    coord_cartesian(
      ylim = c(350, 530)) +
    theme_minimal() +
    labs( # title = paste("Number of observations over all mobility measurements",  sep="") ,
         x ="Daily observations",
         y ="N. of observations")
  
  hist_nobs_over_time_fe9
  
  ggsave("plots/nobs_over_time_fe9.png",
         plot = hist_nobs_over_time_fe9,
         device = NULL,
         scale = 1,
         width = 21,
         height = 8,
         units = "cm",
         dpi = 300,
         limitsize = TRUE)
  
  sub_date_res_fe9all<- date_res_fe9all[date_res_fe9all %in% c(as.Date(as.Date("2020-08-16"):as.Date("2020-09-11")))]
  
  ggplot(mapping = aes(sub_date_res_fe9all)) +   
    geom_histogram(binwidth = 1)
  
  sub_date_res_fe9all<- date_res_fe9all[date_res_fe9all %in% c(as.Date(as.Date("2020-08-17"):as.Date("2020-09-10")))]
  
  ggplot(mapping = aes(sub_date_res_fe9all)) + 
    geom_histogram(binwidth = 1)
  
  ifrm(date_res_fe9all)
  ifrm(sub_date_res_fe9all)
  ifrm(hist_nobs_over_time_fe9)
  
  # add split indicator
  df_MAIN <- add_column(df_MAIN, split_period = NA, .after = "date_day_asfactor")
  df_MAIN$split_period[df_MAIN$date_day < as.Date("2020-08-17")]   <- "before_split"
  # df_MAIN$split_period[df_MAIN$date_day %in% c(as.Date(as.Date("2020-08-17"):as.Date("2020-09-10")))] <- "split_period"
  df_MAIN$split_period[df_MAIN$date_day > as.Date("2020-09-10") ]  <- "after_split"
  df_MAIN$split_period <- as.factor(df_MAIN$split_period)
  df_MAIN$split_period %>% table
  14606+24521 + sum(is.na(df_MAIN$split_period))
  dim(df_MAIN)
  df_MAIN[df_MAIN$split_period %in% "after_split","date_day"]
  
  }
# regression formulation
{
  fe21a <- feols(retail_and_recreation ~ 
                   ld_cornet +
                   ld_w_sci + 
                   ld_w_dist + 
                   ld_w_bitrade_cepii +
                   ld_w_lang_both |
                   iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), fsplit = ~split_period)
  fe21b <- feols(grocery_and_pharmacy ~ 
                   ld_cornet +
                   ld_w_sci + 
                   ld_w_dist + 
                   ld_w_bitrade_cepii +
                   ld_w_lang_both |
                   iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), fsplit = ~split_period)
  fe21c <- feols(parks ~ 
                   ld_cornet +
                   ld_w_sci + 
                   ld_w_dist + 
                   ld_w_bitrade_cepii +
                   ld_w_lang_both |
                   iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), fsplit = ~split_period)
  fe21d <- feols(transit_stations ~ 
                   ld_cornet +
                   ld_w_sci + 
                   ld_w_dist + 
                   ld_w_bitrade_cepii +
                   ld_w_lang_both |
                   iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), fsplit = ~split_period)
  fe21e <- feols(workplaces ~ 
                   ld_cornet +
                   ld_w_sci + 
                   ld_w_dist + 
                   ld_w_bitrade_cepii +
                   ld_w_lang_both |
                   iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), fsplit = ~split_period)
  fe21f <- feols(residential ~ 
                   ld_cornet +
                   ld_w_sci + 
                   ld_w_dist + 
                   ld_w_bitrade_cepii +
                   ld_w_lang_both |
                   iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), fsplit = ~split_period)
  
                   # old bottom line of specifics. 
                   # iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"), fsplit = ~split_period)
}
etable(fe21a, fe21b, fe21c, tex = F) 
etable(fe21d, fe21e, fe21f, tex = F)
# export only the subset regressions
etable(fe21a[[3]], fe21a[[2]], 
       fe21b[[3]], fe21b[[2]], 
       fe21c[[3]], fe21c[[2]], tex = F) 
etable(fe21d[[3]], fe21d[[2]], 
       fe21e[[3]], fe21e[[2]], 
       fe21f[[3]], fe21f[[2]], tex = F)

  
#   > robustness II  fe22, DROP country below 95%, all variables, HCSE clustered by iso + week, removed inconsistent countries --------------------------------------
# finding cntry that are have large gaps
{
  df_res_fe9$res_fe9a
  table(df_res_fe9$iso2)
  
  with(df_res_fe9,
       cntry_res_fe9all <<- c( iso2[ !is.na(res_fe9a) ], 
                                 iso2[ !is.na(res_fe9b) ], 
                                 iso2[ !is.na(res_fe9c) ], 
                                 iso2[ !is.na(res_fe9d) ], 
                                 iso2[ !is.na(res_fe9e) ], 
                                 iso2[ !is.na(res_fe9f) ] )
       )
  
  table(cntry_res_fe9all) %>% sort
  sort(table(cntry_res_fe9all) /   table(cntry_res_fe9all) %>% max )
  round(sort(table(cntry_res_fe9all) /  table(cntry_res_fe9all) %>% max )*100, digits = 1 )
    
  cntry_in_fe9 <- unique(cntry_res_fe9all)
  cntry_in_fe9_below95 <- df_iso_names$iso2[
    match(c( "LI", "AF", "GE", "RS", "AG", "PG", "BZ", "BW"), df_iso_names$iso2)]
  cntry_in_fe9_above95 <- cntry_in_fe9[!(cntry_in_fe9 %in% cntry_in_fe9_below95)]
  df_iso_names$county_name[match(cntry_in_fe9_below95, df_iso_names$iso2)]
  
  
  # vector with countries below 95% coverage
  df_MAIN$cntry_coverage[ df_MAIN$iso2 %in% cntry_in_fe9_below95] <- "below95"
  df_MAIN$cntry_coverage[ df_MAIN$iso2 %in% cntry_in_fe9_above95] <- "above95"
  df_MAIN$cntry_coverage <- as.factor(df_MAIN$cntry_coverage)
  
  ifrm(cntry_res_fe9all)  
  ifrm(cntry_in_fe9)
  ifrm(cntry_in_fe9_below95)
  ifrm(cntry_in_fe9_above95)
  }
# regression formulation
{
  fe22a <- feols( retail_and_recreation ~ 
                    ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"),
                    fsplit = ~cntry_coverage )
  fe22b <- feols( grocery_and_pharmacy ~ 
                    ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"),
                    fsplit = ~cntry_coverage )
  fe22c <- feols( parks ~ 
                    ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"),
                    fsplit = ~cntry_coverage )
  fe22d <- feols( transit_stations ~ 
                    ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"),
                    fsplit = ~cntry_coverage )
  fe22e <- feols( workplaces ~ 
                    ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"),
                    fsplit = ~cntry_coverage )
  fe22f <- feols( residential ~ 
                    ld_cornet + ld_w_sci + ld_w_dist + ld_w_bitrade_cepii  + ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor , data = df_MAIN, cluster = c("iso2", "date_day"),
                    fsplit = ~cntry_coverage )
}
etable(fe9a, fe22a)
etable(fe22a, fe22b, fe22c, fe22d, fe22e, fe22f, tex = F)
etable(fe22a[[2]], fe22b[[2]], fe22c[[2]], fe22d[[2]], fe22e[[2]], fe22f[[2]], tex = F)

etable(fe21a[[2]], fe21a[[3]], 
       fe21b[[2]], fe21b[[3]], 
       fe21c[[2]], fe21c[[3]], 
       fe21d[[2]], fe21d[[3]], 
       fe21e[[2]], fe21e[[3]], 
       fe21f[[2]], fe21f[[3]], tex = F)

#   > robustness III fe23, LAGS 0-3, all variables, HCSE clustered by iso + week   --------------------------------------
# regression formulation
{
  # fe23a_lag
  {
    fe23a_d0 <- feols(retail_and_recreation ~ 
                    ld_cornet +
                    ld_w_sci + 
                    ld_w_dist + 
                    ld_w_bitrade_cepii +
                    ld_w_lang_both |
                    iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23a_d1 <- feols(retail_and_recreation ~ 
                    ld_cornet + d(ld_cornet,1) +
                    ld_w_sci + d(ld_w_sci,1) + 
                    ld_w_dist + d(ld_w_dist,1) + 
                    ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + 
                    ld_w_lang_both + d(ld_w_lang_both,1) |
                    iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23a_d2 <- feols(retail_and_recreation ~ 
                    ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) +
                    ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + 
                    ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +
                    ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) +  d(ld_w_bitrade_cepii,2) +
                    ld_w_lang_both + d(ld_w_lang_both,1) + d(ld_w_lang_both,2) |
                    iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23a_d3 <- feols(retail_and_recreation ~ 
                     ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) + d(ld_cornet,3) +
                     ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + d(ld_w_sci,3) +
                     ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +  d(ld_w_dist,3) +
                     ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + d(ld_w_bitrade_cepii,2) + d(ld_w_bitrade_cepii,3) + 
                     ld_w_lang_both + d(ld_w_lang_both,1)  + d(ld_w_lang_both,2) + d(ld_w_lang_both,3)  |
                     iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
  }
  # fe23b_lag
  {
    fe23b_d0 <- feols(grocery_and_pharmacy ~ 
                        ld_cornet +
                        ld_w_sci + 
                        ld_w_dist + 
                        ld_w_bitrade_cepii +
                        ld_w_lang_both |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23b_d1 <- feols(grocery_and_pharmacy ~ 
                        ld_cornet + d(ld_cornet,1) +
                        ld_w_sci + d(ld_w_sci,1) + 
                        ld_w_dist + d(ld_w_dist,1) + 
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + 
                        ld_w_lang_both + d(ld_w_lang_both,1) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23b_d2 <- feols(grocery_and_pharmacy ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + 
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) +  d(ld_w_bitrade_cepii,2) +
                        ld_w_lang_both + d(ld_w_lang_both,1) + d(ld_w_lang_both,2) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23b_d3 <- feols(grocery_and_pharmacy ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) + d(ld_cornet,3) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + d(ld_w_sci,3) +
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +  d(ld_w_dist,3) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + d(ld_w_bitrade_cepii,2) + d(ld_w_bitrade_cepii,3) + 
                        ld_w_lang_both + d(ld_w_lang_both,1)  + d(ld_w_lang_both,2) + d(ld_w_lang_both,3)  |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
  }
  # fe23c_lag
  {
    fe23c_d0 <- feols(parks ~ 
                        ld_cornet +
                        ld_w_sci + 
                        ld_w_dist + 
                        ld_w_bitrade_cepii +
                        ld_w_lang_both |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23c_d1 <- feols(parks ~ 
                        ld_cornet + d(ld_cornet,1) +
                        ld_w_sci + d(ld_w_sci,1) + 
                        ld_w_dist + d(ld_w_dist,1) + 
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + 
                        ld_w_lang_both + d(ld_w_lang_both,1) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23c_d2 <- feols(parks ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + 
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) +  d(ld_w_bitrade_cepii,2) +
                        ld_w_lang_both + d(ld_w_lang_both,1) + d(ld_w_lang_both,2) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23c_d3 <- feols(parks ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) + d(ld_cornet,3) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + d(ld_w_sci,3) +
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +  d(ld_w_dist,3) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + d(ld_w_bitrade_cepii,2) + d(ld_w_bitrade_cepii,3) + 
                        ld_w_lang_both + d(ld_w_lang_both,1)  + d(ld_w_lang_both,2) + d(ld_w_lang_both,3)  |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
  }
  # fe23d_lag
  {
    fe23d_d0 <- feols(transit_stations ~ 
                        ld_cornet +
                        ld_w_sci + 
                        ld_w_dist + 
                        ld_w_bitrade_cepii +
                        ld_w_lang_both |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23d_d1 <- feols(transit_stations ~ 
                        ld_cornet + d(ld_cornet,1) +
                        ld_w_sci + d(ld_w_sci,1) + 
                        ld_w_dist + d(ld_w_dist,1) + 
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + 
                        ld_w_lang_both + d(ld_w_lang_both,1) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23d_d2 <- feols(transit_stations ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + 
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) +  d(ld_w_bitrade_cepii,2) +
                        ld_w_lang_both + d(ld_w_lang_both,1) + d(ld_w_lang_both,2) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23d_d3 <- feols(transit_stations ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) + d(ld_cornet,3) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + d(ld_w_sci,3) +
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +  d(ld_w_dist,3) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + d(ld_w_bitrade_cepii,2) + d(ld_w_bitrade_cepii,3) + 
                        ld_w_lang_both + d(ld_w_lang_both,1)  + d(ld_w_lang_both,2) + d(ld_w_lang_both,3)  |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
  }
  # fe23e_lag
  {
    fe23e_d0 <- feols(workplaces ~ 
                        ld_cornet +
                        ld_w_sci + 
                        ld_w_dist + 
                        ld_w_bitrade_cepii +
                        ld_w_lang_both |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23e_d1 <- feols(workplaces ~ 
                        ld_cornet + d(ld_cornet,1) +
                        ld_w_sci + d(ld_w_sci,1) + 
                        ld_w_dist + d(ld_w_dist,1) + 
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + 
                        ld_w_lang_both + d(ld_w_lang_both,1) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23e_d2 <- feols(workplaces ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + 
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) +  d(ld_w_bitrade_cepii,2) +
                        ld_w_lang_both + d(ld_w_lang_both,1) + d(ld_w_lang_both,2) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23e_d3 <- feols(workplaces ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) + d(ld_cornet,3) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + d(ld_w_sci,3) +
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +  d(ld_w_dist,3) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + d(ld_w_bitrade_cepii,2) + d(ld_w_bitrade_cepii,3) + 
                        ld_w_lang_both + d(ld_w_lang_both,1)  + d(ld_w_lang_both,2) + d(ld_w_lang_both,3)  |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
  }
  # fe23f_lag
  {
    fe23f_d0 <- feols(residential ~ 
                        ld_cornet +
                        ld_w_sci + 
                        ld_w_dist + 
                        ld_w_bitrade_cepii +
                        ld_w_lang_both |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23f_d1 <- feols(residential ~ 
                        ld_cornet + d(ld_cornet,1) +
                        ld_w_sci + d(ld_w_sci,1) + 
                        ld_w_dist + d(ld_w_dist,1) + 
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + 
                        ld_w_lang_both + d(ld_w_lang_both,1) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23f_d2 <- feols(residential ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + 
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) +  d(ld_w_bitrade_cepii,2) +
                        ld_w_lang_both + d(ld_w_lang_both,1) + d(ld_w_lang_both,2) |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
    fe23f_d3 <- feols(residential ~ 
                        ld_cornet + d(ld_cornet,1) + d(ld_cornet,2) + d(ld_cornet,3) +
                        ld_w_sci + d(ld_w_sci,1) + d(ld_w_sci,2) + d(ld_w_sci,3) +
                        ld_w_dist + d(ld_w_dist,1) + d(ld_w_dist,2) +  d(ld_w_dist,3) +
                        ld_w_bitrade_cepii + d(ld_w_bitrade_cepii,1) + d(ld_w_bitrade_cepii,2) + d(ld_w_bitrade_cepii,3) + 
                        ld_w_lang_both + d(ld_w_lang_both,1)  + d(ld_w_lang_both,2) + d(ld_w_lang_both,3)  |
                        iso2_asfactor + date_day_asfactor, data = df_MAIN, cluster = c("iso2", "date_day"), panel.id = c("iso2", "date_day"))
  }
}

order_etable_output_fe23 <- c("ld_cornet", "d(ld_cornet, 1)", "d(ld_cornet, 2)", "d(ld_cornet, 3)",
                              "ld_w_sci", "d(ld_w_sci, 1)", "d(ld_w_sci, 2)", "d(ld_w_sci, 3)", 
                              "ld_w_dist", "d(ld_w_dist, 1)", "(ld_w_dist, 2)", "d(ld_w_dist, 3)", 
                              "ld_w_bitrade_cepii", "d(ld_w_bitrade_cepii, 1)", "d(ld_w_bitrade_cepii, 2)", "d(ld_w_bitrade_cepii, 3)", 
                              "ld_w_lang_both", "d(ld_w_lang_both, 1)", "d(ld_w_lang_both, 2)", "d(ld_w_lang_both, 3)") 

etable(fe23a_d0, fe23a_d1, fe23a_d2, fe23a_d3, order = order_etable_output_fe23, tex = F)
etable(fe23b_d0, fe23b_d1, fe23b_d2, fe23b_d3, order = order_etable_output_fe23, tex = F)
etable(fe23c_d0, fe23c_d1, fe23c_d2, fe23c_d3, order = order_etable_output_fe23, tex = F)
etable(fe23d_d0, fe23d_d1, fe23d_d2, fe23d_d3, order = order_etable_output_fe23, tex = F)
etable(fe23e_d0, fe23e_d1, fe23e_d2, fe23e_d3, order = order_etable_output_fe23, tex = F)
etable(fe23f_d0, fe23f_d1, fe23f_d2, fe23f_d3, order = order_etable_output_fe23, tex = F)

etable(fe23a_d0, fe23a_d1, fe23a_d2, fe23a_d3, order = order_etable_output_fe23, drop.section = c("fixef", "stats"), tex = T)
etable(fe23b_d0, fe23b_d1, fe23b_d2, fe23b_d3, order = order_etable_output_fe23, drop.section = c("fixef", "stats"), tex = T)
etable(fe23c_d0, fe23c_d1, fe23c_d2, fe23c_d3, order = order_etable_output_fe23, drop.section = c("fixef", "stats"), tex = T)
etable(fe23d_d0, fe23d_d1, fe23d_d2, fe23d_d3, order = order_etable_output_fe23, drop.section = c("fixef", "stats"), tex = T)
etable(fe23e_d0, fe23e_d1, fe23e_d2, fe23e_d3, order = order_etable_output_fe23, drop.section = c("fixef", "stats"), tex = T)
etable(fe23f_d0, fe23f_d1, fe23f_d2, fe23f_d3, order = order_etable_output_fe23, drop.section = c("fixef", "stats"), tex = T)

etable( fe23a_d3, fe23b_d3, fe23c_d3, fe23d_d3, fe23e_d3, fe23f_d3, drop.section = c("fixef", "stats"), tex = F)


# COMPARISON  =======================================================================================================================================
#   > reg results comparison    --------------------------------------

# check if etable uses HC1 SE
fe7a
modelsummary(fe7a, vcov = "HC1")

# HCSE vs standard SE
etable( fe2a_standardSE, fe3a_standardSE, fe4a_standardSE, fe5a_standardSE, fe6a_standardSE, fe7a_standardSE, fe7a_standardSE_no_day_FE )
etable( fe2a, fe3a, fe4a, fe5a, fe6a, fe7a, fe7a_no_day_FE, tex = F)
(11.47/9.260  -1)*100
( (11.47-9.260)/9.260)*100
(33.65/16.15  -1)*100



# effects in fe9
etable( fe9a, fe9b, fe9c, fe9d, fe9e, fe9f, tex = F , drop.section = c("stats", "fixef"))
(-34.67+ -676.7) / 2 
(5.933+83.41) /2 
25.89/2
(-34.67+ -676.7+ -25.89)  

# % change ld sci to ld cornet
c(-9.030/ -16.34,
-8.982 / -34.67,
-6.587 / -11.99,
3.208 / 5.933) ^-1


# rest

df_res_fe9$date_day %>% max-
df_res_fe9$date_day %>% min
df_res_fe9$iso2 %>% unique %>% length

df_res_fe9$iso2[!is.na(df_res_fe9$res_fe9a)] %>% unique %>% length
df_res_fe9$iso2[!is.na(df_res_fe9$res_fe9b)] %>% unique %>% length
df_res_fe9$iso2[!is.na(df_res_fe9$res_fe9c)] %>% unique %>% length
df_res_fe9$iso2[!is.na(df_res_fe9$res_fe9d)] %>% unique %>% length
df_res_fe9$iso2[!is.na(df_res_fe9$res_fe9e)] %>% unique %>% length
df_res_fe9$iso2[!is.na(df_res_fe9$res_fe9f)] %>% unique %>% length



#   > robust result comparison    --------------------------------------

# SPLIT 
etable(fe21a, fe21b) 
etable(fe21c, fe21d )
etable(fe21e, fe21f)

etable(fe21a[[3]], fe21a[[2]], 
       fe21b[[3]], fe21b[[2]], 
       fe21c[[3]], fe21c[[2]], tex = F)[3,] 
etable(fe21d[[3]], fe21d[[2]], 
       fe21e[[3]], fe21e[[2]], 
       fe21f[[3]], fe21f[[2]], tex = F)[3,]
# ld_cornet
# c(-17.28 - -9.125, -12.43 -  -7.112, -18.46 - -7.350, -14.33 - -8.334,  -10.59 - -7.559, 5.388 - 3.236)
# sort(c(-17.28 - -9.125, -12.43 -  -7.112, -18.46 - -7.350, -14.33 - -8.334,  -10.59 - -7.559, 5.388 - 3.236))
c( -16.53 + -8.934, -12.43 +  -7.112 , -18.46 +  -7.350, 
   -14.33 +  -8.334,  -10.59 +  -7.559,  5.388 + 3.236) 
c( -16.53 + -8.934, -12.43 +  -7.112 , -18.46 +  -7.350, 
   -14.33 +  -8.334,  -10.59 +  -7.559,  5.388 + 3.236) %>% sort

c(-8.934 -  -16.53, -7.112  - -12.43,  -7.350 - -18.46, 
  -8.334 -  -14.33, -7.559 - -10.59,  3.236 - 5.388 )  %>% sort

# ld_sci
c(-63.92 - -12.91, -31.52 - -8.292, -127.6 - -13.13, 
  -67.25-   -8.760, -38.86 - -12.10,  25.07 - 5.066)
c(-63.92 - -12.91, -31.52 - -8.292, -127.6 - -13.13, 
  -67.25 - -8.760, -38.86 - -12.10,  25.07 - 5.066) %>% sort
# c(-64.29 - -13.06, -31.52 - -8.292, -127.6 - -13.13, -67.25 - -8.760,  -38.86 - -12.10,  25.07 - 5.066)
# sort(c(-64.29 - -13.06, -31.52 - -8.292, -127.6 - -13.13, -67.25 - -8.760,  -38.86 - -12.10,  25.07 - 5.066))


# DROP
etable(fe22a[[1]], fe22a[[2]], 
       fe22b[[1]], fe22b[[2]], 
       fe22c[[1]], fe22c[[2]], tex = F) 
etable(fe22d[[1]], fe22d[[2]], 
       fe22e[[1]], fe22e[[2]], 
       fe22f[[1]], fe22f[[2]], tex = F)

etable(fe22a[[2]], fe22b[[2]], fe22c[[2]], fe22d[[2]], fe22e[[2]], fe22f[[2]], tex = F)

# % of obs dropped
c(
  fe22a$below95$nobs / fe22a$`Full sample`$nobs,
  fe22b$below95$nobs / fe22b$`Full sample`$nobs,
  fe22c$below95$nobs / fe22c$`Full sample`$nobs,
  fe22d$below95$nobs / fe22d$`Full sample`$nobs,
  fe22e$below95$nobs / fe22e$`Full sample`$nobs,
  fe22f$below95$nobs / fe22f$`Full sample`$nobs 
  ) %>% mean


# LAGS
etable( fe23a_d3, fe23b_d3, fe23c_d3, fe23d_d3, fe23e_d3, fe23f_d3, drop.section = c("fixef", "stats"), tex = F)



# EXPORT TABLES  =======================================================================================================================================
#   > export reg tables  --------------------------------------
path_tables <- "plots/reg_tables_latex/" 
if(!file.exists( path_tables ) ){
  dir.create( path_tables )
}


# export reg table fe1 to 7
txt_export <- file(paste(path_tables,"etable_fe1to7.txt", sep=""))
options(width=10000)
sink(txt_export, append = TRUE, type = "output") # Writing console output to log file
sink(txt_export, append = TRUE, type = "message")
etable( fe2a, fe3a, fe4a, fe5a, fe6a, fe7a, fe7a_no_day_FE, tex = T)
closeAllConnections() # Close connection to log file

# export reg table fe9
txt_export <- txt_export <- file(paste(path_tables,"etable_fe9.txt", sep=""))
options(width=10000)
sink(txt_export, append = TRUE, type = "output") # Writing console output to log file
sink(txt_export, append = TRUE, type = "message")
etable(fe9a, fe9b, fe9c, fe9d, fe9e, fe9f, drop.section = c("fixef"), tex = T)
closeAllConnections() # Close connection to log file
ifrm(txt_export)

# export reg table fe21
txt_export <- txt_export <- file(paste(path_tables,"etable_fe21.txt", sep=""))
options(width=10000)
sink(txt_export, append = TRUE, type = "output") # Writing console output to log file
sink(txt_export, append = TRUE, type = "message")

etable(fe21a[[3]], fe21a[[2]], 
       fe21b[[3]], fe21b[[2]], 
       fe21c[[3]], fe21c[[2]], drop.section = c("fixef"), tex = T)
cat("\n")
cat("\n")
etable(fe21d[[3]], fe21d[[2]], 
       fe21e[[3]], fe21e[[2]], 
       fe21f[[3]], fe21f[[2]], drop.section = c("fixef"), tex = T) 

closeAllConnections() # Close connection to log file
ifrm(txt_export)

# export reg table fe22
txt_export <- txt_export <- file(paste(path_tables,"etable_fe22.txt", sep=""))
options(width=10000)
sink(txt_export, append = TRUE, type = "output") # Writing console output to log file
sink(txt_export, append = TRUE, type = "message")
etable(fe22a[[2]], fe22b[[2]], fe22c[[2]], fe22d[[2]], fe22e[[2]], fe22f[[2]], drop.section = c("fixef"), tex = T)
closeAllConnections() # Close connection to log file
ifrm(txt_export)

# export reg table fe23
txt_export <- txt_export <- file(paste(path_tables,"etable_fe23.txt", sep=""))
options(width=10000)
sink(txt_export, append = TRUE, type = "output") # Writing console output to log file
sink(txt_export, append = TRUE, type = "message")
etable( fe23a_d3, fe23b_d3, fe23c_d3, fe23d_d3, fe23e_d3, fe23f_d3, drop.section = c("fixef", "stats"), tex = T)
closeAllConnections() # Close connection to log file
ifrm(txt_export)


#   > end Table export----




# PLOTS RESIDUALS  =======================================================================================================================================
#   > rename cntry names in df_res_fe9   --------------------------------------
df_res_fe9$cntry_name %>% unique
long_names <- c("United Arab Emirates", "Antigua and Barbuda", "Bosnia and Herzegovina", "North Macedonia",  
  "Papua New Guinea",   "Trinidad and Tobago", "Dominican Republic",  "Myanmar (Burma)",  
  "Guinea-Bissau", "The Bahamas")
short_names <- c("U.A. Emirates", "Antigua & B.", "Bosnia & Herz.", "N. Macedonia",  
                "Papua N.G.",   "Trinidad-T.", "Domin. Rep.",  "Myanmar",  
                "Guinea-Bis.", "Bahamas")
for(i in 1: length(long_names) ){
  df_res_fe9$cntry_name[ (df_res_fe9$cntry_name %in% long_names[i] )] <- short_names[i]
}
unique(df_res_fe9$cntry_name)

ifrm(long_names)
ifrm(short_names)

#   > qq for residuals  --------------------------------------
export_fe_qqplots(df_res_fe9)


#   > tbars for residuals  --------------------------------------
obs_fe_tbarplots(df_res_fe9)


#   > acf for residuals  --------------------------------------
export_fe_acf_plots(df_res_fe9)



 # PLOTS OTHERS  =======================================================================================================================================
#   > tseries MOBILITY by sub_region  --------------------------------------
# create data frame to later plot
{
  start_xlim_date <- min(df_MAIN$date_day)
  end_xlim_date <- max(df_MAIN$date_day)
  
  df_plot_mob_t <- data.frame( 
    cntry_name = NA,
    iso2 = rep(unique(df_MAIN$iso2), each = length(start_xlim_date:end_xlim_date)), 
    iso2_factor = NA,
    region = NA, 
    sub_region = NA, 
    date_day = rep(as.Date(start_xlim_date:end_xlim_date), times = length(unique(df_MAIN$iso2)))  )
  
  df_plot_mob_t$cntry_name <- df_iso_names$county_name[ match(df_plot_mob_t$iso2, df_iso_names$iso2)]
  df_plot_mob_t$region <- df_iso_names$region[ match(df_plot_mob_t$iso2, df_iso_names$iso2)]
  df_plot_mob_t$sub_region <- df_iso_names$sub_region[ match(df_plot_mob_t$iso2, df_iso_names$iso2)]
  df_plot_mob_t$iso2_factor <- as.factor(df_plot_mob_t$iso2)
  
  df_plot_mob_t$retail_and_recreation <- df_MAIN$retail_and_recreation[
    match( interaction( df_plot_mob_t$iso2, df_plot_mob_t$date_day) , interaction(df_MAIN$iso2, df_MAIN$date_day) ) ]
  df_plot_mob_t$grocery_and_pharmacy <- df_MAIN$grocery_and_pharmacy[
    match( interaction( df_plot_mob_t$iso2, df_plot_mob_t$date_day), interaction(df_MAIN$iso2, df_MAIN$date_day))]
  df_plot_mob_t$parks <- df_MAIN$parks[
    match( interaction( df_plot_mob_t$iso2, df_plot_mob_t$date_day), interaction(df_MAIN$iso2, df_MAIN$date_day))]
  df_plot_mob_t$transit_stations <- df_MAIN$transit_stations[
    match( interaction( df_plot_mob_t$iso2, df_plot_mob_t$date_day), interaction(df_MAIN$iso2, df_MAIN$date_day))]
  df_plot_mob_t$workplaces <- df_MAIN$workplaces[
    match( interaction( df_plot_mob_t$iso2, df_plot_mob_t$date_day), interaction(df_MAIN$iso2, df_MAIN$date_day))]
  df_plot_mob_t$residential <- df_MAIN$residential[
    match( interaction( df_plot_mob_t$iso2, df_plot_mob_t$date_day), interaction(df_MAIN$iso2, df_MAIN$date_day))]
  
  head(df_plot_mob_t)
  subregion_names <- unique(df_plot_mob_t$sub_region)
  mob_names_column <- colnames(df_plot_mob_t)[7:12]
  
  ifrm(start_xlim_date)
  ifrm(end_xlim_date)
  
} 
# mean percentage for March in Western Europe for reatail_recreation and grocery_pharamcies
{
  head(df_plot_mob_t)
  mob_march_west_europe <- subset(df_plot_mob_t, sub_region == "Western Europe" & date_day >= as.Date("2020-03-01") & date_day <= as.Date("2020-03-31") )
  mob_march_west_europe$sub_region %>% table
  mob_march_west_europe$date_day %>% table
  
  mob_march_west_europe$retail_and_recreation %>% mean
  mean_mob_march_west_europe <-
    aggregate(
    mob_march_west_europe[c("retail_and_recreation", "grocery_and_pharmacy")],
    list(mob_march_west_europe$cntry_name), 
    mean, na.rm =T )
  mean_mob_march_west_europe
  print(paste("mean for retail_recreation in Western Europe for March 2020 =>  ",mean_mob_march_west_europe$retail_and_recreation %>% mean),sep="")
  print(paste("mean for gorcery_and_pharmacy in Western Europe for March 2020 =>  ",mean_mob_march_west_europe$grocery_and_pharmacy %>% mean),sep="")
  
  ifrm(mob_march_west_europe)
  ifrm(mean_mob_march_west_europe)
}
# loop to generate plots by sub_region
{
  for(i in 1: length(mob_names_column) ){
    
    graphics.off()
    if(!file.exists("plots/mob_tseries_plots") ){
      dir.create("plots/mob_tseries_plots") }
    
    if(!file.exists( paste("plots/mob_tseries_plots","/", mob_names_column[i], sep="") ) ){
      dir.create( paste("plots/mob_tseries_plots","/", mob_names_column[i], sep="") ) }
    
    for(ii in 1: length(subregion_names)  ){
      
      df_plot_ld_t_pcountry_SUB_REGION <- subset(df_plot_mob_t, sub_region == paste(subregion_names[ii], sep = "")  )
      
      ggplot(data = df_plot_ld_t_pcountry_SUB_REGION, aes(x = date_day, y= get(mob_names_column[i]), group = as.factor(cntry_name) ))+
        geom_line(aes(color= as.factor(cntry_name) ), size = 0.25) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      
        theme_minimal() +
        labs(#title=paste("Changes in visits to ", mob_names_title[i], " in ", subregion_names[ii], sep=""),
             x="Daily observations", 
             y ="% change from baseline", 
             color = "")
      
      ggsave(
        paste("plots/mob_tseries_plots","/", mob_names_column[i],"/",mob_names_column[i], "_", subregion_names[ii],".png", sep=""),
        plot = last_plot(),
        #device = NULL,
        scale = 1,
        width = 16,
        height = 8,
        units = "cm",
        dpi = dpi_ggplots,
        limitsize = TRUE)
      
      ifrm(df_plot_ld_t_pcountry_SUB_REGION)
    }
    
  }
  
  ifrm(df_plot_mob_t)
  ifrm(subregion_names)
  ifrm(mob_names_column)
  ifrm(i)
  ifrm(ii)
}


#   > geo map plot nobs > MAP PLOTTING-----------------------------------------------------------------------------------
{
df_map_world <- map_data("world")
df_map_fe9a_blnc <- df_map_world
df_map_fe9a_blnc
# if used, must be addressed properly
df_map_fe9a_blnc$region[df_map_fe9a_blnc$region == "USA"] <- "United States"
#
df_map_fe9a_blnc$iso2 <- df_iso_names$iso2[match(df_map_fe9a_blnc$region, df_iso_names$county_name)]
cnt_df_res_fe9a <- count(subset(df_res_fe9, !is.na(res_fe9a)), vars = iso2)
cnt_df_res_fe9a
cnt_df_res_fe9a$nobs / 319
colnames(cnt_df_res_fe9a) <- c("iso2", "nobs")
df_map_fe9a_blnc <- left_join(df_map_fe9a_blnc, cnt_df_res_fe9a, by = "iso2" )
# View(df_map_fe9a_blnc)

# erase small counts (blow 249 => give NA, to keep as grey polygons on map)
df_map_fe9a_blnc$nobs %>% sort
df_map_fe9a_blnc$ nobs[df_map_fe9a_blnc$nobs < 100] <- NA

map_fe9a_blnc <- ggplot(df_map_fe9a_blnc, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = nobs), color = "black", size = 0.1) + 
  scale_fill_gradient( name ="number of observations", low = "yellow", high = "forest green", na.value = "grey80") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        rect = element_blank(), 
        legend.position = "bottom",  #c(0.2, 0.2)) + 
        legend.background = element_rect(fill="grey80", size=0.5, linetype="solid", color = "black")) +


  labs(title = "") # Number of observations by country for regression fe9a
map_fe9a_blnc

ggsave("plots/nobs_map_fe9a.png",
      plot = map_fe9a_blnc,
      device = NULL,
      scale = 1,
      width = 16,
      height = 10.25,
      units = "cm",
      dpi = dpi_ggplots,
      limitsize = TRUE)

rm(df_map_world, df_map_fe9a_blnc, cnt_df_res_fe9a, map_fe9a_blnc)
}



# MOVE PLOTS  =======================================================================================================================================
#  > to one folder  --------------------------------------
figrues_thesis_path <- "plots/0_use_in_thesis/"
{
# move the plots I need for the paper to one single location
  if( !file.exists(figrues_thesis_path) ){
    dir.create(figrues_thesis_path) }
  
  # nobs over time "gap-period"

  file.copy(from = "plots/nobs_over_time_fe9.png",  
            to = paste( figrues_thesis_path,"nobs_over_time_fe9.png", sep ="" )
            , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  
  # tbar plots and world map
  file.copy(from = "plots/fe_nobs_tbarplots/fe9a_tbar.png", 
              to = paste( figrues_thesis_path,"fe9a_tbars.png", sep ="" )
              , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  file.copy(from = "plots/nobs_map_fe9a.png",
              to = paste( figrues_thesis_path, "nobs_map_fe9a.png", sep = "" )
              , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  
  # tseris of mobility
  file.copy(from = "plots/mob_tseries_plots/retail_and_recreation/retail_and_recreation_Western Europe.png",  
              to = paste( figrues_thesis_path,"retail_and_recreation_Western Europe.png", sep ="")
              , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE) 
  file.copy(from = "plots/mob_tseries_plots/grocery_and_pharmacy/grocery_and_pharmacy_Western Europe.png",
              to = paste( figrues_thesis_path,"grocery_and_pharmacy_Western Europe.png", sep = "")
              , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  
  # all QQ plots
  file.copy(from = "plots/fe_res_qqplots/fe9a_res_qq.png",   to = paste( figrues_thesis_path,"fe9a_res_qq.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_qqplots/fe9b_res_qq.png",   to = paste( figrues_thesis_path,"fe9b_res_qq.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_qqplots/fe9c_res_qq.png",   to = paste( figrues_thesis_path,"fe9c_res_qq.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_qqplots/fe9d_res_qq.png",   to = paste( figrues_thesis_path,"fe9d_res_qq.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_qqplots/fe9e_res_qq.png",   to = paste( figrues_thesis_path,"fe9e_res_qq.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_qqplots/fe9f_res_qq.png",   to = paste( figrues_thesis_path,"fe9f_res_qq.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  
  # all ACF plots
  file.copy(from = "plots/fe_res_acf_plots/fe9a_res_acf.png",  to = paste( figrues_thesis_path,"fe9a_res_acf.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_acf_plots/fe9b_res_acf.png",  to = paste( figrues_thesis_path,"fe9b_res_acf.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_acf_plots/fe9c_res_acf.png",  to = paste( figrues_thesis_path,"fe9c_res_acf.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_acf_plots/fe9d_res_acf.png",  to = paste( figrues_thesis_path,"fe9d_res_acf.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_acf_plots/fe9e_res_acf.png",  to = paste( figrues_thesis_path,"fe9e_res_acf.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )
  file.copy(from = "plots/fe_res_acf_plots/fe9f_res_acf.png",  to = paste( figrues_thesis_path,"fe9f_res_acf.png", sep = ""), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE )

  
  # statement of ownership 
  file.copy(from = "writing/statement_of_authorship_RH_sign.png", 
            to = paste( figrues_thesis_path,"statement_of_authorship_RH_sign.png", sep = "")
            , overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  
  ifrm("figrues_thesis_path")
}

file.copy( from = figrues_thesis_path, to = "writing/0_thesis/", overwrite = T, recursive = T, copy.mode = T)
if(dir.exists("writing/0_thesis/figures") ){
  unlink("writing/0_thesis/figures", recursive = T)  }
file.rename( from = "writing/0_thesis/0_use_in_thesis", to = "writing/0_thesis/figures/")


# RESIDUAL ANALYSIS  =======================================================================================================================================
#   > Normality of Residuals (Jarque Bera)     --------------------------------------
df_res_plot <- df_res_fe9

  res_fe_names <- colnames(df_res_plot)[4:9]
  path_df_res_plot <- paste("plots/", "reg_tables_latex",  sep="")
  if(!file.exists(path_df_res_plot )){
    dir.create(path_df_res_plot )
  }
  for(i in 1:6){
    
    print(c(colnames(df_res_plot)[c(1:3)], res_fe_names[i]) )
    
    sub_df_res_plot_noNA <- subset(df_res_plot, !is.na(df_res_plot[,i+3]))[,c(1:3,i+3)]
    colnames(sub_df_res_plot_noNA)[4] <- "jb_res_of_fe"
    
    assign(paste("jb_table_", res_fe_names[i], sep=""),
           sub_df_res_plot_noNA %>%
             group_by(as.factor(cntry_name)) %>%
             do(tidy(jarque.bera.test(.$jb_res_of_fe)))  )
  }
  
  df_jb_table <- data.frame(
    cntry_name =   sort( unique ( c(  
      as.character(get(paste("jb_table_", res_fe_names[1], sep =""))$`as.factor(cntry_name)`),  
      as.character(get(paste("jb_table_", res_fe_names[2], sep =""))$`as.factor(cntry_name)`),  
      as.character(get(paste("jb_table_", res_fe_names[3], sep =""))$`as.factor(cntry_name)`),  
      as.character(get(paste("jb_table_", res_fe_names[4], sep =""))$`as.factor(cntry_name)`),  
      as.character(get(paste("jb_table_", res_fe_names[5], sep =""))$`as.factor(cntry_name)`),  
      as.character(get(paste("jb_table_", res_fe_names[6], sep =""))$`as.factor(cntry_name)`) 
    ) ) ) 
  )
  
  for(i in 1:6){
    jb_tab <- get(paste("jb_table_", res_fe_names[i], sep =""))
    df_jb_table[[res_fe_names[i]]] <- NA
    
    df_jb_table[[res_fe_names[i]]] [
      match(as.character(jb_tab$`as.factor(cntry_name)`), df_jb_table$cntry_name )  ] <- 
      round(jb_tab$p.value, digit = 4)
  }
  
  colnames(df_jb_table) <- c("Country", mob_names_title)
  
  options(width=10000)
  txt_export <- file(paste(path_df_res_plot,"/jarque_bera_a-f", ".txt",  sep=""))
  sink(txt_export, append = TRUE, type = "output") # Writing console output to log file
  sink(txt_export, append = TRUE, type = "message")

  xtable(df_jb_table)
  
  closeAllConnections() # Close connection to log file
  options(width=100)
  print("table exported")
  
  rm(list = c(paste("jb_table_", res_fe_names[1],sep =""),
              paste("jb_table_", res_fe_names[2],sep =""),
              paste("jb_table_", res_fe_names[3],sep =""),
              paste("jb_table_", res_fe_names[4],sep =""),
              paste("jb_table_", res_fe_names[5],sep =""),
              paste("jb_table_", res_fe_names[6],sep ="") ))
  ifrm(df_res_plot)
  ifrm(res_fe_names)
  ifrm(path_df_res_plot)
  ifrm(sub_df_res_plot_noNA)
  ifrm((jb_tab))
  ifrm(df_jb_table)
  ifrm(i)
  ifrm(txt_export)
  
  #
  y<-xtable(df_jb_table)
  y[1,2]
  #
  
#   > end Table export----
  
  
  
# END  =======================================================================================================================================
{
  print("end file: MODEL")
  options(width = 500)
  ifrm(MAIN_PATH)
  ifrm(install_packages_new)
  
  # ifrm(mob_names_title)
  # ifrm(dpi_ggplots)
  # ifrm(a4_ggplots_height)
  # ifrm(a4_ggplots_width)
  
  print("cheers to countless hours of coding! :) thanks for reading.")
}
  

###########################0
### packages_functions.R ###
###########################0

getwd()

#  packages  =======================================================================================================================================
{
  # install all packeages again if needed
  if( exists("install_packages_new") ==T ){
    if(install_packages_new == T){
    
    print("re-install all packages")
      {
        
    # install.packages("AER")  
    # install.packages("readr")
    # install.packages("MASS")
    # install.packages("stats")
    # install.packages("base")
    # install.packages("ggplot2")
    # install.packages("dplyr")
    # install.packages("stargazer")
    # install.packages("lattice")
    # install.packages("tinytex")
    # install.packages("chron")
    # install.packages("xlsx")
    # install.packages("xtable")
    # install.packages("httr")
    # install.packages("jsonlite")
    # install.packages("methods")
    # install.packages("XML")
    # install.packages("xml2")
    # install.packages("rjson")
    # install.packages("tidyverse")
    # install.packages("plm")
    # install.packages("tibble")
    # install.packages("fixest")
    # install.packages("modelsummary")
    # install.packages("devtools")
    # install.packages("qqplotr")
    # install.packages("tseries")
    # install.packages("scales")
        
    }
  
    }else if( install_packages_new ==F ){print("skip re-install all packages")}
  }else if(exists("install_packages_new") ==F ){
  }
  # activate all packages 
  {
    library(AER)  
    library(readr)
    library(MASS)
    library(stats)
    library(base)
    library(ggplot2)
    library(dplyr)
    library(stargazer)
    library(lattice)
    library(tinytex)
    library(chron)
    library(xlsx)
    library(xtable)
    library(httr)
    library(jsonlite)
    library(methods)
    library(XML)
    library(xml2)
    library(rjson)
    library(tidyverse)
    library(plm)
    library(tibble)
    library(fixest)
    library(modelsummary)
    library(devtools)
    library(qqplotr)
    library(tseries)
    library(fastDummies)
    library(scales)
  
  }
}


#  functions =======================================================================================================================================
# settings for several plots   ------------------------------------------------------------------
mob_names_title <- c("retail and recreation", "grocery and pharmacy", "parks", "transit stations", "workplaces", "residential")
dpi_ggplots <- 600
a4_ggplots_height <- 28.7
a4_ggplots_width <- 21


# ifrm > remove object if existing  ------------------------------------------------------------------
ifrm <- function(obj, env = globalenv()) {
  obj <- deparse(substitute(obj))
  if(exists(obj, envir = env)) {
    rm(list = obj, envir = env)
  }
} # END FUNCTION --


# get_DEGROOT_weights > function to compute weights for each exogenous variable. ------------------------------------------------------------------
get_DEGROOT_weights <- function(fun_weight_name, fun_iso_o, fun_iso_d, fun_value){
  
  weight_name <<-  fun_weight_name   # "" name of the new weight column in MAIN
  iso_o <<-        fun_iso_o
  iso_d <<-        fun_iso_d
  value <-        fun_value
  
  NEW_degroot <<- data.frame(iso_o, iso_d, value)    #now calling all the countries that are in MAIN and sci, with their names and values
  df_MAIN.cntry_names <<- unique(df_MAIN$iso2)
  df_MAIN.cntry_names
  
  if( !( paste(weight_name) %in% names(df_MAIN) ) & !( "NEW_weight" %in% names(df_MAIN) ) ){
    df_MAIN <<- add_column(df_MAIN, NEW_weight  = NA, .before = "retail_and_recreation_percent_change_from_baseline") # create column of NA after date_day for trade weights
  }else{print(paste("column *",weight_name, "* in df_MAIN is already created", sep =""))}
  colnames(df_MAIN)[match("NEW_weight", colnames(df_MAIN))] <<- paste(weight_name)  # rename column according to the new weight
  
  
  
  for(i in 1: length(df_MAIN.cntry_names)){
    id_df_MAIN.cntry.i <<- df_MAIN$iso2 %in% df_MAIN.cntry_names[i]
    table(id_df_MAIN.cntry.i)
    df_MAIN.cntry.i <<-  df_MAIN[id_df_MAIN.cntry.i,]
    df_MAIN.cntry.NOTi_names <<- df_MAIN.cntry_names[!(df_MAIN.cntry_names %in% df_MAIN.cntry_names[i] ) ]
    
    df_MAIN.cntry_names
    df_MAIN.cntry_names[i]
    df_MAIN.cntry.NOTi_names
    
    df_MAIN.cntry.i$iso2
    df_MAIN.cntry_names[i] %in% df_MAIN.cntry.NOTi_names
    
    for(ii in 1: length(df_MAIN.cntry.NOTi_names) ){
      
      if(  ( (df_MAIN.cntry_names[i]        %in% unique(NEW_degroot$iso_o))  &
             (df_MAIN.cntry.NOTi_names[ii] %in% unique(NEW_degroot$iso_d))  ) == T  ){
        df_MAIN.cntry.NOTi_names[ii]
        id_iso_o.i_and_iso_d.NOTi <<- (NEW_degroot$iso_o %in% df_MAIN.cntry_names[i]) & 
          (NEW_degroot$iso_d %in% df_MAIN.cntry.NOTi_names[ii])
        
        if( T %in% id_iso_o.i_and_iso_d.NOTi){
          if( as.vector(table(id_iso_o.i_and_iso_d.NOTi))[2] == 1){
            if(!exists("n_iso_o.i_and_iso_d.NOTi")){    # creating a vector that contains all the row numbers to then call out the iso names and values
              n_iso_o.i_and_iso_d.NOTi <<- which(id_iso_o.i_and_iso_d.NOTi == T)
            }else if(exists("n_iso_o.i_and_iso_d.NOTi")){
              n_iso_o.i_and_iso_d.NOTi <<- append(n_iso_o.i_and_iso_d.NOTi, which(id_iso_o.i_and_iso_d.NOTi == T))
            }
          }
        }  
      }else if(  ( (df_MAIN.cntry_names[i]      %in% unique(NEW_degroot$iso_o))  &
                   (df_MAIN.cntry.NOTi_names[ii] %in% unique(NEW_degroot$iso_o))  ) == T  ){} 
      # if iso MAIN NOT in NEW_degroot, no point in adding it, because no mobility data to pair it with
      
    } # loop ii
    
    if(  exists("n_iso_o.i_and_iso_d.NOTi") ){
      NEW_degroot.i <<- data.frame(         #now calling all the countries that are in MAIN and NEW_degroot, with their names and values
        iso_d.NOTi_names =  NEW_degroot$iso_d[n_iso_o.i_and_iso_d.NOTi], #df_aggr_dot_gdp$Counterpart.iso2[n_user.i_and_fr.NOTi],
        n_in_NEW_degroot =  n_iso_o.i_and_iso_d.NOTi,
        val =               NEW_degroot$value[n_iso_o.i_and_iso_d.NOTi],  #df_aggr_dot_gdp$sum_dot[n_user.i_and_fr.NOTi], #df_aggr_dot_gdp$trade_gravity[n_user.i_and_fr.NOTi], 
        stand_val =         (NEW_degroot$value[n_iso_o.i_and_iso_d.NOTi] / sum(NEW_degroot$value[n_iso_o.i_and_iso_d.NOTi], na.rm=T)  ) #df_aggr_dot_gdp$sum_dot[n_user.i_and_fr.NOTi]/ sum(df_aggr_dot_gdp$sum_dot[n_user.i_and_fr.NOTi]))# (df_aggr_dot_gdp$trade_gravity[n_user.i_and_fr.NOTi])/sum(df_aggr_dot_gdp$trade_gravity[n_user.i_and_fr.NOTi]) # STANDARDIZE FOR ALL COUNTRIES (ALSO NOT IN mob) OR ONLY FOR INCLUDED ONES
      )
      sum(NEW_degroot.i$stand_val, na.rm = T)
      ifrm(n_iso_o.i_and_iso_d.NOTi)
    }
    # length(NEW_degroot.i$iso_d.NOTi_names) < length(df_MAIN.cntry.NOTi_names)
    
    
    for(t in 1: nrow(df_MAIN.cntry.i)  ){
      id_t.cntry.i <<- df_MAIN$date_day %in% df_MAIN.cntry.i$date_day[t] & df_MAIN$iso2 %in% df_MAIN.cntry_names[i]
      id_t.cntry.i
      id_t.cntry.NOTi <<- df_MAIN$date_day %in% df_MAIN.cntry.i$date_day[t] & df_MAIN$iso2 %in% df_MAIN.cntry.NOTi_names
      table(id_t.cntry.i)
      table(id_t.cntry.NOTi)
      
      
      df_MAIN.cntry.NOTi_names
      NEW_degroot.i$iso_d.NOTi_names
      
      t_common_names.NOTi <<- intersect(df_MAIN$iso2[id_t.cntry.NOTi], NEW_degroot.i$iso_d.NOTi_names)
      
      NEW_degroot.i_iso_d.NOTi_names_all <<-      NEW_degroot.i$iso_d.NOTi_names[ NEW_degroot.i$iso_d.NOTi_names %in% df_MAIN$iso2[id_t.cntry.NOTi]]
      NEW_degroot.i_iso_d.NOTi_stand_val_all <<-      NEW_degroot.i$stand_val[ NEW_degroot.i$iso_d.NOTi_names %in% df_MAIN$iso2[id_t.cntry.NOTi]]
      NEW_degroot.i_iso_d.NOTi_names_ordered <<-  NEW_degroot.i_iso_d.NOTi_names_all[order(match(NEW_degroot.i_iso_d.NOTi_names_all, t_common_names.NOTi))]
      NEW_degroot.i_iso_d.NOTi_stand_val_ordered <<- NEW_degroot.i_iso_d.NOTi_stand_val_all[order(match(NEW_degroot.i_iso_d.NOTi_names_all, t_common_names.NOTi))]
      
      df_MAIN.NOTi_names_all <<- df_MAIN$iso2[id_t.cntry.NOTi][df_MAIN$iso2[id_t.cntry.NOTi] %in% t_common_names.NOTi]
      df_MAIN.NOTi_ld_all <<- df_MAIN$ld_cornet[id_t.cntry.NOTi][df_MAIN$iso2[id_t.cntry.NOTi] %in% t_common_names.NOTi]
      df_MAIN.NOTi_names_ordered <<- df_MAIN.NOTi_names_all[order(match(df_MAIN.NOTi_names_all, t_common_names.NOTi))]
      df_MAIN.NOTi_ld_ordered <<- df_MAIN.NOTi_ld_all[order(match(df_MAIN.NOTi_names_all, t_common_names.NOTi))]
      
      t_common_names.NOTi
      df_MAIN.NOTi_names_ordered == t_common_names.NOTi
      NEW_degroot.i_iso_d.NOTi_names_ordered == t_common_names.NOTi
      
      stopifnot( length(NEW_degroot.i_iso_d.NOTi_names_ordered) == length(df_MAIN.NOTi_names_ordered)  )
      df_MAIN[c("iso2", "date_day")][id_t.cntry.i,]
      
      df_MAIN.cntry.i[paste(weight_name)][t,] <<- sum(NEW_degroot.i_iso_d.NOTi_stand_val_ordered * df_MAIN.NOTi_ld_ordered , na.rm = T)
      
      df_MAIN[paste(weight_name)][id_df_MAIN.cntry.i,] <<- df_MAIN.cntry.i[paste(weight_name)]
      
    } # end loop t
    
  } # end loop i
  
  ifrm(weight_name)
  ifrm(iso_o)
  ifrm(iso_d)
  # ifrm(value)
  ifrm(NEW_degroot)
  ifrm(df_MAIN.cntry_names)
  ifrm(id_df_MAIN.cntry.i)
  ifrm(df_MAIN.cntry.i)
  ifrm(id_iso_o.i_and_iso_d.NOTi)
  ifrm(NEW_degroot.i)
  ifrm(id_t.cntry.i)
  ifrm(id_t.cntry.NOTi)
  ifrm(t_common_names.NOTi)
  ifrm(df_MAIN.cntry.NOTi_names)
  ifrm(NEW_degroot.i_iso_d.NOTi_names_all)
  ifrm(NEW_degroot.i_iso_d.NOTi_stand_val_all)
  ifrm(NEW_degroot.i_iso_d.NOTi_names_ordered)
  ifrm(NEW_degroot.i_iso_d.NOTi_stand_val_ordered)
  ifrm(df_MAIN.NOTi_names_all)
  ifrm(df_MAIN.NOTi_ld_all)
  ifrm(df_MAIN.NOTi_names_ordered)
  ifrm(df_MAIN.NOTi_ld_ordered)
  
} # END FUNCTION --


# export_obs_fe_tseriesplots > function that counts time and object identifiers for reach fe, and exports it to plots  ------------------------------------------------------------------
obs_fe_tbarplots <- function(obj, env = globalenv()) {
  
  df_res_plot <<- obj
  path_df_res_plot <<- paste("plots/", "fe_nobs_tbarplots", sep="")
  if(!file.exists(path_df_res_plot )){
    dir.create(path_df_res_plot )
  }
  for(i in 1:6){
    
    # find if observation there and set number as value to plot later bar at that height in plot
    df_res_plot <<- cbind(df_res_plot, new_var =  NA) 
    df_res_plot$new_var[ !is.na(df_res_plot[,i+3]) ] <<- match( subset(df_res_plot, !is.na(df_res_plot[,i+3]))$cntry_name , sort( unique( subset(df_res_plot, !is.na(df_res_plot[,i+3]))$cntry_name ), decreasing = T)  )  

  

    # rename variable
    colnames(df_res_plot)[i+9] <<- paste("res_", paste(str_sub(colnames(df_res_plot)[i+3], 5,8), sep = ""), "_blnc", sep = "")
    
    # plot tseries graph
    sub_df_res_plot_noNA <<- subset(df_res_plot, !is.na(df_res_plot[,i+9]))[,c(1:3,i+9)]
    print(colnames(sub_df_res_plot_noNA))
    
    graphics.off()
    
    obs_tseries_plot <<- ggplot(sub_df_res_plot_noNA, mapping = aes(x = as.Date(date_day), y = sub_df_res_plot_noNA[,4] )) + # ,  color = cntry_name
      geom_point(shape = 15, size = 0.25) +
      scale_color_discrete(guide = F) +
      scale_fill_discrete(guide = F) +
      # scale_y_continuous(breaks = unique(match( sub_df_res_plot_noNA$cntry_name , sort( unique(sub_df_res_plot_noNA$cntry_name), decreasing = T ) ) ), 
      #                    labels = unique(sub_df_res_plot_noNA$cntry_name) ) +
      scale_y_continuous(breaks = unique(match( sub_df_res_plot_noNA$cntry_name , sort( unique(sub_df_res_plot_noNA$cntry_name), decreasing = T ) ) ), 
                         labels = unique(sub_df_res_plot_noNA$cntry_name) ) +
      theme_minimal() +
      theme(axis.text.y = element_text(color = "grey20", size = 7.5, angle = 0, hjust = 0, vjust = 0, face = "plain") ) + 
      labs(#title = paste("Oberservations over time, using ", mob_names_title[i], " mobility (by country)", sep="" ), 
           x ="Daily observations ", 
           y ="" ) 

    ggsave(
      paste(path_df_res_plot,"/", str_sub(colnames(sub_df_res_plot_noNA)[4], 5, 8), "_tbar.png", sep=""),
      plot = obs_tseries_plot,
      device = NULL,
      scale = 1,
      width = 21,  #14.85, half din a4
      height = 23,   #10.5,
      units = "cm",
      dpi =  dpi_ggplots,
      limitsize = TRUE)
    
    # ifrm(sub_df_res_plot_noNA)
    ifrm(obs_tseries_plot)
    print("plot exported")
    
  }
  # ifrm(df_res_plot)
  # ifrm(path_df_res_plot)

  
}# END FUNCTION -- 


# export_fe_qqplots > export residuals of df_res_feX in QQPlots  ------------------------------------------------------------------
export_fe_qqplots <- function(obj, env = globalenv()) {
  # sources:
  #     > https://www.youtube.com/watch?v=Z1apVUu5vJ4&ab_channel=EquitableEquations
  #     > https://www.youtube.com/watch?v=xOF70-3Mmkc&ab_channel=StatswithR
  #     > https://www.youtube.com/watch?v=zzUfWDKDjHE&ab_channel=StatswithR

  
  df_res_plot <<- obj
  path_df_res_plot <<- paste("plots/fe_res_qqplots", sep="")
  if(!file.exists(path_df_res_plot )){
    dir.create(path_df_res_plot )
  }
  
  for(i in 1:6){
    print(df_res_plot[,c(1:3,i+3)] %>% names)
    sub_df_res_plot_noNA <<- subset(df_res_plot, !is.na(df_res_plot[,i+3]))[,c(1:3,i+3)]
    
    
    graphics.off()
    
    qq_res_plot <<- ggplot(sub_df_res_plot_noNA, mapping = aes(sample = sub_df_res_plot_noNA[,4], 
                                                               group = as.factor(cntry_name), fill = "red" )) + # color = as.factor(cntry_name), 
      stat_qq_band(alpha = 0.40) +
      stat_qq_line(size = 0.15) +
      stat_qq_point(shape = 16 , size = 0.01  ) +
      
      scale_color_discrete(guide=FALSE) +
      scale_fill_discrete(guide=FALSE) +
      scale_x_continuous(breaks = c( round(min(sub_df_res_plot_noNA[,4]), digits = 0), round(0, digits = 0), round(max(sub_df_res_plot_noNA[,4]), digits = 0) ) )+
      scale_y_continuous(breaks = c( round(min(sub_df_res_plot_noNA[,4]), digits = 0), round(0, digits = 0), round(max(sub_df_res_plot_noNA[,4]), digits = 0) ) )+
      facet_wrap(~cntry_name) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 8.3) ) +
      labs(# title =  paste("Residuals QQ-plot, using ", mob_names_title[i],  " mobility (by country)", sep="") ,
           x ="theoratical normal distribution",
           y ="residual's distribution")

    ggsave(
      paste(path_df_res_plot,"/", str_sub(names(sub_df_res_plot_noNA)[4],5),"_res_qq.png", sep=""), 
      plot = qq_res_plot,
      device = NULL,
      scale = 1,
      width = a4_ggplots_width, 
      height = a4_ggplots_height,
      units = "cm",
      dpi =  dpi_ggplots,
      limitsize = TRUE)
    
    ifrm(qq_res_plot)
    ifrm(sub_df_res_plot_noNA)
    print("plot exported")
    
  }
  ifrm(df_res_plot)
  ifrm(path_df_res_plot)
  
  
} # END FUNCTION -- 


# export_fe_acf_plots ------------------------------------------------------------------
export_fe_acf_plots <- function(obj, env = globalenv()) {
  #
  # obj<-df_res_fe9
  # colnames(df_res_plot)[i+3]
  #
  df_res_plot <<- obj
  path_df_res_plot <<- paste("plots/", "fe_res_acf_plots", sep="")
  if(!file.exists(path_df_res_plot )){
    dir.create(path_df_res_plot )
  }
  
  for(i in 1:6){
    
    sub_df_res_plot <<- subset(df_res_plot, !is.na(df_res_plot[,i+3]))
    print(colnames(sub_df_res_plot)[c(1:3, i+3)])
    iso_names_acf_fun <<- unique(sub_df_res_plot$cntry_name)
    iso_names_acf_fun
    
    df_res_acf <<- data.frame(
      cntry_name = rep(iso_names_acf_fun, each = 30),
      lag = rep(1:30, times = length(iso_names_acf_fun) ))
    df_res_acf$acf_value <<- NA
    
    for(ii in 1: length(iso_names_acf_fun) ){
      
      vec_df_res_plot <- subset(sub_df_res_plot, cntry_name == iso_names_acf_fun[ii])[,i+3]

      df_res_acf$acf_value[ df_res_acf$cntry_name %in% iso_names_acf_fun[ii] ] <- acf(vec_df_res_plot, lag.max = 29, na.action = na.pass, plot = F)$acf
    }
    df_res_acf
    
    fe_res_acf_plot <<- ggplot(data = df_res_acf, mapping = aes(x = lag, y = acf_value), 
                               group = as.factor(cntry_name), fill = as.factor(cntry_name) ) +  # color = as.factor(cntry_name), 
      geom_hline( aes(yintercept = 0)) + # , color = cntry_name
      geom_segment(mapping = aes(xend = lag, yend = 0)) + # ,color = cntry_name
      
      scale_color_discrete(guide=FALSE) +
      scale_fill_discrete(guide=FALSE) +
      scale_x_continuous(breaks = seq(0,29,10) ) +
      scale_y_continuous(breaks = seq(0,1,0.5) ) +
      facet_wrap(~ as.factor(cntry_name)) +
      theme_minimal() +
        theme(
          strip.text = element_text(size = 12.5) ) +
      labs( # title = paste("Residuals ACF plot, using ", mob_names_title[i], " mobility (by country)",  sep="") , 
           x ="lags", 
           y ="correlation") 
    
    ggsave(
      paste(path_df_res_plot,"/", substr(colnames(df_res_plot)[i+3],5,8), "_res_acf.png", sep=""),
      plot = fe_res_acf_plot,
      device = NULL,
      scale = 1.5,
      width = a4_ggplots_width, 
      height = a4_ggplots_height,
      units = "cm",
      dpi =  dpi_ggplots,
      limitsize = TRUE)
    
    ifrm(sub_df_res_plot)
    ifrm(iso_names_acf_fun)
    ifrm(df_res_acf)
    ifrm(fe_res_acf_plot)
    print("plot exported")
    
  }
  
  ifrm(df_res_plot)
  ifrm(path_df_res_plot)
  
}# END FUNCTION --


#  END =======================================================================================================================================
print("end file: packages_functions.R")
ifrm(install_packages_new)


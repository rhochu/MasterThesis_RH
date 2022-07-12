#######################
### R1_dataimport.R ###
#######################

getwd()
t_start <- Sys.time()

# reimport_data_R1_raw = T/F   ----------------------------------
if (reimport_data_R1_raw == F) {
  load("data/0_R_data/R1_import.Rdata")
}else if(reimport_data_R1_raw == T){
  
  # mobility
  df_mob<- read.csv("data/b_google mobility/google mobility csvs/Global_Mobility_Report.csv",header=T,sep=",")
  
  # social proximity
  df_sci<- read.table(file = "data/a_facebook_sci/2020-12-16_country_country.tsv", header = T, sep = "\t")
  
  # lockdown / movement restrictions
  # df_acaps<-read.xlsx("data/c_acaps/acaps_covid19_government_measures_dataset_0.xlsx", sheetIndex = 2)
  # df_mrm<-read§.delim("data/d_movement-range-data-2021-02-21/movement-range-2021-02-21.txt")
  df_cornet<- read.csv("data/e_CoronaNet/1_coronanet_release_allvars.csv",header = T,sep = ",")
  
  # iso_names
  json.iso2 <- rjson::fromJSON(file = "http://country.io/names.json")
  json.iso3 <- rjson::fromJSON(file = "http://country.io/iso3.json")
  json.iso2_3<- names(json.iso3)==names(json.iso2) # check if ISO 2 letter code identical for all
  table(json.iso2_3)["TRUE"]
  
  df_iso_names <- data.frame(
    county_name = matrix(unlist(json.iso2), nrow=length(json.iso2), byrow=TRUE),
    iso3 =        matrix(unlist(json.iso3), nrow=length(json.iso3), byrow=TRUE),
    iso2 =        names(json.iso3))
  ifrm(json.iso2)
  ifrm(json.iso3)
  ifrm(json.iso2_3)
  
  # add continents to iso_names
  df_iso_cont <-read.csv("data/f_contients2/continents2.csv", sep = ",")
  head(df_iso_cont)
  df_iso_names$region <- df_iso_cont$region[match(df_iso_names$iso2, df_iso_cont$alpha.2) ]
  df_iso_names$sub_region <- df_iso_cont$sub.region[match(df_iso_names$iso2, df_iso_cont$alpha.2) ]
  table(df_iso_names$sub.region)
  ifrm(df_iso_cont)
  
  # fill up missing values
  df_iso_names[is.na(df_iso_names$region),]
  df_iso_names[ df_iso_names$county_name %in% "South Africa" ,]
  df_iso_names$region[ df_iso_names$county_name %in% "Namibia"] <- "Africa"
  df_iso_names$sub_region[ df_iso_names$county_name %in% "Namibia"] <- "Sub-Saharan Africa"
  df_iso_names[ df_iso_names$county_name %in% "Serbia" ,]
  df_iso_names$region[ df_iso_names$county_name %in% "Kosovo"] <- "Europe"
  df_iso_names$sub_region[ df_iso_names$county_name %in% "Kosovo"] <- "Southern Europe"

  
  # trade proximity
  # df_imf_dot <- read.csv("data/h_imf_dot/DOT_06-27-2021 01-57-15-41_timeSeries.csv",header=T,sep=",")
  # df_imf_names<-read.xlsx("data/h_imf_dot/co.xlsx", sheetIndex = 2, header = TRUE) # cells in xlsx file previously merged by hand, for easier import
  # https://data.imf.org/?sk=9D6028D4-F14A-464C-A2F2-59B2CD424B85  > general
  # https://data.imf.org/?sk=9D6028D4-F14A-464C-A2F2-59B2CD424B85&sId=1539174008154  > data download DOTS
  # https://www.imf.org/external/pubs/ft/weo/2014/01/weodata/co.xlsx   > data download NAMES
  
  # df_wb_gdp <- read.csv("data/i_gdp/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_2531304.csv",header=T,sep=",", skip = 3)
  # https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2019&start=1960
  
  # cepii: geodistance, trade history, direction of trade, exchange rates, gdp population 
  df_cepii_dist <- read.csv("data/j_cepii/cepii_geodist/dist_cepii.csv", header = T, sep = ",")
  df_cepii_geo  <- read.xlsx("data/j_cepii/cepii_geodist/geo_cepii.xls", sheetIndex = 1, header =T)
  df_cepii_bitrade <- read.csv("data/j_cepii/cepii_tradehist/TRADHIST_BITRADE_BITARIFF_3.csv", header = T, sep = ",")
  df_cepii_exrates <- read.csv("data/j_cepii/cepii_tradehist/TRADHIST_EXCHANGE_RATES.csv", header = T, sep = ",")
  df_cepii_gdp_pop <- read.csv("data/j_cepii/cepii_tradehist/TRADHIST_GDP_POP.csv", header = T, sep = ",")
  
  
  # save_raw_data T/F   -------------------------------------
  if(save_data_R1_import == T){
    save(list =  ls(.GlobalEnv),file = "data/0_R_data/R1_import.Rdata")
  }
  
} 



# end  ----------------------------------------
t_end <- Sys.time()
t_R1 <- t_end-t_start
ifrm(t_start)
ifrm(t_end)

print(paste("end file: R1_dataimport.R", ";  runtime ", round(t_R1[[1]],2)," ", units(t_R1), sep=""))


ifrm(reimport_data_R1_raw)
ifrm(save_data_R1_raw)
ifrm(save_data_R1_import)

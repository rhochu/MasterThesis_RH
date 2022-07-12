##################################################
### Master Thesis -  data_aggregation_MASTER.R ###
##################################################
# author:     Raul Hochuli (raul.hochuli@uzh.ch)
# University of Zurich, Department of Economics
# supervisor: Prof. Dr. David Yanagizawa-Drott (david.yanagizawa-drott@econ.uzh.ch), 
#             Dr. Alexandra Schubert (lexi.schubert@econ.uzh.ch)


# Adjust directory HERE 
MAIN_PATH <- "/Users/raulhochuli/Dropbox/0_raulhochuli/Dokumente_DB/2_Ausbildung/UZH/21_FS/MasterThesis_RaulHochuli_2013_713_110"  
install_packages_new <- F


# start --------------------------------------
rm(list=setdiff(ls(), c("MAIN_PATH", "install_packages_new")))
setwd(MAIN_PATH)
getwd()
list.files() # should show whole content of the master thesis folder
graphics.off()
cat("\014")

t_Rall_start <- Sys.time()


# packages & functions   --------------------------------------
# > get all packages and functions first. In case 
install_packages_new <- F     # T, if packages are not yet installed. installs all packages freshly and restarts R,     
                              # F, if packages are already installed.
source("stat/packages_functions.R") # get all packages & functions



# R1 dataimport   --------------------------------------
reimport_data_R1_raw = T    # T, running the importing script, collecting raw data,     
# >                         # F, importing .Rdata- file, of previous run
  save_data_R1_import = T   # T, saving Global Environment at end of R1 file
                            # F, not saving 
source("stat/data_aggregation_subfiles/R1_dataimport.R")



# R2 setupMAIN  --------------------------------------
reduce_sample_for_test = F    # T, reduces df_mob to a small test sample, used for short runtime while function for weight aggregation
                              # F, takes full sample, for actual computation later
run_setupmain = T             # T, importing -Rdata - file
# >                           # F, running the scirpt to setup MAIN data set
  save_data_R2_setupmain = T  # T, saving MAIN data set (after aggregating it)
                              # F, not saving
source("stat/data_aggregation_subfiles/R2_setupMAIN.R")



# R3 weights SCI --------------------------------------
sanity_check_sci_matrix = F     # T, checks if df_sci in long format is complete as matrix in both dimensions
                                # F, omits this check
run_sci_weights_generation = T  # T, runs script to create sci weights and adds them to df_MAIN
# >                             # F, does not run script but just loads save_data_R3_weights
  save_data_R3_weights = T      # T, saving Global Envir (after weights creation)
                                # F, not saving
source("stat/data_aggregation_subfiles/R3_weights_SCI.R")



# R4b weights DOTS  --------------------------------------
sanity_check_bitrade = F                  # T, run sanity check for bitrade  
                                          # F, don't run 
run_cepii_bitrade_weights_generation = T  # T, runs script to create DOTS weights (of CEPII) and adds them to df_MAIN 
# >                                       # F, laod R4b_weights.Rdata
  save_data_R4b_weights = T               # T, saving Global Envir 
                                          # F, not saving
source("stat/data_aggregation_subfiles/R4b_weights_BITRADE.R")



# R5 weights DIST  --------------------------------------
run_dist_weights_generation = T   # T,runs script to create DIST weights and adds them to df_MAIN 
# >                               # F, does not run script but just loads data_R5_weights
  save_data_R5_weights = T        # T, saving Global Envir 
                                  # F, not saving
source("stat/data_aggregation_subfiles/R5_weights_DIST.R")


 
# R6 weights GRAV   --------------------------------------
run_grav_weights_generation = T   # T, runs script to create GRAV weights and adds them to df_MAIN 
# >                               # F, does not run script but just loads data_R6_weights
  save_data_R6_weights = T        # T, saving Global Envir 
                                  # F, not saving
source("stat/data_aggregation_subfiles/R6_weights_GRAV.R")



# R7 weights LANG   --------------------------------------
run_lang_weights_generation = T   # T, runs script to create LANG weights and adds them to df_MAIN 
# >                               # F, does not run script but just loads data_R7_weights
  save_data_R7_weights = T        # T, saving Global Envir 
                                  # F, not saving
source("stat/data_aggregation_subfiles/R7_weights_LANG.R")



# save df_MAIN & Envir   --------------------------------------
save(list =  ls(.GlobalEnv), file = "data/0_R_data/MASTER_Envir_final.Rdata")
write.csv(df_MAIN,"data/0_R_data/df_MAIN_final.csv", row.names = T)



# end  --------------------------------------
t_Rall_end <- Sys.time()
t_Rall <- t_Rall_end-t_Rall_start
ifrm(t_Rall_start)
ifrm(t_Rall_end)
t_Rall



# time overview  --------------------------------------
t_R1
t_R2
t_R3
t_R4b
t_R5
t_R6
t_R7
t_Rall

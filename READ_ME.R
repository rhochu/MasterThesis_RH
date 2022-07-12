#################################0
### Master Thesis -  READ_ME.R ###
#################################0
# author:     Raul Hochuli (raul.hochuli@uzh.ch)
# University of Zurich, Department of Economics
# supervisor: Prof. Dr. David Yanagizawa-Drott (david.yanagizawa-drott@econ.uzh.ch), 
#             Dr. Alexandra Schubert (lexi.schubert@econ.uzh.ch)



# intro  =========================================================================================================

# Dear Reader, 
# This short R-file tries to quickly describe the major components of the statistical and computational 
# part of my master thesis. For the entire analysis, I used R as it provided all enough capability for 
# data cleaning, while still offering highly sophisticated regression and plotting packages.  
# Most files are written for sourcing, this means they should NOT actively executed by you (but you can 
# of course look at them). These supportive files are written in such a way, that the important main 
# files access them and execute them when it is required. For the script to run properly, it is 
# necessary to adjust the main directory path to the folder "MasterThesis_RaulHochuli_2013_713_110". From 
# there, the script access all subfolders, supportive files independently 
# Set the directory here:
MAIN_PATH <- "/Users/raulhochuli/Dropbox/0_raulhochuli/Dokumente_DB/2_Ausbildung/UZH/21_FS/MasterThesis_RaulHochuli_2013_713_110"  
setwd(MAIN_PATH)
getwd()


# packages_functions.R  > supportive file  =========================================================================

# This file contains all packages required for the script to run smoothly as well as all functions I wrote myself. 
# It is therefore executed by every main file, at the very beginning, so that all necessary functions are loaded.  
# If the script is run for the first time, it is most likely that the packages have to be installed before they 
# can get activated /called. You can do this by turning "install_packages_new" on (set to TRUE) at the beginning of 
# each main file. 

# WARNING: this will install all packages anew and reset the R session each time, which will take some time. It has
# to be done only one however. Afterwards, "install_packages_new" should be turned off (set to FALSE)


# data_aggregation_MASTER.R  > main file  =============================================================================

# This file functions as a "cockpit" for running the entire data aggregation process. It has 7 chapters, 
# 1 for every major data aggregation operation. At the beginning of each chapter in the master file, the necessary 
# controls can be turned on (set to TRUE) or off (set to FALSE). All controls are commented, to explain shortly 
# what they do. After setting the controls, the master file access the corresponding subfile of the chapter, which 
# is stored in the folder "stats/data_aggregation_subfiles". A the end of each subfile, the script will save the 
# intermediate results by saving the entire R environment to "data/0_R_data" (if the controls are set right).

# WARNING I: the entire data aggregation file performs a lot of tasks! Depending on your computer, this can take a long
# time (for my laptop well over 1 hour). The data aggregation part can be skipped if needed. The MODEL.R file, 
# automatically calls up the latest final version of aggregated data environment, so you can start running models and 
# plots right away.
# WARNING II: during the aggregation process, I ran some sanity checks on the data sources, to be sure, that the data 
# I use makes sense. This runs even much more tasks which increases the run time to well over 10 hours. Using the 
# controls in the master file you can (and should) easily skip these sanity checks. 
# WARNING III: The controls must be set. Without the controls defined, the subfiles will not run, and give an error messages. 

# Open the data_aggregation_MASTER.R - file here:
file.edit("stat/data_aggregation_MASTER.R")


# MODEL.R  > main file  ===================================================================================================

# This file contains the heart and soul of the analysis. It has several chapters, structuring the script into the 
# regression setups, plotting and residual analysis. The regression chapter formulates, runs and shows all regressions incl.
# the robustness checks using the packages "fixest". At the end of the regression setup chapter, the relevant output tables 
# are shown in the console and also exported as txt-files in latex. Using ggplot2, the plot chapter uses self-written 
# functions to export a series of plots for visualization, most importantly for the residual analysis. The last chapter 
# gives formal tests for the residual analysis, to support the visual conclusions more accurately. 

# WARNING: "fixest" is incredibly fast, such that all models are run within less than a minute. However especially the plot 
# functions need more time for plotting and then exporting the graphs. This second part takes therefore roughly 10 min.

# Open the MODEL.R - file here:
file.edit("stat/MODEL.R")


#-----------------
# 0-config
# 
# Objective: set up file paths and load libraries for project
#-----------------

rm(list=ls())

#------------------------
# load required libraries
#------------------------
library(here)
library(ggplot2)
library(pdfsearch)
library(dplyr)
library(gridExtra)
library(openxlsx)
library(writexl)
library(rdhs)
library(plyr)
library(readxl)
#libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)    # for creating tables with Haven labeled data
library(naniar)   # to use replace_with_na function
library(here)       # to get R project path
library(sjlabelled) # to set variables label
library(survey)  # to calculate weighted ratio for GAR
library(srvyr)
library(dplyr)
library(labelled)

library(rvest) #used to group countries by region (dhs)

library(readxl) #used to read excel of world bank country wealth categories

library(utils)
library(haven)
library(survey)
library(foreign)
library(plyr)
library(magrittr) 
library(readxl)
library(haven)
#library(xlsx) #needs java downloaded
library(openxlsx)
library(fuzzyjoin)
library(stringdist)
#------------------------
# define data paths
#------------------------
# Box path
if(Sys.getenv("USER")=="yoshika"){
  box_path = "~/Library/CloudStorage/Box-Box/safewatertime/"
}else if(Sys.getenv("USER")=="oliviaw"){ # fill in for own computer
  box_path = "~/Library/CloudStorage/Box-Box/safewatertime/"
}

fulltexts = paste0(box_path, "Safe water womens time full text")
practicetexts = paste0(box_path, "Practice Text read")

dhszips = paste0(box_path, "data/dhs/zip files")
dhszips_out = paste0(box_path, "data/dhsDHS_unzipped_files")
population = paste0(box_path, "data/population_WB.csv")
dhs_dta =  paste0(box_path, "data/dhs/DHS_dta")
pr_dta =  paste0(box_path, "data/dhs/pr_dta")
mics3_dta  =  paste0(box_path, "data/mics/MICS_data/MICS3")
mics4_dta  =  paste0(box_path, "data/mics/MICS_data/MICS4")
mics5_dta  =  paste0(box_path, "data/mics/MICS_data/MICS5")
mics6_dta  =  paste0(box_path, "data/mics/MICS_data/MICS6")
#------------------------
# define local results paths
#------------------------
if(Sys.getenv("USER")=="___"){
  data_dir = paste0(here::here(),"/data/")
}else if(Sys.getenv("USER")=="-"){ 
  data_dir = paste0(here::here())
}

reports_path = paste0(here::here(), "/7-reports")

#------------------------
# define figure/table paths
#------------------------
fig_path = paste0(here::here(), "/5-figures")
tab_path = paste0(here::here(), "/6-tables")
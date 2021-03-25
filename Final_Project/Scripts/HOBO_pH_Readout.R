# Preprocessing script for hobo logger data
# Brings in sinlge raw .csv files and exports tidy files into both a general tidy folder for temporary storage and an output folder

# created: 9-23-2020 by Danielle Barnas
# modified: 9-29-2020


## Load Libraries and Functions
rm(list=ls())

library(tidyverse)
library(lubridate)
library(here)

source(here("Final_Project","Scripts","Define_Functions.R"))


########################
# File Names
########################

Serial<-'197' # pH Probe Serial Number

# Date of in situ logs
Launch<-'2021-01-18 09:00:00' # Maintain date time format "YYYY-MM-DD HH:MM:SS"
Retrieval<-'2021-01-20 15:00:00' # Maintain date time format "YYYY-MM-DD HH:MM:SS"


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

### Read in Logger Files
path.Log<-here("Final_Project","Data")

pHLog<-pH_cleanup(path = path.Log, pH_serial = Serial)

# Parse date filters into date and type vector types
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
pHLog<-pHLog%>%filter(between(date,Launch,Retrieval)) 

# write file
write_csv(pHLog,paste0(here("Final_Project","Output"),'/pH_',Serial,'_',Sys.Date(),'.csv'))


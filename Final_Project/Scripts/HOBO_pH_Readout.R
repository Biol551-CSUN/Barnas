##########################################################################
##########################################################################
#### Pre-processing script for HOBO pH logger data
#### Brings in raw .csv files by Serial number and exports a tidy file

# Author: Danielle Barnas
# created: 9-23-2020
# modified: 3-25-2021

##########################################################################
##########################################################################

########################
### Load Libraries
########################

library(tidyverse)
library(lubridate)
library(mooreasgd)
library(here)

rm(list=ls())

########################
### Serial, Date, and File Path
########################

# pH probe serial number
Serial<-'197'
# Log date
log.date<- '2021-01-18'
# Path to folder storing logger .csv files
path.Log<-here("Final_Project","Data")

########################
### Launch and Retrieval Times
########################

# "YYYY-MM-DD HH:MM:SS"

# Date of in situ logs
Launch<-'2021-01-18 09:00:00'
Retrieval<-'2021-01-20 15:00:00'


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

########################
### Read in Logger Files
########################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number
pHLog<-pH_cleanup(path = path.Log, pH_serial = Serial)

########################
### Parse date and time
########################

# Parse launch and retrieval dates into date and type vector types
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

########################
### Filter by date and time
########################

pHLog<-pHLog%>%filter(between(date,Launch,Retrieval)) 

########################
# Write CSV file and graph data
########################

write_csv(pHLog,paste0(here("Final_Project","Output"),'/pH_',Serial,'_',log.date,'.csv'))

pHLog %>%
  ggplot(aes(x= date, y= TempInSitu))+   #setup plot with x and y data
  geom_line()

pHLog %>%
  ggplot(aes(x= date, y= pH))+   #setup plot with x and y data
  geom_line()


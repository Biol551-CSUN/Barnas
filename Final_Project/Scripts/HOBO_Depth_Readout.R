##########################################################################
##########################################################################
#### Pre-processing script for HOBO water level logger data
#### Brings in calibrated .csv files by Serial number and exports a tidy file

# Author: Danielle Barnas
# created: 3-10-2020
# modified: 3-25-2021

##########################################################################
##########################################################################

### BEFORE RUNNING SCRIPT ###
# Load HoboWare .hobo datafile in HoboWare software program
# Select Series 1 and 2 for Measurements Abs Pres and Temp in kPa and degC, respectively
# Deselect all events
# Select the Barometric Compensation Assistant and click Process...
# Choose Salt Water (1,025.000 kg/m3)
# Check to Use a Reference Water Level and enter 0.000 Meters
# Select a Reference Time when Logger was exactly at the water surface prior to or post-deployment
# Call Resultant Series Name: Water Level and click Create New Series
# Check that Series 3 for Water Level in meters is selected and click Plot
# Export file into your dated data folder (path below)

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

# water level probe serial number
Serial<-'876'
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
Retrieval<-'2021-01-18 13:20:00'


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

########################
### Read in Logger Files
########################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number
depthLog<-WL_cleanup(path = path.Log, wl_serial = Serial)

########################
### Parse date and time
########################

# Parse launch and retrieval dates into date and type vector types
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

########################
### Filter by date and time
########################
depthLog<-depthLog%>%filter(between(date,Launch,Retrieval)) 

########################
# Write CSV file and graph data
########################

write_csv(depthLog,paste0(here("Final_Project","Output"),'/depth_',Serial,'_',log.date,'.csv'))

# Plotting
depthLog %>%
  ggplot(aes(x= date, y= TempInSitu))+   #setup plot with x and y data
  geom_line()
depthLog %>% 
  ggplot(aes(x= date, y= -Depth))+   #setup plot with x and y data
  geom_line()

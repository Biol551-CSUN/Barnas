

# created: 3-10-2020 by Danielle Barnas
# modified: 9-29-2020

### BEFORE RUNNING SCRIPT
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

rm(list=ls())

library(tidyverse)
library(lubridate)
library(here)

source(here("Final_Project","Scripts","Define_Functions.R"))


########################
# File Names
########################

Serial<-'876'

# Date of in situ logs
Launch<-'2021-01-18 09:00:00' # Maintain date time format "YYYY-MM-DD HH:MM:SS"
Retrieval<-'2021-01-18 13:20:00' # Maintain date time format "YYYY-MM-DD HH:MM:SS"


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

### Read in Logger Files
path.Log<-here("Final_Project","Data")

depthLog<-WL_cleanup(path = path.Log, wl_serial = Serial)

# Parse date filters into date and type vector types
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
depthLog<-depthLog%>%filter(between(date,Launch,Retrieval)) 

# write file
write_csv(depthLog,paste0(here("Final_Project","Output"),'/depth_',Serial,'_',Sys.Date(),'.csv'))


# Plot the data
data.pres %>% # this is the dataframe
  ggplot(aes(x= date, y= TempInSitu))+   #setup plot with x and y data
  geom_line()
data.pres %>% # this is the dataframe
  ggplot(aes(x= date, y= -Depth))+   #setup plot with x and y data
  geom_line()

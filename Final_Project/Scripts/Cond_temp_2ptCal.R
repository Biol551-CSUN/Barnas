##########################################################################
##########################################################################
#### Two-point Conductivity Calibration script for HOBO Conductivity-Temperature logger data
#### Brings in raw .csv files by Serial number and exports a tidy file

#### Reference: https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf
#### Reference: https://www.aqion.de/site/112

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
library(gsw)
library(mooreasgd)
library(here)

rm(list=ls())

###################################
### Serial, Date, and File Paths
###################################

# CT Probe Serial Number
Serial<-'324'
# Log date
log.date<- '2021-01-18'
# Path to folder storing logger .csv files
path.cal<-here("Final_Project","Data") # Calibration file path
path.log<-here("Final_Project","Data") # Logged in situ file path
 
###################################
### Launch and Retrieval Times
###################################

# "YYYY-MM-DD HH:MM:SS"

# HIGH CALIBRATION POINT
# Pre-deployment calibration
startHigh1<-'2021-01-18 06:23:00'
endHigh1<-'2021-01-18 06:29:00'

# Post-deployment calibration
startHigh2<-'2021-01-20 13:03:00'
endHigh2<-'2021-01-20 13:07:00'

# LOW CALIBRATION POINT
# Pre-deployment calibration
startLow1<-'2021-01-18 06:32:00'
endLow1<-'2021-01-18 06:40:00'

# Post-deployment calibration
startLow2<-'2021-01-20 13:23:00'
endLow2<-'2021-01-20 13:32:00'

# Common Garden
CGstart<-'2021-01-18 07:59:00'
CGend<-'2021-01-18 08:05:00'

# Date of in situ logs
Launch<-'2021-01-18 11:30:00'
Retrieval<-'2021-01-20 11:00:00'


###################################
### Conductivity Calibration Standards and Logging Interval
###################################

# Two-Point Calibration Standards
# High Value
refHigh<-40900 # uS/cm at 25deg C
# Low Value
refLow<-1413 # uS/cm at 25deg C

# In Situ Recording Interval
int<-10 #seconds

###################################
### Pressure data
###################################

# COMMENT OUT ONE OF THE FOLLOWING

### If pairing with Pressure/Depth Logger data
# (NOTE: Water Level data must first be calibrated and processed through HOBOware)
Serial.depth<-'877' # Serial number of paired hobo pressure logger
path.depth<-here("Final_Project","Data") # Water Level file path 


### If data were recorded at a consistent pressure (bar)
#Pres_bar<-1


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
### Read in Calibration and Logger Files
############################################################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number

# Conductivity Calibration files
condCal<-CT_cleanup(path = path.cal, ct_serial = Serial)

# In Situ Conductivity files
condLog<-CT_cleanup(path = path.log, ct_serial = Serial)

############################################################
### Parse date and time
############################################################

# Parse launch and retrieval dates into date and type vector types
# High Point Calibrations
startHigh1<-startHigh1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endHigh1<-endHigh1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startHigh2<-startHigh2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endHigh2<-endHigh2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
# Low Point Calibrations
startLow1<-startLow1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endLow1<-endLow1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startLow2<-startLow2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endLow2<-endLow2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
# Common Garden
CGstart<-CGstart%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGend<-CGend%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
# Logs in situ
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

############################################################
### Filter by date and time
############################################################

# High Point Calibration
highCal<-condCal%>%filter(between(date,startHigh1,endHigh1)|between(date,startHigh2,endHigh2))
# Low Point Calibration
lowCal<-condCal%>%filter(between(date,startLow1,endLow1)|between(date,startLow2,endLow2))
# Common Garden and Logs in situ
condLog<-condLog%>%filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval)) 

# Join Calibration and Logged files
condCal<-union(lowCal,highCal)
CT.data<-union(condCal,condLog)

# Offset time if necessary
#CT.data$date<-CT.data$date - seconds(5) 

############################################################
### Load Pressure Data from HOBO Water Level Loggers
############################################################

# Required for In Situ Practical Salinity Calculations
if(exists('Serial.depth')) {
  data.pres<-WL_cleanup(path = path.depth, wl_serial = Serial.depth) # reads and tidies data
  data.pres<-data.pres%>%
    dplyr::filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>% 
    dplyr::rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    dplyr::mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  data.pres<-tibble::tibble(date=CT.data$date, AbsPressure_bar=Pres_bar) # creates a dataframe with Pressure column = 0
}

# Join Pressure data to CT dataframe
CT.data<-CT.data%>%
  left_join(data.pres,by='date')

# Add calibration Abs Pressure = 1 bar
condCal<-CT.data%>%
  filter(between(date,startHigh1,endHigh1)|between(date,startHigh2,endHigh2)|
         between(date,startLow1,endLow1)|between(date,startLow2,endLow2)) %>% 
  mutate(AbsPressure_bar = 1)
condLog<-CT.data %>% 
  filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))

# Join all dataframes with Abs Pressure
CT.data<-full_join(condCal,condLog)

############################################################
### Nonlinear Temperature Compensation
############################################################

# source: https://www.aqion.de/site/112
CT.data<-CT.data %>%
  mutate(A = (1.37023 * (TempInSitu - 20)) + 8.36 * (10^(-4) * ((TempInSitu - 20)^2))) %>%
  mutate(B = 109 + TempInSitu) %>%
  mutate(Sp_Conductance = 0.889 * (10^(A/B)) * E_Conductivity)

# Calculate temperature correction factor (f25) using ISO-7888 (1985)
# source: https://cdn.standards.iteh.ai/samples/14838/ffffe623f2654f4ea96ea3ceb22de16f/ISO-7888-1985.pdf

# # Constants
# a<-as.numeric('0.962144')
# n<-as.numeric('0.965078')
# A<-as.numeric('-0.198058')
# B<-as.numeric('-1.992186')
# C<-as.numeric('231.17628')
# D<-as.numeric('86.39123')
# 
# # Calculate viscosity and f25 (temperature compensation factor)
# # then calculate temperature compensated conductivity (Specific Conductance) through equation: SC = EC * f25
# CT.data<-CT.data%>%
#   mutate(vis = A + exp(B + (C / (TempInSitu + D))))%>%
#   mutate(f25 = ((1 - a) + a * ((vis)^n)) * 1.116)%>%
#   mutate(Sp_Conductance = E_Conductivity * f25)

############################################################
### Two Point Calibration 
############################################################

# Logger data in low calibration
rawLow<-CT.data%>%
  filter(between(date,startLow1,endLow1))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric
# Logger data in high calibration
rawHigh<-CT.data%>%
  filter(between(date,startHigh1,endHigh1))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()

# Calibration
rawRange<-rawHigh - rawLow
refRange<-refHigh - refLow

CT.data<-CT.data %>%
  mutate(Sp_Conductance_cal = (((Sp_Conductance - rawLow) * refRange) / rawRange) + refLow)


############################################################
### Calculate drift between pre- and post-deployment calibrations
### Only required for long deployments
############################################################

# # Average logger data in first high calibration
# meanHigh1<-CT.data%>%
#   filter(between(date,startHigh1,endHigh1))%>%
#   summarise(mean(Sp_Conductance_cal))%>%
#   as.numeric()
# # Average logger data in second high calibration
# meanHigh2<-CT.data%>%
#   filter(between(date,startHigh2,endHigh2))%>%
#   summarise(mean(Sp_Conductance_cal))%>%
#   as.numeric()
# 
# # Average logger data in first low calibration
# meanLow1<-CT.data%>%
#   filter(between(date,startLow1,endLow1))%>%
#   summarise(mean(Sp_Conductance_cal))%>%
#   as.numeric()
# # Average logger data in second low calibration
# meanLow2<-CT.data%>%
#   filter(between(date,startLow2,endLow2))%>%
#   summarise(mean(Sp_Conductance_cal))%>%
#   as.numeric()
# 
# # Drift between high calibration readings
# drift.off.High<-meanHigh1-meanHigh2
# # Drift between low calibration readings
# drift.off.Low<-meanLow1-meanLow2
# 
# # Drift = high cal drift
# #drift.off<-min(c(drift.off.High,drift.off.Low))
# #drift.off<-sum(drift.off.High,drift.off.Low)/2
# drift.off<-drift.off.High
# 
# drift.corr=drift.off/length(condLog$date)

############################################################
### Apply drift calculation
### Only required for long deployments
############################################################

# condLog<-CT.data%>%
#   filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
#   arrange(date)%>%
#   mutate(drift.correction.new=drift.corr)%>% # establish a column filled with the drift correction value
#   mutate(drift.correction=cumsum(drift.correction.new))%>% # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
#   select(-drift.correction.new)%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.correction)
# condCal1b<-CT.data%>%
#   filter(between(date,startHigh1,endHigh1)|between(date,startLow1,endLow1))%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal)
# condCal2b<-CT.data%>%
#   filter(between(date,startHigh2,endHigh2)|between(date,startLow2,endLow2))%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.off)
# condCal<-union(condCal1b,condCal2b) # Join calibration files together
# CT.data<-full_join(condCal,condLog) # Join Calibration and Logged files

############################################################
### Calculate Salinity using gsw package for the PSS-78 equation
############################################################

CT.data<-CT.data%>%
  mutate(SalinityInSitu_2pCal=gsw_SP_from_C(C = Sp_Conductance_cal*0.001, t = 25, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation


############################################################
### Write CSV file and graph data
############################################################

write_csv(CT.data,paste0(here("Final_Project","Output"),'/CT_',Serial,'_2pt_Cal_',log.date,'.csv'))

# Plotting

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_2pCal>24)%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal,color=TempInSitu))+
  geom_line()

## Temperature ~ Salinity
# CT.data %>% 
#   filter(between(date,Launch,Retrieval))%>%
#   ggplot(aes(y=SalinityInSitu_2pCal,x=TempInSitu))+
#   geom_point() + 
#   geom_smooth(method = "lm")

# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   ggplot(aes(x=date,y=Depth,color=SalinityInSitu_2pCal))+
#   geom_line()+
#   scale_colour_gradient(high = "#132B43",low = "#56B1F7")+
#   coord_flip()
# 
# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   ggplot(aes(x=date,y=Depth,color=TempInSitu))+
#   geom_line()+
#   coord_flip()
# 
# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   filter(SalinityInSitu_2pCal>24)%>%
#   ggplot(aes(x=date,y=TempInSitu,color=SalinityInSitu_2pCal))+
#   geom_line()+
#   scale_colour_gradient(high = "#132B43",low = "#56B1F7")
# 
# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   filter(SalinityInSitu_2pCal>24)%>%
#   ggplot(aes(x=date,y=SalinityInSitu_2pCal,color=TempInSitu))+
#   geom_line()
# 


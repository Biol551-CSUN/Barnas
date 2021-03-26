##########################################################################
##########################################################################
#### One-point Conductivity Calibration script for HOBO Conductivity-Temperature logger data
#### Brings in raw .csv files by Serial number and exports a tidy file

#### Reference: # https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# Author: Danielle Barnas
# created: 9-23-2020
# modified: 3-25-2021

##########################################################################
##########################################################################

###################################
### Load Libraries
###################################

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
Serial<-'354'
# Log date
log.date<- '2021-01-18'
# Path to folder storing logger .csv files
path.cal<-here("Final_Project","Data") # Calibration file path
path.log<-here("Final_Project","Data") # Logged in situ file path


###################################
### Launch and Retrieval Times
###################################

# "YYYY-MM-DD HH:MM:SS"

# Date of calibrations
startCal1<-'2021-01-18 06:24:50'
endCal1<-'2021-01-18 06:31:00'

# Date of in situ logs
Launch<-'2021-01-18 08:00:00'
Retrieval<-'2021-01-18 11:30:00'

###################################
### Conductivity Calibration Standards and Logging Interval
###################################

# One-Point Calibration Standard
oneCal<-50000 # uS/cm at 25deg C

# In Situ Recording Interval
int<-10 #seconds

###################################
### Pressure data
###################################

# COMMENT OUT ONE OF THE FOLLOWING

### If pairing with Pressure/Depth Logger data
# (NOTE: Water Level data must first be calibrated and processed through HOBOware)
Serial.depth<-'876' # Serial number of paired hobo pressure logger
path.depth<-here("Final_Project","Data") # Water Level file path 


### If data were recorded at a consistent pressure (bar)
#Pres_bar<-0


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
# Calibration
startCal1<-startCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal1<-endCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
# Logs in situ
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)


############################################################
### Filter by date and time
############################################################

# Calibration
condCal<-condCal%>%filter(between(date,startCal1,endCal1)) 
# Logs in situ
condLog<-condLog%>%filter(between(date,Launch,Retrieval)) 

# Join Calibration and Logged files
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
    dplyr::filter(between(date,Launch,Retrieval))%>%
    dplyr::rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    dplyr::mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  data.pres<-tibble::tibble(date=CT.data$date, AbsPressure_bar=Pres_bar) # creates a dataframe with Pressure column = 0
}

# Join Pressure data to CT dataframe
CT.data<-CT.data%>%
  left_join(data.pres,by='date')

############################################################
### One Point Calibration
############################################################

# Slope for 50000 uS/cm
Cond_Reference<-1060*mean(condCal$TempInSitu)+23500
Cal_Measure<-mean(condCal$E_Conductivity)
offset<-Cond_Reference-Cal_Measure

CT.data<-CT.data%>%
  mutate(Sp_Conductance=E_Conductivity+offset)

CT.data<-CT.data%>%
  mutate(Sp_Cond_mS.cm=Sp_Conductance*0.001)%>% # Sp_Conductance in mS/cm
  mutate(SalinityInSitu_1pCal=gsw_SP_from_C(C = Sp_Cond_mS.cm, t = TempInSitu, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation

############################################################
### Write CSV file and graph data
############################################################

write_csv(CT.data,paste0(here("Final_Project","Output"),'/CT_',Serial,'_1pt_',log.date,'.csv'))

# Plotting
CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>25)%>% # to ignore outliers
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()


#### IGNORE BELOW - DO NOT RUN


## INSTRUMENT DRIFT COMPENSATION 

## account for instrument drift over time
## reference: https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# each correction is based on linear drift over time
# data taken closer to initial calibration are corrected less than data taken toward the end of the monitoring period
# ft = correction factor
# t = the time interval for each data point that has passed since deployment
# total.t = total deployment time
# ft<-(t / total.t)

# one-point calibrations
# C = drift-corrected water quality parameter value
# m = uncorrected value
# si = value of the calibration standard
# sf = the value read by the instrument for the calibration standard after the total deployment time (total.t)
# C1<-m+ft*(si-sf)


############################################################
############################################################
# # One Point Calibration with Drift
# 
# # Drift correction factor
# 
# # each correction is based on linear drift over time
# # data taken closer to initial calibration are corrected less than data taken toward the end of the monitoring period
# t<-int #seconds  # t = the time interval for each data point that has passed since deployment
# total.t<-as.numeric(as.duration(Retrieval-Launch),"seconds") #seconds # total.t = total deployment time
# ft<-(t / total.t) # ft = correction factor
# 
# # one-point calibrations
# # C1<-m+ft*(si-sf)
# # C = drift-corrected water quality parameter value
# # Sp_Conductance = m = uncorrected value (mS/cm)
# si<-oneCal # the calibration standard
# sf<-CT.data%>% # the value read by the instrument in the calibration standard after the total deployment time (total.t)
#   filter(between(date,startCal1,endCal1))%>%
#   summarise(Middle=mean(Sp_Conductance)) # use avg calibration reading to avoid skew by placing logger in or taking logger out of calibration solution
# sf<-as.numeric(sf[1,])
# 
# CT.data<-CT.data%>%
#   mutate(Sp_Cond_mS.cm=Sp_Conductance*0.001+ft*(si-sf))%>% # Sp_Conductance in mS/cm
#   mutate(SalinityInSitu_1pCal=gsw_SP_from_C(C = Sp_Cond_mS.cm, t = TempInSitu, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation
# 
# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   #  filter(SalinityInSitu_1pCal>25)%>% # to ignore outliers
#   ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
#   geom_line()
# 
# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   ggplot(aes(x=date,y=TempInSitu))+
#   geom_line()
# 

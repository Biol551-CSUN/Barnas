##########################################################################
##########################################################################
#### Two-point Conductivity Calibration script for HOBO Conductivity-Temperature logger data
#### Brings in raw .csv files by Serial number and exports a tidy file

#### Reference: # https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

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

# common garden
CGstart<-'2021-01-18 07:59:00'
CGend<-'2021-01-18 08:05:00'

# Date of in situ logs
Launch<-'2021-01-18 11:30:00'
Retrieval<-'2021-01-20 11:00:00'


###################################
### Conductivity Calibration Standards and Logging Interval
###################################

# Two-Point Calibration Standards
# Low Value
refLow<-1413 # uS/cm at 25deg C
# High Value
refHigh<-50000 # uS/cm at 25deg C

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
############################################################
### Read in and Calibration and Logger Files

# Conductivity Calibration files
#path.Cal<-'Data/Cond_temp/Calibration'
#file.names.Cal<-basename(list.files(path.Cal, pattern = c(cal.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
#condCal <- file.names.Cal %>%
#  map_dfr(~ read_csv(file.path(path.Cal, .),skip=1,col_names=TRUE))
#condCal<-condCal%>% # Filter specified probe by Serial number
#  select(contains('Date'),contains(Serial))%>%
#  mutate(Serial=Serial)%>%
#  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
#  drop_na()
#condCal$date<-condCal$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# In Situ Conductivity files
path.Log<-paste0('Data/Cond_temp')
file.names.Log<-basename(list.files(path.Log, pattern = c(file.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
condLog <- file.names.Log %>%
  map_dfr(~ read_csv(file.path(path.Log, .),skip=1,col_names=TRUE))
condLog<-condLog%>% # Filter specified probe by Serial number
  select(contains('Date'),contains(serial),-contains("Low"))%>%
  mutate(Serial=serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
  drop_na()
condLog$date<-condLog$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# Parse date filters into date and type vector types
startHigh1<-startHigh1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endHigh1<-endHigh1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startHigh2<-startHigh2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endHigh2<-endHigh2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startLow1<-startLow1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endLow1<-endLow1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startLow2<-startLow2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endLow2<-endLow2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGstart<-CGstart%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGend<-CGend%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
lowCal<-condCal%>%filter(between(date,startLow1,endLow1)|between(date,startLow2,endLow2))
highCal<-condCal%>%filter(between(date,startHigh1,endHigh1)|between(date,startHigh2,endHigh2))
condLog<-condLog%>%filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval)) 
condCal<-union(lowCal,highCal) # Join calibration files together
CT.data<-union(condCal,condLog)# Join Calibration and Logged files
#CT.data$date<-CT.data$date - seconds(5) # offset time if necessary

############################################################
# Load Pressure Data from HOBO Pressure Loggers for In Situ Practical Salinity Calculations

if(exists('Serial.depth')) {
  path.depth<-'Data/Depth/Calibrated_files'
  file.names.depth<-basename(list.files(path.depth,pattern = c(Serial.depth, file.date,"csv$"), recursive = F))
  Depth.data <- file.names.depth %>%
    purrr::map_dfr(~ read_csv(file.path(path.depth, .),col_names=TRUE))
  Depth.data<-Depth.data%>%
    dplyr::filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
    dplyr::rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    dplyr::mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  Depth.data<-tibble::tibble(date=CT.data$date, AbsPressure_bar=Pres_bar)
}
CT.data<-CT.data%>% # amend to larger dataframe
  left_join(Depth.data,by='date')

############################################################
# Nonlinear Temperature Compensation

# Calculate temperature correction factor (f25) using ISO-7888 (1985)
# source: https://cdn.standards.iteh.ai/samples/14838/ffffe623f2654f4ea96ea3ceb22de16f/ISO-7888-1985.pdf

# Constants
a<-as.numeric('0.962144')
n<-as.numeric('0.965078')
A<-as.numeric('-0.198058')
B<-as.numeric('-1.992186')
C<-as.numeric('231.17628')
D<-as.numeric('86.39123')

# Calculate viscosity and f25 (temperature compensation factor)
# then calculate temperature compensated conductivity (Specific Conductance) through equation: SC = EC * f25
CT.data<-CT.data%>%
  mutate(vis = A + exp(B + (C / (TempInSitu + D))))%>%
  mutate(f25 = ((1 - a) + a * ((vis)^n)) * 1.116)%>%
  mutate(Sp_Conductance = E_Conductivity * f25)

############################################################
# Two Point Calibration 

rawLow<-CT.data%>%
  filter(between(date,startLow2,endLow2))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric
rawHigh<-CT.data%>%
  filter(between(date,startHigh2,endHigh2))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()

rawRange<-rawHigh - rawLow
refRange<-refHigh - refLow

CT.data<-CT.data %>%
  mutate(Sp_Conductance_cal = (((Sp_Conductance - rawLow) * refRange) / rawRange) + refLow)

############################################################
## Correct for drift between pre- and post-deployment calibrations

meanHigh1<-CT.data%>%
  filter(between(date,startHigh1,endHigh1))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()
meanHigh2<-CT.data%>%
  filter(between(date,startHigh2,endHigh2))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()

meanLow1<-CT.data%>%
  filter(between(date,startLow1,endLow1))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()
meanLow2<-CT.data%>%
  filter(between(date,startLow2,endLow2))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()

drift.off.High<-meanHigh1-meanHigh2
drift.off.Low<-meanLow1-meanLow2
drift.off<-min(c(drift.off.High,drift.off.Low))
#drift.off<-sum(drift.off.High,drift.off.Low)/2
drift.corr=drift.off/length(condLog$date)

CGpres<-CT.data%>%
  filter(between(date,CGstart,CGend))%>%
  mutate(AbsPressure_bar=0)
condLog<-CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  union(CGpres)%>%
  arrange(date)%>%
  mutate(drift.correction.new=drift.corr)%>% # establish a column filled with the drift correction value
  mutate(drift.correction=cumsum(drift.correction.new))%>% # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
  select(-drift.correction.new)%>%
  mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.correction)
condCal1b<-CT.data%>%
  filter(between(date,startHigh1,endHigh1)|between(date,startLow1,endLow1))%>%
  mutate(Sp_Conductance.calDrift = Sp_Conductance_cal)
condCal2b<-CT.data%>%
  filter(between(date,startHigh2,endHigh2)|between(date,startLow2,endLow2))%>%
  mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.off)
condCal<-union(condCal1b,condCal2b)%>%
  mutate(AbsPressure_bar = 0) # Join calibration files together
CT.data<-full_join(condCal,condLog) # Join Calibration and Logged files

############################################################
# Calculate Salinity using gsw package for the PSS-78 equation

CT.data<-CT.data%>%
  mutate(SalinityInSitu_1pCal=gsw_SP_from_C(C = Sp_Conductance.calDrift*0.001, t = 25, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation

############################################################
# Graph data

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>24)%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal,color=TempInSitu))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=-Depth,color=SalinityInSitu_1pCal))+
  geom_line()+
  scale_colour_gradient(high = "#132B43",low = "#56B1F7")

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=-Depth,color=TempInSitu))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>24)%>%
  ggplot(aes(x=date,y=TempInSitu,color=SalinityInSitu_1pCal))+
  geom_line()+
  scale_colour_gradient(high = "#132B43",low = "#56B1F7")

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>24)%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal,color=TempInSitu))+
  geom_line()+
  ggsave(paste0('Output/CT_Cal/CT_',serial,'_',file.date,'endCalJan21.png'),height = 10,width = 13)

# Write CSV file
write_csv(CT.data,paste0('Data/Cond_temp/Calibrated_files/',file.date,'_CT',serial,'_insitu_2pcal_sameDayCal.csv'))



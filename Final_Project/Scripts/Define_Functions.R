### Functions for cleaning and processing HOBO logger data from raw csv exported from HOBOware
### Created by Danielle Barnas
### Created on 2021-March-8

################################################################
################################################################

### Create a function for cleaning CT HOBO logger data 
### from a raw csv exported from HOBOware
### Created by Danielle Barnas
### Created on 2021-March-8

# Create CT Cleaning Function
CT_cleanup <- function(path, ct_serial, recursive_tf = FALSE) {

file.names.Cal<-basename(list.files(path, pattern = c(ct_serial,"csv$", recursive = recursive_tf))) #list all csv file names in the folder and subfolders

condCal <- file.names.Cal %>%
  purrr::map_dfr(~ readr::read_csv(file.path(path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata

condCal<-condCal %>% 
  dplyr::select(contains('Date'), contains(ct_serial), contains("High Range"), contains("Temp")) %>% # Filter specified probe by Serial number
  dplyr::mutate(Serial=paste0("CT_",ct_serial)) %>% # add column for CT serial number
  dplyr::rename(date=contains("Date"),
         TempInSitu=contains("Temp"),
         E_Conductivity=contains("High Range")) %>% 
  tidyr::drop_na()

condCal$date <- condCal$date %>% 
  readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
                 na = character(), 
                 locale = default_locale(), 
                 trim_ws = TRUE) 

 return(condCal)
}

################################################################
################################################################

### Create a function for cleaning multiple HOBO CT logger data files
### from a raw csv exported from HOBOware
### Created by Danielle Barnas
### Created on 2021-March-26

CT_roundup<-function(data.path, output.path, ct.serial, tf_write, tf_recursive = FALSE){
  
  # Create a list of all files within the directory folder
  file.names.Cal<-basename(list.files(data.path, pattern = c("csv$", recursive = tf_recursive))) #list all csv file names in the folder and subfolders
  
  # Create an empty dataframe to store all subsequent tidied df's into
  full_df <- tibble::tibble(
    date = as.POSIXct(NA),
    List.ID = as.character(),
    TempInSitu = as.numeric(),
    E_Conductivity = as.numeric(),
    Sp_Conductance = as.numeric())
  
  # For each listed file:
  ## Load the dataframe,
  ## Tidy the data,
  ## Create a column for temperature-compensated specific conductance, and
  ## Export the new file to an output folder
  for(i in 1:length(file.names.Cal)) {
    Data_ID<-file.names.Cal[[i]]
    
    file.names<-basename(list.files(data.path, pattern = c(Data_ID, "csv$", recursive = tf_recursive))) #list all csv file names in the folder and subfolders
    
    condCal <- file.names %>%
      purrr::map_dfr(~ readr::read_csv(file.path(data.path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata
    
    condCal<-condCal %>%
      dplyr::select(contains('Date'), contains("High Range"), contains("Temp")) %>%
      dplyr::mutate(List.ID=Data_ID) %>%  # add column for file ID
      dplyr::rename(date=contains("Date"),
                    TempInSitu=contains("Temp"),
                    E_Conductivity=contains("High Range")) %>%
      tidyr::drop_na() %>%
      tidyr::separate(col = 'List.ID', into = c('List.ID',NA), sep = ".csv", remove = T) # remove the '.csv'
    
    # Format date and time
    condCal$date <- condCal$date %>%
      readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
                            na = character(),
                            locale = default_locale(),
                            trim_ws = TRUE)
    
    ############################################################
    ### Nonlinear Temperature Compensation
    ############################################################
    
    # https://www.aqion.de/site/112
    condCal<-condCal %>%
      dplyr::mutate(A = (1.37023 * (TempInSitu - 20)) + 8.36 * (10^(-4) * ((TempInSitu - 20)^2))) %>%
      dplyr::mutate(B = 109 + TempInSitu) %>%
      dplyr::mutate(Sp_Conductance = 0.889 * (10^(A/B)) * E_Conductivity) %>%
      dplyr::select(-c(A,B))
    
    full_df <- full_df %>%
      dplyr::full_join(condCal) # save your dataframes into a larger df
    
    if(tf_write == TRUE) {
      write.csv(condCal, paste0(output.path,'/',Data_ID,'_SpConductance.csv'))
    }
  }
  
  if(exists('ct.serial')) {
    pattern <- grep(x = full_df$List.ID, pattern = ct.serial, value = TRUE)
    full_df <- full_df %>%
      dplyr::filter(List.ID == pattern[2])
  }
  
  return(full_df) # return a list of dataframes
}


################################################################
################################################################

### Create a function for cleaning Water Level/Pressure/Depth HOBO logger data
### from a calibrated csv exported from HOBOware
### Created by Danielle Barnas
### Created on 2021-March-8

# Create Water Level Cleaning Function
WL_cleanup <- function(path, wl_serial, recursive_tf = FALSE) {
  
  file.names.Cal<-basename(list.files(path, pattern = c(wl_serial,"csv$", recursive = recursive_tf))) #list all csv file names in the folder and subfolders
  
  depthLog <- file.names.Cal %>%
    purrr::map_dfr(~ readr::read_csv(file.path(path, .), skip=1, col_names=T)) # read all csv files at the file path, skipping 1 line of metadata
  
  depthLog<-depthLog %>% 
    dplyr::select(contains('Date'), contains(wl_serial), contains("Temp"),contains("Abs Pres"), contains("Water Level")) %>% # Filter specified probe by Serial number
    dplyr::mutate(Serial=paste0("WL_",wl_serial)) %>% # add column for CT serial number
    dplyr::rename(date=contains("Date"),
           TempInSitu=contains("Temp"),
           AbsPressure=contains("Abs Pres"),
           Depth=contains("Water Level")) %>% 
    tidyr::drop_na()
  
  depthLog$date <- depthLog$date %>% 
    readr::parse_datetime(format = "%m/%d/%y %H:%M:%S %p", # Convert 'date' to date and time vector type
                   na = character(), 
                   locale = default_locale(), 
                   trim_ws = TRUE) 
  
  return(depthLog)
}


### Create a function for cleaning pH HOBO logger data from a raw csv exported 
### from HOBOmobile (mobile app)
### Created by Danielle Barnas
### Created on 2021-March-8

# Create pH Cleaning Function
pH_cleanup <- function(path, pH_serial, recursive_tf = FALSE) {
  
  # list all csv file names in the folder and subfolders
  file.names.Cal<-basename(list.files(path, pattern = c(pH_serial,"csv$", recursive = recursive_tf)))
  
  # read all csv files at the file path, skipping 1 line of metadata
  pHLog <- file.names.Cal %>%
    purrr::map_dfr(~ readr:::read_csv(file.path(path, .), skip=2, col_names=T))
  
  # clean file: only select useful columns, create serial# column, rename columns
  pHLog<-pHLog %>% 
    dplyr::select(contains('Date'), contains(Serial), contains("temp"), mV, pH) %>%
    dplyr::mutate(Serial=paste0("pH_",Serial)) %>%
    dplyr::rename(date=contains("Date"),
           TempInSitu=contains("Temp")) %>% 
    tidyr::drop_na()
  
  return(pHLog)
}


### Create a function for a one-point conductivity calibration
### Created by Danielle Barnas
### Created on 2021-March-26

one_cal<-function(data, date, temp, EC, cal_reference, startCal, endCal) {
  
  ############################################################
  ### One Point Calibration
  ############################################################
  
  # Logger data in pre-deployment calibration
  Cal.Log<-data%>%
    filter(between({{date}},{{startCal}},{{endCal}}))%>%
    summarise(mean(Sp_Conductance))%>%
    as.numeric
  
  # Offset between the calibration reference and the logger reading
  offset<-cal_reference-Cal.Log
  
  # Apply offset to logger data
  data<-data%>%
    mutate(Sp_Conductance_cal=Sp_Conductance+offset)

  return(data)
  }



### Create a function for a two-point conductivity calibration
### Created by Danielle Barnas
### Created on 2021-March-26




### Create a function for a logger drift
### Created by Danielle Barnas
### Created on 2021-March-26


############################################################
### Apply drift calculation
### Only required for long deployments
############################################################

# # Logger data in pre-deployment calibration
# preCal<-CT.data%>%
#   filter(between(date,startCal1,endCal1))%>%
#   summarise(mean(Sp_Conductance))%>%
#   as.numeric
# # Logger data in post-deployment calibration
# postCal<-CT.data%>%
#   filter(between(date,startCal2,endCal2))%>%
#   summarise(mean(Sp_Conductance))%>%
#   as.numeric()

# # Drift between high calibration readings
# drift.off<-preCal-postCal
# # Drift correction factor
# drift.corr=drift.off/length(condLog$date)
# 
# condLog<-CT.data%>%
#   filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
#   arrange(date)%>%
#   mutate(drift.correction.new=drift.corr)%>% # establish a column filled with the drift correction value
#   mutate(drift.correction=cumsum(drift.correction.new))%>% # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
#   select(-drift.correction.new)%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.correction)
# condCal1b<-CT.data%>%
#   filter(between(date,startCal1,endCal1))%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal)
# condCal2b<-CT.data%>%
#   filter(between(date,startCal2,endCal2))%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.off)
# condCal<-union(condCal1b,condCal2b) # Join calibration files together
# CT.data<-full_join(condCal,condLog) # Join Calibration and Logged files













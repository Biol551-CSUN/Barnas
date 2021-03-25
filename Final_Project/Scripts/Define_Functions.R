### Functions for cleaning and processing HOBO logger data from raw csv exported from HOBOware
### Created by Danielle Barnas
### Created on 2021-March-8

################################################################
################################################################

### Create a function for cleaning CT HOBO logger data from a raw csv exported from HOBOware
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

### Create a function for cleaning Water Level/Pressure/Depth HOBO logger data from a calibrated csv exported from HOBOware
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


### Create a function for cleaning pH HOBO logger data from a raw csv exported from HOBOmobile (mobile app)
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



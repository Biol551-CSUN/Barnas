### For Loops Lab
### Week 13, Monday 4/26/2021
### Group1

##Libraries
library(tidyverse)
library(here)

## Standard for loop

file.path <- here("Week13","Data","homework") # file path
files <- dir(path = file.path,pattern = ".csv") # find files in path; one file per tidepool
files

# create empty matrix for storing data
pool.data <- data.frame(matrix(nrow = length(files), ncol = 6))
colnames(pool.data) <- c("filename", "tide_pool_id", "mean_temp", "sd_temp","mean_lux", "sd_lux")

for(i in 1:length(files)){ # 1:4
  mydata <- read_csv(paste0(file.path,"/",files[i]))
  pool.data$filename[i] <- files[i]
  pool.data$tide_pool_id[i] <- paste0("TP_",i)
  pool.data$mean_temp[i] <- mean(mydata$Temp.C, na.rm = TRUE)
  pool.data$sd_temp[i] <- sd(mydata$Temp.C, na.rm = TRUE)
  pool.data$mean_lux[i] <- mean(mydata$Intensity.lux, na.rm = TRUE)
  pool.data$sd_lux[i] <- sd(mydata$Intensity.lux, na.rm = TRUE)
}
pool.data


## Using purr package

file.path <- here("Week13","Data","homework") # file path
files <- dir(path = file.path,pattern = ".csv", full.names = T) # find files in path; one file per tidepool

data<-files %>%
  set_names() %>% # sets the id of each list to the file name
  map_df(read_csv, .id = "filename") %>% # map everything to a dataframe and put the id (from set_names()) in a column called filename
  group_by(filename) %>% # effectively groups by tide pool
  summarise(mean_temp = mean(Temp.C, na.rm = TRUE),
            sd_temp = mean(Temp.C,na.rm = TRUE),
            mean_lux = mean(Intensity.lux, na.rm = TRUE),
            sd_lux = mean(Intensity.lux, na.rm = TRUE))
data

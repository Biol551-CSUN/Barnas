## Week 2 Sample Script for importing data
## Created by Danielle Barnas
## Created on 2021-02-03
################################################

## Load Libraries ##
library(tidyverse)
library(here)


## Read in Data ##
weightdata<-read_csv(here("Week2","Data","weightdata.csv"))


## Data Analysis ##
head(weightdata)
tail(weightdata)
View(weightdata)

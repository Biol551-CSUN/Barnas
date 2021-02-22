### types of joins
### Created by Danielle Barnas
### Created on 2021-02-22
####################################

# gif count: 1
rm(list=ls())

### Load Libraries ###
library(tidyverse)
library(here)


### Bring in Data ###

envi <- read_csv(here("Week5","Data","site.characteristics.data.csv"))
tpc <- read_csv(here("Week5","Data","topt_data.csv"))

View(envi)
View(tpc)

glimpse(envi)
glimpse(tpc)

### Data Analysis ###

# pivot wider environmental data
envi_wide <- envi %>%
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>%
  arrange(site.letter) # arrange in dataframe by site in descending order
View(envi_wide)

## Left Join ##

# left_join brings together two data frame based on the left (x) data frame
# left_join(x, y)
# if either data set was missing data, the join would use the max data from the x dataframe

fulldata_left <- left_join(tpc,envi_wide) # joins by = "site.letter"

head(fulldata_left)

## Relocate ##
# relocate columns in dataframe

# where() asks a question: where are all the numerics, or characters, or factors etc.
# .after tells which columns to put after the first set of columns
# .before tells which columns to put before the first set of columns
# can relocate two or more column types

fulldata_left <- left_join(tpc,envi_wide) %>%
  relocate(where(is.numeric), .after = where(is.character))


### Think, Pair, Share ###
fulldata_summary <- fulldata_left %>%
  pivot_longer(cols = E:substrate.cover, # pivot longer
               values_to = "values",
               names_to = "measurements") %>%
  group_by(site.letter, measurements) %>% # group by site and measurement
  summarise(meanVal=mean(values), varVal=var(values)) # get mean and variance

## alternative
fulldata_left %>%
  group_by(site.letter) %>%
  summarise_at(vars(E:substrate.cover), # choose variables to summarise
               list(mean=mean, var=var), na.rm =  T) # get the mean and variance and label columns with '_mean' and '_var'


### Create your own tibble ###
# tibble is a data frame but simpler
# create a tibble with tibble() function

T1 <- tibble(Site.ID = c("A", "B", "C", "D"), # creates site ID column called Site.ID with sites A, B, C, D
             Temperature = c(14.1, 16.7, 15.3, 12.8)) # creates a column called temperature with 4 data points
T1

T2 <- tibble(Site.ID = c("A", "B", "D", "E"), # creates site ID column for sites A, B, D, E
             pH = c(7.3, 7.8, 8.1, 7.9)) # creates column called pH with 4 data points

T2

### Comparing Join Functions ###
# left join
left_join(T1,T2) # excludes site E data

# right join
right_join(T1,T2) # excludes site C data

# inner join
# only keeps the rows that are present in both data sets
# order of columns within function does not matter
inner_join(T1,T2) # only includes sites A, B, D

# full join
# keeps all data and produces NAs for all missing data
# order of columns within function does not matter
full_join(T1,T2) # includes all sites A thru E

# semi join
# keeps all rows from the first data set where there are matching values in the second data set
# but only keeps the columns from the first data set
semi_join(T1,T2) # Temperature column for sites A, B, D
semi_join(T2,T1) # pH column for sites A, B, D

# anti join
# keeps only the data that is missing from either data set
# but only keeps the columns from the first data set
anti_join(T1,T2)
anti_join(T2,T1)

### Silly package of the day ### 
library(cowsay)
# https://cran.r-project.org/web/packages/cowsay/vignettes/cowsay_tutorial.html

say("Hello, world!", "shark") # this is amazing

say("R you must practice", "yoda")

say("'Cause I'm a lady, that's why", "cat")













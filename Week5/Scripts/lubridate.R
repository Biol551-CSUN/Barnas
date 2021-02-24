### Lubridate package
### Created by Danielle Barnas
### Created on 2021-02-24
####################################

# gif count: 1
rm(list=ls())

### Load Libraries ###
library(tidyverse)
library(lubridate)
library(here)


### Using Lubridate ###
## now()
now()
# tells me what time and date it is right now
now(tzone="EST")
# can tell you time in other time zones
now(tzone="GMT")

## today()
today()
# only tells me the current date
today(tzone="GMT")
# can tell the date in other time zones

## AM or PM
am(now())
# asks T or F question: is it the AM right now?
pm(now())

leap_year(now())
# asks T or F question: are we in a leap year right now?


## Date specifications for lubridate ##
# Dates must be a CHARACTER type

## Converting from character to date in various formats
ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24, 2021") # with or without a comma
dmy("24/02/2021")

## Converting date_time character to date_time
ymd_hms("2021-02-24 10:22:20 PM") # better to use military time, but this will work
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24, 2021 10:22 PM")

## Can make a vector of dates ##
# character vector
datetimes <- c("02/24/2021 22:22:20", 
               "02/25/2021 11:21:10",
               "02/26/2021 08:01:52")
# convert to datetime
datetimes <-mdy_hms(datetimes)

# extract the month
month(datetimes) # numeric
month(datetimes, label = T) # abbreviated
month(datetimes, label = T, abbr = F) # full month name

# extract the day
day(datetimes)
wday(datetimes, label = T) # extract day of the week

# can extract any level of your datetime
hour(datetimes)
minute(datetimes)
second(datetimes)


## Adding dates and times
# hours() is used to convert a numeric to an 'hour' type
datetimes + hours(4) # adds four hours
datetimes + days(2) # adds two days
# also works with minutes() seconds() years() months()


## Rounding dates
round_date(datetimes, "minutes") # round to the nearest minute
round_date(datetimes, "5 mins") # round to the nearest 5 minutes
round_date(datetimes, "hour")


### Think, Pair, Share ###

## Bring in Data ##
cond<-read_csv(here("Week5","Data","CondData.csv"))
View(cond)

# use mutate to change date column to datetime type
cond <- cond %>%
  mutate(DateTime = ymd_hms(date))


## Today's awesome R package ##
library(CatterPlots)
x <- c(1:10) # make up some data
y <- c(1:10)
# creates plot with blue cats of type 5 (there are 11 types)
catplot(xs=x, ys=y, cat=5, catcolor='blue')











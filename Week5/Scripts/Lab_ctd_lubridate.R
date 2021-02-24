### Lubridate Lab
### Created by Danielle Barnas
### Created on 2021-02-24
####################################

rm(list=ls())

### Load Libraries ###
library(tidyverse)
library(lubridate)
library(here)
library(cowplot)

### Read in Data ###

cond <- read_csv("Week5/Data/CondData.csv")
cond <- cond %>%
  mutate(DateTime = ymd_hms(date)) %>%  # convert to datetime
  mutate(DateTime = DateTime - seconds(2)) %>%  # subtract 2 seconds to round to whole 10 seconds
  select(-date)

depth <- read_csv("Week5/Data/DepthData.csv")
depth <- depth %>% 
  mutate(DateTime = ymd_hms(date)) %>%  # convert to datetime
  select(-date)

# join dataframes, only keeping data from both dataframes
ctd <- inner_join(cond,depth) %>% # join dataframes by DateTime
  mutate(Hours = hour(DateTime)) %>% # new column extracting hour value
  mutate(Minutes = minute(DateTime)) %>% # new column extracting minute value
  group_by(Hours,Minutes) %>% # group data by hours and minutes
  summarise(DateTime = mean(DateTime, na.rm = T), # take average of data by minute
            Depth_mean = mean(Depth, na.rm = T),
            Temperature_mean = mean(TempInSitu, na.rm = T),
            Salinity_mean = mean(SalinityInSitu_1pCal, na.rm = T)) %>% 
  mutate(DateTime = round_date(DateTime, "10 seconds")) %>% # round data to the nearest 10 seconds
  relocate(where(is.POSIXct), .before = where(is.numeric)) %>%  # move datetime to first column
  unite(col = "hour_minute",
        c(Hours,Minutes),
        sep = ":",
        remove = T)

# save csv
write_csv(ctd, here("Week5","Output","ctd_summarized.csv"))

### Plotting a Stitched Profile ###

# create plot for salinity timeseries
sal<-ggplot(ctd, 
            aes(x = DateTime, y = Salinity_mean, colour = Temperature_mean)) +
  geom_line() + # line plot
  theme_bw() +
  scale_colour_gradient(high = "red3",
                        low = "blue") +
  labs(title = "CTD Timeseries",
       subtitle = "January 15, 2021",
       y = "Mean Salinity (psu)", # rename x axis
       x = "") + # remove x axis name (redundant with theme code below)
  theme(legend.position = "none", # remove legend
        axis.title.x=element_blank(), # remove x axis elements from top graph
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 12))
        
# create plot for temperature tieseries
temp<-ggplot(ctd, 
             aes(x = DateTime, y = Depth_mean, colour = Temperature_mean)) +
  geom_line() + # line plot
  scale_y_reverse() + # reverse axis scale for depth
  theme_bw() +
  scale_colour_gradient(high = "red3",
                        low = "blue") +
  labs(y = "Depth (m)",
       x = "Time",
       colour = "Temperature (C)") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 12))

# combine plots
# resource: https://wilkelab.org/cowplot/articles/plot_grid.html
plot_grid(sal, temp, # plots to combine
          labels = c('A', 'B'), 
          ncol = 1, # stacks plots
          label_size = 12, # size of A and B labels
          rel_heights = c(1,1.3), # adjust relative heights of plots
          align = "v") + # align the figures vertically (match x axes)
  ggsave(here("Week5","Output","CTD_plot.pdf"), height = 10, width = 15) # save plot



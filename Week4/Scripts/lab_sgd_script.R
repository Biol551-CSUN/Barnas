### Lab Assignment working with sgd biogeochemical data from Maunalua
### Created by Danielle Barnas
### Created on 2021-02-17
####################################

### Clear Environment ###
rm(list = ls())

### Load Libraries ###
library(tidyverse)
library(here)
library(PNWColors)


### Bring in Data ###
chemdata<-read_csv(here("Week4","Data","chemicaldata_maunalua.csv"))
View(chemdata)
glimpse(chemdata)


### Data Analysis - Part 1 ###
chemdata_tidy <- chemdata %>%
  filter(complete.cases(.)) %>% # filters out everything that is not a complete row
  separate(col = Tide_time, # column you want to separate
           into = c("Tide", "Time"), # name of the new columns
           sep = "_", # what you are separating by
           remove = T) %>% # delete the original column
  filter(Season == "SPRING") %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot
               names_to = "Variables", # the names of the new columns with all the column names
               values_to = "Values") %>% # the names of the new columns with all the values
  group_by(Variables, Site, Tide) %>% # group Variables by site and tide
  summarise(Value_mean = mean(Values, na.rm = T), # get mean
            Value_variance = var(Values, na.rm = T), # get variance
            Value_sd = sd(Values, na.rm = T), # get standard deviation
            Value_se = (Value_sd / sqrt(length(Values)))) %>% # get standard error
  write_csv(here("Week4","Output","lab_summary.csv")) # export csv

View(chemdata_tidy)


### Data Analysis - Part 2 ###
chemdata_clean <- chemdata %>%
  filter(complete.cases(.)) %>% # filters out everything that is not a complete row
  separate(col = Tide_time, # column you want to separate
           into = c("Tide", "Time"), # name of the new columns
           sep = "_", # separate columns by _
           remove = T) %>% # delete the original column
  rename('Nitrate+Nitrite (umol/L)' = "NN", # rename column headings for clarification when plotting
         'Phosphate (umol/L)' = "Phosphate",
         'Silicate (umol/L)' = "Silicate",
         'TA (umol/kg)' = "TA",
         'Percent SGD' = "percent_sgd",
         'Temperature (C)' = "Temp_in") %>%
  pivot_longer(cols = c('Temperature (C)', 'Phosphate (umol/L)':'Percent SGD'), # the cols you want to pivot
               names_to = "Variables", # the names of the new columns with all the column names
               values_to = "Values") %>%
  mutate(Site = ifelse(Site == "BP", "Black Point", "Wailupe")) # rename sites

### Data Visualization ###
pal<-pnw_palette("Starfish",2) # set color palette

ggplot(data = chemdata_clean,
       aes(x = Salinity,
           y = Values,
           color = Site)) + # group by site
  geom_line() + # line plot
  labs(y = "Biogeochemical Parameters") +
  theme_bw() + # black-white theme
  theme(legend.position = "right",
        axis.title = element_text(size = 15), # axis title text
        text = element_text(size = 15), # axis scale text
        legend.title = element_text(size = 15), # legend title text
        legend.text = element_text(size = 12)) + # legend text
  facet_wrap(~Variables, scales = "free") +
  scale_color_manual(values = pal) +
  ggsave(here("Week4","Output","sgd_plot.pdf"), width = 15, height = 15) # save pdf





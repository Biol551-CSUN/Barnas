### tidyr package
### Created by Danielle Barnas
### Created on 2021-02-17
####################################

# gif count: 1

### Load Libraries ###
library(tidyverse)
library(here)


### Bring in Data ###
chemdata<-read_csv(here("Week4","Data","chemicaldata_maunalua.csv"))
View(chemdata)
glimpse(chemdata)

### Data Analysis ###
chemdata_clean <- chemdata %>%
  filter(complete.cases(.)) %>% # filters out everything that is not a complete row
                                #sometimes the dot is needed to pull in the dataframe from the level above
  separate(col = Tide_time, # column you want to separate
           into = c("Tide", "Time"), # name of the new columns
           sep = "_", # what you are separating by
           remove = T) %>% # delete the original column
  unite(col = "Site_Zone", # the name of the new column
        c(Site,Zone), # the columns to unite
        sep = ".", # how you are uniting the column data. Put a . in the middle
        remove = F) # keep the original columns

### Pivot Longer ###
chemdata_long <- chemdata_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. his says select the temp to percent SGD cols
               names_to = "Variables", # the names of the new columns with all the column names
               values_to = "Values") # the names of the new columns with all the values
chemdata_long %>%
  group_by(Variables, Site) %>% # group by everything we want
  summarise(Param_means = mean(Values, na.rm = T), # get mean
            Param_vars = var(Values, na.rm = T)) # get variance

### Think, Pair, Share ###
# mean, variance, and sd for all variables by site, zone, and tide
chemdata_long %>%
  group_by(Variables, Site, Zone, Tide) %>% # group by everything we want
  summarise(Param_means = mean(Values, na.rm = T), # get mean
            Param_vars = var(Values, na.rm = T), # get variance
            Param_sd = sd(Values, na.rm = T), # get standard deviation
            Param_se = Param_sd / sqrt(length(Values)))

### Facet Wrap ###
chemdata_long %>%
  ggplot(aes(x = Site, y = Values)) + 
  geom_boxplot() + 
  facet_wrap(~Variables, scales = "free") # every plot has its own y scale

### Pivot Wider ###
chemdata_wide <- chemdata_long %>%
  pivot_wider(names_from = Variables, # column with the names for the new columns
              values_from = Values) # column with the values


### Notes ###
# Wide data: one observation per row and all the different variables are columns
  # easier to record or view data
# Long data: one unique measurement per row and all the info about that measurement in the same row
  # easier to summarize using group_by()
  # easier to facet_wrap when plotting
# Packages for beautiful tables (publication quality)
  # Nyssa will send through the Slack

### Clean Tidying ###

chemdata_clean <- chemdata %>%
  filter(complete.cases(.)) %>% # filters out everything that is not a complete row
  separate(col = Tide_time, # column you want to separate
           into = c("Tide", "Time"), # name of the new columns
           sep = "_", # what you are separating by
           remove = T) %>% # delete the original column
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. his says select the temp to percent SGD cols
               names_to = "Variables", # the names of the new columns with all the column names
               values_to = "Values") %>% # the names of the new columns with all the values
  group_by(Variables, Site, Time) %>% # group by everything we want
  summarise(Param_means = mean(Values, na.rm = T)) %>% # get mean
  pivot_wider(names_from = Variables, # pivot to wide format again
              values_from = Param_means) %>%
  write_csv(here("Week4","Output","summary.csv")) # export csv
            
View(chemdata_clean)
  
### Today's awesome R package ###
# devtools::install_github("R-CoderDotCom/ggbernie@main")
library(ggbernie)
ggplot(chemdata) +
  geom_point()


### Final Project ###
# can i take lter quadrat data and put the quadrats on a map
# then embed in each point the species 
  
  
### Code for the Baby Monitoring Shiny App
### Baby weight over time for each lil beeb

## Load Libraries
library(tidyverse)
library(here)
library(png)
library(grid)
library(ggimage)



## Read in Data
babydata <- read_csv(here("Week10","Data","HatchBabyExport.csv"))

## Data Analysis for Baby Weight


babyweight <- babydata %>% 
  rename(Name = 'Baby Name') %>% 
  filter(Activity == "Weight") %>% 
  select(-'End Time') %>% 
  separate(col = 'Start Time', into = c('Date','Time'), sep = " ", remove = T) %>% 
  mutate(Date = lubridate::mdy(Date)) %>% 
  mutate(Amount = as.numeric(Amount))

ggplot(babyweight, aes(x = Date, y = Amount)) +
  xlab("Date") +
  ylab("Baby Weight (lb)") +
  ggtitle(paste(" Weight of Baby Micah","\n in February and March, 2021", sep = " ")) + 
  geom_point(color = "orchid4") +
  geom_smooth(se=F, color = "orchid4", method = "lm") +
  theme_classic(base_size = 16)


## Data Analysis for Diaper use

# create personal palette
palette<-c("mediumaquamarine", "steelblue", "orchid4")

# create list of emoji image paths
images <- c(here("Week10","Scripts","Baby_Monitoring","www","Poop.png"),
            here("Week10","Scripts","Baby_Monitoring","www","Drip.png"),
            here("Week10","Scripts","Baby_Monitoring","www","Skull.png"))

diaper <- babydata %>% 
  rename(Name = 'Baby Name',
         Date = 'Start Time') %>% 
  filter(Activity == "Diaper") %>% 
  mutate(Date = lubridate::mdy_hm(Date)) %>% 
  separate(col = 'Date', into = c('Date','Time'), sep = " ") %>% 
  select(c(Name,Date,Amount,Notes)) %>% 
  group_by(Name) %>% 
  count(Amount) %>% 
  mutate(images = images)


diaper %>% 
  ggplot(aes(x = Amount, y = n, fill = Amount)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette) +
  theme_classic(base_size = 16) +
  labs(x = "Diaper Condition",
       y = "Total Diaper Count",
       title = " Number of diapers dealt with \n (lovingly) in February and March, 2021") +
  geom_image(aes(image = images), size = 0.1) +
  facet_wrap(~Name)







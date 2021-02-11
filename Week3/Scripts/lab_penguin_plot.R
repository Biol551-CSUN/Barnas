### Plotting Homework Penguin Data ###
### Created by Danielle Barnas
### Created on 2021-02-10

#################################################################

### Load Libraries ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(PNWColors)

rm(list=ls())


### Load Data ###
View(penguins)

penguins<-penguins%>%
  drop_na(sex) # remove na's from relevant data

# assign color palette
pal <- pnw_palette("Winter",2) # assign color palette, selecting 2 colors

ggplot(data=penguins,
       aes(x = species, 
           y = flipper_length_mm,
           color = sex, # group data by sex
           fill = sex # fills in boxplots by sex
           )) + 
  geom_boxplot(alpha=0.5) + # create boxplot and set the fill transparency
  geom_jitter(alpha=0.5, # overlay raw data and set point transparency
              position = position_jitterdodge( # set the position of the overlain points to be over their respective boxplots
              jitter.width = NULL,
              jitter.height = 0,
              dodge.width = 0.75
              )) +
  labs(x = "Species", # change labeling of figure elements
       y = "Flipper Length (mm)",
       color = "Sex",
       fill = "Sex",
       title = "Flipper length of male and female penguins",
       subtitle = "Penguin distribution by island"
  ) + 
  theme_bw() + # set theme to Black-White
  theme(axis.title = element_text(size = 20), # change text size for both axes
        legend.position = "right" # move the legend to the top of the figure
  ) +
  scale_color_manual(values = pal, labels=c("Female","Male")) + # set the boxplot outline colors to PNW and change value labels
  scale_fill_manual(values = pal, labels=c("Female","Male")) + # set the boxplot fill colors to PNW and change value labels to be consistent with color
  facet_wrap( ~ island, ncol = 3) + # show three figures, one per island
  ggsave(here("Week3","Output","lab_penguin_plot.png"), width = 10, height = 8) # safe figure
  


  
  
  
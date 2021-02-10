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
  drop_na(sex)


# assign color palette
pal <- pnw_palette("Winter",2)

ggplot(data=penguins,
       aes(x = species, 
           y = flipper_length_mm,
           color = sex,
           fill = sex
           )) + 
  geom_boxplot(alpha=0.5) + 
  geom_jitter(alpha=0.5, position = position_jitterdodge(
    jitter.width = NULL,
    jitter.height = 0,
    dodge.width = 0.75
  )) +
  labs(x = "Species",
       y = "Flipper Length (mm)",
       color = "Sex",
       fill = "Sex",
       title = "Flipper length of male and female penguins",
       subtitle = "Penguin distribution by island"
  ) + 
  theme_bw() +
  theme(axis.title = element_text(size = 20), # change text size for both axes
        legend.position = "right" # move the legend to the top of the figure
  ) +
  scale_color_manual(values = pal, labels=c("Female","Male")) +
  scale_fill_manual(values = pal, labels=c("Female","Male")) +
  facet_wrap( ~ island, ncol = 3) +
  ggsave(here("Week3","Output","lab_penguin_plot.png"), width = 10, height = 8)
  


  
  
  
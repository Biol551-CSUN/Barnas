### Week 4: Data wrangling Homework
### Created by Danielle Barnas
### Created on 2021-02-15
######################################

### Assignment
# Calculate the mean and variance of body mass by species, island, and sex without NAs.
# Filter out male (exclude) penguins, then calculate the log body mass, 
# then select only the columns for species, island, sex, and log body mass, 
# then use these data to make any plot.
# Use clear and clean labels and save the plot

### Load libraries
library(tidyverse)
library(palmerpenguins)
library(here)

glimpse(penguins)

### Data Analysis
penguin_summary<-penguins %>% # name new dataframe
  drop_na(c(species,island,sex)) %>% # drop NAs from three columns
  group_by(species,island,sex) %>%
  summarise(mean_BM = mean(body_mass_g),
            variance_BM = var(body_mass_g))

penguin_plot<-penguins %>%
  filter(sex != "male") %>%
  mutate(log_body_mass = log(body_mass_g)) %>%
  select(species,island,sex,log_body_mass)

  ggplot(data = penguin_plot,
         aes(y = log_body_mass,
             x = species,
             color = island,
             fill = island)) +
    geom_boxplot(alpha = 0.7) + 
    theme_bw() + # change plot theme to black-white
    labs(title = "Boxplot of log-transformed female penguin body mass (g)",
         subtitle = "Species grouped by island",
         color = "Island", # rename legend
         fill = "Island", 
         x = "Species", # label x axis
         y = "log(Body Mass) (g)") + # label y axis
    scale_color_viridis_d() +  # color blind friendly color palette
    scale_fill_viridis_d() +
    ggsave(here("Week4","Output","dplyr_penguin_hw.png"), width = 10, height = 13)
  
  





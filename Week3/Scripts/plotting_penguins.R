### Plotting Penguin Data ###
### Created by Danielle Barnas
### Created on 2021-02-10

#################################################################

### Load Libraries ###
library(palmerpenguins)
library(tidyverse)
library(here)

# library(ggthemes)
# library(PNWColors)
# library(ghibli)
library(beyonce)

# library(patchwork)
# library(ggstatsplot)


### Load Data ###
# The data is part of the package and is called penguins
glimpse(penguins)


### Data Analysis ###
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
       ) + 
  scale_color_viridis_d() # color blind friendly pallet

## Scales ##
# naming scheme:
  # 1. scale
  # 2. name of the primary 
  # 3. 

###### Limits ###
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_viridis_d() + # color blind friendly pallet
  scale_x_continuous(limits = c(10,20)) + # change limits of the x axis; or could be discrete if not continuous data
  scale_y_continuous(limits = c(30,50)) # change limits of the y axis


###### Breaks and Labels ###
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  #scale_color_viridis_d() + # color blind friendly palette
  scale_color_manual(values = c("orange", "purple", "green")) # manually change color scales
  #scale_x_continuous(breaks = c(14, 17, 21), # shows ticks at these values on the x axis
  #                   labels = c("low", "medium", "high")) # changes labels at the ticks from numbers to these labels


####### Installing Color Palettes ###


## First install devtools
## manual color palettes
# calecopal
# PNWColors
# Beyonce


#devtools::install_github("dill/beyonce") # install the beyonce color palette package
#devtools::install_github("an-bui/calecopal") # install the calecopal color palette package
#devtools::install_github("jakelawlor/PNWColors") # install the PNW color palette package
#install.packages('ghibli') # install the studio ghibli color palette

# library(PNWColors)
# pal <- pnw_palette("Starfish",3)

# library(ghibli)
# ghibli_palette(12)

library(beyonce)
beyonce_palette(10) # see the colors in this palette

ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) # allows you to choose the color palette (number 10) and a specific subset of colors


###### Changing Coordinates ###


# Flip the axes
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) + # allows you to choose the color palette (number 10) and a specific subset of colors
  coord_flip() # flip x and y axes


# Fix the axes
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) + # allows you to choose the color palette (number 10) and a specific subset of colors
  coord_fixed() # fix axes


# Transform x and y axes (log10)
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10")


# Make coordinates polar
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) + # allows you to choose the color palette (number 10) and a specific subset of colors
  coord_polar("x") # make axis polar
# can be useful for a timeseries


###### Themes ###
# theme ELEMENTS specify the non-data elements you can control
# datanovia.com/en/blog/ggplot-themes-gallery/
# library(ggthemes)


# theme_classic()
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) + # allows you to choose the color palette (number 10) and a specific subset of colors
  theme_classic() # removes all gridlines


# theme_bw()
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) + # allows you to choose the color palette (number 10) and a specific subset of colors
  theme_bw() # maintains faint grey gridlines



# theme_bw() with customizations
plot1<-ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species, # groups the data by species (allows geom_smooth to plot separate best fit lines)
                     color = species)) +  # colors by species groups
  geom_point() + 
  geom_smooth(method = "lm") +  # plots best fit line with confidence intervals; lm = linear model, creates linear regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = "Species"
  ) + 
  scale_color_manual(values = beyonce_palette(10)[c(1,5,3)]) + # allows you to choose the color palette (number 10) and a specific subset of colors
  theme_bw() +
  theme(axis.title = element_text(size = 20, # change text size for both axes
                                  color = "#003b6f"), # change axis text colors to TARDIS blue
        panel.background = element_rect(fill = "linen"), # change graph background color (can be any color or hex)
        legend.position = "top" # move the legend to the top of the figure
        ) +
  ggsave(here("Week3","Output","penguins.png"), width = 7, height = 5) # in inches


###### ggplot2 Extensions Resources
# exts.ggplot2.tidyverse.org/gallery
# ggplot2-book.org/mastery.html












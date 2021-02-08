### Graphing our first ggplot
# Created by Danielle Barnas
# Created on 2021-02-08
#################################################################
## Grammar of Graphics (ggplot2)

## Structure summary:
# ggplot (data = dataframe, mapping = aes(x-variable, y-variable)) + 
# geom_xxx + 

## Definitions
# mapping = layering on top of something
# aes = aesthetics
# geom = geometry / type of plot you're creating

## Getting started
library(tidyverse)

## Bring in data
library(palmerpenguins)
glimpse(penguins) # allows us to see a subset of the data

####################################################################
### Setting up the ggplot

# stages data. Plots tab shows blank white because no structure yet
ggplot(data = penguins) 


# fills in x and y labels and adds gridlines and scales
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm)) # i can define y


# gives us a basic black and white graph
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm)) + # i can define y
       geom_point() # geometry = point scatterplot

# gives a warning: removed 2 rows containing missing values: where there are NAs in the data


# color our data by species
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species)) + # adds color and legend
  geom_point()


# add title and subtitle
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species)) + # adds color and legend
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins") # subtitle


# add x and y labels
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species)) + # adds color and legend
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)") # add x and y label


# change legend title
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species)) + # adds color and legend
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species") # add legend title


# add caption
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species)) + # adds color and legend
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       caption = "Source: Palmer Station LTER / PalmerStation Package") # add bottom caption


# layer a color scheme that is useful for people with colorblindness
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species)) + # adds color and legend
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       caption = "Source: Palmer Station LTER / PalmerStation Package") +  # add bottom caption
  scale_color_viridis_d() # sets a color scale


# add shape aesthetic
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species, # adds color and legend
                     shape = island)) +  # adds shape by island
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       caption = "Source: Palmer Station LTER / PalmerStation Package") +  # add bottom caption
  scale_color_viridis_d() # sets a color scale


# add shape aesthetic
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species, # adds color and legend
                     shape = species)) +  # adds shape by species
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       shape = "Species", # so we only get one legend
       caption = "Source: Palmer Station LTER / PalmerStation Package") +  # add bottom caption
  scale_color_viridis_d() # sets a color scale


# add size aesthetic
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species, # adds color and legend
                     shape = species, # adds shape by species
                     size = body_mass_g)) + # adds size by the body mass
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       shape = "Species", # so we only get one legend
       caption = "Source: Palmer Station LTER / PalmerStation Package") +  # add bottom caption
  scale_color_viridis_d() # sets a color scale


# add alpha / transparency aesthetic
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm, # i can define y
                     color = species, # adds color and legend
                     shape = species, # adds shape by species
                     size = body_mass_g, # adds size by the body mass
                     alpha = flipper_length_mm)) + # adds transparency by flipper length
  geom_point() + 
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       shape = "Species", # so we only get one legend
       caption = "Source: Palmer Station LTER / PalmerStation Package") +  # add bottom caption
  scale_color_viridis_d() # sets a color scale


## aesthetics that can be changed
# color
# shape
# alpha (transparency)


## Mapping vs Setting
# Mapping: determine size, shape, alpha, etc based on the values of hte variable
# Settings: applies to all data, not dependent on value of the variable. used to make the plot pretty but not based on data

# change size and alpha of all data points
ggplot(data = penguins, # i can describe the dataframe
       mapping = aes(x = bill_depth_mm, # i can define x
                     y = bill_length_mm # i can define y
                     )) +
  geom_point(size = 2, alpha = 0.5) + # change size and transparency of all data
  labs(title = "Bill depth and length", # title
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", # subtitle
       x = "Bill depth (mm)", y = "Bill length (mm)", # add x and y label
       color = "Species", # add legend title
       shape = "Species", # so we only get one legend
       caption = "Source: Palmer Station LTER / PalmerStation Package") +  # add bottom caption
  scale_color_viridis_d() # sets a color scale


## Faceting
# Making smaller plots that display subsets of the data
# Useful for exploring conditional relationhips and large data

## Basic ggplot code

# sex as function of species
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() +
  facet_grid(species ~ sex) # species on the y is a function of sex on the x


# species as function of sex
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() +
  facet_grid(sex ~ species) # sex on the y is a function of species on the x


# facet wrap by species
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~ species) # split data by species


# facet wrap by species, change col number
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~ species, ncol = 2) # split data by species with two columns


# facet wrap by species, change row number
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~ species, nrow = 2) # split data by species with two rows


## facet_grid() is always 2D square grid
## facet_wrap() has more flexibility for customization of layout


# facet grid with colors by species (redundant)
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species
                     )) +
  geom_point() +
  scale_color_viridis_d() + 
  facet_grid(sex ~ species)


# remove lengend
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species
       )) +
  geom_point() +
  scale_color_viridis_d() + 
  facet_grid(sex ~ species) + 
  guides(color = FALSE) # removes legend


## Resources

# data-to-viz.com
# r-graph-gallery.com
# ggplot cheatsheets on github
# ggplot2.tidyverse.org/reference/
# r-statistics.co/ top 50 ggplots
# wilkelab.org/practicalgg/articles

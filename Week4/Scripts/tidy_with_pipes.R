### Week 4: Data wrangling
### Created by Danielle Barnas
### Created on 2021-02-15
######################################

### Load libraries
library(tidyverse)
library(palmerpenguins)
library(here)

glimpse(penguins)

### Filter
filter(.data = penguins, sex == "female") # dplyr calls upon the dataset using .data =

filter(.data = penguins, body_mass_g > 4000, sex == "female")
filter(.data = penguins, body_mass_g > 4000 & sex == "female")
filter(.data = penguins, year == "2008" | year == "2009", island != "Dream", species == "Adelie" | species == "Gentoo")

filter(.data = penguins, species %in% c("Adelie","Gentoo"))

### Mutate
data2<-mutate(.data = penguins,
              body_mass_kg = body_mass_g/1000,
              bill_length_depth = bill_length_mm/bill_depth_mm)

View(data2)

### Mutate with ifelse()
data2<- mutate(.data =  penguins,
               after_2008 = ifelse(year>2008, "After 2008", "Before 2008"))

data3<-mutate(.data = penguins,
              flipper_body = flipper_length_mm + body_mass_g,
              Sex = ifelse(sex == "male", "Male", "Female")) # add flipper lengtha nd body mass
# mutate if else to create new column where male and female are capitalized

View(data3)

penguins %>%
  filter(sex == "female") %>%
  mutate(log_mass = log(body_mass_g)) %>%
  select(Species = species,island,Sex = sex,log_mass) %>% # can rename a column while you select columns
  View()

penguins %>%
  group_by(species, island, sex) %>% # groups our data in preparation of the next step
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = T), # good practice to remove na's when doing summaries to skip over any missing data
            max_bill_length = max(bill_length_mm, na.rm = T))


penguins %>%
  drop_na(sex) # removes NAs from the specific column

penguins %>%
  drop_na() # removes NAs from the whole dataframe

penguins %>%
  drop_na(sex) %>% # removes missing data from sex
  group_by(species, island, sex) %>% # groups our data in preparation of the next step
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = T), # good practice to remove na's when doing summaries to skip over any missing data
            max_bill_length = max(bill_length_mm, na.rm = T)) # calculating mean and max bill length for these groups


penguins %>% # if i saved this as an object, the object would be the name of my plot because of the ggplot function below
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) + # can pipe diresctly to a ggplot
  geom_boxplot()

# dad joke function!!
# library(devtools)
# install_github("jhollist/dadjoke")
# library(dadjoke)
# groan()

### Homework
# calculate the mean and variance of body mass by species, island, and sex without NAs
# filter out male penguins, then calculate teh log body mass, then select only the columns for species, island, sex, and log body mass, then use these data to make any plot
# clear and clean labels and save the plot











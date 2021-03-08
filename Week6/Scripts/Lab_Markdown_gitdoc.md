Homework Markdown Document
================
Danielle Barnas and Group 1
3/3/2021

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Data Analysis](#data-analysis)
      - [Bring in data](#bring-in-data)
      - [Create Table showing SGD parameters by
        Site](#create-table-showing-sgd-parameters-by-site)
      - [Create Figure showing SGD parameters by
        Site](#create-figure-showing-sgd-parameters-by-site)

# Introduction

This document will display figures and information from submarine
groundwater data taken in Hawaii by Dr. Nyssa Silbiger and her team.

# Load Libraries

``` r
library(tidyverse)
library(here)
library(kableExtra)
library(PNWColors)
```

# Data Analysis

### Bring in data

**Load data with read\_csv**

``` r
chem<-read_csv(here("Week6","Data","sgd_chem_data_HI.csv"))
```

### Create Table showing SGD parameters by Site

**Round decimals for mean and standard error**

``` r
chem$Value_mean<-round(chem$Value_mean, digits = 3) # 3 decimal places
chem$Value_se<-round(chem$Value_se, digits = 2) # 2 decimal places
```

**Tidy data and create table**

``` r
chem_table<-chem %>% 
  unite(col = 'Mean +/- s.e.', Value_mean, Value_se, sep = " +/- ") %>% # combine mean and se columns
  select(-c(Value_variance, Value_sd)) %>%  # remove unnecessary columns
  pivot_wider(names_from = Site, values_from = 'Mean +/- s.e.') %>% # one column per site
  rename('Black Point  (mean +/- s.e.)' = BP, 'Wailupe  (mean +/- s.e.)' = W) %>%  # rename column headers
  mutate(Tide = ifelse(Tide == "High", "High Tide", "Low Tide")) %>% # relabel high and low tides
  select(-Variables) %>%  # remove variables column; will label variables below
  rename(Variables = Tide) %>% # rename tide column; variable names will be added later
  kbl(caption = "Table of measured SGD parameters at Black Point and Wailupe") %>% # create table with title
  kable_classic_2() %>% # format table style
  kable_styling(fixed_thead = T) %>% 
  row_spec(row = 0, bold = T, background = "#89689d", color = "#fff") %>%  # header specifications
  pack_rows(index = c("Nitrate+Nitrite"=2, # label variables above each set of High and Low Tide
                      "% SGD"=2, 
                      "pH"=2, 
                      "Phosphate"=2, 
                      "Salinity"=2, 
                      "Silicate"=2, 
                      "TA"=2, 
                      "Temperature in situ"=2),
            label_row_css = "background-color: #e69b99; color: #363434;") # set background color (consistent with PNW scale in plot) for row headers 
chem_table
```

<table class=" lightable-classic-2 table" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;'>

<caption>

Table of measured SGD parameters at Black Point and Wailupe

</caption>

<thead>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;color: #fff !important;background-color: #89689d !important;">

Variables

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;color: #fff !important;background-color: #89689d !important;">

Black Point (mean +/- s.e.)

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;color: #fff !important;background-color: #89689d !important;">

Wailupe (mean +/- s.e.)

</th>

</tr>

</thead>

<tbody>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>Nitrate+Nitrite</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

1.248 +/- 0.23

</td>

<td style="text-align:left;">

0.179 +/- 0.01

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

5.801 +/- 1.43

</td>

<td style="text-align:left;">

1.117 +/- 0.31

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>% SGD</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

1.344 +/- 0.16

</td>

<td style="text-align:left;">

0.401 +/- 0.03

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

3.785 +/- 0.8

</td>

<td style="text-align:left;">

3.147 +/- 0.8

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>pH</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

8.105 +/- 0.03

</td>

<td style="text-align:left;">

8.009 +/- 0.01

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

8.066 +/- 0.01

</td>

<td style="text-align:left;">

7.959 +/- 0.01

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>Phosphate</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

0.184 +/- 0.01

</td>

<td style="text-align:left;">

0.114 +/- 0

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

0.266 +/- 0.03

</td>

<td style="text-align:left;">

0.172 +/- 0.02

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>Salinity</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

34.272 +/- 0.05

</td>

<td style="text-align:left;">

34.545 +/- 0.01

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

33.463 +/- 0.26

</td>

<td style="text-align:left;">

33.611 +/- 0.27

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>Silicate</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

10.672 +/- 1.25

</td>

<td style="text-align:left;">

3.106 +/- 0.26

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

30.051 +/- 6.34

</td>

<td style="text-align:left;">

24.354 +/- 6.17

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>TA</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

2252.746 +/- 9.75

</td>

<td style="text-align:left;">

2276.765 +/- 3.69

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

2281.068 +/- 8.78

</td>

<td style="text-align:left;">

2253.017 +/- 6.3

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="background-color: #e69b99; color: #363434;">

<strong>Temperature in situ</strong>

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

High Tide

</td>

<td style="text-align:left;">

24.922 +/- 0.19

</td>

<td style="text-align:left;">

24.665 +/- 0.13

</td>

</tr>

<tr>

<td style="text-align:left;padding-left: 2em;" indentlevel="1">

Low Tide

</td>

<td style="text-align:left;">

24.39 +/- 0.05

</td>

<td style="text-align:left;">

24.04 +/- 0.07

</td>

</tr>

</tbody>

</table>

### Create Figure showing SGD parameters by Site

**Tidy data and create plot**

``` r
pal<-pnw_palette("Starfish", 2) # set color palette for figure

chem_plot<-chem %>% 
  select(-(Value_variance:Value_se)) %>% # only retain mean values and standard error
  pivot_wider(names_from = Variables, values_from = Value_mean) %>% # pivot wider by variables
  rename('Nitrate+Nitrite' = NN, '% SGD' = percent_sgd, Temperature = Temp_in) %>% # rename col headings
  pivot_longer(cols = 'Nitrate+Nitrite':Temperature, names_to = "Variables", values_to = "MeanVal") %>% # retain temp as its own col
  mutate(Site = ifelse(Site == "BP", "Black Point", "Wailupe")) %>% 
  ggplot(aes(x = Tide,
             y = MeanVal,
             color = Site,
             fill = Site)) + 
  geom_col(position = "dodge", size = 0.4) +
  theme_bw() +
  labs(title = "Figure of mean SGD parameters at high and low tide",
       subtitle = "Locations: Black Point and Wailupe",
       y = "Mean Parameter Values",
       x = "Tide",
       caption = "Source: Lubarsky et al. 2018") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  facet_wrap(~Variables, scales = "free")
chem_plot  
```

<img src="Lab_hw_Markdown_gitdoc_files/figure-gfm/ChemPlot-1.png" style="display: block; margin: auto;" />

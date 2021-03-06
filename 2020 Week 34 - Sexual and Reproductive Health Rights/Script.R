# Makeover Monday Week #34
# Theme:  Sexual Health and Reproductive Rights
# What This Script Does:  Visualizes unmet contraceptive needs among women throughout countries on the African continent
# Script by Alex Elfering
# Last Updated:    24 August 2020     

# Will always point to data source 
setwd("~/Documents/GitHub/Makeover-Monday-Challenges/2020 Week 34 - Sexual and Reproductive Health Rights")

####  Load packages ####
list.of.packages <- c("sf", 
                      "raster", 
                      'dplyr', 
                      'spData', 
                      'tmap', 
                      'leaflet', 
                      'cartogram')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(readr)

####  Data Loading  ####
mark1 <- read.csv('VIZ5_September_Contraceptive_Use_dataset.csv')

####  Data Cleaning ####
  # Shorter column names for the original data frame
new_columns <- c('Continent', 'Sub_Continent', 'Country', 'Percent', 'Pregnancy_Intention', 'Contraceptive_Availability', 
                 'Contraceptive_Method')
colnames(mark1) <- new_columns

  # This data frame filters on Africa, filters on 'Unmeet need' for Contraceptive_Availability
  # Also changes country names
mark2 <- mark1 %>% 
  dplyr::filter(Continent == 'Africa' & Contraceptive_Availability == 'Unmet need') %>%
  group_by(Continent,
           Sub_Continent,
           Country) %>%
  summarise(Percent = sum(Percent)) %>%
  ungroup() %>%
  mutate(Country = gsub("Gambia", "The Gambia", Country),
         Country = gsub('^Congo$', 'Republic of the Congo', Country),
         Country = gsub('Swaziland', 'eSwatini', Country),
         Country = gsub("C.te d'Ivoire", "Côte d'Ivoire", Country))

  # This dataframe loads geospatial data for African Countries
  # S/O to https://geocompr.github.io/geocompkg/articles/solutions08.html for the solution
africa = world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  full_join(mark2, by = c("name_long" = "Country")) %>% 
  dplyr::select(name_long, subregion, gdpPercap, Percent) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")


####  Aggregating percentages into buckets ####
breaks <- c(0, 0.05, .10, .15, .20, .25, .30, .35, 0.4)
tags <- c("0-5%", "5-10%", "10-15%", "15-20%", "20-25%", "35-30%", "30-35%", "35-40%")

africa$group <- cut(africa$Percent, breaks = breaks, labels = tags)

####  Plotting the visualization ####

# This visualization plots the percentages by country
ggplot(africa) +
  geom_sf(aes(geometry = geom, 
              fill = group),
          color = 'white') +
  # This extra plot highlights countries with highest percentages
  geom_sf(data = subset(africa, name_long %in% c('Benin', 'Democratic Republic of the Congo')),
          mapping = aes(geometry = geom),
          size = 1,
          color = 'black',
          fill = NA) +
  # Color scale
  scale_fill_manual(values = c('#ffecc7', '#ffc98e', '#f9a65f', '#e8873c', '#d26922', '#b94d11', '#9e320c')) +
  guides(fill = guide_legend(nrow = 1)) +
  # Labels
  labs(#title = 'Percent of Women with Unmet Contraceptive Needs',
       #subtitle = 'Unmet contraceptive needs can have a lasting impact\non education, employment, and economic wellbeing.',
       fill = 'Key:'#,
       #caption = 'Visualization by Alex Elfering\nSource: Guttmacher Institute, Operation Fistula\nFor more information, please visit https://www.guttmacher.org/'
       ) +
  # Theme manipulation
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.text = element_text(size = 14, family = 'Arial'),
        legend.title = element_text(size = 14, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
        ) 
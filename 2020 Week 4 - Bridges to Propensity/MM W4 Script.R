# Makeover Monday Week #4, 2020
# Bridges to Prosperity

# By Alex Elfering

library(dplyr)
library(tidyverse)
library(tidyr)
library(reshape2)
library(ggplot2)

# source: https://data.world/makeovermonday/2020w4

bridges <- read.csv('Bridges Dataset_2020.01.14.csv')
indicators <- read.csv('Global Indicators Dataset_2020.01.21.csv')

####  General notes ####
# Good to know: Bridges 2 Prosperity works with isolated communities to create access to essential resouces
# Has worked with 20 countries to build 318 bridges that connect 1.1 million people
# Positive affects: Increase in labor and farm income, as well as increasing female workforce participation rates

####  Tidy clean up ####
str(bridges)
str(indicators)

head(bridges)
head(indicators)

# How many NA values by column?
na_bridges <- sapply(bridges, function(y) sum(length(which(is.na(y)))))

dplyr::filter(bridges, is.na(Individuals.Directly.Served))
dplyr::filter(bridges, is.na(Span..m.))

# No NA values in the indicators dataset...cool
na_indicators <- sapply(indicators, function(y) sum(length(which(is.na(y))))) 


####  General summary exploration ####
# How many bridges have been built over time?
total_bridges <- bridges %>%
  select(B2P.Fiscal.Year,
         Stage,
         Project.Code) %>%
  group_by(B2P.Fiscal.Year, 
           Stage) %>% 
  summarise(Bridges = n_distinct(Project.Code))

ggplot(total_bridges, aes(x = B2P.Fiscal.Year, y = Bridges), group = Stage) +
  geom_line(aes(group = Stage, color = Stage)) +
  geom_point(aes(group = Stage, color = Stage)) 

# How many people have been affected over time?
total_people <- bridges %>%
  select(B2P.Fiscal.Year,
         Stage,
         Individuals.Directly.Served) %>%
  group_by(B2P.Fiscal.Year,
           Stage) %>%
  summarise(Total.Population.Affected = sum(Individuals.Directly.Served)) %>%
  as.data.frame()

ggplot(total_people, aes(x = B2P.Fiscal.Year, y = Total.Population.Affected), group = Stage) +
  geom_line(aes(group = Stage, color = Stage)) +
  geom_point(aes(group = Stage, color = Stage)) 
  


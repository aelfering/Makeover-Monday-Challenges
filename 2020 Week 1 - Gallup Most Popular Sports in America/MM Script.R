# Makeover Monday Week 1 2020

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)

sports_pop <- read.csv('Sports Popularity.csv')

sports_chg <- sports_pop %>%
  select(Sport, X2017, X2004) %>%
  mutate(Change = X2017-X2004) %>%
  arrange(desc(Change))


  
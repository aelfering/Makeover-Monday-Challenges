# makeover monday 2020 week 2

library(ggplot2)
library(dplyr)
library(waffle)
library(ggwaffle)

setwd("~/Documents/GitHub/Makeover-Monday-Challenges/2020 Week 2 - Pesticides Banned in the United States")

pesticide <- read.csv('MM data week 2 2020.csv')

names(pesticide) <- c('Category', 'Pounds.used.in.US.Ag', 'Pct.Total')

pesticide_number_chg <- dplyr::mutate(pesticide, Pounds.used.in.US.Ag = as.numeric(gsub(",", "", Pounds.used.in.US.Ag)))
total_pounds <- pesticide_number_chg[[2]][[1]]

pest_clean <- pesticide_number_chg %>%
  filter(Category != 'Total',
         Category %in% c('Banned in EU', 'Banned in CHN', 'Banned in BRA')) %>%
  mutate(Total.Pounds = total_pounds) %>%
  mutate(Pct = ceiling((Pounds.used.in.US.Ag/Total.Pounds) * 100),
         Category = as.character(Category)) %>%
  select(Category, 
         Pounds.used.in.US.Ag, 
         Pct)

eu_pest <- c(EU = pest_clean[[3]][[1]], Other = 100 - pest_clean[[3]][[1]])
chn_pest <- c(China = pest_clean[[3]][[2]], Other = 100 - pest_clean[[3]][[2]])
bra_pest <- c(Brazil = pest_clean[[3]][[3]], Other = 100 - pest_clean[[3]][[3]])

eu_pesticide <- waffle(eu_pest, 
                       title = '#MakeoverMonday 2020 Week #2 | The United States Uses Many Pesticides Banned Across the World',
                       rows = 5, 
                       colors = c("#D81B60", "#909799"))
chn_pesticide <- waffle(chn_pest, 
                        rows = 5, 
                        colors = c("#1E88E5", "#909799"))
bra_pesticide <- waffle(bra_pest, 
                        rows = 5, 
                        colors = c("#FFC107", "#909799"),
                        xlab = '\nVisual by Alex Elfering\nSource: Donley, Nathan; The USA lags behind other agricultural nations in banning harmful pesticides\nNumbers are rounded in this visualization. One square = 1%.')

iron(eu_pesticide, chn_pesticide, bra_pesticide)




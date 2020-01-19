# Makeover Monday 2020 Week #3
# Free sugars as percent of total calories

# Script by Alex Elfering
# 19 January 2020
# Source: https://www.nutrition.org.uk/nutritioninthenews/new-reports/ndnsyears7and8.html

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)

sugar <- read.csv('2020W3.csv')

colnames(sugar) <- c('Age.Group', '2008-09', '2010-11', '2012-13', '2014-15')

# I want to focus specifically on age groups right now
sugar_filtered <- dplyr::filter(sugar, grepl('Children', Age.Group) | grepl('Adults', Age.Group))

sugar_melt <- melt(sugar_filtered)
colnames(sugar_melt) <- c('Age.Group', 'Years', 'Percent')

sugar_percent <- dplyr::mutate(sugar_melt, Percent = Percent/100)

ggplot(sugar_percent, aes(x = Years, y = Percent), group = Age.Group) +
  scale_y_continuous(limits=c(0.03,.17),
                     breaks = seq(0, .20, by = .05)) +
  geom_line(aes(group = Age.Group)) +
  geom_point(aes(group = Age.Group)) +
  geom_hline(yintercept = .05, line_type = 'dashed')
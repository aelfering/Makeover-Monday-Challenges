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

# More palatable column names
colnames(sugar) <- c('Age.Group', '2008', '2010', '2012', '2014')

# I want to focus specifically on age groups right now
# By Gender has more limited data
sugar_filtered <- dplyr::filter(sugar, grepl('Children', Age.Group) | grepl('Adults', Age.Group))

sugar_melt <- melt(sugar_filtered)
colnames(sugar_melt) <- c('Age.Group', 'Years', 'Percent')

sugar_percent <- dplyr::mutate(sugar_melt, Percent = Percent/100)
sugar_percent$Years <- as.numeric(as.character(sugar_percent$Years))

ggplot(sugar_percent, aes(x = Years, y = Percent), group = Age.Group) +
  # Manually tweaking the x and y axis for labels
  scale_y_continuous(limits=c(0.03,.17),
                     breaks = seq(0, .20, by = .05)) +
  scale_x_continuous(limits=c(2007.75, 2014.25),
                     breaks = seq(2008, 2016, by = 2),
                     labels = c("2008-09", "2010-11", "2012-13", "2014-15", "2016-17")) +
  geom_line(aes(group = Age.Group)) +
  geom_hline(yintercept = .05, linetype="dotted") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # Line Segment
  geom_point(aes(group = Age.Group)) +
  # Labels & Annotations
  labs(title = '#MakeoverMonday 2020 Week 3: Free Sugar Intakes are Higher than Recommended in the UK',
       subtitle = '',
       caption = '\nVisualization by Alex Elfering\nSource: The British Nutrition Foundation',
       y = 'Percent of Total Calories') +
  theme(plot.title = element_text(face = 'bold'))
  
# Makeover Monday Week #6

# How much of your life the US has been at war

library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(ggplot2)

war <- read.csv('US life at war.csv')

# Replace 100 with 1 for correct percentages
war_new_percent <- war %>%
  mutate(New.Percentage = ifelse(X..of.your.life.the.US.has.been.at.war == 100, 1, X..of.your.life.the.US.has.been.at.war))

ggplot(war_new_percent, aes(x = Birth.year, y = New.Percentage)) +
  labs(title = 'How Much of Your Life the United States has Been in War'
       subtitle = ,
       y = 'Percent of Life',
       x = 'Year of Birth') +
  # The Cold War
  annotate("rect", xmin = 1947, xmax = 1991, ymin = 0, ymax = Inf, alpha = .2) +
  # World War I
  annotate("rect", xmin = 1914, xmax = 1918, ymin = 0, ymax = Inf, alpha = .4) +  
  # World War II
  annotate("rect", xmin = 1939, xmax = 1945, ymin = 0, ymax = Inf, alpha = .4) +  
  # Korean War
  annotate("rect", xmin = 1950, xmax = 1953, ymin = 0, ymax = Inf, alpha = .4) +  
  # Vietname War
  annotate("rect", xmin = 1959, xmax = 1975, ymin = 0, ymax = Inf, alpha = .4) +  
  # The Gulf War
  annotate("rect", xmin = 1990, xmax = 1991, ymin = 0, ymax = Inf, alpha = .4) +  
  # War in Afghanistan
  annotate("rect", xmin = 2001, xmax = 2020, ymin = 0, ymax = Inf, alpha = .4) +  
  geom_line(size = 1) +
  scale_colour_hc() + 
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hc()
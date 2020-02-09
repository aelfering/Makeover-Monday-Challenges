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
  mutate(New.Percentage = ifelse(X..of.your.life.the.US.has.been.at.war == 100, 1, X..of.your.life.the.US.has.been.at.war)) %>%
  # Adding dots to emphasize my annotations
  mutate(Dots = ifelse(Birth.year %in% c(1914, 1980, 1995, 2000), 'dot', ''))

ggplot(war_new_percent, aes(x = Birth.year, y = New.Percentage)) +
  labs(title = 'How Much of Your Life the United States has Been in War',
       caption = "\nVisualization by Alex Elfering\nSource: The Washington Post",
       subtitle = "A person who was born in 1914 experienced a variety of wars: both World Wars, the Korean War, the Vietnam War, and the Gulf War.\nHowever, over the long term, these wars made up nearly 40% of their life.\n\nThat stands in sharp contrast to people born in the last 40 years who have experienced war for over 50% of their lives so far. People born\naround the same time as when I was born in 1995 have experienced war 75% of their lives so far, and people born after 2001 have\nexperienced war their entire life. This rapid increase in percentage is largely from the ongoing War in Afghanistan since 2001.",
       y = 'Percent of Life',
       x = 'Year of Birth') +
  theme(plot.title = element_text(face = 'bold', size = 15)) +
  scale_x_continuous(limits = c(1905, 2020),
                     breaks = seq(1905, 2020, by = 10)) +
  # World War I
  annotate("rect", xmin = 1917, xmax = 1918, ymin = 0, ymax = Inf, alpha = .2) +  
  # World War II
  annotate("rect", xmin = 1939, xmax = 1945, ymin = 0, ymax = Inf, alpha = .2) +  
  # Korean War
  annotate("rect", xmin = 1950, xmax = 1953, ymin = 0, ymax = Inf, alpha = .2) +  
  # Vietname War
  annotate("rect", xmin = 1959, xmax = 1975, ymin = 0, ymax = Inf, alpha = .2) +  
  # The Gulf War
  annotate("rect", xmin = 1990, xmax = 1991, ymin = 0, ymax = Inf, alpha = .2) +  
  # War in Afghanistan
  annotate("rect", xmin = 2001, xmax = 2020, ymin = 0, ymax = Inf, alpha = .2) +  
  geom_line(size = 1) +
  geom_point(data = subset(war_new_percent, Dots == 'dot')) +
  scale_colour_hc() + 
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hc() +
  # Annotation Section
  geom_label(aes(x = 1912, y = 0.9, label = "War"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = 'none', label.size = NA, family="Helvetica", size = 4) +
  geom_label(aes(x = 1918, y = 0.22, label = "A person born in\n1914 experienced\nwar 37% of their life."), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = 'none', label.size = NA, size = 4) +
  geom_label(aes(x = 1982, y = 0.27, label = "A person born in\n1980 experienced\nwar 50% of their life."), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = 'none', label.size = NA, size = 4) +
  geom_label(aes(x = 1972, y = 0.9, label = "I was born in 1995 and\nhave experienced war\n75% of my life so far."), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = 'none', label.size = NA, size = 4) +
  geom_label(aes(x = 2002, y = 0.6, label = "A person born in\n2001 or later has\nexperienced war\ntheir entire life."), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = 'none', label.size = NA, size = 4) +
# Arrow section
geom_curve(aes(x = 1918, y = 0.24, xend = 1914, yend = 0.36), 
           colour = "#555555", 
           size=0.5, 
           curvature = -0.2,
           arrow = arrow(length = unit(0.01, "npc"))) +
geom_curve(aes(x = 1914, y = 0.87, xend = 1917, yend = 0.83), 
             colour = "#555555", 
             size=0.5, 
             curvature = 0.2,
             arrow = arrow(length = unit(0.01, "npc")))  
geom_curve(aes(x = 1983, y = 0.26, xend = 1980, yend = 0.5), 
           colour = "#555555", 
           size=0.5, 
           curvature = -0.2,
           arrow = arrow(length = unit(0.01, "npc"))) 


# Makeover Monday Week 1 2020

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)

sports_pop <- read.csv('Sports Popularity.csv')

sports_melt <- melt(sports_pop)
sports_no_x <- sports_melt %>%
  select(Year = variable,
         Sport,
         Percent = value) %>%
  mutate(Year = substr(Year, start = 2, stop = 5),
         Month = 1,
         Date = 1,
         Percent = Percent/100) %>%
  mutate(Full.Date = paste(Month, Date, Year, sep = "-")) %>%
  mutate(Full.Date = mdy(Full.Date))

football <- subset(sports_no_x, sports_no_x$Sport == 'Football')
soccer <- subset(sports_no_x, sports_no_x$Sport == 'Soccer')
basketball <- subset(sports_no_x, sports_no_x$Sport == 'Basketball')
baseball <- subset(sports_no_x, sports_no_x$Sport == 'Baseball')
none <- subset(sports_no_x, sports_no_x$Sport == 'None')

base_graph <- ggplot(sports_no_x, aes(x = Full.Date, y = Percent), group = Sport) +
  geom_line(group = sports_no_x$Sport, 
             colour = '#DEDEDE',
             size = 1) +
  geom_line(data=football, 
             aes(x=Full.Date, y=Percent), 
             colour="#D81B60", 
             size=1) +
  geom_line(data=soccer, 
             aes(x=Full.Date, y=Percent), 
             colour="#1E88E5", 
             size=1) +
  geom_line(data=basketball, 
             aes(x=Full.Date, y=Percent), 
             colour="#FFC107", 
             size=1) +
  geom_line(data=baseball, 
             aes(x=Full.Date, y=Percent), 
             colour="#004D40", 
             size=1) +
  geom_line(data=none, 
             aes(x=Full.Date, y=Percent), 
             colour="#9C9C9C", 
             size=1) +
  geom_point(group = sports_no_x$Sport, 
             colour = '#DEDEDE',
             size = 2) +
  geom_point(data=football, 
             aes(x=Full.Date, y=Percent), 
             colour="#D81B60", 
             size=2) +
  geom_point(data=soccer, 
             aes(x=Full.Date, y=Percent), 
             colour="#1E88E5", 
             size=2) +
  geom_point(data=basketball, 
             aes(x=Full.Date, y=Percent), 
             colour="#FFC107", 
             size=2) +
  geom_point(data=baseball, 
             aes(x=Full.Date, y=Percent), 
             colour="#004D40", 
             size=2) +
  geom_point(data=none, 
             aes(x=Full.Date, y=Percent), 
             colour="#9C9C9C", 
             size=2) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))

base_graph +
  
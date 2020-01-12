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

min_year <- min(sports_no_x$Year)
max_year <- max(sports_no_x$Year)

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
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))

# Annotations
base_graph +
   labs(axis.title.x = 'Year',
        axis.title.y = 'Percent',
        title = "What are American's Favorite Sport to Watch?",
        subtitle = "")

base_graph +
  # Soccer
  geom_label(aes(x = as.Date("2017-05-01"), y = 0.07, label = "Soccer"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +
  geom_segment(aes(x = as.Date("2017-04-30"), y = 0.07, xend = as.Date("2017-01-30"), yend = 0.07), 
               colour = "#555555", size=0.5) +
  # Basketball
  geom_label(aes(x = as.Date("2017-05-01"), y = 0.11, label = "Basketball"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +  
  geom_segment(aes(x = as.Date("2017-04-30"), y = 0.11, xend = as.Date("2017-01-30"), yend = 0.11), 
               colour = "#555555", size=0.5) +
  # Baseball
  geom_label(aes(x = as.Date("2017-05-01"), y = 0.09, label = "Baseball"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +
  geom_segment(aes(x = as.Date("2017-04-30"), y = 0.09, xend = as.Date("2017-01-30"), yend = 0.09), 
               colour = "#555555", size=0.5) +
  # Football
  geom_label(aes(x = as.Date("2017-05-01"), y = 0.37, label = "Football"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +
  geom_segment(aes(x = as.Date("2017-04-30"), y = 0.37, xend = as.Date("2017-01-30"), yend = 0.37), 
               colour = "#555555", size=0.5)

  
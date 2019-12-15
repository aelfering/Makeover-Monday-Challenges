# Makeover Monday 
# 2019 Week 51
# By Alex Elfering

library(dplyr)
library(bbplot)

# Load dataset and remove NA values
nba_defense <- read.csv('NBA Defensive Ratings.csv')
nba_defense[is.na(nba_defense)] = 0




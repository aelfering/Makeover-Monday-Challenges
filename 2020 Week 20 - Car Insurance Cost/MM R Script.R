# Makeover Monday 
# Most Expensive Auto Insurance

# Script by Alex Elfering
# Created 17 May 2020
# Data Sources: ValuePenguin

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(statebins)

insurance <- read.csv('Cost of car insurance by state.csv')

# Remove the $ sign and commas
clean_insurance <- insurance %>%
  mutate(Full.coverage = gsub('\\$', '', Full.coverage),
         Full.coverage = gsub('\\,', '', Full.coverage),
         Minimum.coverage = gsub('\\$', '', Minimum.coverage),
         Minimum.coverage = gsub('\\,', '', Minimum.coverage)) %>%
  mutate(Full.coverage = as.integer(Full.coverage),
         Minimum.coverage = as.integer(Minimum.coverage),
         Difference = Full.coverage-Minimum.coverage,
         National.Average = mean(Full.coverage),
         Percent.Average = National.Average/Full.coverage,
         State = gsub('Washington D.C.', 'District of Columbia', State)) %>%
  mutate(State = as.character(State))

statebins(state_data = clean_insurance,          
          state_col = "State", 
          value_col = "Full.coverage",
          text_color = "black", 
          breaks = 7,
          labels = c("$1,000-$1,500", 
                     "$1,500-$2,000",
                     "$2,000-$2,500",
                     "$2,500-$3,000",
                     "$3,000-$3,500",
                     "$3,500-$4,000",
                     "$4,000-$9,000"),
          brewer_pal="OrRd", 
          font_size = 3, 
          legend_title="Average Annual Cost of Full Coverage")
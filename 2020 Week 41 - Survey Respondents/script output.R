# makeover monday
# week 41 - Survey Respodents by Industry

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(scales)

setwd("~/Documents/GitHub/Makeover-Monday-Challenges/2020 Week 41 - Survey Respondents")

book1 <- read.csv('Book1.csv')

head(book1)
str(book1)

# Data cleaning
new_col_names <- c('Industry.Sector', 'Percent')
colnames(book1) <- new_col_names

book1_pct <- dplyr::mutate(book1, Percent.Respondents = Percent/100)

ggplot(book1_pct,
       aes(x = reorder(Industry.Sector, Percent.Respondents),
           y = Percent.Respondents,
           label = percent(Percent.Respondents))) +
  geom_bar(stat = 'identity',
           position = 'identity') +
  coord_flip() +
  geom_text(aes(hjust = 1.2), color = "white") +
  geom_hline(yintercept = 0,
             size = 1) +
  scale_y_continuous(labels = percent) +
  labs(x = '',
       y = '',
       title = 'Percent of Survey Respondents',
       caption = 'Visualization by Alex Elfering\nSource: dataIQ')






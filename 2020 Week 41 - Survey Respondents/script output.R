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

max_percent <- max(book1_pct$Percent.Respondents)

# reorder the industries to put 'other' at the bottom
book1_pct %>%
  mutate(Is.Not.Other = ifelse(Industry.Sector == 'Other', 1, 0),
         ordering = as.numeric(-Is.Not.Other) + Percent.Respondents,
         Industry.Sector = fct_reorder(Industry.Sector, ordering, .desc = F)) %>% 
  ggplot(aes(x = Industry.Sector,
             y = Percent.Respondents,
             label = percent(Percent.Respondents))) +
  geom_bar(stat = 'identity',
           position = 'identity', 
           fill = '#73a2c6') +
  coord_flip() +
  geom_text(aes(hjust = 1.2), color = "white") +
  geom_hline(yintercept = 0,
             size = 1) +
  scale_y_continuous(limits=c(0,max_percent),
                     breaks = seq(0, max_percent, by = 0.02),
                     labels = percent) +
  labs(x = '',
       y = '',
       title = 'Percent of Respondents',
       subtitle = 'Roughly 13% of organizations interviewed by dataIQ worked in reta ',
       caption = 'Visualization by Alex Elfering | Data Source: dataIQ') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 



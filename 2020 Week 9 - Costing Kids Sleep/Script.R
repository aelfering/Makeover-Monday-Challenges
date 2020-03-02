# Makeover Monday Week #9
# Costing Kids Sleep

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(extrafont)
library(ggthemes)

sleep <- read.csv('Week9MM.csv')

head(sleep)

sleep.grade <- dplyr::mutate(sleep, 
                             Grade = gsub('Ninth grade', '9th', Grade),
                             Grade = gsub('Eighth grade', '8th', Grade),
                             Grade = gsub('Seventh grade', '7th', Grade),
                             Grade = gsub('Sixth grade', '6th', Grade),
                             Grade = gsub('Fifth grade', '5th', Grade),
                             Grade = gsub('Fourth grade', '4th', Grade),
                             Grade = gsub('Third grade', '3rd', Grade),
                             Grade = gsub('Second grade', '2nd', Grade),
                             Grade = gsub('First grade', '1st', Grade),
                             Grade = gsub('Kindergarten', 'Kind', Grade),
                             Grade = gsub('10th grade', '10th', Grade),
                             Grade = gsub('11th grade', '11th', Grade),
                             Grade = gsub('12th grade', '12th', Grade))

grade.order <- c('Kind', '1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th', '11th', '12th')
  
ggplot(sleep.grade, 
       aes(x = `Hours.Needed`, 
           xend = `Hours.Averaged`, 
           y = Grade)) + 
  coord_flip() +
  geom_dumbbell(colour = "#dddddd",
                size = 5,
                colour_x = "#198462",
                colour_xend = "#a8caa6") +
  scale_y_discrete(limits = grade.order) +
  labs(title="How Much Sleep are American Students Missing on Average by Grade?",
       subtitle="The older students become, the less sleep they typically need. However, the older students\nbecome, the less sleep they appear to get on average, and the more that deficit appears to grow.\n",
       caption = '\nVisualization by Alex Elfering\nSource: "Costing Kids Sleep" by David Kloser, Savvy Sleeper (2020)\nAs reported by parents with children in grades between K-12.',
       x = 'Hours',
       y = 'Grade') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 22, hjust = 0),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = element_blank()) 
  








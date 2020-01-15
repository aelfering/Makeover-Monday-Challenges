# makeover monday 2020 week 2

library(ggplot2)
library(dplyr)
library(waffle)
library(ggwaffle)

pesticide <- read.csv('MM data week 2 2020.csv')

names(pesticide) <- c('Category', 'Pounds.used.in.US.Ag', 'Pct.Total')

pesticide_number_chg <- dplyr::mutate(pesticide, Pounds.used.in.US.Ag = as.numeric(gsub(",", "", Pounds.used.in.US.Ag)))
total_pounds <- pesticide_number_chg[[2]][[1]]

pest_clean <- pesticide_number_chg %>%
  filter(Category != 'Total',
         Category %in% c('Banned in EU', 'Banned in CHN', 'Banned in BRA')) %>%
  mutate(Total.Pounds = total_pounds) %>%
  mutate(Pct = ceiling((Pounds.used.in.US.Ag/Total.Pounds) * 100),
         Category = as.character(Category)) %>%
  select(Category, 
         Pounds.used.in.US.Ag, 
         Pct)

pest_other_row <- pest_clean %>%
  add_row(Category = 'Other', 
          Pounds.used.in.US.Ag = total_pounds - pest_clean[[2]][[1]] - pest_clean[[2]][[2]] - pest_clean[[2]][[3]],
          Pct = 100 - pest_clean[[3]][[1]] - pest_clean[[3]][[2]] - pest_clean[[3]][[3]])

ndeep <- 5

pesticide_waffles <- expand.grid(y = 1:ndeep, x = seq_len(ceiling(sum(pest_other_row$Pct) / ndeep)))

pest_vec <- rep(pest_other_row$Category, pest_other_row$Pct)

pesticide_waffles$Category <- c(pest_vec, rep(NA, (nrow(pesticide_waffles) - length(pest_vec))))

pesticide_waffles$colors = ifelse(pesticide_waffles$Category == 'Banned in EU', "#D81B60",
                          ifelse(pesticide_waffles$Category == 'Banned in CHN', "#1E88E5",
                                 ifelse(pesticide_waffles$Category == 'Banned in BRA', "#FFC107", "#909799")))

group.colors <- colorRampPalette(c("#D81B60", "#1E88E5", "#FFC107", "#909799"))(100)

ggplot(pesticide_waffles, aes(x = x, y = y)) + 
  geom_tile(size = 0.5,
            fill = pesticide_waffles$colors,
            colour = ifelse(pesticide_waffles$x == 20 & pesticide_waffles$y == 5, 'black', 'white')) +
  labs(title = '#MakeoverMonday 2020 Week #2 | The United States Uses Many Pesticides Banned Across the World',
       subtitle = "The United States, The European Union, China, and Brazil are not only the world's most dominant agricultural producers, but they are also the world's largest users of pesticide.\nResearch by Nathan Donley from BioMed Central revealed that roughly 1 out of 3 pounds of pesticide used in the United States is banned in the EU, China, and Brazil combined.\nUse of pesticides is harmful not only to the individual, but also to the surrounding ecosystem, too.\n",
       caption = 'Visual by Alex Elfering\nSource: Donley, Nathan; The USA lags behind other agricultural nations in banning harmful pesticides') +
  theme_waffle() +
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  geom_segment(aes(x = 2, y = 6, xend = 2, yend = 5), 
               colour = "#555555", size=0.5, linetype = "dashed") +
  geom_label(aes(x = 1.5, y = 6.2, label = "Banned in the EU"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +
  
  geom_segment(aes(x = 6, y = 6, xend = 6, yend = 5), 
               colour = "#555555", size=0.5, linetype = "dashed") +
  geom_label(aes(x = 5.5, y = 6.2, label = "Banned in China"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +
  
  geom_segment(aes(x = 10, y = 6, xend = 7, yend = 4), 
               colour = "#555555", size=0.5, linetype = "dashed") +
  geom_label(aes(x = 9.5, y = 6.2, label = "Banned in Brazil"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4) +
  
  geom_segment(aes(x = 20, y = 6, xend = 20, yend = 5), 
               colour = "#555555", size=0.5, linetype = "dashed") +
  geom_label(aes(x = 18, y = 6.2, label = "1 square = 1%"), 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 4)



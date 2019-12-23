# 2019 Week 52
# Christmas Spending Between the UK and Europe

library(dplyr)
library(ggplot2)

christmas <- read.csv('Christmas Spending.csv')
christmas$Year <- as.numeric(christmas$Year)

# Pivoted the data to compare 2019 and 2018 spending
amount_18 <- christmas %>%
  filter(Category != 'Total', Year == 2018) %>%
  select(Region,
         Category,
         amount_2018 = Amount)
amount_19 <- christmas %>%
  filter(Category != 'Total', Year == 2019) %>%
  select(Region,
         Category,
         amount_2019 = Amount)
total_amount <- inner_join(amount_18, amount_19, by = c("Region" = "Region",
                                                        "Category" = "Category"))
compare_amounts <- total_amount %>%
  mutate(Pct.Chg = round((amount_2019-amount_2018)/amount_2018, 3),
         Chg = amount_2019-amount_2018)

### Visualizing the data via grouped barplot
ggplot(compare_amounts, 
       aes(x = Category, 
           y = Pct.Chg * 100, 
           fill = as.factor(Region),
           label = scales::percent(Pct.Chg))) +
  geom_col(width=0.5,    
           position=position_dodge(0.5)) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(position = position_dodge(width = 0.5), 
            size = 5, 
            hjust = -0.1) +  
  labs(title="Comparing Average Holiday Spending between the UK and Europe",
       subtitle = "Planned spending on travel in the UK will increase 21.6% from 2018 while the rest of Europe will increase just 5.6%.\nMeanwhile, planned spending on socializing will fall 7.4% in the UK whereas in Europe it will grow 4.7%.",
       y = "Percent Change between 2018 and 2019",
       x = "Category",
       caption = "\nSpending adjusted to British Pound for an average exchange rate of 1EUR = 0.887586GBP. Survey was collected among 7,190 people between September 16-October 11, 2019 in Europe.\nVisualization by Alex Elfering | Source: Deloitte 2019 Christmas Survey via #MakeoverMonday") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 25),
        plot.subtitle = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.caption = element_text(size = 12)) +
  guides(fill=guide_legend(title="Region"))

####  Old facet wrap idea ####
# I was going to use this idea initially before deciding to go with something to easily compare numbers
ggplot(compare_amounts, 
       aes(x = Category, 
           y = Pct.Chg * 100,
           label = scales::percent(Pct.Chg))) +
  geom_bar(stat="identity", 
           position="identity", 
           aes(fill = Region)) +
  coord_flip() +
  facet_wrap(~Region) +
  labs(title = "Britons on the move this Holiday Season",
       y = "Percent Change Between 2018-2019",
       x = "Category",
       caption = "Spending adjusted to British Pound\nVisualization by Alex Elfering | Source: #MakeoverMonday") +
  theme_bw() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(position = position_dodge(width = 1), 
            size = 3, 
            hjust = -0.1) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size= 20),
        strip.text.x = element_text(size = 12))




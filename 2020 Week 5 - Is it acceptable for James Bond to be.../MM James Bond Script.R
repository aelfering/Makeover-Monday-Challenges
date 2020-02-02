# Makeover Monday Week #5
# James Bond Survey
# Code by Alex Elfering
# 2 February 2020

bond <- read.csv('Week5Bond.csv')

bond_melt <- melt(bond)
bond_columns_named <- bond_melt %>%
  select(Category, 
         Response,
         Remain.Leave = variable,
         Percent = value) %>%
  mutate(Percent = Percent/100,
         Majority = ifelse(Percent >= 0.5, "Majority", "Non-Majority"))

ggplot(bond_columns_named, aes(x = Remain.Leave, y = Percent, fill = Response, label = scales::percent(Percent))) +
  geom_bar(stat = "identity", 
           position = "fill") +
  coord_flip() +
  facet_wrap(~Category) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.5) +
  scale_fill_manual(values = c('#4E89D2', '#DCDCDC', '#FF9500')) +
  scale_colour_hc() + 
  scale_y_continuous(labels = scales::percent_format()) +
  theme_hc() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 15, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(title = "YouGov: Do you think it's Acceptable for James Bond to be...?",
       subtitle = "58% of Britons think it is acceptable for James Bond to be starred by an ethnic minority. However, Britons narrowly find a female 
or non-British James Bond unacceptable. However, the overall sample was ambivalent at the notion of a gay James Bond.

While nearly 2 out of 3 people who voted Remain find a gay James Bond acceptable, and 7 out of 10 are fine with a James Bond 
from an ethnic minority, there is less support for either quality from people who voted Leave. Bothe sides, however, are less 
supportive of a non-British James Bond character.",
       caption = "\nNotes: Sample Size is 1,677 GB Adults \n Fieldwork conducted 17th - 18th September 2018 \n Source: YouGov       Visualization by Alex Elfering") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) 


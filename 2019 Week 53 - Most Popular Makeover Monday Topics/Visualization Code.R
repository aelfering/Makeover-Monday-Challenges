# Makeover Monday 2019 Week 53
# Most Popular Dataset Downloads for Makeover Monday

# By Alex Elfering

library(dplyr)
library(lubridate)
library(ggplot2)

mmonday <- read.csv('Downloads by Dataset.csv')

mmonday$date <- dmy(mmonday$date)

#  Removing 2016 and 2017 data since it appears to be missing several weeks of topics
mmonday_18_19 <- mmonday %>%
  filter(Year > 2017) %>%
  as.data.frame()

# How many more downloads in 2019 since 2018?
mmonday_18_19 %>%
  filter(Year == 2018, Week <= 52) %>%
  summarise(Downloads = sum(Downloads))

mmonday_18_19 %>%
  filter(Year == 2019, Week <= 52) %>%
  summarise(Downloads = sum(Downloads))

mmonday_top_10 <- mmonday_18_19 %>%
  group_by(Subject) %>%
  summarise(Downloads = sum(Downloads)) %>%
  arrange(desc(Downloads)) %>%
  mutate(Download_Rank = rank(desc(Downloads))) %>%
  select(Subject, Download_Rank) %>%
  ungroup() %>%
  as.data.frame()

mmonday_top_10_flag <- left_join(mmonday_18_19, mmonday_top_10, by = c('Subject' = 'Subject'))
mmonday_top_10_flag <- mmonday_top_10_flag %>%
  mutate(Top_10_Flag = ifelse(Download_Rank <= 10, TRUE, FALSE))

ggplot(mmonday_top_10_flag, aes(x = date, y = Downloads)) +
  geom_step(color = 'gray') +
  geom_point(shape = 21,
             colour = ifelse(mmonday_top_10_flag$Top_10_Flag == TRUE, "black", "gray"), 
             fill = "gray", 
             size = 2, 
             stroke = 1) +
  theme_minimal() +
  labs(title = "#MakeoverMonday Week 53: Do Downloads Correlate with Submissions?",
       subtitle = "The number of #MakeoverMonday dataset downloads this year sits at 14,657 up from 1,759 downloads last year* through Week 52",
       caption = "\n*2018 had 52 total weeks of challenges while 2019 will end with 53 total weeks of challenges.\nSource:Data-World\nVisualization by Alex Elfering") +
  geom_label(aes(x = as.Date('2019-06-15', "%Y-%m-%d"), y = 750, label = "The 10 most popular\ndownloads were all in\nthe latter half of this year."), 
             hjust = 0, just = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, family="Helvetica", size = 3)






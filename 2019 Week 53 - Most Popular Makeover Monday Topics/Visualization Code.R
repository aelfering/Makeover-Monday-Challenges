# Makeover Monday 2019 Week 53
# Most Popular Dataset Downloads for Makeover Monday

# By Alex Elfering

library(dplyr)
library(lubridate)
library(ggplot2)

mmonday <- read.csv('Downloads by Dataset.csv')

mmonday$date <- dmy(mmonday$date)

mmonday_18_19 <- subset(mmonday, mmonday$Year > 2017) #  Removing 2016 and 2017 data since it appears to be missing several weeks of topics

mmonday_top_10 <- mmonday_18_19 %>%
  group_by(Subject) %>%
  summarise(Downloads = sum(Downloads)) %>%
  arrange(desc(Downloads)) %>%
  mutate(Download_Rank = rank(desc(Downloads))) %>%
  filter(Download_Rank <= 10) %>%
  select(Subject) %>%
  ungroup() %>%
  as.data.frame()


ggplot(mmonday_18_19, aes(x = date, y = Downloads)) +
  geom_step() +
  geom_point(size = 2, color = ifelse(mmonday_18_19$Subject %in% mmonday_top_10, 'orange', 'black'))






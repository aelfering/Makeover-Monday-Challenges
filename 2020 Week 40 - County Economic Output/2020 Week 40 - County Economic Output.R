# makeover monday script

library(urbnmapr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)

setwd("~/Documents/GitHub/Makeover-Monday-Challenges/2020 Week 40 - County Economic Output")

gdp_counties <- read.csv('GDP by County.csv')

new_col_names <- c('year', 'county.fips', 'region', 'sub.region', 'state', 'state.abbr', 'county', 'county.full.name', 'gdp.chained')

colnames(gdp_counties) <- new_col_names

counties_sf <- get_urbn_map("counties", sf = TRUE)
states_sf <- get_urbn_map("states", sf = TRUE)

compare_gdp <- gdp_counties %>%
  filter(year == max(year),
         state.abbr == 'IA') %>%
  select(county.fips,
         region,
         sub.region,
         state,
         state.abbr,
         county,
         county.full.name,
         compare.gdp.chained = gdp.chained) %>%
  mutate(total_compare_gdp = sum(compare.gdp.chained),
         pct_compare_gdp = compare.gdp.chained/total_compare_gdp,
         county.fips = as.character(county.fips)) %>%
  arrange(desc(pct_compare_gdp)) %>%
  mutate(running_pct_compare_gdp = round(cumsum(pct_compare_gdp), 2))

join_counties <- inner_join(counties_sf, compare_gdp, by = c('county_fips' = 'county.fips') )

ggplot(join_counties) +
  geom_sf(data = subset(counties_sf, state_abbv == 'IA'),
          fill = '#eaeaea',
          color = 'white',
          size = 0.5) +
  geom_sf(data = subset(states_sf, state_abbv == 'IA'),
          fill = NA,
          color = 'black',
          size = 1) +
  geom_sf(data = subset(join_counties, running_pct_compare_gdp <= 0.5),
          fill = '#ed6f3c',
          color = 'black',
          size = 1) +
  labs(title = "",
       subtitle = "",
       caption = '') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.title.align = 0.5,
        #legend.key.height = unit(2, "cm"),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 


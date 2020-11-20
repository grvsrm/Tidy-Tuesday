# Prerequisites
library(tidyverse)
library(fpp3)


# Load the data
phones <- read_rds(here("data", "phones.rds"))


# Exploration
phones %>% 
    filter(country == "India",
           !is.na(subscriptions)) %>% 
    ggplot(aes(year, subscriptions, color = type)) +
    geom_line() +
    scale_x_continuous(breaks = 1:30,
                       labels = seq(1990, 2019, 1)) +
    labs(title = "Phone Subscriptions over years",
         color = "",
         x = "",
         y = "Subscriptions per 100 people")

phones %>% 
    ggplot(aes(year, subscriptions, color = type, group = interaction(type, country))) +
    geom_line()

country_sizes <- phones %>% 
    group_by(country) %>% 
    summarise(avg_population = mean(total_pop, na.rm = T))

    
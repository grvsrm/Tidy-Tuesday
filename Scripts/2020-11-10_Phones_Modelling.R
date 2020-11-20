# Prerequisites
library(tidyverse)
library(here)
library(directlabels)


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


country_sizes <- phones %>% 
    group_by(country) %>% 
    summarise(avg_population = mean(total_pop, na.rm = T))

phones %>% 
    semi_join(country_sizes %>% 
                  top_n(40, avg_population)) %>% 
    filter(!is.na(subscriptions)) %>% 
    ggplot(aes(year, subscriptions, color = type, group = interaction(type, country))) +
    geom_line() +
    geom_dl(aes(label = country), method = "last.points") +
    facet_wrap(~continent) 


phones %>% 
    group_by(year, continent) %>% 
    summarise(subs = mean(subscriptions, na.rm = T)) %>% 
    ggplot(aes(year, subs, color = continent)) +
    geom_line() +
    geom_dl(aes(label = continent), method = "last.points") +
    theme(legend.position = "none")

phones %>% 
    semi_join(country_sizes %>% 
                  top_n(40, avg_population)) %>% 
    filter(!is.na(income)) %>% 
    ggplot(aes(year, subscriptions, color = type, group = interaction(type, country))) +
    geom_line() +
    geom_dl(aes(label = country), method = "last.points") +
    facet_wrap(~income) 


summarise_subscriptions = . %>% 
    filter(!is.na(subscriptions)) %>% 
    summarise(avg_subscriptions = mean(subscriptions, na.rm = T),
              median_subscriptions = median(subscriptions),
              p25 = quantile(subscriptions, .25),
              p75 = quantile(subscriptions, .75))

phones %>% 
    group_by(country) %>% 
    summarise_subscriptions()



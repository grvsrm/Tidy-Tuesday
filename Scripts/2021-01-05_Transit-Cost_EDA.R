# Pre requisites
library(tidyverse)
library(here)
library(scales)
theme_set(theme_light())


# Read the data
transit_cost <- read_rds(here("data", "transit-cost-clean.rds"))



# Explore
transit_cost %>% 
    filter(country_name == "India") %>% 
    mutate(line = fct_reorder(line, year)) %>% 
    ggplot(aes(xmin = start_year, xmax = end_year, y = line)) +
    geom_errorbarh(aes(color = city)) +
    labs(x = "Year",
         y = "",
         color = "City")

transit_cost %>% 
    ggplot(aes(cost_km_millions)) +
    geom_histogram()

transit_cost %>% 
    filter(!is.na(cost_km_millions),
           tunnel_per == 1) %>% 
    mutate(country_name = fct_lump(country_name, 12)) %>% 
    add_count(country_name) %>% 
    mutate(country_name = glue::glue("{country_name} ({n})"),
           country_name = fct_reorder(country_name, cost_km_millions)) %>% 
    ggplot(aes(cost_km_millions, country_name)) +
    geom_boxplot() +
    scale_x_continuous(labels = dollar_format()) +
    labs(x = "Cost/km (Millions USD)",
         y = "")

transit_cost %>% 
    filter(!is.na(cost_km_millions),
           tunnel_per == 1,
           country_name == "China") %>% 
    mutate(city = fct_lump(city, 12)) %>% 
    add_count(city) %>% 
    mutate(city = glue::glue("{city} ({n})"),
           city = fct_reorder(city, cost_km_millions)) %>% 
    ggplot(aes(cost_km_millions, city)) +
    geom_boxplot() +
    scale_x_continuous(labels = dollar_format()) +
    labs(x = "Cost/km (Millions USD)",
         y = "") +
    expand_limits(x = 0)

transit_cost %>% 
    filter(country_name == "China") %>% 
    mutate(decade = start_year%/%5*5) %>% 
    ggplot(aes(decade, cost_km_millions, group = decade)) +
    geom_boxplot() +
    geom_jitter()

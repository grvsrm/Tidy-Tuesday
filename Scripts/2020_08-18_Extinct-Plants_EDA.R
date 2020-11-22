# Prerequisites ----
library(tidyverse)
library(here)
library(scales)

theme_set(theme_light())

# Data load ----

plants <- read_rds(here("data", "plants.rds"))
threats <- read_rds(here("data", "threats.rds"))

# EDA ----
plants %>% 
    count(country = fct_lump(country, 10), sort = T) %>% 
    mutate(country = fct_reorder(country, n)) %>% 
    filter(country != "Other") %>% 
    ggplot(aes(n, country)) +
    geom_col()

plants %>% 
    count(continent, sort = T) %>% 
    mutate(continent = fct_reorder(continent, n)) %>% 
    ggplot(aes(n, continent)) +
    geom_col()

plants %>% 
    filter(!is.na(year_last_seen)) %>% 
    count(year_last_seen, continent) %>% 
    mutate(year_last_seen = fct_reorder(year_last_seen, n, sum)) %>% 
    ggplot(aes(n, year_last_seen, fill = continent)) +
    geom_col() +
    labs(title = "BY when most Species went extinct in various continent ?",
         x = "Number of Species",
         y = "",
         fill = "Continent",
         caption = "Data Source : R4DS")


threats %>% 
    filter(threatened == 1) %>% 
    count(threat_type, continent) %>% 
    mutate(threat_type = fct_reorder(threat_type, n, sum)) %>% 
    ggplot(aes(n, threat_type, fill = continent)) +
    geom_col() +
    labs(title = "What are the most common threats in various continent ?",
         x = "",
         y = "",
         fill = "Continent",
         caption = "Data Source : R4DS")



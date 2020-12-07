# Prerequisites ----
library(tidyverse)
library(here)
library(scales)
library(tidytext)
library(RColorBrewer)


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


by_continent_threat <- threats %>% 
    count(threat_type, continent) %>% 
    mutate(threat_type = fct_reorder(threat_type, n, sum))


by_continent_threat %>% 
    mutate(threat_type = reorder_within(threat_type, n, continent)) %>% 
    ggplot(aes(n, threat_type, fill = continent)) +
    geom_col() +
    facet_wrap(~continent, scales = "free") +
    scale_y_reordered() +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = "What are the most common threats in various continent ?",
         x = "",
         y = "",
         fill = "Continent",
         caption = "Data Source : R4DS")


threats %>% 
    count(year_last_seen, threat_type, continent) %>% 
    mutate(threat_type = fct_reorder(threat_type, -n, sum))  %>% 
    filter(fct_lump(threat_type, 9, w=n) != "Other") %>% 
    ggplot(aes(year_last_seen, n, fill = continent)) +
    geom_col() +
    facet_wrap(~threat_type) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "How the species have gone extinct due to various reasons ?",
         x = "Last Seen",
         y = "Number of Species",
         caption = "Data Source : R4DS")

threats %>% 
    tabyl(country) %>% 
    arrange(desc(percent)) %>% 
    as_tibble()

plants %>% 
    tabyl(country) %>% 
    arrange(desc(percent)) %>% 
    as_tibble()

plants %>% 
    count(genus, sort = T)

actions %>% 
    count(group, sort = T)

actions %>% 
    count(action_type, sort = T)



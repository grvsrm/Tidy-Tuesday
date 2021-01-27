# Pre requisites
library(tidyverse)
library(here)
library(tidymodels)
library(janitor)


# Download the data
key_crop_yields <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv")

land_use <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv")


key_crop_yields


top_countries <- land_use %>% 
    clean_names() %>% 
    filter(!is.na(code)) %>% 
    group_by(entity) %>% 
    filter(year == max(year)) %>% 
    ungroup() %>% 
    slice_max(total_population_gapminder, n =30) %>% 
    pull(entity)


key_crop_yields %>% 
    clean_names() %>% 
    pivot_longer(wheat_tonnes_per_hectare: bananas_tonnes_per_hectare,
                 names_to = "crop",
                 values_to = "yield",
                 values_drop_na = T) %>% 
    mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>% 
    filter(crop %in% c("wheat", "maize", "barley", "rice"),
           entity %in% top_countries)


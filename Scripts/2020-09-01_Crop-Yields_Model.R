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


# Pre-requisites
library(tidyverse)
library(tidymodels)
library(here)
library(scales)
library(janitor)

theme_set(theme_light())

# Load the data
food_consumption <- read_rds(here("data", "food_consumption.rds"))


food <- food_consumption %>% 
    select(-co2_emmission) %>% 
    pivot_wider(names_from = food_category, values_from = consumption) %>% 
    mutate(asia = case_when(continent != "Asia" ~ "Not Asia",
                                 TRUE ~ "Asia")) %>% 
    select(-country, -continent) %>% 
    clean_names() %>% 
    mutate_if(is_character, factor)

    




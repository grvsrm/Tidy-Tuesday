# Pre-requisites
library(tidyverse)
library(here)
library(janitor)
library(countrycode)


# Download tha data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>% 
    write_rds(here("data", "food_consumption_raw.rds"))


# Clean the data
food_consumption <- read_rds(here("data", "food_consumption_raw.rds")) %>% 
    mutate(continent = countrycode(country, origin = "country.name",
                                   destination = "continent")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))



# Save the final clean data

food_consumption %>% 
    write_rds(here("data", "food_consumption.rds"))



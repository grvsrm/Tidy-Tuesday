# Pre-requisites
library(tidyverse)
library(here)
library(janitor)


# Download tha data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>% 
    write_rds(here("data", "food_consumption_raw.rds"))


# Clean the data
food_consumption <- read_rds(here("data", "food_consumption_raw.rds"))


# Save the final clean data

food_consumption %>% 
    write_rds(here("data", "food_consumption.rds"))



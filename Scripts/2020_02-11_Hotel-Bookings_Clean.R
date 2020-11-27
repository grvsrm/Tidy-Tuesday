# Prerequisites

library(tidyverse)
library(here)
library(janitor)


# Downloadthe data
# read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv') %>% 
#     write_rds(here("data", "hotel_bookings_raw.rds"))
# 

    

# Clean the data

hotel_cleaned <- read_rds(here("data", "hotel_bookings_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))

# Save the data
hotel_cleaned %>% 
    write_rds(here("data", "hotel_bookings.rds"))


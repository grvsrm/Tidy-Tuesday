# Prerequisites
library(tidyverse)
library(here)
library(janitor)
library(scales)
library(broom)

theme_set(theme_light())

# load the data

big_epa_cars <- read_rds(here("data", "big_epa_cars.rds"))

big_epa_cars %>% 
    select(barrels08, city_mpg = city08, combined_mpg = comb08,
           cylinders, displ, drive, eng_id, eng_dscr, fuel_cost08,
           guzzler, highway_mpg = highway08, highway_e,
           make, model, v_class, year, you_save_spend,
           charger)
big_epa_cars %>% summary()

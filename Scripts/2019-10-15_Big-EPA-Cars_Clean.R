# Prerequisites
library(tidyverse)
library(here)
library(janitor)


# Download the data

# read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv") %>%
#     write_rds(here("data", "big_epa_cars_raw.rds"))



# Clean the data

big_epa_cars_cleaned <- read_rds(here("data", "big_epa_cars_raw.rds")) %>% 
    mutate(uses_electricity = ifelse(highwayE>0, "uses_electricity", "doesn't use electricity"))


big_epa_cars_cleaned %>% 
    count(fuelType)

# Save the clean data

big_epa_cars_cleaned %>% 
    write_rds(here("data", "big_epa_cars.rds"))

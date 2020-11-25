# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)


# Download and save the data as an rds object

readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv') %>% 
    write_rds(here("data", "wind_turbine_raw.rds"))


# Cleaning

wind_turbine <-  read_rds(here("data", "wind_turbine_raw.rds")) %>% 
    transmute(turbine_capacity = turbine_rated_capacity_k_w,
           commissioning_date = parse_number(commissioning_date),
           model = fct_lump(model, 10),
           province_territory = fct_lump(province_territory, 8),
           rotor_diameter_m,
           hub_height_m) %>% 
    filter(!is.na(turbine_capacity)) %>% 
    mutate_if(is.character, factor)

wind_turbine %>% 
    write_rds(here("data", "wind_turbine.rds"))



# End of Script
# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)


# Download and save the data as an rds object

readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv') %>% 
    write_rds(here("data", "wind_turbine.rds"))


# Cleaning
wind_turbine_raw <- read_rds(here("data", "wind_turbine.rds"))

wind_turbine <-  wind_turbine_raw %>% 
    mutate(turbine_capacity = row_number(),
           commissioning_date = parse_number(commissioning_date),
           model = fct_lump(model, 10),
           province_territory = fct_lump(province_territory, 8)) %>% 
    filter(!is.na(turbine_rated_capacity_k_w)) %>% 
    select(turbine_capacity, commissioning_date, model, province_territory, rotor_diameter_m,
           hub_height_m, turbine_rated_capacity_k_w) %>% 
    mutate_if(is.character, factor)


##### Modelling


# Data Split
turbine_split <- wind_turbine %>% 
    initial_split(strata = turbine_rated_capacity_k_w)

turbine_train <- training(turbine_split)
turbine_test <- testing(turbine_split)


# Resamples

turbine_cv <- turbine_train %>% 
    vfold_cv()
    
# Recipe

# Model Spec
turbine_spec <- decision_tree(cost_complexity = tune(),
                              tree_depth = tune(),
                              min_n = tune()) %>% 
    set_engine(engine = "rpart") %>% 
    set_mode(mode = "regression")


# Prerequisites

library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(themis)

# Load the data

hotel_cleaned <- read_rds(here("data", "hotel_bookings.rds"))

hotel_stays <- hotel_cleaned %>% 
    filter(is_canceled == 0) %>% 
    mutate(children = case_when(children + babies >0 ~ "children",
                                TRUE ~ "none"),
           required_car_parking_spaces = case_when(required_car_parking_spaces >0 ~ "parking",
                                                   TRUE ~ "none"),
           arrival_date_month = factor(arrival_date_month, levels = month.name)) %>% 
    select(children, hotel, arrival_date_month, meal, adr, adults,
           required_car_parking_spaces,total_of_special_requests,
           stays_in_week_nights, stays_in_weekend_nights) %>% 
    mutate_if(is_character, factor)
    

# Modeling

# Split
hotel_split <- hotel_stays %>% 
    initial_split(strata = children)

hotel_train <- hotel_split %>% training()
hotel_test <- hotel_split %>% testing()


# Recipe
hotel_recipe <- hotel_train %>%
    recipe(children~.) %>% 
    step_downsample(all_outcomes()) %>% 
    step_downsample(required_car_parking_spaces) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>% 
    step_zv(all_numeric()) %>% 
    step_normalize(all_numeric()) %>% 
    prep()

# Model Spec

knn_spec <- nearest_neighbor() %>% 
    set_mode("classification") %>% 
    set_engine("kknn")


tree_spec <- decision_tree() %>% 
    set_mode("classification") %>% 
    set_engine("rpart")

# Fit

knn_fit <- fit(knn_spec,
    children~.,
    data = juice(hotel_recipe))


tree_fit <- fit(tree_spec,
    children~.,
    data = hotel_recipe %>% juice())



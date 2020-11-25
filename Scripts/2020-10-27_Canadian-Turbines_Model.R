# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)

# Load the clean data
turbines <- read_rds(here("data", "wind_turbine.rds"))

turbines

# Modeling

# Data Split

turbine_split <- turbines %>% 
    initial_split(strata = turbine_capacity)

turbine_train <- turbine_split %>% training()
turbine_test <- turbine_split %>% testing()


# Model Spec

lm_spec <- linear_reg() %>% 
    set_engine("lm")

dt_spec <- decision_tree() %>% 
    set_mode("regression") %>% 
    set_engine("rpart")

#  Linear Fit

lm_fit <- fit(lm_spec,
    turbine_capacity~.,
    turbine_train)

lm_fit %>% 
    predict(turbine_test) %>% 
    bind_cols(turbine_test) %>% 
    rmse(turbine_capacity, .pred)

# Decision Tree Fit
dt_fit <- fit(dt_spec,
    turbine_capacity~.,
    data = turbine_train)

dt_fit %>% 
    predict(turbine_test) %>% 
    bind_cols(turbine_test) %>% 
    rmse(turbine_capacity, .pred)


# End of Script

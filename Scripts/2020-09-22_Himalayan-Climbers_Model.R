# Prerequisites
library(tidyverse)
library(here)
library(tidymodels)
library(lubridate)

# Load the data
expeditions <- read_rds(here("data", "expedition.rds"))

expeditions %>% view()


# Prepare the data
expedition_df <- expeditions %>% 
    transmute(peak_name, year, season, 
              basecamp_month = month(basecamp_date, label = T),
              highpoint_month = month(highpoint_date, label = T),
              highpoint_metres, members, hired_staff, oxygen_used, days_to_highpoint, success) %>% 
    filter(season != "Unknown",
           success != "Other") %>% 
    mutate_if(is.character, factor)


# Split

expedition_split <- expedition_df %>% 
    initial_split(strata = success)

expedition_train <- expedition_split %>% training()    
expedition_test <- expedition_split %>% testing()


# Recipe

expedition_recipe <- expedition_train %>%
    recipe(success~.) %>% 
    step_other(peak_name) %>% 
    step_knnimpute(all_predictors(), neighbors = 5) %>% 
    step_dummy(all_nominal(), -all_outcomes())

expedition_recipe


# Model Spec
glm_spec <- logistic_reg() %>% 
    set_engine("glm")

rf_spec <- rand_forest() %>% 
    set_mode("classification") %>% 
    set_engine("ranger")


# Workflow
expedition_wf <- workflow() %>% 
    add_recipe(expedition_recipe)


# Fit GLM
glm_fit <- expedition_wf %>% 
    add_model(glm_spec) %>% 
    fit(data = expedition_train)

glm_fit %>% 
    pull_workflow_fit()


glm_fit %>% predict(expedition_test) %>% 
    bind_cols(success = expedition_test$success) %>% 
    accuracy(success, .pred_class)

# Fit RF

rf_fit <- expedition_wf %>% 
    add_model(rf_spec) %>% 
    fit(data = expedition_train)

rf_fit %>% 
    pull_workflow_fit()

rf_fit %>% 
    predict(expedition_test) %>% 
    bind_cols(success = expedition_test$success) %>% 
    accuracy(success, .pred_class)

# So random forest appears to be a better fit on test data in these settings
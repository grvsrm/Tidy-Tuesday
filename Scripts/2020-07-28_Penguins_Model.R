# Prerrequisites
library(tidyverse)
library(here)
library(tidymodels)


# Load the data

penguins <- read_rds(here("data", "penguins.rds"))


# Tidymodelling

# Split the data

penguin_split <- penguins %>% 
    initial_split(strata = species)

penguin_train <- penguin_split %>% training()

penguin_test <- penguin_split %>% testing()


# Recipe
penguin_recipe <- penguin_train %>% 
    recipe(species~.) %>% 
    update_role(year, new_role = "id") %>% 
    step_knnimpute(all_predictors(), neighbors = 5) %>% 
    step_dummy(all_nominal(), -all_outcomes())

# Prepped recipe
penguin_recipe %>% prep() %>% 
    juice() %>% view()

# Resamples
penguin_boots <- penguin_train %>% 
    bootstraps()


# Model Spec

# random forest specs
rf_spec <- rand_forest() %>% 
    set_engine("ranger") %>% 
    set_mode("classification")



# Workflow

penguin_wf <- workflow() %>% 
    add_recipe(penguin_recipe) %>% 
    add_model(rf_spec)


# Fit

rf_rs <- penguin_wf %>% 
    fit_resamples(resamples = penguin_boots)

rf_rs %>% 
    collect_metrics()

penguin_wf %>% 
    fit(data = penguin_train) %>% 
    predict(new_data = penguin_test) %>%
    bind_cols(penguin_test) %>%
    accuracy(species, .pred_class)


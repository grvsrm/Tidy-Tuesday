# Pre-requisites ----

library(tidyverse)
library(here)
library(janitor)
library(tidymodels)


# Read the data ----

penguins <- read_rds(here("data", "penguins.rds"))

# EDA ----

# Summaries
penguins %>% 
    count(species)
    
penguins %>% 
    count(island)

penguins %>% 
    tabyl(species, island) %>% 
    adorn_title()

penguin_pivoted <- penguins %>% 
    pivot_longer(3:6, names_to = "metric", values_to = "value")


penguin_pivoted %>% 
    ggplot(aes(species, value, color = species)) +
    geom_boxplot() +
    geom_jitter() +
    facet_wrap(~metric, scales = "free")

# Bill depth, Bill Length, Body mass & flipper length are good identifiers of three penguins. 
# These variables must be used as predictors in the classification model.


penguins %>% 
    tabyl(species, sex)

# It would be nice if we can impute sex in missing rows rather than removing these observations. knn imputtaion may work becuase
# categorical variables are good differentiators..

penguins %>% 
    count(year)


penguins

# Split
penguin_split <- penguins %>% 
    initial_split(strata = sex)

penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)

# Resamples
penguin_boot <- penguin_train %>% 
    bootstraps()

# Model Spec
glm_spec <- logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode("classification")


rf_spec <- rand_forest() %>% 
    set_engine("ranger") %>% 
    set_mode("classification")


# Workflow
penguin_wf <- workflow() %>% 
    add_formula(sex~.)


# Train the model

glm_fit <- penguin_wf %>% 
    add_model(glm_spec) %>% 
    fit(data = penguin_train)
 
glm_rs <- penguin_wf %>% 
    add_model(glm_spec) %>% 
    fit_resamples(resamples = penguin_boot,
                  control = control_resamples(save_pred = T))

rf_rs <- penguin_wf %>% 
    add_model(rf_spec) %>% 
    fit_resamples(resamples = penguin_boot,
                  control = control_resamples(save_pred = T))


# Results
glm_rs %>% 
    collect_metrics()


rf_rs %>% 
    collect_metrics()

glm_rs %>% 
    conf_mat_resampled()

rf_rs %>% 
    conf_mat_resampled()

# End of Script

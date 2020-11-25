# Pre-requisites
library(tidyverse)
library(tidymodels)
library(here)
library(scales)
library(janitor)

theme_set(theme_light())

# Load the data
food_consumption <- read_rds(here("data", "food_consumption.rds"))


food <- food_consumption %>% 
    select(-co2_emmission) %>% 
    pivot_wider(names_from = food_category, values_from = consumption) %>% 
    mutate(asia = case_when(continent != "Asia" ~ "Not Asia",
                                 TRUE ~ "Asia")) %>% 
    select(-country, -continent) %>% 
    clean_names() %>% 
    mutate_if(is_character, factor)

# Explore
food %>% 
    GGally::ggpairs(aes(color = asia))

# Modeling

# Resamples

food_boot <- food %>% 
    bootstraps(strata = asia)

# GLM Model Spec
glm_spec <- logistic_reg() %>% 
    set_engine("glm")

# Fit

glm_fit <- fit_resamples(glm_spec,
              asia~.,
              resamples = food_boot,
              control = control_resamples(save_pred = T, verbose = T),
              metrics = metric_set(accuracy, sensitivity, specificity, roc_auc))

glm_fit %>% 
    collect_metrics()



# RF Model Spec with tuning parameters

rf_spec <- rand_forest(mtry = tune(),
            trees = 1000,
            min_n = tune()) %>% 
    set_mode("classification") %>% 
    set_engine("ranger")


# Fit 

rf_fit <- tune_grid(rf_spec,
          asia~.,
          resamples = food_boot,
          control = control_resamples(save_pred = T, verbose = T),
          metrics = metric_set(accuracy, sensitivity, specificity, roc_auc))

rf_fit %>% 
    collect_metrics()
glm_fit %>% 
    collect_metrics()

rf_fit %>% 
    show_best("roc_auc")


# End of Sript

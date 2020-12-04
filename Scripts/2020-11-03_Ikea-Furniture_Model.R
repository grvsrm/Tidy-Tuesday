# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)
library(usemodels)
library(textrecipes)

# Load the data

ikea_cleaned <- read_rds(here("data" ,"ikea_cleaned.rds"))

# A linear model to predict prices category wise
ikea_cleaned %>% 
    select(item_id, category, price_usd, sellable_online, other_colors, designer, category_total) %>% 
    nest(data = c(item_id, price_usd, sellable_online, other_colors, designer, 
                  category_total)) %>% 
    mutate(model = map(data, ~lm(price_usd~sellable_online + other_colors + designer + category_total, data = .)),
           tidied = map(model, tidy)) %>% 
    unnest(tidied)


 
# Data for model

ikea_df <- ikea_cleaned %>% 
    select(price, name, category, depth, height, width) %>% 
    mutate(price = log10(price)) %>% 
    mutate_if(is_character, factor)


# Modeling


# Data Split
set.seed(123)

ikea_split <- ikea_df %>% 
    initial_split(strata = price)

ikea_train <- ikea_split %>% training()
ikea_test <- ikea_split %>% testing()


# Resamples
set.seed(234)
ikea_folds <- ikea_train %>% 
    bootstraps(strata = price)

# Recipe
ikea_recipe <- ikea_train %>% 
    recipe(price~.) %>% 
    step_clean_levels(name, category) %>% 
    step_other(name, category, threshold = 0.01) %>% 
    step_knnimpute(depth, width, height) %>% 
    prep()


# Model Spec
rf_spec <- rand_forest(mtry = tune(),
                       trees = 1000,
                       min_n = tune()) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")

# Workflow
ikea_wf <- workflow() %>% 
    add_recipe(ikea_recipe) %>% 
    add_model(rf_spec)

# Parallel Processing (If reqd)
library(doParallel)
library(tictoc)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# Fit
tic()

rf_fit <- tune_grid(ikea_wf,
          resamples = ikea_folds,
          control = control_resamples(save_pred = T, verbose = T),
          metrics = metric_set(rmse, rsq, mae),
          grid = 10)

toc()
stopCluster(cl)
registerDoSEQ()

#######

# Evaluation

rf_fit %>% 
    autoplot("performance")

# Finalize the workflow with best tuning parameters
final_wf <- ikea_wf %>% 
    finalize_workflow(parameters = select_best(rf_fit, metric = "rmse"))

# Lets check our model on complete data

final_wf %>% 
    last_fit(ikea_split) %>% 
    collect_metrics()


# End of script






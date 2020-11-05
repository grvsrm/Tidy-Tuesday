# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)

# Load the clean data
wind_turbine <- read_rds(here("data", "wind_turbine.rds"))


##### Modelling


# Data Split
turbine_split <- wind_turbine %>% 
    initial_split(strata = turbine_rated_capacity_k_w)

turbine_train <- training(turbine_split)
turbine_test <- testing(turbine_split)


# Resamples

turbine_cv <- turbine_train %>% 
    vfold_cv()


# Model Spec
turbine_spec <- decision_tree(cost_complexity = tune(),
                              tree_depth = tune(),
                              min_n = tune()) %>% 
    set_engine(engine = "rpart") %>% 
    set_mode(mode = "regression")


# Grid Spec

turbine_grid <- grid_regular(cost_complexity(),
                             tree_depth(),
                             min_n(),levels = 5)

# Set up parallel processing
doParallel::registerDoParallel()


# Fit the model
set.seed(343)

turbine_res <- tune_grid(
    turbine_spec,
    turbine_rated_capacity_k_w~.,
    resamples = turbine_cv,
    grid = turbine_grid,
    metrics = metric_set(rmse, rsq, mae, mape),
    control = control_grid(verbose = T,allow_par = T)
)

# Results
turbine_res %>% collect_metrics()

turbine_res %>% autoplot()

turbine_res %>% show_best()

turbine_res %>% select_best("rmse")


# Lets finalize the model
turbine_final_spec <- turbine_spec %>% 
    finalize_model(turbine_res %>%
                       select_best("rmse"))

# Lets fit this final model to the entire data

final_fit <- turbine_final_spec %>% 
    fit(turbine_rated_capacity_k_w~., 
        turbine_train)


final_res <- turbine_final_spec %>% 
    last_fit(turbine_rated_capacity_k_w~.,
             turbine_split)

## Lets check final results
final_res %>% collect_metrics()


# Lets save this model for future(if reqd)

final_fit %>% 
    write_rds(here("models", "wind_turbine_dt_model.rds"))

# In future if we want to predict anything on this model then...

# First  method
final_fit %>% 
    predict(turbine_test[44,])


# Second Method
final_res$.workflow[[1]] %>% 
    predict(turbine_train[44,])

predict(final_res$.workflow[[1]], turbine_train[2,])

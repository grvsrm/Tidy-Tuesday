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
set.seed(234)
turbine_cv <- turbine_train %>% 
    vfold_cv()


# Model Spec

# Tree Spec with tuning parameters
dt_spec <- decision_tree(cost_complexity = tune(),
                         tree_depth = tune(),
                         min_n = tune()) %>% 
    set_mode("regression") %>% 
    set_engine("rpart")

# Tuning Grid
dt_grid <- grid_regular(cost_complexity(),
             tree_depth(),
             min_n(),
             levels = 4)


# Initiate parallel processing
doParallel::registerDoParallel()

# Fit
set.seed(345)

dt_res <- tune_grid(dt_spec,
          turbine_capacity~.,
          resamples = turbine_cv,
          grid = dt_grid,
          metrics = metric_set(rmse, mae, rsq, mape),
          control = control_resamples(save_pred = T, verbose = T)
)


dt_res %>% collect_metrics()
dt_res %>% collect_predictions()
dt_res %>% autoplot()
dt_res %>% show_best()
dt_res %>% select_best()


# Finalize Model Spec
dt_final_spec <- dt_spec %>% 
    finalize_model(dt_res %>% select_best("rmse"))


# Finally we have two options to use this model that has final specs

# Method 1, It simply fitsthe model, no predictions or results are evaluated
dt_final_fit <- fit(dt_final_spec,
    turbine_capacity~.,
    turbine_train)


# Method 2, It not only fits the model, also results are evaluated
dt_final_res <- last_fit(dt_final_spec,
                    turbine_capacity~.,
                    turbine_split)
dt_final_res$.metrics


# Now if we want to predict using these two final outcomes

# Method 1 using output of fit
dt_final_fit %>% 
    predict(turbine_train[44,])


# Method 1 using output of last_fit
dt_final_res$.workflow[[1]] %>% 
    predict(turbine_train[44,])

# As we can see, both give same results, so these two are just to different methods, use anyone as per your need. 


# Last thing that we should do is check the variable importance plot. 
# Because in trees we can't see the output in a simplistic manner

library(vip)

dt_final_fit %>% 
    vip(fill = "red", alpha = .4) +
    scale_y_continuous(labels = comma_format(), expand = c(0,0))


dt_final_res %>% 
    collect_predictions() %>% 
    ggplot(aes(turbine_capacity, .pred)) +
    geom_point() +
    geom_abline()

# End of script
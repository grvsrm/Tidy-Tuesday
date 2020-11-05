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

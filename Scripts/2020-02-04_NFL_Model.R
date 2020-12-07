# Pre-requisites
library(tidyverse)
library(here)
library(janitor)
library(tidymodels)




# Load the data

attendance <- read_rds(here("data", "attendance.rds"))
standings <- read_rds(here("data", "standings.rds"))


attendance_joined <- attendance %>% 
    left_join(standings) %>% 
    select(week, everything())


attendance_df <- attendance_joined %>% 
    filter(!is.na(weekly_attendance)) %>% 
    select(weekly_attendance, team_name, year, week,
           margin_of_victory, strength_of_schedule, playoffs) %>% 
    mutate_if(is.character, factor)


# Model

# Split

nfl_split <- attendance_df %>% 
    initial_split(strata = playoffs)

nfl_train <- attendance_split %>% 
    training()

nfl_test <- attendance_split %>% 
    testing()


# Model Spec

# lm
lm_spec <- linear_reg() %>% 
    set_engine("lm")

# rf
rf_spec <- rand_forest() %>% 
    set_mode("regression") %>% 
    set_engine("ranger")


# lm fit
lm_fit <- fit(lm_spec,
    weekly_attendance ~.,
    data = nfl_train)

lm_fit %>% tidy() %>% 
    arrange(p.value)

# rf fit
rf_fit <- fit(rf_spec,
    weekly_attendance~.,
    data = nfl_train)


# Results

lm_res <- lm_fit %>% 
    predict(nfl_train) %>% 
    mutate(actual = nfl_train$weekly_attendance,
           data = "train") %>% 
    bind_rows(lm_fit %>% 
                  predict(nfl_test) %>% 
                  mutate(actual = nfl_test$weekly_attendance,
                         data = "test"))

rf_res <- rf_fit %>% 
    predict(nfl_train) %>% 
    mutate(actual = nfl_train$weekly_attendance,
           data = "train") %>% 
    bind_rows(rf_fit %>% 
                  predict(nfl_test) %>% 
                  mutate(actual = nfl_test$weekly_attendance,
                         data = "test"))
final_res <- lm_res %>% 
    mutate(algo = "lm") %>% 
    bind_rows(rf_res %>% 
                  mutate(algo = "rf"))


# Lets examine the results
final_res %>% 
    group_by(algo, data) %>% 
    rmse(actual, .pred)

# Lets visualize the results
final_res %>% 
    ggplot(aes(actual, .pred, color = algo)) +
    geom_point() +
    facet_wrap(~data)



# Let try again with resampling

# Resamples
nfl_cv <- nfl_train %>% 
    vfold_cv(strata = playoffs)

# Lets fit the model again

# lm fit with resamples

lm_fit_cv <- fit_resamples(lm_spec,
              weekly_attendance~.,
              resamples = nfl_cv,
              metrics = metric_set(rmse, rsq),
              control = control_resamples(save_pred = T))


# rf fit with resamples

rf_fit_cv <- fit_resamples(rf_spec,
                           weekly_attendance~.,
                           resamples = nfl_cv,
                           metrics = metric_set(rmse, rsq),
                           control = control_resamples(save_pred = T))


# Lets examine the results
lm_fit_cv %>% 
    collect_metrics()

rf_fit_cv %>% 
    collect_metrics()

# End of Script
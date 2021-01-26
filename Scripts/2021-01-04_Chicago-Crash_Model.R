# Pre requisites
library(tidyverse)
library(here)
library(lubridate)
library(RSocrata)
library(tidymodels)
library(baguette)
library(themis)

theme_set(theme_light())

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
#crash_raw <- as_tibble(read.socrata(crash_url))

# crash_raw %>% 
#     write_rds(here("data", "chicago_crash_raw.rds"))

crash_raw  <- read_rds(here("data", "chicago_crash_raw.rds"))

crash_raw %>% 
    skimr::skim()

crash <- crash_raw %>%
    transmute(
        injuries = if_else(injuries_total > 0, "injuries", "none"),
        crash_date,
        crash_hour,
        report_type = if_else(report_type == "", "Unknown", report_type),
        num_units,
        posted_speed_limit,
        weather_condition,
        roadway_surface_cond,
        first_crash_type,
        trafficway_type,
        prim_contributory_cause,
        latitude,
        longitude
    ) %>%
    drop_na() %>% 
    mutate_if(is_character, factor)

# EDA
crash %>% 
    mutate(crash_date = floor_date(crash_date, unit = "week")) %>% 
    count(crash_date, injuries) %>% 
    filter(crash_date != first(crash_date),
           crash_date != last(crash_date)) %>% 
    ggplot(aes(crash_date, n, color = injuries)) +
    geom_line(size = 1) +
    labs(title = "Car Accidents pattern across weeks from 2018 till date", 
         x = "Week of Accident",
         y = "",
         color = "") +
    expand_limits(y = 0)

crash %>% 
    mutate(crash_date = floor_date(crash_date, unit = "week")) %>% 
    count(crash_date, injuries) %>% 
    filter(crash_date != first(crash_date),
           crash_date != last(crash_date)) %>% 
    group_by(crash_date) %>% 
    mutate(percent_injury = n/sum(n)) %>%
    ungroup() %>% 
    filter(injuries == "injuries") %>% 
    ggplot(aes(crash_date, percent_injury)) +
    geom_line(size = 2, color = "sky blue") +
    expand_limits(y = 0) +
    scale_y_continuous(labels = percent)

crash %>% 
    mutate(crash_day = wday(crash_date, label = T)) %>% 
    count(crash_day, injuries) %>% 
    ggplot(aes(crash_day, n, color = injuries, group = injuries)) +
    geom_line()

# Data Split
set.seed(2021)
crash_split <- crash %>%
    initial_split(strata = injuries)

crash_train <- training(crash_split)
crash_test <- testing(crash_split)

set.seed(123)
crash_folds <- crash_train %>% 
    vfold_cv(strata = injuries)

# Recipe
crash_rec <- crash_train %>% 
    recipe(injuries~.) %>% 
    step_date(crash_date) %>% 
    step_rm(crash_date) %>% 
    step_other(weather_condition,
               first_crash_type,
               trafficway_type,
               prim_contributory_cause,
               other = "OTHER") %>% 
    step_downsample(injuries)


# Model Spec
bag_spec <- bag_tree(min_n = 10) %>% 
    set_engine("rpart", times = 25) %>% 
    set_mode("classification")


# Workflow

crash_wf <- workflow() %>% 
    add_recipe(crash_rec) %>% 
    add_model(bag_spec)


# Fit resamples
crash_fit <- fit_resamples(crash_wf,
                           resamples = crash_folds)


# Results
crash_fit %>% 
    collect_metrics()


# Save Model and predictions
crash_model <- butcher::butcher(crash_fit$.workflow[[1]])

lobstr::obj_size(crash_model)


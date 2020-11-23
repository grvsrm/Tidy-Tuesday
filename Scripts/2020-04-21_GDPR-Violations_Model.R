# Prerrequisites

library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
theme_set(theme_light())

# Load the data
gdpr_violations <- read_rds(here("data", "gdpr_violations.rds"))



# Modelling

# Data Preparation
gdpr_tidy <- gdpr_violations %>% 
    transmute(id, price, country,
              article_violated,
              articles = str_extract_all(article_violated, "Art. [:digit:]+|Art.[:digit:]+")) %>% 
    mutate(total_articles = map_int(articles, length)) %>% 
    unnest(articles) %>% 
    add_count(articles) %>% 
    filter(n>10) %>% 
    select(-n)

gdpr_df <- gdpr_tidy %>% 
    mutate(value = 1) %>% 
    select(-article_violated) %>% 
    pivot_wider(names_from = articles, values_from = value, values_fn = list(value = max), values_fill = 0) %>% 
    clean_names() %>% 
    mutate_if(is.character, factor)

# Recipe

recipe(price~., data = gdpr_df) %>% 
    update_role(id, new_role = "id") %>% 
    step_log(price, offset = 1) %>% 
    step_other(country, other = "Other")
    



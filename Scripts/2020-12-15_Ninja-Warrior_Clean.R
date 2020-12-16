# Pre requisites
library(tidyverse)
library(here)
library(janitor)


# Data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv') %>% 
    write_rds(here("data", "ninja warrior_raw.rds"))



# Clean the data

ninja_cleaned <- read_rds(here("data", "ninja warrior_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    mutate(obstacle_name = str_replace(obstacle_name, "\\(Modified\\) ", ""),
           round_stage = str_remove(round_stage, " \\(Regional/City\\)"))

ninja_cleaned %>% 
    write_rds(here("data", "ninja_warrior_cleaned.rds"))

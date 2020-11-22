# Prerequisites ----
library(tidyverse)
library(here)
library(janitor)

# Download ----
tt <- tidytuesdayR::tt_load("2020-08-18")


# Save raw ----
tt$plants %>% 
    write_rds(here("data", "plants_raw.rds"))

tt$threats %>% 
    write_rds(here("data", "threats_raw.rds"))

tt$actions %>% 
    write_rds(here("data", "actions_raw.rds"))


# Cleaning ----
plants <- read_rds(here("data", "plants_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    mutate(year_last_seen = fct_relevel(year_last_seen, "Before 1900"))
    
threats <- read_rds(here("data", "threats_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))

actions <- read_rds(here("data", "actions_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))


# Save final ----
plants %>% 
    write_rds(here("data", "plants.rds"))

threats %>% 
    write_rds(here("data", "threats.rds"))

actions %>% 
    write_rds(here("data", "actions.rds"))



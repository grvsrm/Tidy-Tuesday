# Prerequisites ----
library(tidyverse)
library(here)
library(janitor)
library(rvest)


# Download ----
tt <- tidytuesdayR::tt_load("2020-08-18")

# Web scraping 
links <- read_html("http://www.orchidspecies.com/indexbulb.htm") %>% 
    html_nodes("li a") %>% 
    html_text() %>% 
    as_tibble()

links_df <- links %>% 
    mutate(value = str_trim(value)) %>% 
    separate(value, c("genus", "species", "rest"), sep = " ", remove = T, extra = "merge") %>% 
    filter(!is.na(rest))


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
    mutate(year_last_seen = fct_relevel(year_last_seen, "Before 1900")) %>% 
    separate(binomial_name, c("genus", "species"), sep = " ", remove = F)
    
threats <- read_rds(here("data", "threats_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    filter(threatened == 1) %>% 
    mutate(year_last_seen = fct_relevel(year_last_seen, "Before 1900")) %>% 
    select(-threatened) %>% 
    separate(binomial_name, c("genus", "species"), sep = " ", remove = F)


actions <- read_rds(here("data", "actions_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    filter(action_taken == 1) %>% 
    mutate(year_last_seen = fct_relevel(year_last_seen, "Before 1900")) %>% 
    select(-action_taken) %>% 
    separate(binomial_name, c("genus", "species"), sep = " ", remove = F) %>% 
    filter(!action_type == "Unknown")
    




# Save final ----
plants %>% 
    write_rds(here("data", "plants.rds"))

threats %>% 
    write_rds(here("data", "threats.rds"))

actions %>% 
    write_rds(here("data", "actions.rds"))



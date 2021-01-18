# Pre requisites
library(readr)
library(janitor)
library(here)
library(dplyr)


# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv') %>% 
    write_rds(here("data", "transit-cost-raw.rds"))


# Read and clean the data
read_rds(here("data", "transit-cost-raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    filter(!is.na(e)) %>%
    rename("ID" = e,
           "iso2c" = country) %>%
    mutate(country_name = countrycode::countrycode(iso2c, "iso2c", "iso.name.en"),
           country_name = case_when(is.na(country_name) ~ "United Kingdom",
                                    TRUE ~ country_name)) %>% 
    write_rds(here("data", "transit-cost-clean.rds"))





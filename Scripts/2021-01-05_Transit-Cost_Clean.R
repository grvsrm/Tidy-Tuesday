# Pre requisites
library(readr)
library(janitor)
library(here)
library(dplyr)
library(lubridate)

# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv') %>% 
    write_rds(here("data", "transit-cost-raw.rds"))


# Read and clean and save the data
read_rds(here("data", "transit-cost-raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    filter(!is.na(e)) %>%
    rename("ID" = e,
           "iso2c" = country) %>%
    mutate(country_name = countrycode::countrycode(iso2c, "iso2c", "country.name"),
           country_name = case_when(is.na(country_name) ~ "United Kingdom",
                                    TRUE ~ country_name),
           continent = countrycode::countrycode(country_name, "country.name", "continent"),
           tunnel_per = tunnel/length) %>%
    mutate_at(vars(start_year, end_year, real_cost), as.numeric) %>% 
    write_rds(here("data", "transit-cost-clean.rds"))






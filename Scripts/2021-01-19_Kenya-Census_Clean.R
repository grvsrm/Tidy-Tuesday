# Pre requisites
library(tidyverse)
library(here)
library(janitor)

# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv') %>% 
    write_rds(here("data", "gender-raw.rds"))
    

read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv') %>% 
    write_rds(here("data", "crops-raw.rds"))


read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv') %>% 
    write_rds(here("data", "household-raw.rds"))


# Clean and Save the data

gender <- read_rds(here("data", "gender-raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    mutate(county = str_replace(county, "Total", "Kenya"))

crop <- read_rds(here("data", "crops-raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    pivot_longer(farming:khat_miraa, names_to = "type", values_to = "yield", values_drop_na = T) %>% 
    mutate(sub_county = str_to_title(sub_county)) %>% 
    rename("county" = sub_county)

household <- read_rds(here("data", "household-raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    mutate(county = case_when(county == "TanaRiver" ~ "Tana River",
                              county == "WestPokot" ~ "West Pokot",
                              county == "TransNzoia" ~ "Trans Nzoia",
                              county == "UasinGishu" ~ "Uasin Gishu",
                              county == "NairobiCity" ~ "Nairobi City",
                              county == "HomaBay" ~ "Homa Bay",
                              TRUE ~ county))

household %>% 
    left_join(gender) %>% 
    left_join(crop) %>% 
    write_rds(here("data", "kenya-clean.rds"))

# End of script
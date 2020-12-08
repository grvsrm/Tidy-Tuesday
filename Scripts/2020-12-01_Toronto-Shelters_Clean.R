# Pre requisites
library(tidyverse)
library(janitor)
library(here)
library(tidyimpute)


# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv') %>% 
    write_rds(here("data", "toronto_shelter_raw.rds"))


read_rds(here("data", "toronto_shelter_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    remove_constant() %>% 
    tidyimpute::impute_median(capacity) %>% 
    write_rds(here("data", "toronto_shelter.rds"))


# End of script


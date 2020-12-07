# Pre-requisites
library(tidyverse)
library(janitor)
library(here)


read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv') %>% 
    write_rds(here("data", "astronauts_raw.rds"))

astronauts_raw <- read_rds(here("data", "astronauts_raw.rds"))

astronauts <- astronauts_raw %>% 
    clean_names() %>% 
    remove_empty() %>% 
    mutate_if(is.character, factor)

astronauts %>% 
    write_rds(here("data", "astronauts.rds"))


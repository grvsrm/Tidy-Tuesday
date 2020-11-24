# Pre-requisites

library(tidyverse)
library(here)
library(janitor)


# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
    write_rds(here("data", "penguins_raw.rds"))


# Clean

penguins <- read_rds(here("data", "penguins_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))


penguins



# Save the clean data
penguins %>% 
    write_rds(here("data", "penguins.rds"))

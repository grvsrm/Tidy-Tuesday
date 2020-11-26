# Prerequisites
library(tidyverse)
library(here)

# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv') %>% 
    write_rds(here("data", "kids_raw.rds"))

# Clean the data

kids <- read_rds(here("data", "kids_raw.rds"))


# Save the data

kids %>%
    write_rds(here("data", "kids.rds"))

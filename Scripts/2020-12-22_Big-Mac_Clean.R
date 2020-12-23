# Pre requisites
library(tidyverse)
library(janitor)
library(here)
library(lubridate)

# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') %>% 
    write_rds(here("Data", "bigmac_raw.rds"))



# Clean the data
bigmac_clean <- read_rds(here("data", "bigmac_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    rename("country" = name)

# Save the clean data

bigmac_clean %>% 
    write_rds(here("data", "bigmac_cleaned.rds"))

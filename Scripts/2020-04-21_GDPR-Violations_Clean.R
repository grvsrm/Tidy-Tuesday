# Prerequisites
library(tidyverse)
library(janitor)
library(here)
library(lubridate)

# Get the Data

read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv') %>% 
    write_rds(here("data", "gdpr_violations_raw.rds"))

read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv') %>% 
    write_rds(here("data", "gdpr_text_raw.rds"))


# Clean the data
gdpr_violation <- read_rds(here("data", "gdpr_violations_raw.rds")) %>% 
    mutate(date = mdy(date),
           date = na_if(date, "1970-01-01")) %>% 
    rename("country" = name)






# Save the data

gdpr_violation %>% 
    write_rds(here("data", "gdpr_violations.rds"))

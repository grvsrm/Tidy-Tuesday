# Prerequisites
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# Download the data

# read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv") %>%
#     write_rds(here("data", "big_epa_cars_raw.rds"))



# Clean the data

big_epa_cars_cleaned <- read_rds(here("data", "big_epa_cars_raw.rds")) %>% 
    mutate(uses_electricity = ifelse(highwayE>0, "uses_electricity", "doesn't use electricity")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    mutate(charger = paste(t_charger, s_charger),
           charger = case_when(charger == "TRUE S" ~ "TS",
                               charger == "NA S" ~ "S",
                               charger == "TRUE NA" ~ "T",
                               TRUE ~ "No Charger")) %>% 
    select(-t_charger, -s_charger) %>% 
    mutate_if(is.character, factor)


big_epa_cars_cleaned

# Save the clean data

big_epa_cars_cleaned %>% 
    write_rds(here("data", "big_epa_cars.rds"))


# End of Script
# Prerequisites
library(tidytuesdayR)
library(tidyverse)
library(here)


# Download and save the data
tt <- tt_load("2020-11-10")

tt$mobile %>% 
    write_rds(here("data", "mobile_raw.rds"))

tt$landline %>% 
    write_rds(here("data", "landline_raw.rds"))


mobile <- read_rds(here("data", "mobile_raw.rds")) %>% 
    rename(subscriptions = mobile_subs) %>% 
    mutate(type = "mobile")
landline <- read_rds(here("data", "landline_raw.rds")) %>% 
    rename(subscriptions = landline_subs) %>% 
    mutate(type = "landline")

phones <- mobile %>% 
    bind_rows(landline) %>% 
    rename(country = entity)

phones %>% 
    write_rds(here("data", "phones.rds"))
    




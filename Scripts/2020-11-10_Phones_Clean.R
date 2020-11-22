# Prerequisites
library(tidytuesdayR)
library(tidyverse)
library(here)
library(WDI)
library(janitor)

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

country_incomes <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", 
                                     "SP.POP.TOTL"),
                       start = 2005, end = 2005, extra = T) %>% 
    as_tibble() %>% 
    select(code = iso3c, income, gdp = NY.GDP.PCAP.PP.KD, pop = SP.POP.TOTL) %>% 
    filter(!is.na(income))

phones <- mobile %>% 
    bind_rows(landline) %>% 
    rename(country = entity) %>% 
    inner_join(country_incomes) %>% 
    mutate(income = fct_relevel(income, "Low income", "Lower middle income", "Upper middle income", "High income")) %>% 
    clean_names() %>% 
    remove_empty() %>% 
    distinct()


phones %>% 
    write_rds(here("data", "phones.rds"))
    
# End of script
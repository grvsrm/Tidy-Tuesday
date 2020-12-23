# Pre requisites
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
theme_set(theme_light())

# Download the data
bigmac <- read_rds(here("Data", "bigmac_cleaned.rds"))

# Lets see how local prices and dollar prices have moved over time for Big Mac in various countries 

bigmac %>% 
    select(country, date, local_price, dollar_price) %>% 
    pivot_longer(3:4, names_to = "price", values_to = "value") %>% 
    ggplot(aes(date, value, color = price)) +
    geom_line(size = 1) +
    facet_wrap(~country, scales = "free", ncol = 9) +
    theme_void()


# Prerequisites
library(tidyverse)



# Load the data
phones <- read_rds(here("data", "phones.rds"))


# Exploration
phones %>% 
    filter(country == "India",
           !is.na(subscriptions)) %>% 
    ggplot(aes(year, subscriptions, color = type)) +
    geom_line() +
    labs(title = "Phone Subscriptions over years",
         color = "",
         x = "",
         y = "Subscriptions per 100 people")

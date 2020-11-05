# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)

# Load the data

ikea_cleaned <- read_rds(here("data" ,"ikea_cleaned.rds"))

ikea_cleaned %>% 
    select(item_id, category, price_usd, sellable_online, other_colors, designer, category_total) %>% 
    nest(data = c(item_id, price_usd, sellable_online, other_colors, designer, 
                  category_total)) %>% 
    mutate(model = map(data, ~lm(price_usd~sellable_online + other_colors + designer + category_total, data = .)),
           tidied = map(model, tidy)) %>% 
    unnest(tidied)


 


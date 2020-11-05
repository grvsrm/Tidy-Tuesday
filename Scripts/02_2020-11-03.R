# Load the libraries
library(tidyverse)
library(tidymodels)
library(here)

# Load the data

ikea_cleaned <- read_rds(here("data" ,"ikea_cleaned.rds"))

ikea_cleaned %>% 
    select(item_id, category, price_usd, sellable_online, other_colors, designer, category_total) %>% 
    nest(-category) %>% 
    map()

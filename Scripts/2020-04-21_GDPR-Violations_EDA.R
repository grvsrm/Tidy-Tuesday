# Prerequisite
library(tidyverse)
library(here)
library(scales)

theme_set(theme_light())

# Load the data

gdpr_violation <- read_rds(here("data", "gdpr_violations.rds"))

# Explore ----
gdpr_violation %>% 
    group_by(country) %>% 
    summarise(total = sum(price)) %>% 
    arrange(desc(total))

# Another way of doing the same thing
gdpr_violation %>% 
    count(country = fct_lump(country,10, w = price), wt = price , sort = T, name = "total_fine") %>% 
    mutate(country = fct_reorder(country, total_fine)) %>% 
    ggplot(aes(total_fine, country)) +
    geom_col() +
    scale_x_continuous(labels = dollar_format())



# TidyTeesday Data Exploration
# Author - Gaurav S

# Load the essential libraries
library(tidyverse)
library(scales)
theme_set(theme_light())
library(ggthemes)
library(glue)


## Download the dataset
tt_data <- tidytuesdayR::tt_load(tidytuesdayR::last_tuesday())

tt_data$ikea %>% 
    write_rds("Data/ikea_raw.rds")

## Load the dataset
ikea <- read_rds("Data/ikea_raw.rds")

ikea_cleaned <- ikea %>% 
    select(-X1) %>% 
    mutate(category = str_to_title(category))

### Tidy Exploration starts here..

ikea_cleaned %>% 
    count(category, sort = T) %>% 
    mutate(category = fct_reorder(category, n)) %>% 
    ggplot(aes(n, category)) +
    geom_col() +
    labs(title = "Number of Items per each Category",
         x = "",
         y= "")

ikea_cleaned %>% 
    add_count(category, name = "category_total") %>% 
    mutate(category = glue("{category} ({category_total})"),
           category = fct_reorder(category, price)) %>% 
    ggplot(aes(price, category, color = category)) +
    geom_boxplot(show.legend = F) +
    geom_jitter(width = 0, height = 0.2, alpha = 0.4, show.legend = F) +
    theme_classic() +
    scale_x_log10()



ikea_cleaned %>%
    ggplot(aes(price, fill = category)) +
    geom_histogram() +
    facet_wrap( ~ category, scales = "free") +
    theme(legend.position = "none") +
    scale_x_log10()


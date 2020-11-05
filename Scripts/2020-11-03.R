# TidyTeesday Data Exploration
# Author - Gaurav S

# Load the essential libraries
library(tidyverse)
library(scales)
theme_set(theme_light())
library(ggthemes)
library(glue)
library(ggridges)


## Download the dataset
tt_data <- tidytuesdayR::tt_load(tidytuesdayR::last_tuesday())

tt_data$ikea %>% 
    write_rds("Data/ikea_raw.rds")

## Load the dataset
ikea <- read_rds("Data/ikea_raw.rds")

## Clean the data (If reqd)
ikea_cleaned <- ikea %>% 
    select(-X1) %>% 
    mutate(category = str_to_title(category),
           price_usd = 0.27 * price,
           short_description = str_trim(str_replace_all(short_description, "\\s+", ""))) %>% 
    add_count(category, name = "category_total")

ikea_cleaned %>% 
    write_rds("Data/ikea_cleaned.rds")

### Tidy Exploration starts here..

ikea_cleaned %>% 
    count(category, sort = T) %>% 
    mutate(category = fct_reorder(category, n)) %>% 
    ggplot(aes(n, category)) +
    geom_col() +
    labs(title = "Number of Items per each Category",
         x = "",
         y= "")


##################################################################


ikea_cleaned %>%
    mutate(
        category = glue("{category} ({category_total})"),
        category = fct_reorder(category, price_usd)
    ) %>%
    ggplot(aes(price_usd, category, color = category)) +
    geom_boxplot(show.legend = F) +
    geom_jitter(
        width = 0,
        height = 0.2,
        alpha = 0.4,
        show.legend = F
    ) +
    theme_classic() +
    scale_x_log10(label = dollar_format()) +
    labs(title = "How much do items in each category cost ?",
         x = "Price(USD)",
         y = "")


###################################################################


ikea_cleaned %>%
    ggplot(aes(price_usd, fill = category)) +
    geom_histogram() +
    facet_wrap(~ category, scales = "free") +
    theme(legend.position = "none") +
    scale_x_log10(labels = dollar_format()) +
    labs(title = "How much do items in each category cost ?",
         x = "Price(USD)",
         y = "")


###################################################################


ikea_cleaned %>%
    mutate(
        category = glue("{category} ({category_total})"),
        category = fct_reorder(category, price_usd)
    ) %>%
    ggplot(aes(price_usd, category)) +
    geom_density_ridges() +
    scale_x_log10(label = dollar_format()) +
    labs(title = "How much do items in each category cost ?",
         x = "Price(USD)",
         y = "")

##################################################################

ikea_cleaned %>% 
    mutate(name = fct_lump(name, 20)) %>% 
    filter(!name == "Other") %>% 
    count(name, category, sort = T) %>% 
    mutate(name = fct_reorder(name, n, sum),
           category = fct_reorder(category, n, sum)) %>% 
    ggplot(aes(n, name, fill = category)) +
    geom_col() +
    scale_fill_discrete(guide = guide_legend(reverse = T))

##################################################################

ikea_volume <- ikea_cleaned %>% 
    select(item_id, name, short_description, category, price_usd, depth, width, height) %>% 
    mutate(volume_m3 = depth * width * height/1e6) %>% 
    filter(!is.na(volume_m3),
           volume_m3 >= 0.01) %>% 
    arrange(desc(volume_m3)) %>% 
    add_count(category, name = "category_total")


ikea_volume%>% 
    mutate(
        category = glue("{category} ({category_total})"),
        category = fct_reorder(category, volume_m3)
    ) %>%
    ggplot(aes(volume_m3, category)) +
    geom_boxplot() +
    scale_x_log10() +
    labs(title = "How much volume do items have in each category?",
         x = "Volume (in cubic metrers)",
         y = "")
    
    
##################################################################

ikea_volume %>% 
    mutate(category = fct_lump(category, 6)) %>% 
    ggplot(aes(volume_m3, price_usd, color = category)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    geom_smooth(se = F)

##################################################################
ikea %>% 
    group_by(designer) %>% 
    summarise(n_items = n(),
              n_names = n_distinct(name),
              n_categoties = n_distinct(category)) %>% 
    arrange(desc(n_names))

##################################################################

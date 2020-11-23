# Prerequisite
library(tidyverse)
library(here)
library(scales)

theme_set(theme_light())

# Load the data

gdpr_violation <- read_rds(here("data", "gdpr_violations.rds"))
gdpr_text <- read_rds(here("data", "gdpr_text.rds"))


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
    scale_x_continuous(labels = dollar_format()) +
    labs(title = "Which countries have paid most fines?",
         y = "",
         x = "",
         caption = "Data Source: R4DS")


gdpr_violation %>% 
    count(country = fct_lump(country, 6, w= price), month = floor_date(date, "month"), wt = price, name = "total_fine") %>% 
    mutate(country = fct_reorder(country, -total_fine, sum )) %>% 
    ggplot(aes(month, total_fine, fill = country)) +
    geom_col()

separated_articles <- gdpr_violation %>% 
    separate_rows(article_violated, sep = "\\|") %>% 
    extract(article_violated, "article_number", "Art\\. ?(\\d+)", remove = F)


separated_articles %>% 
    add_count(id) %>% 
    mutate(price_per_article = price/n) %>%
    group_by(article_number = fct_lump(article_number, 8, w = price)) %>% 
    summarise(total_fine = sum(price_per_article),
              violations = n()) %>% 
    arrange(desc(total_fine))
 
gdpr_violation %>% 
    mutate(type = fct_lump(type, 8, w = price),
           type = fct_reorder(type, price),
           country = fct_lump(country, 5)) %>% 
    ggplot(aes(price, type)) +
    geom_boxplot() +
    geom_jitter(aes(color = country)) +
    scale_x_log10(labels = dollar_format())


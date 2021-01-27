# Pre requisites
library(tidyverse)
library(here)
library(scales)
library(janitor)
library(ggrepel)

theme_set(theme_light())


# Load the data

kenya_census <- read_rds(here("data", "kenya-clean.rds"))
gender <- read_rds(here("data", "gender-raw.rds"))
crops <- read_rds(here("data", "crops-raw.rds"))
household <- read_rds(here("data", "household-raw.rds"))

# EDA


gender %>% 
    clean_names() %>% 
    filter(county != "Total") %>% 
    mutate(pct_male = male/total) %>% 
    ggplot(aes(total, pct_male)) +
    geom_point() +
    geom_text_repel(aes(label = county)) +
    geom_hline(yintercept = 0.5, lty = 2, color = "red") +
    expand_limits(y=c(0.4,0.6)) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = percent)


gender %>% 
    clean_names() %>% 
    filter(county != "Total") %>% 
    pivot_longer(male:intersex, names_to = "gender", values_to = "population") %>% 
    mutate(county = fct_reorder(county, population, sum)) %>% 
    ggplot(aes(population, county, fill = gender)) +
    geom_col() +
    scale_x_continuous(labels = comma)
    

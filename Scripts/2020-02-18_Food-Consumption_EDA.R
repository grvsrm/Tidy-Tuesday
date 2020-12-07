# Pre-requisites
library(tidyverse)
library(here)
library(janitor)
library(scales)
library(tidytext)
library(patchwork)

theme_set(theme_light())

# Load tha data
food_consumption <- read_rds(here("data", "food_consumption_raw.rds"))


# EDA
food_consumption %>% 
    count(country = fct_lump(country, 10, w = co2_emmission), wt = co2_emmission, name = "co2", sort = T) %>% 
    filter(country != "Other") %>% 
    mutate(country = fct_reorder(country, co2)) %>% 
    ggplot(aes(co2, country)) +
    geom_col(alpha = 0.7) +
    labs(title = "Which countries have the highest co2 emmission ?",
         subtitle = "Top 10 countries are displayed",
         y = "",
         x = "Co2 Emission (Kg CO2/person/year)")


food_consumption %>% 
    group_by(food_category, country) %>% 
    summarise(consumption = sum(consumption)) %>% 
    top_n(10) %>% 
    ungroup() %>% 
    mutate(country = reorder_within(country, consumption, food_category)) %>% 
    ggplot(aes(consumption, country, fill = food_category)) +
    geom_col() +
    scale_y_reordered() +
    facet_wrap(~food_category, scales = "free") +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Paired") +
    labs(title = "How countries consume various types of food ?",
         subtitle = "Top 10 countries are displayed",
         y = "",
         x = "Consumption (kg/person/year)")

food_consumption %>% 
    group_by(food_category, country) %>% 
    summarise(co2 = sum(co2_emmission)) %>% 
    top_n(10) %>% 
    ungroup() %>% 
    mutate(country = reorder_within(country, co2, food_category)) %>% 
    ggplot(aes(co2, country, fill = food_category)) +
    geom_col() +
    scale_y_reordered() +
    facet_wrap(~food_category, scales = "free") +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Paired") +
    labs(title = "How countries consume various types of food ?",
         subtitle = "Top 10 countries are displayed",
         y = "",
         x = "Consumption (kg/person/year)")

#RColorBrewer::display.brewer.all()

food_consumption %>% 
    ggplot(aes(consumption, co2_emmission)) +
    geom_point(aes(color = food_category)) +
    facet_wrap(~food_category, scales = "free") +
    theme(legend.position = "none")

# Seems co2 emmission is calculated based on some formula using food consumption. 
# Because we see a high correlation between these two quantities


food_consumption %>% 
    filter(country == "USA") %>% 
    mutate(food_category = fct_reorder(food_category, co2_emmission)) %>% 
    ggplot(aes(co2_emmission, food_category)) +
    geom_col() +
    labs(title = "How does USA consume various types of food ?",
         y = "",
         x = "Consumption (kg/person/year)")

food_consumption %>% 
    filter(country == "India") %>% 
    mutate(food_category = fct_reorder(food_category, co2_emmission)) %>% 
    ggplot(aes(co2_emmission, food_category)) +
    geom_col() +
    labs(title = "How does USA consume various types of food ?",
         y = "",
         x = "Consumption (kg/person/year)")

food_consumption %>% 
    ggplot(aes(x = fct_reorder(food_category, consumption), y = consumption, color = country)) +
    geom_jitter() +
    theme(legend.position = "none") +
    coord_flip() +
    labs(title = "What is the most consumed food in the world?",
         x = "",
         y = "Consumption (kg/person/year)") +
    scale_y_continuous(expand = c(0,0))

# Impact of Beef Eating

p1 <- food_consumption %>% 
    ggplot(aes(consumption, co2_emmission)) +
    geom_point(aes(color = food_category)) +
    labs(title = "How various food categories relate with carbon footprint?",
         x = "Consumption (kg/person/year)",
         y = "Co2 Emission (Kg CO2/person/year)",
         color = "")

p2 <- food_consumption %>% 
    count(food_category, wt = mean(co2_emmission), name = "mean_co2_emit") %>% 
    mutate(food_category = fct_reorder(food_category, mean_co2_emit)) %>% 
    ggplot(aes(mean_co2_emit, food_category)) +
    geom_point(aes(color = food_category)) +
    geom_col(aes(color = food_category), width = 0.05) +
    theme(legend.position = "none") +
    labs(title = "Which food category has the highest carbon footprint?",
         x = "Mean Co2 Emission (Kg CO2/person/year)",
         y = "")

(p1)/(p2)

# This visualization clearly suggests why Beef eating should be strongly discouraged.

# End of script

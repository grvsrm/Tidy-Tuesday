# Pre requisites
library(tidyverse)
library(here)
library(lubridate)
library(ggrepel)
library(gganimate)
library(widyr)

theme_set(theme_light())

# Download the data
bigmac <- read_rds(here("Data", "bigmac_cleaned.rds"))

# Lets see how local prices and dollar prices have moved over time for Big Mac in various countries 

bigmac %>% 
    filter(!is.na(gdp_dollar)) %>% 
    ggplot(aes(date, local_price, color = country)) +
    geom_line(show.legend = F)

bigmac %>% 
    select(country, date, local_price, dollar_price) %>% 
    pivot_longer(3:4, names_to = "price", values_to = "value") %>% 
    ggplot(aes(date, value, color = price)) +
    geom_line(size = 1) +
    facet_wrap(~country, scales = "free", ncol = 9) +
    theme_void()

bigmac %>% 
    mutate(country = fct_reorder(country, local_price, function(.) max(.)/min(.))) %>% 
    ggplot(aes(date, local_price, color = country)) +
    geom_line(show.legend = F, size = 1) +
    facet_wrap(~country, scales = "free_y") +
    labs(x = "time",
         y = "Price of Big Mac in local currency")


bigmac %>% 
    filter(country_total == max(country_total)) %>% 
    group_by(country) %>% 
    summarise(big_mac_inflation = last(local_price)/first(local_price)) %>% 
    mutate(country = fct_reorder(country, big_mac_inflation)) %>% 
    ggplot(aes(big_mac_inflation, country)) +
    geom_col() +
    geom_text(aes(label = paste0(round(big_mac_inflation, 1),"x")), hjust = -0.3, color = "gray40") +
    scale_x_log10(breaks =  c(1,3,10,30,100)) +
    labs(y = "",
         x = "Change in first and last price of Big Mac" )

bigmac %>% 
    filter(country_total == max(country_total)) %>% 
    select(date, country, local_price, dollar_ex, usd_raw, gdp_dollar, usd_adjusted) %>% 
    filter(!is.na(gdp_dollar)) %>% 
    ggplot(aes(date, usd_raw)) +
    geom_line(size = 1) +
    expand_limits(y = 0) +
    facet_wrap(~country) +
    theme_void()


bigmac %>% 
    filter(country_total == max(country_total),
           date == max(date)) %>% 
    ggplot(aes(gdp_dollar, usd_raw)) +
    geom_point(size = 1) +
    geom_text_repel(aes(label = country)) +
    geom_smooth(method = "lm") +
    labs(x = "GDP per capita (dollars)",
         y = "Raw Big Mac index relative to USD")


bigmac %>% 
    filter(date == max(date),
           !is.na(usd_adjusted)) %>% 
    mutate(country = fct_reorder(country, usd_adjusted)) %>% 
    ggplot(aes(usd_adjusted, country)) +
    geom_col() +
    labs(x = "Big Mac Index (GDP Adjusted)",
         y = "")


bigmac %>% 
    filter(country_total == max(country_total)) %>% 
    select(date, country, local_price, dollar_ex, usd_raw, gdp_dollar, usd_adjusted) %>% 
    filter(!is.na(gdp_dollar),
           country != "United States") %>% 
    mutate(country = fct_reorder(country, usd_raw)) %>% 
    ggplot(aes(date, usd_adjusted, color = country)) +
    geom_line(size = 1, show.legend = F) +
    geom_hline(yintercept = 0, lty = 3, color = "gray40", size = 1) +
    expand_limits(y = 0) +
    facet_wrap(~country) +
    labs(y = "Big Mac Index (GDP Adjusted)",
         x = "") +
    theme_minimal()


bigmac %>% 
    filter(country_total == max(country_total),
           !is.na(gdp_dollar)) %>% 
    ggplot(aes(gdp_dollar, usd_adjusted)) +
    geom_point(size = 1) +
    geom_text_repel(aes(label = country)) +
    geom_smooth(method = "lm") +
    transition_time(date) +
    # transition_manual(date) +
    labs(x = "GDP per capita (dollars)",
         y = "Raw Big Mac index relative to USD",
         title = "{ frame_time }")


bigmac %>% 
    filter(!is.na(gdp_dollar)) %>% 
    pairwise_cor(country, date, local_price, sort = T)

# End of Script    





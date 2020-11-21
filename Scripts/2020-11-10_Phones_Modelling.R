# Prerequisites
library(tidyverse)
library(here)
library(directlabels)
library(scales)
library(gganimate)

theme_set(theme_light())

# Load the data
phones <- read_rds(here("data", "phones.rds"))


# Exploration
phones %>% 
    filter(country == "India",
           !is.na(subscriptions)) %>% 
    ggplot(aes(year, subscriptions, color = type)) +
    geom_line() +
    scale_x_continuous(breaks = 1:30,
                       labels = seq(1990, 2019, 1)) +
    labs(title = "Phone Subscriptions over years",
         color = "",
         x = "",
         y = "Subscriptions per 100 people")


country_sizes <- phones %>% 
    group_by(country) %>% 
    summarise(avg_population = mean(total_pop, na.rm = T))

phones %>% 
    semi_join(country_sizes %>% 
                  top_n(40, avg_population)) %>% 
    filter(!is.na(subscriptions)) %>% 
    ggplot(aes(year, subscriptions, color = type, group = interaction(type, country))) +
    geom_line() +
    geom_dl(aes(label = country), method = "last.points") +
    facet_wrap(~continent) 


phones %>% 
    group_by(year, continent) %>% 
    summarise(subs = mean(subscriptions, na.rm = T)) %>% 
    ggplot(aes(year, subs, color = continent)) +
    geom_line() +
    geom_dl(aes(label = continent), method = "last.points") +
    theme(legend.position = "none")

phones %>% 
    semi_join(country_sizes %>% 
                  top_n(40, avg_population)) %>% 
    filter(!is.na(income)) %>% 
    ggplot(aes(year, subscriptions, color = type, group = interaction(type, country))) +
    geom_line() +
    geom_dl(aes(label = country), method = "last.points") +
    facet_wrap(~income) 


summarise_subscriptions = . %>% 
    filter(!is.na(subscriptions)) %>% 
    summarise(avg_subscriptions = mean(subscriptions, na.rm = T),
              median_subscriptions = median(subscriptions),
              p25 = quantile(subscriptions, .25),
              p75 = quantile(subscriptions, .75))

by_year_income <- phones %>% 
    filter(!is.na(income)) %>% 
    group_by(year, income, type) %>% 
    summarise_subscriptions()


by_year_income %>% 
    ggplot(aes(year, median_subscriptions, color = type)) +
    geom_line() +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.1) +
    facet_wrap(~income) +
    labs(title = "How do mobile and landline adoption differ in various income countries",
         subtitle = "Ribbon shows 25th and 75th percentile",
         y = "Median Subscriptions per person",
         x = "",
         color = "",
         caption = "Data Source: WDI API and R4DS")


by_year_income %>% 
    ggplot(aes(year, median_subscriptions, color = income)) +
    geom_line(size = 1) +
    facet_wrap(~type, nrow = 2) +
    labs(title = "How do mobile and landline adoption differ in various income countries",
         subtitle = "Ribbon shows 25th and 75th percentile",
         y = "Median Subscriptions per person",
         x = "",
         color = "",
         caption = "Data Source: WDI API and R4DS")    


countries_summarized <- mobile %>% 
    bind_rows(landline) %>% 
    rename(country = entity) %>% 
    filter(!is.na(subscriptions)) %>% 
    select(-total_pop, -gdp_per_cap) %>% 
    pivot_wider(names_from = "type",
                values_from = "subscriptions") %>% 
    group_by(continent, country, code) %>% 
    summarise(year_past_50_mobile = na_if(min(year[mobile >= 50]),Inf),
              peak_mobile = max(mobile),
              peak_landline = max(landline, na.rm = T),
              n_mobile = sum(!is.na(mobile))) %>% 
    inner_join(country_incomes, by = "code") %>% 
    filter(n_mobile > 25) %>% 
    arrange(desc(year_past_50_mobile))

countries_summarized %>% 
    ggplot(aes(year_past_50_mobile, gdp, color = continent)) +
    geom_point(aes(size = pop)) +
    scale_y_log10(labels = dollar) +
    labs(title = "Adoption of Mobile phones in different countries",
         subtitle = "By 50% of the population",
         x = "",
         y = "GDP per capita (In 2005)",
         caption = "Data Source : WDI & R4DS",
         size = "Population",
         color = "")

countries_summarized %>% 
    filter(!is.na(gdp)) %>% 
    ggplot(aes(gdp, peak_landline, color = continent)) +
    geom_point(aes(size = pop)) +
    scale_x_log10(labels = dollar) +
    facet_wrap(~continent) +
    theme(legend.position = "none")

#### World Data
world_map_mobile <- map_data("world") %>% 
    as_tibble() %>% 
    left_join(maps::iso3166 %>% as_tibble(), by = c(region = "mapname")) %>% 
    left_join(mobile, c(a3 = "code"))


world_map_mobile %>% 
    ggplot(aes(long, lat, group = group, fill = subscriptions)) +
    geom_polygon() +
    transition_manual(year) +
    coord_fixed(2) +
    scale_fill_gradient2(high = "red", low = "blue", midpoint = 30) +
    ggthemes::theme_map()



phones %>% 
    filter(year == 2015) %>% 
    top_n(9, subscriptions) %>% 
    mutate(country = fct_reorder(country, subscriptions)) %>% 
    ungroup() %>% 
    ggplot(aes(subscriptions, country, fill = country)) +
        geom_col() +
#    transition_time(year) +
    theme(legend.position = "none") +
    labs(title = "Year: {frame_time}")


#RColorBrewer::display.brewer.all()

# End of Script



library(janitor)

# Pre requisites
library(tidyverse)
library(here)
library(tidymodels)
library(janitor)
library(ggrepel)

theme_set(theme_light())

# Download the data
key_crop_yields <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv")

land_use <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv")


key_crop_yields


top_countries <- land_use %>% 
    clean_names() %>% 
    filter(!is.na(code)) %>% 
    group_by(entity) %>% 
    filter(year == max(year)) %>% 
    ungroup() %>% 
    slice_max(total_population_gapminder, n =30) %>% 
    pull(entity)


tidy_yields <- key_crop_yields %>% 
    clean_names() %>% 
    pivot_longer(wheat_tonnes_per_hectare: bananas_tonnes_per_hectare,
                 names_to = "crop",
                 values_to = "yield",
                 values_drop_na = T) %>% 
    mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>% 
    filter(crop %in% c("wheat", "maize", "barley", "rice"),
           entity %in% top_countries)


tidy_yields %>% 
    ggplot(aes(year, yield, color = crop)) +
    geom_line(size = 1) +
    facet_wrap(~entity) +
    theme_minimal() +
    labs(title = "Yield across various countries over years",
         x = "")


# Model
yield_res <- tidy_yields %>% 
    nest(data = c(year, yield)) %>% 
    mutate(model = map(data, ~lm(yield~year, data = .)),
           coef = map(model, tidy)) %>% 
    unnest(coef)


# Results
yield_res %>% 
    filter(term == "year",
           entity != "World") %>% 
    mutate(p.value = p.adjust(p.value)) %>% 
    ggplot(aes(estimate, p.value)) +
    geom_point(aes(color = crop), size = 2) +
    facet_wrap(~crop) +
    scale_y_log10() +
    geom_vline(xintercept = 0, lty = 2) +
    geom_text_repel(aes(label = entity)) +
    theme(legend.position = "none")
        

# End of script 
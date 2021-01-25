# Pre requisites
library(tidyverse)
library(here)
library(scales)

theme_set(theme_light())


# Load the data
artwork <- read_rds(here("data", "artwork_clean.rds")) %>% 
    filter(artist != "Turner, Joseph Mallord William")

artwork %>%
    count(artist, sort = T) %>% 
    mutate(artist = fct_reorder(artist, n),
           artist = fct_lump(artist, 20, w = n)) %>% 
    filter(artist != "Other") %>% 
    ggplot(aes(n, artist)) +
    geom_col() +
    labs(title = "Artists with most number of artworks (excluded Turner, Joseph Mallord William)",
         x ="",
         y="")


artwork %>% 
    mutate(area = width * height) %>% 
    ggplot(aes(area)) +
    geom_histogram() +
    scale_x_log10(labels = comma)


artwork %>% 
    ggplot(aes(width)) +
    geom_histogram() +
    scale_x_log10(labels = comma)

artwork %>% 
    ggplot(aes(height)) +
    geom_histogram() +
    scale_x_log10(labels = comma)

artwork %>% names()

# Pre requisistes
library(tidyverse)
library(here)
library(scales)
library(tidylo)
library(tidytext)


# Data
ninja_warrior <- read_rds(here("data", "ninja_warrior_cleaned.rds"))


ninja_warrior %>% 
    filter(round_stage %in% c("Qualifying", "Finals")) %>% 
    count(round_stage, obstacle_name) %>% 
    bind_log_odds(round_stage, obstacle_name, n) %>% 
    arrange(desc(log_odds_weighted)) %>% 
    filter(round_stage == "Finals") %>% 
    top_n(15, abs(log_odds_weighted)) %>% 
    mutate(obstacle_name = fct_reorder(obstacle_name, log_odds_weighted)) %>% 
    ggplot(aes(log_odds_weighted, obstacle_name)) +
    geom_col()


ninja_warrior %>% 
    filter(round_stage == "Qualifying") %>% 
    group_by(obstacle_name) %>% 
    summarise(avg_order = median(obstacle_order),
              n_rounds = n()) %>% 
    arrange(desc(avg_order))


ninja_warrior %>% 
    filter(round_stage %in% c("Qualifying", "Finals")) %>% 
    unite(season_location, season, location) %>% 
    group_by(round_stage) %>% 
    mutate(total_rounds = n_distinct(season_location)) %>% 
    group_by(round_stage, obstacle_name) %>% 
    summarise(avg_position = median(obstacle_order),
              n_rounds = n(),
              pct_rounds = n_rounds/first(total_rounds)) %>% 
    arrange(desc(n_rounds)) %>% 
    top_n(10, pct_rounds) %>% 
    ungroup() %>% 
    mutate(obstacle_name = reorder_within(obstacle_name, avg_position, round_stage)) %>% 
    ggplot(aes(avg_position, obstacle_name, size = pct_rounds)) +
    geom_point() +
    scale_y_reordered() +
    scale_x_continuous(breaks = 1:10) +
    facet_wrap(~round_stage, nrow = 2, scales = "free") +
    labs(title = "How often does various obstacles occur in qualifying and final rounds",
         x = "Average Position",
         y = "",
         size = "% of courses")


    


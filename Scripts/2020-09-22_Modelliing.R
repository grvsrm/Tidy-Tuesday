# Load the libraries
library(tidyverse)
library(scales)
library(here)
library(ebbr)
theme_set(theme_light())


# Load the data from r objects
peaks <- read_rds(here("data", "peaks_raw.rds"))
members <- read_rds(here("data", "members_raw.rds"))
expedition <- read_rds(here("data", "expedition_raw.rds"))

# Data Exploration

peaks %>% 
    ggplot(aes(height_metres)) +
    geom_histogram()

peaks %>% 
    arrange(desc(height_metres)) %>% 
    head(30) %>% 
    mutate(peak_name = fct_reorder(peak_name, height_metres)) %>% 
    ggplot(aes(height_metres, peak_name, color = climbing_status)) +
    geom_point() +
    labs(title = "Tallest Peaks in Himalayas",
         x = "Height in Meters",
         y = "",
         color = "")

peaks_summary <- expedition %>% 
    group_by(peak_id, peak_name) %>% 
    summarise(n_climbs = n(),
              across(members:hired_staff_deaths, sum),
              first_climb = min(year)) %>% 
    ungroup() %>% 
    arrange(desc(n_climbs)) %>% 
    mutate(pct_deaths = member_deaths/members,
           pct_hired_staff_death = hired_staff_deaths/hired_staff) %>% 
    inner_join(peaks %>% select(peak_id, height_metres))

# How deadly these mountains are???

peaks_eb <- peaks_summary %>% 
    arrange(desc(pct_deaths)) %>% 
    filter(members >= 20) %>% 
    add_ebb_estimate(member_deaths, members)

peaks_eb %>%
    ggplot(aes(pct_deaths, .fitted)) +
    geom_point(size = 2, alpha = 0.5) +
    geom_abline(color = "red") +
    geom_text(aes(label = peak_name), check_overlap = T, hjust = 1, vjust = 1) +
    scale_x_continuous(labels = percent_format()) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Adjusted Deaths",
         x = "Death Rate (raw)",
         y = "Death Rate (Empirical Bayes Adjusted")

peaks_eb %>% 
    filter(members >= 200) %>% 
    mutate(peak_name = fct_reorder(peak_name, .fitted)) %>% 
    ggplot(aes(.fitted, peak_name)) +
    geom_point(aes(size = members)) +
    geom_errorbarh(aes(xmin = .low, xmax = .high), alpha = 0.5) +
    scale_x_continuous(labels = percent) +
    labs(title = "How deadly these mountains are???",
         x = "Death Rate (Empirical Bayes Adjusted + 95% credible interval)",
         y = "")




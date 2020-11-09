# Load the libraries
library(tidyverse)
library(scales)
library(here)
library(ebbr)
library(broom)
theme_set(theme_light())


# Load the data from r objects
peaks <- read_rds(here("data", "peaks_raw.rds"))
members <- read_rds(here("data", "members_raw.rds"))
expedition <- read_rds(here("data", "expedition.rds"))

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

summary_expeditions <- function(tbl) {
    tbl %>% 
    summarise(n_climbs = n(),
          pct_success = mean(success == "Success"),
          across(members:hired_staff_deaths, sum),
          first_climb = min(year)) %>% 
        mutate(pct_deaths = member_deaths/members,
        pct_hired_staff_death = hired_staff_deaths/hired_staff)
}

peaks_summary <- expedition %>% 
    group_by(peak_id, peak_name) %>% 
    summary_expeditions() %>% 
    ungroup() %>% 
    arrange(desc(n_climbs)) %>% 
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
         subtitle = "Only peaks with atleast 200 climbers have been included",
         x = "Death Rate (Empirical Bayes Adjusted + 95% credible interval)",
         y = "",
         caption = "Data Source: R4DS Tidy Tuesday 2020-09-22")

peaks_eb %>% 
    filter(members >= 100) %>% 
    ggplot(aes(height_metres, .fitted)) +
    geom_point(aes(size = members)) +
    geom_text(aes(label = peak_name), hjust = 1, vjust = 1, check_overlap = T)


expedition %>% 
    ggplot(aes(days_to_highpoint)) +
    geom_histogram()

expedition %>% 
    filter(!is.na(peak_name),
           !is.na(days_to_highpoint),
           success == "Success") %>% 
    mutate(peak_name = fct_lump(peak_name, 10),
           peak_name = fct_reorder(peak_name, days_to_highpoint)) %>% 
    ggplot(aes(days_to_highpoint, peak_name)) +
    geom_boxplot() +
    labs(title = "How long does it take to reach high point of the peak?",
         x = "Days to reach high point from basecamp",
         y = "",
         caption = "Data Source: R4DS Tidy Tuesday Data",
         subtitle = "Successful climbs only")

expedition %>% 
    filter(peak_name == "Everest") %>% 
    ggplot(aes(days_to_highpoint)) +
    geom_density(aes(color = success, fill = success), alpha = 0.4) +
    labs(title = "How long does it take to reach high point of the Everest?",
         x = "Days to reach high point from basecamp",
         y = "",
         caption = "Data Source: R4DS Tidy Tuesday Data",
         subtitle = "Successful climbs only")

everest_by_decade <- expedition %>% 
    filter(peak_name == "Everest") %>% 
    mutate(decade = pmax(10*year%/%10, 1970)) %>% 
    group_by(decade) %>% 
    summary_expeditions()
    
everest_by_decade %>% 
    ggplot(aes(decade, pct_deaths)) +
    geom_point(aes(size = members, color = "All Climbers")) +
    geom_line(aes(color = "All Climbers")) +
    geom_point(aes(y = pct_hired_staff_death ,size = members, color = "Hired Staff")) +
    geom_line(aes(y = pct_hired_staff_death ,color = "Hired Staff")) +
    scale_x_continuous(breaks = seq(1970,2010, 10), 
                       labels = c("> 1980", seq(1980, 2010, 10))) +
    scale_y_continuous(labels = percent) +
    expand_limits(y = 0) +
    labs(title = " The Everest has become less deadly over time..",
     x = "",
     y = "",
     color = "",
     caption = "Data Source: R4DS Tidy Tuesday Data",
     subtitle = "Successful climbs only")


model_glm <- members %>% 
    filter(peak_name == "Everest") %>% 
    mutate(leader = expedition_role == "Leader") %>% 
    glm(died ~ year + sex + age + leader + hired * oxygen_used, data = ., family = "binomial")

model_glm %>% 
    tidy(conf.int = T, exponentiate = T) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(term = fct_reorder(term, estimate)) %>% 
    ggplot(aes(estimate, term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = )

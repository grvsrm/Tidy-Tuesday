# Prerequisites
library(tidyverse)
library(here)
library(janitor)
library(scales)
library(broom)

theme_set(theme_light())

# load the data

big_epa_cars <- read_rds(here("data", "big_epa_cars.rds"))

big_epa_cars_sel <-  big_epa_cars %>%  
    select(city08, highway08, make, model, cylinders, displ, drive, engId, eng_dscr)

big_epa_cars %>% 
    ggplot(aes(highway08, city08, color = uses_electricity)) +
    geom_point() +
    geom_abline(color = "orange", size = 1, lty = 2) +
    facet_wrap(~uses_electricity) +
    labs(title = "How does fuel efficiency differ in city and highway?",
         x = "Highway MPG",
         y = "City MPG") +
    theme(legend.position = "none") +
    expand_limits(x = 0, y = 0)


big_epa_cars %>% 
    filter(cityE == 0) %>% 
    mutate(VClass = fct_lump(VClass, 8),
           VClass = fct_reorder(VClass, city08)) %>% 
    ggplot(aes(city08, VClass)) +
    geom_boxplot()


big_epa_cars %>% 
    filter(cityE == 0, !is.na(drive)) %>% 
    mutate(drive = fct_lump(drive, 8),
           drive = fct_reorder(drive, city08)) %>% 
    ggplot(aes(city08, drive)) +
    geom_boxplot()

big_epa_cars %>% 
    filter(cityE == 0) %>% 
    ggplot(aes(city08, cylinders, group = cylinders)) +
    geom_boxplot()

big_epa_cars %>% 
    filter(cityE == 0) %>% 
    ggplot(aes(displ, city08)) +
    geom_point(size =2) +
    expand_limits(y = 0) +
    geom_smooth(method = "lm")

# Model ----

# Goal is to predict fuel efficiency for non-electric cars

non_electric_cars <- big_epa_cars %>% 
    filter(cityA08 == 0, # Cars with single fuel source
           cityE == 0) # Only Non Electric Cars 


# A simple linear model
lm(city08~displ, data = non_electric_cars) %>% 
    augment() %>% 
    names()
    ggplot(aes(displ, city08)) +
    geom_point() +
    geom_line(aes(y = .fitted), size = 2, color = "red")


# Linear model with splines
library(splines)
lm(city08 ~ ns(displ, 2), data = non_electric_cars) %>% 
    summary()






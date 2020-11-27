# Prerequisites
library(tidyverse)
library(here)
library(janitor)



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





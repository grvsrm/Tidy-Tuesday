# Pre requisites
library(tidyverse)



# Read the data
shelter <- read_rds(here("data", "toronto_shelter.rds"))

shelter %>% 
    summary()

shelter %>% 
    count(shelter_city)

shelter %>% 
    count(shelter_city, shelter_name) %>% 
    view()

shelter %>% 
    mutate(occupancy_status = if_else(capacity-occupancy >= 0, "Full", "Not Full Yet")) %>% 
    count(occupancy_status)



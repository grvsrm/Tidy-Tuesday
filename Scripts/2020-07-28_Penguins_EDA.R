# Pre-requisites

library(tidyverse)
library(here)
library(janitor)



# Read the data

penguins <- read_rds(here("data", "penguins.rds"))


penguins

# EDA

penguins %>% 
    

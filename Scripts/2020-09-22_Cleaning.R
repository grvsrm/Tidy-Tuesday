# Load the libraries
library(tidyverse)
library(scales)
library(here)
theme_set(theme_light())


# Download the data
tt <- tidytuesdayR::tt_load("2020-09-22")

# Save the raw data
tt$peaks %>% 
    write_rds(here("data",
                   "peaks_raw.RDS"))
tt$members %>% 
    write_rds(here("data",
                   "members_raw.rds"))
tt$expeditions %>% 
    write_rds(here("data",
                   "expedition_raw.rds"))

# Load the data from r objects
peaks <- read_rds(here("data", "peaks_raw.rds"))
members <- read_rds(here("data", "members_raw.rds"))
expedition <- read_rds(here("data", "expedition_raw.rds"))

# Clean the data (If needed)

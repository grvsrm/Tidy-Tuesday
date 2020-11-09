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
expedition_raw <- read_rds(here("data", "expedition_raw.rds"))

# Clean the data (If needed)
na_reason <- c("Did not attempt climb", "Did not reach base camp", "Unknown", "Attempt rumoured")

expedition <- expedition_raw %>% 
    mutate(success = case_when(str_detect(termination_reason, "Success") ~ "Success",
                              termination_reason %in% na_reason ~ "Other",
                              TRUE ~ "Failure"))

# Save the clean data
expedition %>% 
    write_rds(here("data", "expedition.rds"))

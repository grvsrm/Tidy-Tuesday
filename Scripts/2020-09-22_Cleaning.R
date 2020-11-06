# Load the libraries
library(tidyverse)
library(scales)
theme_set(theme_light())


# Download the data
tt <- tidytuesdayR::tt_load("2020-09-22")
peaks_raw <- tt$peaks
members_raw <- tt$members
expeditions_raw <- tt$expeditions

# Save the data
peaks_raw %>% 
    write_rds("peaks_raw.RDS")
members_raw %>% 
    write_rds("members_raw.rds")
expeditions_raw %>% 
    write_rds("expedition_raw.rds")

# Transform the data (If needed)
peaks %>% 
    count(climbing_status)

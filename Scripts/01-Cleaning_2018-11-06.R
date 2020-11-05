# Load the libraries
library(tidyverse)
library(tidymodels)



# Download and save the data as an rds object

ttfile <- tidytuesdayR::tt_load("2018-11-06")

ttfile$us_wind %>% 
    write_rds(here("data", "us_wind_raw.rds"))


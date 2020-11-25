# Pre-requisites
library(tidyverse)
library(tidymodels)
library(here)
library(scales)

theme_set(theme_light())

# Load tha data
food_consumption <- read_rds(here("data", "food_consumption_raw.rds"))


food_consumption

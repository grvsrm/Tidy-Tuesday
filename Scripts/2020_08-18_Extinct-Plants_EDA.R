# Prerequisites ----
library(tidyverse)
library(here)
library(scales)

theme_set(theme_light())

# Data load ----

plants <- read_rds(here("data", "plants.rds"))

# Pre requisites

library(tidyverse)
library(lubridate)
library(here)
library(ragg)
library(patchwork)


# Fonts
library(showtext)
font_add_google("Oswald", "oswald")
font_add_google("Heebo", "heebo")
font_add_google("DM Mono", "dmmono")

showtext_auto(enable = T)
showtext_opts(dpi = 300)


# Data
big_mac <- read_rds(here("data", "bigmac_raw.rds"))


# Visualization
eurozone_countries <- c("Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", 
                        "Greece", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                        "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain")


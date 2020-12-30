# Pre requisites
library(tidyverse)
library(here)
library(patchwork)
library(cowplot)


# Fonts
library(showtext)
font_add_google()
showtext_auto()

# Load the data
women <- read_rds(here("data", "women-2020.rds"))


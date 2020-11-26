# Prerequisites
library(tidyverse)
library(tidykids)
library(jkmisc)
library(readxl)
library(here)
library(ggforce)
library(ggrepel)
library(glue)


# Load the data

kids <- read_rds(here("data", "kids.rds"))

# child_poverty <- 
    
    
child_poverty <- read_excel(here("data", "state-5-U18-pov-inc-010918.xlsx"), skip = 4) %>% 
    janitor::clean_names() %>% 
    select(state, child_poverty_rate = x2016_acs_child_poverty_rate) %>% 
    filter(!is.na(state), !is.na(child_poverty_rate)) %>% 
    mutate(year = "2016")



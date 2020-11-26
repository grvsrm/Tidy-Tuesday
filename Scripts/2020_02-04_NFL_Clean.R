# Pre-requisites
library(tidyverse)
library(here)
library(janitor)


# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv') %>% 
    write_rds(here("data", "attendance_raw.rds"))

read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv') %>% 
    write_rds(here("data", "standings_raw.rds"))

read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv') %>% 
    write_rds(here("data", "games_raw.rds"))


# Clean the data

attendance <- read_rds(here("data", "attendance_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))

standings <- read_rds(here("data", "standings_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols"))

# Save the data
attendance %>% 
    write_rds(here("data", "attendance.rds"))

standings %>% 
    write_rds(here("data", "standings.rds"))

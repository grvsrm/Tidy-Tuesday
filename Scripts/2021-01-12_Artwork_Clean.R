# Pre requisites
library(readr)
library(here)
library(janitor)


# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv') %>% 
    write_rds(here("data", "artwork_raw.rds"))



read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv") %>% 
    write_rds(here("data", "artists_raw.rds"))


# Clean the data
read_rds(here("data", "artwork_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows" , "cols")) %>% 
    write_rds(here("data", "artwork_clean.rds"))


read_rds(here("data", "artists_raw.rds")) %>% 
    clean_names() %>% 
    remove_empty(c("rows" , "cols")) %>% 
    write_rds(here("data", "artists_clean.rds"))

# End of script
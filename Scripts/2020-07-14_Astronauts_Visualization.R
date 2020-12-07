# Pre-requisites
library(tidyverse)
library(janitor)
library(here)
library(magrittr)

# Read the data
astronauts <- read_rds(here("data", "astronauts.rds"))


astronauts %>% count(occupation, sort = T)

astronauts_by_country <- astronauts %>% 
    mutate(occupation = fct_lump(occupation, 5)) %>% 
    count(nationality, occupation, wt = total_hrs_sum, name = "totalhrs")


outliers <- astronauts_by_country %>%
    count(nationality, wt = totalhrs) %>% 
    top_n(-10, n) %>% 
    select(nationality)

astronauts_by_country %<>% 
    anti_join(outliers)


data_id <- astronauts_by_country %>% 
    select(nationality) %>% 
    unique() %>% 
    mutate(id = row_number())

data_plot <- astronauts_by_country %>% 
    left_join(data_id)


labeldata <- data_plot %>% 
    count(nationality, id, wt = totalhrs, name = "tot") %>% 
    mutate(no_of_bar = nrow(labeldata),
           angle = 90 - 360 * (id-0.5) /no_of_bar, # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
           hjust = ifelse(angle < -90, 1, 0),
           angle = ifelse(angle < -90, angle+180, angle))


data_plot %>%
    ggplot(aes(id, log(totalhrs), fill = occupation)) +
    geom_col(position = "identity",alpha = 0.5) +
    scale_fill_manual(values = c("#dbdce1", "#97bbc7", "#fbc213", "#2348a3", "#fb9a99", "#2348c3")) +
    theme_minimal() +
    coord_polar() +
    geom_text(
        data = labeldata,
        aes(id,log(tot) + 10,
            label = nationality,
            hjust = hjust
        ),
        color = "gray",
        fontface = "bold",
        alpha = 0.6,
        size = 2,
        angle = labeldata$angle,
        inherit.aes = FALSE
    ) +
    labs(
        title = "Time Spent In Space",
        caption = paste0(
            "For: #TidyTuesday,
                        Source: Corlett, Stavnichuk, and Komarova 2020\n",
            "Visualization: @ManasiM_10"
        )
    ) +
    theme(
        panel.background = element_rect(fill = "#202736", color = "#202736"),
        plot.background = element_rect(fill = "#202736", color = "#202736"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        #plot.margin = unit(rep(-1,4), "cm"),
        legend.text = element_text(colour = "gray", size = 8),
        legend.title = element_blank(),
        plot.title = element_text(
            color = "#efcd00",
            size = 15,
            face = "bold.italic",
            hjust = 0.8
        )
    )

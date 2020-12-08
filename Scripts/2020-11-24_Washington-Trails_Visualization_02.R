# Pre requisites
library(tidyverse)
library(here)
library(ggtext)
library(showtext)
library(ragg)
library(patchwork)

# Font
font_add_google("Roboto", "roboto")
font_add_google("Oswald", "oswald")
font_add_google("Fira Sans", "fira")
showtext_opts(dpi = 180)
showtext_auto(enable = TRUE)


# Data
hike <- read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

hike

# Colors 

# flashy
bck_clr <- "#5ECCF3FF"
txt_clr <- "#17406DFF"
# map_sel <- "#f33827"
# map_unsel <- "#f3b35e"

#or not flashy
# bck_clr <- "#DCF0F7"
# txt_clr <- "#17406DFF"
map_sel <- "#5ed94d"
map_unsel <- "#f7ebdc"


library(maps)
world_map <- map_data("state") %>% 
    as_tibble()

usa_map <- world_map %>% 
    filter(!subregion %in% c("Hawaii", "Alaska")) %>% 
    mutate(color = case_when(region == "washington" ~ "1",
                             TRUE ~ "0")) %>% 
    ggplot(aes(long, lat, group = group, fill = color)) +
    geom_polygon(color = "black", size = 0.1, show.legend = F) +
    scale_fill_manual(values = c("1" = map_sel, "0" = map_unsel)) +
    theme_void()


mountain <- tibble(x = c(0,30,40,60,80,100,130),
                  y = c(0,80,60,100,60,80,0),
                  group = "1")
mountain2 <- tibble(x = c(0,20,55, 90, (30*45-(130*80))/(-80), 130),
                    y = c(0,20*80/30,40, 50, 45,0),
                    group = "2")


ggplot()+
    geom_polygon(data = mountain, aes(x = x, y = y, group = group), fill = "white")  +
    geom_polygon(data = mountain2, aes(x = x, y = y, group = group), fill = "grey") +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightblue"),
          panel.background = element_rect(fill = "lightblue"))


# Data Prep

data <- hike %>% 
    unnest(features) %>% 
    group_by(features) %>% 
    summarise(n = n(),
              perc = n/nrow(hike)*100) %>% 
    ungroup() %>% 
    mutate(x1 = list(c(0,30,40,60,80,100,130)),
           y1 = list(c(0,80,60,100,60,80,0)),
           x2 = list(c(0,20,55, 90, (30*45-(130*80))/(-80), 130)),
           y2 = list(c(0,20*80/30,40, 50, 45,0)))

white_mountain <- data %>% 
    unnest(x1,y1) %>% 
    mutate(x1 = x1*perc,
           y1 = y1*perc) %>% 
    group_by(features) %>% 
    mutate(offset = mean(x1)) %>% 
    ungroup() %>% 
    mutate(x1 = x1-offset)

offset <- white_mountain %>% 
    group_by(features) %>% 
    summarise(offset = unique(offset))

grey_mountain <- data %>% 
    bind_cols(offset %>% select(offset)) %>% 
    unnest(x2, y2) %>% 
    mutate(x2 = x2*perc, 
           y2 = y2*perc,
           x2 = x2 - offset)

labels <- white_mountain %>% 
    group_by(features) %>% 
    summarise(high = max(y1),
              offset = max(offset),
              perc = scales::percent(max(perc)/100)) %>% 
    ungroup() %>% 
    mutate(features = fct_reorder(features, perc))


# Main plot

features_facet <- ggplot() +
    geom_polygon(data = white_mountain, aes(x = x1, y = y1, group = features), fill = "white", color = "grey50") +
    geom_polygon(data = grey_mountain, aes(x = x2, y = y2, group = features), fill = "grey70") +
    geom_text(data = labels, aes(1200, high+700, label = perc), inherit.aes = FALSE, family = "oswald", color = txt_clr, size = 16, fontface = "bold") +
    scale_y_continuous(limits = c(0,8000)) +
    facet_wrap(~features, ncol = 5, strip.position = "bottom") +
    labs(title = "Which features can you expect during your hikes in Washington state",
         subtitle = "Percentage of hikes for each feature",
         caption = "Visualisation: Christophe Nicault | Data: Washington Trails Association") +
    theme_void() +
    theme(plot.background = element_rect(fill = bck_clr, color = NA),
          panel.background = element_rect(fill = bck_clr),
          #panel.spacing = unit(1, "lines"),
          panel.border = element_rect(color = bck_clr, fill = NA),
          strip.text = element_markdown(family = "roboto", size = 36, color = txt_clr),
          plot.title = element_markdown(family = "oswald", size = 56, color = txt_clr, hjust = 0.5, margin = margin(0,0,0,20)),
          plot.subtitle = element_markdown(family = "roboto", size = 40, color = txt_clr, hjust = 0.5, margin = margin(10,0,10,20)),
          plot.caption =  element_markdown(family = "fira", size = 26, hjust = 1, margin = margin(10,0,0,0)),
          plot.margin = margin(20,10,0,10))

final <- features_facet +
    inset_element(usa_map, left = 0, bottom = 0.9, top = 1.1, right = 0.2, clip = TRUE)

# Save plot

ggsave(here("plots", "washington-trails.png"),
       plot = final,
       width = 20,
       height = 10,
       device = agg_png(res = 300))

dev.off()

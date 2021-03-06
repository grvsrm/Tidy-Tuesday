library(tidyverse)
library(here)
library(jkmisc)
library(ggraph)
library(tidygraph)
library(glue)
library(ggtext) 
library(magick)

ikea <- read_rds(here("data", "ikea_raw.rds"))

lvl_one <- ikea %>% 
    group_by(category) %>% 
    summarize(price = sum(price),
              n = n()) %>% 
    mutate(from = "root") %>% 
    rename(to = category)

lvl_two <- ikea %>% 
    group_by(category, name) %>% 
    summarize(price = sum(price),
              n = n()) %>% 
    rename(from = category, 
           to = name) %>% 
    mutate(idx = row_number())

edges <- bind_rows(lvl_one, lvl_two) %>% 
    select(from, to, n, price) %>% 
    distinct()


root <- edges %>% 
    summarize(across(c(price, n), sum),
                  node = "root")

lvl_one_nodes <- lvl_one %>% 
    select(node = to, n, price) 

lvl_two_nodes <- lvl_two %>%
    ungroup() %>% 
    select(node = to, n, price, idx)

nodes <- bind_rows(root, lvl_one_nodes, lvl_two_nodes) %>% 
    distinct(node, n, price) %>% 
    select(node, n, price) 

graph <- tbl_graph(nodes, edges)

final_graph <- graph %>% 
    activate(nodes) %>% 
    filter(!node_is_isolated())

stem_labels <- create_layout(final_graph, layout = 'dendrogram', circular = TRUE) %>% 
    filter(leaf == FALSE) %>% 
    slice(-1) %>% 
    mutate(percent = n/sum(n, na.rm = TRUE)) %>% 
    mutate(label = str_to_upper(str_replace_all(str_wrap(node, 1), "(?<=.)(?!$)", " "))) %>% 
    mutate(xend = case_when(node == "Sofas & armchairs" ~ x - 0.1,
                            node == "Sideboards, buffets & console tables" ~ x -0.07,
                            node == "Outdoor furniture" ~ x + 0.01,
                            node == "Room dividers" ~ x + 0.02,
                            node == "Nursery furniture" ~ x + 0.05,
                            node == "Children's furniture" ~ x + 0.1,
                            node == "Chests of drawers & drawer units" ~ x + 0.15,
                            TRUE ~ x)) %>%
    mutate(yend = case_when(node == "Sofas & armchairs" ~ y + 0.1,
                            node == "Room dividers" ~ y,
                            TRUE ~ y)) %>% 
    mutate(xold = x,
           x = xend,
           yold = y,
           y = yend)


lines <- stem_labels %>% 
    mutate(xold = case_when(node == "Sofas & armchairs" ~ xold + 0.01,
                            TRUE ~ xold),
           y = case_when(node == "Sofas & armchairs" ~ y - 0.1,
                         TRUE ~ y))


plot <- ggraph(final_graph, layout = 'dendrogram', circular = TRUE) + 
    geom_edge_diagonal(colour  = "#FFDA1A", alpha = 0.1, width = 0.9) +
    geom_node_text(aes(x = x*3, y = y*3, label = glue("{label}\n({scales::percent(percent, accuracy = 0.1)})"),
                       filter = leaf == FALSE & node != 'root', 
                       hjust = ifelse(between(node_angle(x,y), 90, 270), 1, 0)), 
                   size = 3, color = "blue", family = "Noto Sans Bold", data = stem_labels) +
    geom_segment(aes(x = 2*xold, xend = 2.8*x, y = 2*y, yend = 2.8*yend), data = lines, color = "blue", size = 0.2) +
    geom_node_point(aes(filter = leaf), colour  = "#FFDA1A") +
    annotate("label", x = 0, y = 0.15, label = "IKEA", family = "Anton", colour  = "#FFDA1A", size = 20, label.size = 0, fill = "#0051ba") +
    annotate("label", x = 0, y = -0.085, label = "Catalogue", family = "Anton", colour  = "#FFDA1A", size = 8, label.size = 0, fill = "#0051ba") +
    annotate("label", x = 0, y = -0.235, label = "Categories", family = "Anton", colour  = "#FFDA1A", size = 8, label.size = 0, fill = "#0051ba") +
    labs(x = NULL, 
         y = NULL,
         title = NULL,
         subtitle = NULL,
         caption = "**Data**: IKEA & Kaggle | **Graphic**: @jakekaupp") +
#    theme_jk(dark = TRUE, 
#             grid = FALSE,
#             markdown = TRUE) +
    theme(legend.position= "none",
          plot.background = element_rect(fill = "#0051ba"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    expand_limits(x = c(-2, 2), y = c(-2, 2)) +
    coord_equal() 

ggsave(here("Plots", "ikea_plot.png"), plot, width = 12, height = 12)

image_read(here("Plots", "ikea_plot.png")) %>% 
    image_trim() %>% 
    image_write(here("Plots", "ikea_plot.png"))

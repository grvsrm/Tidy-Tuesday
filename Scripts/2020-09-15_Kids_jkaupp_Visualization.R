# Prerequisites
library(tidyverse)
library(jkmisc)
library(readxl)
library(here)
library(ggforce)
library(ggrepel)
library(glue)
library(extrafont)
loadfonts()

# Load the data

kids <- read_rds(here("data", "kids.rds"))

# child_poverty <- 
    
    
child_poverty <- read_excel(here("data", "state-5-U18-pov-inc-010918.xlsx"), skip = 4) %>% 
    janitor::clean_names() %>% 
    select(state, child_poverty_rate = x2016_acs_child_poverty_rate) %>% 
    filter(!is.na(state), !is.na(child_poverty_rate)) %>% 
    mutate(year = 2016)

plot_data <- kids %>% 
    filter(variable == "PK12ed", year == 2016) %>% 
    left_join(child_poverty) %>% 
    select(!contains("variable")) %>% 
    mutate(ratio = inf_adj_perchild/child_poverty_rate)


overall <- plot_data %>% 
    summarise(across(c(inf_adj_perchild, child_poverty_rate), mean)) %>% 
    mutate(ratio = inf_adj_perchild/child_poverty_rate)

highlights <- plot_data %>% 
    top_n(3, inf_adj_perchild) %>% 
    bind_rows(plot_data %>% 
                  top_n(3, child_poverty_rate)) %>% 
    mutate(desc = glue("Education Expenditure: $ {round(inf_adj_perchild,2)}k
                       Child Poverty Rate : {child_poverty_rate}%"))

labels <- plot_data %>% 
    anti_join(highlights) %>% 
    left_join(bind_cols(state = state.name, abb = state.abb)) %>% 
    bind_rows(overall) %>% 
    mutate(abb = if_else(is.na(abb), "US Avg", abb),
           color = if_else(abb == "US Avg", "#cb5a4c", "gray60"))

plot_data %>% 
    ggplot(aes(child_poverty_rate, inf_adj_perchild)) +
    geom_point(size =2) +    
    geom_point(data = overall, color = "#cb5a4c", size = 2) +
    geom_text_repel(data = labels, aes(label = abb, color = color), family = "Comic Sans MS") +
    geom_mark_circle(data = highlights, aes(label = state, group = state, description = desc),
                     expand = unit(3, "mm"),
                     label.family = c("Comic Sans MS", "Comic Sans MS"),
                     label.minwidth = unit(70, "mm"),
                     label.fontsize = c(14,10)) +
    scale_x_continuous(limits = c(0,35), breaks = seq(0,30,10), labels = c("0", "10", "20", "30%")) +
    scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5), labels = c("0", "5", "10", "15", "$20k")) +
    scale_color_identity() +
    labs(x= "",
         y= "",
         title = "**Relationship Between Education Spending and Poverty in 2016 America**",
         subtitle = '"Most people believe that students do better in well-funded schools and that public education should provide a level playing field for all children. Nearly half of the funding for public schools in the United States, however, is provided<br>through local taxes, generating large differences in funding between wealthy and impoverished communities (National Center for Education Statistics, 2000a)."<br>_Biddle, B., and D. C. Berliner. "Unequal school." Educational Leadership 59.8 (2002): 48-59._',
         caption = "**Data**: {tidykids} | **Graphic**: @jakekaupp") +
    theme_jk(dark = FALSE,
             markdown = TRUE,
             base_family = "Comic Sans MS",
             plot_title_family = "Comic Sans MS",
             plot_title_size = 20,
             caption_family = "Comic Sans MS",
             subtitle_family = "Comic Sans MS",
             grid = "XY")


ggsave(here("Plots", "kids_jkaupp_plot.png"), width = 20, height = 11, units = "in", dev = ragg::agg_png())


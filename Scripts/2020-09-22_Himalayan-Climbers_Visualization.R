library(tidyverse)
library(ggtext)
library(showtext)
library(here)

font_add_google("Fjalla One")
showtext_auto()


tt <- tidytuesdayR::tt_load(2020, week = 39)
peaks <- tt$peaks
members <- tt$members

peaks_tidy <- members %>% 
    group_by(peak_name) %>% 
    summarise(n = n(),
              success = sum(success),
              fail = n - success) %>% 
    ungroup() %>% 
    pivot_longer(success:fail, "success") %>% 
    mutate(peak_name = str_to_upper(peak_name) %>% fct_reorder(n)) %>% 
    arrange(desc(n)) %>% 
    slice_head(n = 40) 

png(here("plots", "2020-w39-himalayanclimbing.png"), 
    height = 9, width = 7, units = "in", res = 300)


peaks_tidy %>% 
    ggplot(aes(value, peak_name, group = peak_name, color = success)) +
    geom_point(size = 4, stroke = 4, shape = 1) +
    geom_line(color = "gray20", size = 1, linetype = "longdash") +
    labs(
        x = "",
        y = "",
        title = "MOST POPULAR TWENTY <span style='color:lightsteelblue'>PEAKS</span>",
        subtitle = "How many mountain climbers have <span style='color:navy'>succeeded</span> or <span style='color:firebrick'>failed</span> <br> with their attempted on the most popular twenty <span style='color:black'>Himalayan</span> peaks",
        caption = "\n \n Data: himalayandatabase.com
    Visualization by @botanagin"
    ) +
    scale_x_log10() + 
    scale_color_manual(values = c("firebrick", "navy")) +
    theme_minimal() +
    theme(
        plot.title = element_markdown(size = 20, family = "Fjalla One"),
        plot.subtitle = element_markdown(size = 14, family = "Fjalla One", 
                                         color = "gray30"),
        plot.caption = element_text(size = 8, family = "Fjalla One",
                                    color = "gray30"),
        axis.text = element_text(size = 9, family = "Fjalla One"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "whitesmoke"),
        plot.margin = margin(40, 40, 20, 20)
    ) +
    ggsave("Plots/2020-w39-himalayanclimbing.png", device = png())

           
# Pre requisites

library(tidyverse)
library(lubridate)
library(ggtext)
library(here)

# Fonts
library(showtext)

font_add_google("Merriweather")
font_add_google("Old Standard TT")
showtext_auto()

# Data
bigmac <- read_rds(here("data", "bigmac_raw.rds"))


# Prep Data

bigmac_tr <- bigmac %>%
    filter(name == "Turkey") %>% 
    mutate(
        local_price = if_else(year(date) < 2005, local_price / 1e6, local_price), 
        local_price_lag = lag(local_price),
        inflation_rate = (local_price - local_price_lag) / local_price_lag * 100,
        inflation = if_else(inflation_rate < 0, "negative", "positive"),
        valued = if_else(usd_adjusted < 0, "under", "over")
    ) %>% 
    select(date, local_price, inflation_rate, inflation, usd_adjusted, valued) %>% 
    drop_na() %>% 
    # Add zero value to in between dates manually
    add_row(date = as_date("2016-10-12"), usd_adjusted = 0.001, valued = "over", .after = 11) %>% 
    add_row(date = as_date("2016-10-12"), usd_adjusted = -0.001, valued = "under", .after = 12) %>%
    # Specify min and max geom text labels
    mutate(label = case_when(usd_adjusted == max(usd_adjusted) ~ usd_adjusted,
                             usd_adjusted == min(usd_adjusted) ~ usd_adjusted,
                             TRUE ~ NA_real_)) 


# Plot

bigmac_tr %>% 
    ggplot(aes(date, usd_adjusted, fill = valued, label = scales::percent(label))) +
    geom_area() +
    geom_text(data = filter(bigmac_tr, label > 0),
              family = "Old Standard TT", fontface = "bold",
              position = position_nudge(y = -0.06), size = 12) +
    geom_text(data = filter(bigmac_tr, label < 0),
              family = "Old Standard TT", fontface = "bold",
              position = position_nudge(y = 0.06), size = 12) +
    geom_point(aes(y = label)) +
    scale_fill_manual(values = c("#de2a42", "#ffffff")) +
    labs(
        title = "__Dive Into the Turkish Lira (₺) with the <span style='color: #ffc72c'>Big Mac</span> Index <br>__",
        subtitle = "_“The big mac index was invented by The Economist in 1986 as a lighthearted guide to whether currencies <br> are at their “correct” level. It is based on the theory of purchasing-power parity (PPP), the notion that <br> in the long run exchange rates should move towards the rate that would equalise the prices of <br> an identical basket of goods and services (in this case, a burger) in any two countries” — The Economist_ <br><br> This graph shows how much the Turkish Lira has been over/under valued against the US Dollar in the last decade. <br>",
        caption = " \n \n \n Data by The Economist \n Visualization by @botanagin"
    ) +
    theme_void() +
    theme(
        axis.text.x = element_text(family = "Old Standard TT", face = "bold",
                                   margin = margin(t = -5.25, b = 5.25, unit = "cm"),size = 30),
        legend.position = "none",
        plot.background = element_rect(fill = "gray60"),
        plot.title = element_markdown(family = "Merriweather", size = 48, 
                                      hjust = 0.5),
        plot.subtitle = element_markdown(family = "Merriweather", size = 30, 
                                         hjust = 0.5),
        plot.caption = element_text(family = "Merriweather", size = 30, 
                                    hjust = 0.5),
        plot.margin = margin(t = 2, r = 1, b = 2, l = 1, unit = "cm")
    )

ggsave(here("plots", "2020-w52-bigmacindex.png"), device = ragg::agg_png(), width = 12, height = 12, units = "in", dpi = 300)

dev.off()

# End of script
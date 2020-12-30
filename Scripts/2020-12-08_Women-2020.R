# Pre requisites
library(tidyverse)
library(here)
library(countrycode)
library(waffle)

# Add Fonts
library(showtext)

font_add(family = "FontAwesome5Brands-Regular",
         regular = "C:/Users/Gaurav/Documents/R/win-library/4.0/waffle/fonts/fa-brands-400.ttf")
font_add(family = "FontAwesome5Free-Solid",
         regular = "C:/Users/Gaurav/Documents/R/win-library/4.0/waffle/fonts/fa-solid-900.ttf")

showtext_auto(enable = T)

font_families()
font_add_google(name = "Roboto")

# Download the data
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv') %>% 
  write_rds(here("data", "women-2020.rds"))

women <- read_rds(here("data", "women-2020.rds"))

# Prepare Data

women_cleaned <- women %>% 
    mutate(continent = countrycode(women$country,"country.name","continent"),
           continent = case_when(country == "Wales, UK" ~ "Europe",
                               country == "Northern Ireland" ~ "Europe",
                               country == "Exiled Uighur from Ghulja (in Chinese, Yining)" ~ "Asia",
                               TRUE ~ continent)) %>% 
    filter(!name == "Unsung hero")

# With square pie
women_square_pie <- women_cleaned %>%
    count(category, continent) %>% 
    ggplot(aes(fill = continent,colour = continent, values = n)) +
    geom_waffle(color = "#F7F7F7",size = 1.2 ,n_rows = 5) +
    facet_wrap(~category) +
    coord_equal() +
    scale_fill_manual(name = "", values = c("#EF476F","#FFD166","#0EAD69","#4ECDC4","#118AB2")) +
    theme_minimal() +
    theme_enhance_waffle() +
    labs(title = "Women of 2020",
         subtitle = "This year 100 Women is highlighting those who are leading change and making a difference <br> during these turbulent times.",
         caption = "Data : Women of 2020(Tidy Tuesday) | Visualization : Gaurav S ") +
    theme(
        plot.background = element_rect(color = "#F7F6F3", fill = "#F7F6F3"),
        plot.title = element_text(family = "Nerko One",size = 200,hjust = 0.5,margin = margin(30, 0, 0, 0),color = "#333600"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 80,family = 'Roboto',color = '#2E383D'),
        plot.caption = element_text(size = 50,family = 'Roboto',color = '#2E383D'),
        plot.caption.position =  "plot",
        legend.position = "none"
        )

      
women_square_pie


# Save Plots
women_square_pie +
    ggsave(here("plots","women-square-pie.png"),device = ragg::agg_png(),width = 20, height = 16,units = "in")



####


# See Later
# With glyphs

women_glyph <- women_cleaned %>% 
    count(category, continent) %>% 
    ggplot(aes(label = continent, colour = continent, values = n)) +
    geom_pictogram(n_rows = 3 ,show.legend = F, size = 20) +
    facet_wrap(~category) +
    coord_equal() +
    scale_fill_manual(name = "", values = c("#EF476F","#FFD166","#0EAD69","#4ECDC4","#118AB2")) +
    scale_label_pictogram(name = NULL,values = c("female", "female", "female", "female", "female")) +
    theme_void() +
    theme_enhance_waffle() +
    labs(title = "Women of 2020",
         subtitle = "",
         caption = "Data : Women of 2020(Tidy Tuesday) | Visualization : Gaurav S") +
    theme(plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
          plot.title = element_text(family="Nerko One", size = 40, hjust = 0.5, margin = margin(30,0,0,0),color = "#333600"),
          )

women_glyph

wafle_1 <- women_cleaned %>% 
    filter(category == "Leadership") %>% 
    count(continent) %>% 
    waffle(use_glyph = "female",title = "Leadership", glyph_size = 30, flip = T, size = 0)


wafle_2 <- women_cleaned %>% 
    filter(category == "Knowledge") %>% 
    count(continent) %>% 
    waffle(use_glyph = "female",title = "Knowledge", glyph_size = 30, flip = T,size = 0)

plot_2 <- iron(wafle_1, wafle_2)

# Save Plots
iron(wafle_1, wafle_2) +
    ggsave(here("plots","glyph-plot.png"),device = ragg::agg_png(),width = 14, height = 20,units = "in",dpi = 180)


women_glyph +
    ggsave(here("plots","women-glyph.png"),device = ragg::agg_png(),width = 14, height = 20,units = "in",dpi = 180)

###########
annotate("text", x = 20, y = 21, label = "College",
         family = "Nerko One",size = 20, color = "#FFD166") +
    annotate("text", x = 20, y = 30, label = "First Job",
             family = "Nerko One",size = 20, color = "#0EAD69") +
    annotate("text", x = 20, y = 35, label = "Unemployed",
             family = "Nerko One",size = 20, color = "#4ECDC4")
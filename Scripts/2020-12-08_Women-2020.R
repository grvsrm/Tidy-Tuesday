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

# Prepare Data
women <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

women_cleaned <- women %>% 
    mutate(continent = countrycode(women$country,"country.name","continent"),
           continent = case_when(country == "Wales, UK" ~ "Europe",
                               country == "Northern Ireland" ~ "Europe",
                               country == "Exiled Uighur from Ghulja (in Chinese, Yining)" ~ "Asia",
                               TRUE ~ continent)) %>% 
    filter(!name == "Unsung hero")

# With square pie
women_cleaned %>% 
    count(category, continent) %>% 
    ggplot(aes(fill = continent,colour = continent, values = n)) +
    geom_waffle(color = "#F7F7F7",size = 1, n_rows = 6, make_proportional = T, flip = T) +
    facet_wrap(~category) +
    coord_equal() +
    scale_fill_manual(name = "", values = c("#EF476F","#FFD166","#0EAD69","#4ECDC4","#118AB2")) +
    theme_void() +
    theme_enhance_waffle() +
    labs(title = "Women of 2020",
         subtitle = "",
         caption = "Data : Women of 2020(Tidy Tuesday) | Visualization : Gaurav S") +
    theme(plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
          plot.title = element_text(family="Nerko One", size = 40, hjust = 0.5, margin = margin(30,0,0,0),color = "#333600"),
    )


# With glyphs

women_cleaned %>% 
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

wafle_1 <- women_cleaned %>% 
    filter(category == "Leadership") %>% 
    count(continent) %>% 
    waffle(use_glyph = "female",title = "Leadership", glyph_size = 18, flip = T)


wafle_2 <- women_cleaned %>% 
    filter(category == "Knowledge") %>% 
    count(continent) %>% 
    waffle(use_glyph = "female",title = "Leadership", glyph_size = 18, flip = T)

iron(wafle_1, wafle_2)

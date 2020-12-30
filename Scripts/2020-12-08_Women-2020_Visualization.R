# Pre requisites
library(tidyverse)
library(here)
library(ggtext)
library(countrycode)
library(patchwork)
library(png) # To read png images 
library(grid)

# Fonts
library(showtext)
font_add_google("Abril Fatface", "abril")
font_add_google("Roboto", "roboto slab")

showtext_auto()


# Load the data
women <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# Prep the data

women_df <- women %>% 
    mutate(index = row_number(),
           continent = countrycode(country, "country.name", "continent"))

# Fill NAs with appropriate continents
women_df[1, 8] = "Worldwide"
women_df[63, 8] = "Europe"
women_df[42, 8] = "Asia"
women_df[95, 8] = "Europe"

women_df <-  women_df %>% 
    mutate(category_num = case_when(category == "Creativity" ~ 4,
                                               category == "Identity" ~ 3,
                                               category == "Knowledge" ~ 2,
                                               category == "Leadership" ~ 1,
                                               TRUE ~ 5))



colors <- c("#ffa600", "#ff6e54", "#dd5182", "#955196", "#444e86", "#003f5c")
#Palette from: https://learnui.design/tools/data-color-picker.html

#Bring custom icons
image3 <- readPNG("Data/Images/noun_persona_410777.png") 
id_icon <- rasterGrob(image3, interpolate=FALSE)

image1 <- readPNG("Data/Images/noun_leadership_2909348.png")
leadership_icon <- rasterGrob(image1, interpolate=FALSE)

image2 <- readPNG("Data/Images/noun_knowledge_2824220.png")
knowledge_icon <- rasterGrob(image2, interpolate=FALSE)

image4 <- readPNG("Data/Images/noun_creativity_3017281.png")
creativity_icon <- rasterGrob(image4, interpolate=FALSE)

image5 <- readPNG("Data/Images/noun_Wonder Woman_638563.png")
allwomen_icon <- rasterGrob(image5, interpolate=FALSE)


# Create plot

women_df %>% 
    ggplot(aes(category_num, 1, fill = continent, width = .5)) + 
    geom_bar(stat = "identity", size = 1.5, color="papayawhip") +
    scale_x_continuous(limits = c(0.75, 5.25)) +
    scale_y_continuous(limits = c(0, 37)) +
    scale_fill_manual(values = colors) +
    theme(aspect.ratio = 1) +
    theme(panel.background = element_rect(fill = "papayawhip"),
          plot.background = element_rect("papayawhip")) + 
    # To add custom icons to plot
    annotation_custom(id_icon, xmin=2.5, xmax=3.5, ymin=16, ymax=21) +
    annotation_custom(leadership_icon, xmin=0.5, xmax=1.5, ymin=28, ymax=33) +
    annotation_custom(knowledge_icon, xmin=1.5, xmax=2.5, ymin=31.4, ymax=36.4) +
    annotation_custom(creativity_icon, xmin=3.5, xmax=4.5, ymin=20.7, ymax=25.7) +
    annotation_custom(allwomen_icon, xmin=4.5, xmax=5.5, ymin=23, ymax=28) +
    geom_segment(aes(x=4.8, y=2, xend=4.8, yend=22), color="#bc5090") +
    geom_segment(aes(x=4.8, y=22, xend = 5.2, yend=22), color="#bc5090", 
                 arrow = arrow(length = unit(0.1, "cm"))) + 
    # Add labels to plot
    labs(title = "100 Influential & Inspiring Women in 2020",
         subtitle = "This year, BBC News honored 100 women leaders, intellectuals, activists, and creatives.\nThey left a space, called Unsung Hero, to recognize the unsung heroines worldwide.",
         caption = "Created by @eliane_mitchll | Source: BBC News | #TidyTuesday Week 50 | See Github for Icons") +
    theme(axis.title.x = element_blank(),
      axis.text.y = element_text(size=7),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(family = "abril", size = 60)) +
    theme(plot.subtitle = element_text(family = "roboto slab", size = 30, hjust=0),
          plot.title = element_text(hjust = 0),
          plot.caption = element_text(size = 30),
          legend.position = c(0.70, 0.90),
          legend.text=element_text(size=30),
          legend.title = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = "papayawhip"))




# Save plot
ggsave(here("plots", "women-2020.png"), device = ragg::agg_png(), width = 10, height = 10, units = "in")


dev.off()

# Icon Citations
# creativity by mynamepong from the Noun Project: https://thenounproject.com/icon/3017281/
# Wonder Woman by Éléonore Sabaté from the Noun Project: https://thenounproject.com/icon/638563/
# knowledge by Alena from the Noun Project: https://thenounproject.com/icon/2824220/
# persona by Yu luck from the Noun Project: https://thenounproject.com/search/?q=self+identity&i=410777

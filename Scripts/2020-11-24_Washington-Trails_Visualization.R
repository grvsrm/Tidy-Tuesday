# Prerequisites

library(tidyverse)
library(here)
library(widyr)
library(cowplot)

library(showtext)
showtext_auto()

# Download the data
hike <- read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))


hike %>% 
    count(name,sort=TRUE)


hike_2 <- hike %>% 
    separate(length,into = c("length","type"),sep = ",") %>% 
    mutate(length=parse_number(length),
           type=str_squish(type),
           length_num=case_when(type=="one-way"~length,
                                type=="roundtrip"~length/2,
                                TRUE ~ length)) %>% 
    relocate(name,location,length,type,length_num)


#Find all the hikes with no dog mention in features
hike_2$name[!str_detect(hike_2$features,"Dogs" )]

#Dog Friendly/unfriendly Hikes
hike_3 <- hike_2 %>% 
    mutate(seq=1:n()) %>% 
    unnest(features) %>% 
    group_by(name) %>% 
    mutate(dogs_allowed=case_when(features=="Dogs allowed on leash"~"Dogs allowed",
                                  features=="Dogs not allowed"~"Dogs not allowed",
                                  TRUE ~ NA_character_)) %>% 
    relocate(dogs_allowed) %>% 
    fill(.,dogs_allowed,.direction="downup") %>% 
    mutate(dogs_allowed=replace_na(dogs_allowed,"Not mentioned")) %>% 
    relocate(seq) %>% 
    ungroup() %>% 
    mutate(rating_num=parse_number(rating))


#Correlation

`%nin%` = Negate(`%in%`)

#What are dog friendly/unfriendly hike most correlated to
cor_pair <- hike_3 %>% 
    count(name,seq,features) %>% 
    pairwise_cor(features,seq,n) %>% 
    arrange(desc(correlation))

#Filtering out correlation with Dogs either allowed/not allowed
dogs_cor <- cor_pair %>% 
    filter(str_detect(item1,"Dogs"))  %>% 
    rename("features"=item2) %>% 
    mutate(item1=case_when(item1=="Dogs allowed on leash"~"Dogs allowed",
                           TRUE~item1)) %>%
    filter(features %nin% c("Dogs allowed on leash","Dogs not allowed"))

palette_color <- c("#017EB1","#F27256")

title<-paste0("Hiking with your furry friend ")

dog <- "Data/images/dog.png"

dog_na <- "Data/images/no dog.png"

paw <- "Data/images/paw.png"

plot <- dogs_cor %>%
    mutate(features=case_when(str_detect(features,"Meadows")~"Meadows",
                              TRUE~ features)) %>% 
    mutate(features=fct_reorder(features,correlation)) %>% 
    ggplot(aes(y = features, x = item1, fill = item1, alpha = correlation*2)) +
    geom_tile(width = 0.8, height = 0.8, show.legend = FALSE) +
    scale_fill_manual(values = palette_color) +
    labs(
        x = '',
        y = 'Features of the hike',
        title = title,
        subtitle = 'Exploring Washington trails (wta.org) dataset to find out which factors correlate with dog allowed/not allowed feature. \nInteresting to see that hikes where dogs are allowed are mostly kid friendly where as Established campsite, waterfalls hikes usually do not allow dogs. \n\nDarker color represents positive correlation, lighter color represent negative correlation.\n',
        caption = 'Data: https://www.wta.org | Icons by Flaticon | Viz: Gaurav S,'
    ) +
    theme(axis.line=element_blank(),
          axis.text.x = element_blank(),
          # axis.text.y = element_blank(),
          # axis.text.x = element_text(size=8, face="bold", colour = "black"),
          axis.text.y = element_text(size=8, face="bold", colour = "black"),
          axis.title.y = element_text(size=8, face="bold", colour = "Red"),
          axis.ticks.x = element_blank(),
          plot.background = element_rect(color = "#FFA9AA", fill = "#F7F6F3", size = 3),
          # axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          #  plot.background=element_blank(),
          plot.title = element_text(
              size = 32,
              color = 'red',
              family = "Agency FB"
          ),
          plot.subtitle = element_text(
              size = 9,
              family = 'amatic-sc',
              color = '#2E383D',
          ),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          plot.caption = element_text(
              color = '#cccccc', size = 7, margin = margin(t = 6), hjust = 0.9825,
              face = 'plain'
          ),
          plot.margin = margin(20,10,10,10)
          ) +
    scale_x_discrete(expand = c(0.2,0.2),position='top')


plot

plot_1 <- ggdraw()+
    draw_plot(plot)+
    draw_image(dog, x = 0.05, y = 0.25, scale = 0.065,hjust = 0.2)+
    draw_image(dog_na, x = 0.52, y = 0.25, scale = 0.065,hjust=0.2)+
    draw_image(paw, x = 0.36, y = 0.429, scale = 0.065,hjust=0.2)+
    draw_image(paw, x = 0.31, y = 0.429, scale = 0.065,hjust=0.2)


# file_path <- here(2020, "plots")
plot_1 %>% 
    ggsave(filename = "Washington.png",path = here("Plots"))

ggsave(paste0(file_path, ".pdf"), plot_1, width = 9.5, height = 6, device = cairo_pdf)

pdftools::pdf_convert(paste0(file_path, ".pdf"), filenames = paste0(file_path, ".png"), dpi = 300)
ggsave(paste0(file_path, ".jpg"), plot_1, width = 9.5, height = 6, dpi = 100,
       type = "cairo")




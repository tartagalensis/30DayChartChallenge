library(tidyverse)
library(magick)
library(showtext)

font_add_google("Lato")
showtext_auto()

Sys.setlocale("LC_TIME", "es_ES")


recursos_naturales <- read_csv("dia_06/natural-resources.csv") %>% 
        janitor::clean_names() %>% 
        print()
        
petroleo_argentina <- recursos_naturales %>% 
        filter(entity == "Argentina") %>% 
        select(entity, year, oil_production, oil_consumption,
               oil_imports, oil_exports, oil_reserves) %>% 
        print()

library(ggtext)
ggplot(petroleo_argentina) +
        geom_area(aes(x = year, y = oil_production/1000000), fill = "darkblue", size = 1,
                  alpha=0.6 , size=1, colour="black") +
        geom_area(aes(x = year, y = oil_consumption/1000000), fill = "darkred", size = 1,
                  alpha=0.6 , size=1, colour="black") +
        theme_minimal() +
        labs(title = "Argentina:<span style='color:darkblue;'>producción</span> y <span style='color:darkred;'>consumo</span> de petróleo crudo (1980-2020)",
             y = "Millones de métros cúbicos (m3)",
             subtitle = "Última medición de cada año",
             caption = "Our World in Data | @tartagalensis") +
        theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 15, face="bold", color = "black", family = "Lato"),
              axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 15, face="bold", color = "black", family = "Lato"),
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 15, face="italic"),
              legend.position = "none",
              axis.title.y     = element_text(vjust = 0.5, hjust=0.5, size = 15, face="bold", color = "black", family = "Lato"),
              axis.title.x     = element_blank(),
              axis.ticks       = element_blank(),
              plot.title = element_markdown(family = "Lato", face = "bold", size = 25))


## SAVE
ggsave(plot = last_plot(), filename = "dia_06/petroleo_arg.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_06/petroleo_arg.jpg") %>%
        image_trim() %>%
        image_write("dia_06/petroleo_arg.jpg")         

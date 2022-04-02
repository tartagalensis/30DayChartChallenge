library(tidyverse)
library(janitor)
library(treemapify)
library(magick)

partidos <- read_csv("dia_01/data/WorldCupMatches.csv") %>% 
        clean_names() %>% 
        print()

local_goles <-  partidos %>% 
        select(equipo = home_team_name, goles = home_team_goals) %>% 
        print()

visitante_goles <-  partidos %>% 
        select(equipo = away_team_name,goles = away_team_goals) %>% 
        
        print()

goles_mundiales <- bind_rows(local_goles, visitante_goles) %>% 
        group_by(equipo) %>% 
        summarise(goles = sum(goles)) %>% 
        ungroup() %>% 
        drop_na() %>% 
        arrange(desc(goles)) %>% 
        mutate(goles_equipo = paste(equipo, goles),
               argentina = if_else(equipo == "Argentina", "Argentina", "aaotro")) %>% 
        print()

        
ggplot(goles_mundiales, aes(area = goles, fill = argentina, label = goles_equipo) ) +
        geom_treemap(color = "black") +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15,
                          grow = TRUE) +
        scale_fill_manual(values = c("darkred", "#75aadb")) +
        labs(title = "Goles en mundiales por país",
             subtitle = "#30DayChartChallenge",
             caption = "Fuente: FIFA (2015) | @tartagalensis") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 25),
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 15),
              legend.position = "none")


        
        
## SAVE
ggsave(plot = last_plot(), filename = "dia_01/plots/treemap_goles.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_01/plots/treemap_goles.jpg") %>%
        image_trim() %>%
        image_write("dia_01/plots/treemap_goles.jpg")

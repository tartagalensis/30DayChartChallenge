library(tidyverse)
library(magick)


temas_2018 <- tibble(tema = c("Inseguridad","Inflación","Desempleo","Corrupción","Bajos Salarios",
                              "Educación","Pobreza","Narcotráfico","Justicia","Los políticos"),
                    posicion = c(1,2,3,4,5,
                                 6,7,8,9,10)) %>% 
        mutate(year = 2018) %>% 
        print()

temas_2019 <- tibble(tema = c("Inflación","Desempleo","Pobreza","Inseguridad","Corrupción","Bajos salarios",
                              "Los políticos","Deuda externa","Justicia","Educación"),
                     posicion = c(1,2,3,4,5,
                                  6,7,8,9,10)) %>% 
        mutate(year = 2019) %>% 
        print()

temas_2020 <- tibble(tema = c("Corrupción","Inseguridad","Inflación","Desempleo","Los políticos",
                              "Pobreza","Justicia","Educación","Bajos salarios","Epidemias y enfermedades"),
                     posicion = c(1,2,3,4,5,
                                  6,7,8,9,10)) %>% 
        mutate(year = 2020) %>% 
        print()

temas_2021 <- tibble(tema = c("Inflación","Corrupción","Inseguridad","Los políticos","Pobreza",
                              "Desempleo","Justicia","Bajos salarios","Educación","Deuda externa"),
                     posicion = c(1,2,3,4,5,
                                  6,7,8,9,10)) %>% 
        mutate(year = 2021) %>% 
        print()


issues <- temas_2018 %>% 
        bind_rows(temas_2019) %>% 
        bind_rows(temas_2020) %>% 
        bind_rows(temas_2021) %>% 
        group_by(year, tema) %>% 
        print()



ggplot(data = issues, aes(x = year, y =reorder(posicion, -posicion),
                               group = tema, color=tema)) +
        geom_line( size = 2) +
        geom_point( size = 4) +
        labs(y = "Posición",
             title = "Principales problemas de la Argentina",
             subtitle = "Última medición de cada año",
             caption = "ESPOP - Universidad de San Andrés | @tartagalensis") +
        scale_x_continuous(position = "top") +
        theme_void() +
        viridis::scale_color_viridis(discrete = T,option= "inferno") +
        theme(plot.title = element_text(color="black", size=25, face="bold"), 
              axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 15, face="bold", color = "black"),
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 15, face="italic"),
              legend.position = "none",
              axis.title.y     = element_blank(),
              axis.text.y      = element_blank(),
              axis.title.x     = element_blank(),
              axis.ticks       = element_blank()) +
        geom_text(data = issues %>% filter(year == "2018"), 
                  aes(label = tema),
                  fontface = "bold", 
                  color = "black",
                  vjust= 1.5,
                  size = 5,
                  hjust = -0.01) +
        geom_text(data = issues %>% filter(year == "2021"), 
                  aes(label = tema) , 
                  #hjust = -0.1, 
                  color = "black",
                  fontface = "bold",
                  vjust= 1.6,
                  size = 5,
                  hjust = 0.65)


## SAVE
ggsave(plot = last_plot(), filename = "dia_05/problemas_arg.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_05/problemas_arg.jpg") %>%
        image_trim() %>%
        image_write("dia_05/problemas_arg.jpg") 

library(tidyverse)
library(zoo)
library(magick)

Sys.setlocale("LC_TIME", "es_ES")


tartagal_clima_2021 <- read.csv2("dia_03/registro_temperatura365d_smn 2.txt", skip = 2) %>% 
        as_tibble() %>% 
        janitor::clean_names() %>% 
        filter(grepl('TARTAGAL', x)) %>% 
        separate(x, c("A", "B", "C", "D", "E", "F")) %>% 
        mutate(tmax = paste(B,C, sep = "."),
               tmin = paste(D,E, sep = "."),
               fecha = lubridate::dmy(A)) %>% 
        select(fecha, tmin, tmax, lugar = F) %>% 
        mutate(tmax = as.numeric(tmax),
               tmin = as.numeric(tmin)) %>% 
        mutate(amplitud = tmax-tmin) %>% 
        group_by(lugar) %>% 
        mutate(amplitud_media_movil = rollmean(amplitud, k = 7, fill = NA),
               tmin_media_movil = rollmean(tmin, k = 7, fill = NA),
               tmax_media_movil = rollmean(tmax, k = 7, fill = NA)) %>% 
        print()


tartagal_clima_2021 %>% 
        ggplot() +
        geom_line(aes(x = fecha, y =  tmin_media_movil), color = "darkblue",size = 1.5) +
        geom_line(aes(x = fecha, y = tmax_media_movil),color = "darkred",size = 1.5) +
        labs(title = "Temperatura en Tartagal (último año)",
             subtitle = "Registro de temperaturas mínimas y máximas - Media móvil de 7 días",
             x = "",
             y = "°C",
             caption = "SMN | @tartagalensis") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 25),
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 15, face="italic"),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title.y = element_text(face= "bold",size = 20),
              legend.position = "none")

## SAVE
ggsave(plot = last_plot(), filename = "dia_03/temperatura_tgal.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_03/temperatura_tgal.jpg") %>%
        image_trim() %>%
        image_write("dia_03/temperatura_tgal.jpg")


tartagal_clima_2021 %>% 
        ggplot() +
        geom_line(aes(x = fecha, y =  amplitud_media_movil), color = "darkgreen", size = 1.5) +
        labs(title = "Amplitud térmica en Tartagal (último año)",
             subtitle = "Media móvil de 7 días",
             x = "",
             y = "°C",
             caption = "SMN | @tartagalensis") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 25),
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 15, face="italic"),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title.y = element_text(face= "bold",size = 20),
              legend.position = "none")

## SAVE
ggsave(plot = last_plot(), filename = "dia_03/amplitud_tgal.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_03/amplitud_tgal.jpg") %>%
        image_trim() %>%
        image_write("dia_03/amplitud_tgal.jpg")


tartagal_clima_2021 %>% ungroup() %>% select(tmin, tmax, amplitud) %>% summary()

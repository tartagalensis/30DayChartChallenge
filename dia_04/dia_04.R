library(tidyverse)
library(magick)

volumen_forestal <- read.csv2("https://datos.magyp.gob.ar/dataset/8f740aac-66f3-448f-b8e0-f6a77b359f96/resource/08bc81d2-21cf-4d89-99dd-721c1da3afba/download/inventario-nacional-de-plantaciones-forestales-por-volumen.csv",
                              ,encoding = "latin1",
                              sep = ",") %>% 
        as_tibble() %>% 
        print()

#cortina
cortina <- volumen_forestal %>% select(provincia, genero_id, 
                            unidad_medida = unimedcort_id, volumen = cortina) %>% 
        mutate(volumen = as.numeric(volumen)) %>% 
        drop_na() %>% 
        print()

#macizo
macizo <- volumen_forestal %>% select(provincia, genero_id, 
                            unidad_medida = unimedmac_id, volumen = macizo) %>% 
        mutate(volumen = as.numeric(volumen)) %>% 
        drop_na() %>% 
        print()

volumen_forestal <- bind_rows(cortina, macizo) %>% 
        group_by(provincia, genero_id) %>% 
        summarise(volumen_total = sum(volumen)) %>% 
        ungroup() %>%
        print()


ggplot(volumen_forestal, aes(x = reorder(provincia, volumen_total), y = volumen_total/1000000, fill = genero_id)) +
        geom_col() +
        coord_flip() +
        viridis::scale_fill_viridis(discrete = TRUE, option="magma") +
        labs(title = "Plantaciones forestales por volumen - Argentina (2019)",
             x = "",
             y = "km3",
             fill = "Género",
             caption = "Fuente: Dirección Nacional de Desarrollo Foresto Industrial | @tartagalensis") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 25),
              plot.caption = element_text(size = 15, face="italic"),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title.x = element_text(face= "bold",size = 20),
              legend.position = "none")


## SAVE
ggsave(plot = last_plot(), filename = "dia_04/volumen_forestal.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_04/volumen_forestal.jpg") %>%
        image_trim() %>%
        image_write("dia_04/volumen_forestal.jpg") 

library(tidyverse)
library(ggimage)


medallero <- tibble(pais = c("Brasil", "Brasil","Brasil","Brasil","Brasil",
                             "Alemania","Alemania","Alemania","Alemania",
                             "Italia","Italia","Italia","Italia",
                             "Argentina","Argentina",
                             "Francia", "Francia",
                             "Uruguay","Uruguay",
                             "España","Inglaterra"),
       copas = c(5,4,3,2,1,
                 4,3,2,1,
                 4,3,2,1,
                 2,1,
                 2,1,
                 2,1,
                 1,1))

orden <- c("Brasil","Alemania","Italia","Argentina",
  "Francia","Uruguay","España","Inglaterra")


ggplot(medallero, aes(x = reorder(pais,copas),y = copas)) +
        geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge") +
        coord_flip() +
        geom_emoji(aes(image = '1f3c6'), size=.1) +
        theme_minimal() +
        labs(x = "",
             y = "",
             title = "Los que saben cuanto pesa",
             subtitle = "Copas del Mundo por país",
             caption = "Fuente: FIFA | @tartagalensis") +
        theme(axis.text.x = element_blank(),
              line = element_blank()) +
        theme(plot.title = element_text(face = "bold", size = 25),
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 15),
              axis.text.y = element_text(face = "bold", size = 20),
              legend.position = "none")


## SAVE
ggsave(plot = last_plot(), filename = "dia_02/copas.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_02/copas.jpg") %>%
        image_trim() %>%
        image_write("dia_02/copas.jpg")


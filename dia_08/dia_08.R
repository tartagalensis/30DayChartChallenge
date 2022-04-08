# Paquetes
library(tidyverse)
library(raster)
library(elevatr)
library(rgeoboundaries)
library(sf)
library(viridis)
library(rgdal)
library(rnaturalearth)
library(geoAr)
library(ggtext)
library(showtext)
library(magick)


font_add_google("Encode Sans")
showtext_auto()
#Geo por deptos
salta <- geoAr::get_geo(geo = "SALTA", level = "departamento")

#Boundaries Arg
arg <- gb_adm1("ARG")

#Boundaries Salta
salta_bound <- arg[24,]

#Datos elevacion
elevation_data <- get_elev_raster(locations = salta_bound, z = 9, clip = "locations")

elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"

# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

# Plot
ggplot() +
        geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
        #geom_sf(data = salta, color = "grey", fill = NA, size = 1.2) +
        geom_sf(data = salta_bound, color = "white", fill = NA, size = 1.2) +
        coord_sf() +
        scale_fill_viridis_c(option = "inferno", direction = -1) +
        theme_void() +
        labs(title = "Elevación en la provincia de Salta",
             fill = "Elevación (metros)",
             caption = "Fuentes: GeoAr y elevatr | @tartagalensis") +
        theme(plot.caption = element_text(size = 15, family = "Encode Sans", face="italic"),
              plot.title = element_text(family = "Encode Sans", face = "bold", size = 25),
              legend.text = element_text(family = "Encode Sans", size = 7.5),
              legend.title = element_text(family = "Encode Sans", face = "bold", size = 10),
              legend.position = "bottom")


## SAVE
ggsave(plot = last_plot(), filename = "dia_08/elevacion_salta.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_08/elevacion_salta.jpg") %>%
        image_trim() %>%
        image_write("dia_08/elevacion_salta.jpg")   



### ELEVACION EN DEPARTAMENTO SAN MARTIN ####
san_martin_bound <- salta[9,]


#Datos elevacion
elevation_data <- get_elev_raster(locations = san_martin_bound, z = 9, clip = "locations")

elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"

# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

# Plot
ggplot() +
        geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
        #geom_sf(data = salta, color = "grey", fill = NA, size = 1.2) +
        geom_sf(data = san_martin_bound, color = "white", fill = NA, size = 1.2) +
        coord_sf() +
        scale_fill_viridis_c(option = "inferno", direction = -1) +
        theme_void() +
        labs(title = "Elevación del Departamento San Martín (Salta)",
             fill = "Elevación (metros)",
             caption = "Fuentes: GeoAr y elevatr | @tartagalensis") +
        theme(plot.caption = element_text(size = 15, family = "Encode Sans", face="italic"),
              plot.title = element_text(family = "Encode Sans", face = "bold", size = 25),
              legend.text = element_text(family = "Encode Sans", size = 7.5),
              legend.title = element_text(family = "Encode Sans", face = "bold", size = 10),
              legend.position = "bottom")


## SAVE
ggsave(plot = last_plot(), filename = "dia_08/elevacion_sanmartin.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_08/elevacion_sanmartin.jpg") %>%
        image_trim() %>%
        image_write("dia_08/elevacion_sanmartin.jpg")   


# Pruebo con klm del campo
campo_bounds <- read_sf("~/Downloads/campo_del_medio.kml") %>% 
        as_tibble() %>% 
        select(-Name, -Description) %>% 
        st_as_sf()

campo_bounds <- st_cast(campo_bounds, "MULTIPOLYGON")


#Datos elevacion
elevation_data <- get_elev_raster(locations = st_zm(campo_bounds), z = 9, clip = "locations")

elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"

# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

# Plot
ggplot() +
        geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
        #geom_sf(data = salta, color = "grey", fill = NA, size = 1.2) +
        #geom_sf(data = campo_bounds, color = "white", fill = NA, size = 1.2) +
        coord_sf() +
        scale_fill_viridis_c(option = "inferno", direction = -1) +
        theme_void() +
        labs(title = "Elevación del Campo",
             fill = "Elevación (metros)") +
        theme(plot.caption = element_text(size = 15, family = "Encode Sans", face="italic"),
              plot.title = element_text(family = "Encode Sans", face = "bold", size = 25),
              legend.text = element_text(family = "Encode Sans", size = 7.5),
              legend.title = element_text(family = "Encode Sans", face = "bold", size = 10),
              legend.position = "bottom")


## SAVE
ggsave(plot = last_plot(), filename = "dia_08/elevacion_campo.jpg",
       width = 10, height = 10, units = "in")

## TRIM
image_read("dia_08/elevacion_campo.jpg") %>%
        image_trim() %>%
        image_write("dia_08/elevacion_campo.jpg")

elevation_data %>% summary

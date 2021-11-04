# Librerias:
library(tidyverse)
library(sf)
library(leaflet)
library(ggimage)
library(MASS)
library(viridis)
library(ggtext)

# Funciones propias:
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

# Datos:
# En mi caso, los datos se encuentran en mi disco duro externo.
# Para replicar el mapa, se requiere que te descargues el DENUE más actualizado.
# Descargar de acá: https://www.inegi.org.mx/app/descarga/?ti=6

# DATOS:
d1 = read_csv("/Volumes/Extreme SSD/DATASETS/INEGI - DENUE/DENUE/DENUE/DENUE_COMIDA/denue_00_72_1_csv/conjunto_de_datos/denue_inegi_72_1.csv",
           locale = locale(encoding = "WINDOWS-1252")) %>%
  filter(entidad %in% c("CIUDAD DE MÉXICO", "MÉXICO" , "MORELOS", "PUEBLA", "TLAXCALA")) %>%
  filter(nombre_act == "Restaurantes con servicio de preparación de tacos y tortas")

# unique(d1$entidad)

d2 = read_csv("/Volumes/Extreme SSD/DATASETS/INEGI - DENUE/DENUE/DENUE/DENUE_COMIDA/denue_00_72_2_csv/conjunto_de_datos/denue_inegi_72_2.csv",
           locale = locale(encoding = "WINDOWS-1252")) %>%
  filter(entidad %in% c("CIUDAD DE MÉXICO", "MÉXICO" , "MORELOS", "PUEBLA", "TLAXCALA")) %>%
  filter(nombre_act == "Restaurantes con servicio de preparación de tacos y tortas")

# Juntamos las dos bases de datos:
d = rbind(d1, d2) %>%
  st_as_sf(coords = c("longitud", "latitud"),
           crs = 4326)

# Leemos y Filtramos las entidades de interés
ents <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>%
  filter(ENTIDAD %in% c("Ciudad de México", "México" ,
                        "Morelos", "Puebla",
                        "Tlaxcala"))

# Leemos la capa de municipios:
mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")

# d %>%
#   ggplot(aes()) +
#   geom_sf(alpha = 0.01, color = "orange") +
#   theme_void() +

# Hacemos la visualización
# Los textos se los agregué en Keynote:
# Mapa para TW:
d %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(color = "orange",
                   opacity = 0.01,
                   radius = 0.1) %>%
  addPolygons(data = ents,
              fill = NA,
              color = "white",
              weight = 0.2)

# Mapa para IG:
mpio_int = mpios %>%
  filter(CVE_ENT == "09")
# %>%
#   filter(NOM_MUN == "Cuauhtémoc")

tacos_mpio_int = st_intersection(mpio_int, d)

tacos_mpio = tacos_mpio_int %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])

plot(tacos_mpio_int, max.plot = 1)

# tacos_mpio %>%
#   ggplot(aes(x = X, y = Y)) +
#   geom_image(aes(image = "taco.png"),
#              size = 0.03)

tacos_mpio$density <- get_density(tacos_mpio$X, tacos_mpio$Y, n = 100)

ggplot(tacos_mpio) +
  # geom_sf(data = mpio_int,
  #         fill = NA,
  #         color = "white", size = 1, alpha = 0.1) +
  geom_point(aes(x = X, y = Y,
                 color = density,
                 alpha = 0.2)) +
  viridis::scale_color_viridis(option = "magma") +
  labs(subtitle = "Densidad de taquerías y torterías<br>en la Ciudad de México",
       title = "DENUE, 2021 - INEGI",
       x = "",
       y = "",
       caption = "<b>Fuente: </b>Elaboración propia con datos del DENUE más reciente para 2021.<br>Fecha de elaboración: 01-Noviembre-2021.<br>@JuvenalCamposF") +
  theme_bw() +
  theme(axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.subtitle = element_markdown(hjust = 0.5,
                                      face = "bold",
                                      size = 22),
        legend.position = "none",
        plot.caption = element_markdown(size = 12),
        plot.title = element_markdown(hjust = 0.5, size = 20),
        text = element_text(family = "EB Garamond", color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))

?viridis::scale_color_viridis()

ggsave("densidad_taquerías_cdmx.png",
       device = "png",
       height = 13.50/1.4,
       width = 10.80/1.4)


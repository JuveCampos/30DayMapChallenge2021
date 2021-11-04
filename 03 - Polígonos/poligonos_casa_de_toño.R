# JORGE JUVENAL CAMPOS FERREIRA
# #30DayMapChallenge Dia 03 - Polígonos

# Librerias:
library(tidyverse)
library(sf)
library(ggimage) # Necesita internet para funcionar
library(dismo)

# Datos:
# Descargar el DENUE de la página de INEGI para que funcione
# Especialmente el tabulado 01 que trata de restaurantes y servicios de hospedaje
d1 = read_csv("/Volumes/Extreme SSD/DATASETS/INEGI - DENUE/DENUE/DENUE/DENUE_COMIDA/denue_00_72_1_csv/conjunto_de_datos/denue_inegi_72_1.csv"
              # ,
              # locale = locale(encoding = "WINDOWS-1252")
              ) %>%
  filter(cve_ent %in% "09")

# Red Nacional de Caminos (lo intenté usar para dar contexto pero metía mucho ruido)
rnc = readRDS("CIUDAD DE MÉXICO.rds")
# plot(rnc, max.plot = 1)

# Shape de las entidades.
# Nos quedamos con el polígono de la CMDX
edos = readRDS("shape_ent.rds") %>%
  filter(ENTIDAD == 9)

plot(edos, max.plot = 1)

# Prueba con los datos del Estarbucks:
starbucks = d1 %>%
  filter(str_detect(nom_estab, pattern = "STARBUCKS"))  %>%
  # st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
  select(X = "longitud", Y = "latitud") %>%
  as.matrix()

# Extraemos los datos de la Casa de Antonio:

# Puntos (como tibble)
cdt_puntos = d1 %>%
  filter(str_detect(nom_estab, "LA CASA DE TO\xd1O")) %>%
  slice(-4)

# Puntos (como objeto geográfica)
cdt_puntos_sf <- cdt_puntos %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# Puntos (como objeto matríz)
cdt_matrix = cdt_puntos %>%
  select(X = "longitud", Y = "latitud") %>%
  as.matrix()

# Polígonos Voronoi de los puntos
cdt_voronoi =  dismo::voronoi(cdt_matrix, ext = c(cdt_matrix[,"X"] %>% min() - .1,
                                                  cdt_matrix[,"X"] %>% max() + .1,
                                                  cdt_matrix[,"Y"] %>% min() - .1,
                                                  cdt_matrix[,"Y"] %>% max() + .1)) %>%
  st_as_sf()
st_crs(cdt_voronoi) = 4326

# Polígonos voronoi intersectados
cdt_voronoi = cdt_voronoi %>%
  st_intersection(edos)


# Visualizacion:
nrow(cdt_voronoi)

# Colores de los polígonos (un aleatorio de verdes):
colores = sample(RColorBrewer::brewer.pal(7, "Greens"),
       size = nrow(cdt_voronoi),
       replace = T)

# Código de la visualización:
  ggplot() +
    # geom_sf(data = edos) +
    geom_sf(data = cdt_voronoi, fill = colores, color = "white") +
    # geom_sf(data = rnc, color = "blue", alpha = 0.1) +
    geom_image(data = cdt_puntos, aes(x = longitud,
                                      y = latitud,
                                      image = "cdt.png"),
               size = 0.02) +
    # scale_x_continuous(limits = c(-98.9, -99.4)) + # Esto mataba mi código de ggimage, por eso lo quité
    labs(title = "CDMX dividida por proximidad a una\nCasa de Toño",
         subtitle = str_wrap("...si dividieramos a la Ciudad de México no por alcaldías, sino por la proximidad a la Casa de Toño más cercana, se vería así: ", 90),
         caption = "*Elaboración propia con datos del DENUE (INEGI) más reciente (2021)\nObtención de las divisiones por polígonos de Voronoi\n@JuvenalCamposF - #30DayMapChallenge - Día 03: Polígonos.\nEn donde parece que únicamente hay un punto, pueden ser dos CDT muy pegadas."
         ) +
    theme_bw() +
    theme(plot.background = element_rect(fill = "#002451"),
          panel.background = element_rect(fill = "#002451"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          text = element_text(color = "white", family = "Poppins"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          plot.caption = element_text(hjust = 1, size = 9),
          plot.subtitle = element_text(hjust = 0.5, size = 9))

  # Guardamos la imagen.
  ggsave("casa_de_antonio.png",
         device = "png",
         height = 7,
         width = 7)


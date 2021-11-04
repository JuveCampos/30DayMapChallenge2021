# Librerias:
library(tidyverse)
library(sf)
library(ggimage)
# library(ggchicklet)
library(cowplot)

# Bases de datos:

# Lineas de metro, descargadas del portal de Datos Abiertos de la CDMX
lineas <- read_sf("lineas_de_metro/lineas_de_metro.shp")  %>%
  mutate(linea = str_extract(name, pattern = "\\d+|\\w+$"))
# Estaciones de metro, descargadas del portal de Datos Abiertos de la CDMX
estaciones <- read_sf("estaciones_del_metro/estaciones_del_metro.shp") %>%
  mutate(descrpt = str_extract(descrpt, pattern = "\\d+|\\w+$"))
# Modifiqué el nombre de Taxqueña porque me daba problemas más adelante
estaciones$name[156] <- "Tasquena"
# which(estaciones$name == "Tasqueña")

# Leemos la base de fechas de inauguración
# Igual extraída de la página del MEtro CDMX y procesada en Excel
fechas <- readxl::read_xlsx("fechas_lineas_metro.xlsx") %>%
  separate(col = Tramo,
           sep = " - ",
           into = c("Origen", "Destino")) %>%
  mutate(date = as.Date(str_c(Dia, mes, año, sep = "-"),
                        format = "%d-%m-%Y")) %>%
  arrange(date) %>%
  mutate(distancia = c(0, diff(date))) %>%
  mutate(linea_tiempo = cumsum(distancia))

# Cargamos las direcciones de los íconos de las estaciones relevantes:
# Estos íconos fueron obtenidos a mano de las páginas de las líneas del metro
# Podría mejorar si usamos jpegs en vez de pngs
iconos <- list.files("iconos/")
iconos_files <- tibble(src = str_c("iconos/", list.files("iconos/")),
                       mini = str_remove_all(src, pattern = "iconos/|\\.png") %>%
                         str_replace_all(pattern = "\\_", replacement = " "))

# Hacemos una tabla con los colores de las líneas del metro:
colores_metro = tibble::tribble(
  ~Linea,          ~Color, ~hex,
     "1", "Rosa Mexicano",   NA,
     "2",          "Azul",   NA,
     "3",   "Verde Olivo",   NA,
     "4",          "Cian",   NA,
     "5",      "Amarillo",   NA,
     "6",          "Rojo",   NA,
     "7",       "Naranja",   NA,
     "8",         "Verde",   NA,
     "9",          "Café",   NA,
     "A",        "Morado",   NA,
     "B",  "Verde y Gris",   NA,
    "12",           "Oro",   NA
  )
colores_metro$hex = c("#F34186",
  "#0b509c",
  "#aaa300",
  "#6eC09c",
  "#FFe200",
  "#FF1200",
  "#FF580B",
  "#017c40",
  "#311201",
  "#760173",
  "gray",
  "#AF9244")

# Estaciones unicas:
sort(unique(c(fechas$Origen,
              fechas$Destino)))

# Corroboramos que tengamos tood
sort(estaciones$name)
fechas$Origen[!(fechas$Origen %in% estaciones$name)]
fechas$Destino[!(fechas$Destino %in% estaciones$name)]

# Hacemos un vector con los nombres en minusculas de las estaciones para
# que el merge quede bien
iconos_min = iconos %>%
  str_remove(pattern = ".png") %>%
  str_replace_all(pattern = "_", replacement = " ") %>%
  str_replace_all(pattern = c("á" = "a",
                              "é" = "e",
                              "í" = "i",
                              "ó" = "o",
                              "ú" = "u"))

nom_est = str_to_lower(estaciones$name) %>%
  str_replace_all(pattern = c("á" = "a",
                              "é" = "e",
                              "í" = "i",
                              "ó" = "o",
                              "ú" = "u"))

# Estaciones:

# Estado 01:

# i =  4

# Hacemos el bucle de recorte de los shapes de las líneas:
# Como son 32 fechas de inauguración, hacemos 32 recortes:
for(i in 1:32){

    tramos_hasta = fechas %>%
      slice(i) %>%
      group_by(LÍNEA) %>%
      filter(date == max(date))

    linea = lineas %>%
      filter(linea %in% unique(tramos_hasta$LÍNEA))

    org = tramos_hasta$Origen
    dst = tramos_hasta$Destino

    estaciones_interes = estaciones %>%
      filter(name %in% c(org, dst))
    # %>%
    #   filter(descrpt %in% tramos_hasta$LÍNEA)

    estaciones_coordenadas = estaciones_interes %>%
      mutate(X = st_coordinates(.)[,1],
             Y = st_coordinates(.)[,2])

    if(!(i %in% c(23, 17, 3, 4))){

    minX = min(estaciones_coordenadas$X)
    maxX = max(estaciones_coordenadas$X)
    minY = min(st_coordinates(linea)[,2])
    maxY = max(st_coordinates(linea)[,2])

    } else {

      minY = min(estaciones_coordenadas$Y)
      maxY = max(estaciones_coordenadas$Y)
      minX = min(st_coordinates(linea)[,1])
      maxX = max(st_coordinates(linea)[,1])

    }

    cuadro <- expand.grid(x = c(minX,
                                maxX),
                          y = c(minY,
                                maxY))  %>%
      as.matrix()
    cuadro <- cuadro[c(1,2,4,3,1),]
    cuadro <- list(cuadro)
    cuadro <- st_polygon(cuadro) %>% st_sfc(crs = 4326)

    ggplot() +
      geom_sf(data = linea) +
      geom_sf(data = cuadro, fill = NA) +
      geom_sf(data = estaciones_coordenadas)

    linea_interes <- st_intersection(linea, cuadro)
    plot(linea_interes)

    st_write(linea_interes,
             str_c("sub_shapes/", ifelse(str_length(i) == 1,
                                         yes = str_c("0", i),
                                         no  = str_c( i)),
                   ".geojson"))
    print(i)

}

# Ahora si, hacemos el mapa:

# Juntamos todas las líneas recortadas en una sola base de datos:
# Cada tramo inaugurado es un renglón.
shapes_mini = lapply(str_c("sub_shapes/",
                           list.files("sub_shapes/")), st_read) %>%
  do.call(rbind, .) %>%
  left_join(colores_metro, by = c("linea" = "Linea"))

# j = 1

# Hacemos el bucle que genera 32 mapas, cada mapa con una nueva línea inaugurada pegada:
for(j in 1:32){

    tramos_hasta = fechas %>%
      slice(j)

    estaciones_interes_2 = c(tramos_hasta$Origen,
      tramos_hasta$Destino)

    estaciones_interes = estaciones %>%
      filter(name %in% estaciones_interes_2) %>%
      mutate(mini = str_to_lower(name) %>%
               str_replace_all(pattern = c("á" = "a",
                                           "é" = "e",
                                           "í" = "i",
                                           "ó" = "o",
                                           "ú" = "u"))) %>%
      left_join(iconos_files) %>%
      mutate(X = st_coordinates(.)[,1],
             Y = st_coordinates(.)[,2])

    datos_mini = shapes_mini %>%
      slice(1:j)

    map = datos_mini %>%
      ggplot() +
      geom_sf(color = datos_mini$hex,
              size = 2) +
      # geom_point(data = estaciones_interes,
      #            aes(x = X, y = Y),
      #            color = "blue",
      #            pch = 21,
      #            fill = "white",
      #            size = 3) +
      geom_image(data = estaciones_interes,
                 aes(x = X, y = Y, image = src),
                 size = 0.07) +
      scale_y_continuous(limits = c(19.27, 19.54)) +
      scale_x_continuous(limits = c(-99.25, -98.95)) +
      labs(title = str_c("Estaciones del metro, ", tramos_hasta$año),
           subtitle = str_c("Tramo ", tramos_hasta$Origen, " - ", tramos_hasta$Destino,
                            " (Línea ", tramos_hasta$LÍNEA, ")", "\n", "Inaugurado el día ", tramos_hasta$Fecha),
           x = "", y = "") +
      theme_bw()   +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            text = element_text(family = "EB Garamond"),
            plot.title = element_text(family = "EB Garamond", face = "bold", hjust = 0.5, size = 23),
            plot.subtitle = element_text(family = "EB Garamond", hjust = 0.5, size = 18))


    map# max(fechas$linea_tiempo)
    # fechas$linea_tiempo[j]

    plt_avance = tibble(x = 1,
           y = fechas$linea_tiempo[j],
           ymax = max(fechas$linea_tiempo),
           label = fechas$Fecha[j]) %>%
      ggplot(aes(x = x, y = ymax)) +
      geom_col(fill = "gray80",
                    # radius = grid::unit(15, 'mm'),
                    alpha = 1
                    ) +
    coord_flip() +
    scale_x_continuous(limits=c(0, 2)) +
      geom_col(aes(y = y), fill = "skyblue",
                    # radius = grid::unit(15, 'mm'),
                    alpha = 0.4) +
      scale_y_continuous(expand = expansion(c(0.3, 0.3), 1)) +
      geom_label(aes(y = y,
                    label = str_wrap(label, 15)),
                vjust = 0.5,
                hjust = 0.5,
                size= 3,
                family = "EB Garamond") +
      theme_void() +
      labs(caption = "Elaboración propia con datos del Portal de Datos Abiertos de la CDMX.\n") +
      theme(text = element_text(family = "EB Garamond"),
            plot.caption = element_text(hjust = 1, size = 12))

    plt_avance


    plt_final = cowplot::plot_grid(map, plt_avance, ncol = 1,align = "v", rel_heights = c(0.9, 0.2))

    plt_final

    ggsave(
           str_c("finales/",ifelse(str_length(j) == 1,
                                   yes = str_c("0", j),
                                   no  = str_c( j)), ".jpeg"),
           device = "jpeg",
           height = 8,
           width = 7.6)

    print(j)

}


# CREACIÓN DEL GIF.
library(magick)

# Carpeta donde estan las imagenes
root = "finales/"

str_c(root, list.files(root)) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("pp.gif") # Escribe el gif en el directorio.


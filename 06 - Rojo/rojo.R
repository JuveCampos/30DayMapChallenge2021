# Librerias:
library(tidyverse)
library(sf)
library(cartogram)
library(ggtext)

# Datos:
vio = readxl::read_xlsx("../DATASETS/SESNSP - Delincuencia/Municipal-Delitos-2015-2021_sep2021/2021.xlsx") %>%
  janitor::clean_names()
ents <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")


vio1 = vio %>%
  mutate(cve_municipio = ifelse(str_length(cve_municipio) == 4,
                                yes = str_c("0", cve_municipio),
                                no = str_c(cve_municipio))) %>%
  mutate(clave_ent = ifelse(str_length(clave_ent) == 1,
                            yes = str_c("0", clave_ent),
                            no = str_c(clave_ent))) %>%
  filter(tipo_de_delito == "Homicidio") %>%
  filter(subtipo_de_delito == "Homicidio culposo")

h_c = vio1 %>%
  group_by(clave_ent, entidad) %>%
  count() %>%
  print(n = Inf) %>%
  ungroup() %>%
  rename(CVE_EDO = clave_ent,
         no_homicidios = n)

abbr_estados = tibble(CVE_EDO = ents$CVE_EDO,
                      ENTIDAD = ents$ENTIDAD,
                      ABREVIATURA = c("Ags",
                                      "BC",
                                      "BCS",
                                      "Cam",
                                      "Coah",
                                      "Col",
                                      "Chis",
                                      "Chih",
                                      "CDMX",
                                      "Dgo",
                                      "Gto",
                                      "Gro",
                                      "Hgo",
                                      "Jal",
                                      "Mex",
                                      "Mich",
                                      "Mor",
                                      "Nay",
                                      "NL",
                                      "Oax",
                                      "Pue",
                                      "Qro",
                                      "QROO",
                                      "SLP",
                                      "Sin",
                                      "Son",
                                      "Tab",
                                      "Tamps",
                                      "Tlax",
                                      "Ver",
                                      "Yuc",
                                      "Zac"))

mapa <- left_join(ents, h_c) %>%
  left_join(abbr_estados) %>%
  st_transform(crs = 2163) %>%
  cartogram_dorling(weight = "no_homicidios") %>%
  mutate(area = as.numeric(st_area(.))) %>%
  mutate(ent_2 = str_extract(string = entidad, pattern = "^\\w+")) %>%
  mutate(label1 = ifelse(area >= 22439319246,
                        yes = str_c(ABREVIATURA),
                        no = NA)) %>%
  mutate(label2 = ifelse(area >= 22439319246,
                         yes = str_c(prettyNum(no_homicidios, big.mark = ",")),
                         no = NA)) %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])

# sort(mapa$area)

b1 = mapa %>%
  st_buffer(dist = 40000) %>%
  st_union()

b2 = mapa %>%
  st_buffer(dist = 80000) %>%
  st_union()

b3 = mapa %>%
  st_buffer(dist = 120000) %>%
  st_union()

reds = c("white", RColorBrewer::brewer.pal(n = 5, "Reds"))

# plot(b3, max.plot = 1)
ggplot(data = mapa) +
  # geom_sf(data = b3, fill = reds[2], color = reds[2]
  #         # ,
  #         # RColorBrewer::brewer.pal(n = 6, "YlOrRd")[1]
  #         ) +
  geom_sf(data = b2, fill = reds[3], color = reds[3]
          # ,
          # RColorBrewer::brewer.pal(n = 6, "YlOrRd")[2]
          ) +
  geom_sf(data = b1, fill = reds[4], color = reds[4]
          # ,
          # RColorBrewer::brewer.pal(n = 6, "YlOrRd")[3]
          ) +
  geom_sf(
    fill = reds[5],
    color = "white"
    # aes(fill = no_homicidios)
    ) +
  geom_text(aes(x = X, y = Y, label = label1),
            color = "white",
            family = "Poppins",
            fontface = "bold",
            size = 2,
            vjust = -0.8) +
  geom_text(aes(x = X, y = Y, label = label2),
            color = "white",
            family = "Poppins",
            size = 2,
            vjust = 1.1) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 6, "YlOrRd")[4:6]) +
  labs(title = "Número de homicidios culposos por<br>entidad federativa",
       subtitle = "Casos de Homicidio culposo registrados de Enero hasta Septiembre del 2021",
       caption = "Datos de Delitos a nivel municipal del SESNSP reportados hasta Septiembre del 2021<br>#30DayMapChallenge - Dia 06: Rojo<br>@JuvenalCamposF") +
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      face = "bold",
                                      size = 18),
        legend.position = "none",
        plot.caption = element_markdown(size = 8),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         size = 12),
        text = element_text(family = "EB Garamond",
                            color = "black"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        # panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "gray85")
                                         ,
        panel.background = element_rect(fill = "gray85"
                                          # RColorBrewer::brewer.pal(n = 6, "YlOrRd")[2]
                                        ))

ggsave("mapa_rojo.png",
       device = "png",
       dpi = 300,
       height = 6,
       width = 6)

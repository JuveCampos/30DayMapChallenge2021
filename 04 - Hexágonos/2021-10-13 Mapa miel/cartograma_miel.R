
# Librerias:
library(sf)
library(tidyverse)
library(ggtext)
# library(rayshader)

# Datos:
hex = st_read("hexagonos_estatales_dv.geojson")
plot(hex, max.plot = 1)
miel <- read_csv("2020.csv", locale = locale(encoding = "WINDOWS-1252")) %>%
  filter(Nomproducto == "Miel")
# Recordemos que la miel está en toneladas.

# Sacamos por estado la producción:
datos = miel %>%
  mutate(Cveestado = ifelse(str_length(Cveestado) == 1,
                            yes = str_c("0", Cveestado),
                            no = Cveestado)) %>%
  group_by(Cveestado, Nomestado) %>%
  summarise(prod_estatal = sum(Volumen)) %>%
  ungroup() %>%
  mutate(pp = 100*(prod_estatal/sum(prod_estatal))) %>%
  mutate(label = str_wrap(str_c(Nomestado, "\n", round(pp, 2), "%"), 10))

mapa <- left_join(hex, datos, by = c("region" = "Cveestado")) %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])

# mapa_menos = st_buffer(mapa, dist = -10)


# Gráfica:
grafica = mapa %>%
  ggplot(aes(fill = pp)) +
  geom_sf(size = 2,
          opacity = 0.8,
          color = "gray40") +
  geom_sf(data = mapa_menos,
          fill = "blue") +
  geom_text(aes(x = X,
                y = Y,
                label = label),
            size = 2.5,
            family = "EB Garamond") +
  scale_fill_gradientn(colors = c("white", "yellow", "orange"),
                       breaks = seq(0, 10, by = 2),
                       labels = scales::comma_format(suffix = "%")) +
  labs(title = "Aportación de cada estado<br>a la producción nacional de Miel de Abeja",
       subtitle = "Año 2020",
       fill = "Porcentaje de aportación (%)",
       caption = "Elaboración propia con datos del SIAP, 2020.\nSecretaría de Desarrollo Rural - México.") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid =  element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
        plot.title = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
        axis.title = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5
                               ))

grafica
ggsave("mapa_miel.png",
       device = "png",
       height = 7,
       width = 6.5)


# grafica = mapa %>%
#   ggplot(aes(fill = pp)) +
#   geom_sf(size = 2,
#           # opacity = 0.8,
#           color = "gray40") +
#   geom_text(aes(x = X,
#                 y = Y,
#                 label = label),
#             size = 3,
#             family = "EB Garamond") +
#   scale_fill_gradientn(colors = c("white", "yellow", "orange"),
#                        breaks = c(0, 3, 6, 9, 12),
#                        labels = scales::comma_format(suffix = "%")) +
#   labs(title = "Aportación de cada estado<br>a la producción nacional de Miel de Abeja",
#        subtitle = "Año 2020",
#        fill = "Porcentaje de aportación (%)",
#        caption = "Elaboración propia con datos del SIAP, 2020.\nSecretaría de Desarrollo Rural - México.") +
#   theme_bw() +
#   theme(panel.background = element_blank(),
#         panel.grid =  element_blank(),
#         axis.text = element_blank(),
#         panel.border = element_blank(),
#         axis.ticks = element_blank(),
#         plot.subtitle = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
#         plot.title = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
#         axis.title = element_blank(),
#         legend.position = "bottom") +
#   guides(fill = guide_colorbar(title.position = "top",
#                                title.hjust = 0.5,
#                                barwidth = 10,
#                                barheight = 0.5))
#
# # Rayshader:
# plot_gg(grafica,
#         multicore = TRUE,
#         width=5,
#         height=5,
#         scale=250,
#         windowsize=c(1400,866),
#         zoom = 0.55, phi = 30)
# render_snapshot()
#

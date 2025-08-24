# Instalamos la libreria, si se requiere:
# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("diegovalle/mxmaps")

# Ejemplo:
library("mxmaps")

df_mxstate_2020$value <- df_mxstate_2020$pop
mxstate_choropleth(df_mxstate_2020,
                   title = "Total population, by state")

data("df_mxstate_2020")
df_mxstate_2020$value = df_mxstate_2020$afromexican / df_mxstate_2020$pop * 100
mxhexbin_choropleth(df_mxstate_2020, num_colors = 1,
                    title = "Percentage of the population that identifies as Afro-Mexican",
                    legend = "%")

?mxhexbin_choropleth

library("viridis")
library("scales")
library(tidyverse)
data("df_mxstate_2020")
df_mxstate_2020$value = df_mxstate_2020$afromexican / df_mxstate_2020$pop
df_mxstate_2020$state_abbr_2 = str_c(df_mxstate_2020$state_abbr, "\n", round(100*df_mxstate_2020$value, 1), "%")


# Will show a warning, look at the municipio examples to see how to remove it
?mxhexbin_choropleth
plt = mxhexbin_choropleth(df_mxstate_2020,
                    num_colors = 1,
                    label_color = "white",
                    shadow_color = "black",
                    title = "Percentage of the population that identifies as Afro-Mexican",
                    legend = "%",
                    label_size = 2.8) +
  scale_fill_viridis("percentage",
                     labels = percent) +
  theme(legend.position = "bottom",
        plot.title.position = "plot")
# ?mxhexbin_choropleth
plt

plt$coordinates
bd <- plt$data
plt$labels
plt$labels

# plot(plt)

r = left_join(plt$data,
          df_mxstate_2020 %>% select(state_abbr, state_abbr_2))

plt$data = plt$data %>%
  mutate(state_abbr = r$state_abbr_2)

plt$data
plt

# plotly::ggplotly(plt)
#
# class(plt)
# ?df_mxstate
#
# df_mxstate$state_abbr = df_mxstate_2020$state_abbr

library(sf)
bd = as_tibble(bd)
# bd_sf = as_tibble(bd) %>%
#   st_as_sf(crs = 4326,
#            coords = c(long,lat))

hexs = lapply(unique(bd$id), function(x){
  # x = bd$id[1]
  r = bd %>%
    filter(id == x) %>%
    select(long, lat) %>%
    as.matrix() %>%
    list() %>%
    st_polygon() %>%
    st_sfc() %>%
    st_cast(to = "POLYGON") %>%
    st_sf()

  r$state_abbr = bd %>%
    filter(id == x) %>%
    pull(state_abbr) %>%
    first()

  return(r)
})



a = hexs %>%
  do.call(rbind, .)

a2 = left_join(a, df_mxstate)

a2$state_abbr[a2$state_abbr == "DF"] <- "CDMX"
a2$region[a2$state_abbr == "CDMX"] <- "09"
a2$state_name[a2$state_abbr == "CDMX"] <- "Ciudad de México"
a2$state_name_official[a2$state_abbr == "CDMX"] <- "Ciudad de México"
a2$state_abbr_official[a2$state_abbr == "CDMX"] <- "CDMX"

hexagonos_chidos <- a2 %>%
  select(state_abbr, region,  state_name,
         state_name_official,state_abbr_official )

st_write(hexagonos_chidos,
         "hexagonos_estatales_dv.geojson")


ggplot(data = bd,
       aes(x = long,
                      y = lat,
                      group = group,
                      fill = value)) +
  geom_polygon(color = "white") +
  geom_text(aes(label = state_abbr))


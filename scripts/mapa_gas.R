## Remake de Estimación de TAR 
# DAOA
# 02/02/2024

# Preparar Espacio --------------------------------------------------------

dev.off()
rm(list = ls())
pacman::p_load(tidyverse, scales, sf, janitor, readxl, leaflet, RColorBrewer, htmlwidgets, rmapshaper)
options(scipen = 999)

# Cargar Datos ------------------------------------------------------------

mapa_municipios <- read_sf('/Users/dortega/Desktop/marco_geoestadistico/mg_2024_integrado/conjunto_de_datos/00mun.shp')
precios <- read_excel('input/ubi_tar_precios_act.xlsx')

# 60 mbytes
object.size(mapa_municipios)/(1024*1024)

# 2.8 mbytes
mapa_municipios <- ms_simplify(mapa_municipios, keep = 0.01,keep_shapes = FALSE)
object.size(mapa_municipios)/(1024*1024)

# Analisis ----------------------------------------------------------------

precios <- precios %>% mutate(CVE_ENT = str_pad(cve_entidad, side = 'left', pad = '0', width = 2), 
                              CVE_MUN = str_pad(cve_municipio, side = 'left', pad = '0', width = 3),
                              CVEGEO = paste0(CVE_ENT,CVE_MUN))

lista_proveedores <- precios %>% group_by(CVEGEO) %>% summarise(lista_proveedores = paste(nombre_tar, collapse = ", "), .groups = "drop")


precios <- precios %>% group_by(CVEGEO) %>% summarise(precio_tar_promedio = mean(precio_tar, na.rm = T), 
                                                      precio_vendedores = mean(precio_vendedores), 
                                                      ganancia = mean(ganancia), 
                                                      n_proveedores = n_distinct(nombre_tar)) %>% 
  left_join(lista_proveedores)

mapa <- mapa_municipios %>% left_join(precios) 

max(precios$ganancia)

mapa <- mapa %>% mutate(cuartiles = case_when(ganancia <2.13 ~ '2.13', 
                                              ganancia >=2.13 &  ganancia <2.39 ~'2.13 - 2.39',
                                              ganancia >=2.39 &  ganancia <2.66 ~'2.39 - 2.66',
                                              ganancia >=2.66 &  ganancia <2.98 ~'2.66 - 2.98',
                                              ganancia >=2.98 ~'2.98 - 3.5', 
                                              T ~ 'Sin Información'
))
# Map ---------------------------------------------------------------------

labels_0 <- sprintf(
  "Información Municipal<br/><strong>Municipio:</strong> %s<br/><strong>Precio promedio TAR:</strong> %g<br/><strong>Precio promedio vendedores:</strong> %g<br/><strong>Ganancia promedio:</strong> %g<br/><strong>Número de proveedores:</strong> %g",
  mapa[mapa$cuartiles=='Sin Información', ]$NOMGEO, mapa[mapa$cuartiles=='Sin Información', ]$precio_tar_promedio, mapa[mapa$cuartiles=='Sin Información', ]$precio_vendedores, mapa[mapa$cuartiles=='Sin Información', ]$ganancia, mapa[mapa$cuartiles=='Sin Información', ]$n_proveedores
) %>% lapply(htmltools::HTML)

labels_1 <- sprintf(
  "Información Municipal<br/><strong>Municipio:</strong> %s<br/><strong>Precio promedio TAR:</strong> %g<br/><strong>Precio promedio vendedores:</strong> %g<br/><strong>Ganancia promedio:</strong> %g<br/><strong>Número de proveedores:</strong> %g",
  mapa[mapa$cuartiles == "2.13" & !is.na(mapa$cuartiles), ]$NOMGEO, mapa[mapa$cuartiles == "2.13" & !is.na(mapa$cuartiles), ]$precio_tar_promedio, mapa[mapa$cuartiles == "2.13" & !is.na(mapa$cuartiles), ]$precio_vendedores, mapa[mapa$cuartiles == "2.13" & !is.na(mapa$cuartiles), ]$ganancia, mapa[mapa$cuartiles == "2.13" & !is.na(mapa$cuartiles), ]$n_proveedores
) %>% lapply(htmltools::HTML)

labels_2 <- sprintf(
  "Información Municipal<br/><strong>Municipio:</strong> %s<br/><strong>Precio promedio TAR:</strong> %g<br/><strong>Precio promedio vendedores:</strong> %g<br/><strong>Ganancia promedio:</strong> %g<br/><strong>Número de proveedores:</strong> %g",
  mapa[mapa$cuartiles == "2.13 - 2.39"& !is.na(mapa$cuartiles), ]$NOMGEO, mapa[mapa$cuartiles == "2.13 - 2.39"& !is.na(mapa$cuartiles), ]$precio_tar_promedio, mapa[mapa$cuartiles == "2.13 - 2.39"& !is.na(mapa$cuartiles), ]$precio_vendedores, mapa[mapa$cuartiles == "2.13 - 2.39"& !is.na(mapa$cuartiles), ]$ganancia, mapa[mapa$cuartiles == "2.13 - 2.39"& !is.na(mapa$cuartiles), ]$n_proveedores
) %>% lapply(htmltools::HTML)

labels_3 <- sprintf(
  "Información Municipal<br/><strong>Municipio:</strong> %s<br/><strong>Precio promedio TAR:</strong> %g<br/><strong>Precio promedio vendedores:</strong> %g<br/><strong>Ganancia promedio:</strong> %g<br/><strong>Número de proveedores:</strong> %g",
  mapa[mapa$cuartiles == "2.39 - 2.66"& !is.na(mapa$cuartiles), ]$NOMGEO, mapa[mapa$cuartiles == "2.39 - 2.66"& !is.na(mapa$cuartiles), ]$precio_tar_promedio, mapa[mapa$cuartiles == "2.39 - 2.66"& !is.na(mapa$cuartiles), ]$precio_vendedores, mapa[mapa$cuartiles == "2.39 - 2.66"& !is.na(mapa$cuartiles), ]$ganancia, mapa[mapa$cuartiles == "2.39 - 2.66"& !is.na(mapa$cuartiles), ]$n_proveedores
) %>% lapply(htmltools::HTML)

labels_4 <- sprintf(
  "Información Municipal<br/><strong>Municipio:</strong> %s<br/><strong>Precio promedio TAR:</strong> %g<br/><strong>Precio promedio vendedores:</strong> %g<br/><strong>Ganancia promedio:</strong> %g<br/><strong>Número de proveedores:</strong> %g",
  mapa[mapa$cuartiles == "2.66 - 2.98"& !is.na(mapa$cuartiles), ]$NOMGEO, mapa[mapa$cuartiles == "2.66 - 2.98"& !is.na(mapa$cuartiles), ]$precio_tar_promedio, mapa[mapa$cuartiles == "2.66 - 2.98"& !is.na(mapa$cuartiles), ]$precio_vendedores, mapa[mapa$cuartiles == "2.66 - 2.98"& !is.na(mapa$cuartiles), ]$ganancia, mapa[mapa$cuartiles == "2.66 - 2.98"& !is.na(mapa$cuartiles), ]$n_proveedores
) %>% lapply(htmltools::HTML)

labels_5 <- sprintf(
  "Información Municipal<br/><strong>Municipio:</strong> %s<br/><strong>Precio promedio TAR:</strong> %g<br/><strong>Precio promedio vendedores:</strong> %g<br/><strong>Ganancia promedio:</strong> %g<br/><strong>Número de proveedores:</strong> %g",
  mapa[mapa$cuartiles == "2.98 - 3.5"& !is.na(mapa$cuartiles), ]$NOMGEO, mapa[mapa$cuartiles == "2.98 - 3.5"& !is.na(mapa$cuartiles), ]$precio_tar_promedio, mapa[mapa$cuartiles == "2.98 - 3.5"& !is.na(mapa$cuartiles), ]$precio_vendedores, mapa[mapa$cuartiles == "2.98 - 3.5"& !is.na(mapa$cuartiles), ]$ganancia, mapa[mapa$cuartiles == "2.98 - 3.5"& !is.na(mapa$cuartiles), ]$n_proveedores
) %>% lapply(htmltools::HTML)
# Color pallet
levels(mapa$cuartiles)

pal <- colorFactor(
  palette = c("#F2F2F2", "#D1D3A2", "#798234", "#A3AD62", "#323616", "grey"),
  domain = mapa$cuartiles
)

# fix crs
st_crs(mapa)
mapa <- st_transform(mapa, crs = 4326)

# Leaflet
# leaflet(mapa) %>% 
#   addProviderTiles(providers$OpenStreetMap) %>% 
#   addPolygons(fillColor = ~pal(cuartiles),
#               weight = 0.1,
#               color = "black",
#               opacity = 1,
#               dashArray = "1",
#               fillOpacity = 0.7,
#               highlightOptions = highlightOptions(
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 0.7,
#                 bringToFront = TRUE), 
#               label = labels,
#               labelOptions = labelOptions(
#                 style = list("font-weight" = "normal", padding = "3px 8px"),
#                 textsize = "15px",
#                 direction = "auto")) %>% 
#   addLegend(
#     pal = pal,
#     values = ~cuartiles,
#     title = "Rango de ganancias (quintiles)",
#     position = "topright"
#   )



# Leaflet 2
mapa_toggles <- leaflet(mapa) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(
    data = mapa[mapa$cuartiles=='Sin Información', ],
    fillColor = ~pal(cuartiles),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    group = "Sin información", 
    label = labels_0,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    data = mapa[mapa$cuartiles == "2.13" & !is.na(mapa$cuartiles), ],
    fillColor = ~pal(cuartiles),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    group = "0 - 2.13", 
    label = labels_1,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    data = mapa[mapa$cuartiles == "2.13 - 2.39"& !is.na(mapa$cuartiles), ],
    fillColor = ~pal(cuartiles),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    group = "2.13 - 2.39", 
    label = labels_2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    data = mapa[mapa$cuartiles == "2.39 - 2.66"& !is.na(mapa$cuartiles), ],
    fillColor = ~pal(cuartiles),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    group = "2.39 - 2.66", 
    label = labels_3,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    data = mapa[mapa$cuartiles == "2.66 - 2.98"& !is.na(mapa$cuartiles), ],
    fillColor = ~pal(cuartiles),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    group = "2.66 - 2.98", 
    label = labels_4,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    data = mapa[mapa$cuartiles == "2.98 - 3.5"& !is.na(mapa$cuartiles), ],
    fillColor = ~pal(cuartiles),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    group = "2.98 - 3.5", 
    label = labels_5,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  # Add layer control for toggling quintile groups
  addLayersControl(
    overlayGroups = c(
      "Sin información", 
      "0 - 2.13", 
      "2.13 - 2.39", 
      "2.39 - 2.66", 
      "2.66 - 2.98", 
      "2.98 - 3.5"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(
    pal = pal,
    values = ~cuartiles,
    title = "Rango de ganancias (quintiles)",
    position = "topleft"
  )

mapa_toggles

# Exportar Mapa -----------------------------------------------------------

write_csv(precios, 'output/precios.csv')

saveWidget(mapa_toggles, "output/map_precios_02012025.html")

mapa_toggles

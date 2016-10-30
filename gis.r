library(sp)
library(rgdal)
library(geojsonio)
library(ggmap)
library(maptools)
library(RColorBrewer)
library(leaflet)


layers1 <- ogrListLayers("../gis_data/dpd/DPDPLZ_20161024.shp")

shape1 <- readOGR("../gis_data/dpd/DPDPLZ_20161024.shp", 
                  layer = "DPDPLZ_20161024")

plot(shape1)

file_to_geojson("../gis_data/dpd/DPDPLZ_20161024.shp",
                output = "../gis_data/DPDPLZ_20161024",
                method = "local")

# #############################################################################

area <- readShapePoly("../gis_data/dpd/DPDPLZ_20161024.shp")

colors <- brewer.pal(9, "BuGn")

mapImage <- get_map(location = c(lon = 9.5, lat = 51),
                    color = "color",
                    source = "google",
                    maptype = "roadmap",
                    zoom = 6)

area.points <- fortify(area)


ggmap(mapImage) +
  geom_polygon(
    aes(x = long,
        y = lat,
        group = group),
    data = area.points[c(1:1000),],
    color = colors[9],
    fill = colors[6],
    alpha = 0.5
  ) +
  labs(x = "Longitude",
       y = "Latitude")


# #############################################################################


shapes <- readOGR("../gis_data/dpd/DPDPLZ_20161024.shp",
                  layer = "DPDPLZ_20161024",
                  verbose = FALSE)

leaflet(shapes) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
  )

# #############################################################################

topoData <- readLines("e:/ubuntu_transfer/dpd_3.json") %>% paste(collapse = "\n")

leaflet() %>% setView(lng = 7, lat = 51, zoom = 6) %>%
  addTiles() %>%
  addTopoJSON(topoData, weight = 1, color = "#444444", fill = FALSE)


# #############################################################################

ogrDrivers()

# #############################################################################

layers <- ogrListLayers("e:/ubuntu_transfer/dpd_3.json")

shape <- readOGR("e:/ubuntu_transfer/dpd_3.json", 
                 layer = "OGRGeoJSON")

plot(shape)

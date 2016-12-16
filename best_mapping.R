
file <- "https://opendata.socrata.com/api/views/ddym-zvjk/rows.csv"
starbucks <- read.csv(file)
library(leaflet); library(magrittr)
leaflet() %>% 
  addTiles() %>%
  setView(-84.3847, 33.7613, zoom = 15) %>% 
  addMarkers(data = starbucks, lat = ~ Latitude, lng = ~ Longitude, popup = starbucks$Location)
# addCircleMarkers(data = starbucks, lat = ~ Latitude, lng = ~ Longitude, popup = starbucks$Features...Service)


library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


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
  addPolygons(fillColor = topo.colors(5, alpha = .5), stroke = F)

?addPolygons()
?topo.colors()

### Get lat/long from Address ####
geocodeAdddress <- function(address) {
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  lat_long_list <- lapply(
    address,
    function(i){
      url <- URLencode(
        paste(
          url, 
          i, 
          "&sensor=false", 
          sep = "")
      )
      x <- RJSONIO::fromJSON(url, simplify = FALSE)
      
      if (x$status == "OK") {
        out <- c(x$results[[1]]$geometry$location$lng,
                 x$results[[1]]$geometry$location$lat)
      } else {
        out <- NA
      } 
      Sys.sleep(0.4)
      out
    }
  ) %>% 
    set_names(address)
  return(lat_long_list)
}

x <- geocodeAdddress(c("729 15th St NW, Washington, DC", "Tampa, FL", "Wisconsin", "Florida"))
bind_rows(x) %>% 
  t() %>% 
  as.data.frame() %>% 
  transmute(address = row.names(.), Lat = V1, Lon = V2)



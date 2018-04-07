#Script for Lines on Leaflet Maps

library(leaflet)
library(magrittr)

guthriezones <- read.csv("Guthrie_Zones_Locations.csv")

mydf <- data.frame(Observation = c("A-B", "B-C"),
                   From = c("A","B"),
                   To = c("B","C"),
                   InitialLat = c(4.1,-5.8),
                   InitialLong = c(9.7,18.4),
                   NewLat = c(-5.8,1.1),
                   NewLong = c(18.4,23.15),
                   stringsAsFactors = FALSE)

mydf2 <- data.frame(group = c("A-B", "B-C"),
                    lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))

#Read Shapefile
library(rgdal)
guthrieshape <- readOGR("Guthrie_Zones.shp")

#Render Map
leaflet() %>%
  addPolygons(data = guthrieshape,
              color = "#CCCCCC",
              weight = 0.5,
              smoothFactor = 1,
              opacity = 1.0,
              fillOpacity = 0,
              noClip = TRUE) %>%
  #addProviderTiles(providers$Stamen.TonerBackground) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(data = guthriezones,
                   lat = ~Latitude,
                   lng = ~Longitude,
                   color = "#F45F5F",
                   fillColor = "FFFFFF",
                   fillOpacity = 0.1,
                   label = ~ ~Zone,
                   labelOptions = labelOptions(noHide = TRUE,
                                               direction = "bottom",
                                               textOnly = TRUE,
                                               style = list("color" = "white"))) %>%
  addPolylines(data = mydf2,
               lng = ~long,
               lat = ~lat,
               group = ~group,
               color = "#FF0000",
               popup = ~group)

library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)


setwd("c://Users/leandro/Dropbox/Docs/github/UnaImagen/DataBases/Censo2011/")
setwd("~/Dropbox/Docs/github/UnaImagen/DataBases/Censo2011/")


prueba <- read_sf("basefinal.shp")


#### ----------------- Grafico -----------------


cuts <- quantile(prueba$personaskm, probs = seq(0,1, by=0.1))
display.brewer.all()
pal <- colorBin("RdYlBu", domain = prueba$personaskm, bins = cuts, reverse = T)


m <- leaflet(prueba) %>%
   addTiles() %>%
   setView(-56.1, -32, zoom = 7)  %>%
   addPolygons(
      fillColor = ~pal(personaskm),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
         weight = 5,
         color = "#666",
         dashArray = "",
         fillOpacity = 0.7,
         bringToFront = TRUE)) %>%
   addLegend(pal = pal, values = ~pob, opacity = 0.7, title = NULL,
             position = "bottomright")
m



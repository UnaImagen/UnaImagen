library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)


ShapeCenso2011 <- read_sf("ShapeCenso2011.shp")


#### ----------------- Grafico -----------------


cuts <- quantile(ShapeCenso2011$personaskm, probs = seq(0,1, by=0.1))
#display.brewer.all()
pal <- colorBin("RdYlBu", domain = ShapeCenso2011$personaskm, bins = cuts, reverse = T)


leaflet(ShapeCenso2011) %>%
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
         bringToFront = TRUE),
      label = ~personaskm,
      labelOptions = labelOptions(
         style = list("font-weight" = "normal", padding = "3px 8px"),
         textsize = "15px",
         direction = "auto")) %>%
   addLegend(pal = pal, values = ~pob, opacity = 0.7, title = NULL,
             position = "bottomright")

library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)
library(here)

ShapeCenso2011 <- read_sf("./DataBases/Censo2011/ShapeCenso2011.shp")



#### Informacion previa ------------------------------------------------------

x=100
max(ShapeCenso2011$personaskm)
h1 = hist(ShapeCenso2011$personaskm, breaks =
             seq(0,73500, by = x))
h1$density = h1$counts/sum(h1$counts)*100

plot(h1,
     freq=FALSE,
     main = "",
     ylim = c(0,25),
     xlim = c(0,30000),
     cex.main=1.4, cex.lab=1.5,cex.axis=1.2,
     xlab = "Personas por km2",
     ylab = "Porcentaje",
     col = "black",
     border = F,
     las = 1)



#### ----------------- Grafico -----------------


#cuts <- quantile(ShapeCenso2011$personaskm, probs = seq(0,1, by=0.1))
#display.brewer.all()
ShapeCenso2011$personaskm <- round(ShapeCenso2011$personaskm, digits = 0)
cuts <- stats::quantile(ShapeCenso2011$personaskm,
                        probs = base::c(0, 0.3, 0.4, 0.6, 0.8, 0.85, 0.9, 0.95, 0.96, 0.99, 0.995, 1))
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

library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(haven)

setwd("c://Users/leandro/Dropbox/Docs/1imagen-data/ine/")
setwd("~/Dropbox/Docs/1imagen-data/ine/")

# direccion datos vectoriales: http://ine.gub.uy/documents/10181/18006/Mapas+Vectoriales+a%C3%B1o+2011/97dbcd58-80a8-472c-86cc-e8ecdaafef99

### Levanto mapa del INE con los poligonos -----------------------------

mapaUy <- read_sf("ine_seg_11.shp")
mapaUy <- mapaUy[,8]
mapaUy <- mapaUy[!duplicated(mapaUy$CODSEG),]
mapaUy = st_set_crs(mapaUy, "+proj=utm +zone=21 +south")
st_crs(mapaUy)
mapaUy <- st_transform(mapaUy, "+proj=longlat +datum=WGS84")


#### ------------------- si funciona lo anterior ----------------


mapaUy$CODSEG <- formatC(mapaUy$CODSEG, width=7, flag="0") # fix 7 numbers


### Levanto base de censo -----------------------------

# Base censo: http://www.ine.gub.uy/c/document_library/get_file?uuid=23d15ef3-e5ed-46e8-9bec-bff7ecc604d2&groupId=10181

personas <- readRDS("personas.rds")
personas <- as.data.frame(personas)
head(personas)
colnames(personas)

# creo variable
personas$DPTO <- formatC(personas$DPTO, width=2, flag="0") # fix 2 numbers
personas$SECC <- formatC(personas$SECC, width=2, flag="0") # fix 2 numbers
personas$SEGM <- formatC(personas$SEGM, width=3, flag="0") # fix 3 numbers

personas$CODSEG <- paste0(personas$DPTO,personas$SECC,personas$SEGM)
length(table(personas$CODSEG))


#### Calculo la variable de interes

# personas por codigos
data <- as.data.frame(unique(personas$CODSEG))
colnames(data)[1] <- "CODSEG"
# data$CODSEG <- as.integer(factor(data$CODSEG))

data$pob <- by(personas$PERPH02, personas$CODSEG, nrow)
data$pob <- data$pob / max(data$pob)
# rm(personas)


#### Creo la nueva base ------------------------

prueba <- merge(mapaUy,data,by="CODSEG")
names(prueba)
sum(is.na(prueba$pob))
plot(prueba$pob) # chequeo que este todo ok
st_write(prueba, "basefinal.shp")


#### ----------------- Grafico -----------------

cuts <- quantile(prueba$pob, probs = seq(0,1, by=0.2))
pal <- colorBin("YlOrRd", domain = prueba$pob, bins = cuts)


m <- leaflet(prueba) %>%
  addTiles() %>%
  setView(-56.1, -32, zoom = 6)  %>%
  addPolygons(
    fillColor = ~pal(pob),
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



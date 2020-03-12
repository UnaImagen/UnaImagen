library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)
library(haven)


### ------------ Poligonos vectoriales del INE -----------------------------

# Disponible en: http://ine.gub.uy/documents/10181/18006/Mapas+Vectoriales+a%C3%B1o+2011/97dbcd58-80a8-472c-86cc-e8ecdaafef99

# Departamentos
mapaUydeptos <- read_sf(here::here("./DataBases/IneShapeFiles/ine_depto.shp"))
colnames(mapaUydeptos)
st_crs(mapaUydeptos) # no hay información
mapaUydeptos = st_set_crs(mapaUydeptos, "+proj=utm +zone=21 +south") # no esta establecido el tipo de información
st_crs(mapaUydeptos) # ahora si
mapaUydeptos <- st_transform(mapaUydeptos, "+proj=longlat +datum=WGS84") # transformo info utm a lat long

# Barrios de Montevideo
mapaMdeoBarrios <- read_sf(here::here("./DataBases/IneShapeFiles/ine_barrios_mvd_nbi85.shp"))
colnames(mapaMdeoBarrios)
st_crs(mapaMdeoBarrios) # no hay información
mapaMdeoBarrios = st_set_crs(mapaMdeoBarrios, "+proj=utm +zone=21 +south") # no esta establecido el tipo de información
st_crs(mapaMdeoBarrios) # ahora si
mapaMdeoBarrios<- st_transform(mapaMdeoBarrios, "+proj=longlat +datum=WGS84") # transformo info utm a lat long

# Union de shape files
head(mapaUydeptos)
colnames(mapaUydeptos)
head(mapaMdeoBarrios)
colnames(mapaMdeoBarrios)
colnames(mapaUydeptos)[1] <- colnames(mapaMdeoBarrios)[1]
mapaUydeptos <- mapaUydeptos[,c(1,4,6)]
mapaMdeoBarrios <- mapaMdeoBarrios[,c(1,2,4)]
colnames(mapaMdeoBarrios)[2] <- colnames(mapaUydeptos)[2]
mapaUydeptos <- mapaUydeptos[-which(mapaUydeptos$NOMBRE == "LIMITE CONTESTADO"),]
mapaUydeptos <- mapaUydeptos[-which(mapaUydeptos$NOMBRE == "MONTEVIDEO"),]

mapa <- rbind(mapaUydeptos,mapaMdeoBarrios)

# plot(mapa)
#
# mapaUy$CODSEG <- formatC(mapaUy$CODSEG, width=7, flag="0") # codigo segmento con 7 numeros


### ------------ Información ECH 2018 -----------------------------

# Información de Encuesta continua de hogares tomada de:
# http://www.ine.gub.uy/encuesta-continua-de-hogares1

ing <- read_sav(here::here("./DataBases/ECH/2018/H_2018_Terceros.sav"))
colnames(ing)

ing <- ing[,c(3,4,5,10,12,16,160)]
head(ing)

prueba <- ing %>% group_by(nomdpto) %>%
   summarise(x = weighted.mean(YSVL, pesoano))

prueba2 <- ing %>% group_by(nombarrio) %>%
   summarise(x = weighted.mean(YSVL, pesoano))
prueba2 <- prueba2[-1,]

# Arreglo nombre de barrios
prueba2$NOMBRE <- sort(mapaMdeoBarrios$NOMBRE)

prueba$ingUSD <- round(prueba$x / 30.725, digits = 0) # Dolar promedio tomado del Banco Mundial
prueba2$ingUSD <- round(prueba2$x / 30.725, digits = 0)

# ajusto nombre de las bases
colnames(prueba)[1] <- "NOMBRE"
colnames(prueba2)[1] <- "NOMBRE"

base <- rbind(prueba, prueba2)
colnames(base)[2] <- "IngPesos"
rm(ing)


### ------------ Merge de bases  -----------------------------


mapa <- merge(mapa, base, by = "NOMBRE")
st_write(mapa, (here::here("./DataBases/ECH/2018/ShapeECH2018.shp")))


#### ----------------- Grafico -----------------


mapa <- read_sf(here::here("./DataBases/ECH/2018/ShapeECH2018.shp"))

# display.brewer.all()
pal <- colorNumeric(palette = "Blues", domain = mapa$ingUSD)

leaflet(mapa) %>%
   addTiles() %>%
   setView(-56.1, -32, zoom = 7)  %>%
   addPolygons(
      fillColor = ~pal(ingUSD),
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
      label = sprintf(paste0("%s: %s"," ", "UDS"), mapa$NOMBRE, mapa$ingUSD),
      labelOptions = labelOptions(
         style = list("font-weight" = "normal", padding = "3px 8px"),
         textsize = "15px",
         direction = "auto"))
   # addLegend(pal = pal, values = ~pob, opacity = 0.7, title = NULL,
   #           position = "bottomright")

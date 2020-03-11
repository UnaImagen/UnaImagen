library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(haven)
library(colorspace)
library(RColorBrewer)



### ------------ Poligonos vectoriales del INE -----------------------------

# Disponible en: http://ine.gub.uy/documents/10181/18006/Mapas+Vectoriales+a%C3%B1o+2011/97dbcd58-80a8-472c-86cc-e8ecdaafef99


mapaUy <- read_sf(here::here("./DataBases/IneShapeFiles/ine_seg_11.shp"))
colnames(mapaUy)
mapaUy <- mapaUy[,c(1,8)]
mapaUy <- mapaUy[!duplicated(mapaUy$CODSEG),] #hay segmentos duplicados
mapaUy = st_set_crs(mapaUy, "+proj=utm +zone=21 +south") # no esta establecido el tipo de información
st_crs(mapaUy) # ahora si
mapaUy <- st_transform(mapaUy, "+proj=longlat +datum=WGS84") # transformo info utm a lat long
mapaUy$CODSEG <- formatC(mapaUy$CODSEG, width=7, flag="0") # codigo segmento con 7 numeros


### ---------------------- Base de censo -----------------------------

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
#data$pob <- data$pob / max(data$pob)
# rm(personas)


#### Creo la nueva base

prueba <- merge(mapaUy,data,by="CODSEG")
names(prueba)
sum(is.na(prueba$pob))
plot(prueba$pob) # chequeo que este todo ok
prueba$AREA2 <- (prueba$AREA)/1000000
prueba$personaskm <-  round(prueba$pob / prueba$AREA2, digits = 0)
st_write(prueba, "ShapeCenso2011.shp")


#### ------------------------ Fin de rutina -------------------------------


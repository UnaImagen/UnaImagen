library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)
library(haven)


### ------------ Poligonos vectoriales del INE -----------------------------

# Archivo de polígonos vectoriales disponible en:
# http://ine.gub.uy/documents/10181/18006/Mapas+Vectoriales+a%C3%B1o+2011/97dbcd58-80a8-472c-86cc-e8ecdaafef99


## - Información de departamentos -

# carga de la base
mapaUydeptos <- read_sf(here::here("./DataBases/IneShapeFiles/ine_depto.shp"))
colnames(mapaUydeptos)
# no hay información sobre el tipo de información geográfica
st_crs(mapaUydeptos)
# fija el tipo de información de acuerdo al INE
mapaUydeptos = st_set_crs(mapaUydeptos, "+proj=utm +zone=21 +south")
# verifico informacion geográfica
st_crs(mapaUydeptos)
# transforma información utm a lat long
mapaUydeptos <- st_transform(mapaUydeptos, "+proj=longlat +datum=WGS84")


## - Infromación de barrios de Montevideo -

# carga de la base
mapaMdeoBarrios <- read_sf(here::here("./DataBases/IneShapeFiles/ine_barrios_mvd_nbi85.shp"))
colnames(mapaMdeoBarrios)
# no hay información sobre el tipo de información geográfica
st_crs(mapaMdeoBarrios)
# fija el tipo de información de acuerdo al INE
mapaMdeoBarrios = st_set_crs(mapaMdeoBarrios, "+proj=utm +zone=21 +south")
# verifico informacion geográfica
st_crs(mapaMdeoBarrios)
# transforma información utm a lat long
mapaMdeoBarrios<- st_transform(mapaMdeoBarrios, "+proj=longlat +datum=WGS84")

## - Union de shape files de departamentos y barrios -

head(mapaUydeptos)
colnames(mapaUydeptos)
head(mapaMdeoBarrios)
colnames(mapaMdeoBarrios)
# asigno el mismo nombre a las variables
colnames(mapaUydeptos)[1] <- colnames(mapaMdeoBarrios)[1]
# restrinjo la información necesaria
mapaUydeptos <- mapaUydeptos[,c(1,4,6)]
mapaMdeoBarrios <- mapaMdeoBarrios[,c(1,2,4)]
# asigno mismo nombre a variables
colnames(mapaMdeoBarrios)[2] <- colnames(mapaUydeptos)[2]
# eliminamos límite contestado del mapa
mapaUydeptos <- mapaUydeptos[-which(mapaUydeptos$NOMBRE == "LIMITE CONTESTADO"),]
# eliminamos Montevideo como departamento, para usar los barrios de la ciudad
mapaUydeptos <- mapaUydeptos[-which(mapaUydeptos$NOMBRE == "MONTEVIDEO"),]

# se crea el mapa
mapa <- rbind(mapaUydeptos,mapaMdeoBarrios)
# verifico que el mapa tenga la información
plot(mapa)


### ------------ Información ECH 2018 -----------------------------

# Información de Encuesta continua de hogares tomada de:
# http://www.ine.gub.uy/encuesta-continua-de-hogares1

ing <- read_sav(here::here("./DataBases/ECH/2018/H_2018_Terceros.sav"))
colnames(ing)

# Utilizo las variables de interés, de acuerdo a la información de códigos
# del archivo "Diccionario de Variables ECH 2018.xls"
ing <- ing[,c(3,4,5,10,12,16,160)]
head(ing)

# promedios de ingreso por departamento
info_depto <- ing %>% group_by(nomdpto) %>%
   summarise(x = weighted.mean(YSVL, pesoano))

# promedios de ingreso por barrio de montevideo
info_barrio <- ing %>% group_by(nombarrio) %>%
   summarise(x = weighted.mean(YSVL, pesoano))
info_barrio <- info_barrio[-1,]

# Arreglo nombre de barrios
info_barrio$NOMBRE <- sort(mapaMdeoBarrios$NOMBRE)

# Pas ingresos de pesos a dólares (dolar promedio 2018 tomado del Banco Mundial)
info_depto$ingUSD <- round(info_depto$x / 30.725, digits = 0)
info_barrio$ingUSD <- round(info_barrio$x / 30.725, digits = 0)

# ajusto nombre de las variables
colnames(info_depto)[1] <- "NOMBRE"
colnames(info_barrio)[1] <- "NOMBRE"

# uno las bases de barrios y departamentos
base <- rbind(info_depto, info_barrio)
colnames(base)[2] <- "IngPesos"
rm(ing)


### ------------ Merge de bases de shape e información  -----------------------------

# merge de bases de shape e información
mapa <- merge(mapa, base, by = "NOMBRE")
# guardar base final
st_write(mapa, (here::here("./DataBases/ECH/2018/ShapeECH2018.shp")))


### ----------------- Fin de código -----------------
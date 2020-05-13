
install.packages("rgdal")
install.packages("dplyr")
install.packages("leaflet")
install.packages("sf")
install.packages("RColorBrewer")
install.packages("haven")
install.packages("here")
install.packages("formattable")
install.packages("widgetframe")
install.packages("blogdown")
install.packages("Rcpp")





library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)
library(haven)
library(here)
library(readr)
library(formattable)
library(widgetframe)
library(blogdown)

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

plot(mapaUydeptos)


### ------------ Indicadores Asistenciales ASSE -----------------------------

# Base de datos elaborada en base a Indicadores Asistenciales de ASSE publicados por JUNASA

# Indicadores_asistenciales_ASSE_2013.2019 <- read.csv("~/Documents/Una Imagen/Indicadores_asistenciales_ASSE_2013-2019.csv") %>%
#   mutate(valor=as.numeric(valor)) %>%
#   write_rds("Databases/ASSE/ASSE.rds")

ces <- read_rds("./DataBases/ASSE/ASSE.rds")
head(ces)

# Utilizo las variables de interés, en este caso me quedo con el indicador de cesáreas
# indice de cesareas en 2019 por departamento

info_depto <- filter(ces, año == 2019 & codigo == "Cal_3") %>%
   group_by(departamento) %>%
   summarise(x = valor) %>%
   mutate(NOMBRE = toupper(departamento))  %>%
   transmute(NOMBRE,
             "ind_ces"= x)
rm(ces)

### ------------ Merge de bases de shape e información  -----------------------------

# merge de bases de shape e información
mapa <- merge(mapaUydeptos, info_depto, by= "NOMBRE")

# guardar base final
st_write(mapa, (here::here("./DataBases/ASSE/ShapeASSE2019.shp")))


### ----------------- Fin de código -----------------






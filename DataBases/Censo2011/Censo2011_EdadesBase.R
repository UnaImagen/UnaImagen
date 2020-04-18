## Librerías utilizadas

library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(haven)
library(colorspace)
library(RColorBrewer)
library(data.table)



### ------------ Poligonos vectoriales del INE -----------------------------

# Disponible en: http://ine.gub.uy/documents/10181/18006/Mapas+Vectoriales+a%C3%B1o+2011/97dbcd58-80a8-472c-86cc-e8ecdaafef99


mapaUy <- read_sf(here::here("./DataBases/IneShapeFiles/ine_seg_11.shp"))
colnames(mapaUy)
mapaUy <- mapaUy[,c(1,8)]
# se eliminan segmentos censales duplicados
mapaUy <- mapaUy[!duplicated(mapaUy$CODSEG),]
# fija el tipo de información geográfica de acuerdo al INE
mapaUy = st_set_crs(mapaUy, "+proj=utm +zone=21 +south")
# Verificamos que la información geográfica esté
st_crs(mapaUy)
# transforma información geográfica de utm a lat long
mapaUy <- st_transform(mapaUy, "+proj=longlat +datum=WGS84")
# se crean los códigos censales (7 números) en el archivo de shape
mapaUy$CODSEG <- formatC(mapaUy$CODSEG, width=7, flag="0")


### ---------------------- Base de censo -----------------------------

# Base censo: http://www.ine.gub.uy/c/document_library/get_file?uuid=23d15ef3-e5ed-46e8-9bec-bff7ecc604d2&groupId=10181
# La base de transforma a un archivo rds para disminuir su tamaño

personas <- readr::read_rds(here::here("./DataBases/Censo2011/personas.rds"))
personas <- as.data.frame(personas)
head(personas)
colnames(personas)

# se fija el número de los códigos para poder unirlos y crear el código de segmento censal
personas$DPTO <- formatC(personas$DPTO, width=2, flag="0") # fix 2 numbers
personas$SECC <- formatC(personas$SECC, width=2, flag="0") # fix 2 numbers
personas$SEGM <- formatC(personas$SEGM, width=3, flag="0") # fix 3 numbers
# se unen los códigos anteriores para crear el código de segmento censal
personas$CODSEG <- paste0(personas$DPTO,personas$SECC,personas$SEGM)
# verifica que estén todos los códigos
length(table(personas$CODSEG))


#### Calculo la variable de interés: ratio +65/-15

# determino los códigos censales únicos
data <- as.data.frame(unique(personas$CODSEG))
# cambio el nombre a la variable a "CODSEG" utilizado por el INE
colnames(data)[1] <- "CODSEG"
# tabla de # personas por edad
table(personas$PERNA01)

edadmenos <- c(0:14)
edadmas <- c(66:112)

# personas mas de 65 por codigo censal
edad65 <- setDT(personas)[personas$PERNA01 %in% edadmas, .N, by = .(CODSEG)]
colnames(edad65)[1] <- "CODSEG"
colnames(edad65)[2] <- "Edad65"
# personas de menos de 15 por codigo censal
edad15 <- setDT(personas)[personas$PERNA01 %in% edadmenos, .N, by = .(CODSEG)]
colnames(edad15)[1] <- "CODSEG"
colnames(edad15)[2] <- "Edad15"

# merge de bases
edad <- merge(data,edad15, by="CODSEG", all.x = T)
edad <- merge(edad,edad65, by="CODSEG", all.x = T)
edad$ind_envej <- edad$Edad65 / edad$Edad15
quantile(edad$ind_envej, probs = seq(0,1, 0.1), na.rm = T)
rm(edad15, edad65)
edad$Edad15 <- edad$Edad65 <- NULL

### ---------------------- Se une la base de shapes y población  ----------------------

# Union de las bases
envejecimiento <- merge(mapaUy,edad,by="CODSEG")
names(envejecimiento)
# Verifico que no hayan NA en la base
sum(is.na(envejecimiento$ind_envej))
# gráfico de prueba para ver que toda la información esta ok
plot(envejecimiento$ind_envej)
# Calculo la población por kilómetro cuadrado
envejecimiento$ind_envej <-  round(envejecimiento$ind_envej, digits = 3)
# Guardo base de datos
st_write(envejecimiento, here::here("DataBases/Censo2011/ShapeCenso2011_envejecimiento.shp"))


#### ------------------------ Fin de rutina -------------------------------


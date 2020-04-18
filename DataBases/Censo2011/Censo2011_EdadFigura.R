library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)
library(here)

ShapeCenso2011 <- read_sf("./DataBases/Censo2011/ShapeCenso2011_envejecimiento.shp")



#### Informacion previa ------------------------------------------------------

x=0.05
max(ShapeCenso2011$ind_envej, na.rm = T)
h1 = hist(ShapeCenso2011$ind_envej, breaks =
             seq(0,14, by = x),na.rm=T)
h1$density = h1$counts/sum(h1$counts)*100

plot(h1,
     freq=FALSE,
     main = "",
     ylim = c(0,06),
     xlim = c(0,6),
     cex.main=1.4, cex.lab=1.5,cex.axis=1.2,
     xlab = "Índice de envejecimiento",
     ylab = "Porcentaje",
     col = "black",
     border = F,
     las = 1)

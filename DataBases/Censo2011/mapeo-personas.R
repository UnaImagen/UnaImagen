library(rgdal)
setwd("c://Users/leandro/Dropbox/Docs/1imagen-data/ine/")

# dirección datos vectoriales: http://ine.gub.uy/documents/10181/18006/Mapas+Vectoriales+a%C3%B1o+2011/97dbcd58-80a8-472c-86cc-e8ecdaafef99

### Levanto mapa del INE con los polígonos -----------------------------


mapaUy <- readOGR( 
  dsn= (".") , 
  layer="ine_seg_11",
  verbose=FALSE,
)

class(mapaUy)
names(mapaUy)

mapaUy$CODSEG <- formatC(mapaUy$CODSEG, width=7, flag="0") # fix 8 numbers



### Levanto base de censo -----------------------------

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


#### Calculo la variable de interés

# personas por códigos
data <- as.data.frame(unique(personas$CODSEG))
colnames(data)[1] <- "CODSEG"
data$prueba <- as.integer(factor(data$CODSEG))

data$pob <- by(personas$PERPH02, personas$CODSEG, nrow)
data$pob <- data$pob / max(data$pob)

rm(personas)


#### Creo la nueva base ------------------------

prueba <- merge(mapaUy,data,by="CODSEG")
names(prueba)
prueba <- prueba[,c(1,14)]
sum(is.na(prueba$pob))

plot(prueba$pob)



#### ----------------- Gráfico -----------------

cuts <- quantile(prueba$pob, probs = seq(0,1, by=0.05, na.rm=T))


library(dplyr)
library(leaflet)

plot(mapaUy)


m <- leaflet(prueba) %>% 
  setView(-56.1, -32, zoom = 6) %>% 
  addTiles()  %>% 
  addPolygons()

m









m <- leaflet(prueba) %>% addTiles() %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              data = prueba,
              fillColor = ~colorQuantile("YlOrRd", pob),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
setView(m, -56.1, -32, zoom = 6)


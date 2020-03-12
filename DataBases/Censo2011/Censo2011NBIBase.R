library(rgdal)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)


#### -----

ShapeCenso2011 <- read_sf(here::here("./DataBases/Censo2011/ShapeCenso2011.shp"))

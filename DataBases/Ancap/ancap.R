# ---------------------------- Gas Oil -------------------------------------------
library(dplyr)
gasoil <- readxl::read_excel(here::here("./DataBases/Ancap/ventas-dpto.-gasol.gasoil.-glp-diarias-al-14.06.2020.xlsx"), sheet = "Gasoil", skip = 4) %>%
  dplyr::slice(-(25:26))

# saca depto
depto <- c("Depto.: ")
gasoil[,1] <- as.data.frame(sapply(gasoil[,1], function(x)
  gsub(paste(depto, collapse = '|'), '', x)))

# saca Año/Mes
texto <- c("Año/Mes: ")
gasoil[1,] <- as.data.frame(t(sapply(gasoil[1,], function(x)
  gsub(paste(texto, collapse = '|'), '', x))))

# creo serie año/mes/dia
am <- as.data.frame(t(gasoil[1,]), row.names = F)
n <- nrow(am)
for (i in 2:n) {
  if  (is.na(am$V1[i])) {
    am$V1[i] = am$V1[i-1]
  } else {
    am$V1[i] = am$V1[i]
  }
}



#am$V1 <- as.character(as.factor(am$V1))
dia <- as.data.frame(t(gasoil[2,]), row.names = F)
amdia <- as.data.frame(paste(am$V1,dia$V1, sep = "/"))
colnames(amdia)[1] <- "fecha"
amdia$fecha <- as.Date(amdia$fecha, format("%Y/%m/%d"))
rm(am,dia) # borra auxiliares
gasoil <- gasoil[-2,]

gasoil <- as.data.frame(t(gasoil), row.names = F) # Transpose data frame
gasoil$V1 <- amdia$fecha # agrega fecha
rm(amdia)

# Agrego nombres
names(gasoil) <- lapply(gasoil[1,], as.character)
colnames(gasoil)[1] <- "fecha"
colnames(gasoil)[2] <- "dia_semana"
gasoil <- gasoil[-1,]

# Creo variables ----------------------------------------------------------


# transformo los numeros de factores a numerico
gasoil$semanas <- aweek::date2week(gasoil$fecha, numeric = T, factor = T)
gasoilsemana <- gasoil[,-c(1,2,25)]
gasoilsemana <- as.data.frame(apply(gasoilsemana, 2, function(x) as.numeric(as.character(x))))
gasoilsemana$fecha <- gasoil$fecha
gasoilsemana$semanas <- gasoil$semanas
gasoilsemana$dia_semana <- gasoil$dia_semana

# calculo la suma para cada departamento
gasoilsemana$anio <- as.factor(as.character(format(gasoilsemana$fecha,"%Y")))
gasoilsemana$semanas <- as.factor(as.numeric(gasoilsemana$semanas))

for(i in colnames(gasoilsemana[-c(22:25)])) {
   gasoilsemana[i] <- ave(gasoilsemana[i],
                          gasoilsemana[,c("anio","semanas")],
                          FUN = function(x) colSums(x, na.rm = T))
}

# me quedo con una observacion por semana
gasoil2 <- dplyr::distinct(gasoilsemana, semanas, anio, .keep_all = TRUE)

# salvo la base de datos
saveRDS(gasoil2, here::here("./DataBases/Ancap/gasoil_new.rds"))

# Creo variables índices
gasoil2 <- gasoil2[!gasoil2$semanas == 1,] #saco la primer semana

for(i in colnames(gasoil2[-c(22:25)])) {
  for(j in gasoil2$anio) {
    z <- (gasoil2[which(gasoil2$semanas ==2 & gasoil2$anio ==j),][i])
    gasoil2[gasoil2$anio ==j,][i] <- gasoil2[gasoil2$anio ==j,][i] / z[1,1] *100
  }
}

# salvo la base de datos
saveRDS(gasoil2, here::here("./DataBases/Ancap/gasoil_new.rds"))


# agrego fechas
gasoil <- readRDS(here::here("./DataBases/Ancap/gasoil_new.rds"))
gasoil$coronavirus <- ifelse(gasoil$anio == 2020 &
                               as.numeric(as.factor(gasoil$semanas)) > 11, 1, 0)
gasoil$turismo <- ifelse(gasoil$anio == 2020 &
                           as.numeric(as.factor(gasoil$semanas)) == 15 |
                           gasoil$anio == 2019 &
                           as.numeric(as.factor(gasoil$semanas)) == 16 |
                           gasoil$anio == 2018 &
                           as.numeric(as.factor(gasoil$semanas)) == 13, 1, 0)

# salvo la base de datos
saveRDS(gasoil, here::here("./DataBases/Ancap/gasoil_new.rds"))


# ---------------------------- NAFTA -----------------------------------------------------------------


nafta <- readxl::read_excel(here::here("./DataBases/Ancap/ventas-dpto.-gasol.gasoil.-glp-diarias-al-14.06.2020.xlsx"), sheet = "Gasolinas", skip = 4) %>%
  dplyr::slice(-(25:26))

# saca depto
depto <- c("Depto.: ")
nafta[,1] <- as.data.frame(sapply(nafta[,1], function(x)
  gsub(paste(depto, collapse = '|'), '', x)))

# saca Año/Mes
texto <- c("Año/Mes: ")
nafta[1,] <- as.data.frame(t(sapply(nafta[1,], function(x)
  gsub(paste(texto, collapse = '|'), '', x))))

# creo serie año/mes/dia
am <- as.data.frame(t(nafta[1,]), row.names = F)
n <- nrow(am)
for (i in 2:n) {
  if  (is.na(am$V1[i])) {
    am$V1[i] = am$V1[i-1]
  } else {
    am$V1[i] = am$V1[i]
  }
}
#am$V1 <- as.character(as.factor(am$V1))
dia <- as.data.frame(t(nafta[2,]), row.names = F)
amdia <- as.data.frame(paste(am$V1,dia$V1, sep = "/"))
colnames(amdia)[1] <- "fecha"
amdia$fecha <- as.Date(amdia$fecha, format("%Y/%m/%d"))
rm(am,dia) # borra auxiliares
nafta <- nafta[-2,]

nafta <- as.data.frame(t(nafta), row.names = F) # Transpose data frame
nafta$V1 <- amdia$fecha # agrega fecha
rm(amdia)

# Agrego nombres
names(nafta) <- lapply(nafta[1,], as.character)
colnames(nafta)[1] <- "fecha"
colnames(nafta)[2] <- "dia_semana"
nafta <- nafta[-1,]



# Creo variables ----------------------------------------------------------


# transformo los numeros de factores a numerico
nafta$semanas <- aweek::date2week(nafta$fecha, numeric = T, factor = T)
naftasemana <- nafta[,-c(1,2,25)]
naftasemana <- as.data.frame(apply(naftasemana, 2, function(x) as.numeric(as.character(x))))
naftasemana$fecha <- nafta$fecha
naftasemana$semanas <- nafta$semanas
naftasemana$dia_semana <- nafta$dia_semana

# calculo la suma para cada departamento
naftasemana$anio <- as.factor(as.character(format(naftasemana$fecha,"%Y")))
naftasemana$semanas <- as.factor(as.numeric(naftasemana$semanas))

for(i in colnames(naftasemana[-c(22:25)])) {
  naftasemana[i] <- ave(naftasemana[i],
                         naftasemana[,c("anio","semanas")],
                         FUN = function(x) colSums(x, na.rm = T))
}

# me quedo con una observacion por semana
nafta2 <- dplyr::distinct(naftasemana, semanas, anio, .keep_all = TRUE)

# salvo la base de datos
saveRDS(nafta2, here::here("./DataBases/Ancap/nafta_new.rds"))


# Creo variables índices
nafta2 <- nafta2[!nafta2$semanas == 1,] #saco la primer semana

for(i in colnames(nafta2[-c(22:25)])) {
  for(j in nafta2$anio) {
    z <- (nafta2[which(nafta2$semanas ==2 & nafta2$anio ==j),][i])
    nafta2[nafta2$anio ==j,][i] <- nafta2[nafta2$anio ==j,][i] / z[1,1] *100
  }
}

# salvo la base de datos
saveRDS(nafta2, here::here("./DataBases/Ancap/nafta_new.rds"))


# agrego fechas
nafta <- readRDS(here::here("./DataBases/Ancap/nafta_new.rds"))
nafta$coronavirus <- ifelse(nafta$anio == 2020 &
                               as.numeric(as.factor(nafta$semanas)) > 11, 1, 0)
nafta$turismo <- ifelse(nafta$anio == 2020 &
                           as.numeric(as.factor(nafta$semanas)) == 15 |
                           nafta$anio == 2019 &
                           as.numeric(as.factor(nafta$semanas)) == 16 |
                           nafta$anio == 2018 &
                           as.numeric(as.factor(nafta$semanas)) == 13, 1, 0)

# salvo la base de datos
saveRDS(nafta, here::here("./DataBases/Ancap/nafta_new.rds"))



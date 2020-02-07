
## Librerías usadas -----------

library(tidyverse)
library(pdftools)
library(here)

## Bases de datos de la Corte Electoral ---------------

escrutinio <- readxl::read_xlsx(
   path = here::here("/DataBases/Elecciones2019/Inf_D_Hoja.xlsx"),
   sheet = 1,
   skip = 8
   ) %>%
   select(
      -ACTO,
      -CONVOCATORIA,
      -DEPTO,
      -ESCRUTINIO,
      -HOJA
   ) %>%
   rename_all(
      .funs = tolower
   ) %>%
   mutate(
      es_frente = if_else(lema == "Partido Frente Amplio", "Frente Amplio", "Otro")
   ) %>%
   group_by(
      circuito,
      series,
      es_frente
   ) %>%
   summarise(
      cantidad_votos = sum(cnt_votos)
   ) %>%
   pivot_wider(
      names_from = es_frente,
      values_from = cantidad_votos
   )


escrutinio

escrutinio2 <- readxl::read_xlsx(
   path = here::here("/DataBases/Elecciones2019/Inf_D_Lema.xlsx"),
   sheet = 1,
   skip = 8
) %>%
   select(
      -ACTO,
      -CONVOCATORIA,
      -DEPTO,
      -ESCRUTINIO,
      -HABILITADO,
      -NO_OBERVAD,
      -OBSERVADOS,
      -T_EMITIDOS,
      -EN_BLANCO,
      -ANULADOS)

x <- colnames(escrutinio)

colnames(escrutinio2) <- x

colnames(escrutinio)[3] <- "FA1a"
colnames(escrutinio)[4] <- "Coalicion1a"
colnames(escrutinio2)[3] <- "FA2a"
colnames(escrutinio2)[4] <- "Coalicion2a"

escrutinio.final <- merge(escrutinio,escrutinio2, by = c("circuito","series")) # Base final: votos por circuito



## Información de los circuitos ----------------

setwd()

# No funciona
pdf1 <- pdf_text(here("DataBases","Elecciones2019","salto.pdf"))

write(pdf1, file = temp.txt, sep = "\t")
# teams are located on the 6th line
teams <- read.table(file_name, skip = 5, nrows = 1)

pdf1 <- strsplit(pdf1, '\r\n') %>% unlist
pdf1 <- read.table(pdf1, skip = 1)

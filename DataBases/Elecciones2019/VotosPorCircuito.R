#=====================#
#### MAPA VOTACIÓN ####
#=====================#

## Librerías usadas -----------

library(tidyverse)
library(magrittr)

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
      es_frente = if_else(lema == "Partido Frente Amplio", "frente_amplio", "coalicion")
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
   ) %>%
   ungroup() %>%
   transmute(
      votacion = "octubre",
      circuito,
      serie = series,
      frente_amplio,
      coalicion
   )


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
      -ANULADOS
   ) %>%
   rename_all(
      .funs = tolower
   ) %>%
   rename(
      frente_amplio = `martínez - villar`,
      coalicion = `lacalle pou - argimón`
   ) %>%
   mutate(
      votacion = "noviembre"
   )

escrutinio %<>%
   bind_rows(
      escrutinio2
   )

## Información de los circuitos ----------------



#===============#
#### THE END ####
#===============#
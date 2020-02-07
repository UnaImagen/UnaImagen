
library(tidyverse)

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


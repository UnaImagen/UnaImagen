#=====================#
#### MAPA VOTACIÓN ####
#=====================#

## Librerías usadas -----------

library(tidyverse, quietly = TRUE)
library(magrittr, quietly = TRUE)

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

file_list <- str_subset(
   string = list.files(
      path = here::here("/DataBases/Elecciones2019")
   ),
   pattern = ".pdf$"
)

header <- "Circuito Serie Desde Hasta Habilitados Localidad Direccion Accesibilidad —_urb/rural"

info_circuitos <- pdftools::pdf_ocr_text(
   pdf = paste0(here::here("/DataBases/Elecciones2019"), "/", file_list[1])
) %>%
   str_split(
      pattern = "\n"
   ) %>%
   unlist() %>%
   tibble::enframe(
      name = NULL
   ) %>%
   mutate(
      value = str_trim(
         string = value,
         side = "both"
      )
   ) %>%
   filter(
      !str_detect(
         string = value,
         pattern = "ELECCIONES NACIONALES 2019"
      ),
      !str_detect(
         string = value,
         pattern = "ae CORTE ELECTORAL"
      ),
      !str_detect(
         string = value,
         pattern = "Republica Oriental del Uruguay"
      ),
      !str_detect(
         string = value,
         pattern = "ELECCIONES NACIONALES"
      ),
      !str_detect(
         string = value,
         pattern = header
      )
   ) %>%
   mutate(
      departamento = str_to_lower(str_remove(file_list[1], ".pdf")),
      circuito = str_extract(
         string = value,
         pattern = "^([:digit:]+)\\b"
      ),
      value = str_remove(
         string = value,
         pattern = "^([:digit:]+)\\b\\s"
      ),
      serie = str_extract(
         string = value,
         pattern = "^([A-Z]{3})\\b"
      ),
      value = str_remove(
         string = value,
         pattern = "^([A-Z]{3})\\b\\s"
      ),
      desde = str_extract(
         string = value,
         pattern = "^([0-9]+)\\b"
      ),
      value = str_remove(
         string = value,
         pattern = "^([0-9]+)\\b\\s"
      ),
      hasta = str_extract(
         string = value,
         pattern = "^([0-9]+)\\b"
      ),
      value = str_remove(
         string = value,
         pattern = "^([0-9]+)\\b\\s"
      ),
      habilitados = str_extract(
         string = value,
         pattern = "^([0-9]+)\\b"
      ),
      value = str_remove(
         string = value,
         pattern = "^([0-9]+)\\b\\s"
      ),
      urbano = str_extract(
         string = value,
         pattern = "(rural|urb)$"
      ),
      value = str_remove(
         string = value,
         pattern = "\\s(rural|urb)$"
      ),
      accesibilidad = str_extract(
         string = value,
         pattern = "\\b(S|N)$"
      ),
      value = str_remove(
         string = value,
         pattern = "\\s(S|N)$"
      ),
      localidad = str_extract(
         string = value,
         pattern = "^([A-Za-z]+)\\b"
      ),
      value = str_remove(
         string = value,
         pattern = "^([A-Za-z]+)\\b\\s"
      ),
      value = str_trim(
         string = value,
         side = "both"
      )
   ) %>%
   transmute(
      departamento,
      circuito = as.numeric(circuito),
      serie,
      desde = as.numeric(desde),
      hasta = as.numeric(hasta),
      habilitados = as.numeric(habilitados),
      localidad,
      direccion = value,
      accesibilidad,
      urbano
   )

#===============#
#### THE END ####
#===============#
#=============#
#### EUTIC ####
#=============#

library(magrittr)
library(tidyverse)

# Carga datos -------------------------------------------------------------
eutic <- haven::read_sav(
   file = here::here("DataBases/EUTIC/BASE HOGARES Y PERSONAS EUTIC 2016.sav")
) %>%
   dplyr::rename_all(
      .funs = tolower
   )


# Construye objeto para el App --------------------------------------------
eutic %>%
   # filter(
   #    p23_1 == 0
   # ) %>%
   dplyr::transmute(
      nper = dplyr::row_number(),
      peso = base::as.integer(peso.per),
      sexo = forcats::as_factor(p12),
      edad = base::as.integer(p13),

      ## Uso de celular
      uso_celular = forcats::as_factor(p23),
      uso_smart_phone = forcats::as_factor(p23_1),
      uso_smart_phone = forcats::fct_collapse(
         .f = uso_smart_phone,
         "Sí" = "Sí",
         "No" = "No",
         other_level = "Ns/Nc"
      ),

      ## Uso de tablet
      uso_tablet = forcats::as_factor(p26),


   )


x %>%
   group_by(
      uso_smart_phone
   ) %>%
   tally() %>%
   plotly::plot_ly(
   x = ~uso_smart_phone,
   y = ~n,
   type = "bar"
)

table(eutic$p26, useNA = "always")
levels(x$uso_smart_phone)

#===============#
#### THE END ####
#===============#
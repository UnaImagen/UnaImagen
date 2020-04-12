#=============#
#### ENGIH ####
#=============#

library(tidyverse)

# Codigueras --------------------------------------------------------------
deptos <- tibble::tribble(
   ~depto_id, ~depto_name,
   01, "Montevideo",
   02, "Artigas",
   03, "Canelones",
   04, "Cerro Largo",
   05, "Colonia",
   06, "Durazno",
   07, "Flores",
   08, "Florida",
   09, "Lavalleja",
   10, "Maldonado",
   11, "Paysandú",
   12, "Río Negro",
   13, "Rivera",
   14, "Rocha",
   15, "Salto",
   16, "San José",
   17, "Soriano",
   18, "Tacuarembó",
   19, "Treinta y Tres"
)

fuentes_de_energia_para_cocinar <- tibble::tribble(
   ~fuente_id, ~fuente_name,
   1, "Energía eléctrica",
   2, "Gas por cañería",
   3, "Supergás",
   4, "Queroseno",
   5, "Leña",
   6, "Ninguna"
)


# ENGIH -------------------------------------------------------------------
hogares <- haven::read_sav(
   file = here::here("DataBases/ENGIH/ENGIH 2016 Base de Datos Hogares.sav")
) %>%
   rename_all(
      .funs = tolower
   ) %>%
   rename(
      year = domanio,
      month = dommes,
      depto = domdepartamento
   ) %>%
   mutate(
      year = as.integer(year),
      month = as.integer(month),
      depto = factor(
         x = as.integer(depto),
         levels = deptos$depto_id,
         labels = deptos$depto_name
      ),
      fuentes_para_cocinar = factor(
         x = d20,
         levels = fuentes_de_energia_para_cocinar$fuente_id,
         labels = fuentes_de_energia_para_cocinar$fuente_name
      )
   )


# Fuentes para cocinar ----------------------------------------------------
hogares %>%
   transmute(
      depto,
      fuentes_para_cocinar = fct_relevel(
         .f = fuentes_para_cocinar,
         "Energía eléctrica",
         after = Inf
      ),
      fuentes_para_cocinar = fct_relevel(
         .f = fuentes_para_cocinar,
         "Supergás",
         after = Inf
      ),
      es_supergas = if_else(fuentes_para_cocinar == "Supergás", 1, 0)
   ) %>%
   ggplot() +
   geom_bar(
      aes(
         x = fct_reorder(
            .f = depto,
            .x = es_supergas,
            .fun = mean
         ),
         y = ..count.. / sum(..count..),
         fill = fuentes_para_cocinar
      ),
      position = "fill"
   ) +
   labs(
      x = NULL,
      y = "Porcentaje",
      fill = "Fuente"
   ) +
   scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
   ) +
   scale_fill_brewer(
      palette = "BrBG",
      direction = -1
   ) +
   coord_flip() +
   theme_minimal()


#===============#
#### THE END ####
#===============#
#=============#
#### ENCoR ####
#=============#

library(tidyverse)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(file = "DataBases/ENCoR/Base ENCoR terceros.sav")

# Ideas, Normas y Actitudes -----------------------------------------------
encor %>%
   select(
      sexo,
      ina44_1,
      ina48_1,
      peso
   ) %>%
   gather(
      key = key,
      value = value,
      -sexo,
      -peso
   ) %>%
   mutate(
      key = case_when(
         key == "ina44_1" ~ "¿a qué edad le parece que\nuna mujer es demasiado joven para\ntener relaciones sexuales?",
         key == "ina48_1" ~ "¿a qué edad le parece que\nun hombre es demasiado joven para\ntener relaciones sexuales?"
      ),
      sexo = as_factor(sexo),
      sexo = forcats::fct_recode(
         .f = sexo,
         "Mujer" = "mujer",
         "Hombre" = "hombre"
      )
   ) %>%
   ggplot() +
   geom_bar(
      aes(x = value, y = ..prop.., fill = as_factor(sexo), weight = peso),
      alpha = 1/2,
      position = "dodge"
   ) +
   facet_wrap(
      facets = ~key
      ) +
   labs(
      x = "Edad",
      y = "Proporción",
      fill = "Sexo del encuestado"
   ) +
   ggthemes::theme_economist() +
   theme(
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
   )

#===============#
#### THE END ####
#===============#
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
         key == "ina44_1" ~ "Mujer", #"¿a qué edad le parece que\nuna mujer es demasiado joven para\ntener relaciones sexuales?",
         key == "ina48_1" ~ "Hombre", #"¿a qué edad le parece que\nun hombre es demasiado joven para\ntener relaciones sexuales?"
      ),
      sexo = as_factor(sexo),
      sexo = forcats::fct_recode(
         .f = sexo,
         "La encuestada es mujer" = "mujer",
         "El encuestado es hombre" = "hombre"
      )
   ) %>%
   ggplot() +
   geom_bar(
      aes(x = value, y = ..prop.., fill = key, weight = peso),
      alpha = 1/2,
      position = "dodge"
   ) +
   facet_wrap(
      facets = ~sexo
      ) +
   labs(
      x = "Edad",
      y = "Porcentaje\n",
      fill = "Pregunta sobre ..."
   ) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
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
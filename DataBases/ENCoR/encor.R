#=============#
#### ENCoR ####
#=============#

library(survey)
library(tidyverse)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(file = "DataBases/ENCoR/Base ENCoR terceros.sav")


# Genera objeto survey ----------------------------------------------------
ps <- survey::svydesign(
   ids = ~1,
   weights = ~peso,
   data = encor
)

summary(ps)

survey::svymean(
   x = encor$ina48_1,
   design = ps,
   na.rm = TRUE
)

survey::svymean(
   x = encor$ina44_1,
   design = ps,
   na.rm = TRUE
)


# Ideas, Normas y Actitudes -----------------------------------------------
encor %>%
   select(
      sexo,
      ina44_1,
      ina48_1,
      peso
   ) %>%
   gather(
      key = pregunta,
      value = edad,
      -sexo,
      -peso
   ) %>%
   mutate(
      pregunta = case_when(
         pregunta == "ina44_1" ~ "Mujer", #"¿a qué edad le parece que\nuna mujer es demasiado joven para\ntener relaciones sexuales?",
         pregunta == "ina48_1" ~ "Hombre", #"¿a qué edad le parece que\nun hombre es demasiado joven para\ntener relaciones sexuales?"
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
      aes(x = edad, y = ..prop.., fill = pregunta, weight = peso),
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
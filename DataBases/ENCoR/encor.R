#=============#
#### ENCoR ####
#=============#

# library(survey)
library(tidyverse)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(file = "DataBases/ENCoR/Base ENCoR terceros.sav")


# Genera objeto survey ----------------------------------------------------
# ps <- survey::svydesign(
#    ids = ~1,
#    weights = ~peso,
#    data = encor
# )

# summary(ps)

# survey::svymean(
#    x = encor$ina48_1,
#    design = ps,
#    na.rm = TRUE
# )

# survey::svymean(
#    x = encor$ina44_1,
#    design = ps,
#    na.rm = TRUE
# )


# Plots -------------------------------------------------------------------
ggplot_bars_sex_questions <- function(hombre, mujer) {

   legend_title <- dplyr::case_when(
      hombre == "ina48_1" ~ "Edad límite inferior para...",
      hombre == "ina49_1" ~ "Edad límite inferior para...",
      hombre == "ina50_1" ~ "Edad límite superior para...",
      hombre == "ina51_1" ~ "Edad límite inferior para..."
   )

   encor %>%
      select(
         sexo,
         !!rlang::sym(mujer),
         !!rlang::sym(hombre),
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
            pregunta == mujer ~ "Mujer",
            pregunta == hombre ~ "Hombre",
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
         fill = legend_title
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggthemes::theme_economist() +
      theme(
         legend.position = "bottom",
         axis.title = element_text(face = "bold"),
         strip.text = element_text(face = "bold"),
         legend.title = element_text(face = "bold")
      )

}


# Edad límite inferior para iniciación sexual --------------------------------------
ggplot_bars_sex_questions(
   hombre = "ina48_1",
   mujer = "ina44_1"
)


# Edad límite inferior para tener hijos -----------------------------------
ggplot_bars_sex_questions(
   hombre = "ina49_1",
   mujer = "ina45_1"
)

# Edad límite superior para tener hijos -----------------------------------
ggplot_bars_sex_questions(
   hombre = "ina50_1",
   mujer = "ina46_1"
)

# Edad límite inferior para abandonar estudios -----------------------------------
ggplot_bars_sex_questions(
   hombre = "ina51_1",
   mujer = "ina47_1"
)


#===============#
#### THE END ####
#===============#
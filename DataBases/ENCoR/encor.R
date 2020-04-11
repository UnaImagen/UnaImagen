#=============#
#### ENCoR ####
#=============#

# library(survey)
library(tidyverse)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(file = "DataBases/ENCoR/Base ENCoR terceros.sav")

# Plots -------------------------------------------------------------------
ggplot_bars_sex_questions <- function(hombre, mujer) {

   legend_title <- dplyr::case_when(
      hombre == "ina48_1" ~ "Edad límite inferior para tener relaciones sexuales",
      hombre == "ina49_1" ~ "Edad límite inferior para tener hijos",
      hombre == "ina50_1" ~ "Edad límite superior para tener hijos",
      hombre == "ina51_1" ~ "Edad límite inferior para abandonar estudios"
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
         aes(x = edad, y = ..prop.., fill = pregunta),
         position = "dodge"
      ) +
      facet_wrap(
         facets = ~sexo
      ) +
      labs(
         x = "\nRespuesta (en años)",
         y = "Porcentaje\n",
         fill = legend_title
      ) +
      scale_y_continuous(
         labels = scales::percent_format(accuracy = 1)
         ) +
      ggthemes::theme_economist() +
      theme(
         legend.position = "bottom",
         axis.title = element_text(face = "bold"),
         strip.text = element_text(face = "bold"),
         legend.title = element_text(face = "bold")
      )

}

sex_plots <- list()

# Edad límite inferior para iniciación sexual --------------------------------------
sex_plots[[1]] <- ggplot_bars_sex_questions(
   hombre = "ina48_1",
   mujer = "ina44_1"
)


# Edad límite inferior para tener hijos -----------------------------------
sex_plots[[2]] <- ggplot_bars_sex_questions(
   hombre = "ina49_1",
   mujer = "ina45_1"
)


# Edad límite superior para tener hijos -----------------------------------
sex_plots[[3]] <- ggplot_bars_sex_questions(
   hombre = "ina50_1",
   mujer = "ina46_1"
)


# Edad límite inferior para abandonar estudios -----------------------------------
sex_plots[[4]] <- ggplot_bars_sex_questions(
   hombre = "ina51_1",
   mujer = "ina47_1"
)

readr::write_rds(x = sex_plots, path = "DataBases/ENCoR/sex_plots.rds")

# Edad materinidad --------------------------------------------------------
encor %>%
   select(
      sexo,
      ina52_1,
      peso
   ) %>%
   group_by(
      sexo,
      ina52_1
   ) %>%
   summarise(
      cantidad = sum(peso)
   ) %>%
   ungroup() %>%
   mutate(
      proporcion = cantidad / sum(cantidad),
      ina52_1 = as_factor(ina52_1),
      ina52_1 = fct_relevel(
         .f = ina52_1,
         "en desacuerdo",
         "de acuerdo"
      ),
      sexo = as_factor(sexo),
      sexo = fct_recode(
         .f = sexo,
         "Mujer" = "mujer",
         "Hombre" = "hombre"
      )
   ) %>%
   ggplot() +
   geom_col(
      aes(x = ina52_1, y = proporcion, fill = sexo),
      position = "dodge"
   ) +
   labs(
      title = "¿Una mujer puede ser madre antes de los 18?",
      x = NULL,
      y = "Porcentaje\n",
      fill = "Sexo del encuestado"
   ) +
   scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
      ) +
   ggthemes::theme_economist() +
   theme(
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
   )

#===============#
#### THE END ####
#===============#
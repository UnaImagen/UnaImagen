#=============#
#### ENCoR ####
#=============#

# library(survey)
library(magrittr)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(
   file = here::here("Base ENCoR terceros.sav")
)

# Genera rds para App -----------------------------------------------------
encor %<>%
   dplyr::mutate(
      # Sexo
      sexo = forcats::as_factor(sexo),

      # Tuvo hijos
      tuvo_hijos = stringr::str_to_sentence(forcats::as_factor(hr10)),
      tuvo_hijos = forcats::as_factor(tuvo_hijos),

      # Cantidad ideal de hijos
      ina40 = dplyr::if_else(ina40 == 1 & base::is.na(ina40_1), 2, base::as.numeric(ina40)),
      ina42 = dplyr::if_else(ina42 == 1 & base::is.na(ina42_1), 2, base::as.numeric(ina42)),
      cantidad_ideal_hijos = dplyr::case_when(
         hr10 == 1 & ina40 == 1 ~ ina40_1,
         hr10 == 1 & ina40 == 2 ~ NA_real_,
         hr10 == 2 & ina42 == 1 ~ ina42_1,
         hr10 == 2 & ina42 == 2 ~ NA_real_,
         TRUE ~ NA_real_
      ),
      cantidad_ideal_hijos = base::factor(
         x = cantidad_ideal_hijos,
         levels = base::sort(base::unique(cantidad_ideal_hijos))
      ),
      cantidad_ideal_hijos = forcats::fct_explicit_na(
         f = cantidad_ideal_hijos,
         na_level = "Ns/Nc"
      ),

      # Cantidad de hijos
      cantidad_hijos = base::as.integer(hr10_1),

      # Edad ideal para tener el primer hijo
      ina41 = dplyr::if_else(ina41 == 1 & base::is.na(ina41_1), 2, base::as.numeric(ina41)),
      ina43 = dplyr::if_else(ina43 == 1 & base::is.na(ina43_1), 2, base::as.numeric(ina43)),
      edad_ideal_primer_hijo = dplyr::case_when(
         hr10 == 1 & ina41 == 1 ~ ina41_1,
         hr10 == 1 & ina41 == 2 ~ NA_real_,
         hr10 == 2 & ina43 == 1 ~ ina43_1,
         hr10 == 2 & ina43 == 2 ~ NA_real_,
         TRUE ~ NA_real_
      ),
      edad_ideal_primer_hijo = base::factor(
         x = edad_ideal_primer_hijo,
         levels = base::sort(base::unique(edad_ideal_primer_hijo))
      ),
      edad_ideal_primer_hijo = forcats::fct_explicit_na(
         f = edad_ideal_primer_hijo,
         na_level = "Ns/Nc"
      ),

      ## Edad límit inferior para tener sexo (mujeres)
      edad_limit_inf_sexo_mujeres = dplyr::if_else(ina44 == 2, base::max(encor$ina44_1, na.rm = TRUE) + 1, ina44_1),
      edad_limit_inf_sexo_mujeres = base::factor(
         x = edad_limit_inf_sexo_mujeres,
         levels = base::sort(base::unique(edad_limit_inf_sexo_mujeres))
      ),
      edad_limit_inf_sexo_mujeres = forcats::fct_explicit_na(
         f = edad_limit_inf_sexo_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_sexo_mujeres = forcats::fct_recode(
         .f = edad_limit_inf_sexo_mujeres,
         Depende = base::as.character(base::max(encor$ina44_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para tener sexo (varones)
      edad_limit_inf_sexo_varones = dplyr::if_else(ina48 == 2, base::max(encor$ina48_1, na.rm = TRUE) + 1, ina48_1),
      edad_limit_inf_sexo_varones = base::factor(
         x = edad_limit_inf_sexo_varones,
         levels = base::sort(base::unique(edad_limit_inf_sexo_varones))
      ),
      edad_limit_inf_sexo_varones = forcats::fct_explicit_na(
         f = edad_limit_inf_sexo_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_sexo_varones = forcats::fct_recode(
         .f = edad_limit_inf_sexo_varones,
         Depende = base::as.character(base::max(encor$ina48_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para tener hijos (mujeres)
      edad_limit_inf_hijos_mujeres = dplyr::if_else(ina45 == 2, base::max(encor$ina45_1, na.rm = TRUE) + 1, ina45_1),
      edad_limit_inf_hijos_mujeres = base::factor(
         x = edad_limit_inf_hijos_mujeres,
         levels = base::sort(base::unique(edad_limit_inf_hijos_mujeres))
      ),
      edad_limit_inf_hijos_mujeres = forcats::fct_explicit_na(
         f = edad_limit_inf_hijos_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_hijos_mujeres = forcats::fct_recode(
         .f = edad_limit_inf_hijos_mujeres,
         Depende = base::as.character(base::max(encor$ina45_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit superior para tener hijos (mujeres)
      edad_limit_sup_hijos_mujeres = dplyr::if_else(ina46 == 2, base::max(encor$ina46_1, na.rm = TRUE) + 1, ina46_1),
      edad_limit_sup_hijos_mujeres = base::factor(
         x = edad_limit_sup_hijos_mujeres,
         levels = base::sort(base::unique(edad_limit_sup_hijos_mujeres))
      ),
      edad_limit_sup_hijos_mujeres = forcats::fct_explicit_na(
         f = edad_limit_sup_hijos_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_sup_hijos_mujeres = forcats::fct_recode(
         .f = edad_limit_sup_hijos_mujeres,
         Depende = base::as.character(base::max(encor$ina46_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para tener hijos (varones)
      edad_limit_inf_hijos_varones = dplyr::if_else(ina49 == 2, base::max(encor$ina49_1, na.rm = TRUE) + 1, ina49_1),
      edad_limit_inf_hijos_varones = base::factor(
         x = edad_limit_inf_hijos_varones,
         levels = base::sort(base::unique(edad_limit_inf_hijos_varones))
      ),
      edad_limit_inf_hijos_varones = forcats::fct_explicit_na(
         f = edad_limit_inf_hijos_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_hijos_varones = forcats::fct_recode(
         .f = edad_limit_inf_hijos_varones,
         Depende = base::as.character(base::max(encor$ina49_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit superior para tener hijos (varones)
      edad_limit_sup_hijos_varones = dplyr::if_else(ina50 == 2, base::max(encor$ina50_1, na.rm = TRUE) + 1, ina50_1),
      edad_limit_sup_hijos_varones = base::factor(
         x = edad_limit_sup_hijos_varones,
         levels = base::sort(base::unique(edad_limit_sup_hijos_varones))
      ),
      edad_limit_sup_hijos_varones = forcats::fct_explicit_na(
         f = edad_limit_sup_hijos_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_sup_hijos_varones = forcats::fct_recode(
         .f = edad_limit_sup_hijos_varones,
         Depende = base::as.character(base::max(encor$ina50_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para abandonar estudios (mujeres)
      edad_limit_inf_abandonar_estudios_mujeres = dplyr::if_else(ina47 == 2, base::max(encor$ina47_1, na.rm = TRUE) + 1, ina47_1),
      edad_limit_inf_abandonar_estudios_mujeres = base::factor(
         x = edad_limit_inf_abandonar_estudios_mujeres,
         levels = base::sort(base::unique(edad_limit_inf_abandonar_estudios_mujeres))
      ),
      edad_limit_inf_abandonar_estudios_mujeres = forcats::fct_explicit_na(
         f = edad_limit_inf_abandonar_estudios_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_abandonar_estudios_mujeres = forcats::fct_recode(
         .f = edad_limit_inf_abandonar_estudios_mujeres,
         Depende = base::as.character(base::max(encor$ina47_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para abandonar estudios (varones)
      edad_limit_inf_abandonar_estudios_varones = dplyr::if_else(ina50 == 2, base::max(encor$ina50_1, na.rm = TRUE) + 1, ina50_1),
      edad_limit_inf_abandonar_estudios_varones = base::factor(
         x = edad_limit_inf_abandonar_estudios_varones,
         levels = base::sort(base::unique(edad_limit_inf_abandonar_estudios_varones))
      ),
      edad_limit_inf_abandonar_estudios_varones = forcats::fct_explicit_na(
         f = edad_limit_inf_abandonar_estudios_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_abandonar_estudios_varones = forcats::fct_recode(
         .f = edad_limit_inf_abandonar_estudios_varones,
         Depende = base::as.character(base::max(encor$ina50_1, na.rm = TRUE) + 1)
      ),

      ## Madre antes de los 18
      madre_antes_18 = forcats::as_factor(ina52_1),

      ## Una mujer puede decidir no tener hijos
      mujer_no_tener_hijos = forcats::as_factor(ina52_2),

      ## Madre vivir en pareja sin estar casada
      mujer_vivir_en_pareja_sin_casarse = forcats::as_factor(ina52_3),

      ## Una mujer puede tener hijos/as con la pareja que vive sin estar casada
      mujer_tener_hijos_con_concu = forcats::as_factor(ina52_4),

      ## Una mujer puede tener un trabajo completo teniendo hijos/as menores de 3 años
      mujer_trabajar_full_con_hijos_menores_3 = forcats::as_factor(ina52_5),

      ## Una mujer puede separarse o divorciarse teniendo hijos menores de 12 años
      mujer_divorciarse_con_hijos_menores_12 = forcats::as_factor(ina52_6),

      ## El cuidado de los hijos debe ser tarea principalmente de la mujer
      cuidado_hijos_mujer_ppal = forcats::as_factor(ina52_7),

      ## Una mujer se realiza plenamente cuando es madre
      mujer_se_realiza_cuando_es_madre = forcats::as_factor(ina52_8),

      ## Cuando la mujer tiene un trabajo de jornada completa la vida familiar se perjudica
      mujer_trabaja_full_perjudica_flia = forcats::as_factor(ina52_9),

      ## Un hombre se realiza plenamente cuando es padre
      varon_se_realiza_cuando_es_padre = forcats::as_factor(ina52_10),


   ) %>%
   dplyr::select(
      sexo,
      edad_act,
      tuvo_hijos,

      cantidad_ideal_hijos,
      cantidad_hijos,
      edad_ideal_primer_hijo,
      edad_limit_inf_sexo_mujeres,
      edad_limit_inf_sexo_varones,
      edad_limit_inf_hijos_mujeres,
      edad_limit_inf_hijos_varones,
      edad_limit_sup_hijos_mujeres,
      edad_limit_sup_hijos_varones,
      edad_limit_inf_abandonar_estudios_mujeres,
      edad_limit_inf_abandonar_estudios_varones,

      madre_antes_18,
      mujer_no_tener_hijos,
      mujer_vivir_en_pareja_sin_casarse,
      mujer_tener_hijos_con_concu,
      mujer_trabajar_full_con_hijos_menores_3,
      mujer_divorciarse_con_hijos_menores_12,
      cuidado_hijos_mujer_ppal,
      mujer_se_realiza_cuando_es_madre,
      mujer_trabaja_full_perjudica_flia,
      varon_se_realiza_cuando_es_padre

   ) %>%
   dplyr::distinct()

readr::write_rds(x = encor, path = here::here("encore.rds"))


# Métodos anticonceptivos -------------------------------------------------
metodos_anticonceptivos <- haven::read_sav(
   file = here::here("Base ENCoR terceros.sav")
) %>%
   dplyr::mutate(
      # Sexodu
      sexo = forcats::as_factor(sexo),

      # Tuvo hijos
      tuvo_hijos = stringr::str_to_sentence(forcats::as_factor(hr10)),
      tuvo_hijos = forcats::as_factor(tuvo_hijos),

      # Edad actual
      edad_actual = base::as.integer(edad_act),
      rango_edad = dplyr::case_when(
         dplyr::between(edad_actual, 15, 20) ~ "[15-20]",
         dplyr::between(edad_actual, 21, 30) ~ "[21-30]",
         dplyr::between(edad_actual, 31, 40) ~ "[31-40]",
         dplyr::between(edad_actual, 41, 50) ~ "[41-50]"
      ),
      rango_edad = forcats::as_factor(rango_edad),
      rango_edad = forcats::fct_relevel(
         .f = rango_edad,
         "[15-20]",
         "[21-30]",
         "[31-40]",
         "[41-50]"
      ),

      # Virgen?
      tuvo_primera_relacion = stringr::str_to_sentence(forcats::as_factor(ma0)),
      tuvo_primera_relacion = forcats::as_factor(tuvo_primera_relacion),

      # Edad primera relación
      edad_primera_relacion = base::as.integer(ma24_1)

   ) %>%
   dplyr::select(
      numero,
      nper,
      sexo,
      tuvo_hijos,
      rango_edad,
      tuvo_primera_relacion,
      edad_primera_relacion,
      tidyselect::starts_with("ma25_"),
      tidyselect::starts_with("ma26_"),
      tidyselect::starts_with("ma27_"),
   ) %>%
   dplyr::distinct()

primera_relacion <- metodos_anticonceptivos %>%
   dplyr::select(
      numero,
      nper,
      sexo,
      tuvo_hijos,
      rango_edad,
      tuvo_primera_relacion,
      edad_primera_relacion,
      tidyselect::starts_with("ma25_")
   ) %>%
   dplyr::filter(
      tuvo_primera_relacion == "Sí"
   ) %>%
   dplyr::mutate_all(
      .funs = ~forcats::as_factor(stringr::str_to_sentence(forcats::as_factor(.)))
   ) %>%
   dplyr::mutate(
      edad_primera_relacion = base::as.integer(base::as.character(edad_primera_relacion))
   ) %>%
   tidyr::pivot_longer(
      cols = tidyselect::starts_with("ma25_"),
      names_to = "metodo_primera_relacion",
      values_to = "value_primera_relacion"
   ) %>%
   dplyr::filter(
      value_primera_relacion == "Sí"
   ) %>%
   dplyr::mutate(
      metodo_primera_relacion = dplyr::case_when(
         metodo_primera_relacion == "ma25_1" ~ "Pastillas anticonceptivas",
         metodo_primera_relacion == "ma25_2" ~ "Condón femenino",
         metodo_primera_relacion == "ma25_3" ~ "DIU",
         metodo_primera_relacion == "ma25_4" ~ "Método del calendario",
         metodo_primera_relacion == "ma25_5" ~ "Retiro",
         metodo_primera_relacion == "ma25_6" ~ "Inyección anticonceptiva",
         metodo_primera_relacion == "ma25_7" ~ "Implantes",
         metodo_primera_relacion == "ma25_8" ~ "Ligadura de trompas",
         metodo_primera_relacion == "ma25_9" ~ "Vasectomía",
         metodo_primera_relacion == "ma25_10" ~ "Pastilla del día después",
         metodo_primera_relacion == "ma25_11" ~ "Método de lactancia materna",
         metodo_primera_relacion == "ma25_12" ~ "No utilizó",
         metodo_primera_relacion == "ma25_13" ~ "No tuvo relaciones sexuales",
         metodo_primera_relacion == "ma25_14" ~ "Condón masculino",
         metodo_primera_relacion == "ma25_15" ~ "Ns/Nc",
         metodo_primera_relacion == "ma25_16" ~ "Otro"
      )
   ) %>%
   dplyr::select(
      -value_primera_relacion,
      -tuvo_primera_relacion
   )

ultimos_seis_meses <- metodos_anticonceptivos %>%
   dplyr::select(
      numero,
      nper,
      tuvo_primera_relacion,
      tidyselect::starts_with("ma26_")
   ) %>%
   dplyr::filter(
      tuvo_primera_relacion == "Sí"
   ) %>%
   dplyr::mutate_all(
      .funs = ~forcats::as_factor(stringr::str_to_sentence(forcats::as_factor(.)))
   ) %>%
   tidyr::pivot_longer(
      cols = tidyselect::starts_with("ma26_"),
      names_to = "metodo_ultimos_seis_meses",
      values_to = "value_ultimos_seis_meses"
   ) %>%
   dplyr::filter(
      value_ultimos_seis_meses == "Sí"
   ) %>%
   dplyr::mutate(
      metodo_ultimos_seis_meses = dplyr::case_when(
         metodo_ultimos_seis_meses == "ma26_1" ~ "Pastillas anticonceptivas",
         metodo_ultimos_seis_meses == "ma26_2" ~ "Condón femenino",
         metodo_ultimos_seis_meses == "ma26_3" ~ "DIU",
         metodo_ultimos_seis_meses == "ma26_4" ~ "Método del calendario",
         metodo_ultimos_seis_meses == "ma26_5" ~ "Retiro",
         metodo_ultimos_seis_meses == "ma26_6" ~ "Inyección anticonceptiva",
         metodo_ultimos_seis_meses == "ma26_7" ~ "Implantes",
         metodo_ultimos_seis_meses == "ma26_8" ~ "Ligadura de trompas",
         metodo_ultimos_seis_meses == "ma26_9" ~ "Vasectomía",
         metodo_ultimos_seis_meses == "ma26_10" ~ "Pastilla del día después",
         metodo_ultimos_seis_meses == "ma26_11" ~ "Método de lactancia materna",
         metodo_ultimos_seis_meses == "ma26_12" ~ "No utilizó",
         metodo_ultimos_seis_meses == "ma26_13" ~ "No tuvo relaciones sexuales",
         metodo_ultimos_seis_meses == "ma26_14" ~ "Condón masculino",
         metodo_ultimos_seis_meses == "ma26_15" ~ "Ns/Nc",
         metodo_ultimos_seis_meses == "ma26_16" ~ "Otro"
      )
   ) %>%
   dplyr::select(
      -value_ultimos_seis_meses,
      -tuvo_primera_relacion
   )

ultima_relacion <- metodos_anticonceptivos %>%
   dplyr::select(
      numero,
      nper,
      tuvo_primera_relacion,
      tidyselect::starts_with("ma27_")
   ) %>%
   dplyr::filter(
      tuvo_primera_relacion == "Sí"
   ) %>%
   dplyr::mutate_all(
      .funs = ~forcats::as_factor(stringr::str_to_sentence(forcats::as_factor(.)))
   ) %>%
   tidyr::pivot_longer(
      cols = tidyselect::starts_with("ma27_"),
      names_to = "metodo_ultima_relacion",
      values_to = "value_ultima_relacion"
   ) %>%
   dplyr::filter(
      value_ultima_relacion == "Sí"
   ) %>%
   dplyr::mutate(
      metodo_ultima_relacion = dplyr::case_when(
         metodo_ultima_relacion == "ma27_1" ~ "Pastillas anticonceptivas",
         metodo_ultima_relacion == "ma27_2" ~ "Condón femenino",
         metodo_ultima_relacion == "ma27_3" ~ "DIU",
         metodo_ultima_relacion == "ma27_4" ~ "Método del calendario",
         metodo_ultima_relacion == "ma27_5" ~ "Retiro",
         metodo_ultima_relacion == "ma27_6" ~ "Inyección anticonceptiva",
         metodo_ultima_relacion == "ma27_7" ~ "Implantes",
         metodo_ultima_relacion == "ma27_8" ~ "Ligadura de trompas",
         metodo_ultima_relacion == "ma27_9" ~ "Vasectomía",
         metodo_ultima_relacion == "ma27_10" ~ "Pastilla del día después",
         metodo_ultima_relacion == "ma27_11" ~ "Método de lactancia materna",
         metodo_ultima_relacion == "ma27_12" ~ "No utilizó",
         metodo_ultima_relacion == "ma27_13" ~ "No tuvo relaciones sexuales",
         metodo_ultima_relacion == "ma27_14" ~ "Condón masculino",
         metodo_ultima_relacion == "ma27_15" ~ "Ns/Nc",
         metodo_ultima_relacion == "ma27_16" ~ "Otro"
      )
   ) %>%
   dplyr::select(
      -value_ultima_relacion,
      -tuvo_primera_relacion
   )

metodos_anticonceptivos <- primera_relacion %>%
   dplyr::left_join(
      ultimos_seis_meses,
      by = base::c("numero", "nper")
   ) %>%
   dplyr::left_join(
      ultima_relacion,
      by = base::c("numero", "nper")
   )

readr::write_rds(x = metodos_anticonceptivos, path = "metodos_anticonceptivos.rds")

#===============#
#### THE END ####
#===============#
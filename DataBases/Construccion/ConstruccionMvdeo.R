#=============================#
#### CONSTRUCCIÓN EN MVDEO ####
#=============================#

library(tidyverse, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(plotly, quietly = TRUE)

# Parseo datos ------------------------------------------------------------
datos_aux <- readxl::read_xls(
   path = here::here("/DataBases/Construccion/Nº Per Sup y Tip Obra_1990.xls"),
   sheet = 1,
   col_names = "col_data",
   range = "A01:A348"
) %>%
   mutate(
      col_data = str_trim(
         string = col_data,
         side = "both"
      )
   )


starts <- str_which(
   string = datos_aux$col_data,
   pattern = "Nueva"
)

ends <- vector(mode = "numeric")
for (i in starts) {

   aux <- datos_aux$col_data[i:length(datos_aux$col_data)]

   end <- i
   end_hallado <- FALSE
   j <- 1
   n <- length(aux)

   while (!end_hallado && j <= n) {

      if (is.na(aux[j])) {

         end_hallado <- TRUE
         end <- i + j - 2

      } else {

         j <- j + 1

      }

   }

   ends <- c(ends, end)

}

years <- datos_aux %>%
   filter(
      str_detect(
         string = col_data,
         pattern = "^([0-9]{4})$"
      )
   ) %>%
   pull(
      col_data
   ) %>%
   as.numeric(.)

rangos <- rbind(
   years,
   starts,
   ends
)

col_names <- c(
   "tipo",
   "total_permisos", "total_superficie",
   "borrar1",
   "vivienda_permisos", "vivienda_superficie",
   "borrar2",
   "comercio_permisos", "comercio_superficie",
   "borrar3",
   "industria_permisos", "industria_superficie",
   "borrar4",
   "otros_permisos", "otros_superficie",
   "borrar5",
   "varios_permisos", "varios_superficie",
   "sindato_permisos", "sindato_superficie"
)

construccion <- tibble::tibble(
   year = numeric(),
   tipo = character(),
   destino = character(),
   permisos = numeric(),
   superficie = numeric()
)

for (i in 1:dim(rangos)[2]) {

   construccion %<>%
      bind_rows(
         readxl::read_xls(
            path = here::here("/DataBases/Construccion/Nº Per Sup y Tip Obra_1990.xls"),
            sheet = 1,
            range = paste0("A", rangos["starts", i], ":", "T", rangos["ends", i]),
            col_names = col_names,
            na = c("-", "..", "...")
         ) %>%
            select(
               -starts_with("borrar"),
               -starts_with("total")
            ) %>%
            mutate(
               year = as.numeric(rangos["years", i])
            ) %>%
            gather(
               key = destino,
               value = value,
               -tipo,
               -year
            ) %>%
            separate(
               col = destino,
               into = c("destino", "variable"),
               sep = "_",
               convert = FALSE
            ) %>%
            pivot_wider(
               names_from = variable,
               values_from = value
            ) %>%
            select(
               year,
               tipo,
               destino,
               permisos,
               superficie
            ) %>%
            mutate_at(
               .vars = c("permisos", "superficie"),
               .funs = as.numeric
            )
      )

}

construccion %<>%
   filter(
      !(is.na(permisos) & is.na(superficie))
   ) %>%
   mutate(
      tipo = str_remove(
         string = tipo,
         pattern = "[:punct:]"
      ),
      tipo = str_to_sentence(
         string = tipo,
         locale = "es"
      ),
      tipo = case_when(
         tipo %in% c(
            "Otros y varios simultáneos",
            "Sin dato de tipo de obra",
            "Varios simultáneos",
            "Simultáneos",
            "Sin dato",
            "Otras"
            ) ~ "Otros",
         tipo %in% c(
            "Modificación en obra",
            "Modificación de obra"
            ) ~ "Modificación de obra",
         TRUE ~ tipo
      )
   )


construccion %>%
   group_by(
      year,
      tipo
   ) %>%
   summarise(
      permisos = sum(permisos)
   ) %>%
   ungroup() %>%
   plot_ly() %>%
   add_trace(
      x = ~year,
      y = ~permisos,
      color = ~tipo,
      type = "scatter",
      mode = "lines+markers"
   )


#===============#
#### THE END ####
#===============#
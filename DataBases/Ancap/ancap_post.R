library(hrbrthemes)
library(tidyverse)

las_que_no <-  c("semanas", "fecha", "dia_semana", "anio", "coronavirus", "turismo")

nafta <- readRDS("DataBases/Ancap/nafta_new.rds") %>%
   pivot_longer(- all_of(las_que_no), names_to = "departamento", values_to = "valor") %>%
   mutate(combustible = "nafta")

gasoil <- readRDS("DataBases/Ancap/gasoil_new.rds") %>%
   pivot_longer(- all_of(las_que_no), names_to = "departamento", values_to = "valor") %>%
   mutate(combustible = "gasoil")

combustibles <- nafta %>%
   bind_rows(gasoil)

combustibles_aux <- combustibles %>%
   filter(anio != 2020) %>%
   group_by(semanas, combustible, departamento) %>%
   summarise(coronavirus = first(coronavirus),
             turismo = first(turismo),
             valor = mean(valor)) %>%
   mutate(anio = "2018-2019")

combustible_total <- combustibles %>%
   filter(anio == 2020) %>%
   select(anio, semanas, combustible, departamento, coronavirus, turismo, valor) %>%
   bind_rows(combustibles_aux) %>%
   mutate(semanas = as.numeric(semanas))

plot <- combustible_total %>%
   filter(combustible == "nafta") %>%
   filter(departamento == "Total x Dia") %>%
   filter(semanas < 21) %>%
   ggplot(aes(semanas, valor, group = anio, color = anio)) +
   geom_point(size = 1.25) +
   geom_line(size = 1) +
   scale_color_manual(values = c("2018-2019" = "#628098", "2020" = "#9AB5EF")) +
   geom_segment(aes(x = 12, xend = 12, y = 0, yend = 87.5), inherit.aes = FALSE, color = "#cccccc") +
   annotate("text", x = 12, y = 90, label = "COVID-19", color = "#2b2b2b", family = "Arial Narrow") +
   #annotate("rect", xmin = 13, xmax = 13.99, ymin = 0, ymax = 121.72655, alpha = .1) +
   labs(title = "Evolución de las ventas semanales de nafta (2018 - 2020)",
        subtitle = "Índice 100 = segunda semana de cada año.",
        caption = "Fuente: elaboración propia en base a datos de ANCAP.\n
                   Información actualizada al 19 de mayo de 2020.",
        x = "Semanas",
        y = "Índice",
        color = "Año") +
   theme_ipsum(axis = T, ticks = T) +
   theme(axis.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 12),
         axis.text.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
   scale_x_continuous(breaks = seq(0, 21, by = 1),
                      expand = c(0, 0), limits = c(0, 21)) +
   scale_y_continuous(expand = c(0, 0), limits = c(0, 123))

ggsave("combustibles.png", plot = plot, path = "static/img", dpi = 550, width = 10, height = 6)


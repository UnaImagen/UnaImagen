library(genderizeR)
library(hrbrthemes)
library(tidyverse)
library(janitor)
library(stringi)
library(readr)
library(rvest)

# Paso 1: uso los datos de la convocatoria 2019

sni_permanencias <- readxl::read_xlsx("DataBases/sni_permanencias.xlsx") %>%
  mutate(primer_nombre = stri_trans_general(str_to_lower(str_extract(Nombre, "\\w+")), "Latin-ASCII"),
         segundo_nombre = stri_trans_general(str_to_lower(str_extract(Nombre, "\\w+$")), "Latin-ASCII"),
         segundo_nombre = case_when(
           primer_nombre == segundo_nombre ~ NA_character_,
           T ~ segundo_nombre),
         nivel_2 = case_when(
           Nivel == "Iniciación dos\r\naños" ~ "Iniciación",
           Nivel == "Iniciación dos años" ~ "Iniciación",
           Nivel == "Iniciación tres" ~ "Iniciación",
           Nivel == "Iniciación tres\r\naños" ~ "Iniciación",
           Nivel == "Iniciación tres años" ~ "Iniciación",
           Nivel == "Iniciación un año" ~ "Iniciación",
           Nivel == "Nivel I dos años" ~ "Nivel I",
           Nivel == "Nivel I tres años" ~ "Nivel I",
           Nivel == "Nivel I un año" ~ "Nivel I",
           Nivel == "Nivel II cuatro" ~ "Nivel II",
           Nivel == "Nivel II cuatro\r\naños" ~ "Nivel II",
           Nivel == "Nivel II cuatro años" ~ "Nivel II",
           Nivel == "Nivel II dos años" ~ "Nivel II",
           Nivel == "Nivel II tres años" ~ "Nivel II",
           Nivel == "Nivel II un año" ~ "Nivel II",
           Nivel == "Nivel III cuatro\r\naños" ~ "Nivel III",
           Nivel == "Nivel III cuatro años" ~ "Nivel III",
           Nivel == "Nivel III cuatro años\r\naños" ~ "Nivel III",
           Nivel == "Nivel III tres años" ~ "Nivel III"
         ))

primer_nombre <- sni_permanencias %>%
  pull(primer_nombre)

segundo_nombre <- sni_permanencias %>%
  pull(segundo_nombre)

gender_pn <- genderizeR::findGivenNames(primer_nombre, country = "UY", language = "es") %>%
  rename(primer_nombre = name) %>%
  distinct(primer_nombre, gender)

sni_permanencias <- sni_permanencias %>%
  left_join(gender_pn) %>%
  janitor::clean_names() %>%
  transmute(nombre,
            apellido,
            primer_nombre,
            gender,
            area,
            nivel = nivel_2,
            categoria)

faltantes <- sni_permanencias %>%
  filter(is.na(gender)) %>%
  pull(nombre)

gender_aux <- genderizeR::findGivenNames(faltantes,  language = "es") %>%
  rename(primer_nombre = name,
         gender_aux = gender) %>%
  distinct(primer_nombre, gender_aux)

sni_permanencias <- sni_permanencias %>%
  left_join(gender_aux) %>%
  mutate(gender = coalesce(gender, gender_aux),
         sexo = case_when(
           gender == "male" ~ "Varón",
           gender == "female" ~ "Mujer"),
         nivel = factor(nivel, levels = c("Iniciación", "Nivel I", "Nivel II", "Nivel III"))) %>%
  select(- starts_with("gender")) %>%
  write_rds("DataBases/sni_permanencias.rds")

# 2) Mateo scrappeó los datos del SNI sin actualizar

html_sni <- "https://sni.org.uy/buscador/" %>%
  read_html()

tabla_investigadores <- html_sni %>%
  html_nodes(".vc_table.tabla_buscador") %>%
  html_table() %>%
  .[[1]] %>%
  clean_names() %>%
  transmute(categoria, area, sub_area, nivel, investigador,
            nombre   = str_extract_all(investigador, pattern = "\\,.+"),
            apellido = str_extract_all(investigador, pattern = ".+\\,"),
            nombre   = str_replace_all(nombre, pattern = "^, ", replace = ""),
            apellido = str_replace_all(apellido, pattern = " ,$", replace = ""),
            nombre   = str_to_title(nombre),
            apellido = str_to_title(apellido)) %>%
  arrange(categoria, area, sub_area, nivel, apellido) %>%
  readr::write_rds("DataBases/tabla_investigadores.rds")

sni <- tabla_investigadores %>%
  mutate(primer_nombre = stri_trans_general(str_to_lower(str_extract(nombre, "\\w+")), "Latin-ASCII"),
         segundo_nombre = stri_trans_general(str_to_lower(str_extract(nombre, "\\w+$")), "Latin-ASCII"),
         segundo_nombre = case_when(
           primer_nombre == segundo_nombre ~ NA_character_,
           T ~ segundo_nombre),
         nivel = factor(nivel, levels = c("Iniciación", "Nivel I", "Nivel II", "Nivel III")))

primer_nombre <- sni %>%
  pull(primer_nombre)

segundo_nombre <- sni %>%
  pull(segundo_nombre)

gender_pn <- genderizeR::findGivenNames(primer_nombre, country = "UY", language = "es") %>%
  rename(primer_nombre = name) %>%
  distinct(primer_nombre, gender)

sni <- sni %>%
  left_join(gender_pn) %>%
  transmute(categoria,
            area,
            sub_area,
            nivel,
            investigador,
            nombre,
            apellido,
            primer_nombre,
            gender,
            categoria)

faltantes <- sni %>%
  filter(is.na(gender)) %>%
  pull(primer_nombre)

gender_aux <- genderizeR::findGivenNames(faltantes,  language = "es") %>%
  rename(primer_nombre = name,
         gender_aux = gender) %>%
  distinct(primer_nombre, gender_aux)

sni <- sni %>%
  left_join(gender_aux) %>%
  mutate(gender = coalesce(gender, gender_aux),
         sexo = case_when(
           gender == "male" ~ "Varón",
           gender == "female" ~ "Mujer"),
         area = ifelse(area == "Ciencias Médicas y de la\r\nSalud", "Ciencias Médicas y de la Salud", area)) %>%
  select(- gender, - gender_aux) %>%
  readr::write_rds("DataBases/sni.rds")

# 3) ACtualizo los dtaos del SNI con la convocatoria 2019 y corrige MUCHO a mano)

sni_permanencias <- read_rds("DataBases/sni_permanencias.rds") %>%
  select(nombre, apellido, primer_nombre, nivel, area, sexo)

total_sni <- read_rds("DataBases/sni.rds") %>%
  select(nombre, apellido, primer_nombre, nivel, area, sexo) %>%
  bind_rows(sni_permanencias) %>%
  mutate(area = case_when(
    area == "Ciencias Médicas y de la\r\nSalud" ~ "Ciencias Médicas y de la Salud",
    T ~ area
  ),
  nombre = str_trim(nombre, side = "right"),
  apellido = str_trim(apellido, side = "right")) %>%
  distinct(nombre, apellido, primer_nombre, nivel, area, sexo) %>%
  group_by(apellido, nombre) %>%
  summarise(primer_nombre = first(primer_nombre),
            nivel = last(nivel),
            area = first(area),
            sexo = first(sexo)) %>%
  ungroup() %>%
  filter(nombre != "álvaro Gustavo") %>%
  filter(nombre != "Bernardo Rómulo") %>%
  filter(nombre != "César Augusto") %>%
  filter(apellido != "Abreu") %>%
  filter(nombre != "álvaro Hugo") %>%
  filter(nombre != "Verónica Lucy") %>%
  filter(nombre != "Sergio Alexandre") %>%
  filter(nombre != "Carlos Tabaré Fidel") %>%
  filter(nombre != "Federico Jesãºs") %>%
  filter(apellido != "Fraiman") %>%
  filter(apellido != "Fornaro") %>%
  filter(apellido != "Varela Pensado,") %>%
  filter(apellido != "Perdomo") %>%
  filter(apellido != "Perciante") %>%
  filter(apellido != "Peluffo Bossio,") %>%
  filter(apellido != "Paulino") %>%
  filter(apellido != "Otero") %>%
  filter(apellido != "Olivera Cajiga,") %>%
  filter(apellido != "Olazabal") %>%
  filter(apellido != "Vanrell Majó,") %>%
  filter(apellido != "Achigar Pereira") %>%
  filter(apellido != "álvarez Pedrosian") %>%
  filter(apellido != "Trías Tejería") %>%
  filter(apellido != "Tancredi Machado") %>%
  filter(apellido != "Yannicelli") %>%
  filter(apellido != "Viola") %>%
  filter(apellido != "Souza Antognazza") %>%
  filter(apellido != "Soust-verdaguer") %>%
  filter(apellido != "Slomovitz") %>%
  filter(apellido != "Sensale") %>%
  filter(apellido != "Schelotto Guillamon") %>%
  filter(apellido != "Sanchez Tellechea") %>%
  filter(apellido != "Sanabria") %>%
  filter(apellido != "Romero Rodriguez") %>%
  filter(apellido != "Romano Granito,") %>%
  filter(apellido != "Rittatore Calvo,") %>%
  filter(apellido != "Risso Ferrand,") %>%
  filter(apellido != "Pouso") %>%
  filter(apellido != "Potrie") %>%
  filter(apellido != "Pizarro") %>%
  filter(apellido != "Méndez") %>%
  filter(apellido != "Mazzilli Vanzini,") %>%
  filter(apellido != "Martínez D´alto") %>%
  filter(apellido != "Piñeiro Guerra,") %>%
  filter(apellido != "Perez Miles") %>%
  filter(apellido != "Perez De Sierra") %>%
  filter(apellido != "Markarian Abrahamian") %>%
  filter(apellido != "Marizcurrena") %>%
  filter(apellido != "Lanzaro Peyre,") %>%
  filter(apellido != "Irisarri") %>%
  filter(apellido != "Langone") %>%
  filter(apellido != "Gonzalez Guyer") %>%
  filter(apellido != "Enciso Deleon") %>%
  filter(apellido != "Dominguez") %>%
  filter(apellido != "De Cores") %>%
  filter(apellido != "D´anatro Gómez") %>%
  filter(apellido != "Crosignani") %>%
  filter(apellido != "Cristiani Labat,") %>%
  filter(apellido != "Chiappe") %>%
  filter(apellido != "Celentano Campodonico") %>%
  filter(apellido != "Cavestany  Böcking,") %>%
  filter(apellido != "Broquetas") %>%
  filter(apellido != "Brito Diaz") %>%
  filter(apellido != "Brida") %>%
  filter(apellido != "Block Teper") %>%
  filter(!(nombre == "Camou Soliño" & apellido == "Maria Teresa")) %>%
  filter(!(nombre == "Paula Maria" & apellido == "Cardellino Alvarez")) %>%
  filter(!(nombre == "Adriana María" & apellido == "Cassina Gomez")) %>%
  filter(!(nombre == "Amílcar" & apellido == "Davyt")) %>%
  filter(!(nombre == "Andrea" & apellido == "Delgado Cavaliere")) %>%
  filter(!(nombre == "Pablo Andres" & apellido == "Denis Marinoni")) %>%
  filter(!(nombre == "Laura Pilar" & apellido == "Domínguez Llera")) %>%
  filter(!(nombre == "Maria Ines" & apellido == "Fariello Rico")) %>%
  filter(!(nombre == "Guzman" & apellido == "Favre Silva")) %>%
  filter(!(nombre == "Gustavo" & apellido == "Vazquez")) %>%
  filter(!(nombre == "álvaro" & apellido == "Rovella Osores")) %>%
  filter(!(nombre == "Pablo" & apellido == "Rodríguez Bocca")) %>%
  filter(!(nombre == "Mariela" & apellido == "Quiñones Montoro")) %>%
  filter(!(nombre == "Diego" & apellido == "Queirolo"))  %>%
  filter(!(nombre == "álvaro" & apellido == "Martín Menoni")) %>%
  write_rds("DataBases/total_sni.rds")

# 4) Grafico

read_rds("DataBases/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, nivel) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      nivel == "Iniciación" ~ valor/559,
      nivel == "Nivel I" ~ valor/924,
      nivel == "Nivel II" ~ valor/345,
      nivel == "Nivel III" ~ valor/113
    ),
    label = paste0(round((porcentaje)*100), "%")) %>%
  ggplot(aes(nivel, porcentaje, group = sexo, color = sexo, label = label)) +
  geom_line() +
  geom_point() +
  geom_text(data = sni_2020[sni_2020$sexo == "Mujer", ], aes(vjust = 1.4), size = 3.5) +
  geom_text(data = sni_2020[sni_2020$sexo == "Varón", ], aes(vjust = -.7), size = 3.5) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = seq(0, 0.85, by = .1),) +
  labs(title = "Composición del Sistema Nacional de Investigadores según nivel y sexo: mujeres y varones. Año 2020.",
       caption = "Fuente: elaboración propia en base a datos del SNI.") +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Arial Narrow"),
        legend.title = element_blank(),
        legend.position = "none") +
  ggsave("static/img/sni_2020.png", dpi = 550, width = 11, height = 6)

sni_area <- read_rds("DataBases/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, area) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      area == "Ciencias Agrícolas" ~ valor/236,
      area == "Ciencias Médicas y de la Salud" ~ valor/239,
      area == "Ciencias Naturales y Exactas" ~ valor/689,
      area == "Ciencias Sociales"	 ~ valor/416,
      area == "Humanidades" ~ valor/168,
      area == "Ingeniería y Tecnología" ~ valor/193),
    label = paste0(round((porcentaje)*100), "%"),
    area = factor(area, levels = c("Humanidades", "Ciencias Médicas y de la Salud",
                                   "Ciencias Sociales", "Ciencias Agrícolas",
                                   "Ciencias Naturales y Exactas", "Ingeniería y Tecnología" )))

ggplot(sni_area, aes(area, porcentaje, fill = sexo, label = label)) +
  geom_bar(position = "fill", stat = "identity", alpha = .7) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  geom_text(aes(label = label), position = position_fill(vjust = .5), family = "Arial Narrow") +
  labs(title = "Investigadores del SNI según área de conocimiento y sexo: mujeres y varones. Año 2020.",
       caption = "Fuente: elaboración propia en base a datos del SNI.") +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Arial Narrow"),
        legend.title = element_blank(),
        legend.position = "none") +
  ggsave("static/img/sni_area_2020.png", dpi = 550, width = 12, height = 6)

sni_area_nivel <- read_rds("DataBases/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, nivel, area) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      nivel == "Iniciación"	& area == "Ciencias Agrícolas" ~ valor/92,
      nivel == "Iniciación"	& area == "Ciencias Médicas y de la Salud" ~ valor/75,
      nivel == "Iniciación"	& area == "Ciencias Naturales y Exactas" ~ valor/141,
      nivel == "Iniciación"	& area == "Ciencias Sociales"	 ~ valor/146,
      nivel == "Iniciación"	& area == "Humanidades" ~ valor/50,
      nivel == "Iniciación"	& area == "Ingeniería y Tecnología" ~ valor/55,
      nivel == "Nivel I" & area == "Ciencias Agrícolas" ~ valor/92,
      nivel == "Nivel I" & area == "Ciencias Médicas y de la Salud" ~ valor/116,
      nivel == "Nivel I" & area == "Ciencias Naturales y Exactas" ~ valor/351,
      nivel == "Nivel I" & area == "Ciencias Sociales" ~ valor/202,
      nivel == "Nivel I" & area == "Humanidades" ~ valor/80,
      nivel == "Nivel I" & area == "Ingeniería y Tecnología" ~ valor/83,
      nivel == "Nivel II" & area == "Ciencias Agrícolas" ~ valor/40,
      nivel == "Nivel II"	& area == "Ciencias Médicas y de la Salud" ~ valor/30,
      nivel == "Nivel II" & area == "Ciencias Naturales y Exactas" ~ valor/148,
      nivel == "Nivel II" & area == "Ciencias Sociales" ~ valor/53,
      nivel == "Nivel II" & area == 	"Humanidades"	 ~ valor/27,
      nivel == "Nivel II" & area == "Ingeniería y Tecnología" ~ valor/47,
      nivel == "Nivel III" & area == 	"Ciencias Agrícolas" ~ valor/12,
      nivel == "Nivel III" & area == 	"Ciencias Médicas y de la Salud" ~ valor/18,
      nivel == "Nivel III" & area == 	"Ciencias Naturales y Exactas" ~ valor/49,
      nivel == "Nivel III" & area == 	"Ciencias Sociales" ~ valor/15,
      nivel == "Nivel III" & area == 	"Humanidades"	 ~ valor/11,
      nivel == "Nivel III" & area == 	"Ingeniería y Tecnología"	 ~ valor/8
    ),
    label = paste0(round((porcentaje)*100), "%"))

ggplot(sni_area_nivel, aes(nivel, porcentaje, group = sexo, color = sexo, label = label)) +
  geom_line() +
  geom_point() +
  geom_text(data = sni_area_nivel[sni_area_nivel$sexo == "Mujer", ], aes(vjust = 1.4), size = 2.5) +
  geom_text(data = sni_area_nivel[sni_area_nivel$sexo == "Varón", ], aes(vjust = -.7), size = 2.5) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = seq(0, .95, by = .1),
                     expand = c(.1, .1)) +
  labs(title = "Composición del Sistema Nacional de Investigadores según nivel y sexo: mujeres y varones. Año 2020.",
       caption = "Fuente: elaboración propia en base a datos del SNI.") +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Arial Narrow"),
        legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(~ area) +
  ggsave("static/img/sni_area_nivel_2020.png", dpi = 550, width = 11, height = 7)
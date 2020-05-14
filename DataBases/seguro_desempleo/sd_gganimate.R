library(tidyverse) # general manipulation
library(lubridate) # date manipulation
library(gganimate) # animated charts

historical_sd <- read_rds("DataBases/seguro_desempleo/historical_sd.rds")

# Solicitudes de seguro de desempleo - histórico (1988-2020)

plot <- ggplot(historical_sd, aes(x = date, y = Total)) +
   geom_path(data = historical_sd, aes(x = date, y = Total, group = 1),  colour = "#7b5888") + # definte extra data set to make it static
   scale_y_continuous(breaks = seq(0, 90000, by = 10000),
                      labels = scales::number_format(big.mark = ".")) +
   scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0,0)) +
   labs(title = "Solicitudes mensuales de seguro de desempleo en Uruguay (1988-2020)",
        x = "",
        y = "",
        caption = "Fuente: elaboración propia en base a datos del BPS.") +
   theme_minimal() +
   theme(panel.grid = element_blank(),
         panel.grid.major.y = element_line(colour = "grey95"),
         axis.title.y = element_text(size = 8, vjust = 3),
         axis.text.y = element_text(size = 8),
         axis.line.x = element_line(),
         plot.title = element_text(hjust = 0.5, face = "bold"),
         plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
         plot.caption = element_text(colour = "grey50"),
         text = element_text(family = "Arial Narrow")) +
   transition_reveal(date) +
   view_follow(fixed_y = TRUE, fixed_x = TRUE) +
   coord_cartesian(clip = 'off') +
   ease_aes('cubic-in-out')

options(gganimate.dev_args = list(height = 4,
                                  width = 4*1.777778,
                                  units = 'in',
                                  type = "cairo",
                                  res = 144))

animate(plot = plot,
        renderer = gifski_renderer("static/img/evolucion_sd.gif"),
        fps = 20, duration = 12, end_pause = 80)


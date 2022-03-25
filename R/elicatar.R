# SCRIPT UDAVIZ - SIN NORMALIZAR, PREVIA NORMALIZACIÓN MANUAL
# RENDIMIENTO ELIMINATORIAS CONMEBOL RUMBO CATAR 
# RENDIMIENTO PORCENTUAL DE LAS SELECCIONES CLASIFICADAS TERMINADA LA FECHA

# LIBRERIAS
library(readr)
library(dplyr)
library(ggplot2)
library(ggradar)
library(scales)
library(data.table)

dat <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/coneli.csv")

# tabla con puntos y goles a favor
tab <- dat %>% 
  mutate(Puntos = (Victorias * 3) + (Empates *1), Goles.diferencia = (Goles.marcados - Goles.recibidos))

# tabla de posiciones
tabg <- tab[order(tab$Puntos, tab$Goles.diferencia, decreasing = TRUE),]

# tabla de posiciones de acuerdo a filtro de búsqueda
tabx <- tabg %>% 
  filter(Equipo %in% c("Brasil", "Argentina", "Ecuador", "Uruguay", "Colombia")) %>% 
  select(Equipo, Puntos, Partidos.Jugados, Victorias, Derrotas, Empates) %>%
  rename(group = Equipo) %>%
  mutate(Puntos = Puntos / (Partidos.Jugados * 3), 
         Victorias = (Victorias / Partidos.Jugados), 
         Derrotas = Derrotas / Partidos.Jugados,
         Empates = Empates / Partidos.Jugados,
         Partidos.Jugados = Partidos.Jugados / Partidos.Jugados) %>%
  mutate_at(vars(-group), funs(rescale_none))

tabx

#ggradar(tabx)

ggradar(tabx, #font.radar = "Circular Air",
        background.circle.colour = "lightgray",
        axis.line.colour = "steelblue",
        grid.label.size = 5, 
        axis.label.size = 5, 
        group.point.size = 5, 
        group.line.width = 1.5,
        legend.title = "Selecciones",
        legend.position = "bottom",
        legend.text.size = 10) + 
  scale_color_manual(values = c("#3366ff", "#33ff00", "#cc0033", "#ffcc33", "#ccffff")) +
  labs(title = "Rendimiento de selecciones clasificadas terminada la Fecha 8", 
       subtitle = "Eliminatorias Sudamericanas al mundial Qatar 2022 - Corte 8 Junio 2021",
       caption = "powered by UDAVIZ") +
  #theme_minimal()
  theme(plot.title = element_text(hjust = 0.5, size = 22), 
        plot.subtitle = element_text(hjust = 0.5, size = 13))
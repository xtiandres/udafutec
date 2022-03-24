# CAMERICA BRASIL - ZEROZERO.PT - LECTURA "BRASIL2021.CSV"

library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggradar)
library(data.table)
library(gridExtra)

data <- read.csv("/home/xut/Documents/udaviz/R/studio/camerica/data/brasil2021.csv")
#data
dtotal <- data %>% mutate(PTOS = (V * 3) + (E *1), GD = (G - GS))
#dtotal

dga <- dtotal %>% 
  select(EQUIPO, J, V, E, D, G, PEN, AG, V3, GS, GRUPO, PTOS, GD) %>% 
  filter(GRUPO == "A")
dga <- dga[order(dga$PTOS, dga$GD, decreasing = TRUE),]
dga <- select(dga, -GRUPO)
dga

dgb <- dtotal %>% 
  select(EQUIPO, J, V, E, D, G, PEN, AG, V3, GS, GRUPO, PTOS, GD) %>% 
  filter(GRUPO == "B")
dgb <- dgb[order(dgb$PTOS, dgb$GD, decreasing = TRUE),]
dgb <- select(dgb, -GRUPO)
#dgb

txt <- paste("J PARTIDOS JUGADOS",
             "V VICTORIAS",
             "E EMPATES",
             "D DERROTAS",
             "G GOLES MARCADOS",
             "PEN PENALES",
             "AG AUTOGOLES",
             "V3 GOLEADAS",
             "GS GOLES RECIBIDOS",
             "PTOS PUNTOS",
             "GD GOL DIFERENCIA",
             sep = "\n")

plotdga <- dga %>% rename(group = EQUIPO) %>% mutate_at(vars(-group), funs(rescale))
ggradar(plotdga, 
        grid.label.size = 3,
        axis.label.size = 3) + 
  facet_wrap(~group) +
  scale_color_manual(values = c("#3366ff", "#33ff00", "#cc0033", "#330066", "#00ccff")) +
  labs(title = "Rendimiento GRUPO A", 
       subtitle = "Copa América Brasil 2021 - Corte 16 Junio",
       caption = "powered by UDAVIZ") +
  theme_minimal() +
  theme(legend.position = "none")
# ELIMINATORIAS SUDAMERICANAS - PROBABILIDADES

library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggradar)
library(data.table)
library(gridExtra)
library(ggrepel)
library(grid)
library(ggiraphExtra)
library(fmsb)
library(ggridges)
library(formattable)
library(plyr)
library(vcd)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(ggpubr)

# DATA
eli <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/elipro.txt")

eli <- gather(eli,
              key = "posicion",
              value = "valor",
              Primero:Sexto)

eli$Equipos <- factor(eli$Equipos, 
                      levels = c("Brasil", "Argentina", "Ecuador", "Uruguay",
                                 "Peru", "Chile", "Colombia", "Bolivia",
                                 "Paraguay", "Venezuela"))
eli$valor <- as.numeric(eli$valor)
eli$porce <- eli$valor/100

#Tabla 2da Etapa PJ-Ptos-G-E-D-GF-GC-GD
#tabla2 <- mutate(tabla2,
#                 GolesDiferencia = GolesFavor - GolesContra)


#Gráficos

#Dos variables- Categoría vs Cantidad

#Bar chart
#t1 <- ggplot(tablat,
#             aes(y = Equipos,
#                 x = Puntos))

#t1 + geom_bar(stat = "identity",
#              fill = "cornflowerblue") +
#  geom_text(aes(label = Puntos),
#            vjust = -0.25) +
#  labs(title = "Test1",
#       subtitle = "Puntaje Tabla Acumulada")

#Grouped kernel density plots - Issues
#t2 <- ggplot(tablat,
#             aes(x = PartidosJugados,
#                 fill = Equipos))

#t2 + geom_density(alpha = .4) +
#  labs(title = "Test2",
#       subtitle = "Distribución de Puntaje por Equipos")

#Ridgeline plots - Issues
#t3 <- ggplot(tablat,
#             aes(x = Puntos,
#                 y = Equipos,
#                 fill = Puntos))

#t3 + geom_density_ridges() +
#  theme_ridges() + 
#  labs(title = "Test3",
#       subtitle = "Distribución de Puntaje por Equipos")

#Cleveland Dot Charts - Gráfico Tabla Acumulada Equipos vs Puntos
t4 <- ggplot(eli,
             aes(x = posicion,
                 y = porce,
                 size = porce))

t4 + 
  geom_point(alpha = .6) +
  geom_text_repel(label = scales::percent(eli$porce),
                  vjust = 0.5,
                  size = 3.3) +
  #geom_text(aes(label = porce),
  #          vjust = -0.5) +
  facet_wrap(~Equipos, nrow = 2) +
  aes(x = fct_inorder(posicion)) +
  scale_y_continuous(labels = scales::percent) +
  scale_size(range = c(0,5),
             labels = scales::percent) +
  labs(title = "Eliminatorias Qatar - Conmebol",
       subtitle = "Probabilidad de clasificación previa la Jornada 17",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

#Multivariables - Grouping
#t5 <- ggplot(tablat,
#             aes(x = Puntos,
#                 y = Ganados,
#                 color = Equipos,
#                 size = GolesDiferencia))

#t5 + geom_point(alpha = .6) +
#  labs(title = "Test5",
#       subtitle = "Puntaje Tabla Acumulada")



#Multivariables - Faceting - necesito presupuesto oficial
#t6 <- ggplot(tablat,
#             aes(x = Puntos,
#                 y = Ganados))

#t6 + geom_line(color = "grey") +
#  geom_point(color = "blue") +
#  facet_wrap(~Equipos, ncol = 4) +
#  theme_minimal(base_size = 9) +
#  theme(axis.text.x = element_text(angle = 45, 
#                                   hjust = 1)) +
#  labs(title = "Test6",
#       subtitle = "Multivariables Facet")

#Multivariables - Faceting 2 - databsctest
#t7 <- ggplot(tabla2e,
#             aes(x = PartidosJugados, 
#                 y = Eficacia))

#t7 + geom_point(size = 3) +
#geom_errorbar(aes(ymin = mean - se, 
#                  ymax = mean + se),
#              width = .1) +
#scale_y_continuous(breaks = seq(70000, 140000, 10000),
#                   label = scales::dollar) +
#  facet_grid(~ Equipos) +
#  theme_bw() +
#  theme(legend.position = "none",
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor.y = element_blank()) +
#  labs(x="", 
#       y="", 
#       title="T7",
#       subtitle = "(T7)") +
#  scale_color_brewer(palette="Set1")

#Por fechas - necesito presupuesto oficial
#t8 <- ggplot(databsc,
#             aes(x = Fecha,
#                 y = GolLocal))

#t8 + geom_line(color = "indianred3",
#               size = 1) +
#  geom_smooth() +
#  labs(title = "Test8",
#       subtitle = "xxx")


#SELECCION VARIABLES Y FILTRO DE EQUIPOS
#df <- dtotal %>% 
#  select(EQUIPO, V, G, E, D, GS, PEN, AG, V3) %>% 
#  filter(EQUIPO == "Argentina" | EQUIPO == "Brasil")

#NOMBRE DE VARIABLES  
#colnames(df) <- c("EQUIPO", 
#                  "VICTORIAS", 
#                  "GOLES MARCADOS", 
#                  "EMPATES",
#                  "DERROTAS",
#                  "GOLES RECIBIDOS",
#                  "PENALES",
#                  "AUTOGOLES",
#                  "GOLEADAS")
#df
#SELECCION SIN VARIABLE EQUIPO
#df <- select(df, -EQUIPO)
#AÑADIR VALORES MAXIMOS Y MINIMOS PARA RADARCHART
#df <- rbind(rep(12,5) , rep(0,2) , df)
#df

#COLORES DEL RADARCHART
#colors_border=c("#ffff00", "#0000ff")
#colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

#svglite::svglite("final.svg")

#RADARCHART
#radarchart(df, 
#           axistype=1,
#           seg = 6,
#custom polygon
#           pcol=colors_border,
#           pfcol=colors_in,
#           plwd=4,
#          plty=1,
#custom the grid
#          cglcol="grey", 
#           cglty=1, 
#           axislabcol="navy", 
#           caxislabels=seq(0,12,2), cglwd=0.7,
#custom labels
#           vlcex=.8,

#)

#legend(x=1.1, y=1,
#       legend = c("Argentina", "Brasil"),
#legend = rownames(dea[-c(1,2),]), 
#       bty = "n", 
#       pch=20, 
#col=colors_border,
#       col=c("#0000ff", "#ffff00"),
#       text.col = "black", 
#       cex=1, 
#       pt.cex=3)

#dev.off()
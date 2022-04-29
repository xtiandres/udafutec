# COPA AMERICA BRASIL - CUARTOS DE FINAL - ARGENTINA VS ECUADOR
# RADAR CHART

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

data <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/brasil2021.csv")

#CALCULO DE PUNTAJE Y GOL DIFERENCIA
dtotal <- data %>% mutate(PTOS = (V * 3) + (E *1), GD = (G - GS))

#SELECCION VARIABLES Y FILTRO DE EQUIPOS
dae <- dtotal %>% 
  select(EQUIPO, V, G, E, D, GS, PEN, AG, V3) %>% 
  filter(EQUIPO == "Argentina" | EQUIPO == "Ecuador")

#NOMBRE DE VARIABLES  
colnames(dae) <- c("EQUIPO", 
                   "VICTORIAS", 
                   "GOLES MARCADOS", 
                   "EMPATES",
                   "DERROTAS",
                   "GOLES RECIBIDOS",
                   "PENALES",
                   "AUTOGOLES",
                   "GOLEADAS")
dae
#SELECCION SIN VARIABLE EQUIPO
dae <- select(dae, -EQUIPO)
#AÑADIR VALORES MAXIMOS Y MINIMOS PARA RADARCHART
dae <- rbind(rep(7,5) , rep(0,2) , dae)
dae

#COLORES DEL RADARCHART
colors_border=c("#ffff00", "#0000ff")
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )


#svg("argvecu.svg", width = 14, height = 7)

#RADARCHART
radarchart(dae, 
           axistype=1,
           seg = 7,
           #custom polygon
           pcol=colors_border,
           pfcol=colors_in,
           plwd=4,
           plty=1,
           #custom the grid
           cglcol="grey", 
           cglty=1, 
           axislabcol="navy", 
           caxislabels=seq(0,7,1), cglwd=0.7,
           #custom labels
           vlcex=.8,
           
)

legend(x=1.1, y=1,
       legend = c("Argentina", "Ecuador"),
       #legend = rownames(dea[-c(1,2),]), 
       bty = "n", 
       pch=20, 
       #col=colors_border,
       col=c("#0000ff", "#ffff00"),
       text.col = "black", 
       cex=1, 
       pt.cex=3)

#dev.off()
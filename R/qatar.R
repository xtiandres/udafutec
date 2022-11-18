# QATAR - 2022
# SCRIPT DE TODAS LAS JORNADAS

# LIBRERIAS
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
library(forcats)
library(RColorBrewer)

# DATASET 
datecu <- read_csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/ecuador.csv")
datqat <- read_csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/qatar.csv")
datmpi <- read_csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/pi.csv")

# PARTIDO INAUGURAL
datmpi <- mutate(datmpi, 
                 IL = ifelse(GL >= GV,
                             "SI",
                             "NO"))
pi <- ggplot(datmpi,
             aes(x = Local,
                 fill = IL)) +
  geom_bar(position = 'stack') +
  #geom_line(color = 'grey') +
  #geom_point(color = 'blue') +
  facet_wrap(~Mundial) +
  theme_minimal()
#+
#  aes(x = fct_inorder(Mundial))
  

# TABLA TOTAL ECUADOR - QATAR
decuqat <- data.frame(
  Seleccion = c("Ecuador", "Qatar"),
  PromedioEdad = c(mean(datecu$Edad), mean(datqat$Edad)),
  ValorSeleccion = c(sum(datecu$Valoracion), sum(datqat$Valoracion)),
  Rank = c(44, 50),
  PartidosJugados22 = c(10, 7),
  Victorias22 = c(2, 3),
  Empates22 = c(7, 3),
  Derrotas22 = c(1, 1),
  Goles22 = c(6, 8)
)

# TABLA PARCIAL ECUADOR - QATAR
ecqa <- select(decuqat, -PromedioEdad, -ValorSeleccion, -Rank)
colnames(ecqa) <- c("EQUIPO", 
                    "PARTIDOS.2022",
                    "VICTORIAS",
                    "EMPATES",
                    "DERROTAS",
                    "GOLES")
ecqa <- select(ecqa, -EQUIPO)
#AÑADIR VALORES MAXIMOS Y MINIMOS PARA RADARCHART
ecqa <- rbind(rep(10,5) , rep(0,2) , ecqa)
#COLORES DEL RADARCHART
colors_border=c("#ffff00", "#ff0000")
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
#svg("argvecu.svg", width = 14, height = 7)
#RADARCHART
radarchart(ecqa, 
           axistype=2,
           seg = 5,
           #custom polygon
           pcol=colors_border,
           pfcol=colors_in,
           plwd=5,
           plty=1,
           #custom the grid
           cglcol="grey", 
           cglty=3, 
           axislabcol="navy", 
           caxislabels=seq(0,10,5), cglwd=0.7,
           #custom labels
           vlcex=.9,
)
legend(x=1.3, y=1,
       legend = c("Ecuador", "Qatar"),
       #legend = c("Ecuador = mean(datecu$Edad)", mean(datqat$Edad)),
       #legend = rownames(dea[-c(1,2),]), 
       bty = "n", 
       pch=20, 
       #col=colors_border,
       col=c("#ffff00", "#ff0000"),
       text.col = "black", 
       cex=1.3, 
       pt.cex=4)
#legend(x=1.3, y=.9,
#       legend = c(mean(datecu$Edad), mean(datqat$Edad)),
#       bty = "n", 
#       text.col = "black", 
#       cex=1.3)

pi
#dev.off()



# QATAR - 2022

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
datqwc <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/qatar2022.csv")

datmpi <- read_csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/pi.csv")
datecu22 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/ecuador22.csv")
datqat22 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/qatar22.csv")

# VARIABLE VALORACION CAMBIAR k,m AS NUMERIC
datqwc$Valoracion <- ifelse(grepl('m', ignore.case = TRUE, datqwc$Valoracion), as.numeric(gsub("[€m]", "", datqwc$Valoracion)) * 10^6,
                            as.numeric(gsub("[€k]", "", datqwc$Valoracion)) * 10^3)

# ECUADOR
ecu <- filter(datqwc,
              Seleccion == 'Ecuador')
# QATAR
qat <- filter(datqwc,
              Seleccion == 'Qatar')
# TOTAL ECUADOR - QATAR
ecuqat <- data.frame(
  Seleccion = c("Ecuador", "Qatar"),
  PromedioEdad = c(round(mean(ecu$Edad),digits=2), round(mean(qat$Edad),2)),
  ValorSeleccion = c(sum(ecu$Valoracion), sum(qat$Valoracion)),
  Ranking = c(44, 50),
  PartidosJugados = c(10, 7),
  Victorias = c(2, 3),
  Empates = c(7, 3),
  Derrotas = c(1, 1),
  Goles = c(6, 8)
)
write_csv(ecuqat, file = 'data/qatar/ecuadorqatar.csv')

# PARCIAL ECUADOR - QATAR
ecqa <- select(ecuqat, -PromedioEdad, -ValorSeleccion, -Ranking)
colnames(ecqa) <- c("EQUIPO", 
                    "PARTIDOS22",
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

#dev.off()



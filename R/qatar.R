# QATAR WORLD CUP - 2022

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


# *** DATASETS ***
# PLANTILLA DE SELECCIONES
datqwc <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/qatar2022.csv")
# CALENDARIO Y RESULTADOS
dqwc22 <- read_csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/seleccione.csv")
# HISTORICO PARTIDO INAUGURAL
datmpi <- read_csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/pi.csv")
# ECUADOR 2022 HISTORICO
datecu22 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/ecuador22.csv")
# QATAR 2022 HISTORICO
datqat22 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/qatar/qatar22.csv")


# *** DATASET: PLANTILLA DE SELECCIONES datqwc ***
# VARIABLE VALORACION CAMBIAR k,m AS NUMERIC
datqwc$Valoracion <- ifelse(grepl('m', ignore.case = TRUE, datqwc$Valoracion), as.numeric(gsub("[€m]", "", datqwc$Valoracion)) * 10^6,
                            as.numeric(gsub("[€k]", "", datqwc$Valoracion)) * 10^3)

# PLANTILLA ECUADOR
ecu <- filter(datqwc,
              Seleccion == 'Ecuador')
# PLANTILLA QATAR
qat <- filter(datqwc,
              Seleccion == 'Qatar')
# TABLA COMPARATIVA ECUADOR - QATAR
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

# TABLA PARCIAL ECUADOR - QATAR
ecqa <- select(ecuqat, -PromedioEdad, -ValorSeleccion, -Ranking)
colnames(ecqa) <- c("EQUIPO", 
                    "PARTIDOS22",
                    "VICTORIAS",
                    "EMPATES",
                    "DERROTAS",
                    "GOLES")
ecqa <- select(ecqa, -EQUIPO)

#AÑADIR VALORES MAXIMOS Y MINIMOS PARA RADARCHART
#ecqa <- rbind(rep(10,5) , rep(0,2) , ecqa)
#COLORES DEL RADARCHART
#colors_border=c("#ffff00", "#ff0000")
#colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
#svg("argvecu.svg", width = 14, height = 7)
#RADARCHART
#radarchart(ecqa, 
#           axistype=2,
#           seg = 5,
           #custom polygon
#           pcol=colors_border,
#           pfcol=colors_in,
#           plwd=5,
#           plty=1,
           #custom the grid
#           cglcol="grey", 
#           cglty=3, 
#           axislabcol="navy", 
#           caxislabels=seq(0,10,5), cglwd=0.7,
           #custom labels
#           vlcex=.9,
#)
#legend(x=1.3, y=1,
#       legend = c("Ecuador", "Qatar"),
       #legend = c("Ecuador = mean(datecu$Edad)", mean(datqat$Edad)),
       #legend = rownames(dea[-c(1,2),]), 
#       bty = "n", 
#       pch=20, 
       #col=colors_border,
#       col=c("#ffff00", "#ff0000"),
#       text.col = "black", 
#       cex=1.3, 
#       pt.cex=4)
#legend(x=1.3, y=.9,
#       legend = c(mean(datecu$Edad), mean(datqat$Edad)),
#       bty = "n", 
#       text.col = "black", 
#       cex=1.3)


# *** DATASET: CALENDARIO Y RESULTADOS dqwc22 ***
# CREAR GANADOS, EMPATADOS Y PERDIDOS EN DATASET dqwc22
dqwc22 <- dqwc22 %>%
  mutate('G' = ifelse(GSeleccion > GRival, 1, 0)) %>%
  mutate('E' = ifelse(GSeleccion == GRival, 1, 0)) %>%
  mutate('P' = ifelse(GSeleccion < GRival, 1, 0))

dqwc22$Posesion <- as.numeric(dqwc22$Posesion)
dqwc22$`Precision Pases` <- as.numeric(dqwc22$`Precision Pases`)

# FASE DE GRUPOS 
# GRUPO A
grupoa <- data.frame("EQUIPO" =
                       c("Qatar", "Ecuador", "Senegal", "Paises Bajos"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Qatar', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Ecuador', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Senegal', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Paises Bajos', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$G),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$G),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$G),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$E),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$E),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$E),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$P),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$P),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$P),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Qatar')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Ecuador')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Senegal')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Paises Bajos')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Qatar')$G*3) + sum(filter(dqwc22, Seleccion == 'Qatar')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Ecuador')$G*3) + sum(filter(dqwc22, Seleccion == 'Ecuador')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Senegal')$G*3) + sum(filter(dqwc22, Seleccion == 'Senegal')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Paises Bajos')$G*3) + sum(filter(dqwc22, Seleccion == 'Paises Bajos')$E*1))
                     )
grupoa <- grupoa[order(-grupoa$PTOS, -grupoa$GD, -grupoa$GF, grupoa$EQUIPO), ] 

# GRUPO B
grupob <- data.frame("EQUIPO" =
                       c("Inglaterra", "Iran", "Estados Unidos", "Gales"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Inglaterra', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Iran', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Estados Unidos', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Gales', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$G),
                         sum(filter(dqwc22, Seleccion == 'Iran')$G),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$G),
                         sum(filter(dqwc22, Seleccion == 'Gales')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$E),
                         sum(filter(dqwc22, Seleccion == 'Iran')$E),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$E),
                         sum(filter(dqwc22, Seleccion == 'Gales')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$P),
                         sum(filter(dqwc22, Seleccion == 'Iran')$P),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$P),
                         sum(filter(dqwc22, Seleccion == 'Gales')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Iran')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Gales')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Iran')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Gales')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Inglaterra')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Iran')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Iran')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Estados Unidos')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Gales')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Gales')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Inglaterra')$G*3) + sum(filter(dqwc22, Seleccion == 'Inglaterra')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Iran')$G*3) + sum(filter(dqwc22, Seleccion == 'Iran')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Estados Unidos')$G*3) + sum(filter(dqwc22, Seleccion == 'Estados Unidos')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Gales')$G*3) + sum(filter(dqwc22, Seleccion == 'Gales')$E*1))
)
grupob <- grupob[order(-grupob$PTOS, -grupob$GD, -grupob$GF, grupob$EQUIPO), ]

# GRUPO C
grupoc <- data.frame("EQUIPO" =
                       c("Argentina", "Arabia Saudita", "Mexico", "Polonia"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Argentina', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Arabia Saudita', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Mexico', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Polonia', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$G),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$G),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$G),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$E),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$E),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$E),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$P),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$P),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$P),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Argentina')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Mexico')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Polonia')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Argentina')$G*3) + sum(filter(dqwc22, Seleccion == 'Argentina')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$G*3) + sum(filter(dqwc22, Seleccion == 'Arabia Saudita')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Mexico')$G*3) + sum(filter(dqwc22, Seleccion == 'Mexico')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Polonia')$G*3) + sum(filter(dqwc22, Seleccion == 'Polonia')$E*1))
)
grupoc <- grupoc[order(-grupoc$PTOS, -grupoc$GD, -grupoc$GF, grupoc$EQUIPO), ]

# GRUPO D
grupod <- data.frame("EQUIPO" =
                       c("Francia", "Australia", "Dinamarca", "Tunez"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Francia', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Australia', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Dinamarca', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Tunez', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$G),
                         sum(filter(dqwc22, Seleccion == 'Australia')$G),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$G),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$E),
                         sum(filter(dqwc22, Seleccion == 'Australia')$E),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$E),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$P),
                         sum(filter(dqwc22, Seleccion == 'Australia')$P),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$P),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Australia')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Australia')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Francia')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Australia')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Australia')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Dinamarca')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Tunez')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Francia')$G*3) + sum(filter(dqwc22, Seleccion == 'Francia')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Australia')$G*3) + sum(filter(dqwc22, Seleccion == 'Australia')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Dinamarca')$G*3) + sum(filter(dqwc22, Seleccion == 'Dinamarca')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Tunez')$G*3) + sum(filter(dqwc22, Seleccion == 'Tunez')$E*1))
)
grupod <- grupod[order(-grupod$PTOS, -grupod$GD, -grupod$GF, grupod$EQUIPO), ]

# GRUPO E
grupoe <- data.frame("EQUIPO" =
                       c("Espana", "Costa Rica", "Alemania", "Japon"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Espana', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Costa Rica', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Alemania', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Japon', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$G),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$G),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$G),
                         sum(filter(dqwc22, Seleccion == 'Japon')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$E),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$E),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$E),
                         sum(filter(dqwc22, Seleccion == 'Japon')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$P),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$P),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$P),
                         sum(filter(dqwc22, Seleccion == 'Japon')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Japon')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Japon')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Espana')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Costa Rica')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Alemania')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Japon')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Japon')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Espana')$G*3) + sum(filter(dqwc22, Seleccion == 'Espana')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Costa Rica')$G*3) + sum(filter(dqwc22, Seleccion == 'Costa Rica')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Alemania')$G*3) + sum(filter(dqwc22, Seleccion == 'Alemania')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Japon')$G*3) + sum(filter(dqwc22, Seleccion == 'Japon')$E*1))
)
grupoe <- grupoe[order(-grupoe$PTOS, -grupoe$GD, -grupoe$GF, grupoe$EQUIPO), ]

# GRUPO F
grupof <- data.frame("EQUIPO" =
                       c("Belgica", "Canada", "Marruecos", "Croacia"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Belgica', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Canada', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Marruecos', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Croacia', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$G),
                         sum(filter(dqwc22, Seleccion == 'Canada')$G),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$G),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$E),
                         sum(filter(dqwc22, Seleccion == 'Canada')$E),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$E),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$P),
                         sum(filter(dqwc22, Seleccion == 'Canada')$P),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$P),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Canada')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Canada')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Belgica')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Canada')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Canada')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Marruecos')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Croacia')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Belgica')$G*3) + sum(filter(dqwc22, Seleccion == 'Belgica')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Canada')$G*3) + sum(filter(dqwc22, Seleccion == 'Canada')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Marruecos')$G*3) + sum(filter(dqwc22, Seleccion == 'Marruecos')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Croacia')$G*3) + sum(filter(dqwc22, Seleccion == 'Croacia')$E*1))
)
grupof <- grupof[order(-grupof$PTOS, -grupof$GD, -grupof$GF, grupof$EQUIPO), ]

# GRUPO G
grupog <- data.frame("EQUIPO" =
                       c("Brasil", "Serbia", "Suiza", "Camerun"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Brasil', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Serbia', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Suiza', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Camerun', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$G),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$G),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$G),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$E),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$E),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$E),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$P),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$P),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$P),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Brasil')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Serbia')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Suiza')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Camerun')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Brasil')$G*3) + sum(filter(dqwc22, Seleccion == 'Brasil')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Serbia')$G*3) + sum(filter(dqwc22, Seleccion == 'Serbia')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Suiza')$G*3) + sum(filter(dqwc22, Seleccion == 'Suiza')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Camerun')$G*3) + sum(filter(dqwc22, Seleccion == 'Camerun')$E*1))
)
grupog <- grupog[order(-grupog$PTOS, -grupog$GD, -grupog$GF, grupog$EQUIPO), ]

# GRUPO H
grupoh <- data.frame("EQUIPO" =
                       c("Portugal", "Ghana", "Uruguay", "Corea del Sur"),
                     "PJ" =
                       c(sum(dqwc22$Seleccion == 'Portugal', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Ghana', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Uruguay', na.rm = TRUE),
                         sum(dqwc22$Seleccion == 'Corea del Sur', na.rm = TRUE)),
                     'G' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$G),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$G),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$G),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$G)),
                     'E' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$E),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$E),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$E),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$E)),
                     'P' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$P),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$P),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$P),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$P)),
                     'GF' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$GSeleccion),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$GSeleccion)),
                     'GC' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$GRival)),
                     'GD' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Portugal')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Ghana')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Uruguay')$GRival),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$GSeleccion) - sum(filter(dqwc22, Seleccion == 'Corea del Sur')$GRival)),
                     'PTOS' =
                       c(sum(filter(dqwc22, Seleccion == 'Portugal')$G*3) + sum(filter(dqwc22, Seleccion == 'Portugal')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Ghana')$G*3) + sum(filter(dqwc22, Seleccion == 'Ghana')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Uruguay')$G*3) + sum(filter(dqwc22, Seleccion == 'Uruguay')$E*1),
                         sum(filter(dqwc22, Seleccion == 'Corea del Sur')$G*3) + sum(filter(dqwc22, Seleccion == 'Corea del Sur')$E*1))
)
grupoh <- grupoh[order(-grupoh$PTOS, -grupoh$GD, -grupoh$GF, grupoh$EQUIPO), ]


# ECUADOR - SENEGAL
dgra <- filter(dqwc22, Seleccion %in% c('Ecuador', 'Senegal'))
dgec <- dqwc22 %>%
  filter(Seleccion == 'Ecuador') %>%
  group_by(Seleccion) %>%
  summarise(p_gol = mean(GSeleccion, na.rm = TRUE),
            p_remates = mean(Remates, na.rm = TRUE),
            p_rem_arco = mean(`Remates arco`, na.rm = TRUE),
            p_posesion = mean(Posesion, na.rm = TRUE),
            p_pases = mean(Pases, na.rm = TRUE),
            p_pre_pases = mean(`Precision Pases`, na.rm = TRUE),
            p_faltas = mean(Faltas, na.rm = TRUE),
            p_tar_ama = mean(`Tarjetas Amarillas`, na.rm = TRUE),
            p_tar_roj = mean(`Tarjetas Rojas`, na.rm = TRUE),
            p_pos_ade = mean(`Posicion Adelantada`, na.rm = TRUE),
            p_tir_esq = mean(`Tiros Esquina`, na.rm = TRUE))
dgse <- dqwc22 %>%
  filter(Seleccion == 'Senegal') %>%
  group_by(Seleccion) %>%
  summarise(p_gol = mean(GSeleccion, na.rm = TRUE),
            p_remates = mean(Remates, na.rm = TRUE),
            p_rem_arco = mean(`Remates arco`, na.rm = TRUE),
            p_posesion = mean(Posesion, na.rm = TRUE),
            p_pases = mean(Pases, na.rm = TRUE),
            p_pre_pases = mean(`Precision Pases`, na.rm = TRUE),
            p_faltas = mean(Faltas, na.rm = TRUE),
            p_tar_ama = mean(`Tarjetas Amarillas`, na.rm = TRUE),
            p_tar_roj = mean(`Tarjetas Rojas`, na.rm = TRUE),
            p_pos_ade = mean(`Posicion Adelantada`, na.rm = TRUE),
            p_tir_esq = mean(`Tiros Esquina`, na.rm = TRUE))
dgto <- full_join(dgec, dgse)
write_csv(dgto, file = 'data/qatar/ecuadorsenegal.csv')

#dev.off()



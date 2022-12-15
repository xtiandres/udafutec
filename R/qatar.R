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
library(magrittr)
library(ggpol)

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

# PLANTILLA EQUIPO 1
e1 <- filter(datqwc,
             Seleccion == 'France')
# PLANTILLA EQUIPO 2
e2 <- filter(datqwc,
             Seleccion == 'Morocco')
# TABLA COMPARATIVA EQUIPO 1 vs EQUIPO 2
e1e2 <- data.frame(
  Seleccion = c("Francia", "Marruecos"),
  PromedioEdad = c(round(mean(e1$Edad),digits=2), round(mean(e2$Edad),2)),
  ValorSeleccion = c(sum(e1$Valoracion), sum(e2$Valoracion)),
  Ranking = c(4, 22)
)
write_csv(e1e2, file = 'data/qatar/e1e2.csv')


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
# CREAR GANADOS, EMPATADOS, PERDIDOS, GANADOSPENAL EN DATASET dqwc22
dqwc22 <- dqwc22 %>%
  mutate('G' = ifelse(GSeleccion > GRival, 1, 0)) %>%
  mutate('E' = ifelse(GSeleccion == GRival, 1, 0)) %>%
  mutate('P' = ifelse(GSeleccion < GRival, 1, 0)) %>%
  mutate('GP' = ifelse(PSeleccion > PRival, 1, 0)) 
# SETUP FORMATO NUMERICO Posesion y Precision Pases dqwc22
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
rownames(grupoa) <- 1:nrow(grupoa)

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

# *** OCTAVOS DE FINAL dqwc22 ***
# CREAR CLASIFICA
dqwc22of <- dqwc22 %>%
  filter(Etapa == 'Octavos de Final') %>%
  mutate('Clasifica' = ifelse(G == 1 | GP == 1, 1, 0))
# *** CUARTOS DE FINAL dqwc22 ***
dqwc22cf <- dqwc22 %>%
  filter(Etapa == 'Cuartos de Final') %>%
  mutate('Clasifica' = ifelse(G == 1 | GP == 1, 1, 0))
# *** SEMIFINAL dqwc22 ***
dqwc22sf <- dqwc22 %>%
  filter(Etapa == 'Semifinal') %>%
  mutate('Clasifica' = ifelse(G == 1 | GP == 1, 1, 0))


# *** PARTIDOS PARTICULARES dqwc22 ***
# ECUADOR - SENEGAL
dgra <- filter(dqwc22, Seleccion %in% c('Ecuador', 'Senegal'))
# ECUADOR
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
# SENEGAL
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

# GRUPO A - GRUPO B
grupoa <- mutate(grupoa, GRUPO = 'A')
grupob <- mutate(grupob, GRUPO = 'B')
xyz <- rbind(grupoa, grupob)
long <- gather(xyz,
               key = 'variable',
               value = 'value',
               PJ:PTOS)



#ggplot(long,
#       aes(x = EQUIPO,
#           y = value,
#           fill = variable)) +
#  geom_bar(stat = 'identity', position = position_dodge()) +
#  aes(x = fct_inorder(EQUIPO)) +
#  scale_fill_brewer(palette = 'Dark2') +
#  theme_minimal() +
#  scale_y_continuous(breaks = seq(-6, 7, 1), 
#                     limits=c(-6, 7)) #+
  #coord_flip()

# LOLLIPOP CHART - GRUPO A OK1DIC
#ggplot(xyz,
#       aes(x = EQUIPO,
#           y = PTOS,
#           label = PTOS)) +
#  geom_point(stat='identity', fill="black", size=6)  +
#  geom_segment(aes(y = 0, 
#                   x = EQUIPO, 
#                   yend = PTOS, 
#                   xend = EQUIPO), 
#               color = "black") +
#  aes(x = fct_inorder(EQUIPO)) +
#  theme_minimal() +
#  geom_text(color='white', size=3) #+
  #coord_flip()

# PYRAMID
ggplot(long, aes(y = value, x = variable, fill = GRUPO)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  labs(title="Email Campaign Funnel") +
  scale_fill_brewer(palette = "Dark2")  # Color palette

#ggplot(long, aes(x = variable, y = value, fill = GRUPO)) +
#  geom_bar(data = subset(long, EQUIPO == 'Ecuador'), stat = "identity") + 
#  geom_bar(data = subset(long, EQUIPO == 'Senegal'), stat = "identity") + 
  #scale_y_continuous(labels = function(z) paste0(abs(z), "%")) +          # CHANGE
#  ggtitle("Male vs Female Population Comparison") +
#  labs(x = "Age group", y = "Percentage", fill = 'EQUIPO') +
#  coord_flip()



#ggplot(xyz, aes(x = EQUIPO, y = PTOS, size = PTOS)) +
#  geom_point(alpha = .5, 
#             fill="cornflowerblue", 
#             color="black", 
#             shape=21) +
#  aes(x = fct_inorder(EQUIPO)) +
#  scale_y_continuous(breaks = seq(0, 7, 1), 
#                     limits=c(0, 7))

#ggplot(xyz, aes(x = EQUIPO, y = PTOS, fill = PTOS)) +
#  geom_bar(position = position_dodge(),
#           stat = 'identity',
#           width = .5) +
#  aes(x = fct_inorder(EQUIPO)) +
#  scale_y_reverse() +
#  scale_y_continuous(breaks = seq(0, 7, 1), 
#                     limits=c(0, 7))

#g2 <- ggplot(grupob, 
#             aes(x = EQUIPO, y = PTOS)) +
#  geom_bar(stat = 'identity',
#           width = .5) +
#  aes(x = fct_inorder(EQUIPO)) +
#  scale_y_continuous(breaks = seq(0, 7, 1), 
#                     limits=c(0, 7))


#dev.off()



#Campeonato Ecuatoriano 2022 - Total, 1ra Etapa y 2da Etapa

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

#Primera Etapa
dat223 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ec223.txt")
#data1

#Segunda Etapa
#data2 <- read.csv("/home/xut/Documents/udaviz/R/studio/camerica/data/ec20212.csv")
#data2

#Primera y Segunda Etapa
#datat <- full_join(data1, data2)
#datat


# 1ra Etapa - Rendimiento Local, Visita
dat223_x <- dat223 %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0)) %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0)) %>%
  mutate(nL = percent(VL * 1 + EL * 0.25)) %>%
  mutate(nV = percent(VV * 1 + EV * 0.75))
#mutate(nL = VL * 1 + EL * 0.25) %>%


#Datos BSC
datbsc <- filter(dat223,
                 Local == "Barcelona SC" | Visita == "Barcelona SC")
datbsc_l <- datbsc %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbsc_v <- datbsc %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Emelec
datcse <- filter(dat223,
                 Local == "Emelec" | Visita == "Emelec")
datcse_l <- datcse %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcse_v <- datcse %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Macará
datmac <- filter(dat223,
                 Local == "Macará" | Visita == "Macará")
datmac_l <- datmac %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datmac_v <- datmac %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Mushuc Runa
datmus <- filter(dat223,
                 Local == "Mushuc Runa" | Visita == "Mushuc Runa")
datmus_l <- datmus %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datmus_v <- datmus %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Universidad Católica
datuca <- filter(dat223,
                 Local == "Universidad Católica" | Visita == "Universidad Católica")
datuca_l <- datuca %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datuca_v <- datuca %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos LDU Quito
datldu <- filter(dat223,
                 Local == "LDU Quito" | Visita == "LDU Quito")
datldu_l <- datldu %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datldu_v <- datldu %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Gualaceo SC
datgua <- filter(dat223,
                 Local == "Gualaceo SC" | Visita == "Gualaceo SC")
datgua_l <- datgua %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datgua_v <- datgua %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Delfín SC
datdel <- filter(dat223,
                 Local == "Delfín SC" | Visita == "Delfín SC")
datdel_l <- datdel %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datdel_v <- datdel %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Orense SC
datore <- filter(dat223,
                 Local == "Orense SC" | Visita == "Orense SC")
datore_l <- datore %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datore_v <- datore %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Aucas
datauc <- filter(dat223,
                 Local == "Aucas" | Visita == "Aucas")
datauc_l <- datauc %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datauc_v <- datauc %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Guayaquil City
datgye <- filter(dat223,
                 Local == "Guayaquil City" | Visita == "Guayaquil City")
datgye_l <- datgye %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datgye_v <- datgye %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Deportivo Cuenca
datcue <- filter(dat223,
                 Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca")
datcue_l <- datcue %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcue_v <- datcue %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Cumbayá FC
datcum <- filter(dat223,
                 Local == "Cumbayá FC" | Visita == "Cumbayá FC")
datcum_l <- datcum %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcum_v <- datcum %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Independiente del Valle
datidv <- filter(dat223,
                 Local == "Independiente del Valle" | Visita == "Independiente del Valle")
datidv_l <- datidv %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datidv_v <- datidv %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Técnico Univ.
dattec <- filter(dat223,
                 Local == "Técnico Univ." | Visita == "Técnico Univ.")
dattec_l <- dattec %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dattec_v <- dattec %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

#Datos Nueve de Octubre
datnue <- filter(dat223,
                 Local == "Nueve de Octubre" | Visita == "Nueve de Octubre")
datnue_l <- datnue %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datnue_v <- datnue %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))


#Tabla Acumulada 1raEtapa PJ-Ptos-G-E-D-GF-GC
taba3 = data.frame("EQUIPOS" =
                     c("Barcelona SC",
                       "LDU Quito",
                       "Emelec",
                       "Independiente del Valle",
                       "Aucas",
                       "Macará",
                       "Técnico Univ.",
                       "Guayaquil City",
                       "Mushuc Runa",
                       "Nueve de Octubre",
                       "Deportivo Cuenca",
                       "Universidad Católica",
                       "Gualaceo SC",
                       "Orense SC",
                       "Delfín SC",
                       "Cumbayá FC"),
                   "PJ" =
                     c(NROW(datbsc$Jornada),
                       NROW(datldu$Jornada),
                       NROW(datcse$Jornada),
                       NROW(datidv$Jornada),
                       NROW(datauc$Jornada),
                       NROW(datmac$Jornada),
                       NROW(dattec$Jornada),
                       NROW(datgye$Jornada),
                       NROW(datmus$Jornada),
                       NROW(datnue$Jornada),
                       NROW(datcue$Jornada),
                       NROW(datuca$Jornada),
                       NROW(datgua$Jornada),
                       NROW(datore$Jornada),
                       NROW(datdel$Jornada),
                       NROW(datcum$Jornada)),
                   "PTOS"=
                     c(sum(datbsc_l$VL*3,
                           datbsc_v$VV*3,
                           datbsc_l$EL*1,
                           datbsc_v$EV*1),
                       sum(datldu_l$VL*3,
                           datldu_v$VV*3,
                           datldu_l$EL*1,
                           datldu_v$EV*1),
                       sum(datcse_l$VL*3,
                           datcse_v$VV*3,
                           datcse_l$EL*1,
                           datcse_v$EV*1),
                       sum(datidv_l$VL*3,
                           datidv_v$VV*3,
                           datidv_l$EL*1,
                           datidv_v$EV*1),
                       sum(datauc_l$VL*3,
                           datauc_v$VV*3,
                           datauc_l$EL*1,
                           datauc_v$EV*1),
                       sum(datmac_l$VL*3,
                           datmac_v$VV*3,
                           datmac_l$EL*1,
                           datmac_v$EV*1),
                       sum(dattec_l$VL*3,
                           dattec_v$VV*3,
                           dattec_l$EL*1,
                           dattec_v$EV*1),
                       sum(datgye_l$VL*3,
                           datgye_v$VV*3,
                           datgye_l$EL*1,
                           datgye_v$EV*1),
                       sum(datmus_l$VL*3,
                           datmus_v$VV*3,
                           datmus_l$EL*1,
                           datmus_v$EV*1),
                       sum(datnue_l$VL*3,
                           datnue_v$VV*3,
                           datnue_l$EL*1,
                           datnue_v$EV*1),
                       sum(datcue_l$VL*3,
                           datcue_v$VV*3,
                           datcue_l$EL*1,
                           datcue_v$EV*1),
                       sum(datuca_l$VL*3,
                           datuca_v$VV*3,
                           datuca_l$EL*1,
                           datuca_v$EV*1),
                       sum(datgua_l$VL*3,
                           datgua_v$VV*3,
                           datgua_l$EL*1,
                           datgua_v$EV*1),
                       sum(datore_l$VL*3,
                           datore_v$VV*3,
                           datore_l$EL*1,
                           datore_v$EV*1),
                       sum(datdel_l$VL*3,
                           datdel_v$VV*3,
                           datdel_l$EL*1,
                           datdel_v$EV*1),
                       sum(datcum_l$VL*3,
                           datcum_v$VV*3,
                           datcum_l$EL*1,
                           datcum_v$EV*1)),
                   "GF"=
                     c(sum(datbsc_l$GL,
                           datbsc_v$GV),
                       sum(datldu_l$GL,
                           datldu_v$GV),
                       sum(datcse_l$GL,
                           datcse_v$GV),
                       sum(datidv_l$GL,
                           datidv_v$GV),
                       sum(datauc_l$GL,
                           datauc_v$GV),
                       sum(datmac_l$GL,
                           datmac_v$GV),
                       sum(dattec_l$GL,
                           dattec_v$GV),
                       sum(datgye_l$GL,
                           datgye_v$GV),
                       sum(datmus_l$GL,
                           datmus_v$GV),
                       sum(datnue_l$GL,
                           datnue_v$GV),
                       sum(datcue_l$GL,
                           datcue_v$GV),
                       sum(datuca_l$GL,
                           datuca_v$GV),
                       sum(datgua_l$GL,
                           datgua_v$GV),
                       sum(datore_l$GL,
                           datore_v$GV),
                       sum(datdel_l$GL,
                           datdel_v$GV),
                       sum(datcum_l$GL,
                           datcum_v$GV)),
                   "GC"=
                     c(sum(datbsc_l$GV,
                           datbsc_v$GL),
                       sum(datldu_l$GV,
                           datldu_v$GL),
                       sum(datcse_l$GV,
                           datcse_v$GL),
                       sum(datidv_l$GV,
                           datidv_v$GL),
                       sum(datauc_l$GV,
                           datauc_v$GL),
                       sum(datmac_l$GV,
                           datmac_v$GL),
                       sum(dattec_l$GV,
                           dattec_v$GL),
                       sum(datgye_l$GV,
                           datgye_v$GL),
                       sum(datmus_l$GV,
                           datmus_v$GL),
                       sum(datnue_l$GV,
                           datnue_v$GL),
                       sum(datcue_l$GV,
                           datcue_v$GL),
                       sum(datuca_l$GV,
                           datuca_v$GL),
                       sum(datgua_l$GV,
                           datgua_v$GL),
                       sum(datore_l$GV,
                           datore_v$GL),
                       sum(datdel_l$GV,
                           datdel_v$GL),
                       sum(datcum_l$GV,
                           datcum_v$GL))
)
# Tabla Acumulada 1raEtapa EQUIPOS, PJ, PTOS, GF, GC, GD
taba3 <- mutate(taba3, GD = GF - GC)
# Tabla Acumulada 1raEtapa ORDENAR
taba3 <- taba3[order(-taba3$PTOS, -taba3$GD, -taba3$GF), ]
taba3 <- select(taba3, EQUIPOS, PTOS, GD)
# Tabla Acumulada 1raEtapa Filas ordenadas
rownames(taba3) <- 1:nrow(taba3)
taba3 <- mutate(taba3, J3 = rownames(taba3))
#taba3$J2 <- as.numeric(taba3$J2)
#a2 <- ggplot(taba3, aes(x = EQUIPOS, y = J2, color = EQUIPOS)) 
#a2 +
#  geom_point(size = 2) +
#aes(y = fct_inorder(row.names())) +
#  theme_minimal()
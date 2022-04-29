# CAMPEONATO ECUATORIANO FUTBOL 2021
# TOTAL TABLA ACUMULADA, 1ra Etapa y 2da Etapa

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
data1 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ec20211.csv")
#data1

#Segunda Etapa
data2 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ec20212.csv")
#data2

#Primera y Segunda Etapa
datat <- full_join(data1, data2)
#datat


#Tablas completa y tablas 1ra y 2da etapa

#Datos completos BSC
databsc <- filter(datat,
                  Local == "BarcelonaSC" | Visita == "BarcelonaSC")

databsctest <- databsc %>%
  filter(Local == "BarcelonaSC" | Visita == "BarcelonaSC") %>%
  mutate(Localía = ifelse(Local == "BarcelonaSC", "Si", "No")) %>%
  mutate(VLocal = ifelse(GolLocal > GolVisita, 1,0)) %>%
  mutate(VVisita = ifelse(GolVisita > GolLocal, 1,0)) %>%
  mutate(ELocal = ifelse(Localía == "Si" & GolLocal == GolVisita, 1,0)) %>%
  mutate(EVisita = ifelse(Localía == "No" & GolVisita == GolLocal, 1,0)) %>%
  mutate(nLocal = VLocal * 100 + ELocal * 0.25) %>%
  mutate(nVisita = VVisita * 100 + EVisita * 0.75)
  

#Datos Etapa 1 BSC
databsc1 <- filter(databsc,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
#Datos Etapa 2 BSC
databsc2 <- filter(databsc,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datauca <- filter(datat,
                  Local == "UniversidadCatolica" | Visita == "UniversidadCatolica")
datauca1 <- filter(datauca,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datauca2 <- filter(datauca,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataolm <- filter(datat,
                  Local == "Olmedo" | Visita == "Olmedo")
dataolm1 <- filter(dataolm,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataolm2 <- filter(dataolm,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataldu <- filter(datat,
                  Local == "LDUQuito" | Visita == "LDUQuito")
dataldu1 <- filter(dataldu,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataldu2 <- filter(dataldu,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datanue <- filter(datat,
                  Local == "NuevedeOctubre" | Visita == "NuevedeOctubre")
datanue1 <- filter(datanue,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datanue2 <- filter(datanue,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datamus <- filter(datat,
                  Local == "MushucRuna" | Visita == "MushucRuna")
datamus1 <- filter(datamus,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datamus2 <- filter(datamus,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataauc <- filter(datat,
                  Local == "Aucas" | Visita == "Aucas")
dataauc1 <- filter(dataauc,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataauc2 <- filter(dataauc,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datagua <- filter(datat,
                  Local == "GuayaquilCity" | Visita == "GuayaquilCity")
datagua1 <- filter(datagua,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datagua2 <- filter(datagua,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datamac <- filter(datat,
                  Local == "Macará" | Visita == "Macará")
datamac1 <- filter(datamac,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datamac2 <- filter(datamac,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataman <- filter(datat,
                  Local == "MantaFC" | Visita == "MantaFC")
dataman1 <- filter(dataman,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataman2 <- filter(dataman,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataore <- filter(datat,
                  Local == "OrenseSC" | Visita == "OrenseSC")
dataore1 <- filter(dataore,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataore2 <- filter(dataore,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataidv <- filter(datat,
                  Local == "IndependientedelValle" | Visita == "IndependientedelValle")
dataidv1 <- filter(dataidv,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataidv2 <- filter(dataidv,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datatec <- filter(datat,
                  Local == "TécnicoUniv" | Visita == "TécnicoUniv")
datatec1 <- filter(datatec,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datatec2 <- filter(datatec,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datadel <- filter(datat,
                  Local == "DelfínSC" | Visita == "DelfínSC")
datadel1 <- filter(datadel,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datadel2 <- filter(datadel,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
dataeme <- filter(datat,
                  Local == "Emelec" | Visita == "Emelec")
dataeme1 <- filter(dataeme,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
dataeme2 <- filter(dataeme,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))
datacue <- filter(datat,
                  Local == "DeportivoCuenca" | Visita == "DeportivoCuenca")
datacue1 <- filter(datacue,
                   Jornada %in% c("J1",
                                  "J2",
                                  "J3",
                                  "J4",
                                  "J5",
                                  "J6",
                                  "J7",
                                  "J8",
                                  "J9",
                                  "J10",
                                  "J11",
                                  "J12",
                                  "J13",
                                  "J14",
                                  "J15"))
datacue2 <- filter(datacue,
                   Jornada %in% c("J16",
                                  "J17",
                                  "J18",
                                  "J19",
                                  "J20",
                                  "J21",
                                  "J22",
                                  "J23",
                                  "J24",
                                  "J25",
                                  "J26",
                                  "J27",
                                  "J28",
                                  "J29",
                                  "J30"))


#Tablas Victorias(local,visita), Empates(local,visita) y Derrotas(local,visita)

#Tabla Total
bscvicl <- databsc %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
bscvicv <- databsc %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
bscempl <- databsc %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
bscempv <- databsc %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
bscderl <- databsc %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
bscderv <- databsc %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
bscvicl1 <- databsc1 %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
bscvicv1 <- databsc1 %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
bscempl1 <- databsc1 %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
bscempv1 <- databsc1 %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
bscderl1 <- databsc1 %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
bscderv1 <- databsc1 %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
bscvicl2 <- databsc2 %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
bscvicv2 <- databsc2 %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
bscempl2 <- databsc2 %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
bscempv2 <- databsc2 %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
bscderl2 <- databsc2 %>%
  filter(Local == "BarcelonaSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
bscderv2 <- databsc2 %>%
  filter(Visita == "BarcelonaSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
ucavicl <- datauca %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
ucavicv <- datauca %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
ucaempl <- datauca %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
ucaempv <- datauca %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
ucaderl <- datauca %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
ucaderv <- datauca %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
ucavicl1 <- datauca1 %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
ucavicv1 <- datauca1 %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
ucaempl1 <- datauca1 %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
ucaempv1 <- datauca1 %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
ucaderl1 <- datauca1 %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
ucaderv1 <- datauca1 %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
ucavicl2 <- datauca2 %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
ucavicv2 <- datauca2 %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
ucaempl2 <- datauca2 %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
ucaempv2 <- datauca2 %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
ucaderl2 <- datauca2 %>%
  filter(Local == "UniversidadCatolica") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
ucaderv2 <- datauca2 %>%
  filter(Visita == "UniversidadCatolica") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
olmvicl <- dataolm %>%
  filter(Local == "Olmedo") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
olmvicv <- dataolm %>%
  filter(Visita == "Olmedo") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
olmempl <- dataolm %>%
  filter(Local == "Olmedo") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
olmempv <- dataolm %>%
  filter(Visita == "Olmedo") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
olmderl <- dataolm %>%
  filter(Local == "Olmedo") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
olmderv <- dataolm %>%
  filter(Visita == "Olmedo") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
olmvicl1 <- dataolm1 %>%
  filter(Local == "Olmedo") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
olmvicv1 <- dataolm1 %>%
  filter(Visita == "Olmedo") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
olmempl1 <- dataolm1 %>%
  filter(Local == "Olmedo") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
olmempv1 <- dataolm1 %>%
  filter(Visita == "Olmedo") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
olmderl1 <- dataolm1 %>%
  filter(Local == "Olmedo") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
olmderv1 <- dataolm1 %>%
  filter(Visita == "Olmedo") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
olmvicl2 <- dataolm2 %>%
  filter(Local == "Olmedo") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
olmvicv2 <- dataolm2 %>%
  filter(Visita == "Olmedo") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
olmempl2 <- dataolm2 %>%
  filter(Local == "Olmedo") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
olmempv2 <- dataolm2 %>%
  filter(Visita == "Olmedo") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
olmderl2 <- dataolm2 %>%
  filter(Local == "Olmedo") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
olmderv2 <- dataolm2 %>%
  filter(Visita == "Olmedo") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
lduvicl <- dataldu %>%
  filter(Local == "LDUQuito") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
lduvicv <- dataldu %>%
  filter(Visita == "LDUQuito") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
lduempl <- dataldu %>%
  filter(Local == "LDUQuito") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
lduempv <- dataldu %>%
  filter(Visita == "LDUQuito") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
lduderl <- dataldu %>%
  filter(Local == "LDUQuito") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
lduderv <- dataldu %>%
  filter(Visita == "LDUQuito") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
lduvicl1 <- dataldu1 %>%
  filter(Local == "LDUQuito") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
lduvicv1 <- dataldu1 %>%
  filter(Visita == "LDUQuito") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
lduempl1 <- dataldu1 %>%
  filter(Local == "LDUQuito") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
lduempv1 <- dataldu1 %>%
  filter(Visita == "LDUQuito") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
lduderl1 <- dataldu1 %>%
  filter(Local == "LDUQuito") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
lduderv1 <- dataldu1 %>%
  filter(Visita == "LDUQuito") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
lduvicl2 <- dataldu2 %>%
  filter(Local == "LDUQuito") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
lduvicv2 <- dataldu2 %>%
  filter(Visita == "LDUQuito") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
lduempl2 <- dataldu2 %>%
  filter(Local == "LDUQuito") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
lduempv2 <- dataldu2 %>%
  filter(Visita == "LDUQuito") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
lduderl2 <- dataldu2 %>%
  filter(Local == "LDUQuito") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
lduderv2 <- dataldu2 %>%
  filter(Visita == "LDUQuito") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
nuevicl <- datanue %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
nuevicv <- datanue %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
nueempl <- datanue %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
nueempv <- datanue %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
nuederl <- datanue %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
nuederv <- datanue %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
nuevicl1 <- datanue1 %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
nuevicv1 <- datanue1 %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
nueempl1 <- datanue1 %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
nueempv1 <- datanue1 %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
nuederl1 <- datanue1 %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
nuederv1 <- datanue1 %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
nuevicl2 <- datanue2 %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
nuevicv2 <- datanue2 %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
nueempl2 <- datanue2 %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
nueempv2 <- datanue2 %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
nuederl2 <- datanue2 %>%
  filter(Local == "NuevedeOctubre") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
nuederv2 <- datanue2 %>%
  filter(Visita == "NuevedeOctubre") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
musvicl <- datamus %>%
  filter(Local == "MushucRuna") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
musvicv <- datamus %>%
  filter(Visita == "MushucRuna") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
musempl <- datamus %>%
  filter(Local == "MushucRuna") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
musempv <- datamus %>%
  filter(Visita == "MushucRuna") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
musderl <- datamus %>%
  filter(Local == "MushucRuna") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
musderv <- datamus %>%
  filter(Visita == "MushucRuna") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
musvicl1 <- datamus1 %>%
  filter(Local == "MushucRuna") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
musvicv1 <- datamus1 %>%
  filter(Visita == "MushucRuna") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
musempl1 <- datamus1 %>%
  filter(Local == "MushucRuna") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
musempv1 <- datamus1 %>%
  filter(Visita == "MushucRuna") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
musderl1 <- datamus1 %>%
  filter(Local == "MushucRuna") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
musderv1 <- datamus1 %>%
  filter(Visita == "MushucRuna") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
musvicl2 <- datamus2 %>%
  filter(Local == "MushucRuna") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
musvicv2 <- datamus2 %>%
  filter(Visita == "MushucRuna") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
musempl2 <- datamus2 %>%
  filter(Local == "MushucRuna") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
musempv2 <- datamus2 %>%
  filter(Visita == "MushucRuna") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
musderl2 <- datamus2 %>%
  filter(Local == "MushucRuna") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
musderv2 <- datamus2 %>%
  filter(Visita == "MushucRuna") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
aucvicl <- dataauc %>%
  filter(Local == "Aucas") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
aucvicv <- dataauc %>%
  filter(Visita == "Aucas") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
aucempl <- dataauc %>%
  filter(Local == "Aucas") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
aucempv <- dataauc %>%
  filter(Visita == "Aucas") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
aucderl <- dataauc %>%
  filter(Local == "Aucas") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
aucderv <- dataauc %>%
  filter(Visita == "Aucas") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
aucvicl1 <- dataauc1 %>%
  filter(Local == "Aucas") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
aucvicv1 <- dataauc1 %>%
  filter(Visita == "Aucas") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
aucempl1 <- dataauc1 %>%
  filter(Local == "Aucas") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
aucempv1 <- dataauc1 %>%
  filter(Visita == "Aucas") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
aucderl1 <- dataauc1 %>%
  filter(Local == "Aucas") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
aucderv1 <- dataauc1 %>%
  filter(Visita == "Aucas") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
aucvicl2 <- dataauc2 %>%
  filter(Local == "Aucas") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
aucvicv2 <- dataauc2 %>%
  filter(Visita == "Aucas") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
aucempl2 <- dataauc2 %>%
  filter(Local == "Aucas") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
aucempv2 <- dataauc2 %>%
  filter(Visita == "Aucas") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
aucderl2 <- dataauc2 %>%
  filter(Local == "Aucas") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
aucderv2 <- dataauc2 %>%
  filter(Visita == "Aucas") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
guavicl <- datagua %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
guavicv <- datagua %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
guaempl <- datagua %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
guaempv <- datagua %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
guaderl <- datagua %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
guaderv <- datagua %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
guavicl1 <- datagua1 %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
guavicv1 <- datagua1 %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
guaempl1 <- datagua1 %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
guaempv1 <- datagua1 %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
guaderl1 <- datagua1 %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
guaderv1 <- datagua1 %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
guavicl2 <- datagua2 %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
guavicv2 <- datagua2 %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
guaempl2 <- datagua2 %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
guaempv2 <- datagua2 %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
guaderl2 <- datagua2 %>%
  filter(Local == "GuayaquilCity") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
guaderv2 <- datagua2 %>%
  filter(Visita == "GuayaquilCity") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
macvicl <- datamac %>%
  filter(Local == "Macará") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
macvicv <- datamac %>%
  filter(Visita == "Macará") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
macempl <- datamac %>%
  filter(Local == "Macará") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
macempv <- datamac %>%
  filter(Visita == "Macará") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
macderl <- datamac %>%
  filter(Local == "Macará") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
macderv <- datamac %>%
  filter(Visita == "Macará") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
macvicl1 <- datamac1 %>%
  filter(Local == "Macará") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
macvicv1 <- datamac1 %>%
  filter(Visita == "Macará") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
macempl1 <- datamac1 %>%
  filter(Local == "Macará") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
macempv1 <- datamac1 %>%
  filter(Visita == "Macará") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
macderl1 <- datamac1 %>%
  filter(Local == "Macará") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
macderv1 <- datamac1 %>%
  filter(Visita == "Macará") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
macvicl2 <- datamac2 %>%
  filter(Local == "Macará") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
macvicv2 <- datamac2 %>%
  filter(Visita == "Macará") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
macempl2 <- datamac2 %>%
  filter(Local == "Macará") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
macempv2 <- datamac2 %>%
  filter(Visita == "Macará") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
macderl2 <- datamac2 %>%
  filter(Local == "Macará") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
macderv2 <- datamac2 %>%
  filter(Visita == "Macará") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
manvicl <- dataman %>%
  filter(Local == "MantaFC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
manvicv <- dataman %>%
  filter(Visita == "MantaFC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
manempl <- dataman %>%
  filter(Local == "MantaFC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
manempv <- dataman %>%
  filter(Visita == "MantaFC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
manderl <- dataman %>%
  filter(Local == "MantaFC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
manderv <- dataman %>%
  filter(Visita == "MantaFC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
manvicl1 <- dataman1 %>%
  filter(Local == "MantaFC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
manvicv1 <- dataman1 %>%
  filter(Visita == "MantaFC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
manempl1 <- dataman1 %>%
  filter(Local == "MantaFC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
manempv1 <- dataman1 %>%
  filter(Visita == "MantaFC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
manderl1 <- dataman1 %>%
  filter(Local == "MantaFC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
manderv1 <- dataman1 %>%
  filter(Visita == "MantaFC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
manvicl2 <- dataman2 %>%
  filter(Local == "MantaFC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
manvicv2 <- dataman2 %>%
  filter(Visita == "MantaFC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
manempl2 <- dataman2 %>%
  filter(Local == "MantaFC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
manempv2 <- dataman2 %>%
  filter(Visita == "MantaFC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
manderl2 <- dataman2 %>%
  filter(Local == "MantaFC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
manderv2 <- dataman2 %>%
  filter(Visita == "MantaFC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
orevicl <- dataore %>%
  filter(Local == "OrenseSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
orevicv <- dataore %>%
  filter(Visita == "OrenseSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
oreempl <- dataore %>%
  filter(Local == "OrenseSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
oreempv <- dataore %>%
  filter(Visita == "OrenseSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
orederl <- dataore %>%
  filter(Local == "OrenseSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
orederv <- dataore %>%
  filter(Visita == "OrenseSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
orevicl1 <- dataore1 %>%
  filter(Local == "OrenseSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
orevicv1 <- dataore1 %>%
  filter(Visita == "OrenseSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
oreempl1 <- dataore1 %>%
  filter(Local == "OrenseSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
oreempv1 <- dataore1 %>%
  filter(Visita == "OrenseSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
orederl1 <- dataore1 %>%
  filter(Local == "OrenseSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
orederv1 <- dataore1 %>%
  filter(Visita == "OrenseSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
orevicl2 <- dataore2 %>%
  filter(Local == "OrenseSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
orevicv2 <- dataore2 %>%
  filter(Visita == "OrenseSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
oreempl2 <- dataore2 %>%
  filter(Local == "OrenseSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
oreempv2 <- dataore2 %>%
  filter(Visita == "OrenseSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
orederl2 <- dataore2 %>%
  filter(Local == "OrenseSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
orederv2 <- dataore2 %>%
  filter(Visita == "OrenseSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
idvvicl <- dataidv %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
idvvicv <- dataidv %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
idvempl <- dataidv %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
idvempv <- dataidv %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
idvderl <- dataidv %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
idvderv <- dataidv %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
idvvicl1 <- dataidv1 %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
idvvicv1 <- dataidv1 %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
idvempl1 <- dataidv1 %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
idvempv1 <- dataidv1 %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
idvderl1 <- dataidv1 %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
idvderv1 <- dataidv1 %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
idvvicl2 <- dataidv2 %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
idvvicv2 <- dataidv2 %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
idvempl2 <- dataidv2 %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
idvempv2 <- dataidv2 %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
idvderl2 <- dataidv2 %>%
  filter(Local == "IndependientedelValle") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
idvderv2 <- dataidv2 %>%
  filter(Visita == "IndependientedelValle") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
tecvicl <- datatec %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
tecvicv <- datatec %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
tecempl <- datatec %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
tecempv <- datatec %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
tecderl <- datatec %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
tecderv <- datatec %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
tecvicl1 <- datatec1 %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
tecvicv1 <- datatec1 %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
tecempl1 <- datatec1 %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
tecempv1 <- datatec1 %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
tecderl1 <- datatec1 %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
tecderv1 <- datatec1 %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
tecvicl2 <- datatec2 %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
tecvicv2 <- datatec2 %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
tecempl2 <- datatec2 %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
tecempv2 <- datatec2 %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
tecderl2 <- datatec2 %>%
  filter(Local == "TécnicoUniv") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
tecderv2 <- datatec2 %>%
  filter(Visita == "TécnicoUniv") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
delvicl <- datadel %>%
  filter(Local == "DelfínSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
delvicv <- datadel %>%
  filter(Visita == "DelfínSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
delempl <- datadel %>%
  filter(Local == "DelfínSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
delempv <- datadel %>%
  filter(Visita == "DelfínSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
delderl <- datadel %>%
  filter(Local == "DelfínSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
delderv <- datadel %>%
  filter(Visita == "DelfínSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
delvicl1 <- datadel1 %>%
  filter(Local == "DelfínSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
delvicv1 <- datadel1 %>%
  filter(Visita == "DelfínSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
delempl1 <- datadel1 %>%
  filter(Local == "DelfínSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
delempv1 <- datadel1 %>%
  filter(Visita == "DelfínSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
delderl1 <- datadel1 %>%
  filter(Local == "DelfínSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
delderv1 <- datadel1 %>%
  filter(Visita == "DelfínSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
delvicl2 <- datadel2 %>%
  filter(Local == "DelfínSC") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
delvicv2 <- datadel2 %>%
  filter(Visita == "DelfínSC") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
delempl2 <- datadel2 %>%
  filter(Local == "DelfínSC") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
delempv2 <- datadel2 %>%
  filter(Visita == "DelfínSC") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
delderl2 <- datadel2 %>%
  filter(Local == "DelfínSC") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
delderv2 <- datadel2 %>%
  filter(Visita == "DelfínSC") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
emevicl <- dataeme %>%
  filter(Local == "Emelec") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
emevicv <- dataeme %>%
  filter(Visita == "Emelec") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
emeempl <- dataeme %>%
  filter(Local == "Emelec") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
emeempv <- dataeme %>%
  filter(Visita == "Emelec") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
emederl <- dataeme %>%
  filter(Local == "Emelec") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
emederv <- dataeme %>%
  filter(Visita == "Emelec") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
emevicl1 <- dataeme1 %>%
  filter(Local == "Emelec") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
emevicv1 <- dataeme1 %>%
  filter(Visita == "Emelec") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
emeempl1 <- dataeme1 %>%
  filter(Local == "Emelec") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
emeempv1 <- dataeme1 %>%
  filter(Visita == "Emelec") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
emederl1 <- dataeme1 %>%
  filter(Local == "Emelec") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
emederv1 <- dataeme1 %>%
  filter(Visita == "Emelec") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
emevicl2 <- dataeme2 %>%
  filter(Local == "Emelec") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
emevicv2 <- dataeme2 %>%
  filter(Visita == "Emelec") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
emeempl2 <- dataeme2 %>%
  filter(Local == "Emelec") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
emeempv2 <- dataeme2 %>%
  filter(Visita == "Emelec") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
emederl2 <- dataeme2 %>%
  filter(Local == "Emelec") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
emederv2 <- dataeme2 %>%
  filter(Visita == "Emelec") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)

#Tabla Total
cuevicl <- datacue %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
cuevicv <- datacue %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
cueempl <- datacue %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
cueempv <- datacue %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
cuederl <- datacue %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
cuederv <- datacue %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 1ra Etapa
cuevicl1 <- datacue1 %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
cuevicv1 <- datacue1 %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
cueempl1 <- datacue1 %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
cueempv1 <- datacue1 %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
cuederl1 <- datacue1 %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
cuederv1 <- datacue1 %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)
#Tabla 2da Etapa
cuevicl2 <- datacue2 %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(VicLocal = ifelse(GolLocal > GolVisita, 3,0)) %>%
  filter(VicLocal == 3)
cuevicv2 <- datacue2 %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(VicVisita = ifelse(GolVisita > GolLocal, 3,0)) %>%
  filter(VicVisita == 3)
cueempl2 <- datacue2 %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(EmpLocal = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpLocal == 1)
cueempv2 <- datacue2 %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(EmpVisita = ifelse(GolLocal == GolVisita, 1,0)) %>%
  filter(EmpVisita == 1)
cuederl2 <- datacue2 %>%
  filter(Local == "DeportivoCuenca") %>%
  mutate(DerLocal = ifelse(GolLocal < GolVisita, 0,3)) %>%
  filter(DerLocal == 0)
cuederv2 <- datacue2 %>%
  filter(Visita == "DeportivoCuenca") %>%
  mutate(DerVisita = ifelse(GolVisita < GolLocal, 0,3)) %>%
  filter(DerVisita == 0)


#Tabla Acumulada PJ-Ptos-G-E-D-GF-GC
tabla = data.frame("Equipos" =
                     c("Barcelona",
                       "Liga",
                       "Emelec",
                       "Independiente",
                       "Aucas",
                       "Macará",
                       "Técnico",
                       "GuayaquilCity",
                       "Mushuc",
                       "9Octubre",
                       "DeportivoCuenca",
                       "UCatólica",
                       "Manta",
                       "Orense",
                       "Delfín",
                       "Olmedo"),
                   "PartidosJugados" =
                     c(NROW(databsc$Jornada),
                       NROW(dataldu$Jornada),
                       NROW(dataeme$Jornada),
                       NROW(dataidv$Jornada),
                       NROW(dataauc$Jornada),
                       NROW(datamac$Jornada),
                       NROW(datatec$Jornada),
                       NROW(datagua$Jornada),
                       NROW(datamus$Jornada),
                       NROW(datanue$Jornada),
                       NROW(datacue$Jornada),
                       NROW(datauca$Jornada),
                       NROW(dataman$Jornada),
                       NROW(dataore$Jornada),
                       NROW(datadel$Jornada),
                       NROW(dataolm$Jornada)),
                   "Puntos"=
                     c(sum(bscvicl$VicLocal,
                           bscvicv$VicVisita,
                           bscempl$EmpLocal,
                           bscempv$EmpVisita),
                       sum(lduvicl$VicLocal,
                           lduvicv$VicVisita,
                           lduempl$EmpLocal,
                           lduempv$EmpVisita),
                       sum(emevicl$VicLocal,
                           emevicv$VicVisita,
                           emeempl$EmpLocal,
                           emeempv$EmpVisita),
                       sum(idvvicl$VicLocal,
                           idvvicv$VicVisita,
                           idvempl$EmpLocal,
                           idvempv$EmpVisita),
                       sum(aucvicl$VicLocal,
                           aucvicv$VicVisita,
                           aucempl$EmpLocal,
                           aucempv$EmpVisita),
                       sum(macvicl$VicLocal,
                           macvicv$VicVisita,
                           macempl$EmpLocal,
                           macempv$EmpVisita),
                       sum(tecvicl$VicLocal,
                           tecvicv$VicVisita,
                           tecempl$EmpLocal,
                           tecempv$EmpVisita),
                       sum(guavicl$VicLocal,
                           guavicv$VicVisita,
                           guaempl$EmpLocal,
                           guaempv$EmpVisita),
                       sum(musvicl$VicLocal,
                           musvicv$VicVisita,
                           musempl$EmpLocal,
                           musempv$EmpVisita),
                       sum(nuevicl$VicLocal,
                           nuevicv$VicVisita,
                           nueempl$EmpLocal,
                           nueempv$EmpVisita),
                       sum(cuevicl$VicLocal,
                           cuevicv$VicVisita,
                           cueempl$EmpLocal,
                           cueempv$EmpVisita),
                       sum(ucavicl$VicLocal,
                           ucavicv$VicVisita,
                           ucaempl$EmpLocal,
                           ucaempv$EmpVisita),
                       sum(manvicl$VicLocal,
                           manvicv$VicVisita,
                           manempl$EmpLocal,
                           manempv$EmpVisita),
                       sum(orevicl$VicLocal,
                           orevicv$VicVisita,
                           oreempl$EmpLocal,
                           oreempv$EmpVisita),
                       sum(delvicl$VicLocal,
                           delvicv$VicVisita,
                           delempl$EmpLocal,
                           delempv$EmpVisita),
                       sum(olmvicl$VicLocal,
                           olmvicv$VicVisita,
                           olmempl$EmpLocal,
                           olmempv$EmpVisita)),
                   "Ganados"=
                     c(sum(NROW(bscvicl),
                           NROW(bscvicv)),
                       sum(NROW(lduvicl),
                           NROW(lduvicv)),
                       sum(NROW(emevicl),
                           NROW(emevicv)),
                       sum(NROW(idvvicl),
                           NROW(idvvicv)),
                       sum(NROW(aucvicl),
                           NROW(aucvicv)),
                       sum(NROW(macvicl),
                           NROW(macvicv)),
                       sum(NROW(tecvicl),
                           NROW(tecvicv)),
                       sum(NROW(guavicl),
                           NROW(guavicv)),
                       sum(NROW(musvicl),
                           NROW(musvicv)),
                       sum(NROW(nuevicl),
                           NROW(nuevicv)),
                       sum(NROW(cuevicl),
                           NROW(cuevicv)),
                       sum(NROW(ucavicl),
                           NROW(ucavicv)),
                       sum(NROW(manvicl),
                           NROW(manvicv)),
                       sum(NROW(orevicl),
                           NROW(orevicv)),
                       sum(NROW(delvicl),
                           NROW(delvicv)),
                       sum(NROW(olmvicl),
                           NROW(olmvicv))),
                   "Empatados"=
                     c(sum(NROW(bscempl),
                           NROW(bscempv)),
                       sum(NROW(lduempl),
                           NROW(lduempv)),
                       sum(NROW(emeempl),
                           NROW(emeempv)),
                       sum(NROW(idvempl),
                           NROW(idvempv)),
                       sum(NROW(aucempl),
                           NROW(aucempv)),
                       sum(NROW(macempl),
                           NROW(macempv)),
                       sum(NROW(tecempl),
                           NROW(tecempv)),
                       sum(NROW(guaempl),
                           NROW(guaempv)),
                       sum(NROW(musempl),
                           NROW(musempv)),
                       sum(NROW(nueempl),
                           NROW(nueempv)),
                       sum(NROW(cueempl),
                           NROW(cueempv)),
                       sum(NROW(ucaempl),
                           NROW(ucaempv)),
                       sum(NROW(manempl),
                           NROW(manempv)),
                       sum(NROW(oreempl),
                           NROW(oreempv)),
                       sum(NROW(delempl),
                           NROW(delempv)),
                       sum(NROW(olmempl),
                           NROW(olmempv))),
                   "Derrotas"=
                     c(sum(NROW(bscderl),
                           NROW(bscderv)),
                       sum(NROW(lduderl),
                           NROW(lduderv)),
                       sum(NROW(emederl),
                           NROW(emederv)),
                       sum(NROW(idvderl),
                           NROW(idvderv)),
                       sum(NROW(aucderl),
                           NROW(aucderv)),
                       sum(NROW(macderl),
                           NROW(macderv)),
                       sum(NROW(tecderl),
                           NROW(tecderv)),
                       sum(NROW(guaderl),
                           NROW(guaderv)),
                       sum(NROW(musderl),
                           NROW(musderv)),
                       sum(NROW(nuederl),
                           NROW(nuederv)),
                       sum(NROW(cuederl),
                           NROW(cuederv)),
                       sum(NROW(ucaderl),
                           NROW(ucaderv)),
                       sum(NROW(manderl),
                           NROW(manderv)),
                       sum(NROW(orederl),
                           NROW(orederv)),
                       sum(NROW(delderl),
                           NROW(delderv)),
                       sum(NROW(olmderl),
                           NROW(olmderv))),
                   "GolesFavor"=
                     c(sum(bscvicl$GolLocal,
                           bscvicv$GolVisita,
                           bscempl$GolLocal,
                           bscempv$GolVisita,
                           bscderl$GolLocal,
                           bscderv$GolVisita),
                       sum(lduvicl$GolLocal,
                           lduvicv$GolVisita,
                           lduempl$GolLocal,
                           lduempv$GolVisita,
                           lduderl$GolLocal,
                           lduderv$GolVisita),
                       sum(emevicl$GolLocal,
                           emevicv$GolVisita,
                           emeempl$GolLocal,
                           emeempv$GolVisita,
                           emederl$GolLocal,
                           emederv$GolVisita),
                       sum(idvvicl$GolLocal,
                           idvvicv$GolVisita,
                           idvempl$GolLocal,
                           idvempv$GolVisita,
                           idvderl$GolLocal,
                           idvderv$GolVisita),
                       sum(aucvicl$GolLocal,
                           aucvicv$GolVisita,
                           aucempl$GolLocal,
                           aucempv$GolVisita,
                           aucderl$GolLocal,
                           aucderv$GolVisita),
                       sum(macvicl$GolLocal,
                           macvicv$GolVisita,
                           macempl$GolLocal,
                           macempv$GolVisita,
                           macderl$GolLocal,
                           macderv$GolVisita),
                       sum(tecvicl$GolLocal,
                           tecvicv$GolVisita,
                           tecempl$GolLocal,
                           tecempv$GolVisita,
                           tecderl$GolLocal,
                           tecderv$GolVisita),
                       sum(guavicl$GolLocal,
                           guavicv$GolVisita,
                           guaempl$GolLocal,
                           guaempv$GolVisita,
                           guaderl$GolLocal,
                           guaderv$GolVisita),
                       sum(musvicl$GolLocal,
                           musvicv$GolVisita,
                           musempl$GolLocal,
                           musempv$GolVisita,
                           musderl$GolLocal,
                           musderv$GolVisita),
                       sum(nuevicl$GolLocal,
                           nuevicv$GolVisita,
                           nueempl$GolLocal,
                           nueempv$GolVisita,
                           nuederl$GolLocal,
                           nuederv$GolVisita),
                       sum(cuevicl$GolLocal,
                           cuevicv$GolVisita,
                           cueempl$GolLocal,
                           cueempv$GolVisita,
                           cuederl$GolLocal,
                           cuederv$GolVisita),
                       sum(ucavicl$GolLocal,
                           ucavicv$GolVisita,
                           ucaempl$GolLocal,
                           ucaempv$GolVisita,
                           ucaderl$GolLocal,
                           ucaderv$GolVisita),
                       sum(manvicl$GolLocal,
                           manvicv$GolVisita,
                           manempl$GolLocal,
                           manempv$GolVisita,
                           manderl$GolLocal,
                           manderv$GolVisita),
                       sum(orevicl$GolLocal,
                           orevicv$GolVisita,
                           oreempl$GolLocal,
                           oreempv$GolVisita,
                           orederl$GolLocal,
                           orederv$GolVisita),
                       sum(delvicl$GolLocal,
                           delvicv$GolVisita,
                           delempl$GolLocal,
                           delempv$GolVisita,
                           delderl$GolLocal,
                           delderv$GolVisita),
                       sum(olmvicl$GolLocal,
                           olmvicv$GolVisita,
                           olmempl$GolLocal,
                           olmempv$GolVisita,
                           olmderl$GolLocal,
                           olmderv$GolVisita)),
                   "GolesContra"=
                     c(sum(bscvicl$GolVisita,
                           bscvicv$GolLocal,
                           bscempl$GolVisita,
                           bscempv$GolLocal,
                           bscderl$GolVisita,
                           bscderv$GolLocal),
                       sum(lduvicl$GolVisita,
                           lduvicv$GolLocal,
                           lduempl$GolVisita,
                           lduempv$GolLocal,
                           lduderl$GolVisita,
                           lduderv$GolLocal),
                       sum(emevicl$GolVisita,
                           emevicv$GolLocal,
                           emeempl$GolVisita,
                           emeempv$GolLocal,
                           emederl$GolVisita,
                           emederv$GolLocal),
                       sum(idvvicl$GolVisita,
                           idvvicv$GolLocal,
                           idvempl$GolVisita,
                           idvempv$GolLocal,
                           idvderl$GolVisita,
                           idvderv$GolLocal),
                       sum(aucvicl$GolVisita,
                           aucvicv$GolLocal,
                           aucempl$GolVisita,
                           aucempv$GolLocal,
                           aucderl$GolVisita,
                           aucderv$GolLocal),
                       sum(macvicl$GolVisita,
                           macvicv$GolLocal,
                           macempl$GolVisita,
                           macempv$GolLocal,
                           macderl$GolVisita,
                           macderv$GolLocal),
                       sum(tecvicl$GolVisita,
                           tecvicv$GolLocal,
                           tecempl$GolVisita,
                           tecempv$GolLocal,
                           tecderl$GolVisita,
                           tecderv$GolLocal),
                       sum(guavicl$GolVisita,
                           guavicv$GolLocal,
                           guaempl$GolVisita,
                           guaempv$GolLocal,
                           guaderl$GolVisita,
                           guaderv$GolLocal),
                       sum(musvicl$GolVisita,
                           musvicv$GolLocal,
                           musempl$GolVisita,
                           musempv$GolLocal,
                           musderl$GolVisita,
                           musderv$GolLocal),
                       sum(nuevicl$GolVisita,
                           nuevicv$GolLocal,
                           nueempl$GolVisita,
                           nueempv$GolLocal,
                           nuederl$GolVisita,
                           nuederv$GolLocal),
                       sum(cuevicl$GolVisita,
                           cuevicv$GolLocal,
                           cueempl$GolVisita,
                           cueempv$GolLocal,
                           cuederl$GolVisita,
                           cuederv$GolLocal),
                       sum(ucavicl$GolVisita,
                           ucavicv$GolLocal,
                           ucaempl$GolVisita,
                           ucaempv$GolLocal,
                           ucaderl$GolVisita,
                           ucaderv$GolLocal),
                       sum(manvicl$GolVisita,
                           manvicv$GolLocal,
                           manempl$GolVisita,
                           manempv$GolLocal,
                           manderl$GolVisita,
                           manderv$GolLocal),
                       sum(orevicl$GolVisita,
                           orevicv$GolLocal,
                           oreempl$GolVisita,
                           oreempv$GolLocal,
                           orederl$GolVisita,
                           orederv$GolLocal),
                       sum(delvicl$GolVisita,
                           delvicv$GolLocal,
                           delempl$GolVisita,
                           delempv$GolLocal,
                           delderl$GolVisita,
                           delderv$GolLocal),
                       sum(olmvicl$GolVisita,
                           olmvicv$GolLocal,
                           olmempl$GolVisita,
                           olmempv$GolLocal,
                           olmderl$GolVisita,
                           olmderv$GolLocal))
                     )

#Tabla Acumulada PJ-Ptos-G-E-D-GF-GC-GD
tabla <- mutate(tabla,
                   GolesDiferencia = GolesFavor - GolesContra)
#Tabla Acumulada Etapa PJ-Ptos-G-E-D-GF-GC-GD Ordenada
tablat <- tabla[order(-tabla$Puntos, -tabla$GolesDiferencia, -tabla$GolesFavor), ]

#Tabla 1ra Etapa PJ-Ptos-G-E-D-GF-GC
tabla1 = data.frame("Equipos" =
                      c("Barcelona",
                        "Liga",
                        "Emelec",
                        "Independiente",
                        "Aucas",
                        "Macará",
                        "Técnico",
                        "GuayaquilCity",
                        "Mushuc",
                        "9Octubre",
                        "DeportivoCuenca",
                        "UCatólica",
                        "Manta",
                        "Orense",
                        "Delfín",
                        "Olmedo"),
                    "PartidosJugados" =
                      c(NROW(databsc1$Jornada),
                        NROW(dataldu1$Jornada),
                        NROW(dataeme1$Jornada),
                        NROW(dataidv1$Jornada),
                        NROW(dataauc1$Jornada),
                        NROW(datamac1$Jornada),
                        NROW(datatec1$Jornada),
                        NROW(datagua1$Jornada),
                        NROW(datamus1$Jornada),
                        NROW(datanue1$Jornada),
                        NROW(datacue1$Jornada),
                        NROW(datauca1$Jornada),
                        NROW(dataman1$Jornada),
                        NROW(dataore1$Jornada),
                        NROW(datadel1$Jornada),
                        NROW(dataolm1$Jornada)),
                    "Puntos"=
                      c(sum(bscvicl1$VicLocal,
                            bscvicv1$VicVisita,
                            bscempl1$EmpLocal,
                            bscempv1$EmpVisita),
                        sum(lduvicl1$VicLocal,
                            lduvicv1$VicVisita,
                            lduempl1$EmpLocal,
                            lduempv1$EmpVisita),
                        sum(emevicl1$VicLocal,
                            emevicv1$VicVisita,
                            emeempl1$EmpLocal,
                            emeempv1$EmpVisita),
                        sum(idvvicl1$VicLocal,
                            idvvicv1$VicVisita,
                            idvempl1$EmpLocal,
                            idvempv1$EmpVisita),
                        sum(aucvicl1$VicLocal,
                            aucvicv1$VicVisita,
                            aucempl1$EmpLocal,
                            aucempv1$EmpVisita),
                        sum(macvicl1$VicLocal,
                            macvicv1$VicVisita,
                            macempl1$EmpLocal,
                            macempv1$EmpVisita),
                        sum(tecvicl1$VicLocal,
                            tecvicv1$VicVisita,
                            tecempl1$EmpLocal,
                            tecempv1$EmpVisita),
                        sum(guavicl1$VicLocal,
                            guavicv1$VicVisita,
                            guaempl1$EmpLocal,
                            guaempv1$EmpVisita),
                        sum(musvicl1$VicLocal,
                            musvicv1$VicVisita,
                            musempl1$EmpLocal,
                            musempv1$EmpVisita),
                        sum(nuevicl1$VicLocal,
                            nuevicv1$VicVisita,
                            nueempl1$EmpLocal,
                            nueempv1$EmpVisita),
                        sum(cuevicl1$VicLocal,
                            cuevicv1$VicVisita,
                            cueempl1$EmpLocal,
                            cueempv1$EmpVisita),
                        sum(ucavicl1$VicLocal,
                            ucavicv1$VicVisita,
                            ucaempl1$EmpLocal,
                            ucaempv1$EmpVisita),
                        sum(manvicl1$VicLocal,
                            manvicv1$VicVisita,
                            manempl1$EmpLocal,
                            manempv1$EmpVisita),
                        sum(orevicl1$VicLocal,
                            orevicv1$VicVisita,
                            oreempl1$EmpLocal,
                            oreempv1$EmpVisita),
                        sum(delvicl1$VicLocal,
                            delvicv1$VicVisita,
                            delempl1$EmpLocal,
                            delempv1$EmpVisita),
                        sum(olmvicl1$VicLocal,
                            olmvicv1$VicVisita,
                            olmempl1$EmpLocal,
                            olmempv1$EmpVisita)),
                    "Ganados"=
                      c(sum(NROW(bscvicl1),
                            NROW(bscvicv1)),
                        sum(NROW(lduvicl1),
                            NROW(lduvicv1)),
                        sum(NROW(emevicl1),
                            NROW(emevicv1)),
                        sum(NROW(idvvicl1),
                            NROW(idvvicv1)),
                        sum(NROW(aucvicl1),
                            NROW(aucvicv1)),
                        sum(NROW(macvicl1),
                            NROW(macvicv1)),
                        sum(NROW(tecvicl1),
                            NROW(tecvicv1)),
                        sum(NROW(guavicl1),
                            NROW(guavicv1)),
                        sum(NROW(musvicl1),
                            NROW(musvicv1)),
                        sum(NROW(nuevicl1),
                            NROW(nuevicv1)),
                        sum(NROW(cuevicl1),
                            NROW(cuevicv1)),
                        sum(NROW(ucavicl1),
                            NROW(ucavicv1)),
                        sum(NROW(manvicl1),
                            NROW(manvicv1)),
                        sum(NROW(orevicl1),
                            NROW(orevicv1)),
                        sum(NROW(delvicl1),
                            NROW(delvicv1)),
                        sum(NROW(olmvicl1),
                            NROW(olmvicv1))),
                    "Empatados"=
                      c(sum(NROW(bscempl1),
                            NROW(bscempv1)),
                        sum(NROW(lduempl1),
                            NROW(lduempv1)),
                        sum(NROW(emeempl1),
                            NROW(emeempv1)),
                        sum(NROW(idvempl1),
                            NROW(idvempv1)),
                        sum(NROW(aucempl1),
                            NROW(aucempv1)),
                        sum(NROW(macempl1),
                            NROW(macempv1)),
                        sum(NROW(tecempl1),
                            NROW(tecempv1)),
                        sum(NROW(guaempl1),
                            NROW(guaempv1)),
                        sum(NROW(musempl1),
                            NROW(musempv1)),
                        sum(NROW(nueempl1),
                            NROW(nueempv1)),
                        sum(NROW(cueempl1),
                            NROW(cueempv1)),
                        sum(NROW(ucaempl1),
                            NROW(ucaempv1)),
                        sum(NROW(manempl1),
                            NROW(manempv1)),
                        sum(NROW(oreempl1),
                            NROW(oreempv1)),
                        sum(NROW(delempl1),
                            NROW(delempv1)),
                        sum(NROW(olmempl1),
                            NROW(olmempv1))),
                    "Derrotas"=
                      c(sum(NROW(bscderl1),
                            NROW(bscderv1)),
                        sum(NROW(lduderl1),
                            NROW(lduderv1)),
                        sum(NROW(emederl1),
                            NROW(emederv1)),
                        sum(NROW(idvderl1),
                            NROW(idvderv1)),
                        sum(NROW(aucderl1),
                            NROW(aucderv1)),
                        sum(NROW(macderl1),
                            NROW(macderv1)),
                        sum(NROW(tecderl1),
                            NROW(tecderv1)),
                        sum(NROW(guaderl1),
                            NROW(guaderv1)),
                        sum(NROW(musderl1),
                            NROW(musderv1)),
                        sum(NROW(nuederl1),
                            NROW(nuederv1)),
                        sum(NROW(cuederl1),
                            NROW(cuederv1)),
                        sum(NROW(ucaderl1),
                            NROW(ucaderv1)),
                        sum(NROW(manderl1),
                            NROW(manderv1)),
                        sum(NROW(orederl1),
                            NROW(orederv1)),
                        sum(NROW(delderl1),
                            NROW(delderv1)),
                        sum(NROW(olmderl1),
                            NROW(olmderv1))),
                    "GolesFavor"=
                      c(sum(bscvicl1$GolLocal,
                            bscvicv1$GolVisita,
                            bscempl1$GolLocal,
                            bscempv1$GolVisita,
                            bscderl1$GolLocal,
                            bscderv1$GolVisita),
                        sum(lduvicl1$GolLocal,
                            lduvicv1$GolVisita,
                            lduempl1$GolLocal,
                            lduempv1$GolVisita,
                            lduderl1$GolLocal,
                            lduderv1$GolVisita),
                        sum(emevicl1$GolLocal,
                            emevicv1$GolVisita,
                            emeempl1$GolLocal,
                            emeempv1$GolVisita,
                            emederl1$GolLocal,
                            emederv1$GolVisita),
                        sum(idvvicl1$GolLocal,
                            idvvicv1$GolVisita,
                            idvempl1$GolLocal,
                            idvempv1$GolVisita,
                            idvderl1$GolLocal,
                            idvderv1$GolVisita),
                        sum(aucvicl1$GolLocal,
                            aucvicv1$GolVisita,
                            aucempl1$GolLocal,
                            aucempv1$GolVisita,
                            aucderl1$GolLocal,
                            aucderv1$GolVisita),
                        sum(macvicl1$GolLocal,
                            macvicv1$GolVisita,
                            macempl1$GolLocal,
                            macempv1$GolVisita,
                            macderl1$GolLocal,
                            macderv1$GolVisita),
                        sum(tecvicl1$GolLocal,
                            tecvicv1$GolVisita,
                            tecempl1$GolLocal,
                            tecempv1$GolVisita,
                            tecderl1$GolLocal,
                            tecderv1$GolVisita),
                        sum(guavicl1$GolLocal,
                            guavicv1$GolVisita,
                            guaempl1$GolLocal,
                            guaempv1$GolVisita,
                            guaderl1$GolLocal,
                            guaderv1$GolVisita),
                        sum(musvicl1$GolLocal,
                            musvicv1$GolVisita,
                            musempl1$GolLocal,
                            musempv1$GolVisita,
                            musderl1$GolLocal,
                            musderv1$GolVisita),
                        sum(nuevicl1$GolLocal,
                            nuevicv1$GolVisita,
                            nueempl1$GolLocal,
                            nueempv1$GolVisita,
                            nuederl1$GolLocal,
                            nuederv1$GolVisita),
                        sum(cuevicl1$GolLocal,
                            cuevicv1$GolVisita,
                            cueempl1$GolLocal,
                            cueempv1$GolVisita,
                            cuederl1$GolLocal,
                            cuederv1$GolVisita),
                        sum(ucavicl1$GolLocal,
                            ucavicv1$GolVisita,
                            ucaempl1$GolLocal,
                            ucaempv1$GolVisita,
                            ucaderl1$GolLocal,
                            ucaderv1$GolVisita),
                        sum(manvicl1$GolLocal,
                            manvicv1$GolVisita,
                            manempl1$GolLocal,
                            manempv1$GolVisita,
                            manderl1$GolLocal,
                            manderv1$GolVisita),
                        sum(orevicl1$GolLocal,
                            orevicv1$GolVisita,
                            oreempl1$GolLocal,
                            oreempv1$GolVisita,
                            orederl1$GolLocal,
                            orederv1$GolVisita),
                        sum(delvicl1$GolLocal,
                            delvicv1$GolVisita,
                            delempl1$GolLocal,
                            delempv1$GolVisita,
                            delderl1$GolLocal,
                            delderv1$GolVisita),
                        sum(olmvicl1$GolLocal,
                            olmvicv1$GolVisita,
                            olmempl1$GolLocal,
                            olmempv1$GolVisita,
                            olmderl1$GolLocal,
                            olmderv1$GolVisita)),
                    "GolesContra"=
                      c(sum(bscvicl1$GolVisita,
                            bscvicv1$GolLocal,
                            bscempl1$GolVisita,
                            bscempv1$GolLocal,
                            bscderl1$GolVisita,
                            bscderv1$GolLocal),
                        sum(lduvicl1$GolVisita,
                            lduvicv1$GolLocal,
                            lduempl1$GolVisita,
                            lduempv1$GolLocal,
                            lduderl1$GolVisita,
                            lduderv1$GolLocal),
                        sum(emevicl1$GolVisita,
                            emevicv1$GolLocal,
                            emeempl1$GolVisita,
                            emeempv1$GolLocal,
                            emederl1$GolVisita,
                            emederv1$GolLocal),
                        sum(idvvicl1$GolVisita,
                            idvvicv1$GolLocal,
                            idvempl1$GolVisita,
                            idvempv1$GolLocal,
                            idvderl1$GolVisita,
                            idvderv1$GolLocal),
                        sum(aucvicl1$GolVisita,
                            aucvicv1$GolLocal,
                            aucempl1$GolVisita,
                            aucempv1$GolLocal,
                            aucderl1$GolVisita,
                            aucderv1$GolLocal),
                        sum(macvicl1$GolVisita,
                            macvicv1$GolLocal,
                            macempl1$GolVisita,
                            macempv1$GolLocal,
                            macderl1$GolVisita,
                            macderv1$GolLocal),
                        sum(tecvicl1$GolVisita,
                            tecvicv1$GolLocal,
                            tecempl1$GolVisita,
                            tecempv1$GolLocal,
                            tecderl1$GolVisita,
                            tecderv1$GolLocal),
                        sum(guavicl1$GolVisita,
                            guavicv1$GolLocal,
                            guaempl1$GolVisita,
                            guaempv1$GolLocal,
                            guaderl1$GolVisita,
                            guaderv1$GolLocal),
                        sum(musvicl1$GolVisita,
                            musvicv1$GolLocal,
                            musempl1$GolVisita,
                            musempv1$GolLocal,
                            musderl1$GolVisita,
                            musderv1$GolLocal),
                        sum(nuevicl1$GolVisita,
                            nuevicv1$GolLocal,
                            nueempl1$GolVisita,
                            nueempv1$GolLocal,
                            nuederl1$GolVisita,
                            nuederv1$GolLocal),
                        sum(cuevicl1$GolVisita,
                            cuevicv1$GolLocal,
                            cueempl1$GolVisita,
                            cueempv1$GolLocal,
                            cuederl1$GolVisita,
                            cuederv1$GolLocal),
                        sum(ucavicl1$GolVisita,
                            ucavicv1$GolLocal,
                            ucaempl1$GolVisita,
                            ucaempv1$GolLocal,
                            ucaderl1$GolVisita,
                            ucaderv1$GolLocal),
                        sum(manvicl1$GolVisita,
                            manvicv1$GolLocal,
                            manempl1$GolVisita,
                            manempv1$GolLocal,
                            manderl1$GolVisita,
                            manderv1$GolLocal),
                        sum(orevicl1$GolVisita,
                            orevicv1$GolLocal,
                            oreempl1$GolVisita,
                            oreempv1$GolLocal,
                            orederl1$GolVisita,
                            orederv1$GolLocal),
                        sum(delvicl1$GolVisita,
                            delvicv1$GolLocal,
                            delempl1$GolVisita,
                            delempv1$GolLocal,
                            delderl1$GolVisita,
                            delderv1$GolLocal),
                        sum(olmvicl1$GolVisita,
                            olmvicv1$GolLocal,
                            olmempl1$GolVisita,
                            olmempv1$GolLocal,
                            olmderl1$GolVisita,
                            olmderv1$GolLocal))
)

#Tabla 1ra Etapa PJ-Ptos-G-E-D-GF-GC-GD
tabla1 <- mutate(tabla1,
                 GolesDiferencia = GolesFavor - GolesContra)
#Tabla 1ra Etapa PJ-Ptos-G-E-D-GF-GC-GD Ordenada
tabla1e <- tabla1[order(-tabla1$Puntos, -tabla1$GolesDiferencia), ]

#Tabla 2da Etapa PJ-Ptos-G-E-D-GF-GC
tabla2 = data.frame("Equipos" =
                     c("Barcelona",
                       "Liga",
                       "Emelec",
                       "Independiente",
                       "Aucas",
                       "Macará",
                       "Técnico",
                       "GuayaquilCity",
                       "Mushuc",
                       "9Octubre",
                       "DeportivoCuenca",
                       "UCatólica",
                       "Manta",
                       "Orense",
                       "Delfín",
                       "Olmedo"),
                   "PartidosJugados" =
                     c(NROW(databsc2$Jornada),
                       NROW(dataldu2$Jornada),
                       NROW(dataeme2$Jornada),
                       NROW(dataidv2$Jornada),
                       NROW(dataauc2$Jornada),
                       NROW(datamac2$Jornada),
                       NROW(datatec2$Jornada),
                       NROW(datagua2$Jornada),
                       NROW(datamus2$Jornada),
                       NROW(datanue2$Jornada),
                       NROW(datacue2$Jornada),
                       NROW(datauca2$Jornada),
                       NROW(dataman2$Jornada),
                       NROW(dataore2$Jornada),
                       NROW(datadel2$Jornada),
                       NROW(dataolm2$Jornada)),
                   "Puntos"=
                     c(sum(bscvicl2$VicLocal,
                           bscvicv2$VicVisita,
                           bscempl2$EmpLocal,
                           bscempv2$EmpVisita),
                       sum(lduvicl2$VicLocal,
                           lduvicv2$VicVisita,
                           lduempl2$EmpLocal,
                           lduempv2$EmpVisita),
                       sum(emevicl2$VicLocal,
                           emevicv2$VicVisita,
                           emeempl2$EmpLocal,
                           emeempv2$EmpVisita),
                       sum(idvvicl2$VicLocal,
                           idvvicv2$VicVisita,
                           idvempl2$EmpLocal,
                           idvempv2$EmpVisita),
                       sum(aucvicl2$VicLocal,
                           aucvicv2$VicVisita,
                           aucempl2$EmpLocal,
                           aucempv2$EmpVisita),
                       sum(macvicl2$VicLocal,
                           macvicv2$VicVisita,
                           macempl2$EmpLocal,
                           macempv2$EmpVisita),
                       sum(tecvicl2$VicLocal,
                           tecvicv2$VicVisita,
                           tecempl2$EmpLocal,
                           tecempv2$EmpVisita),
                       sum(guavicl2$VicLocal,
                           guavicv2$VicVisita,
                           guaempl2$EmpLocal,
                           guaempv2$EmpVisita),
                       sum(musvicl2$VicLocal,
                           musvicv2$VicVisita,
                           musempl2$EmpLocal,
                           musempv2$EmpVisita),
                       sum(nuevicl2$VicLocal,
                           nuevicv2$VicVisita,
                           nueempl2$EmpLocal,
                           nueempv2$EmpVisita),
                       sum(cuevicl2$VicLocal,
                           cuevicv2$VicVisita,
                           cueempl2$EmpLocal,
                           cueempv2$EmpVisita),
                       sum(ucavicl2$VicLocal,
                           ucavicv2$VicVisita,
                           ucaempl2$EmpLocal,
                           ucaempv2$EmpVisita),
                       sum(manvicl2$VicLocal,
                           manvicv2$VicVisita,
                           manempl2$EmpLocal,
                           manempv2$EmpVisita),
                       sum(orevicl2$VicLocal,
                           orevicv2$VicVisita,
                           oreempl2$EmpLocal,
                           oreempv2$EmpVisita),
                       sum(delvicl2$VicLocal,
                           delvicv2$VicVisita,
                           delempl2$EmpLocal,
                           delempv2$EmpVisita),
                       sum(olmvicl2$VicLocal,
                           olmvicv2$VicVisita,
                           olmempl2$EmpLocal,
                           olmempv2$EmpVisita)),
                   "Ganados"=
                     c(sum(NROW(bscvicl2),
                           NROW(bscvicv2)),
                       sum(NROW(lduvicl2),
                           NROW(lduvicv2)),
                       sum(NROW(emevicl2),
                           NROW(emevicv2)),
                       sum(NROW(idvvicl2),
                           NROW(idvvicv2)),
                       sum(NROW(aucvicl2),
                           NROW(aucvicv2)),
                       sum(NROW(macvicl2),
                           NROW(macvicv2)),
                       sum(NROW(tecvicl2),
                           NROW(tecvicv2)),
                       sum(NROW(guavicl2),
                           NROW(guavicv2)),
                       sum(NROW(musvicl2),
                           NROW(musvicv2)),
                       sum(NROW(nuevicl2),
                           NROW(nuevicv2)),
                       sum(NROW(cuevicl2),
                           NROW(cuevicv2)),
                       sum(NROW(ucavicl2),
                           NROW(ucavicv2)),
                       sum(NROW(manvicl2),
                           NROW(manvicv2)),
                       sum(NROW(orevicl2),
                           NROW(orevicv2)),
                       sum(NROW(delvicl2),
                           NROW(delvicv2)),
                       sum(NROW(olmvicl2),
                           NROW(olmvicv2))),
                   "Empatados"=
                     c(sum(NROW(bscempl2),
                           NROW(bscempv2)),
                       sum(NROW(lduempl2),
                           NROW(lduempv2)),
                       sum(NROW(emeempl2),
                           NROW(emeempv2)),
                       sum(NROW(idvempl2),
                           NROW(idvempv2)),
                       sum(NROW(aucempl2),
                           NROW(aucempv2)),
                       sum(NROW(macempl2),
                           NROW(macempv2)),
                       sum(NROW(tecempl2),
                           NROW(tecempv2)),
                       sum(NROW(guaempl2),
                           NROW(guaempv2)),
                       sum(NROW(musempl2),
                           NROW(musempv2)),
                       sum(NROW(nueempl2),
                           NROW(nueempv2)),
                       sum(NROW(cueempl2),
                           NROW(cueempv2)),
                       sum(NROW(ucaempl2),
                           NROW(ucaempv2)),
                       sum(NROW(manempl2),
                           NROW(manempv2)),
                       sum(NROW(oreempl2),
                           NROW(oreempv2)),
                       sum(NROW(delempl2),
                           NROW(delempv2)),
                       sum(NROW(olmempl2),
                           NROW(olmempv2))),
                   "Derrotas"=
                     c(sum(NROW(bscderl2),
                           NROW(bscderv2)),
                       sum(NROW(lduderl2),
                           NROW(lduderv2)),
                       sum(NROW(emederl2),
                           NROW(emederv2)),
                       sum(NROW(idvderl2),
                           NROW(idvderv2)),
                       sum(NROW(aucderl2),
                           NROW(aucderv2)),
                       sum(NROW(macderl2),
                           NROW(macderv2)),
                       sum(NROW(tecderl2),
                           NROW(tecderv2)),
                       sum(NROW(guaderl2),
                           NROW(guaderv2)),
                       sum(NROW(musderl2),
                           NROW(musderv2)),
                       sum(NROW(nuederl2),
                           NROW(nuederv2)),
                       sum(NROW(cuederl2),
                           NROW(cuederv2)),
                       sum(NROW(ucaderl2),
                           NROW(ucaderv2)),
                       sum(NROW(manderl2),
                           NROW(manderv2)),
                       sum(NROW(orederl2),
                           NROW(orederv2)),
                       sum(NROW(delderl2),
                           NROW(delderv2)),
                       sum(NROW(olmderl2),
                           NROW(olmderv2))),
                   "GolesFavor"=
                     c(sum(bscvicl2$GolLocal,
                           bscvicv2$GolVisita,
                           bscempl2$GolLocal,
                           bscempv2$GolVisita,
                           bscderl2$GolLocal,
                           bscderv2$GolVisita),
                       sum(lduvicl2$GolLocal,
                           lduvicv2$GolVisita,
                           lduempl2$GolLocal,
                           lduempv2$GolVisita,
                           lduderl2$GolLocal,
                           lduderv2$GolVisita),
                       sum(emevicl2$GolLocal,
                           emevicv2$GolVisita,
                           emeempl2$GolLocal,
                           emeempv2$GolVisita,
                           emederl2$GolLocal,
                           emederv2$GolVisita),
                       sum(idvvicl2$GolLocal,
                           idvvicv2$GolVisita,
                           idvempl2$GolLocal,
                           idvempv2$GolVisita,
                           idvderl2$GolLocal,
                           idvderv2$GolVisita),
                       sum(aucvicl2$GolLocal,
                           aucvicv2$GolVisita,
                           aucempl2$GolLocal,
                           aucempv2$GolVisita,
                           aucderl2$GolLocal,
                           aucderv2$GolVisita),
                       sum(macvicl2$GolLocal,
                           macvicv2$GolVisita,
                           macempl2$GolLocal,
                           macempv2$GolVisita,
                           macderl2$GolLocal,
                           macderv2$GolVisita),
                       sum(tecvicl2$GolLocal,
                           tecvicv2$GolVisita,
                           tecempl2$GolLocal,
                           tecempv2$GolVisita,
                           tecderl2$GolLocal,
                           tecderv2$GolVisita),
                       sum(guavicl2$GolLocal,
                           guavicv2$GolVisita,
                           guaempl2$GolLocal,
                           guaempv2$GolVisita,
                           guaderl2$GolLocal,
                           guaderv2$GolVisita),
                       sum(musvicl2$GolLocal,
                           musvicv2$GolVisita,
                           musempl2$GolLocal,
                           musempv2$GolVisita,
                           musderl2$GolLocal,
                           musderv2$GolVisita),
                       sum(nuevicl2$GolLocal,
                           nuevicv2$GolVisita,
                           nueempl2$GolLocal,
                           nueempv2$GolVisita,
                           nuederl2$GolLocal,
                           nuederv2$GolVisita),
                       sum(cuevicl2$GolLocal,
                           cuevicv2$GolVisita,
                           cueempl2$GolLocal,
                           cueempv2$GolVisita,
                           cuederl2$GolLocal,
                           cuederv2$GolVisita),
                       sum(ucavicl2$GolLocal,
                           ucavicv2$GolVisita,
                           ucaempl2$GolLocal,
                           ucaempv2$GolVisita,
                           ucaderl2$GolLocal,
                           ucaderv2$GolVisita),
                       sum(manvicl2$GolLocal,
                           manvicv2$GolVisita,
                           manempl2$GolLocal,
                           manempv2$GolVisita,
                           manderl2$GolLocal,
                           manderv2$GolVisita),
                       sum(orevicl2$GolLocal,
                           orevicv2$GolVisita,
                           oreempl2$GolLocal,
                           oreempv2$GolVisita,
                           orederl2$GolLocal,
                           orederv2$GolVisita),
                       sum(delvicl2$GolLocal,
                           delvicv2$GolVisita,
                           delempl2$GolLocal,
                           delempv2$GolVisita,
                           delderl2$GolLocal,
                           delderv2$GolVisita),
                       sum(olmvicl2$GolLocal,
                           olmvicv2$GolVisita,
                           olmempl2$GolLocal,
                           olmempv2$GolVisita,
                           olmderl2$GolLocal,
                           olmderv2$GolVisita)),
                   "GolesContra"=
                     c(sum(bscvicl2$GolVisita,
                           bscvicv2$GolLocal,
                           bscempl2$GolVisita,
                           bscempv2$GolLocal,
                           bscderl2$GolVisita,
                           bscderv2$GolLocal),
                       sum(lduvicl2$GolVisita,
                           lduvicv2$GolLocal,
                           lduempl2$GolVisita,
                           lduempv2$GolLocal,
                           lduderl2$GolVisita,
                           lduderv2$GolLocal),
                       sum(emevicl2$GolVisita,
                           emevicv2$GolLocal,
                           emeempl2$GolVisita,
                           emeempv2$GolLocal,
                           emederl2$GolVisita,
                           emederv2$GolLocal),
                       sum(idvvicl2$GolVisita,
                           idvvicv2$GolLocal,
                           idvempl2$GolVisita,
                           idvempv2$GolLocal,
                           idvderl2$GolVisita,
                           idvderv2$GolLocal),
                       sum(aucvicl2$GolVisita,
                           aucvicv2$GolLocal,
                           aucempl2$GolVisita,
                           aucempv2$GolLocal,
                           aucderl2$GolVisita,
                           aucderv2$GolLocal),
                       sum(macvicl2$GolVisita,
                           macvicv2$GolLocal,
                           macempl2$GolVisita,
                           macempv2$GolLocal,
                           macderl2$GolVisita,
                           macderv2$GolLocal),
                       sum(tecvicl2$GolVisita,
                           tecvicv2$GolLocal,
                           tecempl2$GolVisita,
                           tecempv2$GolLocal,
                           tecderl2$GolVisita,
                           tecderv2$GolLocal),
                       sum(guavicl2$GolVisita,
                           guavicv2$GolLocal,
                           guaempl2$GolVisita,
                           guaempv2$GolLocal,
                           guaderl2$GolVisita,
                           guaderv2$GolLocal),
                       sum(musvicl2$GolVisita,
                           musvicv2$GolLocal,
                           musempl2$GolVisita,
                           musempv2$GolLocal,
                           musderl2$GolVisita,
                           musderv2$GolLocal),
                       sum(nuevicl2$GolVisita,
                           nuevicv2$GolLocal,
                           nueempl2$GolVisita,
                           nueempv2$GolLocal,
                           nuederl2$GolVisita,
                           nuederv2$GolLocal),
                       sum(cuevicl2$GolVisita,
                           cuevicv2$GolLocal,
                           cueempl2$GolVisita,
                           cueempv2$GolLocal,
                           cuederl2$GolVisita,
                           cuederv2$GolLocal),
                       sum(ucavicl2$GolVisita,
                           ucavicv2$GolLocal,
                           ucaempl2$GolVisita,
                           ucaempv2$GolLocal,
                           ucaderl2$GolVisita,
                           ucaderv2$GolLocal),
                       sum(manvicl2$GolVisita,
                           manvicv2$GolLocal,
                           manempl2$GolVisita,
                           manempv2$GolLocal,
                           manderl2$GolVisita,
                           manderv2$GolLocal),
                       sum(orevicl2$GolVisita,
                           orevicv2$GolLocal,
                           oreempl2$GolVisita,
                           oreempv2$GolLocal,
                           orederl2$GolVisita,
                           orederv2$GolLocal),
                       sum(delvicl2$GolVisita,
                           delvicv2$GolLocal,
                           delempl2$GolVisita,
                           delempv2$GolLocal,
                           delderl2$GolVisita,
                           delderv2$GolLocal),
                       sum(olmvicl2$GolVisita,
                           olmvicv2$GolLocal,
                           olmempl2$GolVisita,
                           olmempv2$GolLocal,
                           olmderl2$GolVisita,
                           olmderv2$GolLocal))
)

#Tabla 2da Etapa PJ-Ptos-G-E-D-GF-GC-GD
tabla2 <- mutate(tabla2,
                GolesDiferencia = GolesFavor - GolesContra)
#Tabla 2da Etapa PJ-Ptos-G-E-D-GF-GC-GD Ordenada
tabla2e <- tabla2[order(-tabla2$Puntos, -tabla2$GolesDiferencia, -tabla2$GolesFavor), ]
tabla2e <- mutate(tabla2e,
                  Eficacia = (Puntos * 100) / (PartidosJugados * 3))

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
t4 <- ggplot(tablat,
             aes(x = Puntos,
                 y = reorder(Equipos, Puntos)))

t4 + geom_point(color = "blue",
                size = 2) +
  geom_segment(aes(x = 10, 
                   xend = Puntos, 
                   y = reorder(Equipos, Puntos), 
                   yend = reorder(Equipos, Puntos)),
               color = "lightgrey") +
  geom_text(aes(label = Puntos),
            vjust = -0.35) +
  labs(title = "Test4",
       subtitle = "Puntaje Tabla Acumulada",
       x = "Puntos",
       y = "Equipos") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

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
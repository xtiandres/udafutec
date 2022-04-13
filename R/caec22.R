# CAMPEONATO ECUATORIANO 2022
# TODAS LAS JORNADAS

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

# DATASET - COMPILADO DE TODAS LAS JORNADAS 2022
dace22 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ec2022.csv")

# BARCELONA
dbsc <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada == 1)
dbsc_l <- dbsc %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc_v <- dbsc %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc2 <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2))
dbsc2_l <- dbsc2 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc2_v <- dbsc2 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc3 <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3))
dbsc3_l <- dbsc3 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc3_v <- dbsc3 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc4 <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dbsc4_l <- dbsc4 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc4_v <- dbsc4 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc5 <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dbsc5_l <- dbsc5 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc5_v <- dbsc5 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc6 <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dbsc6_l <- dbsc6 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc6_v <- dbsc6 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc7 <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dbsc7_l <- dbsc7 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc7_v <- dbsc7 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# LIGA DE QUITO
dldu <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada == 1)
dldu_l <- dldu %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu_v <- dldu %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu2 <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2))
dldu2_l <- dldu2 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu2_v <- dldu2 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu3 <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3))
dldu3_l <- dldu3 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu3_v <- dldu3 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu4 <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dldu4_l <- dldu4 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu4_v <- dldu4 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu5 <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dldu5_l <- dldu5 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu5_v <- dldu5 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu6 <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dldu6_l <- dldu6 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu6_v <- dldu6 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu7 <- dace22 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dldu7_l <- dldu7 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu7_v <- dldu7 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# INDEPENDIENTE DEL VALLE
didv <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada == 1)
didv_l <- didv %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv_v <- didv %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv2 <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2))
didv2_l <- didv2 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv2_v <- didv2 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv3 <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3))
didv3_l <- didv3 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv3_v <- didv3 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv4 <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
didv4_l <- didv4 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv4_v <- didv4 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv5 <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
didv5_l <- didv5 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv5_v <- didv5 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv6 <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
didv6_l <- didv6 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv6_v <- didv6 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv7 <- dace22 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
didv7_l <- didv7 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv7_v <- didv7 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# EMELEC
deme <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada == 1)
deme_l <- deme %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme_v <- deme %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme2 <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2))
deme2_l <- deme2 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme2_v <- deme2 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme3 <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3))
deme3_l <- deme3 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme3_v <- deme3 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme4 <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
deme4_l <- deme4 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme4_v <- deme4 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme5 <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
deme5_l <- deme5 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme5_v <- deme5 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme6 <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
deme6_l <- deme6 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme6_v <- deme6 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme7 <- dace22 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
deme7_l <- deme7 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme7_v <- deme7 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# AUCAS
dauc <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada == 1)
dauc_l <- dauc %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc_v <- dauc %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc2 <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2))
dauc2_l <- dauc2 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc2_v <- dauc2 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc3 <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3))
dauc3_l <- dauc3 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc3_v <- dauc3 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc4 <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dauc4_l <- dauc4 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc4_v <- dauc4 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc5 <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dauc5_l <- dauc5 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc5_v <- dauc5 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc6 <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dauc6_l <- dauc6 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc6_v <- dauc6 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc7 <- dace22 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dauc7_l <- dauc7 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc7_v <- dauc7 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# MACARÁ
dmac <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada == 1)
dmac_l <- dmac %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac_v <- dmac %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmac2 <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada %in% c(1, 2))
dmac2_l <- dmac2 %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac2_v <- dmac2 %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmac3 <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada %in% c(1, 2, 3))
dmac3_l <- dmac3 %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac3_v <- dmac3 %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmac4 <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dmac4_l <- dmac4 %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac4_v <- dmac4 %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmac5 <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dmac5_l <- dmac5 %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac5_v <- dmac5 %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmac6 <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dmac6_l <- dmac6 %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac6_v <- dmac6 %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmac7 <- dace22 %>% 
  filter(Local == "Macará" | Visita == "Macará") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dmac7_l <- dmac7 %>%
  filter(Local == "Macará") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmac7_v <- dmac7 %>%
  filter(Visita == "Macará") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# ORENSE
dore <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada == 1)
dore_l <- dore %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore_v <- dore %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore2 <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2))
dore2_l <- dore2 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore2_v <- dore2 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore3 <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3))
dore3_l <- dore3 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore3_v <- dore3 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore4 <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dore4_l <- dore4 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore4_v <- dore4 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore5 <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dore5_l <- dore5 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore5_v <- dore5 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore6 <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dore6_l <- dore6 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore6_v <- dore6 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore7 <- dace22 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dore7_l <- dore7 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore7_v <- dore7 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# CUENCA
dcue <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada == 1)
dcue_l <- dcue %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue_v <- dcue %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue2 <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2))
dcue2_l <- dcue2 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue2_v <- dcue2 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue3 <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3))
dcue3_l <- dcue3 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue3_v <- dcue3 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue4 <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dcue4_l <- dcue4 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue4_v <- dcue4 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue5 <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dcue5_l <- dcue5 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue5_v <- dcue5 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue6 <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dcue6_l <- dcue6 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue6_v <- dcue6 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue7 <- dace22 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dcue7_l <- dcue7 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue7_v <- dcue7 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# GUAYAQUIL CITY
dgci <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada == 1)
dgci_l <- dgci %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci_v <- dgci %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci2 <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2))
dgci2_l <- dgci2 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci2_v <- dgci2 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci3 <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3))
dgci3_l <- dgci3 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci3_v <- dgci3 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci4 <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dgci4_l <- dgci4 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci4_v <- dgci4 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci5 <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dgci5_l <- dgci5 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci5_v <- dgci5 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci6 <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dgci6_l <- dgci6 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci6_v <- dgci6 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci7 <- dace22 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dgci7_l <- dgci7 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci7_v <- dgci7 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# CUMBAYA
dcum <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada == 1)
dcum_l <- dcum %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum_v <- dcum %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum2 <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2))
dcum2_l <- dcum2 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum2_v <- dcum2 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum3 <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3))
dcum3_l <- dcum3 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum3_v <- dcum3 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum4 <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dcum4_l <- dcum4 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum4_v <- dcum4 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum5 <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dcum5_l <- dcum5 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum5_v <- dcum5 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum6 <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dcum6_l <- dcum6 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum6_v <- dcum6 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum7 <- dace22 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dcum7_l <- dcum7 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum7_v <- dcum7 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# UCATOLICA
duca <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada == 1)
duca_l <- duca %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca_v <- duca %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca2 <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2))
duca2_l <- duca2 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca2_v <- duca2 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca3 <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3))
duca3_l <- duca3 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca3_v <- duca3 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca4 <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
duca4_l <- duca4 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca4_v <- duca4 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca5 <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
duca5_l <- duca5 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca5_v <- duca5 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca6 <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
duca6_l <- duca6 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca6_v <- duca6 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca7 <- dace22 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
duca7_l <- duca7 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca7_v <- duca7 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# NUEVE
dnoc <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada == 1)
dnoc_l <- dnoc %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc_v <- dnoc %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnoc2 <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada %in% c(1, 2))
dnoc2_l <- dnoc2 %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc2_v <- dnoc2 %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnoc3 <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada %in% c(1, 2, 3))
dnoc3_l <- dnoc3 %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc3_v <- dnoc3 %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnoc4 <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dnoc4_l <- dnoc4 %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc4_v <- dnoc4 %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnoc5 <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dnoc5_l <- dnoc5 %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc5_v <- dnoc5 %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnoc6 <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dnoc6_l <- dnoc6 %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc6_v <- dnoc6 %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnoc7 <- dace22 %>% 
  filter(Local == "Nueve de Octubre" | Visita == "Nueve de Octubre") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dnoc7_l <- dnoc7 %>%
  filter(Local == "Nueve de Octubre") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnoc7_v <- dnoc7 %>%
  filter(Visita == "Nueve de Octubre") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# DELFIN
ddel <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada == 1)
ddel_l <- ddel %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel_v <- ddel %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel2 <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2))
ddel2_l <- ddel2 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel2_v <- ddel2 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel3 <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3))
ddel3_l <- ddel3 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel3_v <- ddel3 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel4 <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
ddel4_l <- ddel4 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel4_v <- ddel4 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel5 <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
ddel5_l <- ddel5 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel5_v <- ddel5 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel6 <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
ddel6_l <- ddel6 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel6_v <- ddel6 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel7 <- dace22 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
ddel7_l <- ddel7 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel7_v <- ddel7 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# MUSHUC RUNA
dmus <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada == 1)
dmus_l <- dmus %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus_v <- dmus %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus2 <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2))
dmus2_l <- dmus2 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus2_v <- dmus2 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus3 <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3))
dmus3_l <- dmus3 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus3_v <- dmus3 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus4 <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dmus4_l <- dmus4 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus4_v <- dmus4 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus5 <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dmus5_l <- dmus5 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus5_v <- dmus5 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus6 <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dmus6_l <- dmus6 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus6_v <- dmus6 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus7 <- dace22 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dmus7_l <- dmus7 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus7_v <- dmus7 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# GUALACEO
dgsc <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada == 1)
dgsc_l <- dgsc %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc_v <- dgsc %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc2 <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2))
dgsc2_l <- dgsc2 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc2_v <- dgsc2 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc3 <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3))
dgsc3_l <- dgsc3 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc3_v <- dgsc3 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc4 <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dgsc4_l <- dgsc4 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc4_v <- dgsc4 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc5 <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dgsc5_l <- dgsc5 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc5_v <- dgsc5 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc6 <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dgsc6_l <- dgsc6 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc6_v <- dgsc6 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc7 <- dace22 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dgsc7_l <- dgsc7 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc7_v <- dgsc7 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# TECNICO U
dtun <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada == 1)
dtun_l <- dtun %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun_v <- dtun %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun2 <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada %in% c(1, 2))
dtun2_l <- dtun2 %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun2_v <- dtun2 %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun3 <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada %in% c(1, 2, 3))
dtun3_l <- dtun3 %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun3_v <- dtun3 %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun4 <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dtun4_l <- dtun4 %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun4_v <- dtun4 %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun5 <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dtun5_l <- dtun5 %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun5_v <- dtun5 %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun6 <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dtun6_l <- dtun6 %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun6_v <- dtun6 %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun7 <- dace22 %>% 
  filter(Local == "Técnico Univ." | Visita == "Técnico Univ.") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dtun7_l <- dtun7 %>%
  filter(Local == "Técnico Univ.") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun7_v <- dtun7 %>%
  filter(Visita == "Técnico Univ.") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))


# TABLA ACUMULADA DESPUES DE CADA JORNADA
tj1 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Macará", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Nueve de Octubre",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Univ."),
                   "PJ" =
                   c(NROW(dbsc$Jornada), NROW(dldu$Jornada), NROW(didv$Jornada), NROW(deme$Jornada),
                     NROW(dauc$Jornada), NROW(dmac$Jornada), NROW(dore$Jornada), NROW(dcue$Jornada),
                     NROW(dgci$Jornada), NROW(dcum$Jornada), NROW(duca$Jornada), NROW(dnoc$Jornada),
                     NROW(ddel$Jornada), NROW(dmus$Jornada), NROW(dgsc$Jornada), NROW(dtun$Jornada)),
                   "PTOS"=
                   c(sum(dbsc_l$VL*3,
                         dbsc_v$VV*3,
                         dbsc_l$EL*1,
                         dbsc_v$EV*1),
                     sum(dldu_l$VL*3,
                         dldu_v$VV*3,
                         dldu_l$EL*1,
                         dldu_v$EV*1),
                     sum(didv_l$VL*3,
                         didv_v$VV*3,
                         didv_l$EL*1,
                         didv_v$EV*1),
                     sum(deme_l$VL*3,
                         deme_v$VV*3,
                         deme_l$EL*1,
                         deme_v$EV*1),
                     sum(dauc_l$VL*3,
                         dauc_v$VV*3,
                         dauc_l$EL*1,
                         dauc_v$EV*1),
                     sum(dmac_l$VL*3,
                         dmac_v$VV*3,
                         dmac_l$EL*1,
                         dmac_v$EV*1),
                     sum(dore_l$VL*3,
                         dore_v$VV*3,
                         dore_l$EL*1,
                         dore_v$EV*1),
                     sum(dcue_l$VL*3,
                         dcue_v$VV*3,
                         dcue_l$EL*1,
                         dcue_v$EV*1),
                     sum(dgci_l$VL*3,
                         dgci_v$VV*3,
                         dgci_l$EL*1,
                         dgci_v$EV*1),
                     sum(dcum_l$VL*3,
                         dcum_v$VV*3,
                         dcum_l$EL*1,
                         dcum_v$EV*1),
                     sum(duca_l$VL*3,
                         duca_v$VV*3,
                         duca_l$EL*1,
                         duca_v$EV*1),
                     sum(dnoc_l$VL*3,
                         dnoc_v$VV*3,
                         dnoc_l$EL*1,
                         dnoc_v$EV*1),
                     sum(ddel_l$VL*3,
                         ddel_v$VV*3,
                         ddel_l$EL*1,
                         ddel_v$EV*1),
                     sum(dmus_l$VL*3,
                         dmus_v$VV*3,
                         dmus_l$EL*1,
                         dmus_v$EV*1),
                     sum(dgsc_l$VL*3,
                         dgsc_v$VV*3,
                         dgsc_l$EL*1,
                         dgsc_v$EV*1),
                     sum(dtun_l$VL*3,
                         dtun_v$VV*3,
                         dtun_l$EL*1,
                         dtun_v$EV*1)),
                   "GF"=
                   c(sum(dbsc_l$GL,
                         dbsc_v$GV),
                     sum(dldu_l$GL,
                         dldu_v$GV),
                     sum(didv_l$GL,
                         didv_v$GV),
                     sum(deme_l$GL,
                         deme_v$GV),
                     sum(dauc_l$GL,
                         dauc_v$GV),
                     sum(dmac_l$GL,
                         dmac_v$GV),
                     sum(dore_l$GL,
                         dore_v$GV),
                     sum(dcue_l$GL,
                         dcue_v$GV),
                     sum(dgci_l$GL,
                         dgci_v$GV),
                     sum(dcum_l$GL,
                         dcum_v$GV),
                     sum(duca_l$GL,
                         duca_v$GV),
                     sum(dnoc_l$GL,
                         dnoc_v$GV),
                     sum(ddel_l$GL,
                         ddel_v$GV),
                     sum(dmus_l$GL,
                         dmus_v$GV),
                     sum(dgsc_l$GL,
                         dgsc_v$GV),
                     sum(dtun_l$GL,
                         dtun_v$GV)),
                   "GC"=
                   c(sum(dbsc_l$GV,
                         dbsc_v$GL),
                     sum(dldu_l$GV,
                         dldu_v$GL),
                     sum(didv_l$GV,
                         didv_v$GL),
                     sum(deme_l$GV,
                         deme_v$GL),
                     sum(dauc_l$GV,
                         dauc_v$GL),
                     sum(dmac_l$GV,
                         dmac_v$GL),
                     sum(dore_l$GV,
                         dore_v$GL),
                     sum(dcue_l$GV,
                         dcue_v$GL),
                     sum(dgci_l$GV,
                         dgci_v$GL),
                     sum(dcum_l$GV,
                         dcum_v$GL),
                     sum(duca_l$GV,
                         duca_v$GL),
                     sum(dnoc_l$GV,
                         dnoc_v$GL),
                     sum(ddel_l$GV,
                         ddel_v$GL),
                     sum(dmus_l$GV,
                         dmus_v$GL),
                     sum(dgsc_l$GV,
                         dgsc_v$GL),
                     sum(dtun_l$GV,
                         dtun_v$GL))
)










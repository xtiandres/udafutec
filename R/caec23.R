# CAMPEONATO ECUATORIANO DE FÚTBOL - 2023 - FASE 1
# SCRIPT DE TODAS LAS JORNADAS
# SCRIPT J1

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
# DATASET - COMPILADO DE J1
dace2301 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ec2023_1.csv")
dace23 <- dace2301

# BARCELONA
dbsc1 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada == 1)
dbsc1_l <- dbsc1 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc1_v <- dbsc1 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc2 <- dace23 %>% 
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
dbsc3 <- dace23 %>% 
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
dbsc4 <- dace23 %>% 
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
dbsc5 <- dace23 %>% 
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
dbsc6 <- dace23 %>% 
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
dbsc7 <- dace23 %>% 
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
dbsc8 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dbsc8_l <- dbsc8 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc8_v <- dbsc8 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc9 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dbsc9_l <- dbsc9 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc9_v <- dbsc9 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc10 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dbsc10_l <- dbsc10 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc10_v <- dbsc10 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc11 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dbsc11_l <- dbsc11 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc11_v <- dbsc11 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc12 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dbsc12_l <- dbsc12 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc12_v <- dbsc12 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc13 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dbsc13_l <- dbsc13 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc13_v <- dbsc13 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc14 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dbsc14_l <- dbsc14 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc14_v <- dbsc14 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc15 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dbsc15_l <- dbsc15 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc15_v <- dbsc15 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dbsc16 <- dace23 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dbsc16_l <- dbsc16 %>%
  filter(Local == "Barcelona SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dbsc16_v <- dbsc16 %>%
  filter(Visita == "Barcelona SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# LIGA DE QUITO
dldu1 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada == 1)
dldu1_l <- dldu1 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu1_v <- dldu1 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu2 <- dace23 %>% 
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
dldu3 <- dace23 %>% 
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
dldu4 <- dace23 %>% 
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
dldu5 <- dace23 %>% 
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
dldu6 <- dace23 %>% 
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
dldu7 <- dace23 %>% 
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
dldu8 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dldu8_l <- dldu8 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu8_v <- dldu8 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu9 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dldu9_l <- dldu9 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu9_v <- dldu9 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu10 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dldu10_l <- dldu10 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu10_v <- dldu10 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu11 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dldu11_l <- dldu11 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu11_v <- dldu11 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu12 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dldu12_l <- dldu12 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu12_v <- dldu12 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu13 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dldu13_l <- dldu13 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu13_v <- dldu13 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu14 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dldu14_l <- dldu14 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu14_v <- dldu14 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu15 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dldu15_l <- dldu15 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu15_v <- dldu15 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dldu16 <- dace23 %>% 
  filter(Local == "LDU Quito" | Visita == "LDU Quito") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dldu16_l <- dldu16 %>%
  filter(Local == "LDU Quito") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dldu16_v <- dldu16 %>%
  filter(Visita == "LDU Quito") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# INDEPENDIENTE DEL VALLE
didv1 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada == 1)
didv1_l <- didv1 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv1_v <- didv1 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv2 <- dace23 %>% 
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
didv3 <- dace23 %>% 
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
didv4 <- dace23 %>% 
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
didv5 <- dace23 %>% 
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
didv6 <- dace23 %>% 
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
didv7 <- dace23 %>% 
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
didv8 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
didv8_l <- didv8 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv8_v <- didv8 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv9 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
didv9_l <- didv9 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv9_v <- didv9 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv10 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
didv10_l <- didv10 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv10_v <- didv10 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv11 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
didv11_l <- didv11 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv11_v <- didv11 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv12 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
didv12_l <- didv12 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv12_v <- didv12 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv13 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
didv13_l <- didv13 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv13_v <- didv13 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv14 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
didv14_l <- didv14 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv14_v <- didv14 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv15 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
didv15_l <- didv15 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv15_v <- didv15 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
didv16 <- dace23 %>% 
  filter(Local == "Independiente del Valle" | Visita == "Independiente del Valle") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
didv16_l <- didv16 %>%
  filter(Local == "Independiente del Valle") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
didv16_v <- didv16 %>%
  filter(Visita == "Independiente del Valle") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# EMELEC
deme1 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada == 1)
deme1_l <- deme1 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme1_v <- deme1 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme2 <- dace23 %>% 
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
deme3 <- dace23 %>% 
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
deme4 <- dace23 %>% 
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
deme5 <- dace23 %>% 
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
deme6 <- dace23 %>% 
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
deme7 <- dace23 %>% 
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
deme8 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
deme8_l <- deme8 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme8_v <- deme8 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme9 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
deme9_l <- deme9 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme9_v <- deme9 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme10 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
deme10_l <- deme10 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme10_v <- deme10 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme11 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
deme11_l <- deme11 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme11_v <- deme11 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme12 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
deme12_l <- deme12 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme12_v <- deme12 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme13 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
deme13_l <- deme13 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme13_v <- deme13 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme14 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
deme14_l <- deme14 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme14_v <- deme14 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme15 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
deme15_l <- deme15 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme15_v <- deme15 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
deme16 <- dace23 %>% 
  filter(Local == "Emelec" | Visita == "Emelec") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
deme16_l <- deme16 %>%
  filter(Local == "Emelec") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
deme16_v <- deme16 %>%
  filter(Visita == "Emelec") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# AUCAS
dauc1 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada == 1)
dauc1_l <- dauc1 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc1_v <- dauc1 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc2 <- dace23 %>% 
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
dauc3 <- dace23 %>% 
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
dauc4 <- dace23 %>% 
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
dauc5 <- dace23 %>% 
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
dauc6 <- dace23 %>% 
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
dauc7 <- dace23 %>% 
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
dauc8 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dauc8_l <- dauc8 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc8_v <- dauc8 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc9 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dauc9_l <- dauc9 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc9_v <- dauc9 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc10 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dauc10_l <- dauc10 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc10_v <- dauc10 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc11 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dauc11_l <- dauc11 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc11_v <- dauc11 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc12 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dauc12_l <- dauc12 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc12_v <- dauc12 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc13 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dauc13_l <- dauc13 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc13_v <- dauc13 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc14 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dauc14_l <- dauc14 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc14_v <- dauc14 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc15 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dauc15_l <- dauc15 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc15_v <- dauc15 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dauc16 <- dace23 %>% 
  filter(Local == "Aucas" | Visita == "Aucas") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dauc16_l <- dauc16 %>%
  filter(Local == "Aucas") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dauc16_v <- dauc16 %>%
  filter(Visita == "Aucas") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# NACIONAL
dnac1 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada == 1)
dnac1_l <- dnac1 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac1_v <- dnac1 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac2 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2))
dnac2_l <- dnac2 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac2_v <- dnac2 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac3 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3))
dnac3_l <- dnac3 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac3_v <- dnac3 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac4 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dnac4_l <- dnac4 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac4_v <- dnac4 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac5 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dnac5_l <- dnac5 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac5_v <- dnac5 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac6 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dnac6_l <- dnac6 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac6_v <- dnac6 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac7 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dnac7_l <- dnac7 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac7_v <- dnac7 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac8 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dnac8_l <- dnac8 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac8_v <- dnac8 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac9 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dnac9_l <- dnac9 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac9_v <- dnac9 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac10 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dnac10_l <- dnac10 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac10_v <- dnac10 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac11 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dnac11_l <- dnac11 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac11_v <- dnac11 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac12 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dnac12_l <- dnac12 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac12_v <- dnac12 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac13 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dnac13_l <- dnac13 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac13_v <- dnac13 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac14 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dnac14_l <- dnac14 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac14_v <- dnac14 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac15 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dnac15_l <- dnac15 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac15_v <- dnac15 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dnac16 <- dace23 %>% 
  filter(Local == "Nacional" | Visita == "Nacional") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dnac16_l <- dnac16 %>%
  filter(Local == "Nacional") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dnac16_v <- dnac16 %>%
  filter(Visita == "Nacional") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# ORENSE SC
dore1 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada == 1)
dore1_l <- dore1 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore1_v <- dore1 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore2 <- dace23 %>% 
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
dore3 <- dace23 %>% 
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
dore4 <- dace23 %>% 
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
dore5 <- dace23 %>% 
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
dore6 <- dace23 %>% 
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
dore7 <- dace23 %>% 
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
dore8 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dore8_l <- dore8 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore8_v <- dore8 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore9 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dore9_l <- dore9 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore9_v <- dore9 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore10 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dore10_l <- dore10 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore10_v <- dore10 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore11 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dore11_l <- dore11 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore11_v <- dore11 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore12 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dore12_l <- dore12 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore12_v <- dore12 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore13 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dore13_l <- dore13 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore13_v <- dore13 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore14 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dore14_l <- dore14 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore14_v <- dore14 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore15 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dore15_l <- dore15 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore15_v <- dore15 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dore16 <- dace23 %>% 
  filter(Local == "Orense SC" | Visita == "Orense SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dore16_l <- dore16 %>%
  filter(Local == "Orense SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dore16_v <- dore16 %>%
  filter(Visita == "Orense SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# CUENCA
dcue1 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada == 1)
dcue1_l <- dcue1 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue1_v <- dcue1 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue2 <- dace23 %>% 
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
dcue3 <- dace23 %>% 
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
dcue4 <- dace23 %>% 
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
dcue5 <- dace23 %>% 
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
dcue6 <- dace23 %>% 
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
dcue7 <- dace23 %>% 
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
dcue8 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dcue8_l <- dcue8 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue8_v <- dcue8 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue9 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dcue9_l <- dcue9 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue9_v <- dcue9 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue10 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dcue10_l <- dcue10 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue10_v <- dcue10 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue11 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dcue11_l <- dcue11 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue11_v <- dcue11 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue12 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dcue12_l <- dcue12 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue12_v <- dcue12 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue13 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dcue13_l <- dcue13 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue13_v <- dcue13 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue14 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dcue14_l <- dcue14 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue14_v <- dcue14 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue15 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dcue15_l <- dcue15 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue15_v <- dcue15 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcue16 <- dace23 %>% 
  filter(Local == "Deportivo Cuenca" | Visita == "Deportivo Cuenca") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dcue16_l <- dcue16 %>%
  filter(Local == "Deportivo Cuenca") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcue16_v <- dcue16 %>%
  filter(Visita == "Deportivo Cuenca") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# GUAYAQUIL CITY
dgci1 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada == 1)
dgci1_l <- dgci1 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci1_v <- dgci1 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci2 <- dace23 %>% 
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
dgci3 <- dace23 %>% 
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
dgci4 <- dace23 %>% 
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
dgci5 <- dace23 %>% 
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
dgci6 <- dace23 %>% 
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
dgci7 <- dace23 %>% 
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
dgci8 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dgci8_l <- dgci8 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci8_v <- dgci8 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci9 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dgci9_l <- dgci9 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci9_v <- dgci9 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci10 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dgci10_l <- dgci10 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci10_v <- dgci10 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci11 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dgci11_l <- dgci11 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci11_v <- dgci11 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci12 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dgci12_l <- dgci12 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci12_v <- dgci12 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci13 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dgci13_l <- dgci13 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci13_v <- dgci13 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci14 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dgci14_l <- dgci14 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci14_v <- dgci14 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci15 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dgci15_l <- dgci15 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci15_v <- dgci15 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgci16 <- dace23 %>% 
  filter(Local == "Guayaquil City" | Visita == "Guayaquil City") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dgci16_l <- dgci16 %>%
  filter(Local == "Guayaquil City") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgci16_v <- dgci16 %>%
  filter(Visita == "Guayaquil City") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# CUMBAYA
dcum1 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada == 1)
dcum1_l <- dcum1 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum1_v <- dcum1 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum2 <- dace23 %>% 
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
dcum3 <- dace23 %>% 
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
dcum4 <- dace23 %>% 
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
dcum5 <- dace23 %>% 
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
dcum6 <- dace23 %>% 
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
dcum7 <- dace23 %>% 
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
dcum8 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dcum8_l <- dcum8 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum8_v <- dcum8 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum9 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dcum9_l <- dcum9 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum9_v <- dcum9 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum10 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dcum10_l <- dcum10 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum10_v <- dcum10 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum11 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dcum11_l <- dcum11 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum11_v <- dcum11 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum12 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dcum12_l <- dcum12 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum12_v <- dcum12 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum13 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dcum13_l <- dcum13 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum13_v <- dcum13 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum14 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dcum14_l <- dcum14 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum14_v <- dcum14 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum15 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dcum15_l <- dcum15 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum15_v <- dcum15 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dcum16 <- dace23 %>% 
  filter(Local == "Cumbayá FC" | Visita == "Cumbayá FC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dcum16_l <- dcum16 %>%
  filter(Local == "Cumbayá FC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dcum16_v <- dcum16 %>%
  filter(Visita == "Cumbayá FC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# UCATOLICA
duca1 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada == 1)
duca1_l <- duca1 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca1_v <- duca1 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca2 <- dace23 %>% 
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
duca3 <- dace23 %>% 
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
duca4 <- dace23 %>% 
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
duca5 <- dace23 %>% 
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
duca6 <- dace23 %>% 
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
duca7 <- dace23 %>% 
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
duca8 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
duca8_l <- duca8 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca8_v <- duca8 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca9 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
duca9_l <- duca9 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca9_v <- duca9 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca10 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
duca10_l <- duca10 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca10_v <- duca10 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca11 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
duca11_l <- duca11 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca11_v <- duca11 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca12 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
duca12_l <- duca12 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca12_v <- duca12 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca13 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
duca13_l <- duca13 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca13_v <- duca13 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca14 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
duca14_l <- duca14 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca14_v <- duca14 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca15 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
duca15_l <- duca15 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca15_v <- duca15 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
duca16 <- dace23 %>% 
  filter(Local == "Universidad Católica" | Visita == "Universidad Católica") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
duca16_l <- duca16 %>%
  filter(Local == "Universidad Católica") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
duca16_v <- duca16 %>%
  filter(Visita == "Universidad Católica") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# LIBERTAD DE LOJA
dllo1 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada == 1)
dllo1_l <- dllo1 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo1_v <- dllo1 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo2 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2))
dllo2_l <- dllo2 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo2_v <- dllo2 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo3 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3))
dllo3_l <- dllo3 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo3_v <- dllo3 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo4 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dllo4_l <- dllo4 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo4_v <- dllo4 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo5 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dllo5_l <- dllo5 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo5_v <- dllo5 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo6 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dllo6_l <- dllo6 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo6_v <- dllo6 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo7 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dllo7_l <- dllo7 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo7_v <- dllo7 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo8 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dllo8_l <- dllo8 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo8_v <- dllo8 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo9 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dllo9_l <- dllo9 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo9_v <- dllo9 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo10 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dllo10_l <- dllo10 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo10_v <- dllo10 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo11 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dllo11_l <- dllo11 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo11_v <- dllo11 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo12 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dllo12_l <- dllo12 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo12_v <- dllo12 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo13 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dllo13_l <- dllo13 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo13_v <- dllo13 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo14 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dllo14_l <- dllo14 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo14_v <- dllo14 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo15 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dllo15_l <- dllo15 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo15_v <- dllo15 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dllo16 <- dace23 %>% 
  filter(Local == "Libertad de Loja" | Visita == "Libertad de Loja") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dllo16_l <- dllo16 %>%
  filter(Local == "Libertad de Loja") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dllo16_v <- dllo16 %>%
  filter(Visita == "Libertad de Loja") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# DELFIN
ddel1 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada == 1)
ddel1_l <- ddel1 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel1_v <- ddel1 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel2 <- dace23 %>% 
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
ddel3 <- dace23 %>% 
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
ddel4 <- dace23 %>% 
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
ddel5 <- dace23 %>% 
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
ddel6 <- dace23 %>% 
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
ddel7 <- dace23 %>% 
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
ddel8 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
ddel8_l <- ddel8 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel8_v <- ddel8 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel9 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
ddel9_l <- ddel9 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel9_v <- ddel9 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel10 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
ddel10_l <- ddel10 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel10_v <- ddel10 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel11 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
ddel11_l <- ddel11 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel11_v <- ddel11 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel12 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
ddel12_l <- ddel12 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel12_v <- ddel12 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel13 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
ddel13_l <- ddel13 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel13_v <- ddel13 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel14 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
ddel14_l <- ddel14 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel14_v <- ddel14 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel15 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
ddel15_l <- ddel15 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel15_v <- ddel15 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
ddel16 <- dace23 %>% 
  filter(Local == "Delfín SC" | Visita == "Delfín SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
ddel16_l <- ddel16 %>%
  filter(Local == "Delfín SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
ddel16_v <- ddel16 %>%
  filter(Visita == "Delfín SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# MUSHUC RUNA
dmus1 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada == 1)
dmus1_l <- dmus1 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus1_v <- dmus1 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus2 <- dace23 %>% 
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
dmus3 <- dace23 %>% 
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
dmus4 <- dace23 %>% 
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
dmus5 <- dace23 %>% 
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
dmus6 <- dace23 %>% 
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
dmus7 <- dace23 %>% 
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
dmus8 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dmus8_l <- dmus8 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus8_v <- dmus8 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus9 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dmus9_l <- dmus9 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus9_v <- dmus9 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus10 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dmus10_l <- dmus10 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus10_v <- dmus10 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus11 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dmus11_l <- dmus11 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus11_v <- dmus11 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus12 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dmus12_l <- dmus12 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus12_v <- dmus12 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus13 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dmus13_l <- dmus13 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus13_v <- dmus13 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus14 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dmus14_l <- dmus14 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus14_v <- dmus14 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus15 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dmus15_l <- dmus15 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus15_v <- dmus15 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dmus16 <- dace23 %>% 
  filter(Local == "Mushuc Runa" | Visita == "Mushuc Runa") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dmus16_l <- dmus16 %>%
  filter(Local == "Mushuc Runa") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dmus16_v <- dmus16 %>%
  filter(Visita == "Mushuc Runa") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# GUALACEO
dgsc1 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada == 1)
dgsc1_l <- dgsc1 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc1_v <- dgsc1 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc2 <- dace23 %>% 
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
dgsc3 <- dace23 %>% 
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
dgsc4 <- dace23 %>% 
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
dgsc5 <- dace23 %>% 
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
dgsc6 <- dace23 %>% 
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
dgsc7 <- dace23 %>% 
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
dgsc8 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dgsc8_l <- dgsc8 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc8_v <- dgsc8 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc9 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dgsc9_l <- dgsc9 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc9_v <- dgsc9 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc10 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dgsc10_l <- dgsc10 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc10_v <- dgsc10 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc11 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dgsc11_l <- dgsc11 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc11_v <- dgsc11 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc12 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dgsc12_l <- dgsc12 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc12_v <- dgsc12 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc13 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dgsc13_l <- dgsc13 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc13_v <- dgsc13 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc14 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dgsc14_l <- dgsc14 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc14_v <- dgsc14 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc15 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dgsc15_l <- dgsc15 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc15_v <- dgsc15 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dgsc16 <- dace23 %>% 
  filter(Local == "Gualaceo SC" | Visita == "Gualaceo SC") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dgsc16_l <- dgsc16 %>%
  filter(Local == "Gualaceo SC") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dgsc16_v <- dgsc16 %>%
  filter(Visita == "Gualaceo SC") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))

# TECNICO U
dtun1 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada == 1)
dtun1_l <- dtun1 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun1_v <- dtun1 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun2 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2))
dtun2_l <- dtun2 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun2_v <- dtun2 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun3 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3))
dtun3_l <- dtun3 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun3_v <- dtun3 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun4 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
dtun4_l <- dtun4 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun4_v <- dtun4 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun5 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
dtun5_l <- dtun5 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun5_v <- dtun5 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun6 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
dtun6_l <- dtun6 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun6_v <- dtun6 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun7 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
dtun7_l <- dtun7 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun7_v <- dtun7 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun8 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
dtun8_l <- dtun8 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun8_v <- dtun8 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun9 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
dtun9_l <- dtun9 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun9_v <- dtun9 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun10 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
dtun10_l <- dtun10 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun10_v <- dtun10 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun11 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
dtun11_l <- dtun11 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun11_v <- dtun11 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun12 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
dtun12_l <- dtun12 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun12_v <- dtun12 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun13 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
dtun13_l <- dtun13 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun13_v <- dtun13 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun14 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
dtun14_l <- dtun14 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun14_v <- dtun14 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun15 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
dtun15_l <- dtun15 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun15_v <- dtun15 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
dtun16 <- dace23 %>% 
  filter(Local == "Técnico Universitario" | Visita == "Técnico Universitario") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
dtun16_l <- dtun16 %>%
  filter(Local == "Técnico Universitario") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
dtun16_v <- dtun16 %>%
  filter(Visita == "Técnico Universitario") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))


# TABLA ACUMULADA DE CADA JORNADA
tj1 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc1$Jornada), NROW(dldu1$Jornada), NROW(didv1$Jornada), NROW(deme1$Jornada),
                     NROW(dauc1$Jornada), NROW(dnac1$Jornada), NROW(dore1$Jornada), NROW(dcue1$Jornada),
                     NROW(dgci1$Jornada), NROW(dcum1$Jornada), NROW(duca1$Jornada), NROW(dllo1$Jornada),
                     NROW(ddel1$Jornada), NROW(dmus1$Jornada), NROW(dgsc1$Jornada), NROW(dtun1$Jornada)),
                 "PTOS"=
                   c(sum(dbsc1_l$VL*3,
                         dbsc1_v$VV*3,
                         dbsc1_l$EL*1,
                         dbsc1_v$EV*1),
                     sum(dldu1_l$VL*3,
                         dldu1_v$VV*3,
                         dldu1_l$EL*1,
                         dldu1_v$EV*1),
                     sum(didv1_l$VL*3,
                         didv1_v$VV*3,
                         didv1_l$EL*1,
                         didv1_v$EV*1),
                     sum(deme1_l$VL*3,
                         deme1_v$VV*3,
                         deme1_l$EL*1,
                         deme1_v$EV*1),
                     sum(dauc1_l$VL*3,
                         dauc1_v$VV*3,
                         dauc1_l$EL*1,
                         dauc1_v$EV*1),
                     sum(dnac1_l$VL*3,
                         dnac1_v$VV*3,
                         dnac1_l$EL*1,
                         dnac1_v$EV*1),
                     sum(dore1_l$VL*3,
                         dore1_v$VV*3,
                         dore1_l$EL*1,
                         dore1_v$EV*1),
                     sum(dcue1_l$VL*3,
                         dcue1_v$VV*3,
                         dcue1_l$EL*1,
                         dcue1_v$EV*1),
                     sum(dgci1_l$VL*3,
                         dgci1_v$VV*3,
                         dgci1_l$EL*1,
                         dgci1_v$EV*1),
                     sum(dcum1_l$VL*3,
                         dcum1_v$VV*3,
                         dcum1_l$EL*1,
                         dcum1_v$EV*1),
                     sum(duca1_l$VL*3,
                         duca1_v$VV*3,
                         duca1_l$EL*1,
                         duca1_v$EV*1),
                     sum(dllo1_l$VL*3,
                         dllo1_v$VV*3,
                         dllo1_l$EL*1,
                         dllo1_v$EV*1),
                     sum(ddel1_l$VL*3,
                         ddel1_v$VV*3,
                         ddel1_l$EL*1,
                         ddel1_v$EV*1),
                     sum(dmus1_l$VL*3,
                         dmus1_v$VV*3,
                         dmus1_l$EL*1,
                         dmus1_v$EV*1),
                     sum(dgsc1_l$VL*3,
                         dgsc1_v$VV*3,
                         dgsc1_l$EL*1,
                         dgsc1_v$EV*1),
                     sum(dtun1_l$VL*3,
                         dtun1_v$VV*3,
                         dtun1_l$EL*1,
                         dtun1_v$EV*1)),
                 "GF"=
                   c(sum(dbsc1_l$GL,
                         dbsc1_v$GV),
                     sum(dldu1_l$GL,
                         dldu1_v$GV),
                     sum(didv1_l$GL,
                         didv1_v$GV),
                     sum(deme1_l$GL,
                         deme1_v$GV),
                     sum(dauc1_l$GL,
                         dauc1_v$GV),
                     sum(dnac1_l$GL,
                         dnac1_v$GV),
                     sum(dore1_l$GL,
                         dore1_v$GV),
                     sum(dcue1_l$GL,
                         dcue1_v$GV),
                     sum(dgci1_l$GL,
                         dgci1_v$GV),
                     sum(dcum1_l$GL,
                         dcum1_v$GV),
                     sum(duca1_l$GL,
                         duca1_v$GV),
                     sum(dllo1_l$GL,
                         dllo1_v$GV),
                     sum(ddel1_l$GL,
                         ddel1_v$GV),
                     sum(dmus1_l$GL,
                         dmus1_v$GV),
                     sum(dgsc1_l$GL,
                         dgsc1_v$GV),
                     sum(dtun1_l$GL,
                         dtun1_v$GV)),
                 "GC"=
                   c(sum(dbsc1_l$GV,
                         dbsc1_v$GL),
                     sum(dldu1_l$GV,
                         dldu1_v$GL),
                     sum(didv1_l$GV,
                         didv1_v$GL),
                     sum(deme1_l$GV,
                         deme1_v$GL),
                     sum(dauc1_l$GV,
                         dauc1_v$GL),
                     sum(dnac1_l$GV,
                         dnac1_v$GL),
                     sum(dore1_l$GV,
                         dore1_v$GL),
                     sum(dcue1_l$GV,
                         dcue1_v$GL),
                     sum(dgci1_l$GV,
                         dgci1_v$GL),
                     sum(dcum1_l$GV,
                         dcum1_v$GL),
                     sum(duca1_l$GV,
                         duca1_v$GL),
                     sum(dllo1_l$GV,
                         dllo1_v$GL),
                     sum(ddel1_l$GV,
                         ddel1_v$GL),
                     sum(dmus1_l$GV,
                         dmus1_v$GL),
                     sum(dgsc1_l$GV,
                         dgsc1_v$GL),
                     sum(dtun1_l$GV,
                         dtun1_v$GL))
)

tj2 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc2$Jornada), NROW(dldu2$Jornada), NROW(didv2$Jornada), NROW(deme2$Jornada),
                     NROW(dauc2$Jornada), NROW(dnac2$Jornada), NROW(dore2$Jornada), NROW(dcue2$Jornada),
                     NROW(dgci2$Jornada), NROW(dcum2$Jornada), NROW(duca2$Jornada), NROW(dllo2$Jornada),
                     NROW(ddel2$Jornada), NROW(dmus2$Jornada), NROW(dgsc2$Jornada), NROW(dtun2$Jornada)),
                 "PTOS"=
                   c(sum(dbsc2_l$VL*3,
                         dbsc2_v$VV*3,
                         dbsc2_l$EL*1,
                         dbsc2_v$EV*1),
                     sum(dldu2_l$VL*3,
                         dldu2_v$VV*3,
                         dldu2_l$EL*1,
                         dldu2_v$EV*1),
                     sum(didv2_l$VL*3,
                         didv2_v$VV*3,
                         didv2_l$EL*1,
                         didv2_v$EV*1),
                     sum(deme2_l$VL*3,
                         deme2_v$VV*3,
                         deme2_l$EL*1,
                         deme2_v$EV*1),
                     sum(dauc2_l$VL*3,
                         dauc2_v$VV*3,
                         dauc2_l$EL*1,
                         dauc2_v$EV*1),
                     sum(dnac2_l$VL*3,
                         dnac2_v$VV*3,
                         dnac2_l$EL*1,
                         dnac2_v$EV*1),
                     sum(dore2_l$VL*3,
                         dore2_v$VV*3,
                         dore2_l$EL*1,
                         dore2_v$EV*1),
                     sum(dcue2_l$VL*3,
                         dcue2_v$VV*3,
                         dcue2_l$EL*1,
                         dcue2_v$EV*1),
                     sum(dgci2_l$VL*3,
                         dgci2_v$VV*3,
                         dgci2_l$EL*1,
                         dgci2_v$EV*1),
                     sum(dcum2_l$VL*3,
                         dcum2_v$VV*3,
                         dcum2_l$EL*1,
                         dcum2_v$EV*1),
                     sum(duca2_l$VL*3,
                         duca2_v$VV*3,
                         duca2_l$EL*1,
                         duca2_v$EV*1),
                     sum(dllo2_l$VL*3,
                         dllo2_v$VV*3,
                         dllo2_l$EL*1,
                         dllo2_v$EV*1),
                     sum(ddel2_l$VL*3,
                         ddel2_v$VV*3,
                         ddel2_l$EL*1,
                         ddel2_v$EV*1),
                     sum(dmus2_l$VL*3,
                         dmus2_v$VV*3,
                         dmus2_l$EL*1,
                         dmus2_v$EV*1),
                     sum(dgsc2_l$VL*3,
                         dgsc2_v$VV*3,
                         dgsc2_l$EL*1,
                         dgsc2_v$EV*1),
                     sum(dtun2_l$VL*3,
                         dtun2_v$VV*3,
                         dtun2_l$EL*1,
                         dtun2_v$EV*1)),
                 "GF"=
                   c(sum(dbsc2_l$GL,
                         dbsc2_v$GV),
                     sum(dldu2_l$GL,
                         dldu2_v$GV),
                     sum(didv2_l$GL,
                         didv2_v$GV),
                     sum(deme2_l$GL,
                         deme2_v$GV),
                     sum(dauc2_l$GL,
                         dauc2_v$GV),
                     sum(dnac2_l$GL,
                         dnac2_v$GV),
                     sum(dore2_l$GL,
                         dore2_v$GV),
                     sum(dcue2_l$GL,
                         dcue2_v$GV),
                     sum(dgci2_l$GL,
                         dgci2_v$GV),
                     sum(dcum2_l$GL,
                         dcum2_v$GV),
                     sum(duca2_l$GL,
                         duca2_v$GV),
                     sum(dllo2_l$GL,
                         dllo2_v$GV),
                     sum(ddel2_l$GL,
                         ddel2_v$GV),
                     sum(dmus2_l$GL,
                         dmus2_v$GV),
                     sum(dgsc2_l$GL,
                         dgsc2_v$GV),
                     sum(dtun2_l$GL,
                         dtun2_v$GV)),
                 "GC"=
                   c(sum(dbsc2_l$GV,
                         dbsc2_v$GL),
                     sum(dldu2_l$GV,
                         dldu2_v$GL),
                     sum(didv2_l$GV,
                         didv2_v$GL),
                     sum(deme2_l$GV,
                         deme2_v$GL),
                     sum(dauc2_l$GV,
                         dauc2_v$GL),
                     sum(dnac2_l$GV,
                         dnac2_v$GL),
                     sum(dore2_l$GV,
                         dore2_v$GL),
                     sum(dcue2_l$GV,
                         dcue2_v$GL),
                     sum(dgci2_l$GV,
                         dgci2_v$GL),
                     sum(dcum2_l$GV,
                         dcum2_v$GL),
                     sum(duca2_l$GV,
                         duca2_v$GL),
                     sum(dllo2_l$GV,
                         dllo2_v$GL),
                     sum(ddel2_l$GV,
                         ddel2_v$GL),
                     sum(dmus2_l$GV,
                         dmus2_v$GL),
                     sum(dgsc2_l$GV,
                         dgsc2_v$GL),
                     sum(dtun2_l$GV,
                         dtun2_v$GL))
)

tj3 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc3$Jornada), NROW(dldu3$Jornada), NROW(didv3$Jornada), NROW(deme3$Jornada),
                     NROW(dauc3$Jornada), NROW(dnac3$Jornada), NROW(dore3$Jornada), NROW(dcue3$Jornada),
                     NROW(dgci3$Jornada), NROW(dcum3$Jornada), NROW(duca3$Jornada), NROW(dllo3$Jornada),
                     NROW(ddel3$Jornada), NROW(dmus3$Jornada), NROW(dgsc3$Jornada), NROW(dtun3$Jornada)),
                 "PTOS"=
                   c(sum(dbsc3_l$VL*3,
                         dbsc3_v$VV*3,
                         dbsc3_l$EL*1,
                         dbsc3_v$EV*1),
                     sum(dldu3_l$VL*3,
                         dldu3_v$VV*3,
                         dldu3_l$EL*1,
                         dldu3_v$EV*1),
                     sum(didv3_l$VL*3,
                         didv3_v$VV*3,
                         didv3_l$EL*1,
                         didv3_v$EV*1),
                     sum(deme3_l$VL*3,
                         deme3_v$VV*3,
                         deme3_l$EL*1,
                         deme3_v$EV*1),
                     sum(dauc3_l$VL*3,
                         dauc3_v$VV*3,
                         dauc3_l$EL*1,
                         dauc3_v$EV*1),
                     sum(dnac3_l$VL*3,
                         dnac3_v$VV*3,
                         dnac3_l$EL*1,
                         dnac3_v$EV*1),
                     sum(dore3_l$VL*3,
                         dore3_v$VV*3,
                         dore3_l$EL*1,
                         dore3_v$EV*1),
                     sum(dcue3_l$VL*3,
                         dcue3_v$VV*3,
                         dcue3_l$EL*1,
                         dcue3_v$EV*1),
                     sum(dgci3_l$VL*3,
                         dgci3_v$VV*3,
                         dgci3_l$EL*1,
                         dgci3_v$EV*1),
                     sum(dcum3_l$VL*3,
                         dcum3_v$VV*3,
                         dcum3_l$EL*1,
                         dcum3_v$EV*1),
                     sum(duca3_l$VL*3,
                         duca3_v$VV*3,
                         duca3_l$EL*1,
                         duca3_v$EV*1),
                     sum(dllo3_l$VL*3,
                         dllo3_v$VV*3,
                         dllo3_l$EL*1,
                         dllo3_v$EV*1),
                     sum(ddel3_l$VL*3,
                         ddel3_v$VV*3,
                         ddel3_l$EL*1,
                         ddel3_v$EV*1),
                     sum(dmus3_l$VL*3,
                         dmus3_v$VV*3,
                         dmus3_l$EL*1,
                         dmus3_v$EV*1),
                     sum(dgsc3_l$VL*3,
                         dgsc3_v$VV*3,
                         dgsc3_l$EL*1,
                         dgsc3_v$EV*1),
                     sum(dtun3_l$VL*3,
                         dtun3_v$VV*3,
                         dtun3_l$EL*1,
                         dtun3_v$EV*1)),
                 "GF"=
                   c(sum(dbsc3_l$GL,
                         dbsc3_v$GV),
                     sum(dldu3_l$GL,
                         dldu3_v$GV),
                     sum(didv3_l$GL,
                         didv3_v$GV),
                     sum(deme3_l$GL,
                         deme3_v$GV),
                     sum(dauc3_l$GL,
                         dauc3_v$GV),
                     sum(dnac3_l$GL,
                         dnac3_v$GV),
                     sum(dore3_l$GL,
                         dore3_v$GV),
                     sum(dcue3_l$GL,
                         dcue3_v$GV),
                     sum(dgci3_l$GL,
                         dgci3_v$GV),
                     sum(dcum3_l$GL,
                         dcum3_v$GV),
                     sum(duca3_l$GL,
                         duca3_v$GV),
                     sum(dllo3_l$GL,
                         dllo3_v$GV),
                     sum(ddel3_l$GL,
                         ddel3_v$GV),
                     sum(dmus3_l$GL,
                         dmus3_v$GV),
                     sum(dgsc3_l$GL,
                         dgsc3_v$GV),
                     sum(dtun3_l$GL,
                         dtun3_v$GV)),
                 "GC"=
                   c(sum(dbsc3_l$GV,
                         dbsc3_v$GL),
                     sum(dldu3_l$GV,
                         dldu3_v$GL),
                     sum(didv3_l$GV,
                         didv3_v$GL),
                     sum(deme3_l$GV,
                         deme3_v$GL),
                     sum(dauc3_l$GV,
                         dauc3_v$GL),
                     sum(dnac3_l$GV,
                         dnac3_v$GL),
                     sum(dore3_l$GV,
                         dore3_v$GL),
                     sum(dcue3_l$GV,
                         dcue3_v$GL),
                     sum(dgci3_l$GV,
                         dgci3_v$GL),
                     sum(dcum3_l$GV,
                         dcum3_v$GL),
                     sum(duca3_l$GV,
                         duca3_v$GL),
                     sum(dllo3_l$GV,
                         dllo3_v$GL),
                     sum(ddel3_l$GV,
                         ddel3_v$GL),
                     sum(dmus3_l$GV,
                         dmus3_v$GL),
                     sum(dgsc3_l$GV,
                         dgsc3_v$GL),
                     sum(dtun3_l$GV,
                         dtun3_v$GL))
)

tj4 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc4$Jornada), NROW(dldu4$Jornada), NROW(didv4$Jornada), NROW(deme4$Jornada),
                     NROW(dauc4$Jornada), NROW(dnac4$Jornada), NROW(dore4$Jornada), NROW(dcue4$Jornada),
                     NROW(dgci4$Jornada), NROW(dcum4$Jornada), NROW(duca4$Jornada), NROW(dllo4$Jornada),
                     NROW(ddel4$Jornada), NROW(dmus4$Jornada), NROW(dgsc4$Jornada), NROW(dtun4$Jornada)),
                 "PTOS"=
                   c(sum(dbsc4_l$VL*3,
                         dbsc4_v$VV*3,
                         dbsc4_l$EL*1,
                         dbsc4_v$EV*1),
                     sum(dldu4_l$VL*3,
                         dldu4_v$VV*3,
                         dldu4_l$EL*1,
                         dldu4_v$EV*1),
                     sum(didv4_l$VL*3,
                         didv4_v$VV*3,
                         didv4_l$EL*1,
                         didv4_v$EV*1),
                     sum(deme4_l$VL*3,
                         deme4_v$VV*3,
                         deme4_l$EL*1,
                         deme4_v$EV*1),
                     sum(dauc4_l$VL*3,
                         dauc4_v$VV*3,
                         dauc4_l$EL*1,
                         dauc4_v$EV*1),
                     sum(dnac4_l$VL*3,
                         dnac4_v$VV*3,
                         dnac4_l$EL*1,
                         dnac4_v$EV*1),
                     sum(dore4_l$VL*3,
                         dore4_v$VV*3,
                         dore4_l$EL*1,
                         dore4_v$EV*1),
                     sum(dcue4_l$VL*3,
                         dcue4_v$VV*3,
                         dcue4_l$EL*1,
                         dcue4_v$EV*1),
                     sum(dgci4_l$VL*3,
                         dgci4_v$VV*3,
                         dgci4_l$EL*1,
                         dgci4_v$EV*1),
                     sum(dcum4_l$VL*3,
                         dcum4_v$VV*3,
                         dcum4_l$EL*1,
                         dcum4_v$EV*1),
                     sum(duca4_l$VL*3,
                         duca4_v$VV*3,
                         duca4_l$EL*1,
                         duca4_v$EV*1),
                     sum(dllo4_l$VL*3,
                         dllo4_v$VV*3,
                         dllo4_l$EL*1,
                         dllo4_v$EV*1),
                     sum(ddel4_l$VL*3,
                         ddel4_v$VV*3,
                         ddel4_l$EL*1,
                         ddel4_v$EV*1),
                     sum(dmus4_l$VL*3,
                         dmus4_v$VV*3,
                         dmus4_l$EL*1,
                         dmus4_v$EV*1),
                     sum(dgsc4_l$VL*3,
                         dgsc4_v$VV*3,
                         dgsc4_l$EL*1,
                         dgsc4_v$EV*1),
                     sum(dtun4_l$VL*3,
                         dtun4_v$VV*3,
                         dtun4_l$EL*1,
                         dtun4_v$EV*1)),
                 "GF"=
                   c(sum(dbsc4_l$GL,
                         dbsc4_v$GV),
                     sum(dldu4_l$GL,
                         dldu4_v$GV),
                     sum(didv4_l$GL,
                         didv4_v$GV),
                     sum(deme4_l$GL,
                         deme4_v$GV),
                     sum(dauc4_l$GL,
                         dauc4_v$GV),
                     sum(dnac4_l$GL,
                         dnac4_v$GV),
                     sum(dore4_l$GL,
                         dore4_v$GV),
                     sum(dcue4_l$GL,
                         dcue4_v$GV),
                     sum(dgci4_l$GL,
                         dgci4_v$GV),
                     sum(dcum4_l$GL,
                         dcum4_v$GV),
                     sum(duca4_l$GL,
                         duca4_v$GV),
                     sum(dllo4_l$GL,
                         dllo4_v$GV),
                     sum(ddel4_l$GL,
                         ddel4_v$GV),
                     sum(dmus4_l$GL,
                         dmus4_v$GV),
                     sum(dgsc4_l$GL,
                         dgsc4_v$GV),
                     sum(dtun4_l$GL,
                         dtun4_v$GV)),
                 "GC"=
                   c(sum(dbsc4_l$GV,
                         dbsc4_v$GL),
                     sum(dldu4_l$GV,
                         dldu4_v$GL),
                     sum(didv4_l$GV,
                         didv4_v$GL),
                     sum(deme4_l$GV,
                         deme4_v$GL),
                     sum(dauc4_l$GV,
                         dauc4_v$GL),
                     sum(dnac4_l$GV,
                         dnac4_v$GL),
                     sum(dore4_l$GV,
                         dore4_v$GL),
                     sum(dcue4_l$GV,
                         dcue4_v$GL),
                     sum(dgci4_l$GV,
                         dgci4_v$GL),
                     sum(dcum4_l$GV,
                         dcum4_v$GL),
                     sum(duca4_l$GV,
                         duca4_v$GL),
                     sum(dllo4_l$GV,
                         dllo4_v$GL),
                     sum(ddel4_l$GV,
                         ddel4_v$GL),
                     sum(dmus4_l$GV,
                         dmus4_v$GL),
                     sum(dgsc4_l$GV,
                         dgsc4_v$GL),
                     sum(dtun4_l$GV,
                         dtun4_v$GL))
)

tj5 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc5$Jornada), NROW(dldu5$Jornada), NROW(didv5$Jornada), NROW(deme5$Jornada),
                     NROW(dauc5$Jornada), NROW(dnac5$Jornada), NROW(dore5$Jornada), NROW(dcue5$Jornada),
                     NROW(dgci5$Jornada), NROW(dcum5$Jornada), NROW(duca5$Jornada), NROW(dllo5$Jornada),
                     NROW(ddel5$Jornada), NROW(dmus5$Jornada), NROW(dgsc5$Jornada), NROW(dtun5$Jornada)),
                 "PTOS"=
                   c(sum(dbsc5_l$VL*3,
                         dbsc5_v$VV*3,
                         dbsc5_l$EL*1,
                         dbsc5_v$EV*1),
                     sum(dldu5_l$VL*3,
                         dldu5_v$VV*3,
                         dldu5_l$EL*1,
                         dldu5_v$EV*1),
                     sum(didv5_l$VL*3,
                         didv5_v$VV*3,
                         didv5_l$EL*1,
                         didv5_v$EV*1),
                     sum(deme5_l$VL*3,
                         deme5_v$VV*3,
                         deme5_l$EL*1,
                         deme5_v$EV*1),
                     sum(dauc5_l$VL*3,
                         dauc5_v$VV*3,
                         dauc5_l$EL*1,
                         dauc5_v$EV*1),
                     sum(dnac5_l$VL*3,
                         dnac5_v$VV*3,
                         dnac5_l$EL*1,
                         dnac5_v$EV*1),
                     sum(dore5_l$VL*3,
                         dore5_v$VV*3,
                         dore5_l$EL*1,
                         dore5_v$EV*1),
                     sum(dcue5_l$VL*3,
                         dcue5_v$VV*3,
                         dcue5_l$EL*1,
                         dcue5_v$EV*1),
                     sum(dgci5_l$VL*3,
                         dgci5_v$VV*3,
                         dgci5_l$EL*1,
                         dgci5_v$EV*1),
                     sum(dcum5_l$VL*3,
                         dcum5_v$VV*3,
                         dcum5_l$EL*1,
                         dcum5_v$EV*1),
                     sum(duca5_l$VL*3,
                         duca5_v$VV*3,
                         duca5_l$EL*1,
                         duca5_v$EV*1),
                     sum(dllo5_l$VL*3,
                         dllo5_v$VV*3,
                         dllo5_l$EL*1,
                         dllo5_v$EV*1),
                     sum(ddel5_l$VL*3,
                         ddel5_v$VV*3,
                         ddel5_l$EL*1,
                         ddel5_v$EV*1),
                     sum(dmus5_l$VL*3,
                         dmus5_v$VV*3,
                         dmus5_l$EL*1,
                         dmus5_v$EV*1),
                     sum(dgsc5_l$VL*3,
                         dgsc5_v$VV*3,
                         dgsc5_l$EL*1,
                         dgsc5_v$EV*1),
                     sum(dtun5_l$VL*3,
                         dtun5_v$VV*3,
                         dtun5_l$EL*1,
                         dtun5_v$EV*1)),
                 "GF"=
                   c(sum(dbsc5_l$GL,
                         dbsc5_v$GV),
                     sum(dldu5_l$GL,
                         dldu5_v$GV),
                     sum(didv5_l$GL,
                         didv5_v$GV),
                     sum(deme5_l$GL,
                         deme5_v$GV),
                     sum(dauc5_l$GL,
                         dauc5_v$GV),
                     sum(dnac5_l$GL,
                         dnac5_v$GV),
                     sum(dore5_l$GL,
                         dore5_v$GV),
                     sum(dcue5_l$GL,
                         dcue5_v$GV),
                     sum(dgci5_l$GL,
                         dgci5_v$GV),
                     sum(dcum5_l$GL,
                         dcum5_v$GV),
                     sum(duca5_l$GL,
                         duca5_v$GV),
                     sum(dllo5_l$GL,
                         dllo5_v$GV),
                     sum(ddel5_l$GL,
                         ddel5_v$GV),
                     sum(dmus5_l$GL,
                         dmus5_v$GV),
                     sum(dgsc5_l$GL,
                         dgsc5_v$GV),
                     sum(dtun5_l$GL,
                         dtun5_v$GV)),
                 "GC"=
                   c(sum(dbsc5_l$GV,
                         dbsc5_v$GL),
                     sum(dldu5_l$GV,
                         dldu5_v$GL),
                     sum(didv5_l$GV,
                         didv5_v$GL),
                     sum(deme5_l$GV,
                         deme5_v$GL),
                     sum(dauc5_l$GV,
                         dauc5_v$GL),
                     sum(dnac5_l$GV,
                         dnac5_v$GL),
                     sum(dore5_l$GV,
                         dore5_v$GL),
                     sum(dcue5_l$GV,
                         dcue5_v$GL),
                     sum(dgci5_l$GV,
                         dgci5_v$GL),
                     sum(dcum5_l$GV,
                         dcum5_v$GL),
                     sum(duca5_l$GV,
                         duca5_v$GL),
                     sum(dllo5_l$GV,
                         dllo5_v$GL),
                     sum(ddel5_l$GV,
                         ddel5_v$GL),
                     sum(dmus5_l$GV,
                         dmus5_v$GL),
                     sum(dgsc5_l$GV,
                         dgsc5_v$GL),
                     sum(dtun5_l$GV,
                         dtun5_v$GL))
)

tj6 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc6$Jornada), NROW(dldu6$Jornada), NROW(didv6$Jornada), NROW(deme6$Jornada),
                     NROW(dauc6$Jornada), NROW(dnac6$Jornada), NROW(dore6$Jornada), NROW(dcue6$Jornada),
                     NROW(dgci6$Jornada), NROW(dcum6$Jornada), NROW(duca6$Jornada), NROW(dllo6$Jornada),
                     NROW(ddel6$Jornada), NROW(dmus6$Jornada), NROW(dgsc6$Jornada), NROW(dtun6$Jornada)),
                 "PTOS"=
                   c(sum(dbsc6_l$VL*3,
                         dbsc6_v$VV*3,
                         dbsc6_l$EL*1,
                         dbsc6_v$EV*1),
                     sum(dldu6_l$VL*3,
                         dldu6_v$VV*3,
                         dldu6_l$EL*1,
                         dldu6_v$EV*1),
                     sum(didv6_l$VL*3,
                         didv6_v$VV*3,
                         didv6_l$EL*1,
                         didv6_v$EV*1),
                     sum(deme6_l$VL*3,
                         deme6_v$VV*3,
                         deme6_l$EL*1,
                         deme6_v$EV*1),
                     sum(dauc6_l$VL*3,
                         dauc6_v$VV*3,
                         dauc6_l$EL*1,
                         dauc6_v$EV*1),
                     sum(dnac6_l$VL*3,
                         dnac6_v$VV*3,
                         dnac6_l$EL*1,
                         dnac6_v$EV*1),
                     sum(dore6_l$VL*3,
                         dore6_v$VV*3,
                         dore6_l$EL*1,
                         dore6_v$EV*1),
                     sum(dcue6_l$VL*3,
                         dcue6_v$VV*3,
                         dcue6_l$EL*1,
                         dcue6_v$EV*1),
                     sum(dgci6_l$VL*3,
                         dgci6_v$VV*3,
                         dgci6_l$EL*1,
                         dgci6_v$EV*1),
                     sum(dcum6_l$VL*3,
                         dcum6_v$VV*3,
                         dcum6_l$EL*1,
                         dcum6_v$EV*1),
                     sum(duca6_l$VL*3,
                         duca6_v$VV*3,
                         duca6_l$EL*1,
                         duca6_v$EV*1),
                     sum(dllo6_l$VL*3,
                         dllo6_v$VV*3,
                         dllo6_l$EL*1,
                         dllo6_v$EV*1),
                     sum(ddel6_l$VL*3,
                         ddel6_v$VV*3,
                         ddel6_l$EL*1,
                         ddel6_v$EV*1),
                     sum(dmus6_l$VL*3,
                         dmus6_v$VV*3,
                         dmus6_l$EL*1,
                         dmus6_v$EV*1),
                     sum(dgsc6_l$VL*3,
                         dgsc6_v$VV*3,
                         dgsc6_l$EL*1,
                         dgsc6_v$EV*1),
                     sum(dtun6_l$VL*3,
                         dtun6_v$VV*3,
                         dtun6_l$EL*1,
                         dtun6_v$EV*1)),
                 "GF"=
                   c(sum(dbsc6_l$GL,
                         dbsc6_v$GV),
                     sum(dldu6_l$GL,
                         dldu6_v$GV),
                     sum(didv6_l$GL,
                         didv6_v$GV),
                     sum(deme6_l$GL,
                         deme6_v$GV),
                     sum(dauc6_l$GL,
                         dauc6_v$GV),
                     sum(dnac6_l$GL,
                         dnac6_v$GV),
                     sum(dore6_l$GL,
                         dore6_v$GV),
                     sum(dcue6_l$GL,
                         dcue6_v$GV),
                     sum(dgci6_l$GL,
                         dgci6_v$GV),
                     sum(dcum6_l$GL,
                         dcum6_v$GV),
                     sum(duca6_l$GL,
                         duca6_v$GV),
                     sum(dllo6_l$GL,
                         dllo6_v$GV),
                     sum(ddel6_l$GL,
                         ddel6_v$GV),
                     sum(dmus6_l$GL,
                         dmus6_v$GV),
                     sum(dgsc6_l$GL,
                         dgsc6_v$GV),
                     sum(dtun6_l$GL,
                         dtun6_v$GV)),
                 "GC"=
                   c(sum(dbsc6_l$GV,
                         dbsc6_v$GL),
                     sum(dldu6_l$GV,
                         dldu6_v$GL),
                     sum(didv6_l$GV,
                         didv6_v$GL),
                     sum(deme6_l$GV,
                         deme6_v$GL),
                     sum(dauc6_l$GV,
                         dauc6_v$GL),
                     sum(dnac6_l$GV,
                         dnac6_v$GL),
                     sum(dore6_l$GV,
                         dore6_v$GL),
                     sum(dcue6_l$GV,
                         dcue6_v$GL),
                     sum(dgci6_l$GV,
                         dgci6_v$GL),
                     sum(dcum6_l$GV,
                         dcum6_v$GL),
                     sum(duca6_l$GV,
                         duca6_v$GL),
                     sum(dllo6_l$GV,
                         dllo6_v$GL),
                     sum(ddel6_l$GV,
                         ddel6_v$GL),
                     sum(dmus6_l$GV,
                         dmus6_v$GL),
                     sum(dgsc6_l$GV,
                         dgsc6_v$GL),
                     sum(dtun6_l$GV,
                         dtun6_v$GL))
)

tj7 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc7$Jornada), NROW(dldu7$Jornada), NROW(didv7$Jornada), NROW(deme7$Jornada),
                     NROW(dauc7$Jornada), NROW(dnac7$Jornada), NROW(dore7$Jornada), NROW(dcue7$Jornada),
                     NROW(dgci7$Jornada), NROW(dcum7$Jornada), NROW(duca7$Jornada), NROW(dllo7$Jornada),
                     NROW(ddel7$Jornada), NROW(dmus7$Jornada), NROW(dgsc7$Jornada), NROW(dtun7$Jornada)),
                 "PTOS"=
                   c(sum(dbsc7_l$VL*3,
                         dbsc7_v$VV*3,
                         dbsc7_l$EL*1,
                         dbsc7_v$EV*1),
                     sum(dldu7_l$VL*3,
                         dldu7_v$VV*3,
                         dldu7_l$EL*1,
                         dldu7_v$EV*1),
                     sum(didv7_l$VL*3,
                         didv7_v$VV*3,
                         didv7_l$EL*1,
                         didv7_v$EV*1),
                     sum(deme7_l$VL*3,
                         deme7_v$VV*3,
                         deme7_l$EL*1,
                         deme7_v$EV*1),
                     sum(dauc7_l$VL*3,
                         dauc7_v$VV*3,
                         dauc7_l$EL*1,
                         dauc7_v$EV*1),
                     sum(dnac7_l$VL*3,
                         dnac7_v$VV*3,
                         dnac7_l$EL*1,
                         dnac7_v$EV*1),
                     sum(dore7_l$VL*3,
                         dore7_v$VV*3,
                         dore7_l$EL*1,
                         dore7_v$EV*1),
                     sum(dcue7_l$VL*3,
                         dcue7_v$VV*3,
                         dcue7_l$EL*1,
                         dcue7_v$EV*1),
                     sum(dgci7_l$VL*3,
                         dgci7_v$VV*3,
                         dgci7_l$EL*1,
                         dgci7_v$EV*1),
                     sum(dcum7_l$VL*3,
                         dcum7_v$VV*3,
                         dcum7_l$EL*1,
                         dcum7_v$EV*1),
                     sum(duca7_l$VL*3,
                         duca7_v$VV*3,
                         duca7_l$EL*1,
                         duca7_v$EV*1),
                     sum(dllo7_l$VL*3,
                         dllo7_v$VV*3,
                         dllo7_l$EL*1,
                         dllo7_v$EV*1),
                     sum(ddel7_l$VL*3,
                         ddel7_v$VV*3,
                         ddel7_l$EL*1,
                         ddel7_v$EV*1),
                     sum(dmus7_l$VL*3,
                         dmus7_v$VV*3,
                         dmus7_l$EL*1,
                         dmus7_v$EV*1),
                     sum(dgsc7_l$VL*3,
                         dgsc7_v$VV*3,
                         dgsc7_l$EL*1,
                         dgsc7_v$EV*1),
                     sum(dtun7_l$VL*3,
                         dtun7_v$VV*3,
                         dtun7_l$EL*1,
                         dtun7_v$EV*1)),
                 "GF"=
                   c(sum(dbsc7_l$GL,
                         dbsc7_v$GV),
                     sum(dldu7_l$GL,
                         dldu7_v$GV),
                     sum(didv7_l$GL,
                         didv7_v$GV),
                     sum(deme7_l$GL,
                         deme7_v$GV),
                     sum(dauc7_l$GL,
                         dauc7_v$GV),
                     sum(dnac7_l$GL,
                         dnac7_v$GV),
                     sum(dore7_l$GL,
                         dore7_v$GV),
                     sum(dcue7_l$GL,
                         dcue7_v$GV),
                     sum(dgci7_l$GL,
                         dgci7_v$GV),
                     sum(dcum7_l$GL,
                         dcum7_v$GV),
                     sum(duca7_l$GL,
                         duca7_v$GV),
                     sum(dllo7_l$GL,
                         dllo7_v$GV),
                     sum(ddel7_l$GL,
                         ddel7_v$GV),
                     sum(dmus7_l$GL,
                         dmus7_v$GV),
                     sum(dgsc7_l$GL,
                         dgsc7_v$GV),
                     sum(dtun7_l$GL,
                         dtun7_v$GV)),
                 "GC"=
                   c(sum(dbsc7_l$GV,
                         dbsc7_v$GL),
                     sum(dldu7_l$GV,
                         dldu7_v$GL),
                     sum(didv7_l$GV,
                         didv7_v$GL),
                     sum(deme7_l$GV,
                         deme7_v$GL),
                     sum(dauc7_l$GV,
                         dauc7_v$GL),
                     sum(dnac7_l$GV,
                         dnac7_v$GL),
                     sum(dore7_l$GV,
                         dore7_v$GL),
                     sum(dcue7_l$GV,
                         dcue7_v$GL),
                     sum(dgci7_l$GV,
                         dgci7_v$GL),
                     sum(dcum7_l$GV,
                         dcum7_v$GL),
                     sum(duca7_l$GV,
                         duca7_v$GL),
                     sum(dllo7_l$GV,
                         dllo7_v$GL),
                     sum(ddel7_l$GV,
                         ddel7_v$GL),
                     sum(dmus7_l$GV,
                         dmus7_v$GL),
                     sum(dgsc7_l$GV,
                         dgsc7_v$GL),
                     sum(dtun7_l$GV,
                         dtun7_v$GL))
)

tj8 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc8$Jornada), NROW(dldu8$Jornada), NROW(didv8$Jornada), NROW(deme8$Jornada),
                     NROW(dauc8$Jornada), NROW(dnac8$Jornada), NROW(dore8$Jornada), NROW(dcue8$Jornada),
                     NROW(dgci8$Jornada), NROW(dcum8$Jornada), NROW(duca8$Jornada), NROW(dllo8$Jornada),
                     NROW(ddel8$Jornada), NROW(dmus8$Jornada), NROW(dgsc8$Jornada), NROW(dtun8$Jornada)),
                 "PTOS"=
                   c(sum(dbsc8_l$VL*3,
                         dbsc8_v$VV*3,
                         dbsc8_l$EL*1,
                         dbsc8_v$EV*1),
                     sum(dldu8_l$VL*3,
                         dldu8_v$VV*3,
                         dldu8_l$EL*1,
                         dldu8_v$EV*1),
                     sum(didv8_l$VL*3,
                         didv8_v$VV*3,
                         didv8_l$EL*1,
                         didv8_v$EV*1),
                     sum(deme8_l$VL*3,
                         deme8_v$VV*3,
                         deme8_l$EL*1,
                         deme8_v$EV*1),
                     sum(dauc8_l$VL*3,
                         dauc8_v$VV*3,
                         dauc8_l$EL*1,
                         dauc8_v$EV*1),
                     sum(dnac8_l$VL*3,
                         dnac8_v$VV*3,
                         dnac8_l$EL*1,
                         dnac8_v$EV*1),
                     sum(dore8_l$VL*3,
                         dore8_v$VV*3,
                         dore8_l$EL*1,
                         dore8_v$EV*1),
                     sum(dcue8_l$VL*3,
                         dcue8_v$VV*3,
                         dcue8_l$EL*1,
                         dcue8_v$EV*1),
                     sum(dgci8_l$VL*3,
                         dgci8_v$VV*3,
                         dgci8_l$EL*1,
                         dgci8_v$EV*1),
                     sum(dcum8_l$VL*3,
                         dcum8_v$VV*3,
                         dcum8_l$EL*1,
                         dcum8_v$EV*1),
                     sum(duca8_l$VL*3,
                         duca8_v$VV*3,
                         duca8_l$EL*1,
                         duca8_v$EV*1),
                     sum(dllo8_l$VL*3,
                         dllo8_v$VV*3,
                         dllo8_l$EL*1,
                         dllo8_v$EV*1),
                     sum(ddel8_l$VL*3,
                         ddel8_v$VV*3,
                         ddel8_l$EL*1,
                         ddel8_v$EV*1),
                     sum(dmus8_l$VL*3,
                         dmus8_v$VV*3,
                         dmus8_l$EL*1,
                         dmus8_v$EV*1),
                     sum(dgsc8_l$VL*3,
                         dgsc8_v$VV*3,
                         dgsc8_l$EL*1,
                         dgsc8_v$EV*1),
                     sum(dtun8_l$VL*3,
                         dtun8_v$VV*3,
                         dtun8_l$EL*1,
                         dtun8_v$EV*1)),
                 "GF"=
                   c(sum(dbsc8_l$GL,
                         dbsc8_v$GV),
                     sum(dldu8_l$GL,
                         dldu8_v$GV),
                     sum(didv8_l$GL,
                         didv8_v$GV),
                     sum(deme8_l$GL,
                         deme8_v$GV),
                     sum(dauc8_l$GL,
                         dauc8_v$GV),
                     sum(dnac8_l$GL,
                         dnac8_v$GV),
                     sum(dore8_l$GL,
                         dore8_v$GV),
                     sum(dcue8_l$GL,
                         dcue8_v$GV),
                     sum(dgci8_l$GL,
                         dgci8_v$GV),
                     sum(dcum8_l$GL,
                         dcum8_v$GV),
                     sum(duca8_l$GL,
                         duca8_v$GV),
                     sum(dllo8_l$GL,
                         dllo8_v$GV),
                     sum(ddel8_l$GL,
                         ddel8_v$GV),
                     sum(dmus8_l$GL,
                         dmus8_v$GV),
                     sum(dgsc8_l$GL,
                         dgsc8_v$GV),
                     sum(dtun8_l$GL,
                         dtun8_v$GV)),
                 "GC"=
                   c(sum(dbsc8_l$GV,
                         dbsc8_v$GL),
                     sum(dldu8_l$GV,
                         dldu8_v$GL),
                     sum(didv8_l$GV,
                         didv8_v$GL),
                     sum(deme8_l$GV,
                         deme8_v$GL),
                     sum(dauc8_l$GV,
                         dauc8_v$GL),
                     sum(dnac8_l$GV,
                         dnac8_v$GL),
                     sum(dore8_l$GV,
                         dore8_v$GL),
                     sum(dcue8_l$GV,
                         dcue8_v$GL),
                     sum(dgci8_l$GV,
                         dgci8_v$GL),
                     sum(dcum8_l$GV,
                         dcum8_v$GL),
                     sum(duca8_l$GV,
                         duca8_v$GL),
                     sum(dllo8_l$GV,
                         dllo8_v$GL),
                     sum(ddel8_l$GV,
                         ddel8_v$GL),
                     sum(dmus8_l$GV,
                         dmus8_v$GL),
                     sum(dgsc8_l$GV,
                         dgsc8_v$GL),
                     sum(dtun8_l$GV,
                         dtun8_v$GL))
)

tj9 = data.frame("EQUIPOS" =
                   c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                     "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                     "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                     "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                 "PJ" =
                   c(NROW(dbsc9$Jornada), NROW(dldu9$Jornada), NROW(didv9$Jornada), NROW(deme9$Jornada),
                     NROW(dauc9$Jornada), NROW(dnac9$Jornada), NROW(dore9$Jornada), NROW(dcue9$Jornada),
                     NROW(dgci9$Jornada), NROW(dcum9$Jornada), NROW(duca9$Jornada), NROW(dllo9$Jornada),
                     NROW(ddel9$Jornada), NROW(dmus9$Jornada), NROW(dgsc9$Jornada), NROW(dtun9$Jornada)),
                 "PTOS"=
                   c(sum(dbsc9_l$VL*3,
                         dbsc9_v$VV*3,
                         dbsc9_l$EL*1,
                         dbsc9_v$EV*1),
                     sum(dldu9_l$VL*3,
                         dldu9_v$VV*3,
                         dldu9_l$EL*1,
                         dldu9_v$EV*1),
                     sum(didv9_l$VL*3,
                         didv9_v$VV*3,
                         didv9_l$EL*1,
                         didv9_v$EV*1),
                     sum(deme9_l$VL*3,
                         deme9_v$VV*3,
                         deme9_l$EL*1,
                         deme9_v$EV*1),
                     sum(dauc9_l$VL*3,
                         dauc9_v$VV*3,
                         dauc9_l$EL*1,
                         dauc9_v$EV*1),
                     sum(dnac9_l$VL*3,
                         dnac9_v$VV*3,
                         dnac9_l$EL*1,
                         dnac9_v$EV*1),
                     sum(dore9_l$VL*3,
                         dore9_v$VV*3,
                         dore9_l$EL*1,
                         dore9_v$EV*1),
                     sum(dcue9_l$VL*3,
                         dcue9_v$VV*3,
                         dcue9_l$EL*1,
                         dcue9_v$EV*1),
                     sum(dgci9_l$VL*3,
                         dgci9_v$VV*3,
                         dgci9_l$EL*1,
                         dgci9_v$EV*1),
                     sum(dcum9_l$VL*3,
                         dcum9_v$VV*3,
                         dcum9_l$EL*1,
                         dcum9_v$EV*1),
                     sum(duca9_l$VL*3,
                         duca9_v$VV*3,
                         duca9_l$EL*1,
                         duca9_v$EV*1),
                     sum(dllo9_l$VL*3,
                         dllo9_v$VV*3,
                         dllo9_l$EL*1,
                         dllo9_v$EV*1),
                     sum(ddel9_l$VL*3,
                         ddel9_v$VV*3,
                         ddel9_l$EL*1,
                         ddel9_v$EV*1),
                     sum(dmus9_l$VL*3,
                         dmus9_v$VV*3,
                         dmus9_l$EL*1,
                         dmus9_v$EV*1),
                     sum(dgsc9_l$VL*3,
                         dgsc9_v$VV*3,
                         dgsc9_l$EL*1,
                         dgsc9_v$EV*1),
                     sum(dtun9_l$VL*3,
                         dtun9_v$VV*3,
                         dtun9_l$EL*1,
                         dtun9_v$EV*1)),
                 "GF"=
                   c(sum(dbsc9_l$GL,
                         dbsc9_v$GV),
                     sum(dldu9_l$GL,
                         dldu9_v$GV),
                     sum(didv9_l$GL,
                         didv9_v$GV),
                     sum(deme9_l$GL,
                         deme9_v$GV),
                     sum(dauc9_l$GL,
                         dauc9_v$GV),
                     sum(dnac9_l$GL,
                         dnac9_v$GV),
                     sum(dore9_l$GL,
                         dore9_v$GV),
                     sum(dcue9_l$GL,
                         dcue9_v$GV),
                     sum(dgci9_l$GL,
                         dgci9_v$GV),
                     sum(dcum9_l$GL,
                         dcum9_v$GV),
                     sum(duca9_l$GL,
                         duca9_v$GV),
                     sum(dllo9_l$GL,
                         dllo9_v$GV),
                     sum(ddel9_l$GL,
                         ddel9_v$GV),
                     sum(dmus9_l$GL,
                         dmus9_v$GV),
                     sum(dgsc9_l$GL,
                         dgsc9_v$GV),
                     sum(dtun9_l$GL,
                         dtun9_v$GV)),
                 "GC"=
                   c(sum(dbsc9_l$GV,
                         dbsc9_v$GL),
                     sum(dldu9_l$GV,
                         dldu9_v$GL),
                     sum(didv9_l$GV,
                         didv9_v$GL),
                     sum(deme9_l$GV,
                         deme9_v$GL),
                     sum(dauc9_l$GV,
                         dauc9_v$GL),
                     sum(dnac9_l$GV,
                         dnac9_v$GL),
                     sum(dore9_l$GV,
                         dore9_v$GL),
                     sum(dcue9_l$GV,
                         dcue9_v$GL),
                     sum(dgci9_l$GV,
                         dgci9_v$GL),
                     sum(dcum9_l$GV,
                         dcum9_v$GL),
                     sum(duca9_l$GV,
                         duca9_v$GL),
                     sum(dllo9_l$GV,
                         dllo9_v$GL),
                     sum(ddel9_l$GV,
                         ddel9_v$GL),
                     sum(dmus9_l$GV,
                         dmus9_v$GL),
                     sum(dgsc9_l$GV,
                         dgsc9_v$GL),
                     sum(dtun9_l$GV,
                         dtun9_v$GL))
)

tj10 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc10$Jornada), NROW(dldu10$Jornada), NROW(didv10$Jornada), NROW(deme10$Jornada),
                      NROW(dauc10$Jornada), NROW(dnac10$Jornada), NROW(dore10$Jornada), NROW(dcue10$Jornada),
                      NROW(dgci10$Jornada), NROW(dcum10$Jornada), NROW(duca10$Jornada), NROW(dllo10$Jornada),
                      NROW(ddel10$Jornada), NROW(dmus10$Jornada), NROW(dgsc10$Jornada), NROW(dtun10$Jornada)),
                  "PTOS"=
                    c(sum(dbsc10_l$VL*3,
                          dbsc10_v$VV*3,
                          dbsc10_l$EL*1,
                          dbsc10_v$EV*1),
                      sum(dldu10_l$VL*3,
                          dldu10_v$VV*3,
                          dldu10_l$EL*1,
                          dldu10_v$EV*1),
                      sum(didv10_l$VL*3,
                          didv10_v$VV*3,
                          didv10_l$EL*1,
                          didv10_v$EV*1),
                      sum(deme10_l$VL*3,
                          deme10_v$VV*3,
                          deme10_l$EL*1,
                          deme10_v$EV*1),
                      sum(dauc10_l$VL*3,
                          dauc10_v$VV*3,
                          dauc10_l$EL*1,
                          dauc10_v$EV*1),
                      sum(dnac10_l$VL*3,
                          dnac10_v$VV*3,
                          dnac10_l$EL*1,
                          dnac10_v$EV*1),
                      sum(dore10_l$VL*3,
                          dore10_v$VV*3,
                          dore10_l$EL*1,
                          dore10_v$EV*1),
                      sum(dcue10_l$VL*3,
                          dcue10_v$VV*3,
                          dcue10_l$EL*1,
                          dcue10_v$EV*1),
                      sum(dgci10_l$VL*3,
                          dgci10_v$VV*3,
                          dgci10_l$EL*1,
                          dgci10_v$EV*1),
                      sum(dcum10_l$VL*3,
                          dcum10_v$VV*3,
                          dcum10_l$EL*1,
                          dcum10_v$EV*1),
                      sum(duca10_l$VL*3,
                          duca10_v$VV*3,
                          duca10_l$EL*1,
                          duca10_v$EV*1),
                      sum(dllo10_l$VL*3,
                          dllo10_v$VV*3,
                          dllo10_l$EL*1,
                          dllo10_v$EV*1),
                      sum(ddel10_l$VL*3,
                          ddel10_v$VV*3,
                          ddel10_l$EL*1,
                          ddel10_v$EV*1),
                      sum(dmus10_l$VL*3,
                          dmus10_v$VV*3,
                          dmus10_l$EL*1,
                          dmus10_v$EV*1),
                      sum(dgsc10_l$VL*3,
                          dgsc10_v$VV*3,
                          dgsc10_l$EL*1,
                          dgsc10_v$EV*1),
                      sum(dtun10_l$VL*3,
                          dtun10_v$VV*3,
                          dtun10_l$EL*1,
                          dtun10_v$EV*1)),
                  "GF"=
                    c(sum(dbsc10_l$GL,
                          dbsc10_v$GV),
                      sum(dldu10_l$GL,
                          dldu10_v$GV),
                      sum(didv10_l$GL,
                          didv10_v$GV),
                      sum(deme10_l$GL,
                          deme10_v$GV),
                      sum(dauc10_l$GL,
                          dauc10_v$GV),
                      sum(dnac10_l$GL,
                          dnac10_v$GV),
                      sum(dore10_l$GL,
                          dore10_v$GV),
                      sum(dcue10_l$GL,
                          dcue10_v$GV),
                      sum(dgci10_l$GL,
                          dgci10_v$GV),
                      sum(dcum10_l$GL,
                          dcum10_v$GV),
                      sum(duca10_l$GL,
                          duca10_v$GV),
                      sum(dllo10_l$GL,
                          dllo10_v$GV),
                      sum(ddel10_l$GL,
                          ddel10_v$GV),
                      sum(dmus10_l$GL,
                          dmus10_v$GV),
                      sum(dgsc10_l$GL,
                          dgsc10_v$GV),
                      sum(dtun10_l$GL,
                          dtun10_v$GV)),
                  "GC"=
                    c(sum(dbsc10_l$GV,
                          dbsc10_v$GL),
                      sum(dldu10_l$GV,
                          dldu10_v$GL),
                      sum(didv10_l$GV,
                          didv10_v$GL),
                      sum(deme10_l$GV,
                          deme10_v$GL),
                      sum(dauc10_l$GV,
                          dauc10_v$GL),
                      sum(dnac10_l$GV,
                          dnac10_v$GL),
                      sum(dore10_l$GV,
                          dore10_v$GL),
                      sum(dcue10_l$GV,
                          dcue10_v$GL),
                      sum(dgci10_l$GV,
                          dgci10_v$GL),
                      sum(dcum10_l$GV,
                          dcum10_v$GL),
                      sum(duca10_l$GV,
                          duca10_v$GL),
                      sum(dllo10_l$GV,
                          dllo10_v$GL),
                      sum(ddel10_l$GV,
                          ddel10_v$GL),
                      sum(dmus10_l$GV,
                          dmus10_v$GL),
                      sum(dgsc10_l$GV,
                          dgsc10_v$GL),
                      sum(dtun10_l$GV,
                          dtun10_v$GL))
)

tj11 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc11$Jornada), NROW(dldu11$Jornada), NROW(didv11$Jornada), NROW(deme11$Jornada),
                      NROW(dauc11$Jornada), NROW(dnac11$Jornada), NROW(dore11$Jornada), NROW(dcue11$Jornada),
                      NROW(dgci11$Jornada), NROW(dcum11$Jornada), NROW(duca11$Jornada), NROW(dllo11$Jornada),
                      NROW(ddel11$Jornada), NROW(dmus11$Jornada), NROW(dgsc11$Jornada), NROW(dtun11$Jornada)),
                  "PTOS"=
                    c(sum(dbsc11_l$VL*3,
                          dbsc11_v$VV*3,
                          dbsc11_l$EL*1,
                          dbsc11_v$EV*1),
                      sum(dldu11_l$VL*3,
                          dldu11_v$VV*3,
                          dldu11_l$EL*1,
                          dldu11_v$EV*1),
                      sum(didv11_l$VL*3,
                          didv11_v$VV*3,
                          didv11_l$EL*1,
                          didv11_v$EV*1),
                      sum(deme11_l$VL*3,
                          deme11_v$VV*3,
                          deme11_l$EL*1,
                          deme11_v$EV*1),
                      sum(dauc11_l$VL*3,
                          dauc11_v$VV*3,
                          dauc11_l$EL*1,
                          dauc11_v$EV*1),
                      sum(dnac11_l$VL*3,
                          dnac11_v$VV*3,
                          dnac11_l$EL*1,
                          dnac11_v$EV*1),
                      sum(dore11_l$VL*3,
                          dore11_v$VV*3,
                          dore11_l$EL*1,
                          dore11_v$EV*1),
                      sum(dcue11_l$VL*3,
                          dcue11_v$VV*3,
                          dcue11_l$EL*1,
                          dcue11_v$EV*1),
                      sum(dgci11_l$VL*3,
                          dgci11_v$VV*3,
                          dgci11_l$EL*1,
                          dgci11_v$EV*1),
                      sum(dcum11_l$VL*3,
                          dcum11_v$VV*3,
                          dcum11_l$EL*1,
                          dcum11_v$EV*1),
                      sum(duca11_l$VL*3,
                          duca11_v$VV*3,
                          duca11_l$EL*1,
                          duca11_v$EV*1),
                      sum(dllo11_l$VL*3,
                          dllo11_v$VV*3,
                          dllo11_l$EL*1,
                          dllo11_v$EV*1),
                      sum(ddel11_l$VL*3,
                          ddel11_v$VV*3,
                          ddel11_l$EL*1,
                          ddel11_v$EV*1),
                      sum(dmus11_l$VL*3,
                          dmus11_v$VV*3,
                          dmus11_l$EL*1,
                          dmus11_v$EV*1),
                      sum(dgsc11_l$VL*3,
                          dgsc11_v$VV*3,
                          dgsc11_l$EL*1,
                          dgsc11_v$EV*1),
                      sum(dtun11_l$VL*3,
                          dtun11_v$VV*3,
                          dtun11_l$EL*1,
                          dtun11_v$EV*1)),
                  "GF"=
                    c(sum(dbsc11_l$GL,
                          dbsc11_v$GV),
                      sum(dldu11_l$GL,
                          dldu11_v$GV),
                      sum(didv11_l$GL,
                          didv11_v$GV),
                      sum(deme11_l$GL,
                          deme11_v$GV),
                      sum(dauc11_l$GL,
                          dauc11_v$GV),
                      sum(dnac11_l$GL,
                          dnac11_v$GV),
                      sum(dore11_l$GL,
                          dore11_v$GV),
                      sum(dcue11_l$GL,
                          dcue11_v$GV),
                      sum(dgci11_l$GL,
                          dgci11_v$GV),
                      sum(dcum11_l$GL,
                          dcum11_v$GV),
                      sum(duca11_l$GL,
                          duca11_v$GV),
                      sum(dllo11_l$GL,
                          dllo11_v$GV),
                      sum(ddel11_l$GL,
                          ddel11_v$GV),
                      sum(dmus11_l$GL,
                          dmus11_v$GV),
                      sum(dgsc11_l$GL,
                          dgsc11_v$GV),
                      sum(dtun11_l$GL,
                          dtun11_v$GV)),
                  "GC"=
                    c(sum(dbsc11_l$GV,
                          dbsc11_v$GL),
                      sum(dldu11_l$GV,
                          dldu11_v$GL),
                      sum(didv11_l$GV,
                          didv11_v$GL),
                      sum(deme11_l$GV,
                          deme11_v$GL),
                      sum(dauc11_l$GV,
                          dauc11_v$GL),
                      sum(dnac11_l$GV,
                          dnac11_v$GL),
                      sum(dore11_l$GV,
                          dore11_v$GL),
                      sum(dcue11_l$GV,
                          dcue11_v$GL),
                      sum(dgci11_l$GV,
                          dgci11_v$GL),
                      sum(dcum11_l$GV,
                          dcum11_v$GL),
                      sum(duca11_l$GV,
                          duca11_v$GL),
                      sum(dllo11_l$GV,
                          dllo11_v$GL),
                      sum(ddel11_l$GV,
                          ddel11_v$GL),
                      sum(dmus11_l$GV,
                          dmus11_v$GL),
                      sum(dgsc11_l$GV,
                          dgsc11_v$GL),
                      sum(dtun11_l$GV,
                          dtun11_v$GL))
)

tj12 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc12$Jornada), NROW(dldu12$Jornada), NROW(didv12$Jornada), NROW(deme12$Jornada),
                      NROW(dauc12$Jornada), NROW(dnac12$Jornada), NROW(dore12$Jornada), NROW(dcue12$Jornada),
                      NROW(dgci12$Jornada), NROW(dcum12$Jornada), NROW(duca12$Jornada), NROW(dllo12$Jornada),
                      NROW(ddel12$Jornada), NROW(dmus12$Jornada), NROW(dgsc12$Jornada), NROW(dtun12$Jornada)),
                  "PTOS"=
                    c(sum(dbsc12_l$VL*3,
                          dbsc12_v$VV*3,
                          dbsc12_l$EL*1,
                          dbsc12_v$EV*1),
                      sum(dldu12_l$VL*3,
                          dldu12_v$VV*3,
                          dldu12_l$EL*1,
                          dldu12_v$EV*1),
                      sum(didv12_l$VL*3,
                          didv12_v$VV*3,
                          didv12_l$EL*1,
                          didv12_v$EV*1),
                      sum(deme12_l$VL*3,
                          deme12_v$VV*3,
                          deme12_l$EL*1,
                          deme12_v$EV*1),
                      sum(dauc12_l$VL*3,
                          dauc12_v$VV*3,
                          dauc12_l$EL*1,
                          dauc12_v$EV*1),
                      sum(dnac12_l$VL*3,
                          dnac12_v$VV*3,
                          dnac12_l$EL*1,
                          dnac12_v$EV*1),
                      sum(dore12_l$VL*3,
                          dore12_v$VV*3,
                          dore12_l$EL*1,
                          dore12_v$EV*1),
                      sum(dcue12_l$VL*3,
                          dcue12_v$VV*3,
                          dcue12_l$EL*1,
                          dcue12_v$EV*1),
                      sum(dgci12_l$VL*3,
                          dgci12_v$VV*3,
                          dgci12_l$EL*1,
                          dgci12_v$EV*1),
                      sum(dcum12_l$VL*3,
                          dcum12_v$VV*3,
                          dcum12_l$EL*1,
                          dcum12_v$EV*1),
                      sum(duca12_l$VL*3,
                          duca12_v$VV*3,
                          duca12_l$EL*1,
                          duca12_v$EV*1),
                      sum(dllo12_l$VL*3,
                          dllo12_v$VV*3,
                          dllo12_l$EL*1,
                          dllo12_v$EV*1),
                      sum(ddel12_l$VL*3,
                          ddel12_v$VV*3,
                          ddel12_l$EL*1,
                          ddel12_v$EV*1),
                      sum(dmus12_l$VL*3,
                          dmus12_v$VV*3,
                          dmus12_l$EL*1,
                          dmus12_v$EV*1),
                      sum(dgsc12_l$VL*3,
                          dgsc12_v$VV*3,
                          dgsc12_l$EL*1,
                          dgsc12_v$EV*1),
                      sum(dtun12_l$VL*3,
                          dtun12_v$VV*3,
                          dtun12_l$EL*1,
                          dtun12_v$EV*1)),
                  "GF"=
                    c(sum(dbsc12_l$GL,
                          dbsc12_v$GV),
                      sum(dldu12_l$GL,
                          dldu12_v$GV),
                      sum(didv12_l$GL,
                          didv12_v$GV),
                      sum(deme12_l$GL,
                          deme12_v$GV),
                      sum(dauc12_l$GL,
                          dauc12_v$GV),
                      sum(dnac12_l$GL,
                          dnac12_v$GV),
                      sum(dore12_l$GL,
                          dore12_v$GV),
                      sum(dcue12_l$GL,
                          dcue12_v$GV),
                      sum(dgci12_l$GL,
                          dgci12_v$GV),
                      sum(dcum12_l$GL,
                          dcum12_v$GV),
                      sum(duca12_l$GL,
                          duca12_v$GV),
                      sum(dllo12_l$GL,
                          dllo12_v$GV),
                      sum(ddel12_l$GL,
                          ddel12_v$GV),
                      sum(dmus12_l$GL,
                          dmus12_v$GV),
                      sum(dgsc12_l$GL,
                          dgsc12_v$GV),
                      sum(dtun12_l$GL,
                          dtun12_v$GV)),
                  "GC"=
                    c(sum(dbsc12_l$GV,
                          dbsc12_v$GL),
                      sum(dldu12_l$GV,
                          dldu12_v$GL),
                      sum(didv12_l$GV,
                          didv12_v$GL),
                      sum(deme12_l$GV,
                          deme12_v$GL),
                      sum(dauc12_l$GV,
                          dauc12_v$GL),
                      sum(dnac12_l$GV,
                          dnac12_v$GL),
                      sum(dore12_l$GV,
                          dore12_v$GL),
                      sum(dcue12_l$GV,
                          dcue12_v$GL),
                      sum(dgci12_l$GV,
                          dgci12_v$GL),
                      sum(dcum12_l$GV,
                          dcum12_v$GL),
                      sum(duca12_l$GV,
                          duca12_v$GL),
                      sum(dllo12_l$GV,
                          dllo12_v$GL),
                      sum(ddel12_l$GV,
                          ddel12_v$GL),
                      sum(dmus12_l$GV,
                          dmus12_v$GL),
                      sum(dgsc12_l$GV,
                          dgsc12_v$GL),
                      sum(dtun12_l$GV,
                          dtun12_v$GL))
)

tj13 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc13$Jornada), NROW(dldu13$Jornada), NROW(didv13$Jornada), NROW(deme13$Jornada),
                      NROW(dauc13$Jornada), NROW(dnac13$Jornada), NROW(dore13$Jornada), NROW(dcue13$Jornada),
                      NROW(dgci13$Jornada), NROW(dcum13$Jornada), NROW(duca13$Jornada), NROW(dllo13$Jornada),
                      NROW(ddel13$Jornada), NROW(dmus13$Jornada), NROW(dgsc13$Jornada), NROW(dtun13$Jornada)),
                  "PTOS"=
                    c(sum(dbsc13_l$VL*3,
                          dbsc13_v$VV*3,
                          dbsc13_l$EL*1,
                          dbsc13_v$EV*1),
                      sum(dldu13_l$VL*3,
                          dldu13_v$VV*3,
                          dldu13_l$EL*1,
                          dldu13_v$EV*1),
                      sum(didv13_l$VL*3,
                          didv13_v$VV*3,
                          didv13_l$EL*1,
                          didv13_v$EV*1),
                      sum(deme13_l$VL*3,
                          deme13_v$VV*3,
                          deme13_l$EL*1,
                          deme13_v$EV*1),
                      sum(dauc13_l$VL*3,
                          dauc13_v$VV*3,
                          dauc13_l$EL*1,
                          dauc13_v$EV*1),
                      sum(dnac13_l$VL*3,
                          dnac13_v$VV*3,
                          dnac13_l$EL*1,
                          dnac13_v$EV*1),
                      sum(dore13_l$VL*3,
                          dore13_v$VV*3,
                          dore13_l$EL*1,
                          dore13_v$EV*1),
                      sum(dcue13_l$VL*3,
                          dcue13_v$VV*3,
                          dcue13_l$EL*1,
                          dcue13_v$EV*1),
                      sum(dgci13_l$VL*3,
                          dgci13_v$VV*3,
                          dgci13_l$EL*1,
                          dgci13_v$EV*1),
                      sum(dcum13_l$VL*3,
                          dcum13_v$VV*3,
                          dcum13_l$EL*1,
                          dcum13_v$EV*1),
                      sum(duca13_l$VL*3,
                          duca13_v$VV*3,
                          duca13_l$EL*1,
                          duca13_v$EV*1),
                      sum(dllo13_l$VL*3,
                          dllo13_v$VV*3,
                          dllo13_l$EL*1,
                          dllo13_v$EV*1),
                      sum(ddel13_l$VL*3,
                          ddel13_v$VV*3,
                          ddel13_l$EL*1,
                          ddel13_v$EV*1),
                      sum(dmus13_l$VL*3,
                          dmus13_v$VV*3,
                          dmus13_l$EL*1,
                          dmus13_v$EV*1),
                      sum(dgsc13_l$VL*3,
                          dgsc13_v$VV*3,
                          dgsc13_l$EL*1,
                          dgsc13_v$EV*1),
                      sum(dtun13_l$VL*3,
                          dtun13_v$VV*3,
                          dtun13_l$EL*1,
                          dtun13_v$EV*1)),
                  "GF"=
                    c(sum(dbsc13_l$GL,
                          dbsc13_v$GV),
                      sum(dldu13_l$GL,
                          dldu13_v$GV),
                      sum(didv13_l$GL,
                          didv13_v$GV),
                      sum(deme13_l$GL,
                          deme13_v$GV),
                      sum(dauc13_l$GL,
                          dauc13_v$GV),
                      sum(dnac13_l$GL,
                          dnac13_v$GV),
                      sum(dore13_l$GL,
                          dore13_v$GV),
                      sum(dcue13_l$GL,
                          dcue13_v$GV),
                      sum(dgci13_l$GL,
                          dgci13_v$GV),
                      sum(dcum13_l$GL,
                          dcum13_v$GV),
                      sum(duca13_l$GL,
                          duca13_v$GV),
                      sum(dllo13_l$GL,
                          dllo13_v$GV),
                      sum(ddel13_l$GL,
                          ddel13_v$GV),
                      sum(dmus13_l$GL,
                          dmus13_v$GV),
                      sum(dgsc13_l$GL,
                          dgsc13_v$GV),
                      sum(dtun13_l$GL,
                          dtun13_v$GV)),
                  "GC"=
                    c(sum(dbsc13_l$GV,
                          dbsc13_v$GL),
                      sum(dldu13_l$GV,
                          dldu13_v$GL),
                      sum(didv13_l$GV,
                          didv13_v$GL),
                      sum(deme13_l$GV,
                          deme13_v$GL),
                      sum(dauc13_l$GV,
                          dauc13_v$GL),
                      sum(dnac13_l$GV,
                          dnac13_v$GL),
                      sum(dore13_l$GV,
                          dore13_v$GL),
                      sum(dcue13_l$GV,
                          dcue13_v$GL),
                      sum(dgci13_l$GV,
                          dgci13_v$GL),
                      sum(dcum13_l$GV,
                          dcum13_v$GL),
                      sum(duca13_l$GV,
                          duca13_v$GL),
                      sum(dllo13_l$GV,
                          dllo13_v$GL),
                      sum(ddel13_l$GV,
                          ddel13_v$GL),
                      sum(dmus13_l$GV,
                          dmus13_v$GL),
                      sum(dgsc13_l$GV,
                          dgsc13_v$GL),
                      sum(dtun13_l$GV,
                          dtun13_v$GL))
)

tj14 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc14$Jornada), NROW(dldu14$Jornada), NROW(didv14$Jornada), NROW(deme14$Jornada),
                      NROW(dauc14$Jornada), NROW(dnac14$Jornada), NROW(dore14$Jornada), NROW(dcue14$Jornada),
                      NROW(dgci14$Jornada), NROW(dcum14$Jornada), NROW(duca14$Jornada), NROW(dllo14$Jornada),
                      NROW(ddel14$Jornada), NROW(dmus14$Jornada), NROW(dgsc14$Jornada), NROW(dtun14$Jornada)),
                  "PTOS"=
                    c(sum(dbsc14_l$VL*3,
                          dbsc14_v$VV*3,
                          dbsc14_l$EL*1,
                          dbsc14_v$EV*1),
                      sum(dldu14_l$VL*3,
                          dldu14_v$VV*3,
                          dldu14_l$EL*1,
                          dldu14_v$EV*1),
                      sum(didv14_l$VL*3,
                          didv14_v$VV*3,
                          didv14_l$EL*1,
                          didv14_v$EV*1),
                      sum(deme14_l$VL*3,
                          deme14_v$VV*3,
                          deme14_l$EL*1,
                          deme14_v$EV*1),
                      sum(dauc14_l$VL*3,
                          dauc14_v$VV*3,
                          dauc14_l$EL*1,
                          dauc14_v$EV*1),
                      sum(dnac14_l$VL*3,
                          dnac14_v$VV*3,
                          dnac14_l$EL*1,
                          dnac14_v$EV*1),
                      sum(dore14_l$VL*3,
                          dore14_v$VV*3,
                          dore14_l$EL*1,
                          dore14_v$EV*1),
                      sum(dcue14_l$VL*3,
                          dcue14_v$VV*3,
                          dcue14_l$EL*1,
                          dcue14_v$EV*1),
                      sum(dgci14_l$VL*3,
                          dgci14_v$VV*3,
                          dgci14_l$EL*1,
                          dgci14_v$EV*1),
                      sum(dcum14_l$VL*3,
                          dcum14_v$VV*3,
                          dcum14_l$EL*1,
                          dcum14_v$EV*1),
                      sum(duca14_l$VL*3,
                          duca14_v$VV*3,
                          duca14_l$EL*1,
                          duca14_v$EV*1),
                      sum(dllo14_l$VL*3,
                          dllo14_v$VV*3,
                          dllo14_l$EL*1,
                          dllo14_v$EV*1),
                      sum(ddel14_l$VL*3,
                          ddel14_v$VV*3,
                          ddel14_l$EL*1,
                          ddel14_v$EV*1),
                      sum(dmus14_l$VL*3,
                          dmus14_v$VV*3,
                          dmus14_l$EL*1,
                          dmus14_v$EV*1),
                      sum(dgsc14_l$VL*3,
                          dgsc14_v$VV*3,
                          dgsc14_l$EL*1,
                          dgsc14_v$EV*1),
                      sum(dtun14_l$VL*3,
                          dtun14_v$VV*3,
                          dtun14_l$EL*1,
                          dtun14_v$EV*1)),
                  "GF"=
                    c(sum(dbsc14_l$GL,
                          dbsc14_v$GV),
                      sum(dldu14_l$GL,
                          dldu14_v$GV),
                      sum(didv14_l$GL,
                          didv14_v$GV),
                      sum(deme14_l$GL,
                          deme14_v$GV),
                      sum(dauc14_l$GL,
                          dauc14_v$GV),
                      sum(dnac14_l$GL,
                          dnac14_v$GV),
                      sum(dore14_l$GL,
                          dore14_v$GV),
                      sum(dcue14_l$GL,
                          dcue14_v$GV),
                      sum(dgci14_l$GL,
                          dgci14_v$GV),
                      sum(dcum14_l$GL,
                          dcum14_v$GV),
                      sum(duca14_l$GL,
                          duca14_v$GV),
                      sum(dllo14_l$GL,
                          dllo14_v$GV),
                      sum(ddel14_l$GL,
                          ddel14_v$GV),
                      sum(dmus14_l$GL,
                          dmus14_v$GV),
                      sum(dgsc14_l$GL,
                          dgsc14_v$GV),
                      sum(dtun14_l$GL,
                          dtun14_v$GV)),
                  "GC"=
                    c(sum(dbsc14_l$GV,
                          dbsc14_v$GL),
                      sum(dldu14_l$GV,
                          dldu14_v$GL),
                      sum(didv14_l$GV,
                          didv14_v$GL),
                      sum(deme14_l$GV,
                          deme14_v$GL),
                      sum(dauc14_l$GV,
                          dauc14_v$GL),
                      sum(dnac14_l$GV,
                          dnac14_v$GL),
                      sum(dore14_l$GV,
                          dore14_v$GL),
                      sum(dcue14_l$GV,
                          dcue14_v$GL),
                      sum(dgci14_l$GV,
                          dgci14_v$GL),
                      sum(dcum14_l$GV,
                          dcum14_v$GL),
                      sum(duca14_l$GV,
                          duca14_v$GL),
                      sum(dllo14_l$GV,
                          dllo14_v$GL),
                      sum(ddel14_l$GV,
                          ddel14_v$GL),
                      sum(dmus14_l$GV,
                          dmus14_v$GL),
                      sum(dgsc14_l$GV,
                          dgsc14_v$GL),
                      sum(dtun14_l$GV,
                          dtun14_v$GL))
)

tj15 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc15$Jornada), NROW(dldu15$Jornada), NROW(didv15$Jornada), NROW(deme15$Jornada),
                      NROW(dauc15$Jornada), NROW(dnac15$Jornada), NROW(dore15$Jornada), NROW(dcue15$Jornada),
                      NROW(dgci15$Jornada), NROW(dcum15$Jornada), NROW(duca15$Jornada), NROW(dllo15$Jornada),
                      NROW(ddel15$Jornada), NROW(dmus15$Jornada), NROW(dgsc15$Jornada), NROW(dtun15$Jornada)),
                  "PTOS"=
                    c(sum(dbsc15_l$VL*3,
                          dbsc15_v$VV*3,
                          dbsc15_l$EL*1,
                          dbsc15_v$EV*1),
                      sum(dldu15_l$VL*3,
                          dldu15_v$VV*3,
                          dldu15_l$EL*1,
                          dldu15_v$EV*1),
                      sum(didv15_l$VL*3,
                          didv15_v$VV*3,
                          didv15_l$EL*1,
                          didv15_v$EV*1),
                      sum(deme15_l$VL*3,
                          deme15_v$VV*3,
                          deme15_l$EL*1,
                          deme15_v$EV*1),
                      sum(dauc15_l$VL*3,
                          dauc15_v$VV*3,
                          dauc15_l$EL*1,
                          dauc15_v$EV*1),
                      sum(dnac15_l$VL*3,
                          dnac15_v$VV*3,
                          dnac15_l$EL*1,
                          dnac15_v$EV*1),
                      sum(dore15_l$VL*3,
                          dore15_v$VV*3,
                          dore15_l$EL*1,
                          dore15_v$EV*1),
                      sum(dcue15_l$VL*3,
                          dcue15_v$VV*3,
                          dcue15_l$EL*1,
                          dcue15_v$EV*1),
                      sum(dgci15_l$VL*3,
                          dgci15_v$VV*3,
                          dgci15_l$EL*1,
                          dgci15_v$EV*1),
                      sum(dcum15_l$VL*3,
                          dcum15_v$VV*3,
                          dcum15_l$EL*1,
                          dcum15_v$EV*1),
                      sum(duca15_l$VL*3,
                          duca15_v$VV*3,
                          duca15_l$EL*1,
                          duca15_v$EV*1),
                      sum(dllo15_l$VL*3,
                          dllo15_v$VV*3,
                          dllo15_l$EL*1,
                          dllo15_v$EV*1),
                      sum(ddel15_l$VL*3,
                          ddel15_v$VV*3,
                          ddel15_l$EL*1,
                          ddel15_v$EV*1),
                      sum(dmus15_l$VL*3,
                          dmus15_v$VV*3,
                          dmus15_l$EL*1,
                          dmus15_v$EV*1),
                      sum(dgsc15_l$VL*3,
                          dgsc15_v$VV*3,
                          dgsc15_l$EL*1,
                          dgsc15_v$EV*1),
                      sum(dtun15_l$VL*3,
                          dtun15_v$VV*3,
                          dtun15_l$EL*1,
                          dtun15_v$EV*1)),
                  "GF"=
                    c(sum(dbsc15_l$GL,
                          dbsc15_v$GV),
                      sum(dldu15_l$GL,
                          dldu15_v$GV),
                      sum(didv15_l$GL,
                          didv15_v$GV),
                      sum(deme15_l$GL,
                          deme15_v$GV),
                      sum(dauc15_l$GL,
                          dauc15_v$GV),
                      sum(dnac15_l$GL,
                          dnac15_v$GV),
                      sum(dore15_l$GL,
                          dore15_v$GV),
                      sum(dcue15_l$GL,
                          dcue15_v$GV),
                      sum(dgci15_l$GL,
                          dgci15_v$GV),
                      sum(dcum15_l$GL,
                          dcum15_v$GV),
                      sum(duca15_l$GL,
                          duca15_v$GV),
                      sum(dllo15_l$GL,
                          dllo15_v$GV),
                      sum(ddel15_l$GL,
                          ddel15_v$GV),
                      sum(dmus15_l$GL,
                          dmus15_v$GV),
                      sum(dgsc15_l$GL,
                          dgsc15_v$GV),
                      sum(dtun15_l$GL,
                          dtun15_v$GV)),
                  "GC"=
                    c(sum(dbsc15_l$GV,
                          dbsc15_v$GL),
                      sum(dldu15_l$GV,
                          dldu15_v$GL),
                      sum(didv15_l$GV,
                          didv15_v$GL),
                      sum(deme15_l$GV,
                          deme15_v$GL),
                      sum(dauc15_l$GV,
                          dauc15_v$GL),
                      sum(dnac15_l$GV,
                          dnac15_v$GL),
                      sum(dore15_l$GV,
                          dore15_v$GL),
                      sum(dcue15_l$GV,
                          dcue15_v$GL),
                      sum(dgci15_l$GV,
                          dgci15_v$GL),
                      sum(dcum15_l$GV,
                          dcum15_v$GL),
                      sum(duca15_l$GV,
                          duca15_v$GL),
                      sum(dllo15_l$GV,
                          dllo15_v$GL),
                      sum(ddel15_l$GV,
                          ddel15_v$GL),
                      sum(dmus15_l$GV,
                          dmus15_v$GL),
                      sum(dgsc15_l$GV,
                          dgsc15_v$GL),
                      sum(dtun15_l$GV,
                          dtun15_v$GL))
)

tj16 = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc16$Jornada), NROW(dldu16$Jornada), NROW(didv16$Jornada), NROW(deme16$Jornada),
                      NROW(dauc16$Jornada), NROW(dnac16$Jornada), NROW(dore16$Jornada), NROW(dcue16$Jornada),
                      NROW(dgci16$Jornada), NROW(dcum16$Jornada), NROW(duca16$Jornada), NROW(dllo16$Jornada),
                      NROW(ddel16$Jornada), NROW(dmus16$Jornada), NROW(dgsc16$Jornada), NROW(dtun16$Jornada)),
                  "PTOS"=
                    c(sum(dbsc16_l$VL*3,
                          dbsc16_v$VV*3,
                          dbsc16_l$EL*1,
                          dbsc16_v$EV*1),
                      sum(dldu16_l$VL*3,
                          dldu16_v$VV*3,
                          dldu16_l$EL*1,
                          dldu16_v$EV*1),
                      sum(didv16_l$VL*3,
                          didv16_v$VV*3,
                          didv16_l$EL*1,
                          didv16_v$EV*1),
                      sum(deme16_l$VL*3,
                          deme16_v$VV*3,
                          deme16_l$EL*1,
                          deme16_v$EV*1),
                      sum(dauc16_l$VL*3,
                          dauc16_v$VV*3,
                          dauc16_l$EL*1,
                          dauc16_v$EV*1),
                      sum(dnac16_l$VL*3,
                          dnac16_v$VV*3,
                          dnac16_l$EL*1,
                          dnac16_v$EV*1),
                      sum(dore16_l$VL*3,
                          dore16_v$VV*3,
                          dore16_l$EL*1,
                          dore16_v$EV*1),
                      sum(dcue16_l$VL*3,
                          dcue16_v$VV*3,
                          dcue16_l$EL*1,
                          dcue16_v$EV*1),
                      sum(dgci16_l$VL*3,
                          dgci16_v$VV*3,
                          dgci16_l$EL*1,
                          dgci16_v$EV*1),
                      sum(dcum16_l$VL*3,
                          dcum16_v$VV*3,
                          dcum16_l$EL*1,
                          dcum16_v$EV*1),
                      sum(duca16_l$VL*3,
                          duca16_v$VV*3,
                          duca16_l$EL*1,
                          duca16_v$EV*1),
                      sum(dllo16_l$VL*3,
                          dllo16_v$VV*3,
                          dllo16_l$EL*1,
                          dllo16_v$EV*1),
                      sum(ddel16_l$VL*3,
                          ddel16_v$VV*3,
                          ddel16_l$EL*1,
                          ddel16_v$EV*1),
                      sum(dmus16_l$VL*3,
                          dmus16_v$VV*3,
                          dmus16_l$EL*1,
                          dmus16_v$EV*1),
                      sum(dgsc16_l$VL*3,
                          dgsc16_v$VV*3,
                          dgsc16_l$EL*1,
                          dgsc16_v$EV*1),
                      sum(dtun16_l$VL*3,
                          dtun16_v$VV*3,
                          dtun16_l$EL*1,
                          dtun16_v$EV*1)),
                  "GF"=
                    c(sum(dbsc16_l$GL,
                          dbsc16_v$GV),
                      sum(dldu16_l$GL,
                          dldu16_v$GV),
                      sum(didv16_l$GL,
                          didv16_v$GV),
                      sum(deme16_l$GL,
                          deme16_v$GV),
                      sum(dauc16_l$GL,
                          dauc16_v$GV),
                      sum(dnac16_l$GL,
                          dnac16_v$GV),
                      sum(dore16_l$GL,
                          dore16_v$GV),
                      sum(dcue16_l$GL,
                          dcue16_v$GV),
                      sum(dgci16_l$GL,
                          dgci16_v$GV),
                      sum(dcum16_l$GL,
                          dcum16_v$GV),
                      sum(duca16_l$GL,
                          duca16_v$GV),
                      sum(dllo16_l$GL,
                          dllo16_v$GV),
                      sum(ddel16_l$GL,
                          ddel16_v$GV),
                      sum(dmus16_l$GL,
                          dmus16_v$GV),
                      sum(dgsc16_l$GL,
                          dgsc16_v$GV),
                      sum(dtun16_l$GL,
                          dtun16_v$GV)),
                  "GC"=
                    c(sum(dbsc16_l$GV,
                          dbsc16_v$GL),
                      sum(dldu16_l$GV,
                          dldu16_v$GL),
                      sum(didv16_l$GV,
                          didv16_v$GL),
                      sum(deme16_l$GV,
                          deme16_v$GL),
                      sum(dauc16_l$GV,
                          dauc16_v$GL),
                      sum(dnac16_l$GV,
                          dnac16_v$GL),
                      sum(dore16_l$GV,
                          dore16_v$GL),
                      sum(dcue16_l$GV,
                          dcue16_v$GL),
                      sum(dgci16_l$GV,
                          dgci16_v$GL),
                      sum(dcum16_l$GV,
                          dcum16_v$GL),
                      sum(duca16_l$GV,
                          duca16_v$GL),
                      sum(dllo16_l$GV,
                          dllo16_v$GL),
                      sum(ddel16_l$GV,
                          ddel16_v$GL),
                      sum(dmus16_l$GV,
                          dmus16_v$GL),
                      sum(dgsc16_l$GV,
                          dgsc16_v$GL),
                      sum(dtun16_l$GV,
                          dtun16_v$GL))
)

tjlv = data.frame("EQUIPOS" =
                    c("Barcelona SC", "LDU Quito", "Independiente del Valle", "Emelec",
                      "Aucas", "Nacional", "Orense SC", "Deportivo Cuenca",
                      "Guayaquil City", "Cumbayá FC", "Universidad Católica", "Libertad de Loja",
                      "Delfín SC", "Mushuc Runa", "Gualaceo SC", "Técnico Universitario"),
                  "PJ" =
                    c(NROW(dbsc15$Jornada), NROW(dldu15$Jornada), NROW(didv15$Jornada), NROW(deme15$Jornada),
                      NROW(dauc15$Jornada), NROW(dnac15$Jornada), NROW(dore15$Jornada), NROW(dcue15$Jornada),
                      NROW(dgci15$Jornada), NROW(dcum15$Jornada), NROW(duca15$Jornada), NROW(dllo15$Jornada),
                      NROW(ddel15$Jornada), NROW(dmus15$Jornada), NROW(dgsc15$Jornada), NROW(dtun15$Jornada)),
                  "VL"=
                    c(sum(dbsc15_l$VL), sum(dldu15_l$VL), sum(didv15_l$VL), sum(deme15_l$VL),
                      sum(dauc15_l$VL), sum(dnac15_l$VL), sum(dore15_l$VL), sum(dcue15_l$VL),
                      sum(dgci15_l$VL), sum(dcum15_l$VL), sum(duca15_l$VL), sum(dllo15_l$VL),
                      sum(ddel15_l$VL), sum(dmus15_l$VL), sum(dgsc15_l$VL), sum(dtun15_l$VL)),
                  "EL"=
                    c(sum(dbsc15_l$EL), sum(dldu15_l$EL), sum(didv15_l$EL), sum(deme15_l$EL),
                      sum(dauc15_l$EL), sum(dnac15_l$EL), sum(dore15_l$EL), sum(dcue15_l$EL),
                      sum(dgci15_l$EL), sum(dcum15_l$EL), sum(duca15_l$EL), sum(dllo15_l$EL),
                      sum(ddel15_l$EL), sum(dmus15_l$EL), sum(dgsc15_l$EL), sum(dtun15_l$EL)),
                  "DL"=
                    c(sum(dbsc15_l$DL), sum(dldu15_l$DL), sum(didv15_l$DL), sum(deme15_l$DL),
                      sum(dauc15_l$DL), sum(dnac15_l$DL), sum(dore15_l$DL), sum(dcue15_l$DL),
                      sum(dgci15_l$DL), sum(dcum15_l$DL), sum(duca15_l$DL), sum(dllo15_l$DL),
                      sum(ddel15_l$DL), sum(dmus15_l$DL), sum(dgsc15_l$DL), sum(dtun15_l$DL)),
                  "VV"=
                    c(sum(dbsc15_v$VV), sum(dldu15_v$VV), sum(didv15_v$VV), sum(deme15_v$VV),
                      sum(dauc15_v$VV), sum(dnac15_v$VV), sum(dore15_v$VV), sum(dcue15_v$VV),
                      sum(dgci15_v$VV), sum(dcum15_v$VV), sum(duca15_v$VV), sum(dllo15_v$VV),
                      sum(ddel15_v$VV), sum(dmus15_v$VV), sum(dgsc15_v$VV), sum(dtun15_v$VV)),
                  "EV"=
                    c(sum(dbsc15_v$EV), sum(dldu15_v$EV), sum(didv15_v$EV), sum(deme15_v$EV),
                      sum(dauc15_v$EV), sum(dnac15_v$EV), sum(dore15_v$EV), sum(dcue15_v$EV),
                      sum(dgci15_v$EV), sum(dcum15_v$EV), sum(duca15_v$EV), sum(dllo15_v$EV),
                      sum(ddel15_v$EV), sum(dmus15_v$EV), sum(dgsc15_v$EV), sum(dtun15_v$EV)),
                  "DV"=
                    c(sum(dbsc15_v$DV), sum(dldu15_v$DV), sum(didv15_v$DV), sum(deme15_v$DV),
                      sum(dauc15_v$DV), sum(dnac15_v$DV), sum(dore15_v$DV), sum(dcue15_v$DV),
                      sum(dgci15_v$DV), sum(dcum15_v$DV), sum(duca15_v$DV), sum(dllo15_v$DV),
                      sum(ddel15_v$DV), sum(dmus15_v$DV), sum(dgsc15_v$DV), sum(dtun15_v$DV))
)

# NO. FILAS EN ACUMULADAS
rownames(tj1) <- 1:nrow(tj1)
rownames(tj2) <- 1:nrow(tj2)
rownames(tj3) <- 1:nrow(tj3)
rownames(tj4) <- 1:nrow(tj4)
rownames(tj5) <- 1:nrow(tj5)
rownames(tj6) <- 1:nrow(tj6)
rownames(tj7) <- 1:nrow(tj7)
rownames(tj8) <- 1:nrow(tj8)
rownames(tj9) <- 1:nrow(tj9)
rownames(tj10) <- 1:nrow(tj10)
rownames(tj11) <- 1:nrow(tj11)
rownames(tj12) <- 1:nrow(tj12)
rownames(tj13) <- 1:nrow(tj13)
rownames(tj14) <- 1:nrow(tj14)
rownames(tj15) <- 1:nrow(tj15)
rownames(tj16) <- 1:nrow(tj16)

# ACUMULADAS - MUTATE "GD"
tj1 <- mutate(tj1, GD = GF - GC)
tj2 <- mutate(tj2, GD = GF - GC)
tj3 <- mutate(tj3, GD = GF - GC)
tj4 <- mutate(tj4, GD = GF - GC)
tj5 <- mutate(tj5, GD = GF - GC)
tj6 <- mutate(tj6, GD = GF - GC)
tj7 <- mutate(tj7, GD = GF - GC)
tj8 <- mutate(tj8, GD = GF - GC)
tj9 <- mutate(tj9, GD = GF - GC)
tj10 <- mutate(tj10, GD = GF - GC)
tj11 <- mutate(tj11, GD = GF - GC)
tj12 <- mutate(tj12, GD = GF - GC)
tj13 <- mutate(tj13, GD = GF - GC)
tj14 <- mutate(tj14, GD = GF - GC)
tj15 <- mutate(tj15, GD = GF - GC)
tj16 <- mutate(tj16, GD = GF - GC)

# ACUMULADAS - ORDENAR: PTOS-GD-GF 
# ACUMULADAS - SELECT: EQUIPOS-PTOS-GD
# ACUMULADAS - MUTATE: NO. POSICION
tj1 <- tj1[order(-tj1$PTOS, -tj1$GD, -tj1$GF, tj1$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J1 = rownames(tj1))
tj2 <- tj2[order(-tj2$PTOS, -tj2$GD, -tj2$GF, tj2$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J2 = rownames(tj2))
tj3 <- tj3[order(-tj3$PTOS, -tj3$GD, -tj3$GF, tj3$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J3 = rownames(tj3))
tj4 <- tj4[order(-tj4$PTOS, -tj4$GD, -tj4$GF, tj4$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J4 = rownames(tj4))
tj5 <- tj5[order(-tj5$PTOS, -tj5$GD, -tj5$GF, tj5$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J5 = rownames(tj5))
tj6 <- tj6[order(-tj6$PTOS, -tj6$GD, -tj6$GF, tj6$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J6 = rownames(tj6))
tj7 <- tj7[order(-tj7$PTOS, -tj7$GD, -tj7$GF, tj7$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J7 = rownames(tj7))
tj8 <- tj8[order(-tj8$PTOS, -tj8$GD, -tj8$GF, tj8$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J8 = rownames(tj8))
tj9 <- tj9[order(-tj9$PTOS, -tj9$GD, -tj9$GF, tj9$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J9 = rownames(tj9))
tj10 <- tj10[order(-tj10$PTOS, -tj10$GD, -tj10$GF, tj10$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J10 = rownames(tj10))
tj11 <- tj11[order(-tj11$PTOS, -tj11$GD, -tj11$GF, tj11$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J11 = rownames(tj11))
tj12 <- tj12[order(-tj12$PTOS, -tj12$GD, -tj12$GF, tj12$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J12 = rownames(tj12))
tj13 <- tj13[order(-tj13$PTOS, -tj13$GD, -tj13$GF, tj13$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J13 = rownames(tj13))
tj14 <- tj14[order(-tj14$PTOS, -tj14$GD, -tj14$GF, tj14$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J14 = rownames(tj14))
tj15 <- tj15[order(-tj15$PTOS, -tj15$GD, -tj15$GF, tj15$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J15 = rownames(tj15))
tj16 <- tj16[order(-tj16$PTOS, -tj16$GD, -tj16$GF, tj16$EQUIPOS), ] %>%
  select(EQUIPOS, PTOS, GD) %>%
  mutate(J16 = rownames(tj16))

# ORDENAR TABLAS ACUMULADAS DE JORNADAS
tja = full_join(tj1, tj2, by = "EQUIPOS")
tjb = full_join(tja, tj3, by = "EQUIPOS")
tjc = full_join(tjb, tj4, by = "EQUIPOS")
tjd = full_join(tjc, tj5, by = "EQUIPOS")
tje = full_join(tjd, tj6, by = "EQUIPOS")
tjf = full_join(tje, tj7, by = "EQUIPOS")
tjg = full_join(tjf, tj8, by = "EQUIPOS")
tjh = full_join(tjg, tj9, by = "EQUIPOS")
tji = full_join(tjh, tj10, by = "EQUIPOS")
tjj = full_join(tji, tj11, by = "EQUIPOS")
tjk = full_join(tjj, tj12, by = "EQUIPOS")
tjl = full_join(tjk, tj13, by = "EQUIPOS")
tjm = full_join(tjl, tj14, by = "EQUIPOS")
tjn = full_join(tjm, tj15, by = "EQUIPOS")
tjo = full_join(tjn, tj16, by = "EQUIPOS")
tjx = select(tjo, "EQUIPOS", 
             "J1", "J2", "J3", "J4", "J5", "J6","J7", "J8", "J9", "J10", "J11", "J12", "J13", "J14", "J15",
             "J16")
tjx <- gather(tjx,
              key = "variable",
              value = "value",
              J1:J16)

# SAVE "tjx" data.frame as txt file
write.table(tjx,"jornadas/tj1_15.txt",sep="\t",row.names=FALSE)
#write.table(tjx,"jornadas/tj1_16.txt",sep="\t",row.names=FALSE)


# GRÁFICO BUMP CHART
# FUNCIÓN COLORES PARA GGPLOT
my_theme <- function() {
  # Colors
  color.background = "grey"
  color.text = "#22211d"
  # Begin construction of chart
  theme_bw(base_size=15) +
    # Format background colors
    theme(panel.background = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background,
                                          color=color.background)) +
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    # Format the legend
    theme(legend.position = "right") +
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold",
                                          vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5,
                                          color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# CONVERSIÓN A NÚMERO DE VARIABLE "value"
tjx$value <- as.numeric(tjx$value)

# GRÁFICO
ggplot(tjx, aes(x = variable, y = value, group = EQUIPOS)) +
  geom_line(aes(color = EQUIPOS, alpha = 1), size = 2) +
  geom_point(aes(color = EQUIPOS, alpha = 1), size = 6) +
  scale_y_reverse(breaks = 1:nrow(tjx)) +
  aes(x = fct_inorder(variable)) +
  #scale_x_discrete(breaks = 1:3) +
  #  theme(legend.position = "TRUE") +
  #  theme_minimal() +
  #  theme(panel.grid.major = element_blank(),
  #        panel.grid.minor = element_blank()) + 
  labs(title = "CAMPEONATO ECUATORIANO DE FÚTBOL 2023",
       subtitle = "Comportamiento por Jornada de los Equipos",
       caption = "powered by Udaviz",
       x = "Jornadas de Campeonato 1 - 16",
       y = "Posiciones Tabla Acumulada") +
  my_theme() +
  scale_color_manual(values = c("#FFBF00", "#FFFF00", "#663399", "#2297E6",
                                "#D83B01", "#8F00FF", "#61D04F", "#42F9F9",
                                "#EF008C", "#F3F3F3", "#B5DAFE", "#009292",
                                "#2F8AC3", "#026645", "#D82C20", "#00009C"))



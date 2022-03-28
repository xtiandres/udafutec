# ELIMINATORIAS CONMEBOL RUMBO QATAR JORNADA 1

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
dateq17 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq17.txt")

# ARGENTINA
#datarg <- filter(dateq1,
#                 Local == "Argentina" | Visita == "Argentina")
datarg <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada == 1)
datarg_l <- datarg %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg_v <- datarg %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg2 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2))
datarg2_l <- datarg2 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg2_v <- datarg2 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg3 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3))
datarg3_l <- datarg3 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg3_v <- datarg3 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg4 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datarg4_l <- datarg4 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg4_v <- datarg4 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg5 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datarg5_l <- datarg5 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg5_v <- datarg5 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg6 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datarg6_l <- datarg6 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg6_v <- datarg6 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg7 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datarg7_l <- datarg7 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg7_v <- datarg7 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg8 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datarg8_l <- datarg8 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg8_v <- datarg8 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg9 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datarg9_l <- datarg9 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg9_v <- datarg9 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg10 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datarg10_l <- datarg10 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg10_v <- datarg10 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg11 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datarg11_l <- datarg11 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg11_v <- datarg11 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg12 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datarg12_l <- datarg12 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg12_v <- datarg12 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg13 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datarg13_l <- datarg13 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg13_v <- datarg13 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg14 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datarg14_l <- datarg14 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg14_v <- datarg14 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg15 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datarg15_l <- datarg15 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg15_v <- datarg15 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg16 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datarg16_l <- datarg16 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg16_v <- datarg16 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datarg17 <- dateq17 %>% 
  filter(Local == "Argentina" | Visita == "Argentina") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datarg17_l <- datarg17 %>%
  filter(Local == "Argentina") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datarg17_v <- datarg17 %>%
  filter(Visita == "Argentina") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datarg18 <- dateq17 %>% 
#  filter(Local == "Argentina" | Visita == "Argentina") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datarg18_l <- datarg18 %>%
#  filter(Local == "Argentina") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datarg18_v <- datarg18 %>%
#  filter(Visita == "Argentina") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# BOLIVIA
datbol <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada == 1)
datbol_l <- datbol %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol_v <- datbol %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol2 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2))
datbol2_l <- datbol2 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol2_v <- datbol2 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol3 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3))
datbol3_l <- datbol3 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol3_v <- datbol3 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol4 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datbol4_l <- datbol4 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol4_v <- datbol4 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol5 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datbol5_l <- datbol5 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol5_v <- datbol5 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol6 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datbol6_l <- datbol6 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol6_v <- datbol6 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol7 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datbol7_l <- datbol7 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol7_v <- datbol7 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol8 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datbol8_l <- datbol8 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol8_v <- datbol8 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol9 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datbol9_l <- datbol9 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol9_v <- datbol9 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol10 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datbol10_l <- datbol10 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol10_v <- datbol10 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol11 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datbol11_l <- datbol11 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol11_v <- datbol11 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol12 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datbol12_l <- datbol12 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol12_v <- datbol12 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol13 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datbol13_l <- datbol13 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol13_v <- datbol13 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol14 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datbol14_l <- datbol14 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol14_v <- datbol14 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol15 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datbol15_l <- datbol15 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol15_v <- datbol15 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol16 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datbol16_l <- datbol16 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol16_v <- datbol16 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbol17 <- dateq17 %>% 
  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datbol17_l <- datbol17 %>%
  filter(Local == "Bolivia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbol17_v <- datbol17 %>%
  filter(Visita == "Bolivia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datbol18 <- dateq17 %>% 
#  filter(Local == "Bolivia" | Visita == "Bolivia") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datbol18_l <- datbol18 %>%
#  filter(Local == "Bolivia") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datbol18_v <- datbol18 %>%
#  filter(Visita == "Bolivia") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# BRASIL
datbra <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada == 1)
datbra_l <- datbra %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra_v <- datbra %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra2 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2))
datbra2_l <- datbra2 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra2_v <- datbra2 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra3 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3))
datbra3_l <- datbra3 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra3_v <- datbra3 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra4 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datbra4_l <- datbra4 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra4_v <- datbra4 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra5 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datbra5_l <- datbra5 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra5_v <- datbra5 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra6 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datbra6_l <- datbra6 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra6_v <- datbra6 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra7 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datbra7_l <- datbra7 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra7_v <- datbra7 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra8 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datbra8_l <- datbra8 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra8_v <- datbra8 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra9 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datbra9_l <- datbra9 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra9_v <- datbra9 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra10 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datbra10_l <- datbra10 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra10_v <- datbra10 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra11 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datbra11_l <- datbra11 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra11_v <- datbra11 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra12 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datbra12_l <- datbra12 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra12_v <- datbra12 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra13 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datbra13_l <- datbra13 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra13_v <- datbra13 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra14 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datbra14_l <- datbra14 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra14_v <- datbra14 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra15 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datbra15_l <- datbra15 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra15_v <- datbra15 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra16 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datbra16_l <- datbra16 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra16_v <- datbra16 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datbra17 <- dateq17 %>% 
  filter(Local == "Brasil" | Visita == "Brasil") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datbra17_l <- datbra17 %>%
  filter(Local == "Brasil") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datbra17_v <- datbra17 %>%
  filter(Visita == "Brasil") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datbra18 <- dateq17 %>% 
#  filter(Local == "Brasil" | Visita == "Brasil") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datbra18_l <- datbra18 %>%
#  filter(Local == "Brasil") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datbra18_v <- datbra18 %>%
#  filter(Visita == "Brasil") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# COLOMBIA
datcol <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada == 1)
datcol_l <- datcol %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol_v <- datcol %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol2 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2))
datcol2_l <- datcol2 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol2_v <- datcol2 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol3 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3))
datcol3_l <- datcol3 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol3_v <- datcol3 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol4 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datcol4_l <- datcol4 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol4_v <- datcol4 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol5 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datcol5_l <- datcol5 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol5_v <- datcol5 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol6 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datcol6_l <- datcol6 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol6_v <- datcol6 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol7 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datcol7_l <- datcol7 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol7_v <- datcol7 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol8 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datcol8_l <- datcol8 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol8_v <- datcol8 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol9 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datcol9_l <- datcol9 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol9_v <- datcol9 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol10 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datcol10_l <- datcol10 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol10_v <- datcol10 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol11 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datcol11_l <- datcol11 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol11_v <- datcol11 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol12 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datcol12_l <- datcol12 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol12_v <- datcol12 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol13 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datcol13_l <- datcol13 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol13_v <- datcol13 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol14 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datcol14_l <- datcol14 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol14_v <- datcol14 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol15 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datcol15_l <- datcol15 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol15_v <- datcol15 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol16 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datcol16_l <- datcol16 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol16_v <- datcol16 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datcol17 <- dateq17 %>% 
  filter(Local == "Colombia" | Visita == "Colombia") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datcol17_l <- datcol17 %>%
  filter(Local == "Colombia") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datcol17_v <- datcol17 %>%
  filter(Visita == "Colombia") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datcol18 <- dateq17 %>% 
#  filter(Local == "Colombia" | Visita == "Colombia") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18))
#datcol18_l <- datcol18 %>%
#  filter(Local == "Colombia") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datcol18_v <- datcol18 %>%
#  filter(Visita == "Colombia") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# CHILE
datchi <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada == 1)
datchi_l <- datchi %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi_v <- datchi %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi2 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2))
datchi2_l <- datchi2 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi2_v <- datchi2 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi3 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3))
datchi3_l <- datchi3 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi3_v <- datchi3 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi4 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datchi4_l <- datchi4 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi4_v <- datchi4 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi5 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datchi5_l <- datchi5 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi5_v <- datchi5 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi6 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datchi6_l <- datchi6 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi6_v <- datchi6 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi7 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datchi7_l <- datchi7 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi7_v <- datchi7 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi8 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datchi8_l <- datchi8 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi8_v <- datchi8 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi9 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datchi9_l <- datchi9 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi9_v <- datchi9 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi10 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datchi10_l <- datchi10 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi10_v <- datchi10 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi11 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datchi11_l <- datchi11 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi11_v <- datchi11 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi12 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datchi12_l <- datchi12 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi12_v <- datchi12 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi13 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datchi13_l <- datchi13 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi13_v <- datchi13 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi14 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datchi14_l <- datchi14 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi14_v <- datchi14 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi15 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datchi15_l <- datchi15 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi15_v <- datchi15 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi16 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datchi16_l <- datchi16 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi16_v <- datchi16 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datchi17 <- dateq17 %>% 
  filter(Local == "Chile" | Visita == "Chile") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datchi17_l <- datchi17 %>%
  filter(Local == "Chile") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datchi17_v <- datchi17 %>%
  filter(Visita == "Chile") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datchi18 <- dateq17 %>% 
#  filter(Local == "Chile" | Visita == "Chile") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datchi18_l <- datchi18 %>%
#  filter(Local == "Chile") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datchi18_v <- datchi18 %>%
#  filter(Visita == "Chile") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# ECUADOR
datecu <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada == 1)
datecu_l <- datecu %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu_v <- datecu %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu2 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2))
datecu2_l <- datecu2 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu2_v <- datecu2 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu3 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3))
datecu3_l <- datecu3 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu3_v <- datecu3 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu4 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datecu4_l <- datecu4 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu4_v <- datecu4 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu5 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datecu5_l <- datecu5 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu5_v <- datecu5 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu6 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datecu6_l <- datecu6 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu6_v <- datecu6 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu7 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datecu7_l <- datecu7 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu7_v <- datecu7 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu8 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datecu8_l <- datecu8 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu8_v <- datecu8 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu9 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9))
datecu9_l <- datecu9 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu9_v <- datecu9 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu10 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10))
datecu10_l <- datecu10 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu10_v <- datecu10 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu11 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11))
datecu11_l <- datecu11 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu11_v <- datecu11 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu12 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12))
datecu12_l <- datecu12 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu12_v <- datecu12 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu13 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13))
datecu13_l <- datecu13 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu13_v <- datecu13 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu14 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14))
datecu14_l <- datecu14 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu14_v <- datecu14 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu15 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15))
datecu15_l <- datecu15 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu15_v <- datecu15 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu16 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datecu16_l <- datecu16 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu16_v <- datecu16 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datecu17 <- dateq17 %>% 
  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datecu17_l <- datecu17 %>%
  filter(Local == "Ecuador") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datecu17_v <- datecu17 %>%
  filter(Visita == "Ecuador") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datecu18 <- dateq17 %>% 
#  filter(Local == "Ecuador" | Visita == "Ecuador") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datecu18_l <- datecu18 %>%
#  filter(Local == "Ecuador") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datecu18_v <- datecu18 %>%
#  filter(Visita == "Ecuador") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# PARAGUAY
datpar <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada == 1)
datpar_l <- datpar %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar_v <- datpar %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar2 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2))
datpar2_l <- datpar2 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar2_v <- datpar2 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar3 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3))
datpar3_l <- datpar3 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar3_v <- datpar3 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar4 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datpar4_l <- datpar4 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar4_v <- datpar4 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar5 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datpar5_l <- datpar5 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar5_v <- datpar5 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar6 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datpar6_l <- datpar6 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar6_v <- datpar6 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar7 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datpar7_l <- datpar7 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar7_v <- datpar7 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar8 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datpar8_l <- datpar8 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar8_v <- datpar8 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar9 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datpar9_l <- datpar9 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar9_v <- datpar9 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar10 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datpar10_l <- datpar10 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar10_v <- datpar10 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar11 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datpar11_l <- datpar11 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar11_v <- datpar11 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar12 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datpar12_l <- datpar12 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar12_v <- datpar12 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar13 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datpar13_l <- datpar13 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar13_v <- datpar13 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar14 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datpar14_l <- datpar14 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar14_v <- datpar14 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar15 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datpar15_l <- datpar15 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar15_v <- datpar15 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar16 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datpar16_l <- datpar16 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar16_v <- datpar16 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datpar17 <- dateq17 %>% 
  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datpar17_l <- datpar17 %>%
  filter(Local == "Paraguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datpar17_v <- datpar17 %>%
  filter(Visita == "Paraguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datpar18 <- dateq17 %>% 
#  filter(Local == "Paraguay" | Visita == "Paraguay") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datpar18_l <- datpar18 %>%
#  filter(Local == "Paraguay") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datpar18_v <- datpar18 %>%
#  filter(Visita == "Paraguay") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# PERÚ
datper <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada == 1)
datper_l <- datper %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper_v <- datper %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper2 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2))
datper2_l <- datper2 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper2_v <- datper2 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper3 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3))
datper3_l <- datper3 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper3_v <- datper3 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper4 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datper4_l <- datper4 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper4_v <- datper4 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper5 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datper5_l <- datper5 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper5_v <- datper5 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper6 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datper6_l <- datper6 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper6_v <- datper6 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper7 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datper7_l <- datper7 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper7_v <- datper7 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper8 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datper8_l <- datper8 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper8_v <- datper8 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper9 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datper9_l <- datper9 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper9_v <- datper9 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper10 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datper10_l <- datper10 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper10_v <- datper10 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper11 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datper11_l <- datper11 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper11_v <- datper11 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper12 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datper12_l <- datper12 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper12_v <- datper12 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper13 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datper13_l <- datper13 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper13_v <- datper13 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper14 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datper14_l <- datper14 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper14_v <- datper14 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper15 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datper15_l <- datper15 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper15_v <- datper15 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper16 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datper16_l <- datper16 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper16_v <- datper16 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datper17 <- dateq17 %>% 
  filter(Local == "Perú" | Visita == "Perú") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datper17_l <- datper17 %>%
  filter(Local == "Perú") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datper17_v <- datper17 %>%
  filter(Visita == "Perú") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datper18 <- dateq17 %>% 
#  filter(Local == "Perú" | Visita == "Perú") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datper18_l <- datper18 %>%
#  filter(Local == "Perú") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datper18_v <- datper18 %>%
#  filter(Visita == "Perú") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# URUGUAY
daturu <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada == 1)
daturu_l <- daturu %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu_v <- daturu %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu2 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2))
daturu2_l <- daturu2 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu2_v <- daturu2 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu3 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3))
daturu3_l <- daturu3 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu3_v <- daturu3 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu4 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
daturu4_l <- daturu4 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu4_v <- daturu4 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu5 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
daturu5_l <- daturu5 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu5_v <- daturu5 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu6 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
daturu6_l <- daturu6 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu6_v <- daturu6 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu7 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
daturu7_l <- daturu7 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu7_v <- daturu7 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu8 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
daturu8_l <- daturu8 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu8_v <- daturu8 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu9 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
daturu9_l <- daturu9 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu9_v <- daturu9 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu10 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
daturu10_l <- daturu10 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu10_v <- daturu10 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu11 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
daturu11_l <- daturu11 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu11_v <- daturu11 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu12 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
daturu12_l <- daturu12 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu12_v <- daturu12 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu13 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
daturu13_l <- daturu13 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu13_v <- daturu13 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu14 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
daturu14_l <- daturu14 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu14_v <- daturu14 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu15 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
daturu15_l <- daturu15 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu15_v <- daturu15 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu16 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
daturu16_l <- daturu16 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu16_v <- daturu16 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
daturu17 <- dateq17 %>% 
  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
daturu17_l <- daturu17 %>%
  filter(Local == "Uruguay") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
daturu17_v <- daturu17 %>%
  filter(Visita == "Uruguay") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#daturu18 <- dateq17 %>% 
#  filter(Local == "Uruguay" | Visita == "Uruguay") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#daturu18_l <- daturu18 %>%
#  filter(Local == "Uruguay") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#daturu18_v <- daturu18 %>%
#  filter(Visita == "Uruguay") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))

# VENEZUELA
datven <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada == 1)
datven_l <- datven %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven_v <- datven %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven2 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2))
datven2_l <- datven2 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven2_v <- datven2 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven3 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3))
datven3_l <- datven3 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven3_v <- datven3 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven4 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4))
datven4_l <- datven4 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven4_v <- datven4 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven5 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5))
datven5_l <- datven5 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven5_v <- datven5 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven6 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6))
datven6_l <- datven6 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven6_v <- datven6 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven7 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7))
datven7_l <- datven7 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven7_v <- datven7 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven8 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8))
datven8_l <- datven8 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven8_v <- datven8 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven9 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9))
datven9_l <- datven9 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven9_v <- datven9 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven10 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
datven10_l <- datven10 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven10_v <- datven10 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven11 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
datven11_l <- datven11 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven11_v <- datven11 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven12 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
datven12_l <- datven12 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven12_v <- datven12 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven13 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
datven13_l <- datven13 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven13_v <- datven13 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven14 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
datven14_l <- datven14 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven14_v <- datven14 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven15 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
datven15_l <- datven15 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven15_v <- datven15 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven16 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
datven16_l <- datven16 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven16_v <- datven16 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
datven17 <- dateq17 %>% 
  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
datven17_l <- datven17 %>%
  filter(Local == "Venezuela") %>%
  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
  mutate(DL = ifelse(GL < GV, 1, 0))
datven17_v <- datven17 %>%
  filter(Visita == "Venezuela") %>%
  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
  mutate(DV = ifelse(GV < GL, 1, 0))
#datven18 <- dateq17 %>% 
#  filter(Local == "Venezuela" | Visita == "Venezuela") %>%
#  filter(Jornada %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
#datven18_l <- datven18 %>%
#  filter(Local == "Venezuela") %>%
#  mutate(VL = ifelse(GL > GV, 1, 0)) %>%
#  mutate(EL = ifelse(GL == GV, 1, 0)) %>%
#  mutate(DL = ifelse(GL < GV, 1, 0))
#datven18_v <- datven18 %>%
#  filter(Visita == "Venezuela") %>%
#  mutate(VV = ifelse(GV > GL, 1, 0)) %>%
#  mutate(EV = ifelse(GV == GL, 1, 0)) %>%
#  mutate(DV = ifelse(GV < GL, 1, 0))


# TABLA ACUMULADA DESPUES DE CADA JORNADA
taeq1 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg$Jornada),
                       NROW(datbol$Jornada),
                       NROW(datbra$Jornada),
                       NROW(datcol$Jornada),
                       NROW(datchi$Jornada),
                       NROW(datecu$Jornada),
                       NROW(datpar$Jornada),
                       NROW(datper$Jornada),
                       NROW(daturu$Jornada),
                       NROW(datven$Jornada)),
                   "PTOS"=
                     c(sum(datarg_l$VL*3,
                           datarg_v$VV*3,
                           datarg_l$EL*1,
                           datarg_v$EV*1),
                       sum(datbol_l$VL*3,
                           datbol_v$VV*3,
                           datbol_l$EL*1,
                           datbol_v$EV*1),
                       sum(datbra_l$VL*3,
                           datbra_v$VV*3,
                           datbra_l$EL*1,
                           datbra_v$EV*1),
                       sum(datcol_l$VL*3,
                           datcol_v$VV*3,
                           datcol_l$EL*1,
                           datcol_v$EV*1),
                       sum(datchi_l$VL*3,
                           datchi_v$VV*3,
                           datchi_l$EL*1,
                           datchi_v$EV*1),
                       sum(datecu_l$VL*3,
                           datecu_v$VV*3,
                           datecu_l$EL*1,
                           datecu_v$EV*1),
                       sum(datpar_l$VL*3,
                           datpar_v$VV*3,
                           datpar_l$EL*1,
                           datpar_v$EV*1),
                       sum(datper_l$VL*3,
                           datper_v$VV*3,
                           datper_l$EL*1,
                           datper_v$EV*1),
                       sum(daturu_l$VL*3,
                           daturu_v$VV*3,
                           daturu_l$EL*1,
                           daturu_v$EV*1),
                       sum(datven_l$VL*3,
                           datven_v$VV*3,
                           datven_l$EL*1,
                           datven_v$EV*1)),
                   "GF"=
                     c(sum(datarg_l$GL,
                           datarg_v$GV),
                       sum(datbol_l$GL,
                           datbol_v$GV),
                       sum(datbra_l$GL,
                           datbra_v$GV),
                       sum(datcol_l$GL,
                           datcol_v$GV),
                       sum(datchi_l$GL,
                           datchi_v$GV),
                       sum(datecu_l$GL,
                           datecu_v$GV),
                       sum(datpar_l$GL,
                           datpar_v$GV),
                       sum(datper_l$GL,
                           datper_v$GV),
                       sum(daturu_l$GL,
                           daturu_v$GV),
                       sum(datven_l$GL,
                           datven_v$GV)),
                   "GC"=
                     c(sum(datarg_l$GV,
                           datarg_v$GL),
                       sum(datbol_l$GV,
                           datbol_v$GL),
                       sum(datbra_l$GV,
                           datbra_v$GL),
                       sum(datcol_l$GV,
                           datcol_v$GL),
                       sum(datchi_l$GV,
                           datchi_v$GL),
                       sum(datecu_l$GV,
                           datecu_v$GL),
                       sum(datpar_l$GV,
                           datpar_v$GL),
                       sum(datper_l$GV,
                           datper_v$GL),
                       sum(daturu_l$GV,
                           daturu_v$GL),
                       sum(datven_l$GV,
                           datven_v$GL))
)
taeq2 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg2$Jornada),
                       NROW(datbol2$Jornada),
                       NROW(datbra2$Jornada),
                       NROW(datcol2$Jornada),
                       NROW(datchi2$Jornada),
                       NROW(datecu2$Jornada),
                       NROW(datpar2$Jornada),
                       NROW(datper2$Jornada),
                       NROW(daturu2$Jornada),
                       NROW(datven2$Jornada)),
                   "PTOS"=
                     c(sum(datarg2_l$VL*3,
                           datarg2_v$VV*3,
                           datarg2_l$EL*1,
                           datarg2_v$EV*1),
                       sum(datbol2_l$VL*3,
                           datbol2_v$VV*3,
                           datbol2_l$EL*1,
                           datbol2_v$EV*1),
                       sum(datbra2_l$VL*3,
                           datbra2_v$VV*3,
                           datbra2_l$EL*1,
                           datbra2_v$EV*1),
                       sum(datcol2_l$VL*3,
                           datcol2_v$VV*3,
                           datcol2_l$EL*1,
                           datcol2_v$EV*1),
                       sum(datchi2_l$VL*3,
                           datchi2_v$VV*3,
                           datchi2_l$EL*1,
                           datchi2_v$EV*1),
                       sum(datecu2_l$VL*3,
                           datecu2_v$VV*3,
                           datecu2_l$EL*1,
                           datecu2_v$EV*1),
                       sum(datpar2_l$VL*3,
                           datpar2_v$VV*3,
                           datpar2_l$EL*1,
                           datpar2_v$EV*1),
                       sum(datper2_l$VL*3,
                           datper2_v$VV*3,
                           datper2_l$EL*1,
                           datper2_v$EV*1),
                       sum(daturu2_l$VL*3,
                           daturu2_v$VV*3,
                           daturu2_l$EL*1,
                           daturu2_v$EV*1),
                       sum(datven2_l$VL*3,
                           datven2_v$VV*3,
                           datven2_l$EL*1,
                           datven2_v$EV*1)),
                   "GF"=
                     c(sum(datarg2_l$GL,
                           datarg2_v$GV),
                       sum(datbol2_l$GL,
                           datbol2_v$GV),
                       sum(datbra2_l$GL,
                           datbra2_v$GV),
                       sum(datcol2_l$GL,
                           datcol2_v$GV),
                       sum(datchi2_l$GL,
                           datchi2_v$GV),
                       sum(datecu2_l$GL,
                           datecu2_v$GV),
                       sum(datpar2_l$GL,
                           datpar2_v$GV),
                       sum(datper2_l$GL,
                           datper2_v$GV),
                       sum(daturu2_l$GL,
                           daturu2_v$GV),
                       sum(datven2_l$GL,
                           datven2_v$GV)),
                   "GC"=
                     c(sum(datarg2_l$GV,
                           datarg2_v$GL),
                       sum(datbol2_l$GV,
                           datbol2_v$GL),
                       sum(datbra2_l$GV,
                           datbra2_v$GL),
                       sum(datcol2_l$GV,
                           datcol2_v$GL),
                       sum(datchi2_l$GV,
                           datchi2_v$GL),
                       sum(datecu2_l$GV,
                           datecu2_v$GL),
                       sum(datpar2_l$GV,
                           datpar2_v$GL),
                       sum(datper2_l$GV,
                           datper2_v$GL),
                       sum(daturu2_l$GV,
                           daturu2_v$GL),
                       sum(datven2_l$GV,
                           datven2_v$GL))
)
taeq3 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg3$Jornada),
                       NROW(datbol3$Jornada),
                       NROW(datbra3$Jornada),
                       NROW(datcol3$Jornada),
                       NROW(datchi3$Jornada),
                       NROW(datecu3$Jornada),
                       NROW(datpar3$Jornada),
                       NROW(datper3$Jornada),
                       NROW(daturu3$Jornada),
                       NROW(datven3$Jornada)),
                   "PTOS"=
                     c(sum(datarg3_l$VL*3,
                           datarg3_v$VV*3,
                           datarg3_l$EL*1,
                           datarg3_v$EV*1),
                       sum(datbol3_l$VL*3,
                           datbol3_v$VV*3,
                           datbol3_l$EL*1,
                           datbol3_v$EV*1),
                       sum(datbra3_l$VL*3,
                           datbra3_v$VV*3,
                           datbra3_l$EL*1,
                           datbra3_v$EV*1),
                       sum(datcol3_l$VL*3,
                           datcol3_v$VV*3,
                           datcol3_l$EL*1,
                           datcol3_v$EV*1),
                       sum(datchi3_l$VL*3,
                           datchi3_v$VV*3,
                           datchi3_l$EL*1,
                           datchi3_v$EV*1),
                       sum(datecu3_l$VL*3,
                           datecu3_v$VV*3,
                           datecu3_l$EL*1,
                           datecu3_v$EV*1),
                       sum(datpar3_l$VL*3,
                           datpar3_v$VV*3,
                           datpar3_l$EL*1,
                           datpar3_v$EV*1),
                       sum(datper3_l$VL*3,
                           datper3_v$VV*3,
                           datper3_l$EL*1,
                           datper3_v$EV*1),
                       sum(daturu3_l$VL*3,
                           daturu3_v$VV*3,
                           daturu3_l$EL*1,
                           daturu3_v$EV*1),
                       sum(datven3_l$VL*3,
                           datven3_v$VV*3,
                           datven3_l$EL*1,
                           datven3_v$EV*1)),
                   "GF"=
                     c(sum(datarg3_l$GL,
                           datarg3_v$GV),
                       sum(datbol3_l$GL,
                           datbol3_v$GV),
                       sum(datbra3_l$GL,
                           datbra3_v$GV),
                       sum(datcol3_l$GL,
                           datcol3_v$GV),
                       sum(datchi3_l$GL,
                           datchi3_v$GV),
                       sum(datecu3_l$GL,
                           datecu3_v$GV),
                       sum(datpar3_l$GL,
                           datpar3_v$GV),
                       sum(datper3_l$GL,
                           datper3_v$GV),
                       sum(daturu3_l$GL,
                           daturu3_v$GV),
                       sum(datven3_l$GL,
                           datven3_v$GV)),
                   "GC"=
                     c(sum(datarg3_l$GV,
                           datarg3_v$GL),
                       sum(datbol3_l$GV,
                           datbol3_v$GL),
                       sum(datbra3_l$GV,
                           datbra3_v$GL),
                       sum(datcol3_l$GV,
                           datcol3_v$GL),
                       sum(datchi3_l$GV,
                           datchi3_v$GL),
                       sum(datecu3_l$GV,
                           datecu3_v$GL),
                       sum(datpar3_l$GV,
                           datpar3_v$GL),
                       sum(datper3_l$GV,
                           datper3_v$GL),
                       sum(daturu3_l$GV,
                           daturu3_v$GL),
                       sum(datven3_l$GV,
                           datven3_v$GL))
)
taeq4 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg4$Jornada),
                       NROW(datbol4$Jornada),
                       NROW(datbra4$Jornada),
                       NROW(datcol4$Jornada),
                       NROW(datchi4$Jornada),
                       NROW(datecu4$Jornada),
                       NROW(datpar4$Jornada),
                       NROW(datper4$Jornada),
                       NROW(daturu4$Jornada),
                       NROW(datven4$Jornada)),
                   "PTOS"=
                     c(sum(datarg4_l$VL*3,
                           datarg4_v$VV*3,
                           datarg4_l$EL*1,
                           datarg4_v$EV*1),
                       sum(datbol4_l$VL*3,
                           datbol4_v$VV*3,
                           datbol4_l$EL*1,
                           datbol4_v$EV*1),
                       sum(datbra4_l$VL*3,
                           datbra4_v$VV*3,
                           datbra4_l$EL*1,
                           datbra4_v$EV*1),
                       sum(datcol4_l$VL*3,
                           datcol4_v$VV*3,
                           datcol4_l$EL*1,
                           datcol4_v$EV*1),
                       sum(datchi4_l$VL*3,
                           datchi4_v$VV*3,
                           datchi4_l$EL*1,
                           datchi4_v$EV*1),
                       sum(datecu4_l$VL*3,
                           datecu4_v$VV*3,
                           datecu4_l$EL*1,
                           datecu4_v$EV*1),
                       sum(datpar4_l$VL*3,
                           datpar4_v$VV*3,
                           datpar4_l$EL*1,
                           datpar4_v$EV*1),
                       sum(datper4_l$VL*3,
                           datper4_v$VV*3,
                           datper4_l$EL*1,
                           datper4_v$EV*1),
                       sum(daturu4_l$VL*3,
                           daturu4_v$VV*3,
                           daturu4_l$EL*1,
                           daturu4_v$EV*1),
                       sum(datven4_l$VL*3,
                           datven4_v$VV*3,
                           datven4_l$EL*1,
                           datven4_v$EV*1)),
                   "GF"=
                     c(sum(datarg4_l$GL,
                           datarg4_v$GV),
                       sum(datbol4_l$GL,
                           datbol4_v$GV),
                       sum(datbra4_l$GL,
                           datbra4_v$GV),
                       sum(datcol4_l$GL,
                           datcol4_v$GV),
                       sum(datchi4_l$GL,
                           datchi4_v$GV),
                       sum(datecu4_l$GL,
                           datecu4_v$GV),
                       sum(datpar4_l$GL,
                           datpar4_v$GV),
                       sum(datper4_l$GL,
                           datper4_v$GV),
                       sum(daturu4_l$GL,
                           daturu4_v$GV),
                       sum(datven4_l$GL,
                           datven4_v$GV)),
                   "GC"=
                     c(sum(datarg4_l$GV,
                           datarg4_v$GL),
                       sum(datbol4_l$GV,
                           datbol4_v$GL),
                       sum(datbra4_l$GV,
                           datbra4_v$GL),
                       sum(datcol4_l$GV,
                           datcol4_v$GL),
                       sum(datchi4_l$GV,
                           datchi4_v$GL),
                       sum(datecu4_l$GV,
                           datecu4_v$GL),
                       sum(datpar4_l$GV,
                           datpar4_v$GL),
                       sum(datper4_l$GV,
                           datper4_v$GL),
                       sum(daturu4_l$GV,
                           daturu4_v$GL),
                       sum(datven4_l$GV,
                           datven4_v$GL))
)
taeq5 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg5$Jornada),
                       NROW(datbol5$Jornada),
                       NROW(datbra5$Jornada),
                       NROW(datcol5$Jornada),
                       NROW(datchi5$Jornada),
                       NROW(datecu5$Jornada),
                       NROW(datpar5$Jornada),
                       NROW(datper5$Jornada),
                       NROW(daturu5$Jornada),
                       NROW(datven5$Jornada)),
                   "PTOS"=
                     c(sum(datarg5_l$VL*3,
                           datarg5_v$VV*3,
                           datarg5_l$EL*1,
                           datarg5_v$EV*1),
                       sum(datbol5_l$VL*3,
                           datbol5_v$VV*3,
                           datbol5_l$EL*1,
                           datbol5_v$EV*1),
                       sum(datbra5_l$VL*3,
                           datbra5_v$VV*3,
                           datbra5_l$EL*1,
                           datbra5_v$EV*1),
                       sum(datcol5_l$VL*3,
                           datcol5_v$VV*3,
                           datcol5_l$EL*1,
                           datcol5_v$EV*1),
                       sum(datchi5_l$VL*3,
                           datchi5_v$VV*3,
                           datchi5_l$EL*1,
                           datchi5_v$EV*1),
                       sum(datecu5_l$VL*3,
                           datecu5_v$VV*3,
                           datecu5_l$EL*1,
                           datecu5_v$EV*1),
                       sum(datpar5_l$VL*3,
                           datpar5_v$VV*3,
                           datpar5_l$EL*1,
                           datpar5_v$EV*1),
                       sum(datper5_l$VL*3,
                           datper5_v$VV*3,
                           datper5_l$EL*1,
                           datper5_v$EV*1),
                       sum(daturu5_l$VL*3,
                           daturu5_v$VV*3,
                           daturu5_l$EL*1,
                           daturu5_v$EV*1),
                       sum(datven5_l$VL*3,
                           datven5_v$VV*3,
                           datven5_l$EL*1,
                           datven5_v$EV*1)),
                   "GF"=
                     c(sum(datarg5_l$GL,
                           datarg5_v$GV),
                       sum(datbol5_l$GL,
                           datbol5_v$GV),
                       sum(datbra5_l$GL,
                           datbra5_v$GV),
                       sum(datcol5_l$GL,
                           datcol5_v$GV),
                       sum(datchi5_l$GL,
                           datchi5_v$GV),
                       sum(datecu5_l$GL,
                           datecu5_v$GV),
                       sum(datpar5_l$GL,
                           datpar5_v$GV),
                       sum(datper5_l$GL,
                           datper5_v$GV),
                       sum(daturu5_l$GL,
                           daturu5_v$GV),
                       sum(datven5_l$GL,
                           datven5_v$GV)),
                   "GC"=
                     c(sum(datarg5_l$GV,
                           datarg5_v$GL),
                       sum(datbol5_l$GV,
                           datbol5_v$GL),
                       sum(datbra5_l$GV,
                           datbra5_v$GL),
                       sum(datcol5_l$GV,
                           datcol5_v$GL),
                       sum(datchi5_l$GV,
                           datchi5_v$GL),
                       sum(datecu5_l$GV,
                           datecu5_v$GL),
                       sum(datpar5_l$GV,
                           datpar5_v$GL),
                       sum(datper5_l$GV,
                           datper5_v$GL),
                       sum(daturu5_l$GV,
                           daturu5_v$GL),
                       sum(datven5_l$GV,
                           datven5_v$GL))
)
taeq6 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg6$Jornada),
                       NROW(datbol6$Jornada),
                       NROW(datbra6$Jornada),
                       NROW(datcol6$Jornada),
                       NROW(datchi6$Jornada),
                       NROW(datecu6$Jornada),
                       NROW(datpar6$Jornada),
                       NROW(datper6$Jornada),
                       NROW(daturu6$Jornada),
                       NROW(datven6$Jornada)),
                   "PTOS"=
                     c(sum(datarg6_l$VL*3,
                           datarg6_v$VV*3,
                           datarg6_l$EL*1,
                           datarg6_v$EV*1),
                       sum(datbol6_l$VL*3,
                           datbol6_v$VV*3,
                           datbol6_l$EL*1,
                           datbol6_v$EV*1),
                       sum(datbra6_l$VL*3,
                           datbra6_v$VV*3,
                           datbra6_l$EL*1,
                           datbra6_v$EV*1),
                       sum(datcol6_l$VL*3,
                           datcol6_v$VV*3,
                           datcol6_l$EL*1,
                           datcol6_v$EV*1),
                       sum(datchi6_l$VL*3,
                           datchi6_v$VV*3,
                           datchi6_l$EL*1,
                           datchi6_v$EV*1),
                       sum(datecu6_l$VL*3,
                           datecu6_v$VV*3,
                           datecu6_l$EL*1,
                           datecu6_v$EV*1),
                       sum(datpar6_l$VL*3,
                           datpar6_v$VV*3,
                           datpar6_l$EL*1,
                           datpar6_v$EV*1),
                       sum(datper6_l$VL*3,
                           datper6_v$VV*3,
                           datper6_l$EL*1,
                           datper6_v$EV*1),
                       sum(daturu6_l$VL*3,
                           daturu6_v$VV*3,
                           daturu6_l$EL*1,
                           daturu6_v$EV*1),
                       sum(datven6_l$VL*3,
                           datven6_v$VV*3,
                           datven6_l$EL*1,
                           datven6_v$EV*1)),
                   "GF"=
                     c(sum(datarg6_l$GL,
                           datarg6_v$GV),
                       sum(datbol6_l$GL,
                           datbol6_v$GV),
                       sum(datbra6_l$GL,
                           datbra6_v$GV),
                       sum(datcol6_l$GL,
                           datcol6_v$GV),
                       sum(datchi6_l$GL,
                           datchi6_v$GV),
                       sum(datecu6_l$GL,
                           datecu6_v$GV),
                       sum(datpar6_l$GL,
                           datpar6_v$GV),
                       sum(datper6_l$GL,
                           datper6_v$GV),
                       sum(daturu6_l$GL,
                           daturu6_v$GV),
                       sum(datven6_l$GL,
                           datven6_v$GV)),
                   "GC"=
                     c(sum(datarg6_l$GV,
                           datarg6_v$GL),
                       sum(datbol6_l$GV,
                           datbol6_v$GL),
                       sum(datbra6_l$GV,
                           datbra6_v$GL),
                       sum(datcol6_l$GV,
                           datcol6_v$GL),
                       sum(datchi6_l$GV,
                           datchi6_v$GL),
                       sum(datecu6_l$GV,
                           datecu6_v$GL),
                       sum(datpar6_l$GV,
                           datpar6_v$GL),
                       sum(datper6_l$GV,
                           datper6_v$GL),
                       sum(daturu6_l$GV,
                           daturu6_v$GL),
                       sum(datven6_l$GV,
                           datven6_v$GL))
)
taeq7 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg7$Jornada),
                       NROW(datbol7$Jornada),
                       NROW(datbra7$Jornada),
                       NROW(datcol7$Jornada),
                       NROW(datchi7$Jornada),
                       NROW(datecu7$Jornada),
                       NROW(datpar7$Jornada),
                       NROW(datper7$Jornada),
                       NROW(daturu7$Jornada),
                       NROW(datven7$Jornada)),
                   "PTOS"=
                     c(sum(datarg7_l$VL*3,
                           datarg7_v$VV*3,
                           datarg7_l$EL*1,
                           datarg7_v$EV*1),
                       sum(datbol7_l$VL*3,
                           datbol7_v$VV*3,
                           datbol7_l$EL*1,
                           datbol7_v$EV*1),
                       sum(datbra7_l$VL*3,
                           datbra7_v$VV*3,
                           datbra7_l$EL*1,
                           datbra7_v$EV*1),
                       sum(datcol7_l$VL*3,
                           datcol7_v$VV*3,
                           datcol7_l$EL*1,
                           datcol7_v$EV*1),
                       sum(datchi7_l$VL*3,
                           datchi7_v$VV*3,
                           datchi7_l$EL*1,
                           datchi7_v$EV*1),
                       sum(datecu7_l$VL*3,
                           datecu7_v$VV*3,
                           datecu7_l$EL*1,
                           datecu7_v$EV*1),
                       sum(datpar7_l$VL*3,
                           datpar7_v$VV*3,
                           datpar7_l$EL*1,
                           datpar7_v$EV*1),
                       sum(datper7_l$VL*3,
                           datper7_v$VV*3,
                           datper7_l$EL*1,
                           datper7_v$EV*1),
                       sum(daturu7_l$VL*3,
                           daturu7_v$VV*3,
                           daturu7_l$EL*1,
                           daturu7_v$EV*1),
                       sum(datven7_l$VL*3,
                           datven7_v$VV*3,
                           datven7_l$EL*1,
                           datven7_v$EV*1)),
                   "GF"=
                     c(sum(datarg7_l$GL,
                           datarg7_v$GV),
                       sum(datbol7_l$GL,
                           datbol7_v$GV),
                       sum(datbra7_l$GL,
                           datbra7_v$GV),
                       sum(datcol7_l$GL,
                           datcol7_v$GV),
                       sum(datchi7_l$GL,
                           datchi7_v$GV),
                       sum(datecu7_l$GL,
                           datecu7_v$GV),
                       sum(datpar7_l$GL,
                           datpar7_v$GV),
                       sum(datper7_l$GL,
                           datper7_v$GV),
                       sum(daturu7_l$GL,
                           daturu7_v$GV),
                       sum(datven7_l$GL,
                           datven7_v$GV)),
                   "GC"=
                     c(sum(datarg7_l$GV,
                           datarg7_v$GL),
                       sum(datbol7_l$GV,
                           datbol7_v$GL),
                       sum(datbra7_l$GV,
                           datbra7_v$GL),
                       sum(datcol7_l$GV,
                           datcol7_v$GL),
                       sum(datchi7_l$GV,
                           datchi7_v$GL),
                       sum(datecu7_l$GV,
                           datecu7_v$GL),
                       sum(datpar7_l$GV,
                           datpar7_v$GL),
                       sum(datper7_l$GV,
                           datper7_v$GL),
                       sum(daturu7_l$GV,
                           daturu7_v$GL),
                       sum(datven7_l$GV,
                           datven7_v$GL))
)
taeq8 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg8$Jornada),
                       NROW(datbol8$Jornada),
                       NROW(datbra8$Jornada),
                       NROW(datcol8$Jornada),
                       NROW(datchi8$Jornada),
                       NROW(datecu8$Jornada),
                       NROW(datpar8$Jornada),
                       NROW(datper8$Jornada),
                       NROW(daturu8$Jornada),
                       NROW(datven8$Jornada)),
                   "PTOS"=
                     c(sum(datarg8_l$VL*3,
                           datarg8_v$VV*3,
                           datarg8_l$EL*1,
                           datarg8_v$EV*1),
                       sum(datbol8_l$VL*3,
                           datbol8_v$VV*3,
                           datbol8_l$EL*1,
                           datbol8_v$EV*1),
                       sum(datbra8_l$VL*3,
                           datbra8_v$VV*3,
                           datbra8_l$EL*1,
                           datbra8_v$EV*1),
                       sum(datcol8_l$VL*3,
                           datcol8_v$VV*3,
                           datcol8_l$EL*1,
                           datcol8_v$EV*1),
                       sum(datchi8_l$VL*3,
                           datchi8_v$VV*3,
                           datchi8_l$EL*1,
                           datchi8_v$EV*1),
                       sum(datecu8_l$VL*3,
                           datecu8_v$VV*3,
                           datecu8_l$EL*1,
                           datecu8_v$EV*1),
                       sum(datpar8_l$VL*3,
                           datpar8_v$VV*3,
                           datpar8_l$EL*1,
                           datpar8_v$EV*1),
                       sum(datper8_l$VL*3,
                           datper8_v$VV*3,
                           datper8_l$EL*1,
                           datper8_v$EV*1),
                       sum(daturu8_l$VL*3,
                           daturu8_v$VV*3,
                           daturu8_l$EL*1,
                           daturu8_v$EV*1),
                       sum(datven8_l$VL*3,
                           datven8_v$VV*3,
                           datven8_l$EL*1,
                           datven8_v$EV*1)),
                   "GF"=
                     c(sum(datarg8_l$GL,
                           datarg8_v$GV),
                       sum(datbol8_l$GL,
                           datbol8_v$GV),
                       sum(datbra8_l$GL,
                           datbra8_v$GV),
                       sum(datcol8_l$GL,
                           datcol8_v$GV),
                       sum(datchi8_l$GL,
                           datchi8_v$GV),
                       sum(datecu8_l$GL,
                           datecu8_v$GV),
                       sum(datpar8_l$GL,
                           datpar8_v$GV),
                       sum(datper8_l$GL,
                           datper8_v$GV),
                       sum(daturu8_l$GL,
                           daturu8_v$GV),
                       sum(datven8_l$GL,
                           datven8_v$GV)),
                   "GC"=
                     c(sum(datarg8_l$GV,
                           datarg8_v$GL),
                       sum(datbol8_l$GV,
                           datbol8_v$GL),
                       sum(datbra8_l$GV,
                           datbra8_v$GL),
                       sum(datcol8_l$GV,
                           datcol8_v$GL),
                       sum(datchi8_l$GV,
                           datchi8_v$GL),
                       sum(datecu8_l$GV,
                           datecu8_v$GL),
                       sum(datpar8_l$GV,
                           datpar8_v$GL),
                       sum(datper8_l$GV,
                           datper8_v$GL),
                       sum(daturu8_l$GV,
                           daturu8_v$GL),
                       sum(datven8_l$GV,
                           datven8_v$GL))
)
taeq9 = data.frame("EQUIPOS" =
                     c("Argentina",
                       "Bolivia",
                       "Brasil",
                       "Colombia",
                       "Chile",
                       "Ecuador",
                       "Paraguay",
                       "Perú",
                       "Uruguay",
                       "Venezuela"),
                   "PJ" =
                     c(NROW(datarg9$Jornada),
                       NROW(datbol9$Jornada),
                       NROW(datbra9$Jornada),
                       NROW(datcol9$Jornada),
                       NROW(datchi9$Jornada),
                       NROW(datecu9$Jornada),
                       NROW(datpar9$Jornada),
                       NROW(datper9$Jornada),
                       NROW(daturu9$Jornada),
                       NROW(datven9$Jornada)),
                   "PTOS"=
                     c(sum(datarg9_l$VL*3,
                           datarg9_v$VV*3,
                           datarg9_l$EL*1,
                           datarg9_v$EV*1),
                       sum(datbol9_l$VL*3,
                           datbol9_v$VV*3,
                           datbol9_l$EL*1,
                           datbol9_v$EV*1),
                       sum(datbra9_l$VL*3,
                           datbra9_v$VV*3,
                           datbra9_l$EL*1,
                           datbra9_v$EV*1),
                       sum(datcol9_l$VL*3,
                           datcol9_v$VV*3,
                           datcol9_l$EL*1,
                           datcol9_v$EV*1),
                       sum(datchi9_l$VL*3,
                           datchi9_v$VV*3,
                           datchi9_l$EL*1,
                           datchi9_v$EV*1),
                       sum(datecu9_l$VL*3,
                           datecu9_v$VV*3,
                           datecu9_l$EL*1,
                           datecu9_v$EV*1),
                       sum(datpar9_l$VL*3,
                           datpar9_v$VV*3,
                           datpar9_l$EL*1,
                           datpar9_v$EV*1),
                       sum(datper9_l$VL*3,
                           datper9_v$VV*3,
                           datper9_l$EL*1,
                           datper9_v$EV*1),
                       sum(daturu9_l$VL*3,
                           daturu9_v$VV*3,
                           daturu9_l$EL*1,
                           daturu9_v$EV*1),
                       sum(datven9_l$VL*3,
                           datven9_v$VV*3,
                           datven9_l$EL*1,
                           datven9_v$EV*1)),
                   "GF"=
                     c(sum(datarg9_l$GL,
                           datarg9_v$GV),
                       sum(datbol9_l$GL,
                           datbol9_v$GV),
                       sum(datbra9_l$GL,
                           datbra9_v$GV),
                       sum(datcol9_l$GL,
                           datcol9_v$GV),
                       sum(datchi9_l$GL,
                           datchi9_v$GV),
                       sum(datecu9_l$GL,
                           datecu9_v$GV),
                       sum(datpar9_l$GL,
                           datpar9_v$GV),
                       sum(datper9_l$GL,
                           datper9_v$GV),
                       sum(daturu9_l$GL,
                           daturu9_v$GV),
                       sum(datven9_l$GL,
                           datven9_v$GV)),
                   "GC"=
                     c(sum(datarg9_l$GV,
                           datarg9_v$GL),
                       sum(datbol9_l$GV,
                           datbol9_v$GL),
                       sum(datbra9_l$GV,
                           datbra9_v$GL),
                       sum(datcol9_l$GV,
                           datcol9_v$GL),
                       sum(datchi9_l$GV,
                           datchi9_v$GL),
                       sum(datecu9_l$GV,
                           datecu9_v$GL),
                       sum(datpar9_l$GV,
                           datpar9_v$GL),
                       sum(datper9_l$GV,
                           datper9_v$GL),
                       sum(daturu9_l$GV,
                           daturu9_v$GL),
                       sum(datven9_l$GV,
                           datven9_v$GL))
)
taeq10 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg10$Jornada),
                        NROW(datbol10$Jornada),
                        NROW(datbra10$Jornada),
                        NROW(datcol10$Jornada),
                        NROW(datchi10$Jornada),
                        NROW(datecu10$Jornada),
                        NROW(datpar10$Jornada),
                        NROW(datper10$Jornada),
                        NROW(daturu10$Jornada),
                        NROW(datven10$Jornada)),
                    "PTOS"=
                      c(sum(datarg10_l$VL*3,
                            datarg10_v$VV*3,
                            datarg10_l$EL*1,
                            datarg10_v$EV*1),
                        sum(datbol10_l$VL*3,
                            datbol10_v$VV*3,
                            datbol10_l$EL*1,
                            datbol10_v$EV*1),
                        sum(datbra10_l$VL*3,
                            datbra10_v$VV*3,
                            datbra10_l$EL*1,
                            datbra10_v$EV*1),
                        sum(datcol10_l$VL*3,
                            datcol10_v$VV*3,
                            datcol10_l$EL*1,
                            datcol10_v$EV*1),
                        sum(datchi10_l$VL*3,
                            datchi10_v$VV*3,
                            datchi10_l$EL*1,
                            datchi10_v$EV*1),
                        sum(datecu10_l$VL*3,
                            datecu10_v$VV*3,
                            datecu10_l$EL*1,
                            datecu10_v$EV*1),
                        sum(datpar10_l$VL*3,
                            datpar10_v$VV*3,
                            datpar10_l$EL*1,
                            datpar10_v$EV*1),
                        sum(datper10_l$VL*3,
                            datper10_v$VV*3,
                            datper10_l$EL*1,
                            datper10_v$EV*1),
                        sum(daturu10_l$VL*3,
                            daturu10_v$VV*3,
                            daturu10_l$EL*1,
                            daturu10_v$EV*1),
                        sum(datven10_l$VL*3,
                            datven10_v$VV*3,
                            datven10_l$EL*1,
                            datven10_v$EV*1)),
                    "GF"=
                      c(sum(datarg10_l$GL,
                            datarg10_v$GV),
                        sum(datbol10_l$GL,
                            datbol10_v$GV),
                        sum(datbra10_l$GL,
                            datbra10_v$GV),
                        sum(datcol10_l$GL,
                            datcol10_v$GV),
                        sum(datchi10_l$GL,
                            datchi10_v$GV),
                        sum(datecu10_l$GL,
                            datecu10_v$GV),
                        sum(datpar10_l$GL,
                            datpar10_v$GV),
                        sum(datper10_l$GL,
                            datper10_v$GV),
                        sum(daturu10_l$GL,
                            daturu10_v$GV),
                        sum(datven10_l$GL,
                            datven10_v$GV)),
                    "GC"=
                      c(sum(datarg10_l$GV,
                            datarg10_v$GL),
                        sum(datbol10_l$GV,
                            datbol10_v$GL),
                        sum(datbra10_l$GV,
                            datbra10_v$GL),
                        sum(datcol10_l$GV,
                            datcol10_v$GL),
                        sum(datchi10_l$GV,
                            datchi10_v$GL),
                        sum(datecu10_l$GV,
                            datecu10_v$GL),
                        sum(datpar10_l$GV,
                            datpar10_v$GL),
                        sum(datper10_l$GV,
                            datper10_v$GL),
                        sum(daturu10_l$GV,
                            daturu10_v$GL),
                        sum(datven10_l$GV,
                            datven10_v$GL))
)
taeq11 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg11$Jornada),
                        NROW(datbol11$Jornada),
                        NROW(datbra11$Jornada),
                        NROW(datcol11$Jornada),
                        NROW(datchi11$Jornada),
                        NROW(datecu11$Jornada),
                        NROW(datpar11$Jornada),
                        NROW(datper11$Jornada),
                        NROW(daturu11$Jornada),
                        NROW(datven11$Jornada)),
                    "PTOS"=
                      c(sum(datarg11_l$VL*3,
                            datarg11_v$VV*3,
                            datarg11_l$EL*1,
                            datarg11_v$EV*1),
                        sum(datbol11_l$VL*3,
                            datbol11_v$VV*3,
                            datbol11_l$EL*1,
                            datbol11_v$EV*1),
                        sum(datbra11_l$VL*3,
                            datbra11_v$VV*3,
                            datbra11_l$EL*1,
                            datbra11_v$EV*1),
                        sum(datcol11_l$VL*3,
                            datcol11_v$VV*3,
                            datcol11_l$EL*1,
                            datcol11_v$EV*1),
                        sum(datchi11_l$VL*3,
                            datchi11_v$VV*3,
                            datchi11_l$EL*1,
                            datchi11_v$EV*1),
                        sum(datecu11_l$VL*3,
                            datecu11_v$VV*3,
                            datecu11_l$EL*1,
                            datecu11_v$EV*1),
                        sum(datpar11_l$VL*3,
                            datpar11_v$VV*3,
                            datpar11_l$EL*1,
                            datpar11_v$EV*1),
                        sum(datper11_l$VL*3,
                            datper11_v$VV*3,
                            datper11_l$EL*1,
                            datper11_v$EV*1),
                        sum(daturu11_l$VL*3,
                            daturu11_v$VV*3,
                            daturu11_l$EL*1,
                            daturu11_v$EV*1),
                        sum(datven11_l$VL*3,
                            datven11_v$VV*3,
                            datven11_l$EL*1,
                            datven11_v$EV*1)),
                    "GF"=
                      c(sum(datarg11_l$GL,
                            datarg11_v$GV),
                        sum(datbol11_l$GL,
                            datbol11_v$GV),
                        sum(datbra11_l$GL,
                            datbra11_v$GV),
                        sum(datcol11_l$GL,
                            datcol11_v$GV),
                        sum(datchi11_l$GL,
                            datchi11_v$GV),
                        sum(datecu11_l$GL,
                            datecu11_v$GV),
                        sum(datpar11_l$GL,
                            datpar11_v$GV),
                        sum(datper11_l$GL,
                            datper11_v$GV),
                        sum(daturu11_l$GL,
                            daturu11_v$GV),
                        sum(datven11_l$GL,
                            datven11_v$GV)),
                    "GC"=
                      c(sum(datarg11_l$GV,
                            datarg11_v$GL),
                        sum(datbol11_l$GV,
                            datbol11_v$GL),
                        sum(datbra11_l$GV,
                            datbra11_v$GL),
                        sum(datcol11_l$GV,
                            datcol11_v$GL),
                        sum(datchi11_l$GV,
                            datchi11_v$GL),
                        sum(datecu11_l$GV,
                            datecu11_v$GL),
                        sum(datpar11_l$GV,
                            datpar11_v$GL),
                        sum(datper11_l$GV,
                            datper11_v$GL),
                        sum(daturu11_l$GV,
                            daturu11_v$GL),
                        sum(datven11_l$GV,
                            datven11_v$GL))
)
taeq12 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg12$Jornada),
                        NROW(datbol12$Jornada),
                        NROW(datbra12$Jornada),
                        NROW(datcol12$Jornada),
                        NROW(datchi12$Jornada),
                        NROW(datecu12$Jornada),
                        NROW(datpar12$Jornada),
                        NROW(datper12$Jornada),
                        NROW(daturu12$Jornada),
                        NROW(datven12$Jornada)),
                    "PTOS"=
                      c(sum(datarg12_l$VL*3,
                            datarg12_v$VV*3,
                            datarg12_l$EL*1,
                            datarg12_v$EV*1),
                        sum(datbol12_l$VL*3,
                            datbol12_v$VV*3,
                            datbol12_l$EL*1,
                            datbol12_v$EV*1),
                        sum(datbra12_l$VL*3,
                            datbra12_v$VV*3,
                            datbra12_l$EL*1,
                            datbra12_v$EV*1),
                        sum(datcol12_l$VL*3,
                            datcol12_v$VV*3,
                            datcol12_l$EL*1,
                            datcol12_v$EV*1),
                        sum(datchi12_l$VL*3,
                            datchi12_v$VV*3,
                            datchi12_l$EL*1,
                            datchi12_v$EV*1),
                        sum(datecu12_l$VL*3,
                            datecu12_v$VV*3,
                            datecu12_l$EL*1,
                            datecu12_v$EV*1),
                        sum(datpar12_l$VL*3,
                            datpar12_v$VV*3,
                            datpar12_l$EL*1,
                            datpar12_v$EV*1),
                        sum(datper12_l$VL*3,
                            datper12_v$VV*3,
                            datper12_l$EL*1,
                            datper12_v$EV*1),
                        sum(daturu12_l$VL*3,
                            daturu12_v$VV*3,
                            daturu12_l$EL*1,
                            daturu12_v$EV*1),
                        sum(datven12_l$VL*3,
                            datven12_v$VV*3,
                            datven12_l$EL*1,
                            datven12_v$EV*1)),
                    "GF"=
                      c(sum(datarg12_l$GL,
                            datarg12_v$GV),
                        sum(datbol12_l$GL,
                            datbol12_v$GV),
                        sum(datbra12_l$GL,
                            datbra12_v$GV),
                        sum(datcol12_l$GL,
                            datcol12_v$GV),
                        sum(datchi12_l$GL,
                            datchi12_v$GV),
                        sum(datecu12_l$GL,
                            datecu12_v$GV),
                        sum(datpar12_l$GL,
                            datpar12_v$GV),
                        sum(datper12_l$GL,
                            datper12_v$GV),
                        sum(daturu12_l$GL,
                            daturu12_v$GV),
                        sum(datven12_l$GL,
                            datven12_v$GV)),
                    "GC"=
                      c(sum(datarg12_l$GV,
                            datarg12_v$GL),
                        sum(datbol12_l$GV,
                            datbol12_v$GL),
                        sum(datbra12_l$GV,
                            datbra12_v$GL),
                        sum(datcol12_l$GV,
                            datcol12_v$GL),
                        sum(datchi12_l$GV,
                            datchi12_v$GL),
                        sum(datecu12_l$GV,
                            datecu12_v$GL),
                        sum(datpar12_l$GV,
                            datpar12_v$GL),
                        sum(datper12_l$GV,
                            datper12_v$GL),
                        sum(daturu12_l$GV,
                            daturu12_v$GL),
                        sum(datven12_l$GV,
                            datven12_v$GL))
)
taeq13 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg13$Jornada),
                        NROW(datbol13$Jornada),
                        NROW(datbra13$Jornada),
                        NROW(datcol13$Jornada),
                        NROW(datchi13$Jornada),
                        NROW(datecu13$Jornada),
                        NROW(datpar13$Jornada),
                        NROW(datper13$Jornada),
                        NROW(daturu13$Jornada),
                        NROW(datven13$Jornada)),
                    "PTOS"=
                      c(sum(datarg13_l$VL*3,
                            datarg13_v$VV*3,
                            datarg13_l$EL*1,
                            datarg13_v$EV*1),
                        sum(datbol13_l$VL*3,
                            datbol13_v$VV*3,
                            datbol13_l$EL*1,
                            datbol13_v$EV*1),
                        sum(datbra13_l$VL*3,
                            datbra13_v$VV*3,
                            datbra13_l$EL*1,
                            datbra13_v$EV*1),
                        sum(datcol13_l$VL*3,
                            datcol13_v$VV*3,
                            datcol13_l$EL*1,
                            datcol13_v$EV*1),
                        sum(datchi13_l$VL*3,
                            datchi13_v$VV*3,
                            datchi13_l$EL*1,
                            datchi13_v$EV*1),
                        sum(datecu13_l$VL*3,
                            datecu13_v$VV*3,
                            datecu13_l$EL*1,
                            datecu13_v$EV*1),
                        sum(datpar13_l$VL*3,
                            datpar13_v$VV*3,
                            datpar13_l$EL*1,
                            datpar13_v$EV*1),
                        sum(datper13_l$VL*3,
                            datper13_v$VV*3,
                            datper13_l$EL*1,
                            datper13_v$EV*1),
                        sum(daturu13_l$VL*3,
                            daturu13_v$VV*3,
                            daturu13_l$EL*1,
                            daturu13_v$EV*1),
                        sum(datven13_l$VL*3,
                            datven13_v$VV*3,
                            datven13_l$EL*1,
                            datven13_v$EV*1)),
                    "GF"=
                      c(sum(datarg13_l$GL,
                            datarg13_v$GV),
                        sum(datbol13_l$GL,
                            datbol13_v$GV),
                        sum(datbra13_l$GL,
                            datbra13_v$GV),
                        sum(datcol13_l$GL,
                            datcol13_v$GV),
                        sum(datchi13_l$GL,
                            datchi13_v$GV),
                        sum(datecu13_l$GL,
                            datecu13_v$GV),
                        sum(datpar13_l$GL,
                            datpar13_v$GV),
                        sum(datper13_l$GL,
                            datper13_v$GV),
                        sum(daturu13_l$GL,
                            daturu13_v$GV),
                        sum(datven13_l$GL,
                            datven13_v$GV)),
                    "GC"=
                      c(sum(datarg13_l$GV,
                            datarg13_v$GL),
                        sum(datbol13_l$GV,
                            datbol13_v$GL),
                        sum(datbra13_l$GV,
                            datbra13_v$GL),
                        sum(datcol13_l$GV,
                            datcol13_v$GL),
                        sum(datchi13_l$GV,
                            datchi13_v$GL),
                        sum(datecu13_l$GV,
                            datecu13_v$GL),
                        sum(datpar13_l$GV,
                            datpar13_v$GL),
                        sum(datper13_l$GV,
                            datper13_v$GL),
                        sum(daturu13_l$GV,
                            daturu13_v$GL),
                        sum(datven13_l$GV,
                            datven13_v$GL))
)
taeq14 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg14$Jornada),
                        NROW(datbol14$Jornada),
                        NROW(datbra14$Jornada),
                        NROW(datcol14$Jornada),
                        NROW(datchi14$Jornada),
                        NROW(datecu14$Jornada),
                        NROW(datpar14$Jornada),
                        NROW(datper14$Jornada),
                        NROW(daturu14$Jornada),
                        NROW(datven14$Jornada)),
                    "PTOS"=
                      c(sum(datarg14_l$VL*3,
                            datarg14_v$VV*3,
                            datarg14_l$EL*1,
                            datarg14_v$EV*1),
                        sum(datbol14_l$VL*3,
                            datbol14_v$VV*3,
                            datbol14_l$EL*1,
                            datbol14_v$EV*1),
                        sum(datbra14_l$VL*3,
                            datbra14_v$VV*3,
                            datbra14_l$EL*1,
                            datbra14_v$EV*1),
                        sum(datcol14_l$VL*3,
                            datcol14_v$VV*3,
                            datcol14_l$EL*1,
                            datcol14_v$EV*1),
                        sum(datchi14_l$VL*3,
                            datchi14_v$VV*3,
                            datchi14_l$EL*1,
                            datchi14_v$EV*1),
                        sum(datecu14_l$VL*3,
                            datecu14_v$VV*3,
                            datecu14_l$EL*1,
                            datecu14_v$EV*1),
                        sum(datpar14_l$VL*3,
                            datpar14_v$VV*3,
                            datpar14_l$EL*1,
                            datpar14_v$EV*1),
                        sum(datper14_l$VL*3,
                            datper14_v$VV*3,
                            datper14_l$EL*1,
                            datper14_v$EV*1),
                        sum(daturu14_l$VL*3,
                            daturu14_v$VV*3,
                            daturu14_l$EL*1,
                            daturu14_v$EV*1),
                        sum(datven14_l$VL*3,
                            datven14_v$VV*3,
                            datven14_l$EL*1,
                            datven14_v$EV*1)),
                    "GF"=
                      c(sum(datarg14_l$GL,
                            datarg14_v$GV),
                        sum(datbol14_l$GL,
                            datbol14_v$GV),
                        sum(datbra14_l$GL,
                            datbra14_v$GV),
                        sum(datcol14_l$GL,
                            datcol14_v$GV),
                        sum(datchi14_l$GL,
                            datchi14_v$GV),
                        sum(datecu14_l$GL,
                            datecu14_v$GV),
                        sum(datpar14_l$GL,
                            datpar14_v$GV),
                        sum(datper14_l$GL,
                            datper14_v$GV),
                        sum(daturu14_l$GL,
                            daturu14_v$GV),
                        sum(datven14_l$GL,
                            datven14_v$GV)),
                    "GC"=
                      c(sum(datarg14_l$GV,
                            datarg14_v$GL),
                        sum(datbol14_l$GV,
                            datbol14_v$GL),
                        sum(datbra14_l$GV,
                            datbra14_v$GL),
                        sum(datcol14_l$GV,
                            datcol14_v$GL),
                        sum(datchi14_l$GV,
                            datchi14_v$GL),
                        sum(datecu14_l$GV,
                            datecu14_v$GL),
                        sum(datpar14_l$GV,
                            datpar14_v$GL),
                        sum(datper14_l$GV,
                            datper14_v$GL),
                        sum(daturu14_l$GV,
                            daturu14_v$GL),
                        sum(datven14_l$GV,
                            datven14_v$GL))
)
taeq15 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg15$Jornada),
                        NROW(datbol15$Jornada),
                        NROW(datbra15$Jornada),
                        NROW(datcol15$Jornada),
                        NROW(datchi15$Jornada),
                        NROW(datecu15$Jornada),
                        NROW(datpar15$Jornada),
                        NROW(datper15$Jornada),
                        NROW(daturu15$Jornada),
                        NROW(datven15$Jornada)),
                    "PTOS"=
                      c(sum(datarg15_l$VL*3,
                            datarg15_v$VV*3,
                            datarg15_l$EL*1,
                            datarg15_v$EV*1),
                        sum(datbol15_l$VL*3,
                            datbol15_v$VV*3,
                            datbol15_l$EL*1,
                            datbol15_v$EV*1),
                        sum(datbra15_l$VL*3,
                            datbra15_v$VV*3,
                            datbra15_l$EL*1,
                            datbra15_v$EV*1),
                        sum(datcol15_l$VL*3,
                            datcol15_v$VV*3,
                            datcol15_l$EL*1,
                            datcol15_v$EV*1),
                        sum(datchi15_l$VL*3,
                            datchi15_v$VV*3,
                            datchi15_l$EL*1,
                            datchi15_v$EV*1),
                        sum(datecu15_l$VL*3,
                            datecu15_v$VV*3,
                            datecu15_l$EL*1,
                            datecu15_v$EV*1),
                        sum(datpar15_l$VL*3,
                            datpar15_v$VV*3,
                            datpar15_l$EL*1,
                            datpar15_v$EV*1),
                        sum(datper15_l$VL*3,
                            datper15_v$VV*3,
                            datper15_l$EL*1,
                            datper15_v$EV*1),
                        sum(daturu15_l$VL*3,
                            daturu15_v$VV*3,
                            daturu15_l$EL*1,
                            daturu15_v$EV*1),
                        sum(datven15_l$VL*3,
                            datven15_v$VV*3,
                            datven15_l$EL*1,
                            datven15_v$EV*1)),
                    "GF"=
                      c(sum(datarg15_l$GL,
                            datarg15_v$GV),
                        sum(datbol15_l$GL,
                            datbol15_v$GV),
                        sum(datbra15_l$GL,
                            datbra15_v$GV),
                        sum(datcol15_l$GL,
                            datcol15_v$GV),
                        sum(datchi15_l$GL,
                            datchi15_v$GV),
                        sum(datecu15_l$GL,
                            datecu15_v$GV),
                        sum(datpar15_l$GL,
                            datpar15_v$GV),
                        sum(datper15_l$GL,
                            datper15_v$GV),
                        sum(daturu15_l$GL,
                            daturu15_v$GV),
                        sum(datven15_l$GL,
                            datven15_v$GV)),
                    "GC"=
                      c(sum(datarg15_l$GV,
                            datarg15_v$GL),
                        sum(datbol15_l$GV,
                            datbol15_v$GL),
                        sum(datbra15_l$GV,
                            datbra15_v$GL),
                        sum(datcol15_l$GV,
                            datcol15_v$GL),
                        sum(datchi15_l$GV,
                            datchi15_v$GL),
                        sum(datecu15_l$GV,
                            datecu15_v$GL),
                        sum(datpar15_l$GV,
                            datpar15_v$GL),
                        sum(datper15_l$GV,
                            datper15_v$GL),
                        sum(daturu15_l$GV,
                            daturu15_v$GL),
                        sum(datven15_l$GV,
                            datven15_v$GL))
)
taeq16 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg16$Jornada),
                        NROW(datbol16$Jornada),
                        NROW(datbra16$Jornada),
                        NROW(datcol16$Jornada),
                        NROW(datchi16$Jornada),
                        NROW(datecu16$Jornada),
                        NROW(datpar16$Jornada),
                        NROW(datper16$Jornada),
                        NROW(daturu16$Jornada),
                        NROW(datven16$Jornada)),
                    "PTOS"=
                      c(sum(datarg16_l$VL*3,
                            datarg16_v$VV*3,
                            datarg16_l$EL*1,
                            datarg16_v$EV*1),
                        sum(datbol16_l$VL*3,
                            datbol16_v$VV*3,
                            datbol16_l$EL*1,
                            datbol16_v$EV*1),
                        sum(datbra16_l$VL*3,
                            datbra16_v$VV*3,
                            datbra16_l$EL*1,
                            datbra16_v$EV*1),
                        sum(datcol16_l$VL*3,
                            datcol16_v$VV*3,
                            datcol16_l$EL*1,
                            datcol16_v$EV*1),
                        sum(datchi16_l$VL*3,
                            datchi16_v$VV*3,
                            datchi16_l$EL*1,
                            datchi16_v$EV*1),
                        sum(datecu16_l$VL*3,
                            datecu16_v$VV*3,
                            datecu16_l$EL*1,
                            datecu16_v$EV*1),
                        sum(datpar16_l$VL*3,
                            datpar16_v$VV*3,
                            datpar16_l$EL*1,
                            datpar16_v$EV*1),
                        sum(datper16_l$VL*3,
                            datper16_v$VV*3,
                            datper16_l$EL*1,
                            datper16_v$EV*1),
                        sum(daturu16_l$VL*3,
                            daturu16_v$VV*3,
                            daturu16_l$EL*1,
                            daturu16_v$EV*1),
                        sum(datven16_l$VL*3,
                            datven16_v$VV*3,
                            datven16_l$EL*1,
                            datven16_v$EV*1)),
                    "GF"=
                      c(sum(datarg16_l$GL,
                            datarg16_v$GV),
                        sum(datbol16_l$GL,
                            datbol16_v$GV),
                        sum(datbra16_l$GL,
                            datbra16_v$GV),
                        sum(datcol16_l$GL,
                            datcol16_v$GV),
                        sum(datchi16_l$GL,
                            datchi16_v$GV),
                        sum(datecu16_l$GL,
                            datecu16_v$GV),
                        sum(datpar16_l$GL,
                            datpar16_v$GV),
                        sum(datper16_l$GL,
                            datper16_v$GV),
                        sum(daturu16_l$GL,
                            daturu16_v$GV),
                        sum(datven16_l$GL,
                            datven16_v$GV)),
                    "GC"=
                      c(sum(datarg16_l$GV,
                            datarg16_v$GL),
                        sum(datbol16_l$GV,
                            datbol16_v$GL),
                        sum(datbra16_l$GV,
                            datbra16_v$GL),
                        sum(datcol16_l$GV,
                            datcol16_v$GL),
                        sum(datchi16_l$GV,
                            datchi16_v$GL),
                        sum(datecu16_l$GV,
                            datecu16_v$GL),
                        sum(datpar16_l$GV,
                            datpar16_v$GL),
                        sum(datper16_l$GV,
                            datper16_v$GL),
                        sum(daturu16_l$GV,
                            daturu16_v$GL),
                        sum(datven16_l$GV,
                            datven16_v$GL))
)
taeq17 = data.frame("EQUIPOS" =
                      c("Argentina",
                        "Bolivia",
                        "Brasil",
                        "Colombia",
                        "Chile",
                        "Ecuador",
                        "Paraguay",
                        "Perú",
                        "Uruguay",
                        "Venezuela"),
                    "PJ" =
                      c(NROW(datarg17$Jornada),
                        NROW(datbol17$Jornada),
                        NROW(datbra17$Jornada),
                        NROW(datcol17$Jornada),
                        NROW(datchi17$Jornada),
                        NROW(datecu17$Jornada),
                        NROW(datpar17$Jornada),
                        NROW(datper17$Jornada),
                        NROW(daturu17$Jornada),
                        NROW(datven17$Jornada)),
                    "PTOS"=
                      c(sum(datarg17_l$VL*3,
                            datarg17_v$VV*3,
                            datarg17_l$EL*1,
                            datarg17_v$EV*1),
                        sum(datbol17_l$VL*3,
                            datbol17_v$VV*3,
                            datbol17_l$EL*1,
                            datbol17_v$EV*1),
                        sum(datbra17_l$VL*3,
                            datbra17_v$VV*3,
                            datbra17_l$EL*1,
                            datbra17_v$EV*1),
                        sum(datcol17_l$VL*3,
                            datcol17_v$VV*3,
                            datcol17_l$EL*1,
                            datcol17_v$EV*1),
                        sum(datchi17_l$VL*3,
                            datchi17_v$VV*3,
                            datchi17_l$EL*1,
                            datchi17_v$EV*1),
                        sum(datecu17_l$VL*3,
                            datecu17_v$VV*3,
                            datecu17_l$EL*1,
                            datecu17_v$EV*1),
                        sum(datpar17_l$VL*3,
                            datpar17_v$VV*3,
                            datpar17_l$EL*1,
                            datpar17_v$EV*1),
                        sum(datper17_l$VL*3,
                            datper17_v$VV*3,
                            datper17_l$EL*1,
                            datper17_v$EV*1),
                        sum(daturu17_l$VL*3,
                            daturu17_v$VV*3,
                            daturu17_l$EL*1,
                            daturu17_v$EV*1),
                        sum(datven17_l$VL*3,
                            datven17_v$VV*3,
                            datven17_l$EL*1,
                            datven17_v$EV*1)),
                    "GF"=
                      c(sum(datarg17_l$GL,
                            datarg17_v$GV),
                        sum(datbol17_l$GL,
                            datbol17_v$GV),
                        sum(datbra17_l$GL,
                            datbra17_v$GV),
                        sum(datcol17_l$GL,
                            datcol17_v$GV),
                        sum(datchi17_l$GL,
                            datchi17_v$GV),
                        sum(datecu17_l$GL,
                            datecu17_v$GV),
                        sum(datpar17_l$GL,
                            datpar17_v$GV),
                        sum(datper17_l$GL,
                            datper17_v$GV),
                        sum(daturu17_l$GL,
                            daturu17_v$GV),
                        sum(datven17_l$GL,
                            datven17_v$GV)),
                    "GC"=
                      c(sum(datarg17_l$GV,
                            datarg17_v$GL),
                        sum(datbol17_l$GV,
                            datbol17_v$GL),
                        sum(datbra17_l$GV,
                            datbra17_v$GL),
                        sum(datcol17_l$GV,
                            datcol17_v$GL),
                        sum(datchi17_l$GV,
                            datchi17_v$GL),
                        sum(datecu17_l$GV,
                            datecu17_v$GL),
                        sum(datpar17_l$GV,
                            datpar17_v$GL),
                        sum(datper17_l$GV,
                            datper17_v$GL),
                        sum(daturu17_l$GV,
                            daturu17_v$GL),
                        sum(datven17_l$GV,
                            datven17_v$GL))
)

# Tabla Acumulada 1raEtapa EQUIPOS, PJ, PTOS, GF, GC, GD
taeq1 <- mutate(taeq1, GD = GF - GC)
taeq2 <- mutate(taeq2, GD = GF - GC)
taeq3 <- mutate(taeq3, GD = GF - GC)
taeq4 <- mutate(taeq4, GD = GF - GC)
taeq5 <- mutate(taeq5, GD = GF - GC)
taeq6 <- mutate(taeq6, GD = GF - GC)
taeq7 <- mutate(taeq7, GD = GF - GC)
taeq8 <- mutate(taeq8, GD = GF - GC)
taeq9 <- mutate(taeq9, GD = GF - GC)
taeq10 <- mutate(taeq10, GD = GF - GC)
taeq11 <- mutate(taeq11, GD = GF - GC)
taeq12 <- mutate(taeq12, GD = GF - GC)
taeq13 <- mutate(taeq13, GD = GF - GC)
taeq14 <- mutate(taeq14, GD = GF - GC)
taeq15 <- mutate(taeq15, GD = GF - GC)
taeq16 <- mutate(taeq16, GD = GF - GC)
taeq17 <- mutate(taeq17, GD = GF - GC)
#taeq18 <- mutate(taeq18, GD = GF - GC)

# Tabla Acumulada 1raEtapa ORDENAR
taeq1 <- taeq1[order(-taeq1$PTOS, -taeq1$GD, -taeq1$GF), ]
taeq1 <- select(taeq1, EQUIPOS, PTOS, GD)
taeq2 <- taeq2[order(-taeq2$PTOS, -taeq2$GD, -taeq2$GF), ]
taeq2 <- select(taeq2, EQUIPOS, PTOS, GD)
taeq3 <- taeq3[order(-taeq3$PTOS, -taeq3$GD, -taeq3$GF), ]
taeq3 <- select(taeq3, EQUIPOS, PTOS, GD)
taeq4 <- taeq4[order(-taeq4$PTOS, -taeq4$GD, -taeq4$GF), ]
taeq4 <- select(taeq4, EQUIPOS, PTOS, GD)
taeq5 <- taeq5[order(-taeq5$PTOS, -taeq5$GD, -taeq5$GF), ]
taeq5 <- select(taeq5, EQUIPOS, PTOS, GD)
taeq6 <- taeq6[order(-taeq6$PTOS, -taeq6$GD, -taeq6$GF), ]
taeq6 <- select(taeq6, EQUIPOS, PTOS, GD)
taeq7 <- taeq7[order(-taeq7$PTOS, -taeq7$GD, -taeq7$GF), ]
taeq7 <- select(taeq7, EQUIPOS, PTOS, GD)
taeq8 <- taeq8[order(-taeq8$PTOS, -taeq8$GD, -taeq8$GF), ]
taeq8 <- select(taeq8, EQUIPOS, PTOS, GD)
taeq9 <- taeq9[order(-taeq9$PTOS, -taeq9$GD, -taeq9$GF), ]
taeq9 <- select(taeq9, EQUIPOS, PTOS, GD)
taeq10 <- taeq10[order(-taeq10$PTOS, -taeq10$GD, -taeq10$GF), ]
taeq10 <- select(taeq10, EQUIPOS, PTOS, GD)
taeq11 <- taeq11[order(-taeq11$PTOS, -taeq11$GD, -taeq11$GF), ]
taeq11 <- select(taeq11, EQUIPOS, PTOS, GD)
taeq12 <- taeq12[order(-taeq12$PTOS, -taeq12$GD, -taeq12$GF), ]
taeq12 <- select(taeq12, EQUIPOS, PTOS, GD)
taeq13 <- taeq13[order(-taeq13$PTOS, -taeq13$GD, -taeq13$GF), ]
taeq13 <- select(taeq13, EQUIPOS, PTOS, GD)
taeq14 <- taeq14[order(-taeq14$PTOS, -taeq14$GD, -taeq14$GF), ]
taeq14 <- select(taeq14, EQUIPOS, PTOS, GD)
taeq15 <- taeq15[order(-taeq15$PTOS, -taeq15$GD, -taeq15$GF), ]
taeq15 <- select(taeq15, EQUIPOS, PTOS, GD)
taeq16 <- taeq16[order(-taeq16$PTOS, -taeq16$GD, -taeq16$GF), ]
taeq16 <- select(taeq16, EQUIPOS, PTOS, GD)
taeq17 <- taeq17[order(-taeq17$PTOS, -taeq17$GD, -taeq17$GF), ]
taeq17 <- select(taeq17, EQUIPOS, PTOS, GD)
#taeq18 <- taeq18[order(-taeq18$PTOS, -taeq18$GD, -taeq18$GF), ]
#taeq18 <- select(taeq18, EQUIPOS, PTOS, GD)

# Tabla Acumulada 1raEtapa Filas ordenadas
rownames(taeq1) <- 1:nrow(taeq1)
taeq1 <- mutate(taeq1, J1 = rownames(taeq1))
rownames(taeq2) <- 1:nrow(taeq2)
taeq2 <- mutate(taeq2, J2 = rownames(taeq2))
rownames(taeq3) <- 1:nrow(taeq3)
taeq3 <- mutate(taeq3, J3 = rownames(taeq3))
rownames(taeq4) <- 1:nrow(taeq4)
taeq4 <- mutate(taeq4, J4 = rownames(taeq4))
rownames(taeq5) <- 1:nrow(taeq5)
taeq5 <- mutate(taeq5, J5 = rownames(taeq5))
rownames(taeq6) <- 1:nrow(taeq6)
taeq6 <- mutate(taeq6, J6 = rownames(taeq6))
rownames(taeq7) <- 1:nrow(taeq7)
taeq7 <- mutate(taeq7, J7 = rownames(taeq7))
rownames(taeq8) <- 1:nrow(taeq8)
taeq8 <- mutate(taeq8, J8 = rownames(taeq8))
rownames(taeq9) <- 1:nrow(taeq9)
taeq9 <- mutate(taeq9, J9 = rownames(taeq9))
rownames(taeq10) <- 1:nrow(taeq10)
taeq10 <- mutate(taeq10, J10 = rownames(taeq10))
rownames(taeq11) <- 1:nrow(taeq11)
taeq11 <- mutate(taeq11, J11 = rownames(taeq11))
rownames(taeq12) <- 1:nrow(taeq12)
taeq12 <- mutate(taeq12, J12 = rownames(taeq12))
rownames(taeq13) <- 1:nrow(taeq13)
taeq13 <- mutate(taeq13, J13 = rownames(taeq13))
rownames(taeq14) <- 1:nrow(taeq14)
taeq14 <- mutate(taeq14, J14 = rownames(taeq14))
rownames(taeq15) <- 1:nrow(taeq15)
taeq15 <- mutate(taeq15, J15 = rownames(taeq15))
rownames(taeq16) <- 1:nrow(taeq16)
taeq16 <- mutate(taeq16, J16 = rownames(taeq16))
rownames(taeq17) <- 1:nrow(taeq17)
taeq17 <- mutate(taeq17, J17 = rownames(taeq17))
#rownames(taeq18) <- 1:nrow(taeq18)
#taeq18 <- mutate(taeq18, J18 = rownames(taeq18))

# UNION
taeqa = full_join(taeq1, taeq2, by = "EQUIPOS")
taeqb = full_join(taeqa, taeq3, by = "EQUIPOS")
taeqc = full_join(taeqb, taeq4, by = "EQUIPOS")
taeqd = full_join(taeqc, taeq5, by = "EQUIPOS")
taeqe = full_join(taeqd, taeq6, by = "EQUIPOS")
taeqf = full_join(taeqe, taeq7, by = "EQUIPOS")
taeqg = full_join(taeqf, taeq8, by = "EQUIPOS")
taeqh = full_join(taeqg, taeq9, by = "EQUIPOS")
taeqi = full_join(taeqh, taeq10, by = "EQUIPOS")
taeqj = full_join(taeqi, taeq11, by = "EQUIPOS")
taeqk = full_join(taeqj, taeq12, by = "EQUIPOS")
taeql = full_join(taeqk, taeq13, by = "EQUIPOS")
taeqm = full_join(taeql, taeq14, by = "EQUIPOS")
taeqn = full_join(taeqm, taeq15, by = "EQUIPOS")
taeqo = full_join(taeqn, taeq16, by = "EQUIPOS")
taeqp = full_join(taeqo, taeq17, by = "EQUIPOS")
#taeqq = full_join(taeqp, taeq18, by = "EQUIPOS")
taeqx = select(taeqp, "EQUIPOS", "J1", "J2", "J3", "J4", "J5", "J6", "J7",
               "J8", "J9", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17")
taeqx <- gather(taeqx,
               key = "variable",
               value = "value",
               J1:J17)

# Save totx data.frame as txt file
write.table(taeqx,"teli.txt",sep="\t",row.names=FALSE)




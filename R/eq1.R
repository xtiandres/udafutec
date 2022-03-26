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
dateq1 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq1.txt")
dateq2 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq2.txt")
dateq3 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq3.txt")
dateq4 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq4.txt")
dateq5 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq5.txt")
dateq6 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq6.txt")
dateq7 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq7.txt")
dateq8 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq8.txt")
dateq9 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq9.txt")
dateq10 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq10.txt")
dateq11 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq11.txt")
dateq12 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq12.txt")
dateq13 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq13.txt")
dateq14 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq14.txt")
dateq15 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq15.txt")
dateq16 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq16.txt")
dateq17 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq17.txt")

# ARGENTINA
datarg <- filter(dateq1,
                 Local == "Argentina" | Visita == "Argentina")
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
datarg2 <- filter(dateq2,
                 Local == "Argentina" | Visita == "Argentina")
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
datarg3 <- filter(dateq3,
                  Local == "Argentina" | Visita == "Argentina")
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
datarg4 <- filter(dateq4,
                  Local == "Argentina" | Visita == "Argentina")
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
datarg5 <- filter(dateq5,
                  Local == "Argentina" | Visita == "Argentina")
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

# BOLIVIA
datbol <- filter(dateq1,
                 Local == "Bolivia" | Visita == "Bolivia")
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
datbol2 <- filter(dateq2,
                 Local == "Bolivia" | Visita == "Bolivia")
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
datbol3 <- filter(dateq3,
                  Local == "Bolivia" | Visita == "Bolivia")
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
datbol4 <- filter(dateq4,
                  Local == "Bolivia" | Visita == "Bolivia")
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
datbol5 <- filter(dateq5,
                  Local == "Bolivia" | Visita == "Bolivia")
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

# BRASIL
datbra <- filter(dateq1,
                 Local == "Brasil" | Visita == "Brasil")
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
datbra2 <- filter(dateq2,
                 Local == "Brasil" | Visita == "Brasil")
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
datbra3 <- filter(dateq3,
                  Local == "Brasil" | Visita == "Brasil")
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
datbra4 <- filter(dateq4,
                  Local == "Brasil" | Visita == "Brasil")
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
datbra5 <- filter(dateq5,
                  Local == "Brasil" | Visita == "Brasil")
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

# COLOMBIA
datcol <- filter(dateq1,
                 Local == "Colombia" | Visita == "Colombia")
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
datcol2 <- filter(dateq2,
                 Local == "Colombia" | Visita == "Colombia")
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
datcol3 <- filter(dateq3,
                  Local == "Colombia" | Visita == "Colombia")
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
datcol4 <- filter(dateq4,
                  Local == "Colombia" | Visita == "Colombia")
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
datcol5 <- filter(dateq5,
                  Local == "Colombia" | Visita == "Colombia")
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

# CHILE
datchi <- filter(dateq1,
                 Local == "Chile" | Visita == "Chile")
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
datchi2 <- filter(dateq2,
                 Local == "Chile" | Visita == "Chile")
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
datchi3 <- filter(dateq3,
                  Local == "Chile" | Visita == "Chile")
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
datchi4 <- filter(dateq4,
                  Local == "Chile" | Visita == "Chile")
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
datchi5 <- filter(dateq5,
                  Local == "Chile" | Visita == "Chile")
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

# ECUADOR
datecu <- filter(dateq1,
                 Local == "Ecuador" | Visita == "Ecuador")
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
datecu2 <- filter(dateq2,
                 Local == "Ecuador" | Visita == "Ecuador")
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
datecu3 <- filter(dateq3,
                  Local == "Ecuador" | Visita == "Ecuador")
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
datecu4 <- filter(dateq4,
                  Local == "Ecuador" | Visita == "Ecuador")
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
datecu5 <- filter(dateq5,
                  Local == "Ecuador" | Visita == "Ecuador")
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

# PARAGUAY
datpar <- filter(dateq1,
                 Local == "Paraguay" | Visita == "Paraguay")
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
datpar2 <- filter(dateq2,
                 Local == "Paraguay" | Visita == "Paraguay")
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
datpar3 <- filter(dateq3,
                  Local == "Paraguay" | Visita == "Paraguay")
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
datpar4 <- filter(dateq4,
                  Local == "Paraguay" | Visita == "Paraguay")
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
datpar5 <- filter(dateq5,
                  Local == "Paraguay" | Visita == "Paraguay")
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

# PERÚ
datper <- filter(dateq1,
                 Local == "Perú" | Visita == "Perú")
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
datper2 <- filter(dateq2,
                 Local == "Perú" | Visita == "Perú")
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
datper3 <- filter(dateq3,
                  Local == "Perú" | Visita == "Perú")
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
datper4 <- filter(dateq4,
                  Local == "Perú" | Visita == "Perú")
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
datper5 <- filter(dateq5,
                  Local == "Perú" | Visita == "Perú")
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

# URUGUAY
daturu <- filter(dateq1,
                 Local == "Uruguay" | Visita == "Uruguay")
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
daturu2 <- filter(dateq2,
                 Local == "Uruguay" | Visita == "Uruguay")
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
daturu3 <- filter(dateq3,
                  Local == "Uruguay" | Visita == "Uruguay")
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
daturu4 <- filter(dateq4,
                  Local == "Uruguay" | Visita == "Uruguay")
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
daturu5 <- filter(dateq5,
                  Local == "Uruguay" | Visita == "Uruguay")
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

# VENEZUELA
datven <- filter(dateq1,
                 Local == "Venezuela" | Visita == "Venezuela")
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
datven2 <- filter(dateq2,
                 Local == "Venezuela" | Visita == "Venezuela")
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
datven3 <- filter(dateq3,
                  Local == "Venezuela" | Visita == "Venezuela")
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
datven4 <- filter(dateq4,
                  Local == "Venezuela" | Visita == "Venezuela")
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
datven5 <- filter(dateq5,
                  Local == "Venezuela" | Visita == "Venezuela")
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

# Tabla Acumulada 1raEtapa EQUIPOS, PJ, PTOS, GF, GC, GD
taeq1 <- mutate(taeq1, GD = GF - GC)
taeq2 <- mutate(taeq2, GD = GF - GC)
taeq3 <- mutate(taeq3, GD = GF - GC)
taeq4 <- mutate(taeq4, GD = GF - GC)
taeq5 <- mutate(taeq5, GD = GF - GC)
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




# UNION
taeqa = full_join(taeq1, taeq2, by = "EQUIPOS")
taeqb = full_join(taeqa, taeq3, by = "EQUIPOS")
taeqc = full_join(taeqb, taeq4, by = "EQUIPOS")
taeqd = full_join(taeqc, taeq5, by = "EQUIPOS")
taeqx = select(taeqd, "EQUIPOS", "J1", "J2", "J3", "J4", "J5")
taeqx <- gather(taeqx,
               key = "variable",
               value = "value",
               J1:J5)

# Save totx data.frame as txt file
write.table(taeqx,"teli.txt",sep="\t",row.names=FALSE)




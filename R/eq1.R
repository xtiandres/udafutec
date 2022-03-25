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

# DATASET
dateq1 <- read_tsv("/home/xut/Documents/udaviz/R/studio/udafutec/data/eq1.txt")

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

# Tabla Acumulada 1raEtapa EQUIPOS, PJ, PTOS, GF, GC, GD
taeq1 <- mutate(taeq1, GD = GF - GC)
# Tabla Acumulada 1raEtapa ORDENAR
taeq1 <- taeq1[order(-taeq1$PTOS, -taeq1$GD, -taeq1$GF), ]
taeq1 <- select(taeq1, EQUIPOS, PTOS, GD)
# Tabla Acumulada 1raEtapa Filas ordenadas
rownames(taeq1) <- 1:nrow(taeq1)
taeq1 <- mutate(taeq1, J1 = rownames(taeq1))
#taba1$J1 <- as.numeric(taba1$J1)
#a1 <- ggplot(taba1, aes(x = EQUIPOS, y = J1, color = EQUIPOS)) 
#a1 +
#  geom_point(size = 2) +
#aes(y = fct_inorder(row.names())) +
#  theme_minimal()
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
dabsc <- dace22 %>% 
  filter(Local == "Barcelona SC" | Visita == "Barcelona SC") %>%
  filter(Jornada %in% c(1,2))

dalocal <- function(dabsc, VL, EL, DL){
  dabsc %>%
    filter(dabsc$Local == "Barcelona SC") %>%
    mutate(!! (VL) := !!rlang::sym(if(dabsc$GL>dabsc$GV)  1)) %>%
    mutate(!! (EL) := !!rlang::sym(if(dabsc$GL == dabsc$GV) 1)) %>%
    mutate(!! (DL) := !!rlang::sym(if(dabsc$GL < dabsc$GV) 1))
  return(data.frame(dalocal_equipo))
}
f.tidy <- function(df, ocl, ncl) {
  df %>%
    mutate(!! (ncl) := !!rlang::sym(ocl) - 1)
}
#ocl <- "pri"
#ncl <- "cua"
VL <- "VL"
EL <- "EL"
DL <- "DL"
#f.tidy(df, ocl, ncl)
dalocal(dabsc, VL, EL, DL)
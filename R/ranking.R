# FIFA RANKING 2018 - 2022

library(readr)
library(dplyr)
library(tidyr)

# DATASET
drf <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifatotal1.csv")
drf1 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ft1.csv")
drf2 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ft2.csv")
drf3 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ft3.csv")
drf4 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ft4.csv")
drf5 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ft5.csv")

#drf <- gather(drf,
#              key = "variable",
#              value = "fecha",
#              Rank)
#drf <- group_by(drf, Fecha)
#drf1 <-filter(drf, Nombre == "France")

drft <- full_join(drf1, drf2, by = "Nombre") %>%
  full_join(., drf3, by = "Nombre") %>% full_join(., drf4, by = "Nombre") %>%
  full_join(., drf5, by = "Nombre")

colnames(drft) <- c("Seleccion", "2022", "F1", "2021", "F2", "2020", "F3", "2019", "F4", "2018", "F5")
drft1 <- select(drft, -F1, -F2, -F3, -F4, -F5)
write.table(drft1,"rankfifa.txt",sep="\t",row.names=FALSE)
#drft2 <- select(drft, Rank.x, Rank.y, Rank.xx, Rank.yy, Rank)

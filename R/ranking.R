# FIFA RANKING 2018 - 2022

library(readr)
library(dplyr)
library(tidyr)

# DATASET
drf1 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fago18.csv")
drf2 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fsep18.csv")
drf3 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/foct18.csv")
drf4 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fnov18.csv")
drf5 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fdic18.csv")
drf6 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/ffeb19.csv")
drf7 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fabr19.csv")
drf8 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fjun19.csv")
drf9 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fjul19.csv")
drf10 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fsep19.csv")
drf11 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/foct19.csv")
drf12 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fnov19.csv")
drf13 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fdic19.csv")
drf14 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/ffeb20.csv")
drf15 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fabr20.csv")
drf16 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fjun20.csv")
drf17 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fjul20.csv")
drf18 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fsep20.csv")
drf19 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/foct20.csv")
drf20 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fnov20.csv")
drf21 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fdic20.csv")
drf22 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/ffeb21.csv")
drf23 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fabr21.csv")
drf24 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fmay21.csv")
drf25 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fago21.csv")
drf26 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fsep21.csv")
drf27 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/foct21.csv")
drf28 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fnov21.csv")
drf29 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fdic21.csv")
drf30 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/ffeb22.csv")
drf31 <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/fifa/fmar22.csv")


#drf <- gather(drf,
#              key = "variable",
#              value = "fecha",
#              Rank)
#drf <- group_by(drf, Fecha)
#drf1 <-filter(drf, Nombre == "France")

drft <- full_join(drf1, drf2, by = "Nombre") %>%
  full_join(., drf3, by = "Nombre") %>% full_join(., drf4, by = "Nombre") %>%
  full_join(., drf5, by = "Nombre") %>% full_join(., drf6, by = "Nombre") %>%
  full_join(., drf7, by = "Nombre") %>% full_join(., drf8, by = "Nombre") %>%
  full_join(., drf9, by = "Nombre") %>% full_join(., drf10, by = "Nombre") %>%
  full_join(., drf11, by = "Nombre") %>% full_join(., drf12, by = "Nombre") %>%
  full_join(., drf13, by = "Nombre") %>% full_join(., drf14, by = "Nombre") %>%
  full_join(., drf15, by = "Nombre") %>% full_join(., drf16, by = "Nombre") %>%
  full_join(., drf17, by = "Nombre") %>% full_join(., drf18, by = "Nombre") %>%
  full_join(., drf19, by = "Nombre") %>% full_join(., drf20, by = "Nombre") %>%
  full_join(., drf21, by = "Nombre") %>% full_join(., drf22, by = "Nombre") %>%
  full_join(., drf23, by = "Nombre") %>% full_join(., drf24, by = "Nombre") %>%
  full_join(., drf25, by = "Nombre") %>% full_join(., drf26, by = "Nombre") %>%
  full_join(., drf27, by = "Nombre") %>% full_join(., drf28, by = "Nombre") %>%
  full_join(., drf29, by = "Nombre") %>% full_join(., drf30, by = "Nombre") %>%
  full_join(., drf31, by = "Nombre")

colnames(drft) <- c("Seleccion", 
                    "Ago18", "X1", "Sep18", "X2", "Oct18", "X3", "Nov18", "X4", "Dic18", "X5",
                    "Feb19", "X6", "Abr19", "X7", "Jun19", "X8", "Jul19", "X9",
                    "Sep19", "X10", "Oct19", "X11", "Nov19", "X12", "Dic19", "X13",
                    "Feb20", "X14", "Abr20", "X15", "Jun20", "X16", "Jul20", "X17",
                    "Sep20", "X18", "Oct20", "X19", "Nov20", "X20", "Dic20", "X21",
                    "Feb21", "X22", "Abr21", "X23", "May21", "X24", "Ago21", "X25",
                    "Sep21", "X26", "Oct21", "X27", "Nov21", "X28", "Dic21", "X29",
                    "Feb22", "X30", "Mar22", "X31")

drftt <- select(drft, -starts_with("X"))

write.table(drftt,"fifa.txt",sep="\t",row.names=FALSE)


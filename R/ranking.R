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
  full_join(., drf29, by = "Nombre") %>% full_join(., drf30, by = "Nombre")

colnames(drft) <- c("Seleccion", 
                    "Ago18", "F1", "Sep18", "F2", "Oct18", "F3", "Nov18", "F4", "Dic18", "F5",
                    "Feb19", "F6", "Abr19", "F7", "Jun19", "F8", "Jul19", "F9",
                    "Sep19", "F10", "Oct19", "F11", "Nov19", "F12", "Dic19", "F13",
                    "Feb20", "F14", "Abr20", "F15", "Jun20", "F16", "Jul20", "F17",
                    "Sep20", "F18", "Oct20", "F19", "Nov20", "F20", "Dic20", "F21",
                    "Feb21", "F22", "Abr21", "F23", "May21", "F24", "Ago21", "F25",
                    "Sep21", "F26", "Oct21", "F27", "Nov21", "F28", "Dic21", "F29",
                    "Feb22", "F30")

drftt <- select(drft, -F1, -F2, -F3, -F4, -F5, -F6, -F7, -F8, -F9, -F10,
                -F11, -F12, -F13, -F14, -F15, -F16, -F17, -F18, -F19, -F20,
                -F21, -F22, -F23, -F24, -F25, -F26, -F27, -F28, -F29, -F30)

write.table(drftt,"fifa.txt",sep="\t",row.names=FALSE)


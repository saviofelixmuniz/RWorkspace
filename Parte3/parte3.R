library(readr)
library(dplyr)

setwd("/home/saviofcm/workspace/R/Parte3")
graduados <- read_csv("graduados_treino.csv", col_types = cols(ALU_NOVAMATRICULA = col_character())) %>%
  mutate(matricula = as.factor(ALU_NOVAMATRICULA))
head(graduados)

str(graduados)
summary(graduados)
View(graduados)
graduados <- graduados %>% arrange(matricula)

graduados.clean <- graduados %>%
  filter(!is.na(MAT_MEDIA_FINAL))

summary(graduados.clean)
View(graduados.clean)
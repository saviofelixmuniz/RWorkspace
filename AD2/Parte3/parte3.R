####################### PRE-PROCESSAMENTO #########################
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

graduados.cra <- graduados.clean %>%
  group_by(ALU_NOVAMATRICULA) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))

head(graduados.cra)

library(reshape2)

graduados.model.input <- graduados.clean %>%
  group_by(ALU_NOVAMATRICULA,DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(ALU_NOVAMATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(ALU_NOVAMATRICULA ~ DISCIPLINA, mean) %>%
  merge(graduados.cra)

head(graduados.model.input)
View(graduados.model.input)

############################################################################

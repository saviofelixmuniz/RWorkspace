####################### PRE-PROCESSAMENTO #########################
library(readr)
library(dplyr)

setwd("~/workspace/R/AD2/Parte3")
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

install.packages("ISLR")
library(ISLR)
library(caret)

graduados.model.input <- na.omit(graduados.model.input)

graduados.selected <- graduados.model.input %>% 
  select(Cálculo.Diferencial.e.Integral.I,
         Álgebra.Vetorial.e.Geometria.Analítica,
         Leitura.e.Produção.de.Textos,
         Programação.I,
         Introdução.à.Computação,
         Laboratório.de.Programação.II,
         Cálculo.Diferencial.e.Integral.II,
         Matemática.Discreta,
         Programação.II,
         Teoria.dos.Grafos,
         Fundamentos.de.Física.Clássica,
         Laboratório.de.Programação.II,
         cra)

set.seed(825)

fitControl <- trainControl(method = "cv",
                           number = 10)

lambdaGrid <- expand.grid(lambda = 10^seq(2, -2, length=100))

ridge <- train(cra~., data = graduados.selected,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)

ridge

validacao <- read.csv("graduados_teste.csv")

validacao <- validacao %>% arrange(ALU_NOVAMATRICULA)
validacao.clean <- validacao %>%
  filter(!is.na(MAT_MEDIA_FINAL))
validacao.cra <- validacao.clean %>%
  group_by(ALU_NOVAMATRICULA) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))
validacao.cra <- validacao.clean %>%
  group_by(ALU_NOVAMATRICULA,DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(ALU_NOVAMATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(ALU_NOVAMATRICULA ~ DISCIPLINA, mean) %>%
  merge(validacao.cra)

validacao.cra <- na.omit(validacao.cra)

ridge.pred <- predict(ridge, validacao.cra)
sqrt(mean(ridge.pred - validacao.cra$cra)^2)

plot(ridge)

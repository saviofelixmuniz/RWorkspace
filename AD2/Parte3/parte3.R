################### PRE-PROCESSAMENTO DO TREINO ####################
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

write.csv(graduados.selected,file = "graduadosSelected.csv")

################# PRE PROCESSAMENTO DO TESTE #################
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

write.csv(validacao.cra,file = "validacaoCRA.csv")

############################# RIDGE ######################################

library(ISLR)
library(caret)

set.seed(825)

fitControl <- trainControl(method = "cv",
                           number = 10)

lambdaGrid <- expand.grid(lambda = 10^seq(-3, -4, length=200))

ridge <- train(cra~., data = graduados.selected,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)

ridge

ridge.pred <- predict(ridge, validacao.cra)
sqrt(mean(ridge.pred - validacao.cra$cra)^2)

plot(ridge)

#################### LASSO ########################

lasso <- train(cra ~., graduados.selected,
               method='lasso',
               preProc=c('scale','center'),
               trControl=fitControl)
lasso
predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, mode='fraction')
lasso.pred <- predict(lasso, validacao.cra)
sqrt(mean(lasso.pred - validacao.cra$cra)^2)

##################  LM  ############################

lmfit <- train(cra ~., data = graduados.selected,
               method='lm',
               trControl = fitControl,
               preProc=c('scale', 'center'))

lmfit
coef(lmfit$finalModel)
lmfit.pred <- predict(lmfit, validacao.cra)
sqrt(mean(lmfit.pred - validacao.cra$cra)^2)
############## PLOTS DE RELEVÂNCIA DE VARIÁVEIS #############################

plot(varImp(ridge, scale = FALSE))

plot(varImp(lasso, scale = FALSE))

plot(varImp(lmfit, scale = FALSE))

############################################################################

graduados.important <- graduados.selected %>% 
select(Matemática.Discreta,
       Programação.II,
       Teoria.dos.Grafos,
       Fundamentos.de.Física.Clássica,
       cra)

graduados.important <- transform(graduados.important, discreta.prog2 = Matemática.Discreta*Programação.II)
graduados.important <- transform(graduados.important, prog2 = Programação.II^2)
graduados.important <- transform (graduados.important, fisicasqrt = Fundamentos.de.Física.Clássica^(1/2))

graduados.important

validacao.cra <- transform (validacao.cra, discreta.prog2 = Matemática.Discreta*Programação.II)
validacao.cra <- transform (validacao.cra, prog2 = Programação.II^2)
validacao.cra <- transform (validacao.cra, fisicasqrt = Fundamentos.de.Física.Clássica^(1/2))

library(dplyr)

testeKaggle.matricula <- read.csv("/home/saviomuniz/Downloads/test.csv")
testeKaggle <- testeKaggle %>% select(Matemática.Discreta,
                                      Programação.II,
                                      Teoria.dos.Grafos,
                                      Fundamentos.de.Física.Clássica)

testeKaggle = transform(testeKaggle, Matemática.Discreta = ifelse(is.na(Matemática.Discreta), mean(Matemática.Discreta, na.rm=TRUE), Matemática.Discreta))
testeKaggle = transform(testeKaggle, Fundamentos.de.Física.Clássica = ifelse(is.na(Fundamentos.de.Física.Clássica), mean(Fundamentos.de.Física.Clássica, na.rm=TRUE), Fundamentos.de.Física.Clássica))
testeKaggle = transform(testeKaggle, Programação.II = ifelse(is.na(Programação.II), mean(Programação.II, na.rm=TRUE), Programação.II))
testeKaggle = transform(testeKaggle, Teoria.dos.Grafos = ifelse(is.na(Teoria.dos.Grafos), mean(Teoria.dos.Grafos, na.rm=TRUE), Teoria.dos.Grafos))

testeKaggle <- transform (testeKaggle, discreta.prog2 = Matemática.Discreta*Programação.II)
testeKaggle <- transform (testeKaggle, prog2 = Programação.II^2)
testeKaggle <- transform (testeKaggle, fisicasqrt = Fundamentos.de.Física.Clássica^(1/2))

na.omit(testeKaggle)
ridgeImproved <- train(cra~., data = graduados.important,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)

ridgeImproved

ridge.pred <- predict(ridgeImproved, testeKaggle)

respostaKaggle <- NULL

respostaKaggle$matricula <- testeKaggle.matricula$matricula
respostaKaggle$cra <- ridge.pred
respostaKaggle <- respostaKaggle %>% select (matricula,cra)
  
write.csv(respostaKaggle, file="respostaKaggle.csv")
sqrt(mean(ridge.pred - validacao.cra$cra)^2)

#0.0254
#0.02038744
#0.01993468
#0.01247922




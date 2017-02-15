data <- read.csv("/home/saviofcm/workspace/R/AD2/Lab3/Parte1/treino_classificacao.csv")

library('dplyr')
library('ggplot2')
library('reshape')

data.evasoes <- data %>% group_by(MAT_TUR_PERIODO) %>% summarise(NumeroEvasoes = sum(EVADIU))

View(data.evasoes)

ggplot(data.evasoes, aes(x = MAT_TUR_ANO, y = NumeroEvasoes)) + geom_bar(stat = "identity")

data.evasoes.totais <- data %>% group_by(EVADIU) %>% summarise(Quantidade = n())

ggplot(data.evasoes.totais, aes(x = EVADIU, y = Quantidade)) + geom_bar(stat = "identity")

data.grouped.matricula <- data %>% group_by(MAT_ALU_MATRICULA) %>% 
                          summarise(AbaixoMedia = mean(MAT_MEDIA_FINAL,na.rm = TRUE) < 4,Evadiu = max(EVADIU) == 1)

ggplot(data.evasoes.totais, aes(x = EVADIU, y = Quantidade)) + geom_bar(stat = "identity")

g <- ggplot(data.grouped.matricula, aes(AbaixoMedia5))
g + geom_bar(aes(fill = Evadiu), position = "fill")

data$disciplina <- gsub(" ", ".", data$disciplina)
subject.wide <- cast(data, MAT_ALU_MATRICULA~disciplina, mean, value= "MAT_MEDIA_FINAL")
subject.wide$MEDIADME <- (subject.wide$Cálculo.Diferencial.e.Integral.I + subject.wide$Álgebra.Vetorial.e.Geometria.Analítica)/2

olindo <- left_join(subject.wide, data.grouped.matricula, "MAT_ALU_MATRICULA")


ggplot(olindo, aes(MEDIADME)) +
  geom_freqpoly() + 
  facet_wrap(~ Evadiu, scales = "free_y")

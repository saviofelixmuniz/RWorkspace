setwd("/home/saviofcm/workspace/R/AD2/Lab4/")
notas <- read.csv("lab4_part1_data.csv")
library(dplyr)
library(tidyr)
library(recommenderlab)

#Treino e teste (2013.2,2014.1 e 2014.2 teste)

test_compare <- notas %>% filter(ALU_ANO_INGRESSO == 2014 | (ALU_ANO_INGRESSO == 2013 & ALU_PERIODO_INGRESSO == 2))
treino <- notas %>% filter (ALU_ANO_INGRESSO <= 2012 | (ALU_ANO_INGRESSO == 2013 & ALU_PERIODO_INGRESSO == 1))

#Tirar as disciplinas do quarto do teste

test_missing <- notas %>% select(-Lógica.Matemática, -Lab..de.Org..e.Arquitetura.de.Computadores,
                           -Org..e.Arquitetura.de.Computadores.I,-Sistemas.de.Informação.I,
                           -Paradigmas.de.Ling..de.Programação,-Métodos.Estatisticos,-Engenharia.de.Software.I)
#Wide pra long

treino_long <- gather(treino, disciplina, nota, Álgebra.Linear.I:Teoria.dos.Grafos, factor_key = TRUE)
test_compare_long <- gather(test_compare, disciplina, nota, Álgebra.Linear.I:Teoria.dos.Grafos, factor_key = TRUE)
test_missing_long <- gather(test_missing, disciplina, nota, Álgebra.Linear.I:Teoria.dos.Grafos, factor_key = TRUE)


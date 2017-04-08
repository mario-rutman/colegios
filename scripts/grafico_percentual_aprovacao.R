# probabilidade de passar em uma materia dado que o aluno tirou nota x na primeira prova

library(tidyverse)
library(magrittr)
library(stringr)
library(ROCR)

df_2014 <- read_csv2("dados/col_piloto_2014.csv",
                     locale = locale(encoding = "ISO-8859-1"))
df_2014 <- df_2014[, -1]
df_2014$ano <- 2014

df_2015 <- read_csv2("dados/col_piloto_2015.csv",
                     locale = locale(encoding = "ISO-8859-1"))
df_2015$ano <- 2015

df_2016 <- read_csv2("dados/col_piloto_2016.csv",
                     locale = locale(encoding = "ISO-8859-1"))
df_2016$ano <- 2016


df_2017 <- read_csv2("dados/col_piloto_2017.csv",
                     locale = locale(encoding = "ISO-8859-1"))
df_2017$ano <- 2017

df <- bind_rows(df_2014, df_2015, df_2016, df_2017)
# ajeitar nome de ensino
df$ensino %<>% str_to_lower()

# adicionar coluna de grupo das disciplinas
vetor_disciplinas <- (unique(df$disciplina))
grupo_disciplina <- c(rep("G1", 3), rep("G2", 3), rep("G3", 2), rep("G4", 2))

df_disciplina <- data.frame(disciplina = vetor_disciplinas,
                            grupo_disciplina = grupo_disciplina,
                            stringsAsFactors = FALSE)

df %<>% left_join(df_disciplina, by = "disciplina")

# para o caso de o aluno ter conseguido 21 pontos em p1 + p2 + p3, remover pf
df %<>% 
  group_by(id_aluno, ensino, turma, disciplina) %>%
  mutate(nota_acum = cumsum(nota)) %>%
  ungroup() %>%
  mutate(nota_acum = ifelse(prova == "pf" & nota_acum >= 21, NA, nota_acum)) %>%
  #group_by(aluno, disciplina) %>%
  mutate(nota_acum = ifelse(prova == "pf" & lag(nota_acum) >= 21, NA,
                            nota + lag(nota_acum))) %>%
  # binario do passou de ano
  group_by(id_aluno, ensino, turma, disciplina) %>%
  mutate(nota_final = max(nota_acum, na.rm = TRUE),
         passou_de_ano = ifelse(nota_final >= 21, 1, 0))

# ver porcentual de alunos reprovados por ensino - turma
df %>%
  group_by(id_aluno, ensino, turma, grupo_disciplina, disciplina) %>%
  summarise(passou_de_ano = max(passou_de_ano)) %>%
  group_by(ensino, grupo_disciplina, disciplina) %>%
  summarise(porc_reprov = round(100 - 100 *  mean(passou_de_ano))) %>%
  ggplot(aes(x = disciplina, y = porc_reprov, fill = grupo_disciplina)) +
    geom_bar(stat = "identity") +
    facet_grid(ensino ~ .)

# regressao logistica p1 ~ passou de ano
df_p1 <- df %>% filter(prova == "p1")
df_p1_treino <- df_p1 %>% filter(ano != 2017)
df_p1_teste <- df_p1 %>% filter(ano == 2017)

model <- glm(passou_de_ano ~ nota, data = df_p1_treino, family=binomial(link='logit'))

fitted.results <- predict(model, newdata = df_p1_teste,
                          type='response')
fitted.results.bin <- ifelse(fitted.results > 0.5,1,0)

# confusion matrix
confmat <- table(df_p1_teste$passou_de_ano, fitted.results.bin)
confmat
confmat/length(fitted.results.bin)
# coeficientes da matriz de confusao
a = confmat[1,1]
b = confmat[1,2]
c = confmat[2,1]
d = confmat[2,2]

tnr = a/(a+b)
fpr = b/(a+b)

tpr = d/(c+d)
fnr = c/(c+d)

# acuracia da matriz de confusao
(tpr + tnr)/(tnr + fpr + tpr + fnr)

# Acurácia do modelo:
mean(fitted.results.bin == df_p1_teste$passou_de_ano)

# exibir curva ROC
pr <- prediction(fitted.results.bin, df_p1_teste$passou_de_ano)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# exibir grafico ggplot
df_p1_teste$predicted_prob <- fitted.results

ggplot(df_p1_teste, aes(x = nota, y = predicted_prob)) +
  geom_point() + geom_line()



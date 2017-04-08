# probabilidade de passar em uma materia dado que o aluno tirou nota x na primeira prova

library(tidyverse)

df_2014 <- read_csv2("dados/col_piloto_2014.csv",
                     locale = locale(encoding = "ISO-8859-1"))
df_2014 <- df_2014[, -1]

df_2015 <- read_csv2("dados/col_piloto_2015.csv",
                     locale = locale(encoding = "ISO-8859-1"))

df_2016 <- read_csv2("dados/col_piloto_2016.csv",
                     locale = locale(encoding = "ISO-8859-1"))

df <- bind_rows(df_2014, df_2015, df_2016)
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

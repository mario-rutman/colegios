library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
# Criando o col_piloto_2014.
# Serão 8 colunas: c(id_aluno, aluno, professor, ensino, turma, prova, disciplina, nota).
# Todas com 22200 linhas.

# ler csv de lista de nomes
Lista_de_nomes <- read_csv2("dados/Lista de nomes.csv")

# Como irei precisar dos alunos de 2014 (555), e os novos de 2015 (185), 2016 (185) e 2017 (185).
# Fiz uma df já com todos os alunos e suas respectivas ids.
id_aluno <- rep(c(1:2000), each = 40)
#id_aluno <- seq(1, 903082, by=1628) # Sequência de números para filtrar da tabela Lista.de.nomes.
aluno <- (Lista_de_nomes[id_aluno, ]) 

#aluno <- rep(sample(Lista_de_nomes$NOME, size = 2000, replace = FALSE), each = 40)
df_todos_alunos_ids <- data.frame(id_aluno, aluno)

# Criando as duas primeiras colunas. 
df_id_aluno <-  df_todos_alunos_ids[1:22200, ]

#  Agora fazendo a coluna professor.
prof <- c("Prof. AARON", "Prof. AARON", "Prof. JONATHAN", "Prof.EDWARDS", "Prof. ALEXANDRE LEAO", 
          "Prof. CABRAL", "Prof. ANDRE LUIZ", "Prof. ALVES", "Prof. LIMA", "Prof. ASTERIO",
          "Prof. CARRIJO", "Prof. CARRIJO", "Prof. BARBOSA", "Prof. JUNIOR",
          "Prof. CARLOS ROBERTO","Prof. BLOCK", "Prof. CRISLEI","Prof. CLEVERSON",
          "Prof. FERNANDES", "Prof. NEPOMUCENO", "Prof. DIEGO MARTINEZ", "Prof. DIEGO MARTINEZ",
          "Prof. PRATA", "Prof. ELAINE APARECIDA", "Prof. RODRIGUES", "Prof. MARQUES",
          "Prof. EVERTON", "Prof. FRANCILENE", "Prof. PROCOPIO", "Prof. GARCIA")
professor <- rep(rep(prof, each = 4), 22200/120)


# Fazendo a coluna ensino.  
ensino <- rep(rep(c("1º médio", "2º médio", "3º médio"), each = 40), 555/3)

# Fazendo a coluna turma.
turma <- rep(rep(c("A", "B", "C", "D", "E"), each = 40), 22200/(40*5))

# Fazendo coluna prova.
prova <- rep(rep(c("p1", "p2", "p3", "pf"), 10), 22200/(10*4))

# Fazendo a coluna disciplina.
disciplina <- rep(rep(c("Port", "Red", "Lin_Estran", "Hist", "Geo", "Soc_Filo", 
                        "Mat", "Fis", "Quim", "Bio"),
                      each = 4), 22200/(10*4))

# Fazendo a coluna nota. É um pouco mais complicado.
# Dividi as disciplinas em 4 grupos: 1)port, red e lin estran; 2) hist, geo e soc filo; 
# 3)mat e fis; 4)quim bio, grupo 4.
# Os alunos podem ser excelentes (E), suas notas variam de 9 a 10;
# ótimos (O), notas entre 7 e 10; regulares (R) notas entre 5 e 8.5; fracos (F), entre 4 e 6.5;
# e insuficientes (I), com notas entre 0 e 6.

E <- c(90:100)
O <- c(70:100)
R <- c(50:85)
F <- c(40:65)
I <- c(0:60)

# Assim teremos alunos EEEE, excelente em todos grupos; 
# o RERR excelente no grupo 2 e regular nas outras disciplinas etc.
# E assim formar uma tabela cujas linhas são definidas pelos tipos de aluno.
# São 3 provas de cada uma das 10 matérias, assim cada tipo de aluno terá 3 notas.

EEEE <- sample(E, 40, replace = TRUE)/10
OOOO <- sample(O, 40, replace = TRUE)/10
RRRR <- sample(R, 40, replace = TRUE)/10
FFFF <- sample(F, 40, replace = TRUE)/10
IIII <- sample(I, 40, replace = TRUE)/10
EEOO <- c(sample(E, replace = T)/10,
          sample(O, 16, replace = T)/10)
OOEE <- c(sample(O, 24, replace = T)/10,
          sample(E, 16, replace = T)/10)
EERR <- c(sample(E, 24, replace = T)/10,
          sample(R, 16, replace = T)/10)
RREE <- c(sample(R, 12, replace = T)/10,
          sample(R, 12, replace = T)/10,
          sample(E, 8, replace = T)/10,
          sample(E, 8, replace = T)/10)
OOOR <- c(sample(O, 12, replace = T)/10,
          sample(O, 12, replace = T)/10,
          sample(O, 8, replace = T)/10,
          sample(R, 8, replace = T)/10)

RROO <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(O, 8, replace = T)/10, sample(O, 8, replace = T)/10)
OORR <- c(sample(O, 12, replace = T)/10, sample(O, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(R, 8, replace = T)/10)
OORO <- c(sample(O, 12, replace = T)/10, sample(O, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(O, 8, replace = T)/10)
OOFR <- c(sample(O, 12, replace = T)/10, sample(O, 12, replace = T)/10, sample(F, 8, replace = T)/10, sample(R, 8, replace = T)/10)
FROO <- c(sample(F, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(O, 8, replace = T)/10, sample(O, 8, replace = T)/10)
RREO <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(E, 8, replace = T)/10, sample(O, 8, replace = T)/10)
FFOO <- c(sample(F, 12, replace = T)/10, sample(F, 12, replace = T)/10, sample(O, 8, replace = T)/10, sample(O, 8, replace = T)/10)
OOFF <- c(sample(O, 12, replace = T)/10, sample(O, 12, replace = T)/10, sample(F, 8, replace = T)/10, sample(F, 8, replace = T)/10)
EOOF <- c(sample(E, 12, replace = T)/10, sample(O, 12, replace = T)/10, sample(O, 8, replace = T)/10, sample(F, 8, replace = T)/10)
FRRO <- c(sample(F, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(O, 8, replace = T)/10)
ORRR <- c(sample(O, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(R, 8, replace = T)/10)
RREO <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(E, 8, replace = T)/10, sample(O, 8, replace = T)/10)
RRRF <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(F, 8, replace = T)/10)
FFOR <- c(sample(F, 12, replace = T)/10, sample(F, 12, replace = T)/10, sample(O, 8, replace = T)/10, sample(R, 8, replace = T)/10)
RRFF <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(F, 8, replace = T)/10, sample(F, 8, replace = T)/10)
FFRR <- c(sample(F, 12, replace = T)/10, sample(F, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(R, 8, replace = T)/10)
FFII <- c(sample(F, 12, replace = T)/10, sample(F, 12, replace = T)/10, sample(I, 8, replace = T)/10, sample(I, 8, replace = T)/10)
IIFF <- c(sample(I, 12, replace = T)/10, sample(I, 12, replace = T)/10, sample(F, 8, replace = T)/10, sample(F, 8, replace = T)/10)
IIRR <- c(sample(I, 12, replace = T)/10, sample(I, 12, replace = T)/10, sample(R, 8, replace = T)/10, sample(R, 8, replace = T)/10)
RRII <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(I, 8, replace = T)/10, sample(I, 8, replace = T)/10)
IIRI <- c(sample(I, 12, replace = T)/10, sample(I, 9, replace = T)/10, sample(R, 8, replace = T)/10, sample(I, 8, replace = T)/10)

# Criar vetor com as notas dos alunos. São 31 tipos de alunos, 10 matérias com 4 provas por ano.
nota <- rep_len(c(EEEE, OOOO, RRRR, FFFF, IIII, EEOO, OOEE, EERR, RREE, OOOR, 
                  RROO, OORR, OORO, OOFR, FROO, RREO, FFOO, OOFF, EOOF, FRRO, 
                  ORRR, RREO, RRRF, FFOR, RRFF, FFRR, FFII, IIFF, IIRR, RRII, IIRI), 22200)

# Agora concluindo o col_piloto_2014.
zzzz <- data.frame(professor, ensino, turma, disciplina, prova, nota)
col_piloto_2014 <- bind_cols(df_id_aluno, zzzz)

write.csv2(col_piloto_2014, file = "dados/col_piloto_2014.csv",
           row.names = FALSE)
# Agora fazendo a turma de 2015. os alunos do 1º foram para o 2º e estas para o 3º.
# Assim é necessário mais 185 nomes de alunos, que ficarão no 1º ano.

#aluno_novo_2015 <- df_todos_alunos_ids [22201:29600, ]
#col_piloto_2015 <- col_piloto_2014 %>% mutate(ensino = ifelse(ensino == "1º médio", "2º médio",
                                           #ifelse(ensino == "2º médio", "3º médio", "1º médio"))) %>%
  #mutate(aluno = ifelse(ensino == "1º", aluno_novo_2015, aluno))

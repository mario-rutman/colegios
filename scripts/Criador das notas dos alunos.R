library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)

# Há as seguintes matérias. port, red e lin estran que é o grupo 1; hist, geo e soc filo, grupo 2
# mat e fis, grupo 3; e quim bio, grupo 4.
# Os alunos podem ser excelentes (E), suas notas variam de 9 a 10;
# ótimos (O), notas entre 7 e 10; regulares (R) notas entre 5 e 8.5; fracos (F), entre 4 e 6.5;
# e insuficientes (I), com notas entre 0 e 6.

E <- c(90:100)
O <- c(70:100)
R <- c(50:85)
F <- c(40:65)
I <- c(0:60)

# Assim teremos alunos EEEE, excelente em todos grupos; 
# o RERR excelento no grupo 2 e regular nas outras matérias etc.
# E assim formar uma tabela cujas linhas são definidas pelos tipos de aluno.
# São 3 provas de cada uma das 10 matérias, assim cada tipo de aluno terá 3 notas.

EEEE <- sample(90:100, 30, replace = TRUE)/10
OOOO <- sample(70:100, 30, replace = TRUE)/10
RRRR <- sample(50:85, 30, replace = TRUE)/10
FFFF <- sample(40:65, 30, replace = TRUE)/10
IIII <- sample(0:65, 30, replace = TRUE)/10
EEOO <- c(sample(90:100, 18, replace = T)/10, sample(70:100, 12, replace = T)/10)
OOEE <- c(sample(70:100, 18, replace = T)/10, sample(90:100, 12, replace = T)/10)
EERR <- c(sample(90:100, 18, replace = T)/10, sample(50:85, 12, replace = T)/10)
RREE <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(E, 6, replace = T)/10, sample(E, 6, replace = T)/10)
OOOR <- c(sample(O, 9, replace = T)/10, sample(O, 9, replace = T)/10, sample(O, 6, replace = T)/10, sample(R, 6, replace = T)/10)
RROO <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(O, 6, replace = T)/10, sample(O, 6, replace = T)/10)
OORR <- c(sample(O, 9, replace = T)/10, sample(O, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(R, 6, replace = T)/10)
OORO <- c(sample(O, 9, replace = T)/10, sample(O, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(O, 6, replace = T)/10)
OOFR <- c(sample(O, 9, replace = T)/10, sample(O, 9, replace = T)/10, sample(F, 6, replace = T)/10, sample(R, 6, replace = T)/10)
FROO <- c(sample(F, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(O, 6, replace = T)/10, sample(O, 6, replace = T)/10)
RREO <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(E, 6, replace = T)/10, sample(O, 6, replace = T)/10)
FFOO <- c(sample(F, 9, replace = T)/10, sample(F, 9, replace = T)/10, sample(O, 6, replace = T)/10, sample(O, 6, replace = T)/10)
OOFF <- c(sample(O, 9, replace = T)/10, sample(O, 9, replace = T)/10, sample(F, 6, replace = T)/10, sample(F, 6, replace = T)/10)
EOOF <- c(sample(E, 9, replace = T)/10, sample(O, 9, replace = T)/10, sample(O, 6, replace = T)/10, sample(F, 6, replace = T)/10)
FRRO <- c(sample(F, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(O, 6, replace = T)/10)
ORRR <- c(sample(O, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(R, 6, replace = T)/10)
RREO <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(E, 6, replace = T)/10, sample(O, 6, replace = T)/10)
RRRF <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(F, 6, replace = T)/10)
FFOR <- c(sample(F, 9, replace = T)/10, sample(F, 9, replace = T)/10, sample(O, 6, replace = T)/10, sample(R, 6, replace = T)/10)
RRFF <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(F, 6, replace = T)/10, sample(F, 6, replace = T)/10)
FFRR <- c(sample(F, 9, replace = T)/10, sample(F, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(R, 6, replace = T)/10)
FFII <- c(sample(F, 9, replace = T)/10, sample(F, 9, replace = T)/10, sample(I, 6, replace = T)/10, sample(I, 6, replace = T)/10)
IIFF <- c(sample(I, 9, replace = T)/10, sample(I, 9, replace = T)/10, sample(F, 6, replace = T)/10, sample(F, 6, replace = T)/10)
IIRR <- c(sample(I, 9, replace = T)/10, sample(I, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(R, 6, replace = T)/10)
RRII <- c(sample(R, 9, replace = T)/10, sample(R, 9, replace = T)/10, sample(I, 6, replace = T)/10, sample(I, 6, replace = T)/10)
IIRI <- c(sample(I, 9, replace = T)/10, sample(I, 9, replace = T)/10, sample(R, 6, replace = T)/10, sample(I, 6, replace = T)/10)

# Criar vetor com as notas dos alunos. São 30 tipos de alunos, 10 matérias com 3 provas por ano.
notas_alunos <- c(EEEE, OOOO, RRRR, FFFF, IIII, EEOO, OOEE, EERR, RREE, OOOR, 
                  RROO, OORR, OORO, OOFR, FROO, RREO, FFOO, OOFF, EOOF, FRRO, 
                  ORRR, RREO, RRRF, FFOR, RRFF, FFRR, FFII, IIFF, IIRR, RRII, IIRI)

# Se temos um colégio com 555 alunos haver? 555x30=16.650 notas. Então é repetir o vetor notas_alunos
# até perfazer 16.650 valores.
notas_col_desc_2014 <- rep(notas_alunos, len = 16650)

# Transformando o vetor notas_col_desc_2014 em matrix. 
notas_col_desc_2014_matrix <- matrix(notas_col_desc_2014, nrow = 555, byrow = TRUE)
 
# Nomeando as colunas da matrix.
provas <- c("p1_port","p2_port", "p3_port", "p1_red", "p2_red", "p3_red", 
         "p1_lin_estran", "p2_lin_estran", "p3_lin_estran", "p1_hist", "p2_hist", "p3_hist", 
         "p1_geo", "p2_geo", "p3_geo", "p1_soc_filo", "p2_soc_filo", "p3_soc_filo", 
         "p1_mat", "p2_mat", "p3_mat", "p1_fis", "p2_fis", "p3_fis", 
         "p1_quim", "p2_quim", "p3_quim", "p1_bio", "p2_bio", "p3_bio")

colnames(notas_col_desc_2014_matrix) <- provas
                                  
#Acrescentando as médias de cada matéria ao df_notas_col_desc_2014.
df_notas_col_desc_2014 <- as.data.frame(notas_col_desc_2014_matrix)
is.data.frame(df_notas_col_desc_2014)
#df_notas_col_desc_2014 <- mutate(df_notas_col_desc_2014, média_port = round((p1_port+p2_port+p3_port)/3, 1), 
                                # média_red = round((p1_red+p2_red+p3_red)/3, 1), média_lin_estran = round((p1_lin_estran+p2_lin_estran+p3_lin_estran)/3, 1),
                                 #média_hist = round((p1_hist+p2_hist+p3_hist)/3, 1), média_geo = round((p1_geo+p2_geo+p3_geo)/3, 1),
                                 #média_soc_filo = round((p1_soc_filo+p2_soc_filo+p3_soc_filo)/3, 1), média_mat = round((p1_mat+p2_mat+p3_mat)/3, 1),
                                 #média_fis = round((p1_fis+p2_fis+p3_fis)/3, 1), média_quim = round((p1_quim+p2_quim+p3_quim)/3, 1),
                                 #média_bio = round((p1_bio+p2_bio+p3_bio)/3, 1))

# Acrescentando as notas da prova final de cada matéria.
provas_finais_matrix <- matrix(c(sample(3:8,5550, replace = T)), nrow = 555, byrow = TRUE)
provas_finais <- c("pf_port", "pf_red", "pf_lin_estran", "pf_hist", "pf_geo", "pf_socio_fil", "pf_mat", 
                  "pf_fis", "pf_quim", "pf_bio")
colnames(provas_finais_matrix) <- provas_finais

# Transformando provas_finais_matrix em data frame.
df_provas_finais <- as.data.frame(provas_finais_matrix)

# Fazer a coluna situação (aprovado x prova final) por matéria.


# Juntando agora as notas finais à notas dos alunos.
df_notas_col_desc_2014 <- bind_cols(df_notas_col_desc_2014, df_provas_finais) 


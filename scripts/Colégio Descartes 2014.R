library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
 

# Fazendo a lista dos 555 nomes de alunos.
Lista_de_nomes <- read_csv("~/Todos arquivos de R/Lista de nomes.csv")
View(Lista_de_nomes)library(readr)
b <- seq(1, 903082, by=1628) # Sequ?ncia de n?meros para filtrar da tabela Lista.de.nomes.
alunos_col_descartes_2014 <- Lista_de_nomes[b,] 


# Fazendo a tabela com os 9 professores de cada s?rie, isto ? 27 nomes para professores.
c <- seq(10, 903082, by=33448) # Sequ?ncia de n?meros para filtrar da tabela Lista.de.nomes.
nomes_prof <- Lista_de_nomes[c,]

# Aqui tenho os vetores com os nomes das mate?rias e os respectivos
# professores de cada uma nos 1?, 2? e 3? M?dio.

prof_1_med <- c("Prof. AARON", "Prof. JONATHAN", "Prof.EDWARDS", "Prof. ALEXANDRE LEAO", "Prof. CABRAL", 
                "Prof. ANDRE LUIZ", "Prof. ALVES", "Prof. LIMA", "Prof. ASTERIO")
prof_2_med <- c("Prof. CARRIJO", "Prof. BARBOSA", "Prof. JUNIOR", "Prof. CARLOS ROBERTO","Prof. BLOCK", "Prof. CRISLEI",
                "Prof. CLEVERSON", "Prof. FERNANDES", "Prof. NEPOMUCENO")
prof_3_med <- c("Prof. DIEGO MARTINEZ", "Prof. PRATA", "Prof. ELAINE APARECIDA", "Prof. RODRIGUES", "Prof. MARQUES",
                "Prof. EVERTON", "Prof. FRANCILENE", "Prof. PROCOPIO", "Prof. GARCIA")

professores <- c(prof_1_med, prof_2_med, prof_3_med)
professores <- rep(professores, len = 9*555)
professores_matrix <- matrix(professores, nrow = 555, byrow = TRUE)

# Nomear as colunas da professores_matrix.
materias <- c("Português e Redação", "Língua Estrangeira", "História", "Geografia", 
              "Sociologia e Filosofia", "Matemática", "Física", "Química", "Biologia")
colnames(professores_matrix) <- materias

# Transformar a matrix em data frame.
df_professores_col_desc_2014 <- as.data.frame(professores_matrix)



# O objetivo aqui ? criar as colunas Ensino e Turma de um col?gio.
# No caso teremos apenas ensino m?dio e 5 turmas de cada n?vel. No col?gio atual temos 555 alunos, logo cada turma ter? 37 alunos.
Ensino <- rep(c("1? M?dio", "2? M?dio", "3? M?dio"), 555/3)
Turma <- rep(c("A", "B", "C", "D", "E"), 555/5)
# Agora ? fazer o data frame com estas duas colunas, que vou chamar de ensino_turma_col_descartes.
ensino_turma_col_desc_2014 <- data.frame(Ensino, Turma)


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


# Juntando ensino/turma, notas e professor para forma o col_desc_ 2014.
col_desc_2014 <- bind_cols(alunos_col_descartes_2014, ensino_turma_col_desc_2014, 
                           df_professores_col_desc_2014, df_notas_col_desc_2014)


#mutate(col_desc_2014, med_port = p1_port/3 + p2_port/3 +p3_port/3) 
# Agora "tidyng", arrumando, col_desc_2014.
#col_desc_2014 <- gather(col_desc_2014, prova, nota, 13:52)

# Olhando ALBERTINA.
#ALBERTINA_BATISTA_DE_PAULA <- col_desc_2014 %>% select(NOME, prova, nota)%>% filter(NOME == "ALBERTINA BATISTA DE PAULA") 
  


#section <- c("MATH111", "MATH111", "ENG111")
#grade <- c(78, 93, 56)
#student <- c("David", "Kristina", "Mycroft")
#gradebook <- data.frame(section, grade, student)
#gradebook
#gradebook <- mutate(gradebook, Pass.Fail = ifelse(grade > 60, "Pass", "Fail"))
#gradebook
#gradebook <- mutate(gradebook, letter = ifelse(grade %in% 60:69, "D",
                                  #ifelse(grade %in% 70:79, "C",
                                        # ifelse(grade %in% 80:89, "B",
                                                #ifelse(grade %in% 90:99, "A", "F")))))
#gradebook








write.csv2(col_desc_2014, "Colégio Descartes 2014.csv")

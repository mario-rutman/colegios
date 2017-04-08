library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)

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


 
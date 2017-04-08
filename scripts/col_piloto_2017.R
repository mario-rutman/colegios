# colegio piloto 2017
# objetivo: replicar a criação das turmas de 2016 para 2017

# carregar pacotes
library(tidyverse)
library(magrittr)
library(stringr)

# 1 ) ler o arquivo de 2016
col_piloto_2016 <- read_csv2("dados/col_piloto_2016.csv",
                             locale = locale(encoding = "ISO-8859-1"))


# ler lista de nomes
Lista_de_nomes <- read_csv2("dados/Lista de nomes.csv") %>% as.data.frame()

id_aluno <- 926:(926 + 184)
#id_aluno <- rep(id_aluno, each = 40)
aluno <- Lista_de_nomes$NOME[id_aluno]

#aluno <- rep(sample(Lista_de_nomes$NOME, size = 2000, replace = FALSE), each = 40)
df_todos_alunos_ids <- data.frame(id_aluno_novo = id_aluno, aluno_novo = aluno,
                                  stringsAsFactors = FALSE)


# ids de alunos do 1a ano de 2016
alunos_1m <- col_piloto_2016$id_aluno[col_piloto_2016$ensino == "3º médio"]
alunos_1m %<>% unique
#alunos_1m <- rep(unique(alunos_1m), each = 40)
df_todos_alunos_ids$id_antiga <- alunos_1m
df_todos_alunos_ids %<>% distinct(id_aluno_novo, aluno_novo, id_antiga)

#left_join para substituir id antiga pela nova
col_piloto_2017 <- col_piloto_2016
col_piloto_2017 %<>% left_join(df_todos_alunos_ids, by = c("id_aluno" = "id_antiga"))

# converter caracters do ensino para minusculo
col_piloto_2017$ensino %<>% str_to_lower()

# substituir series dos alunos
col_piloto_2017$ensino[col_piloto_2017$ensino == "3º médio"] <- NA
col_piloto_2017$ensino[col_piloto_2017$ensino == "2º médio"] <- "3º médio"
col_piloto_2017$ensino[col_piloto_2017$ensino == "1º médio"] <- "2º médio" 
col_piloto_2017$ensino[is.na(col_piloto_2017$ensino)] <- "1º médio" 
# substituir alunos antigos por novos
indice_mudar <- which(col_piloto_2017$ensino == "1º médio")
col_piloto_2017$id_aluno[indice_mudar] <- col_piloto_2017$id_aluno_novo[indice_mudar]
col_piloto_2017$aluno[indice_mudar] <- col_piloto_2017$aluno_novo[indice_mudar]
# remover colunas
col_piloto_2017$id_aluno_novo <- NULL
col_piloto_2017$aluno_novo <- NULL

# salvar output
col_piloto_2017 %>% write.csv2("dados/col_piloto_2017.csv",
                               fileEncoding = "ISO-8859-1",
                               row.names = FALSE)
# salvar em Rda
saveRDS(col_piloto_2017, "col_piloto_2017.Rda")



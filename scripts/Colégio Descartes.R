library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)
b <- seq(1, 1000000, by=2200) # Sequ�ncia de n�meros para filtrar da tabela Lista.de.nomes.
alunos_col_descartes <- Lista.de.nomes[b,] %>% 
  select(NOME)# Filtrando as linhas para ter 450 nomes e selecionando a coluna NOME. 
col_descartes_notas_2014 <- bind_cols(alunos_col_descartes, alunos_ensino_col_descartes, notas_aleatorias_col_descartes)
# Juntando as duas tabelas, para fazer o retrato do Col�gio Descartes em 2014. 
# As notas aleat�rias do Col�gio Descartes foram importadas do Excel.

# Filtragens
medio_1 <- filter(col_descartes_2014,  Ensino == "1� M�dio")# S� as linhas de 1�M�dio. Todas as turmas do 1�M�dio.
medio_2_turma_a <- filter(col_descartes_2014, Ensino == "2� M�dio"  & Turma=="a")# S� as linhas do2� M�dio da turma a.
medio_2_turma_e <- filter(col_descartes_2014, Ensino == "2� M�dio" & Turma == "e") # S� a turma c do 1� m�dio.

# Mais filtragens.
cdf_port <- filter(col_descartes_2014,  p1_port >= 9.5 & p2_port >=9.5 & p3_port >= 9.5) # Alunos cujas notas em port1, 2 e 3 s�o maiores que 9.5.
cdf_port_mat <- filter(col_descartes_2014,  p1_port >= 9.5 & p2_port >=9.5 & p3_port >= 9.5 & p1_mat >= 9.5 & p2_mat >=9.5 & p3_mat >= 9.5)
cdf_mat <- filter(col_descartes_2014,  p1_mat >= 9.5 & p2_mat >=9.5 & p3_mat >= 9.5)
cdf_red <- filter(col_descartes_2014,  p1_red >= 9.5 & p2_red >=9.5 & p3_red >= 9.5)

# Criando a coluna m�dia por mat�ria.
mutate(col_descartes_2014, med_port = round(p1_port/3+p2_port/3+p3_port/3, 1))
col_descartes_notas_e_med_2014 <- mutate(col_descartes_2014, med_port = round(p1_port/3+p2_port/3+p3_port/3, 1), med_red = round(p1_red/3+p2_red/3+p3_red/3,1), med_mat = round(p1_mat/3+p2_mat/3+p3_mat/3,1), med_fis = round(p1_fis/3+p2_fis/3+p3_fis/3,1),med_quim = round(p1_quim/3+p2_quim/3+p3_quim/3,1),med_bio = round(p1_bio/3+p2_bio/3+p3_bio/3,1), med_hist = round(p1_hist/3+p2_hist/3+p3_hist/3,1), med_geo = round(p1_geo/3+p2_geo/3+p3_geo/3,1), med_soc_filo = round(p1_soc_filo/3+p2_soc_filo/3+p3_soc_filo/3,1))

# Fazendo uma tabela s� com as m�dias e resultados.
col_descartes_result_2014 <- select(col_descartes_notas_e_med_2014, NOME, Ensino, Turma, contains("med"))


resultados_2014 <- mutate(col_descartes_result_2014, PORTUGU�S = ifelse(med_port >= 7, "APROVADO", "PROVA FINAL"), REDA��O = ifelse(med_red >= 7, "APROVADO", "PROVA FINAL"), MATEM�TICA = ifelse(med_mat >= 7, "APROVADO", "PROVA FINAL"))


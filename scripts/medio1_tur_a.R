library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)
library(gridExtra)
library(beeswarm)
library(ggiraph)

medio1_tur_a <- col_descartes_resul_2014 %>% 
  filter(Ensino == "1? M?dio" & Turma == "a") %>%
  select(NOME, Ensino, Turma, p1_port) %>%
  mutate(acimade = ntile(p1_port, 100)) %>%
  arrange(desc(acimade)) %>% 
  mutate(percentual = "% de alunos com nota menor") 
 
# comentjedo linha 
  
barplot(medio1_tur_a$acimade, names.arg = medio1_tur_a$NOME,
        xlim = c(0,100), horiz = TRUE, las = 1, cex.names = 0.7, border = NA)
# Gr?fico com boxplots das m?dias de todas as mat?rias.
boxplot(col_descartes_resul_2014[,34:43]) # ? bem l?gico, escolhe a tabela de onde vai extrair os dados
# e d? o intervalo de colunas que interessa.
# Gr?ficos com os boxplots das notas finais de todas mat?rias.
boxplot(col_descartes_resul_2014[,64:73])
boxplot(medio1_tur_a$acimade) # Resultado esperado pois ? uma distribui??o de notas divididas por percentil.
boxplot(medio1_tur_a[,5])

#Plotando a contagem das notas repetidas.
plot(count(medio1_tur_a, p1_port))

#Sequ?ncias.
seq1 <- seq(5, 10, by = 0.1) #Linear de 5 a 10 de 0.1 em 0.1.
seq2 <- seq(5, 10, length = 20) #20 n?meros distribu?dos entre 5 e 10.

# Criando gr?fico vazio usando type = "n".
plot(medio1_tur_a$p1_port, medio1_tur_a$acimade, type = "n",
     xlab = "1? prova de Portug?s", ylab = "% de alunos com notas menores que a sua")
# Adicionar os nomes do alunos.
points(medio1_tur_a$p1_port, medio1_tur_a$acimade, 
       pch = as.character(medio1_tur_a$NOME))

# Criando gr?ficos Beeswarm.
beeswarm(medio1_tur_a$p1_port)# S?o os 31 alunos, cada um no seu centil.
beeswarm(p1_port ~ acimade, data = medio1_tur_a, method = "swarm")
beeswarm(p1_port ~ acimade, data = medio1_tur_a, method = "center")
beeswarm(p1_port ~ acimade, data = medio1_tur_a, method = "hex")
beeswarm(p1_port ~ acimade, data = medio1_tur_a, method = "square")

plot(medio1_tur_a$p1_port, type = "l")
plot(medio1_tur_a$p1_port, type = "b", cex = 1)
plot(medio1_tur_a$p1_port, type = "l", xlab = "% de alunos acima de voc?", ylab = "nota", las = 1, col = "blue", lwd=4, cex.axis=0.8)

#Gr?fico multiples lines.
# Criando o data frame df.
# Repetindo 1,2,3,4 e 5 9 vezes. Criando 45 n?meros entre 1 e 100.
# Colando category a n?meros de 1 a 9, de 5 em 5.
set.seed(45)
df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                 variable=rep(paste0("category", 1:9), each=5))
# plot
ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable))

# Criando o multiples lines para o Col?gio Descartes.
# Cada aluno ser? uma categoria e as p1, p2, p3... ficar?o no eixo do x.
# Assim as 3 provas dever?o ficar em uma mesma coluna e n?o em 3 como est?o hoje. Ao trabalho.
p123_port <- col_descartes_resul_2014 %>% 
  filter(Ensino == "1? M?dio" & Turma == "a") %>%
  select(NOME, p1_port, p2_port, p3_port) %>% 
  gather("port", "nota", -c(1))
# Agora vamos ao gr?fico.
ggplot(data = p123_port, aes(x = port, y = nota, group = NOME, colour = as.factor(NOME))) + geom_line()   

# Fazer um gr?fico com regress?o linear de Portugu?s vs. Reda??o.
# Se fossem alunos de verdade esperar-se-ia uma correla??o forte. Vejamos o que acontece.
linear_model <- lm(med_port ~ med_red, data = col_descartes_resul_2014)
# Agora plotar med_port vs. med_red.
plot(col_descartes_resul_2014$med_port, col_descartes_resul_2014$med_red)
# Usando o abline() para fazer a linha da regress?o linear.
abline(linear_model, lty = 2)

# Colocar o nome do aluno que tirou determinada combina??o de notas.
plot(col_descartes_resul_2014$med_port, col_descartes_resul_2014$med_red, pch = 15)
# Criando a regra para colocar o nome.
index3 <- which(col_descartes_resul_2014$med_port == 5 | col_descartes_resul_2014$med_port == 9.9 | col_descartes_resul_2014$med_port == 8.8)
# Add text giving names of cars next to data points
text(x = col_descartes_resul_2014$med_port[index3], 
     y = col_descartes_resul_2014$med_red[index3],
     labels = col_descartes_resul_2014$NOME[index3], adj = 0)


# Colocando o nome no gr?fico colm?ia. 
beeswarm(medio1_tur_a$p1_port)
# Criando a regra de coloca??o do nome.
i3 <- which(medio1_tur_a$p1_port == 9.9 | medio1_tur_a$p1_port == 9.5)
# Adicionabdo os nomes selecionados pela regra.
text(x = medio1_tur_a$p1_port[i3], 
     labels = medio1_tur_a$NOME[i3], adj = 0)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
# Criando o col_vvr_2014.
# Serão 7 colunas: c(aluno, ensino, turma, prova, disciplina, nota, professor).
# Todas com 22200 linhas.

# Fazendo a lista dos 555 nomes de alunos.
Lista_de_nomes <- read_csv("~/Todos arquivos de R/Lista de nomes.csv")
View(Lista_de_nomes)
b <- seq(1, 903082, by=1628) # Sequência de números para filtrar da tabela Lista.de.nomes.
aluno <- (Lista_de_nomes[b,]) 

 
# Repetindo alunos 40 vezes.
# alunos <- rep(alunos, each = 40)

# Fazendo a coluna ensino.  
ensino <- rep(rep(c("1º médio", "2º Médio", "3º médio"), each = 40), 555/3)

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
EEOO <- c(sample(E, replace = T)/10, sample(O, 16, replace = T)/10)
OOEE <- c(sample(O, 24, replace = T)/10, sample(E, 16, replace = T)/10)
EERR <- c(sample(E, 24, replace = T)/10, sample(R, 16, replace = T)/10)
RREE <- c(sample(R, 12, replace = T)/10, sample(R, 12, replace = T)/10, sample(E, 8, replace = T)/10, sample(E, 8, replace = T)/10)
OOOR <- c(sample(O, 12, replace = T)/10, sample(O, 12, replace = T)/10, sample(O, 8, replace = T)/10, sample(R, 8, replace = T)/10)
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


# Agora fazendo a coluna professor.
prof <- c("Prof. AARON", "Prof. AARON", "Prof. JONATHAN", "Prof.EDWARDS", "Prof. ALEXANDRE LEAO", 
          "Prof. CABRAL", "Prof. ANDRE LUIZ", "Prof. ALVES", "Prof. LIMA", "Prof. ASTERIO",
          "Prof. CARRIJO", "Prof. CARRIJO", "Prof. BARBOSA", "Prof. JUNIOR",
          "Prof. CARLOS ROBERTO","Prof. BLOCK", "Prof. CRISLEI","Prof. CLEVERSON",
          "Prof. FERNANDES", "Prof. NEPOMUCENO", "Prof. DIEGO MARTINEZ", "Prof. DIEGO MARTINEZ",
          "Prof. PRATA", "Prof. ELAINE APARECIDA", "Prof. RODRIGUES", "Prof. MARQUES",
          "Prof. EVERTON", "Prof. FRANCILENE", "Prof. PROCOPIO", "Prof. GARCIA")
professor <- rep(rep(prof, each = 4), 22200/120)

# Agora finalmente criando o colégio.
colegio_vvr_2014 <- data.frame(ensino, turma, prova, disciplina, nota, professor)

# Criando a coluna id_aluno.
colegio_vvr_2014_2 <- colegio_vvr_2014 %>%
  group_by(ensino, turma, disciplina, prova) %>%
  mutate(id_aluno = formatC(row_number(), width = 2, flag = "0")) %>%
  ungroup() %>%
  mutate(id_aluno = paste0(str_sub(ensino, 1, 1), turma, id_aluno))

# Criar nomes de verdade
id_aluno <- sort(unique(colegio_vvr_2014_2$id_aluno))
nome_aluno <- sample(Lista_de_nomes$NOME, size = length(id_aluno), replace = FALSE)
df_nome_aluno <- data.frame(id_aluno, nome_aluno, stringsAsFactors = FALSE)

colegio_vvr_2014_2 <- left_join(colegio_vvr_2014_2, df_nome_aluno,
                                by = "id_aluno")

### simulação: alunos acabaram de fazer p1 e querem saber a nota deles
# pra passar nas materias
sim_p1 <- colegio_vvr_2014_2

sim_p1 %<>% mutate(nota = ifelse(prova != "p1", NA, nota))
sim_p1 <- sim_p1 %>% mutate(nota = ifelse(prova != "p1", NA, nota))

media_chegada <- function(x) {
  round((21 - x)/2, 1)
}


sim_p1 <- sim_p1 %>%
  group_by(nome_aluno, disciplina) %>%
  mutate(nota_novo = ifelse(is.na(nota),
                            media_chegada(sum(nota, na.rm = TRUE)),
                            nota)) %>%
  ungroup()


sim_p1$cor_barra <- NA
sim_p1$cor_barra[sim_p1$prova == "p1" & sim_p1$nota >= 7] <- "blue"
sim_p1$cor_barra[sim_p1$prova == "p1" & sim_p1$nota >= 5 & sim_p1$nota < 7] <- "yellow"
sim_p1$cor_barra[sim_p1$prova == "p1" & sim_p1$nota < 5] <- "red"
sim_p1$cor_barra[is.na(sim_p1$cor_barra)] <- "gray"

aluno_exemplo <- '1A01'
sim_p1_ex <- sim_p1 %>% filter(id_aluno == aluno_exemplo & prova != "pf")

ggplot(sim_p1_ex, aes(x = prova, y = nota_novo)) +
  geom_bar(stat = "identity", aes(fill = cor_barra)) +
  geom_text(aes(label = nota_novo), vjust = -0.3) +
  facet_wrap( ~ disciplina) +
  scale_fill_identity()
  
ggplot(sim_p1, aes(disciplina)) +
  geom_bar()



# Escolhendo um aluno aleatoriamente, 2A10, para fazer os gráficos dele.
alu_alea <- filter(colegio_vvr_2014_2, id_aluno == "2A10")

ggplot(alu_alea, aes(x = prova, y = nota)) +
  geom_jitter() +
  facet_grid(. ~ disciplina)

ggplot(alu_alea, aes(x = prova, y = nota, label=disciplina))+
  geom_text()

ggplot(alu_alea, aes(x = factor(prova), fill = factor(disciplina)))+ geom_bar()

#Gráfico interessante para fazer as notas no eixo y e as 4 provas no eixo x.
#Aqui de todos alunos do colégio. É claro que vai ficar confuso.
#Mas revela um padrão: Existe uma maior concentração de notas apartir de 3,5.
ggplot(colegio_vvr_2014_2, aes(y = nota, x = prova))+
  geom_point(size = 4, alpha = .1, position = "jitter")

# Agora aplicando este gráfico a uma determinada turma.
# O legal deste é que podemos ver quanto cada aluno tirou em cada prova.
# O problema é que quase mistura notas de diferentes provas.
turma_esp <- colegio_vvr_2014_2 %>% filter(disciplina == "Bio", turma == "A", ensino == "1º médio")
ggplot(turma_esp, aes(x = prova, y = nota)) +
  geom_point(size = 3, alpha = .5, position = "jitter")

# No jitter posso aproximar, ou afastar, os pontos.
# Assim resolvo o problema do gráfico acima. 
# No caso para não confundir as notas das provas. 
posn.j <- position_jitter(0.3)
ggplot(turma_esp, aes(x = prova, y = nota)) + geom_point(position = posn.j,
                                                         size = 5, alpha = 0.5)
ggplot(turma_esp, aes(x = prova, y = nota)) + geom_point(position = posn.j,
                                                         size = 8, alpha = 0.5)


# Gráfico cêra de abelha, beeswarm.
# Bee Swarm Tutorial
# Packages used: beeswarm by Aron Charles Eklund
# install.packages("beeswarm")
# beeswarm == enxame de abelhas
library("beeswarm")
options(scipen = 999)
# Load data
#workers <- read.csv("data/income-sample-2014.tsv", sep = "\t", stringsAsFactors=FALSE)

#workers <- income_sample_2014
# Traditional
hist(workers$INCTOT, breaks = 30)# Histograma, de 30 colunas, tabela workers coluna INCTOT, 
stripchart(workers$INCTOT)# Mil quadradinhos, cada um no n?vel de seu INCTOT, mas quando s?o muitos no mesmo n?vel ficam sobrepostos. 

# Beeswarm
beeswarm(workers$INCTOT)# Os mesmos mil INCTOT, mas quando muitos no mesmo n?vel, espalhan-se lateralmente.

# Beeswarm for categories, isto ?, INCTOT separado nos 30 tipos de main_occ. 
beeswarm(INCTOT ~ main_occ, data = workers, method = "swarm")
beeswarm(INCTOT ~ main_occ, data = workers, method = "center")
beeswarm(INCTOT ~ main_occ, data = workers, method = "hex")
beeswarm(INCTOT ~ main_occ, data = workers, method = "square")

# Usando o beeswarm na turma_esp.
beeswarm(prova ~ nota, data = turma_esp, method = "hex")

# Soma o número de algarismos de um vetor.  
sum(nchar(c(12,15,456)))
sum(nchar(c(54:2017)))

# Gráficos de linha.
#ggplot(economics, aes(x = date, y = unemploy)) + 
  #geom_line()
# Adjust plot to represent the fraction of total population that is unemployed
#ggplot(economics, aes(x = date, y = unemploy/pop)) + 
  #geom_line()

# Aqui vou pegar uma determiada turma e disciplina e fazer gráfico de linha 
# de todos alunos ao longo do ano: turma_esp.
ggplot(turma_esp, aes(x = prova, y = nota, col = id_aluno))+
  geom_line()
  

#ggplot(fish.tidy, aes(x = Year, y = Capture, col = Species)) + 
  #geom_line()


#Mais um recurso importante.
#Define o afastamento dos pontos:
#posn.j <- position_jitter(0.3)
#Depois aplica no geom_point. Assim os pontos não ficam sobrepostos nem muito esplhados.
#ggplot(mtcars, aes(x = cyl, y = wt)) + geom_point(position = posn.j)

# Use geom_jitter() instead of geom_point()
#ggplot(Vocab, aes(x = education, y = vocabulary))+
  #geom_jitter()


# Using the above plotting command, set alpha to a very low 0.2
#ggplot(Vocab, aes(x = education, y = vocabulary))+
  #geom_jitter(alpha = 0.2)


# Using the above plotting command, set the shape to 1
#ggplot(Vocab, aes(x = education, y = vocabulary))+
  #geom_jitter(alpha = 0.2, shape = 1)

# Change the position argument to stack
#ggplot(mtcars, aes(x = cyl, fill = am)) +
 # geom_bar(position = "stack")


# Change the position argument to fill
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill")


# Change the position argument to dodge
ggplot(mtcars, aes(x = cyl, fill = am)) +
 geom_bar(position = "dodge")


# Criando o dummy dataset
  df.dummy_data <- data.frame(
    dummy_metric = cumsum(1:20),
    date = seq.Date(as.Date("1980-01-01"), by="1 year", length.out=20))
  # Plot the data using ggplot2 package
  ggplot(data = df.dummy_data, aes(x = date, y = dummy_metric)) +
    geom_line()
  
  
#Criando uma contingency table.
faixa_de_nota <- c("8 a 10", "6 a 8","4 a 6",  "2 a 4", "0 a 2")
pass_dir <- c(55, 29, 25, 12, 1)
prov_fin <- c(40, 39, 26, 15, 4)
reprov <- c(0,0,2,1,1)
prob_condicional <- data.frame(faixa_de_nota, pass_dir, prov_fin, reprov)
mutate(prob_condicional, prob_pas_dir = pass_dir/(pass_dir+prov_fin+reprov),
       prob_prov_fin = prov_fin/(pass_dir+prov_fin+reprov),
       prob_reprov = reprov/(pass_dir+prov_fin+reprov))


# Map cyl to size
ggplot(mtcars, aes(x=wt, y=mpg, size=cyl))+
  geom_point()


# Map cyl to alpha
ggplot(mtcars, aes(x=wt, y=mpg, alpha=cyl))+
  geom_point()


# Map cyl to shape 
ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl))+
  geom_point()


# Map cyl to labels
ggplot(mtcars, aes(x=wt, y=mpg, label=cyl))+
  geom_text()

# Define a hexadecimal color
my_color <- "#123456"

# Set the color aesthetic 
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point()


# Set the color aesthetic and attribute 
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point(col = my_color)


# Set the fill aesthetic and color, size and shape attributes
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + 
  geom_point(size = 8, shape = 23, col = my_color)


# Load the tidyr package
#library(tidyr)
# Fill in the ___ to produce to the correct iris.tidy dataset
#iris.tidy <- iris %>%
  #gather(key, Value, -Species) %>%
  #separate(key, c("Part", "Measure"), "\\.")

plot(alu_alea)
plot(alu_alea$disciplina ~ alu_alea$nota)
plot(alu_alea$prova, alu_alea$nota)
plot(alu_alea$disciplina, alu_alea$nota,
     xlab = "Disciplina",
     ylab = "Nota")
ggplot(alu_alea, aes(x = disciplina, y = nota)) + geom_point()
title("Faixa de notas nos 4 tipos de provas")
plot(alu_alea$disciplina,alu_alea$nota)
title("Intervalo de notas por matéria")


d <- ggplot(alu_alea, aes(disciplina))
d + geom_bar() 

c <- ggplot(alu_alea, aes(nota))
c + geom_histogram(binwidth = 1) 

ggplot(alu_alea, aes(x = disciplina, y = nota)) +
  geom_point(alpha = 0.2)

ggplot(alu_alea, aes(x = factor(prova), y = nota)) +
  geom_point()
ggplot(alu_alea, aes(x = prova, y = disciplina, col = nota)) +
  geom_point()
ggplot(alu_alea, aes(x = prova, y = disciplina, size = nota)) +
  geom_point()
# ggplot(alu_alea, aes(x = prova, y = disciplina, shape = nota)) Nota é variável contínua,
# não aceita shape!!! + geom_point()

ggplot(alu_alea, aes(x = disciplina, y = nota)) +
  geom_smooth(aes(col = prova), se = FALSE)

# Create the object containing the data and aes layers: dia_plot
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# Add a geom layer with + and geom_point()
dia_plot + geom_point()

# Add the same geom layer, but with aes() inside
dia_plot + geom_point(aes(col = clarity))

# Teste para fazer os colégios em 2014, 2015, 2016 e 2017. Vou chamar de coleginho.
alun_coleginho <- c("1.1","1.2","2.1","2.2","3.1","3.2")
ensino_coleginho <- c("1º", "1º", "2º", "2º", "3º", "3º")
coleginho_14 <- data.frame(alun_coleginho, ensino_coleginho, stringsAsFactors = FALSE)

id_aluno <- sort(unique(colegio_vvr_2014_2$id_aluno))
aluno_novo_2015<- c("AQ", "MN")
coleginho_15 <- coleginho_14 %>% mutate(ensino_coleginho = ifelse(ensino_coleginho == "1º", "2º",
                                                  ifelse(ensino_coleginho == "2º", "3º", "1º"))) %>%
  mutate(alun_coleginho = ifelse(ensino_coleginho == "1º", aluno_novo_2015, alun_coleginho))
  
  anul_pf <- col_piloto_2014[1:16, ] 
mutate(anul_pf, nota = ifelse(mean(p1,p2,p3)>= 7, pf == NA,))

# basic scatter plot:
qplot(wt, mpg, data = mtcars)

# Categorical:
# cyl
qplot(wt, mpg, data = mtcars, size = factor(cyl))

# gear
qplot(wt, mpg, data = mtcars, size = factor(gear))

# Continuous
# hp
qplot(wt, mpg, data = mtcars, col = hp)

# qsec
qplot(wt, mpg, data = mtcars, col = qsec)

# Check out the head of ChickWeight
head(ChickWeight)

# Use ggplot() for the second instruction
ggplot(ChickWeight, aes(x = Time, y = weight)) + 
  geom_line(aes(group = Chick))

# Use ggplot() for the third instruction
ggplot(ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick))

# Use ggplot() for the last instruction
ggplot(ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick), alpha = 0.3) + 
  geom_smooth(lwd = 2, se = FALSE)

# titanic is avaliable in your workspace

# Check out the structure of titanic
str(titanic)

# Use ggplot() for the first instruction
ggplot(titanic, aes(x = factor(Pclass), fill = factor(Sex))) + 
  geom_bar(position = "dodge") 


# Use ggplot() for the second instruction
ggplot(titanic, aes(x = factor(Pclass), fill = factor(Sex))) + 
  geom_bar(position = "dodge") +
  facet_grid(". ~Survived")


# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
ggplot(titanic, aes(x = factor(Pclass), y = Age, col = factor(Sex))) + 
  geom_jitter(size = 3, alpha = 0.5, position = posn.j) +
  facet_grid(". ~Survived")
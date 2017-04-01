# Olhando ALBERTINA.
ALBERTINA_BATISTA_DE_PAULA <- col_desc_2014 %>%
  filter(NOME == "ALBERTINA BATISTA DE PAULA")%>%
  select(-c(4:12)) %>%
   gather("matéria", "nota", c(4:43)) 
# Separando a matéria em matéria e prova.
ALBERTINA_BATISTA_DE_PAULA <- mutate(ALBERTINA_BATISTA_DE_PAULA, prova = matéria)




#mutate (med_port = p1_port/3+p2_port/3+p3_port/3, med_red = p1_red/3+p2_red/3+p3_red/3, 
        med_lin_estran = p1_lin_estran/3+p2_lin_estran/3+p3_lin_estran/3,med_hist = p1_hist/3+p2_hist/3+p3_hist/3,
        med_geo = p1_geo/3+p2_geo/3+p3_geo/3, med_soc_filo = p1_soc_filo/3+p2_soc_filo/3+p3_soc_filo/3,
        med_mat = p1_mat/3+p2_mat/3+p3_mat/3, med_fis = p1_fis/3+p2_fis/3+p3_fis/3,
        med_quim = p1_quim/3+p2_quim/3+p3_quim/3, med_bio = p1_bio/3+p2_bio/3+p3_bio/3)  

  #section <- c("MATH111", "MATH111", "ENG111")
#grade <- c(78, 93, 56)
#student <- c("David", "Kristina", "Mycroft")
#gradebook <- data.frame(section, grade, student)
#gradebook
#gradebook <- mutate(gradebook, Pass.Fail = ifelse(grade > 60, "Pass", "Fail"))
#gradebook
#gradebook <- mutate(gradebook, letter = ifelse(grade %in% 60:69, "D",
#ifelse(grade %in% 70:79, "C",
#ifelse(grade %in% 80:89, "B",
#ifelse(grade %in% 90:99, "A", "F")))))
#gradebook

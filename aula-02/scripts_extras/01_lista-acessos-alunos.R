library(tidyverse)

grid_acessos <- readxl::read_xlsx("aula-02/data/acessos_aula_01.xlsx", range = "B3:M35")

total_acessos_registrados <- grid_acessos %>% 
  select(-`2018-04`) %>%
  as.matrix() %>% 
  rowSums()

names(total_acessos_registrados) <- grid_acessos$`2018-04`

acessos_alunos <- as.list(total_acessos_registrados)

grid_acessos %>%
  rename(aluno = `2018-04`) %>%
  gather(key=dia, value = acessos, -aluno) %>%
  filter(acessos > 0) %>%
  mutate(acessos = as.integer(acessos), dia = as.integer(dia)) -> acessos_aluno_dia

save(acessos_alunos, acessos_aluno_dia, file = "aula-02/data/dados_exercicio.RData")

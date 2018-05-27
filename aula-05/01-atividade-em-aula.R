# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse

library(tidyverse)


# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted <- read_csv("aula-05/data/ted_main.csv.gz")
head(ted,20)

# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
# Resposta: Não, as variaveis film_date e publish_date deveriam ser do tipo date e duration do tipo duration

summary(ted)

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

library(lubridate)
#Cristofer, por favor, lembbre do problema para execução do comando duration
#ted$duration <- as.duration(as.numeric(ted$duration))
#str(ted)
ted$film_date <- as_datetime(ted$film_date)
ted$published_date <- as_datetime(ted$published_date)
       
# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted$event <- factor(ted$event)
ted$speaker_occupation <- factor(ted$speaker_occupation)

# Retire do dataframe a variável name

ted$name<- NULL


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas

summary(ted)


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

ted%>%
 mutate(languages = if_else( languages == 0, 1L, languages ))
 
# Verifique os 15 registros com menor data de filmagem. 
ted%>%
  arrange(film_date)%>%
  select(title, film_date)%>%
  head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
#Cristofer, aqui eu entendi que era para criar um novo dataframe, diferente do original (Thais)

ted%>%
  group_by(year(film_date))%>%
  summarise(apresentacoes = n())-> qtde_apres
  qtde_apres


# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
 qtde_apres$apresentacoes
 quantile(qtde_apres$apresentacoes, probs = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
 ind <- which(with( qtde_apres, apresentacoes <= quantile(qtde_apres$apresentacoes, probs = c(0.4))))
 qtde_apres <- qtde_apres[-ind,]
 qtde_apres$apresentacoes
 qtde_apres  
 
# Verifique novamente o resumo dos dados do dataframe
# Cristofer, Aqui eu entendi que era para voltar a trabalhar no dataframe original(Thais)
summary(ted)


# Verifique os 10 registros com maior duração.
# Cristofer, lembrando que eu não consegui alterar o tipo da variavel duration (Thais)

ted%>%
  arrange(desc(duration))%>%
  head(10)


# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas

tres_desvios <- 3 *sd(ted$duration )
media <- mean(ted$duration)
TRES_DESVIOS_DA_MEDIA<- media + tres_desvios
TRES_DESVIOS_DA_MEDIA
ted%>%
  filter(duration >= TRES_DESVIOS_DA_MEDIA)


# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
quantile(ted$duration)
IQR(ted$duration)

  filtro<-((1.5 * IQR(ted$duration))  +  quantile(ted$duration,probs = c(0.75)))
ted%>%  
    filter(duration > filtro)


# Visualize os 10 quantis da quantidade de visualizações

quantile(ted$views, probs = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
  
  COMPARA = ifelse(mean(ted$views) > median(ted$views), 'MEDIA MAIOR', 'MEDIANA MAIOR')
  print(COMPARA)

  COMPARA_DESVIO = ifelse(sd(ted$views) > median( abs( ted$views - median( ted$views ))), 'DESVIO PADRAO MAIOR', 'DESVIO ABSOLUTO MEDIANA MAIOR')
  print(COMPARA_DESVIO)
  
  IQR_MAIOR <- (IQR(ted$views)/median( abs( ted$views - median( ted$views ))))
  print(paste('IQR: ',IQR_MAIOR,'Vezes Maior Desvio Absoluto Mediada'));

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
 
  ind <- which(with( ted, views <= quantile(ted$views, probs = c(0.1))))
  dez_menos <- ted[ind,]
  media_10menos<- mean(dez_menos$languages)
  dv_10menos<- sd(dez_menos$languages)
  mediana_10menos<- median(dez_menos$languages)
  iqr_10menos <- IQR(dez_menos$languages)
  print(paste("Dados Qtde Linguagens 10% Videos Menos Vistos: Media",media_10menos,"Desvio Padrão",dv_10menos,'Mediana', mediana_10menos, 'IQR', iqr_10menos))

  ind <- which(with( ted, views >= quantile(ted$views, probs = c(0.9))))
  dez_mais <- ted[ind,]
  media_10mais<- mean(dez_mais$languages)
  dv_10mais<- sd(dez_mais$languages)
  mediana_10mais<- median(dez_mais$languages)
  iqr_10mais <- IQR(dez_mais$languages)
  print(paste("Dados Qtde Linguagens 10% Videos Mais Vistos: Media",media_10mais,"Desvio Padrão",dv_10mais,'Mediana', mediana_10mais, 'IQR', iqr_10mais))
  
# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro

ted%>%
  filter(str_detect(event,'TED'))%>%
  select(event,views) 

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES

ted%>%
  filter( (str_detect(event,'TED')),(views > median(views)))%>%
  summarise(visoes= n(), menor_ano= min(published_date), media_linguas= mean(languages),dp_linguas= sd(languages),
            CV=((dp_linguas*100)/media_linguas))%>%

ted%>%
  filter( (str_detect(event,'TED')),(views > median(views)),views>10)  

 

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas


  CORRELACAO_VALOR = cor(x = ted$views, y = ted$languages)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO VISUALIZAÇOES E LINGUAS",CORRELACAO_VALOR ,GRAU));
  
  CORRELACAO_VALOR =cor(x=ted$views, y= ted$duration)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO VISUALIZAÇOES E DURACAO",CORRELACAO_VALOR ,GRAU));
  
  CORRELACAO_VALOR =cor(x=ted$views, y= ted$comments)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO VISUALIZAÇOES E COMENTARIOS",CORRELACAO_VALOR ,GRAU));
  
  CORRELACAO_VALOR =cor(x=ted$comments, y= ted$languages)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO COMENTARIOS E LINGUAS",CORRELACAO_VALOR ,GRAU));
  


# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas

  tres_desvios <- 3 *sd(ted$duration )
  media <- mean(ted$duration)
  TRES_DESVIOS_DA_MEDIA<- media + tres_desvios
  TRES_DESVIOS_DA_MEDIA
  ted%>%
  filter(duration >= TRES_DESVIOS_DA_MEDIA)%>%
  CORRELACAO_VALOR = cor(x = views, y = languages)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO VISUALIZAÇOES E LINGUAS",CORRELACAO_VALOR ,GRAU));

  tres_desvios <- 3 *sd(ted$duration )
  media <- mean(ted$duration)
  TRES_DESVIOS_DA_MEDIA<- media + tres_desvios
  TRES_DESVIOS_DA_MEDIA
  ted%>%
  filter(duration >= TRES_DESVIOS_DA_MEDIA)%>%
  CORRELACAO_VALOR =cor(x=views, y= duration)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO VISUALIZAÇOES E DURACAO",CORRELACAO_VALOR ,GRAU));
  
  tres_desvios <- 3 *sd(ted$duration )
  media <- mean(ted$duration)
  TRES_DESVIOS_DA_MEDIA<- media + tres_desvios
  TRES_DESVIOS_DA_MEDIA
  ted%>%
  filter(duration >= TRES_DESVIOS_DA_MEDIA)%>%
  CORRELACAO_VALOR =cor(x=views, y= comments)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO VISUALIZAÇOES E COMENTARIOS",CORRELACAO_VALOR ,GRAU));
  
  tres_desvios <- 3 *sd(ted$duration )
  media <- mean(ted$duration)
  TRES_DESVIOS_DA_MEDIA<- media + tres_desvios
  TRES_DESVIOS_DA_MEDIA
  ted%>%
  filter(duration >= TRES_DESVIOS_DA_MEDIA)%>%
  CORRELACAO_VALOR =cor(x=comments, y= languages)
  if(CORRELACAO_VALOR>=0.9){
    GRAU= ('E MUITO FORTE')
  } else if (CORRELACAO_VALOR>=0.7){
    GRAU= ('E FORTE')
  }else if (CORRELACAO_VALOR >=0.5){
    GRAU = ('E MODERADA')
  }else {
    GRAU = ('E FRACA')
  };
  print(paste("CORRELACAO COMENTARIOS E LINGUAS",CORRELACAO_VALOR ,GRAU));
  

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. 
# Calcule a correlação entre o ano e a mediana da duraçãoe interprete o resultado

   
ted <- read_csv("aula-05/data/ted_main.csv.gz")
ted$film_date <- as_datetime(ted$film_date)
ted%>%
  group_by(year(film_date))%>%
  mutate( mediana = mean(duration), correlacao = cor(x = year(film_date), y = mean(duration))) -> subset_ted

subset_ted



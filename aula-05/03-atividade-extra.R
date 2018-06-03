knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, out.width = "600px", out.height="400px")
Sys.setlocale("LC_ALL", "pt_BR")

if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")

library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)
#' ## Histograma
#' 
#' > ATIVIDADE
#' 
#' 1. Estude o material abaixo que explica a construção de histogramas
#' 
#' - [http://flowingdata.com/2017/06/07/how-histograms-work/](http://flowingdata.com/2017/06/07/how-histograms-work/)
#' - [http://tinlizzie.org/histograms/](http://tinlizzie.org/histograms/)
#' 
#' 2. Estude o help da função `geom_histogram`
#' 
#' 3. Crie um histograma da quantidade de visualizações multifacetado por ano de publicação, 
#' restrito aos anos entre 2012 e 2017.
#' 
#' > FIM ATIVIDADE
### Carga dos dados de exemplo

ted_talks <- read_csv("aula-05/data/ted_main.csv.gz") %>%
  mutate( film_date = as_datetime(film_date) %>% as_date()
        , published_date = as_datetime(published_date)) %>%
  filter(published_date >= ymd(20120101))%>%
  select(title, views, published_date) -> subset_ted_talks

subset_ted_talks

subset_ted_talks %>%
  mutate( year = year( published_date )) %>%
  group_by(year) %>%
  summarise(sum_views = sum(views)) -> ted_talks_recentes
  
  V <- ggplot(ted_talks_recentes, aes(x = year))
  V + geom_histogram(binwidth = 0.1)
  V + geom_histogram(color="black", fill="white")
  V + geom_histogram(aes(weight = sum_views), binwidth = 0.1) + ylab("views") + labs(title="Qtde visualizacoes por ano")
  
  




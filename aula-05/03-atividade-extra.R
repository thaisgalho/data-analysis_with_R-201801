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
           , published_date = as_datetime(published_date),  
             year = year(published_date)) %>% 
     filter(year >= 2012)%>% 
     select(title, views, published_date, year) -> subset_ted_talks 

subset_ted_talks 

ggplot(subset_ted_talks, aes(x = views)) +  
     geom_histogram(color="black", fill="white", bins=12) +  
      facet_wrap(~year, ncol=3) + 
     scale_x_continuous(labels = scales::comma) + 
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
     labs(title="Qtde visualizacoes por ano") 
 


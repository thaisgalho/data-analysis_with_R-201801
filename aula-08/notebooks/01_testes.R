## ----setup, include=FALSE, echo=FALSE, message=FALSE, error=FALSE--------
install.packages("MonetDBLite")
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
Sys.setlocale("LC_ALL", "pt_BR")

library(tidyverse)

## ------------------------------------------------------------------------
library(MonetDBLite)
library(tidyverse)

if( str_detect(getwd(), "notebooks")) {
  dbdir <- "../data/monetdb/ted"
} else {
  dbdir <- "aula-08/data/monetdb/ted"
}

ted_conn <- MonetDBLite::src_monetdblite(dbdir)
ted_main <- tbl( ted_conn, "ted_main" )

ted_main %>%
  group_by(category) %>%
  summarise( mean   = mean( views )
           , sd     = sd( views )
           , median = median( views )
           , iqr    = quantile( views, 0.75 ) - quantile( views, 0.25 )
           , length = n_distinct( url )) %>%
  ungroup() %>%
  arrange(desc(mean)) -> query_summary_ted_main

query_summary_ted_main

## ----warning=FALSE-------------------------------------------------------
ted_top_categories <-
  ted_main %>%
  filter(category %in% c("Jaw-dropping", "Funny", "Inspiring"))

ted_top_categories %>%
  collect() %>%
  ggplot(aes(x=category, y=views / 1000000)) +
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(limits=c(0, 10), breaks = seq(from=0, to=50, by = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45)) +
  labs( x = "Categorias"
      , y = "Visualizações (em milhões)"
      , title = "Boxplot de categorias com maior média de visualizações"
      , subtitle = "Limitado ao máximo de 10 milhões de visualizações.")

## ------------------------------------------------------------------------
wilcox.test(views ~ category, data = ted_top_categories, alternative = "two.sided", subset = category %in% c("Inspiring", "Funny"), conf.int = TRUE)

## ------------------------------------------------------------------------
wilcox.test(views ~ category, data = ted_top_categories, alternative = "two.sided", subset = category %in% c("Inspiring", "Jaw-dropping"), conf.int = TRUE)

## ------------------------------------------------------------------------
wilcox.test(views ~ category, data = ted_top_categories, alternative = "two.sided", subset = category %in% c("Funny", "Jaw-dropping"), conf.int = TRUE)

## ------------------------------------------------------------------------

ted_top_categories %>%
  collect() %>%
  mutate( category = factor( category )) -> local_ted_top_categories

kruskal.test(views ~ category, data = local_ted_top_categories)

## ------------------------------------------------------------------------
pairwise.wilcox.test(local_ted_top_categories$views, local_ted_top_categories$category, p.adjust.method = "BH")

## ------------------------------------------------------------------------
ted_main %>%
  collect() %>%
  ggplot( aes( x = category, y = views )) +
  geom_boxplot( notch = TRUE ) +
  scale_y_continuous( limits = c( 0, 1*10^7 )) +
  theme_minimal() +
  theme( axis.text.x = element_text( angle = 45 ))

## ------------------------------------------------------------------------
with( collect( ted_main ) %>% mutate( category = factor( category )), 
     pairwise.wilcox.test(views, category, p.adjust.method = "BH")) ->
  pairwise_matrix

pairwise_matrix

## ------------------------------------------------------------------------
perm_fun <- function( v_1, v_2 ) {
  x <- c( v_1, v_2 )
  n1 <- length( v_1 )
  n2 <- length( v_2 )
  n <- n1 + n2  
  idx_b <- sample(1:n, n1)  
  idx_a <- setdiff(1:n, idx_b)  
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

pull_views_from <- function( tbl, which_category ) {
  tbl %>% filter( category == which_category ) %>% pull( views )
}

plot_diffs <- function( permutation_diffs, mean_1, mean_2 ) {
    ggplot( aes( x = permutation_diffs ), data = data_frame( permutation_diffs )) +
    geom_histogram( bins = 100, fill = "lightgray", color = "black", alpha = 0.6 ) +
    geom_vline( xintercept = mean_1 - mean_2 ) +
    theme_minimal()
}

## ------------------------------------------------------------------------
perm_diffs <- rep(0, 1000)

all_courageous  <- pull_views_from( ted_main, which_category = "Courageous" )
all_beautiful   <- pull_views_from( ted_main, which_category = "Beautiful"  )
mean_courageous <- mean( all_courageous )
mean_beautiful  <- mean( all_beautiful )

for(i in 1:1000)
  perm_diffs[i] = perm_fun( all_beautiful, all_courageous )

plot_diffs( perm_diffs, mean_courageous, mean_beautiful )

## ------------------------------------------------------------------------
perm_diffs <- rep(0, 1000)

all_inspiring    <- pull_views_from( ted_main, which_category = "Inspiring" )
all_fascinating  <- pull_views_from( ted_main, which_category = "Fascinating"  )
mean_inspiring   <- mean( all_inspiring )
mean_fascinating <- mean( all_fascinating )

for(i in 1:1000)
  perm_diffs[i] = perm_fun( all_inspiring, all_fascinating )

plot_diffs( perm_diffs, mean_inspiring, mean_fascinating )

## ------------------------------------------------------------------------

# Falamos sobre Funções de distribuição cumulativa na aula sobre testes.
# A função ecdf cria uma distribuição cumulativa empírica a partir de uma série de valores observados
f <- ecdf(perm_diffs)

# Qual a probabilidade de observarmos uma diferença do tamanho da diferença entre as médias de visualização dos vídeos categorizados como Inspiring e dos vídeos categorizados como Fascinating?
1 - f(mean_inspiring - mean_fascinating)


## ------------------------------------------------------------------------
plot(f)

## ------------------------------------------------------------------------
summary(f)

## ------------------------------------------------------------------------
MonetDBLite::monetdblite_shutdown()


rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, purrr, tidyverse, data.table, scales, ggplot2)
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
# Path del cual se levantan todos los archivos de probabilidades.
PROBS_PATH    <- '../../probs/'

# Donde se guarda el resultado.
ENSAMPLE_PATH <- '../../ensamples/ranking_voting/'

MILLON    <- 1000000
FROM_RANK <- 29 * MILLON
TO_RANK   <- 31 * MILLON
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
cls <- function() cat("\014")

str_datetime <- function()  format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3")

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

save_output <- function(output, from, to) {
  groups <- output %>% group_by(Predicted) %>% count()
  positives <- groups %>% filter(Predicted == 1) %>% pull(n)
  negatives <- groups %>% filter(Predicted == 0) %>% pull(n)

  OUTPUT_FILE_PATH <- paste(
    ENSAMPLE_PATH, 
    str_datetime(),
    '-ensample-ranking_voting-from_', from,
    '-pto_', to,
    '-pos_', positives,
    '-neg_', negatives,
    '.csv', 
    sep=''
  )
  write_csv(output, OUTPUT_FILE_PATH)
}

load_unified_result <- function(results_path, pattern = '*.csv') {
  setwd(this.path::this.dir())
  list.files(path = results_path, pattern = pattern) %>%
    map(\(filename) list(paste(results_path, filename, sep=''), filename)) %>%
    map(\(args) as.data.frame(read.csv(args[[1]])) %>% mutate(filename = args[[2]])) %>%
    reduce(\(a, b)  a %>% union_all(b))
}



df_rankings <- function(probs) {
  probs['ranking'] <- probs %>% frankv(cols=c('Predicted'))
  
  probs %>%
    group_by(numero_de_cliente) %>%
    summarise(ranking = sum(ranking))
}

ranking_voting <- function(probs, from_rank, to_rank) {
  ordered_raking <- df_rankings(probs) %>% arrange(ranking)

  ordered_raking %>%
    mutate(Predicted = ifelse(ranking >= from_rank & ranking <= to_rank, 1, 0)) %>% 
    select(numero_de_cliente, Predicted)
}

show_info <-function(from, to, positives, negatives) {
  print(paste('from: ', from, ' / to: ',  to,  ' / pos: ',  positives, ' / neg: ', negatives, sep=''))
}

plot_rankings <- function(rankings) {
  min_val <- rankings %>% select(ranking) %>% min()
  max_val <- rankings %>% select(ranking) %>% max()
  
  ggplot(data=rankings, aes(x=ranking)) + 
    geom_histogram(color="black", fill="blue")+
    scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    ggtitle(paste('Distribucion de ranking de clientes (min:', min_val, ', max:', max_val, ')', sep='')) +
    xlab('Ranking (En millones)') +
    ylab('Clientes')
}


pos_count <- function(df) df %>% filter(Predicted == 1) %>% count()
neg_count <- function(df) df %>% filter(Predicted == 0) %>% count()
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
# Se carga  todos lo archivos de probabilidades en un único data frame(union all).
probs <- load_unified_result(PROBS_PATH)

plot_rankings(df_rankings(probs))

result    <- ranking_voting(probs, FROM_RANK, TO_RANK)
positives <- pos_count(result)
negatives <- neg_count(result)

show_info(FROM_RANK, TO_RANK, positives, negatives)
save_output(result, FROM_RANK, TO_RANK)


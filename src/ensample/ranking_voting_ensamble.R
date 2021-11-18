rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, purrr, tidyverse, data.table)
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

TOP_RANKINGS  <- seq(12500, 13500, by=100)
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

save_output <- function(output, top) {
  groups <- output %>% group_by(Predicted) %>% count()
  positives <- groups %>% filter(Predicted == 1) %>% pull(n)
  negatives <- groups %>% filter(Predicted == 0) %>% pull(n)

  OUTPUT_FILE_PATH <- paste(
    ENSAMPLE_PATH, 
    str_datetime(),
    '-ensample-ranking_voting-top_', top,
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

ranking_voting <- function(probs, top_positives = 13000) {
  probs['ranking'] <- probs %>% frankv(cols=c('Predicted'))

  ordered_raking <- probs %>%
    group_by(numero_de_cliente) %>%
    summarise(ranking = sum(ranking)) %>% 
    arrange(ranking)

  ordered_raking %>%
    mutate(Predicted = ifelse(as.numeric(row_number(numero_de_cliente)) <= top_positives, 1, 0)) %>% 
    select(numero_de_cliente, Predicted)
}

show_info <-function(top, positives=0, negatives=0) {
  print(paste('top: ', top, ' / pos: ',  positives, ' / neg: ', negatives, sep=''))
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

cls()
for(top in TOP_RANKINGS) {
  result    <- ranking_voting(probs, top_positives = top)
  positives <- pos_count(result)
  negatives <- neg_count(result)

  show_info(top, positives, negatives)
  save_output(result, top)
}


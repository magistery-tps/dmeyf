rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, purrr, tidyverse)
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
INPUT_PATH       <- '../../probs/'
ENSAMPLE_PATH    <- '../../ensamples/soft_voting/'
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functiions
# ------------------------------------------------------------------------------------------------------------
str_datetime <- function()  format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3")

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

save_output <- function(output, config_type, apply_fn) {
  groups <- output %>% group_by(Predicted) %>% count()
  negatives <- groups[1, 2]
  positives <- groups[2, 2]
  
  OUTPUT_FILE_PATH <- paste(
    ENSAMPLE_PATH, 
    str_datetime(), 
    '_ensample_', 
    config_type, 
    '_',
    toString(apply_fn), 
    '_pos_',
    positives,
    '_neg_',
    negatives,
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

soft_voting_startegy <- function(result, cutoff_probs, apply_fn) {
  result %>%
    group_by(numero_de_cliente) %>% 
    summarise(Predicted = apply_fn(Predicted)) %>%
    mutate(Predicted = cutoff_probs >= Predicted) %>%
    select(numero_de_cliente, Predicted)
}
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
input <- load_unified_result(INPUT_PATH)

cutoff_probs = c()

for(cutoff_prob in cutoff_probs) {
  save_output(soft_voting_startegy(input, cutoff_probs, mean), cutoff_probs, 'mean')
  save_output(soft_voting_startegy(input, cutoff_probs, median), cutoff_probs, 'median')
}

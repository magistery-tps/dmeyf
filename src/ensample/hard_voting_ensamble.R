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
CONFIG_TYPES     <- c('greater_than_6.3') #('unweighted') # default') #, 'overfitted', 'adjusted')
INPUT_PATH       <- '../../final/'
ENSAMPLE_PATH    <- '../../ensamples/hard_voting/'
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


load_config <- function(config_type) {
  read.csv(paste(ENSAMPLE_PATH, 'config_', config_type, '.csv', sep=''))
}


sum_fn    <- function(values, weight) sum(values)
mean_fn   <- function(values, weight) mean(values)
median_fn <- function(values, weight) median(values)

weighted_sum_fn     <- function(values, weight) sum(values * weight)
weighted_mean_fn    <- function(values, weight) mean(values * weight)
weighted_median_fn  <- function(values, weight)  median(values * weight)

hard_voting_startegy <- function(result, config, apply_fn = weighted_sum_fn, default_weight=0) {
  result %>% 
    mutate(
      positives = ifelse(Predicted == 1, 1, 0), 
      negatives = ifelse(Predicted == 0, 1, 0)
    ) %>%
    left_join(config)  %>% 
    mutate(weight = ifelse(is.na(weight), default_weight, weight)) %>% 
    group_by(numero_de_cliente) %>% 
    summarise(
      positives = apply_fn(positives, weight), 
      negatives = apply_fn(negatives, weight)
    ) %>%
    mutate(
      Predicted = ifelse(positives >= negatives, 1, 0)
    ) %>% 
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

for(config_type in CONFIG_TYPES) {
  config <- load_config(config_type)
  save_output(hard_voting_startegy(input, config, weighted_sum_fn), config_type, 'weighted_sum')
  save_output(hard_voting_startegy(input, config, weighted_mean_fn), config_type, 'weighted_mean')
}

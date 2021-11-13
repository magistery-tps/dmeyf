rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, purrr, tidyverse,  mlr3measures)
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
PROBS_PATH      <- '../../probs/'
DATASET_PATH    <- '../../dataset/'
# TEST_SET_PATH  <- paste(DATASET_PATH, 'dataset_from_202009_to_202009_pos_1460_neg_233894.csv', sep='')
# TEST_SET_PATH  <- paste(DATASET_PATH, 'dataset_from_202010_to_202010_pos_1485_neg_235921.csv', sep='')
TEST_SET_PATH  <- paste(DATASET_PATH, 'dataset_from_202011_to_202011_pos_1650_neg_237336.csv', sep='')

ENSAMPLE_PATH   <- '../../ensamples/soft_voting/'
CUTEOFF_PROBS   <- seq(0, 0.5,  0.0001)
F_BETA_SCORE    <- 2
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functiions
# ------------------------------------------------------------------------------------------------------------
cls <- function() cat("\014")

str_datetime <- function()  format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3")

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

save_output <- function(output, cutoff_prob, score) {
  groups <- output %>% group_by(Predicted) %>% count()
  positives <- groups %>% filter(Predicted == 1) %>% pull(n)
  negatives <- groups %>% filter(Predicted == 0) %>% pull(n)

  OUTPUT_FILE_PATH <- paste(
    ENSAMPLE_PATH, 
    str_datetime(),
    '-ensample-soft_voting-f_', F_BETA_SCORE, '_score_', score,
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


group_by_id <- function(result, apply_fn) {
  result %>%
    group_by(numero_de_cliente) %>% 
    summarise(Predicted = apply_fn(Predicted))
}

soft_voting_startegy <- function(grouped, cutoff_prob) {
  grouped %>%
    mutate(Predicted = ifelse(Predicted > cutoff_prob, 1, 0)) %>%
    select(numero_de_cliente, Predicted)
}

classes <- function(df) {
  df %>%
    select(Predicted) %>%
    pull() %>% factor()
}

show_info <-function(cutoff_prob, score, positives_count, negative_count, prefix = '') {
  print(paste(
    prefix,
    'Cutoff Prob: ', 
    cutoff_prob, 
    ' / F-',
    F_BETA_SCORE, 
    '-score: ',
    score, 
    ' / pos: ', 
    positives_count,
    ' / neg: ',
    negative_count,
    sep=''
  ))
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
setwd(this.path::this.dir())
test_set <- read.csv(TEST_SET_PATH)
test_set %>% group_by(Predicted) %>% tally()

probs               <- load_unified_result(PROBS_PATH)
probs_grouped_by_id <-  group_by_id(probs, median)
best_score          <- 0
best_score_result   <- NULL

filtered_probs_grouped_by_id <- probs_grouped_by_id %>%
  inner_join(test_set, by='numero_de_cliente', suffix = c('', ".y")) %>%
  selecy(numero_de_cliente, Predicted)
  
nrow(filtered_probs_grouped_by_id)

test_subset <- test_set %>%
  inner_join(filtered_probs_grouped_by_id, by='numero_de_cliente', suffix = c('', ".y")) %>%
  selecy(numero_de_cliente, Predicted)

test_subset %>%
  group_by(Predicted) %>%
  tally()

for(cutoff_prob in CUTEOFF_PROBS) {
  probs_classes <- soft_voting_startegy(filtered_probs_grouped_by_id, cutoff_prob)
  
  positives <- pos_count(probs_classes)
  negatives <- neg_count(probs_classes)
  if(positives == 0 || negatives == 0) {
    print(paste(cutoff_prob, ' cuteoff_prob does not have positives.', sep=''))
    next
  }

  score <- fbeta(
    truth    = classes(test_subset), 
    response = classes(probs_classes),
    positive = '1',
    beta     = F_BETA_SCORE
  )
  if(is.na(score)) {
    show_info(cutoff_prob, score, positives, negatives)
    next
  }

  if(score > best_score) {
    best_score <- score
    best_score_result <- probs_classes
    show_info(cutoff_prob, best_score, positives, negatives, 'BEST ==> ')
  } else {
    show_info(cutoff_prob, best_score, positives, negatives)
  }
}

save_output(best_score_result, cutoff_prob, best_score)





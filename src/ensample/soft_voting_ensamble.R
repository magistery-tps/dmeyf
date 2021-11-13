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
TEST_SET_PATH  <- paste(DATASET_PATH, 'dataset_from_202009_to_202009_pos_1460_neg_233894.csv', sep='')
# TEST_SET_PATH  <- paste(DATASET_PATH, 'dataset_from_202010_to_202010_pos_1485_neg_235921.csv', sep='')
# TEST_SET_PATH  <- paste(DATASET_PATH, 'dataset_from_202011_to_202011_pos_1650_neg_237336.csv', sep='')

ENSAMPLE_PATH   <- '../../ensamples/soft_voting/'
CUTEOFF_PROBS   <- seq(0.01, 0.6,  0.00001)
F_BETA_SCORE    <- 1.5
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functiions
# ------------------------------------------------------------------------------------------------------------
str_datetime <- function()  format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3")

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

save_output <- function(output, cutoff_prob, score) {
  groups <- output %>% group_by(Predicted) %>% count()
  negatives <- groups[1, 2]
  positives <- groups[2, 2]
  
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

soft_voting_startegy <- function(grouped, cutoff_prob, apply_fn) {
  grouped %>%
    mutate(Predicted = ifelse(cutoff_prob >= Predicted, 1, 0)) %>%
    select(numero_de_cliente, Predicted)
}

classes <- function(df) {
  df %>%
    select(Predicted) %>%
    pull() %>% factor()
}

show_info <-function(cutoff_prob, score, prefix = '') {
  print(paste(
    prefix,
    'Cutoff Prob: ', 
    cutoff_prob, 
    ' / F-',
    F_BETA_SCORE, 
    '-score: ',
    score, 
    sep=''
  ))
} 
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
test_set            <- read.csv(TEST_SET_PATH)
probs               <- load_unified_result(PROBS_PATH)
probs_grouped_by_id <-  group_by_id(probs, mean)
best_score          <- 0
best_score_result   <- NULL

for(cutoff_prob in CUTEOFF_PROBS) {
  probs_classes <- soft_voting_startegy(probs_grouped_by_id, cutoff_prob, mean)
  
  filtered_probs_classes <- probs_classes %>% 
    filter(numero_de_cliente %in% (test_set %>% pull(numero_de_cliente)))

  positives_count <- filtered_probs_classes %>% filter(Predicted == 1) %>% count()
  negative_count <- filtered_probs_classes %>% filter(Predicted == 0) %>% count()
  if(positives_count == 0 | negative_count == 0) {
    print(paste(cutoff_prob, ' cuteoff_prob does not have positives.', sep=''))
    next
  }

  test_subset <- test_set %>% 
    filter(numero_de_cliente %in% (filtered_probs_classes %>% pull(numero_de_cliente)))
  
  score <- fbeta(
    truth    = classes(test_subset), 
    response = classes(filtered_probs_classes),
    positive = '1',
    beta     = F_BETA_SCORE
  )

  if(score > best_score) {
    best_score <- score
    best_score_result <- probs_classes
    show_info(cutoff_prob, best_score, 'BEST ==> ')
    save_output(best_score_result, cutoff_prob, best_score)
  } else {
    show_info(cutoff_prob, best_score)
  }
}






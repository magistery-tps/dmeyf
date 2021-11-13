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
DATASET_PATH  <- '../../dataset/'
INPUT_PATH   <- paste(DATASET_PATH, 'paquete_premium.csv', sep='')
# Daaset content from 201801 to 202101,
date_ranges  <- list(
  list(from='202011', to='202101'),
  list(from='202011', to='202011'),
  list(from='202010', to='202010'),
  list(from='202009', to='202009')
)
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

search_by_date_range <- function(dataset, from, to) {
  dataset %>% 
    filter(foto_mes >= from & foto_mes <= to) %>% 
    mutate(Predicted = ifelse(clase_ternaria == 'CONTINUA', 0, 1)) %>%
    select(numero_de_cliente, Predicted)
}

output_file_path <- function(path, date_range, groups) {
  positives <- groups %>% filter(Predicted == 1) %>% pull(n)
  negatives <- groups %>% filter(Predicted == 0) %>% pull(n)
  paste(
    path, 
    'dataset_from_', 
    date_range$from, 
    '_to_', 
    date_range$to,
    '_pos_',
    positives,
    '_neg_',
    negatives,
    '.csv',
    sep=''
  )
}
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
print(paste('Load', INPUT_PATH, 'dataset...'))
dataset <- read.csv(INPUT_PATH)

print('Generate dataset subsets...')
for(date_range in date_ranges) {
  print(paste('from:', date_range$from, 'to:', date_range$to))

  dataset_part <- search_by_date_range(dataset, from = date_range$from, to = date_range$to)
  groups      <- dataset_part %>% group_by(Predicted) %>% tally()
  
  write_csv(
    dataset_part, 
    output_file_path(DATASET_PATH, date_range, groups)
  )
}

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
# Path del cual se levantan todos los archivos de probabilidades.
PROBS_PATH    <- '../../probs/'

# Path de dataset
DATASET_PATH  <- '../../dataset/'

# Conjunto de test contra erl cual se compara para calcular F-beta-score.
TEST_SET_PATH <- paste(DATASET_PATH, 'dataset_from_202011_to_202011_pos_1650_neg_237336.csv', sep='')

# Donde se guarda el resutlado.
ENSAMPLE_PATH  <- '../../ensamples/soft_voting/'

# Secuencia de probabilidades de corte a probar.
#                     (From,  To, Step)
CUTEOFF_PROBS  <- seq(0.02035, 1,  0.0000001)
#CUTEOFF_PROBS <- seq(0.01671, 0.1,  0.00000001) # median
# CUTEOFF_PROBS  <- seq(0.01705, 1,  0.00000001) # mean

# Beta utilizado en la métrica F-beta-score para encontrar el mejor punto de corte.
F_BETA_SCORE   <- 2

# Función a aplicar sobre las probabilidades por cliente.
# Nota: En general con la mediana en el publico esta dando valores mas bajos.
APPLY_FN       <- mean # median

# Cuanta paciencia tiene el algoritmo hasta encontrar el próximo mejor punto de corte.
MAX_PATIENCE   <- 500
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

save_output <- function(output, cutoff_prob) {
  groups <- output %>% group_by(Predicted) %>% count()
  positives <- groups %>% filter(Predicted == 1) %>% pull(n)
  negatives <- groups %>% filter(Predicted == 0) %>% pull(n)

  OUTPUT_FILE_PATH <- paste(
    ENSAMPLE_PATH, 
    str_datetime(),
    '-ensample-soft_voting-cutoff_prob_', best_cutoff_prob,
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

show_info <-function(cutoff_prob, score, positives_count=0, negative_count=0, prefix = '') {
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
# Load test set..
setwd(this.path::this.dir())
test_set <- read.csv(TEST_SET_PATH)
test_set %>% group_by(Predicted) %>% tally()

# Se carga  todos lo archivos de probabilidades en un único data frame(union all).
probs <- load_unified_result(PROBS_PATH)

# Se agrupa por el numero de cliente y se aplica la función de agregación APPLY_FN.
probs_grouped_by_id <- group_by_id(probs, APPLY_FN)

# Procuramos tener los mismo clientes en ambos conjuntos test y probs.
filtered_probs_grouped_by_id <- probs_grouped_by_id %>%
  inner_join(test_set, by='numero_de_cliente', suffix = c('', ".y")) %>%
  select(numero_de_cliente, Predicted)

test_subset <- test_set %>%
  inner_join(filtered_probs_grouped_by_id, by='numero_de_cliente', suffix = c('', ".y")) %>%
  select(numero_de_cliente, Predicted)

#--------------------------------------------------------------
# Búsqueda del mejor punto de corte evaluando por F-beta-score
#--------------------------------------------------------------
best_score          <- 0
best_cutoff_prob    <- 0
patiences_step      <- 0

cls()
for(cutoff_prob in CUTEOFF_PROBS) {
  if(patiences_step > MAX_PATIENCE) {
    print(paste('Score does not improve after ', patiences_step, ' steps!', sep=''))
    break
  }

  # Definimos la lcases segun la probabilidad de corte.
  probs_classes <- soft_voting_startegy(filtered_probs_grouped_by_id, cutoff_prob)
  
  # Si tenemos unicamente positivos o negativos skipeamos el paso ya que no es algo esperable.
  positives <- pos_count(probs_classes)
  negatives <- neg_count(probs_classes)
  if(positives == 0 || negatives == 0) {
    print(paste(cutoff_prob, ' cuteoff_prob does not have positives.', sep=''))
    next
  }

  # Calculamos el score
  score <- fbeta(
    truth    = classes(test_subset), 
    response = classes(probs_classes),
    positive = '1',
    beta     = F_BETA_SCORE
  )

  # Si el score nos da NA skipeamos el paso.  
  if(is.na(score)) {
    patiences_step <- patiences_step + 1
    if(patiences_step %% 50 == 0) {
      print(paste('Patience step...', patiences_step, sep=''))      
    }
    break
  }

  if(score > best_score) {
    patiences_step   <- 0
    best_score       <- score
    best_cutoff_prob <- cutoff_prob
    show_info(cutoff_prob, score, positives, negatives)
  } else {
    patiences_step <- patiences_step + 1
    if(patiences_step %% 50 == 0) {
      print(paste('Patience step...', patiences_step, sep=''))      
    }
  }
}


# Si se encontró algún punto de corte guardamos el mejor el resultado.
if(best_score > 0) {
  best_score_result <- soft_voting_startegy(probs_grouped_by_id, best_cutoff_prob)
  positives         <- pos_count(best_score_result)
  negatives         <- neg_count(best_score_result)

  show_info(best_cutoff_prob, best_score, positives, negatives, 'BEST >==> ')
  save_output(best_score_result, best_cutoff_prob)
}


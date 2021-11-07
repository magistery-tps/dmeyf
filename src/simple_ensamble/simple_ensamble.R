#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# install.packages('pacman')
library(pacman)
p_load(this.path, purrr, tidyverse)

load_unified_result <- function(results_path, unique_column, pattern = '*.csv') {
  list.files(path = results_path, pattern = pattern) %>%
    map(\(filename) paste(results_path, '/', filename, sep='')) %>%
    map(\(file_path) as.data.frame(read.csv(file_path))) %>%
    reduce(\(a, b)  a %>% union_all(b))
}

setwd(this.path::this.dir())
result <- load_unified_result(
  results_path          = '../../kaggle', 
  unique_column = 'numero_de_cliente'
)

ensamble_result <- result %>% 
  mutate(positives = ifelse(Predicted == 1, 1, 0), negatives = ifelse(Predicted == 0, 1, 0)) %>% 
  group_by(numero_de_cliente) %>% 
  summarize(positives = sum(positives), negatives = sum(negatives)) %>%
  mutate(Predicted = ifelse(positives >= negatives, 1, 0)) %>%
  select(numero_de_cliente, Predicted)

ensamble_result %>% group_by(Predicted) %>% count()

write.csv(ensamble_result, './ensample.csv', row.names = FALSE, quote=FALSE)


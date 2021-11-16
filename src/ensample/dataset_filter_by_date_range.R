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
  list(from='202101', to='202101'),
  list(from='202012', to='202012'),
  list(from='202011', to='202011'),
  list(from='202010', to='202010'),
  list(from='202009', to='202009'),
  list(from='202008', to='202008'),
  list(from='202007', to='202007'),
  list(from='202006', to='202006'),
  list(from='202005', to='202005')
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

plot_monthly_downs <- function(datasset, continua=TRUE) {
  service_down_by_month <- dataset %>%
    mutate(clase_ternaria = ifelse(clase_ternaria == "", "NO_CARGADO", clase_ternaria))
   
  if(continua == FALSE) {
    service_down_by_month <- service_down_by_month %>%
      filter(clase_ternaria != "CONTINUA") %>% 
      filter(clase_ternaria != "NO_CARGADO")
  }
  
  service_down_by_month <- service_down_by_month %>%
    group_by(foto_mes, clase_ternaria) %>% 
    arrange(foto_mes) %>%
    tally() %>% 
    mutate(foto_mes = as.character(foto_mes))
  
  ggplot(service_down_by_month, aes(x=foto_mes, y=n, group=clase_ternaria)) +
    geom_line(aes(color=clase_ternaria)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    ggtitle("Bajas mensuales") +
    labs(y="Clientes", x = "Mes")
}

generate_monthly_datasets <- function(dataset, date_ranges) {
  print('Generate monthly datasets...')
  for(date_range in date_ranges) {
    print(paste('from:', date_range$from, 'to:', date_range$to))
    
    dataset_part <- search_by_date_range(dataset, from = date_range$from, to = date_range$to)
    groups      <- dataset_part %>% group_by(Predicted) %>% tally()
    
    write_csv(
      dataset_part, 
      output_file_path(DATASET_PATH, date_range, groups)
    )
  }
}

load_dataset <- function() {
  print(paste('Load', INPUT_PATH, 'dataset...'))
  read.csv(INPUT_PATH)
}

# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
dataset <- load_dataset()

generate_monthly_datasets(dataset, date_ranges)

plot_monthly_downs(dataset, continua=TRUE)
plot_monthly_downs(dataset, continua=FALSE)

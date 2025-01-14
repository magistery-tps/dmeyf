rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, tidyverse)
p_load_gh('adrianmarino/commons')
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
DATASET_PATH <- '../../dataset/'
INPUT_PATH   <- paste(DATASET_PATH, 'paquete_premium.csv', sep='')
OUTPUT_PATH  <- paste(DATASET_PATH, 'dataset_discharge_yes_no.csv', sep='')
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
write_csv <- function(df, path) write.csv(df, path, row.names = TRUE, quote=FALSE)

filter_discharges <- function(datasset) {
  dataset %>%
    filter(clase_ternaria != "CONTINUA") %>% 
    filter(clase_ternaria != "")
}

load_dataset <- function() {
  print(paste('Load', INPUT_PATH, 'dataset...'))
  read.csv(INPUT_PATH)
}

discharges_summary <- function(df) {
  print(df %>% group_by(clase_ternaria) %>% tally())
}
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
dataset <- load_dataset()

ds <- dataset %>% mutate(clase_ternaria = ifelse('CONTINUA' == clase_ternaria, 0, 1))

write_csv(ds, OUTPUT_PATH)

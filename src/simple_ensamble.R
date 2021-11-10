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
# Functiions
# ------------------------------------------------------------------------------------------------------------
str_datetime <- function()  format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3")

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

load_unified_result <- function(results_path, pattern = '*.csv') {
  setwd(this.path::this.dir())
  list.files(path = results_path, pattern = pattern) %>%
    map(\(filename) list(paste(results_path, filename, sep=''), filename)) %>%
    map(\(args) as.data.frame(read.csv(args[[1]])) %>% mutate(filename = args[[2]])) %>%
    reduce(\(a, b)  a %>% union_all(b))
}

weighted_voting_startegy <- function(result, config, default_weight=0.1) {
  result %>% 
    mutate(
      positives = ifelse(Predicted == 1, 1, 0), 
      negatives = ifelse(Predicted == 0, 1, 0)
    ) %>%
    left_join(config)  %>% 
    mutate(weight = ifelse(is.na(weight), default_weight, weight)) %>% 
    group_by(numero_de_cliente) %>% 
    summarise(
      positives = sum(positives * weight), 
      negatives = sum(negatives * weight)
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
# Global variables
# ------------------------------------------------------------------------------------------------------------
INPUT_PATH       <- '../mesetas/'
ENSAMPLE_PATH    <- '../ensamples/'
OUTPUT_FILE_PATH <- paste(ENSAMPLE_PATH, str_datetime(), '_ensample.csv' , sep='')
CONFIG_FILE_PATH <- paste(ENSAMPLE_PATH, 'config_overfitted_2.csv', sep='')
config           <- read.csv(CONFIG_FILE_PATH)
DEFAULT_WEIGHT   <- 0
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
input   <- load_unified_result(INPUT_PATH)
output  <- weighted_voting_startegy(input, config, DEFAULT_WEIGHT)

output %>% 
  group_by(Predicted) %>% 
  count()

write_csv(output, OUTPUT_FILE_PATH)


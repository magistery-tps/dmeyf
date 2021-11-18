rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, tidyverse, lightgbm)
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
DATASET_PATH  <- '../../dataset/'

INPUT_FILE  <- 'dataset_discharge_yes_no'
# INPUT_FILE  <- 'discharges'
INPUT_PATH  <- paste(DATASET_PATH, INPUT_FILE, '.csv', sep='')

OUTPUT_PATH  <- '../../data/'
OUTPUT_IMG  <- paste(OUTPUT_PATH, INPUT_FILE, '.png', sep='')
OUTPUT_CSV  <- paste(OUTPUT_PATH, INPUT_FILE, '.csv', sep='')
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
load_dataset <- function() {
  print(paste('Load', INPUT_PATH, 'dataset...'))
  read.csv(INPUT_PATH)
}

write_csv <- function(df, path) write.csv(df, path, row.names = FALSE, quote=FALSE)

discharges_summary <- function(df) {
  print(df %>% group_by(clase_ternaria) %>% tally())
}

missings_summary <- function(
  df, 
  missings = c(NA, NULL, '')
) {
  df %>% 
    gather(., key = Variable, value = value) %>%
    mutate(is_missing = if_else(value %in% missings, 1, 0)) %>%
    group_by(Variable) %>% 
    summarise(
      `Nª Categorias` = n_distinct(value),
      `Nª Faltantes`   = sum(is_missing),
      `% Faltantes`    = round(`Nª Faltantes` / nrow(df) * 100, 2)
    ) %>%
    arrange(desc(`% Faltantes`), `Nª Categorias`)
}

feature_importance <- function(
  ds,
  params = list(
    objective               = "binary",
    learning_rate           = 0.1,
    max_depth               = -1L,
    min_data_in_leaf        = 1L,
    min_sum_hessian_in_leaf = 1.0
  ),
  exclude    = c('clase_ternaria', 'target', 'foto_mes', 'X'),
  target     = 'target',
  nrounds    = 5L,
  percentage = TRUE
) {
  dtrain <- lgb.Dataset(
    data = as.matrix(ds %>% select(-exclude)), 
    label = ds %>% select(target) %>% pull()
  )

  model <- lgb.train(
    params  = params, 
    data    = dtrain, 
    nrounds = nrounds
  )
  
  lgb.importance(model, percentage = percentage)  
}

append_canaries <- function(ds, count=30, min = 1, max = 1000) {
  column_size <- nrow(ds)
  for(num in 1:count) {
    values <- runif(n = column_size, min = min, max = max)
    ds[paste('canarito_', num, sep='')] <- paste(values)
  }
  ds
}
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
dataset <- load_dataset()
ncol(dataset)
print('Dataset loaded...')

print('To binary class...')
ds <- dataset %>% mutate(target = ifelse(clase_ternaria == "BAJA+2", 1, 0))
rm(dataset)
ncol(ds)

print('Calculate feature importance...')
tree <- feature_importance(ds)
rm(ds)
write_csv(tree, OUTPUT_CSV)

print('Plot feature importance...')
png(OUTPUT_IMG, width = 1000, height = 1000, pointsize = 30)
lgb.plot.importance(tree, top_n = 20L)
dev.off()


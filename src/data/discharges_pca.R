rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, dplyr, tidyverse, ggbiplot)
p_load_gh('adrianmarino/commons')
import('./pca.R')
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------------------------------------
DATASET_PATH  <- '../../dataset/'
DATA_PATH <- '../../data/'


ORIGINAL_PATH  <- paste(DATASET_PATH, 'paquete_premium.csv', sep='')

DISCHARGES_PATH  <- paste(DATASET_PATH, 'discharges.csv', sep='')

important_columns <- c(
  'Visa_status',
  'Visa_Finiciomora',
  'mprestamos_personales', 
  'mdescubierto_preacordado', 
  'Visa_mfinanciacion_limite',
  'tcallcenter',
  'ccaja_ahorro', 
  'mcuenta_corriente', 
  'tpaquete3',
  'Visa_fultimo_cierre',
  'cprestamos_personales',
  'Visa_delinquency', 	
  'mcuentas_saldo', 	
  'mpayroll', 	
  'Visa_msaldopesos', 	
  'Visa_msaldototal', 	
  'mrentabilidad', 	
  'thomebanking', 	
  'mttarjeta_visa_debitos_automaticos', 
  'Master_mfinanciacion_limite', 
  'Visa_mpagado', 
  'mcaja_ahorro', 
  'ccallcenter_transacciones', 
  'mpasivos_margen', 
  'Master_status', 
  'Master_fultimo_cierre', 
  'Master_Finiciomora', 
  'cmobile_app_trx', 
  'mtarjeta_visa_consumo', 
  'ccomisiones_mantenimiento', 
  'Master_mpagominimo', 
  'ccajas_transacciones', 
  'mcomisiones_mantenimiento', 
  'internet', 
  'Visa_mconsumospesos', 
  'Master_msaldototal'
)
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
load_file <- function(path) {
  setwd(this.path::this.dir())
  print(paste('Load', path, 'dataset...'))
  read.csv(path)
}

ds_pca <- function(
  dataset, 
  variables,
  obs.scale       = 1,
  var.scale       = 3,
  varname.adjust  = 2,
  varname.size    = 4,
  alpha           = 0.08,
  show_discharges = TRUE,
  center          = TRUE, 
  scale           = TRUE
) {
  ds <- dataset %>% 
    dplyr::select(c(variables, 'clase_ternaria')) %>% 
    drop_na()

  if(show_discharges) {
    ds_target <- ds %>% 
      mutate(target = ifelse(clase_ternaria == 'BAJA+2', 1, 0)) %>% 
      dplyr::select(target) %>%
      pull() %>% 
      as.factor()
  } else {
    ds_target <- NULL
  }

  ds <- ds %>% 
    dplyr::select(-clase_ternaria) %>%
    select_if(is.numeric)
  
  pca <- prcomp(ds , center = center, scale. = scale)

  plot_pca(
    pca,
    obs.scale      = obs.scale,
    var.scale      = var.scale,
    varname.adjust = varname.adjust,
    varname.size   = varname.size,
    alpha          = alpha,
    groups         = ds_target
  )
}
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------------------------------------
discharges_2 <- load_file(DISCHARGES_PATH) %>% 
  filter(clase_ternaria == 'BAJA+2') %>% 
  drop_na()

png(paste(DATA_PATH, 'discharges_2-pca.png', sep=''), width = 1000, height = 1000, pointsize = 30)
ds_pca(discharges_2, variables = important_columns[1:6], show_discharges = TRUE)
dev.off()

discharges_1 <- load_file(DISCHARGES_PATH) %>% 
  filter(clase_ternaria == 'BAJA+1') %>% 
  drop_na()

png(paste(DATA_PATH, 'discharges_1-pca.png', sep=''), width = 1000, height = 1000, pointsize = 30)
ds_pca(discharges_1, variables = important_columns[1:6], show_discharges = TRUE)
dev.off()

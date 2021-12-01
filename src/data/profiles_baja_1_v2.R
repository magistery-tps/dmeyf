rm( list=ls() )
gc()
# ------------------------------------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------------------------------------
# install.packages('pacman')
library(pacman)
p_load(this.path, dplyr, tidyverse, ggplot2, scales)
setwd(this.path::this.dir())
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Global variables
# ------------------------------------------------------------------------------------------------------------
DATA_PATH          <- '../../data/'
DATASET_PATH       <- '../../dataset/'
DATASET_FILE_PATH  <- paste0(DATASET_PATH, 'discharges-baja_1.csv')
CLUSTERS_PATH      <- paste0(DATASET_PATH, 'clusters-baja_1-k_3.csv')
# ------------------------------------------------------------------------------------------------------------
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------------------------------------
load_file <- function(path) {
  print(paste('Load', path, 'dataset...'))
  read.csv(path)
}

build_data <- function(ds, clusters) {
  ds %>% mutate(
    date                   = foto_mes,
    edad                   = cliente_edad, 
    antiguedad             = cliente_antiguedad,
    nro_movimientos_cuenta = ctrx_quarter, # Movimiento en cuenta en los ultimo 90 días.
    nro_cobros_de_sueldo   = cpayroll_trx, # Pago de sueldo mensual.
    monto_en_caja_ahorro   = mcaja_ahorro, # Cantidad de operaciones en sucursales.
    visa_monto_consumo     = mtarjeta_visa_consumo,       # Consumos en tarjeta visa.
    visa_nro_trans         = ctarjeta_visa_transacciones, # Transacciones mensuales visa.
    saldo_cuentas          = mcuentas_saldo,              # Salen en cuentas.
    deuda_prestamos        = mprestamos_personales,       # Monto adeudado en prestamos.
    comisiones             = mcomisiones_mantenimiento + mcomisiones_otras, # Monto de comisiones.
    visa_limite            = Visa_mfinanciacion_limite,   # Limite de tarjeta
    nro_transferencias     = mtransferencias_emitidas,    # Transferencias realizadas
    pagos_mis_cuentas      = mpagomiscuentas              # Monto pagados 
  ) %>%
    inner_join(clusters, by ='numero_de_cliente')  
}

cluster_data <- function(ds, k) {
  ds %>%
    filter(cluster == k) %>%
    select(
      date,
      nro_movimientos_cuenta,
      nro_cobros_de_sueldo,
      monto_en_caja_ahorro,
      visa_monto_consumo,
      visa_nro_trans,
      saldo_cuentas,
      deuda_prestamos,
      comisiones,
      visa_limite,
      nro_transferencias,
      pagos_mis_cuentas
    ) %>% 
    group_by(date) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    arrange(date)  
}

plot_line <- function(ds, column) {
  ggplot(ds, aes(date, !!sym(column))) + 
    geom_smooth(se = TRUE, method = 'loess', formula='y ~ x') +
    xlab('Mes') +
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

classes        <- function(ds) ds %>% group_by(clase_ternaria) %>% count()
clusters_count <- function(ds) nrow(ds %>% group_by(cluster) %>% count())
# -----------------------------------------------------------------------------------------------------------
dataset  <- load_file(DATASET_FILE_PATH)
clusters <- load_file(CLUSTERS_PATH) %>% 
  mutate(cluster = factor(cluster2)) %>% 
  select(cluster, numero_de_cliente)

ds <- build_data(dataset, clusters)


cluster = 1
cluster_ds1 <- cluster_data(ds, cluster)
filename <- paste0(DATA_PATH, 'cluster_plots-k_', cluster, '.pdf')
pdf(filename)
plot_line(cluster_ds1, 'nro_movimientos_cuenta')
plot_line(cluster_ds1, 'nro_cobros_de_sueldo')
plot_line(cluster_ds1, 'monto_en_caja_ahorro')
plot_line(cluster_ds1, 'visa_monto_consumo')
plot_line(cluster_ds1, 'visa_nro_trans')
plot_line(cluster_ds1, 'saldo_cuentas')
plot_line(cluster_ds1, 'deuda_prestamos')
plot_line(cluster_ds1, 'comisiones')
plot_line(cluster_ds1, 'visa_limite')
plot_line(cluster_ds1, 'nro_transferencias')
plot_line(cluster_ds1, 'pagos_mis_cuentas')
dev.off()
print(paste0(filename, ' created!'))


cluster = 2
cluster_ds2 <- cluster_data(ds, cluster)
filename <- paste0(DATA_PATH, 'cluster_plots-k_', cluster, '.pdf')
pdf(filename)
plot_line(cluster_ds2, 'nro_movimientos_cuenta')
plot_line(cluster_ds2, 'nro_cobros_de_sueldo')
plot_line(cluster_ds2, 'monto_en_caja_ahorro')
plot_line(cluster_ds2, 'visa_monto_consumo')
plot_line(cluster_ds2, 'visa_nro_trans')
plot_line(cluster_ds2, 'saldo_cuentas')
plot_line(cluster_ds2, 'deuda_prestamos')
plot_line(cluster_ds2, 'comisiones')
plot_line(cluster_ds2, 'visa_limite')
plot_line(cluster_ds2, 'nro_transferencias')
plot_line(cluster_ds2, 'pagos_mis_cuentas')
dev.off()
print(paste0(filename, ' created!'))


cluster = 3
cluster_ds3 <- cluster_data(ds, cluster)
filename <- paste0(DATA_PATH, 'cluster_plots-k_', cluster, '.pdf')
pdf(filename)
plot_line(cluster_ds3, 'nro_movimientos_cuenta')
plot_line(cluster_ds3, 'nro_cobros_de_sueldo')
plot_line(cluster_ds3, 'monto_en_caja_ahorro')
plot_line(cluster_ds3, 'visa_monto_consumo')
plot_line(cluster_ds3, 'visa_nro_trans')
plot_line(cluster_ds3, 'saldo_cuentas')
plot_line(cluster_ds3, 'deuda_prestamos')
plot_line(cluster_ds3, 'comisiones')
plot_line(cluster_ds3, 'visa_limite')
plot_line(cluster_ds3, 'nro_transferencias')
plot_line(cluster_ds3, 'pagos_mis_cuentas')
dev.off()
print(paste0(filename, ' created!'))


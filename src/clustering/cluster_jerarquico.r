rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")

library(pacman)
p_load(this.path, dplyr,  tidyverse, ggplot2)

setwd(this.path::this.dir())
#
#
#
#
# ------------------------------------------------------------------------------------------------------------
# Paramertos
# ------------------------------------------------------------------------------------------------------------
K                  <- 3
OUTPUT_PATH        <-"../../dataset/"
INPUT_DATASET_PATH <- paste(OUTPUT_PATH, "paquete_premium.csv", sep='')
# ------------------------------------------------------------------------------------------------------------
#
#
#
#
#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( INPUT_DATASET_PATH, stringsAsFactors= TRUE)
gc()

#achico el dataset
dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  clase_ternaria =="BAJA+1"  & foto_mes>=202001  & foto_mes<=202011, ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()


campos_buenos  <- c(
  "ctrx_quarter", 
  "cpayroll_trx", 
  "mcaja_ahorro", 
  "mtarjeta_visa_consumo", 
  "ctarjeta_visa_transacciones",
  "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
  "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
  "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
  "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
  "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
  "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
  "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
  "mpagomiscuentas")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#pdf("../../data/cluster_jerarquico.pdf")
#plot( hclust.rf )
#dev.off()


h <- 20
distintos <- 0
while(  h>0  &  !( distintos >=2 & distintos <= K ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

# En  dataset,  la columna  cluster2  tiene el numero de cluster
# sacar estadicas por cluster

df_clusters_size <- dataset[  , .N,  cluster2 ]  #tamaño de los clusters
df_clusters_size

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter


df_clusters          <- dataset[ ,numero_de_cliente, cluster2]
found_clusters_count <- nrow(df_clusters_size)
fwrite(
  df_clusters,
  file       = paste(OUTPUT_PATH, 'clusters-baja_1-k_', found_clusters_count, '.csv', sep=''), 
  row.names  = FALSE, 
  quote      = FALSE
)








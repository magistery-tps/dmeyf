#script para HIBRIDAR  semillerios
#ha vuelto la diversidad genetica !

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(pacman)
p_load(this.path, purrr, tidyverse)
require("data.table")
require("yaml")

setwd(this.path::this.dir())

#notar que son de experimentos distintos, y uno sumo 200 semillas y el otro apenas 70
arch1  <- '../../semillero/1431_claudio/E5053_s1431_200.csv'
arch2  <- '../../semillero/E1030/E1030_s1431_80.csv'
ENSAMPLE_PATH    <- '../../semillero/'


#------------------------------------------------------------------------------
#Aqui empieza el programa


#leo lo datasets
modelo1  <- fread( arch1 )
modelo2  <- fread( arch2 )

#esta hermosamente ordenados por probabilidad descente
modelo1[ , ranking := .I ]
modelo2[ , ranking := .I ]


#los ordeno antes de aparearlos
#como tienen exactamente los mismos numero de cliente, todo va a ir bien
setorder( modelo1, numero_de_cliente )
setorder( modelo2, numero_de_cliente )

modelo_nuevo  <- copy(  modelo1[  , c("numero_de_cliente"), with=FALSE ] )

#ATENCION  el 200 es por las 200 semillas del modelo1, y el 70 por las 70 del modelo2
modelo_nuevo[ , ranking  :=  200*modelo1$ranking + 80*modelo2$ranking ]   

setorder( modelo_nuevo, ranking ) 

for(  corte  in seq( 11000, 14000, 1000) ) #imprimo cortes en 10000, 11000, 12000, 13000, 14000 y 15000
{
  modelo_nuevo[ ,  Predicted := 0L ]
  modelo_nuevo[ 1:corte,  Predicted := 1L ]  #me quedo con los primeros

  #genero el archivo para Kaggle
  fwrite( modelo_nuevo[ , c("numero_de_cliente","Predicted"), with=FALSE], 
          file=  paste0(ENSAMPLE_PATH, 'hibridacion_E5053-E1030', "_",corte, ".csv" ),  
          sep= "," )
}



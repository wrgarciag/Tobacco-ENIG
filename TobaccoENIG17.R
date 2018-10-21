rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         1. MACROS-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#////////////////////////////////////////////////////////////////////
#                         1.1 Paths-----
#////////////////////////////////////////////////////////////////////

PathGD  <- paste0("H:/Mi unidad/02Work/5BIDTobacco/")

# Directorios 
LCode <- paste0(PathGD,"Code/")
LData <- paste0(PathGD,"Data/")
LResu <- paste0(PathGD,"Resu/")

#Subcarpetas
LDEnig17 <- paste0(LData,"Enph17/")

setwd(LCode)

#////////////////////////////////////////////////////////////////////
#                         1.2 Librerias-----
#////////////////////////////////////////////////////////////////////

library(data.table)
library(maptools)
library(ggplot2)
library(foreign)
library(readstata13)
library(survey)  # survey analysis
library(gmodels) # crosstable
library(raster)  # mapas
#library(tmap)    # mapas

#////////////////////////////////////////////////////////////////////
#                         1.3 Funciones-----
#////////////////////////////////////////////////////////////////////

weighted.sum <-function(x, w){
  wsum <- sum(x*w,na.rm = TRUE)
  return(wsum)
} 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         2. DATOS-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#////////////////////////////////////////////////////////////////////
#                         2.1 Originales-----
#////////////////////////////////////////////////////////////////////

## Caracteristicas generales personas
DTPers <- as.data.table(read.spss(paste0(LDEnig17,"Caracteristicas generales personas.sav"),
                                  to.data.frame = TRUE,use.value.labels = FALSE))

## Viviendas y hogares
DTVivi <- as.data.table(read.spss(paste0(LDEnig17,"Viviendas y hogares.sav"),
                    to.data.frame = TRUE,use.value.labels = FALSE))

## Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar

## Gastos diarios personales Urbano
DTGDPUrb <- as.data.table(read.spss(paste0(LDEnig17,"Gastos diarios personales Urbano.sav"),
                                  to.data.frame = TRUE,use.value.labels = FALSE))


#////////////////////////////////////////////////////////////////////
#                         2.2 Procesados-----
#////////////////////////////////////////////////////////////////////

## Gastos diarios personales Urbano

# Convertir a caracter la columna de codigo COICOP
DTGDPUrb[,NC4_CC_P1_1:=as.character(NC4_CC_P1_1)]
# Division producto
DTGDPUrb[,division:=substr(as.character(NC4_CC_P1_1),1,2)]
# Division subclase
DTGDPUrb[,subclase:=substr(as.character(NC4_CC_P1_1),1,6)]
# Dummy para productos de tabaco
DTGDPUrb[,dtabaco:=as.numeric(division=="02")]
# Dummy para cigarrillo
DTGDPUrb[,dcigarrillo:=as.numeric(subclase=="022001")]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         3. RESULTADOS-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#?? Incicadores:
# Gasto total diario en cigarrillos [Personas]
# Gasto diario en cigarrillos (% del total gasto) [Personas]



#////////////////////////////////////////////////////////////////////
#                         3.1 Consultas-----
#////////////////////////////////////////////////////////////////////

#?? Unidad de tiempo para gasto: diario,mensual,anual? 
#  La encuesta incluye pregunta de frecuencia

## Gastos diarios personales Urbano

# Gasto y cantidades de cigarrillo y tabaco

DCGUrb <- DTGDPUrb[,list(GastoTotal  = weighted.sum(x=NC4_CC_P5,
                                                    w=FEX_C),
                         ECigarrillo = weighted.sum(x=NC4_CC_P5[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                         QCigarrillo = weighted.sum(x=NC4_CC_P2[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1])),
                         by=list()]

# Gasto como porcentaje del total
DCGUrb[,Wcigarrillo:=ECigarrillo/GastoTotal*100]
# Precio implicito
DCGUrb[,Pcigarrillo:=ECigarrillo/QCigarrillo]
# Guardar
write.csv(DCGUrb,file = paste0(LResu,"1DCGastoUrbDiarioEnig17.csv"), row.names = FALSE, na="")

#////////////////////////////////////////////////////////////////////
#                         3.2 Visualizacion-----
#////////////////////////////////////////////////////////////////////

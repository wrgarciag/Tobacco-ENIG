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

# Subcarpetas
LDEnig07 <- paste0(LData,"Enig07/")

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

### Caracteristicas----

## Caracteristicas generales personas
DTMlPers <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_persona.sav"),
                                  to.data.frame = TRUE,use.value.labels = FALSE))

## Viviendas y hogares
DTMlVivi <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_vivienda.sav"),
                    to.data.frame = TRUE,use.value.labels = FALSE))

DTMlHoga <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_hogar.sav"),
                                  to.data.frame = TRUE,use.value.labels = FALSE))

## Poblacion edad trabajar
DTMlPet <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_pblcion_edad_trbjar.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Poblacion inactiva
DTMlInac <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_inactivo.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Poblacion ocupada
DTMlOcup <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_ocupado.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Poblacion desocupada
DTMlDeso <- as.data.table(read.spss(paste0(LDEnig07,"Ig_ml_desocupado.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Datos del hogar para medir gastos
DTGsHoga <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gs_hogar.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Datos de la vivienda para medir gastos
DTGsVivi <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gs_vivienda.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Gastos diarios personales----
DTGsdpDiasSema <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdp_dias_sem.sav"),
                                    to.data.frame = TRUE,use.value.labels = FALSE))

DTGsdpDias <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdp_gas_dia.sav"),
                                         to.data.frame = TRUE,use.value.labels = FALSE))

DTGsdpPerc <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdp_perceptores.sav"),
                                     to.data.frame = TRUE,use.value.labels = FALSE))

## Gastos diarios unidad de gasto----

DTGsduCaraAlim <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdu_caract_alim.sav"),
                                       to.data.frame = TRUE,use.value.labels = FALSE))

DTGsduDiasSema <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdu_dias_sem.sav"),
                                      to.data.frame = TRUE,use.value.labels = FALSE))

DTGsduGastAlim <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdu_gasto_alimentos_cap_c.sav"),
                                         to.data.frame = TRUE,use.value.labels = FALSE))

DTGsduMerc <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsdu_mercado.sav"),
                                        to.data.frame = TRUE,use.value.labels = FALSE))

## Gastos menos frecuentes----

DTGsmfComp <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsmf_compra.sav"),
                                         to.data.frame = TRUE,use.value.labels = FALSE))

DTGsmfFormAdqu <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsmf_forma_adqui.sav"),
                                        to.data.frame = TRUE,use.value.labels = FALSE))

DTGsmfServPubl <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gsmf_serv_pub.sav"),
                                            to.data.frame = TRUE,use.value.labels = FALSE))

## Gastos rurales

DTGssrCaraAlim <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gssr_caract_alim.sav"),
                                        to.data.frame = TRUE,use.value.labels = FALSE))

DTGssrGastSema <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gssr_gas_sem.sav"),
                                            to.data.frame = TRUE,use.value.labels = FALSE))

DTGssrGastAlim <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gssr_gasto_alimentos_cap_c.sav"),
                                            to.data.frame = TRUE,use.value.labels = FALSE))

DTGssrMerc <- as.data.table(read.spss(paste0(LDEnig07,"Ig_gssr_mercado.sav"),
                                            to.data.frame = TRUE,use.value.labels = FALSE))


#////////////////////////////////////////////////////////////////////
#                         2.2 Procesados-----
#////////////////////////////////////////////////////////////////////

## Gastos diarios personales Urbano

# Convertir a caracter la columna de codigo COICOP
DTGsdpDias[,GDP_ARTCLO:=as.character(GDP_ARTCLO)]
# Renombrar factor de expansion
DTGsdpDias[,FEX_C:=FACTOR_EXPANSION_EC_E1]
# Division producto
DTGsdpDias[,division:=substr(as.character(GDP_ARTCLO),1,2)]
# Division subclase
DTGsdpDias[,subclase:=substr(as.character(GDP_ARTCLO),1,6)]
# Dummy para productos de tabaco
DTGsdpDias[,dtabaco:=as.numeric(division=="02")]
# Dummy para cigarrillo
DTGsdpDias[,dcigarrillo:=as.numeric(subclase=="022001")]

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
DCGUrb <- DTGsdpDias[,list(GastoTotal  = weighted.sum(x=GDP_VALOR_PGDO_ESTMDO,
                                                    w=FEX_C),
                           ECigarrillo = weighted.sum(x=GDP_VALOR_PGDO_ESTMDO[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                           QCigarrillo    = weighted.sum(x=GDP_CNTDAD_ADQURDA[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                           GastoTotalMes  = weighted.sum(x=GDP_VALOR_PGDO_ESTMDO_MES_AJST,
                                                      w=FEX_C),
                           ECigarrilloMes = weighted.sum(x=GDP_VALOR_PGDO_ESTMDO[dcigarrillo==1],
                                                       w=FEX_C[dcigarrillo==1]),
                           QCigarrilloMes = weighted.sum(x=GDP_CNTDAD_ADQURDA_MES_AJST[dcigarrillo==1],
                                                      w=FEX_C[dcigarrillo==1])),
                           by=list()]

# Gasto como porcentaje del total
DCGUrb[,Wcigarrillo:=ECigarrillo/GastoTotal*100]
# Precio implicito
DCGUrb[,Pcigarrillo:=ECigarrillo/QCigarrillo]
# Gasto como porcentaje del total (Mes)
DCGUrb[,WcigarrilloMes:=ECigarrilloMes/GastoTotalMes*100]
# Precio implicito (Mes)
DCGUrb[,PcigarrilloMes:=ECigarrilloMes/QCigarrilloMes]

# Guardar
write.csv(DCGUrb,file = paste0(LResu,"1DCGastoUrbDiarioEnig07.csv"), row.names = FALSE, na="")

#////////////////////////////////////////////////////////////////////
#                         3.2 Visualizacion-----
#////////////////////////////////////////////////////////////////////

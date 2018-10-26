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

### Caracteristicas----

## Caracteristicas generales personas
DTPers <- as.data.table(read.spss(paste0(LDEnig17,"Caracteristicas generales personas.sav"),
                                  to.data.frame = TRUE,use.value.labels = FALSE))

## Viviendas y hogares
DTVivi <- as.data.table(read.spss(paste0(LDEnig17,"Viviendas y hogares.sav"),
                    to.data.frame = TRUE,use.value.labels = FALSE))

### Gastos diarios----

# ## Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar
# DTGDHUrbCom <- as.data.table(read.spss(
#   paste0(LDEnig17,"Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar.sav"),
#                                    to.data.frame = TRUE,use.value.labels = FALSE))

## Gastos diarios personales Urbano
DTGDPUrb <- as.data.table(read.spss(paste0(LDEnig17,"Gastos diarios personales Urbano.sav"),
                                  to.data.frame = TRUE,use.value.labels = FALSE))

# ## Gastos diarios Urbano - Capitulo C
# DTGDUrbC <- as.data.table(read.spss(paste0(LDEnig17,"Gastos diarios Urbano - Capitulo C.sav"),
#                                     to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos diarios Urbanos - Mercados
# DTGDUrbMerc <- as.data.table(read.spss(paste0(LDEnig17,"Gastos diarios Urbanos - Mercados.sav"),
#                                     to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos diarios Urbanos
# DTGDUrb <- as.data.table(read.spss(paste0(LDEnig17,"Gastos diarios Urbanos.sav"),
#                                        to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ### Gastos personales----
# 
# ## Gastos personales Rural - Comidas preparadas fuera del Hogar
# DTGPRurCom <- as.data.table(read.spss(paste0(LDEnig17,"Gastos personales Rural - Comidas preparadas fuera del Hogar.sav"),
#                                    to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos personales Rural
# DTGPRur <- as.data.table(read.spss(paste0(LDEnig17,"Gastos personales Rural.sav"),
#                                       to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos personales Urbano - Comidas preparadas fuera del hogar
# 
# DTGPUrb <- as.data.table(read.spss(paste0(LDEnig17,"Gastos personales Urbano - Comidas preparadas fuera del hogar.sav"),
#                                    to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ### Gastos semanales----
# 
# ## Gastos semanales Rural - Capitulo C
# DTGSemRurC <- as.data.table(read.spss(paste0(LDEnig17,"Gastos semanales Rural - Capitulo C.sav"),
#                                    to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos semanales Rural - Comidas preparadas fuera del hogar
# DTGSemRurCom <- as.data.table(read.spss(paste0(LDEnig17,"Gastos semanales Rural - Comidas preparadas fuera del hogar.sav"),
#                                       to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos semanales Rurales - Mercados
# DTGSemRurMdos <- as.data.table(read.spss(paste0(LDEnig17,"Gastos semanales Rurales - Mercados.sav"),
#                                       to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos semanales Rurales
# DTGSemRur <- as.data.table(read.spss(paste0(LDEnig17,"Gastos semanales Rurales.sav"),
#                                          to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ### Gastos menos frecuentes----
# 
# ## Gastos menos frecuentes - Articulos
# DTGNFreqA <- as.data.table(read.spss(paste0(LDEnig17,"Gastos menos frecuentes - Articulos.sav"),
#                                      to.data.frame = TRUE,use.value.labels = FALSE))
# 
# ## Gastos menos frecuentes - Medio de pago
# DTGNFreqMP <- as.data.table(read.spss(paste0(LDEnig17,"Gastos menos frecuentes - Medio de pago.sav"),
#                                      to.data.frame = TRUE,use.value.labels = FALSE))


#////////////////////////////////////////////////////////////////////
#                         2.2 Procesados-----
#////////////////////////////////////////////////////////////////////

## Viviendas----

NamesVivi <- c("VIVIENDA","DIRECTORIO",
               "REGION","DOMINIO","P3","P8520S1A1","PERIODO")  

DPVivi <- DTVivi[,colnames(DTVivi) %in% NamesVivi,with=FALSE]

#Limpiar
rm(NamesVivi)

## Gastos diarios personales Urbano----

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

# Mensualizar gasto

DTGDPUrb[ ,ValorPgdoMes:=NC4_CC_P5]
DTGDPUrb[ ,CantAdquMes:=NC4_CC_P2]

DTGDPUrb[,ValorPgdoMes:=ifelse(NC4_CC_P6==1,ValorPgdoMes*4.28,ValorPgdoMes)]
DTGDPUrb[,ValorPgdoMes:=ifelse(NC4_CC_P6==4,ValorPgdoMes*2,ValorPgdoMes)]
DTGDPUrb[,ValorPgdoMes:=ifelse(NC4_CC_P6==6,ValorPgdoMes/2,ValorPgdoMes)]
DTGDPUrb[,ValorPgdoMes:=ifelse(NC4_CC_P6==7,ValorPgdoMes/3,ValorPgdoMes)]

DTGDPUrb[,CantAdquMes:=ifelse(NC4_CC_P6==1,CantAdquMes*4.28,CantAdquMes)]
DTGDPUrb[,CantAdquMes:=ifelse(NC4_CC_P6==4,CantAdquMes*2,CantAdquMes)]
DTGDPUrb[,CantAdquMes:=ifelse(NC4_CC_P6==6,CantAdquMes/2,CantAdquMes)]
DTGDPUrb[,CantAdquMes:=ifelse(NC4_CC_P6==7,CantAdquMes/3,CantAdquMes)]


## Unir con datos vivienda

DPGDPUrb <- merge(DTGDPUrb,DPVivi,
                  by=c("DIRECTORIO"),all.x=TRUE)


## Gastos diarios personales Rural

# Convertir a caracter la columna de codigo COICOP
DTGPRur[,NC2R_CE_P2:=as.character(NC2R_CE_P2)]
# Division producto
DTGPRur[,division:=substr(as.character(NC2R_CE_P2),1,2)]
# Division subclase
DTGPRur[,subclase:=substr(as.character(NC2R_CE_P2),1,6)]
# Dummy para productos de tabaco
DTGPRur[,dtabaco:=as.numeric(division=="02")]
# Dummy para cigarrillo
DTGPRur[,dcigarrillo:=as.numeric(subclase=="022001")]
DTGPRur[,dcigarrillo:=dtabaco]
# Mensualizar gasto

DTGPRur[ ,ValorPgdoMes:=NC2R_CE_P7]
DTGPRur[ ,CantAdquMes:=NC2R_CE_P4S1]

DTGPRur[,ValorPgdoMes:=ifelse(NC2R_CE_P8==1,ValorPgdoMes*4.28,ValorPgdoMes)]
DTGPRur[,ValorPgdoMes:=ifelse(NC2R_CE_P8==4,ValorPgdoMes*2,ValorPgdoMes)]
DTGPRur[,ValorPgdoMes:=ifelse(NC2R_CE_P8==6,ValorPgdoMes/2,ValorPgdoMes)]
DTGPRur[,ValorPgdoMes:=ifelse(NC2R_CE_P8==7,ValorPgdoMes/3,ValorPgdoMes)]

DTGPRur[,CantAdquMes:=ifelse(NC2R_CE_P8==1,CantAdquMes*4.28,CantAdquMes)]
DTGPRur[,CantAdquMes:=ifelse(NC2R_CE_P8==4,CantAdquMes*2,CantAdquMes)]
DTGPRur[,CantAdquMes:=ifelse(NC2R_CE_P8==6,CantAdquMes/2,CantAdquMes)]
DTGPRur[,CantAdquMes:=ifelse(NC2R_CE_P8==7,CantAdquMes/3,CantAdquMes)]

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
                                                    w=FEX_C[dcigarrillo==1]),
                         GastoTotalMes  = weighted.sum(x=ValorPgdoMes,
                                                       w=FEX_C),
                         ECigarrilloMes = weighted.sum(x=ValorPgdoMes[dcigarrillo==1],
                                                       w=FEX_C[dcigarrillo==1]),
                         QCigarrilloMes = weighted.sum(x=CantAdquMes[dcigarrillo==1],
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
write.csv(DCGUrb,file = paste0(LResu,"1DCGastoUrbDiarioEnig17.csv"), row.names = FALSE, na="")

## Gastos diarios personales Rural
# Gasto y cantidades de cigarrillo y tabaco
DCGRur <- DTGPRur[,list(GastoTotal  = weighted.sum(x=NC2R_CE_P7,
                                                    w=FEX_C),
                         ECigarrillo = weighted.sum(x=NC2R_CE_P7[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                         QCigarrillo = weighted.sum(x=NC2R_CE_P4S1[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                         GastoTotalMes  = weighted.sum(x=ValorPgdoMes,
                                                       w=FEX_C),
                         ECigarrilloMes = weighted.sum(x=ValorPgdoMes[dcigarrillo==1],
                                                       w=FEX_C[dcigarrillo==1]),
                         QCigarrilloMes = weighted.sum(x=CantAdquMes[dcigarrillo==1],
                                                       w=FEX_C[dcigarrillo==1])),
                         by=list()]

# Gasto como porcentaje del total
DCGRur[,Wcigarrillo:=ECigarrillo/GastoTotal*100]
# Precio implicito
DCGRur[,Pcigarrillo:=ECigarrillo/QCigarrillo]
# Gasto como porcentaje del total (Mes)
DCGRur[,WcigarrilloMes:=ECigarrilloMes/GastoTotalMes*100]
# Precio implicito (Mes)
DCGRur[,PcigarrilloMes:=ECigarrilloMes/QCigarrilloMes]
# Guardar
write.csv(DCGRur,file = paste0(LResu,"1DCGastoRurDiarioEnig17.csv"), row.names = FALSE, na="")


## Region----

DC <- DPGDPUrb[,list(GastoTotal  = weighted.sum(x=NC4_CC_P5,
                                                         w=FEX_C),
                              ECigarrillo = weighted.sum(x=NC4_CC_P5[dcigarrillo==1],
                                                         w=FEX_C[dcigarrillo==1]),
                              QCigarrillo = weighted.sum(x=NC4_CC_P2[dcigarrillo==1],
                                                         w=FEX_C[dcigarrillo==1]),
                              GastoTotalMes  = weighted.sum(x=ValorPgdoMes,
                                                            w=FEX_C),
                              ECigarrilloMes = weighted.sum(x=ValorPgdoMes[dcigarrillo==1],
                                                            w=FEX_C[dcigarrillo==1]),
                              QCigarrilloMes = weighted.sum(x=CantAdquMes[dcigarrillo==1],
                                                            w=FEX_C[dcigarrillo==1])),
                              by=list(REGION)]

# Gasto como porcentaje del total
DC[,Wcigarrillo:=ECigarrillo/GastoTotal*100]
# Precio implicito
DC[,Pcigarrillo:=ECigarrillo/QCigarrillo]
# Gasto como porcentaje del total (Mes)
DC[,WcigarrilloMes:=ECigarrilloMes/GastoTotalMes*100]
# Precio implicito (Mes)
DC[,PcigarrilloMes:=ECigarrilloMes/QCigarrilloMes]
# Guardar
write.csv(DC,file = paste0(LResu,"1DCGUrbRegionEnig17.csv"), row.names = FALSE, na="")


## Canal compra----

DC <- DTGDPUrb[,list(GastoTotal  = weighted.sum(x=NC4_CC_P5,
                                                    w=FEX_C),
                         ECigarrillo = weighted.sum(x=NC4_CC_P5[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                         QCigarrillo = weighted.sum(x=NC4_CC_P2[dcigarrillo==1],
                                                    w=FEX_C[dcigarrillo==1]),
                         GastoTotalMes  = weighted.sum(x=ValorPgdoMes,
                                                       w=FEX_C),
                         ECigarrilloMes = weighted.sum(x=ValorPgdoMes[dcigarrillo==1],
                                                       w=FEX_C[dcigarrillo==1]),
                         QCigarrilloMes = weighted.sum(x=CantAdquMes[dcigarrillo==1],
                                                       w=FEX_C[dcigarrillo==1])),
                         by=list(NC4_CC_P3)]

# Gasto como porcentaje del total
DC[,Wcigarrillo:=ECigarrillo/GastoTotal*100]
# Precio implicito
DC[,Pcigarrillo:=ECigarrillo/QCigarrillo]
# Gasto como porcentaje del total (Mes)
DC[,WcigarrilloMes:=ECigarrilloMes/GastoTotalMes*100]
# Precio implicito (Mes)
DC[,PcigarrilloMes:=ECigarrilloMes/QCigarrilloMes]
# Guardar
write.csv(DC,file = paste0(LResu,"1DCGUrbCanalEnig17.csv"), row.names = FALSE, na="")


#////////////////////////////////////////////////////////////////////
#                         3.2 Visualizacion-----
#////////////////////////////////////////////////////////////////////


## Region---

DG <- fread(paste0(LResu,"1DCGUrbRegionEnig17.csv"))

g <- ggplot(DG,aes(x=reorder(REGION,PcigarrilloMes), y=PcigarrilloMes))
g <- g + labs(x="", y="Pesos")
g <- g + geom_bar(colour="blue", stat="identity",fill="steelblue") 
g <- g + coord_flip()
g <- g + guides(fill=FALSE)
#g <- g + theme_minimal(base_size = 7, base_family = "Helvetica")
g <- g + theme( aspect.ratio=1)
g <- g + theme(plot.margin = unit(c(0,0,0,0), "cm"))
g <- g + geom_text(aes(label=format(as.numeric(round(PcigarrilloMes,0)),  decimal.mark=",",   big.mark=".",small.mark=".")), hjust=1.6, color="white", size=5)
g

ggsave(paste0(LResu,"1DGGUrbRegionEnig17.png"))



rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         1. Macros-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#////////////////////////////////////////////////////////////////////
#                         1.1 Paths-----
#////////////////////////////////////////////////////////////////////


Usuario <- "wgarcia"

PathGD  <- paste0("C:/Users/",Usuario,"/Google Drive/")
  
# Directorios 

LCode <- paste0(PathGD,"06Coding/1R/MultiProposito/")

LData <- paste0(PathGD,"07DataBases/Data/Multiproposito/Multiproposito17/")

LResu <- paste0(LData,"Resultados/")

setwd(LResu)


#////////////////////////////////////////////////////////////////////
#                         1.2 Librerias-----
#////////////////////////////////////////////////////////////////////



# En caso de no tenerlas descargadas, correer esta linea:

#install.packages(c("data.table"),dependencies = T)

library(data.table)
library(maptools)
library(ggplot2)
library(foreign)
library(readstata13)
library(survey)  # survey analysis
library(gmodels) # crosstable

library(raster)  # mapas
library(tmap)    # mapas


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         2. Datos-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#////////////////////////////////////////////////////////////////////
#                         2.0 Otros-----
#////////////////////////////////////////////////////////////////////



## DBF_MTP_256_1 A. IDENTIFICACION

DTIden <- as.data.table(read.spss(paste0(LData,"Identificacion ( Capitulo A).sav"),
                    to.data.frame = TRUE,use.value.labels = FALSE))

## DBF_MTP_256_2 B. VIVIENDA Y ENTORNO

DTVivi <- as.data.table(read.spss(paste0(LData,"Datos de la vivenda y su entorno  ( Capitulo B).sav"),
                    to.data.frame = TRUE,use.value.labels = FALSE))

# DTVivienda <- read.csv(paste0(LData,"Datos de la vivenda y su entorno  ( Capitulo B).csv"), 
#header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE)
# DTVivienda <- as.data.table(read.dta13(paste0(LData,"Datos de la vivenda y su entorno  ( Capitulo B).dta"),
#convert.factors = FALSE))
# DTVivienda <- as.data.table(read.dta(paste0(LData,"Datos de la vivenda y su entorno  ( Capitulo B).dta"),
#convert.factors = FALSE))


#////////////////////////////////////////////////////////////////////
#                         2.2 Hogares-----
#////////////////////////////////////////////////////////////////////

# DBF_MTP_257_1 C. CONDICIONES HABITACIONALES

# DBF_MTP_257_2 D. SERVICIOS PUBLICOS Y TIC
DTViviTic <- as.data.table(read.spss(paste0(LData,"Servicios publicos domiciliarios y de TIC (Capitulo D).sav"),
                                     to.data.frame = TRUE,use.value.labels = FALSE))

# DBF_MTP_257_3 L. CONDICIONES DE VIDA Y DESEMPENO
# DBF_MTP_257_4 M. GASTO
# DBF_MTP_257_5 M. GASTO_2


#////////////////////////////////////////////////////////////////////
#                         2.3 Personas-----
#////////////////////////////////////////////////////////////////////

# DBF_MTP_258_1 E. COMPOSICIoN DEL HOGAR Y DEMOGRAFIA

DTHoga <- as.data.table(read.spss(paste0(LData,"Composicion del hogar y demografia ( Capitulo E).sav"),
                    to.data.frame = TRUE,use.value.labels = TRUE))

# DBF_MTP_258_2 F. SALUD
# DBF_MTP_258_3 G. ATENCION INTEGRAL DE LOS NINOS Y NINAS MENORES DE 5
# DBF_MTP_258_4 H. EDUCACION
# DBF_MTP_258_5 I. USO DE TECNOLOGIAS DE LA INFORMACION, TIC
# DBF_MTP_258_6 J. PARTICIPACION EN ORGANIZACIONES Y REDES SOCIALES
# DBF_MTP_258_7 K. FUERZA DE TRABAJO







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         3. Resultados-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Base para consultas

Data <- merge(DTViviTic,DTIden[,c("DIRECTORIO","SECUENCIA_P","LOCALIDAD_TEX","CLASE","CODLOCALIDAD")],
                     by=c("DIRECTORIO","SECUENCIA_P"), all.x= TRUE) 

Data <- Data[CLASE==1,]
  

#///////////////////////////////////////////////////////////////////
## Cabecera y localidades: Porcentaje de hogares con conexion a internet
#///////////////////////////////////////////////////////////////////

DCInternetU <- Data[, list(internet = sum(na.omit(FEX_C[NHCDP27==1]))), by=list(LOCALIDAD_TEX,CODLOCALIDAD)]

DCInternetU <- merge(DCInternetU,
                     Data[, list(poblacion = sum(na.omit(FEX_C))), by=list(LOCALIDAD_TEX,CODLOCALIDAD)],all.x=T)

DCInternetU[, internet_prop:=internet/poblacion]

## Renombrar para grafico espacial

names(DCInternetU)[names(DCInternetU)=="CODLOCALIDAD"] <- "CODIGO_LOC"


# Guardar
write.csv(DCInternetU,file = paste0(LResu,"DCInternetU.csv"), row.names = FALSE, na="")


# Cabecera y localidades: Distribucion porcentual de personas de 10 anos o mas por veces 
# a la semana que en los ultimos 30 dias practicaron deporte o realizaron actividad fisica por 30 minutos continuos o mas*
  
# Cabecera y localidades: Porcentaje de personas ocupadas que viven en Bogota
# y trabajan en otro municipio




# Centro poblado rural: Porcentaje de hogares con conexion a internet

# Centro poblado rural: Viviendas por cercanía a lugares o establecimientos que pueden
# causar afectacion




#////////////////////////////////////////////////////////////////////
#                         3.1 Visualizacion-----
#////////////////////////////////////////////////////////////////////

# Unir datos espaciales con Datos de poblacion y crimen
Localidades$CODIGO_LOC <- as.numeric(Localidades$CODIGO_LOC)
Localidades@data       <- merge(Localidades@data, DCInternetU, all.x= TRUE)

# Graficar Mapas
MapInternetU <- qtm(Localidades, "internet_prop",style="col_blind") # plot the basic map

tmap_save(MapInternetU,paste0(LResu,"MapInternetU.jpg"))

# Graficar barras verticales

DCInternetU[,internet_prop:=round(internet_prop*100,1)]

g <- ggplot(DCInternetU,aes(x=reorder(LOCALIDAD_TEX,internet_prop), y=internet_prop))
g <- g + labs(x="", y="Porcentaje")
g <- g + geom_bar(colour="blue", stat="identity",fill="steelblue") 
g <- g + coord_flip()
g <- g + guides(fill=FALSE)
#g <- g + theme_minimal(base_size = 7, base_family = "Helvetica")
g <- g + theme( aspect.ratio=1)
g <- g + theme(plot.margin = unit(c(0,0,0,0), "cm"))
g <- g + geom_text(aes(label=format(as.numeric(internet_prop),  decimal.mark=",",   big.mark=".",small.mark=".")), hjust=1.6, color="white", size=5)
g

ggsave(paste0(LResu,"BPlotDCInternetU.png"))


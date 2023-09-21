###################################################################################################################
###################################################################################################################
###################################################################################################################
#
# INTRODUCCIÓN
# Este código  hace parte del material suplementario del artículo "Delimitación de especies de frailejones del 
# Sumapaz: un experimento de jardín común", adaptado del código de Pineda et al. (en preparción The Nature of Espeletia
# Species). El objetivo es hacer un análisis de delimitación de especies basado en caracteres fenotípicos de frailejones de
# del Páramo Sumapaz, cordillera Oriental de los Andes (colombia) tomados del trabajo de Pineda et al. junto con otros
# especímenes colectados en muestreo posterior (plantas madres).Cprimero realizamos un análisis de los grupos fenotípicos
#de los frailejones silvestres del páramo de Sumapaz, según caracteres vegetativos y florales, basados en los datos de 
#Pineda et al. (2020) y en datos de 43 plantas adicionales que sirvieron como plantas madre para el experimento de 
#jardín común. Posteriormente determinamos si los grupos fenotípicos de las plantas madre corresponden a los grupos de
#su progenie en términos de la supervivencia y tasa de crecimiento en el jardín común durante tres años. 
#Esta correspondencia indicaría que los grupos fenotípicos de frailejones silvestres corresponden a especies que 
#difieren en supervivencia y tasas de crecimiento durante los primeros años de vida.Específicamente, este código busca 
#asignar grupos fenotípicos mediante modelos de mezclas normales (paquete mclust) a las 350 plnatas, incluyendo las
# 43 plantas madres.
#
#DATOS REQUERIDOS PARA CORRER ESTE CÓDIGO:
#Los datos de las variables fenotípicas para el artículo de Pineda et al. acá "meanphenodata_2022Apr27_160817.csv" y
#los datos del muestreo posterior , "meanphenodatapilot_2023Aug02_095547.csv"
#los datos de las coordenadas: "COORDENADAS_PLANTAS _MADRES_PILOTO.csv"
#
#CONTENIDO
# 1) Datos preliminares: Carga de librerías y lectura de datos
# 2) Examinar  la distribución de cada rasgo fenotípico, editar los datos y transformación y rotación de los datos con
#  análisis de componentes principales
# 3) selección de variables para los modelos de mezclas normales
# 4) Ajuste de los modelos de mezlas normales
# 5) Examinar grupos fenotípicos en el mejor modelo de mezclas normales.
# 6)


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 1.1) Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mexclas normales
library(ellipse)
###################################################################################################################
# 1.2) lectura de los datos fenotípicos. Estos datos son promedios de cada variable para cada especímen. 

#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

#leer las tablas de datos, examinar y resumir los datos

mean.phenodata.pineda <- read.table("meanphenodata_2022Apr27_160817.csv", header=T, sep=",")#datos usados en Pineda 
#et al.2020 para la formación de grupos fenotípicos: 21 variables y 1020 observaciones
summary(mean.phenodata.pineda)
head(mean.phenodata.pineda)
dim(mean.phenodata.pineda)# 1020 Especímenes y 21 variables

mean.phenodata.piloto<-read.table("meanphenodatapilot_2023Aug02_095547.csv", header = T, sep = ",")# datos de las
#plantas madres de los con las mismas variables analizadas en Pineda et al.para la conformación de grupos
summary(mean.phenodata.piloto)
head(mean.phenodata.piloto)
dim(mean.phenodata.piloto)# 21 variables y 43 especímenes

#Agregar coordenadas geográficas a las plantas madre del piloto

coordenadas.piloto <- read.table("COORDENADAS_PLANTAS _MADRES_PILOTO.csv",header=T, sep=",")
mean.phenodata.piloto<-merge(mean.phenodata.piloto[,c(-4,-5,-6)], coordenadas.piloto[,c(-2,-3)], by="Collector.Collection.Number")

#combinar las dos tablas para correr el modelo y agrupar los 43 especímenes del piloto al modelo

mean.phenodata<-rbind(mean.phenodata.pineda,mean.phenodata.piloto)
summary(mean.phenodata)
head(mean.phenodata)
dim(mean.phenodata) # con 1063 especímenes y 21 variables

#unidades de medida de cada variabes
measurement.units <- c(NA, NA, NA, "decimal.degrees", "decimal.degrees", "m", "cm",
                       "cm", "cm", "cm", "count", "count", "cm", "cm", "mm", "mm", "mm", "mm", "mm", NA, NA)
data.frame(colnames(mean.phenodata), measurement.units)

#guardar los la tabla combinada 
#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
save(mean.phenodata, file=paste("MeanPhenodata_", 
                                format(Sys.time(), "%Y%B%d_%H%M%S"), ".RData", sep=""))
###################################################################################################################
###################################################################################################################
# 2) Examinar  la distribución de cada rasgo fenotípico, editar los datos y transformación y rotación de los datos con
#    análisis de componentes principales.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 2.1) Examinar gráficamente la distribución de cada rasgo fenotípico en escala logarítmica y linear.
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
pdf("Figuras_sección2.1.pdf")
colnames(mean.phenodata) # nombre de las variables
for (trait.x in c(7:19)) { #histogramas para cada variable
  colnames(mean.phenodata)[trait.x] # Qué variable
  #distribución de la logintud de las filarias estériles en escala lineal.
  hist(mean.phenodata[,trait.x], breaks=100, xlab=colnames(mean.phenodata)[trait.x], main="", col="gray80")
  summary(mean.phenodata[,trait.x])
  #distribución de la logintud de las filarias estériles en escala logarítmica.
  hist(log(mean.phenodata[,trait.x]), breaks=100, xlab=paste("log(",colnames(mean.phenodata)[trait.x],"(", 
                                                             measurement.units[trait.x], "))"), main="", col="gray80")
  summary(log(mean.phenodata[,trait.x]))
  #distribución de la logintud de las filarias estériles en escala logarítmica agregándole uno; puede ser útil cuano 
  #hay ceros en los datos crudos 
  hist(log(mean.phenodata[,trait.x]+1), breaks=100, xlab=paste("log(",colnames(mean.phenodata)[trait.x],
                                                               "+1(", measurement.units[trait.x], "))"), main="", col="gray80")
  summary(log(mean.phenodata[,trait.x]+1))
}
dev.off()

#examinar gráficamente relaciones bivariables
#definir dos caracteres fenotípicos a examinar:
colnames(mean.phenodata)
trait.x <- 17
trait.y <- 19
#revisar los nombres de las variables seleccionadas
colnames(mean.phenodata)[trait.x]
summary(mean.phenodata[,trait.x])
colnames(mean.phenodata)[trait.y]
summary(mean.phenodata[,trait.y])
#graficar la relaciones bivariables
plot(mean.phenodata[,trait.x], mean.phenodata[,trait.y],
     xlab=paste(colnames(mean.phenodata)[trait.x], "(", measurement.units[trait.x], ")"), 
     ylab=paste(colnames(mean.phenodata)[trait.y], "(", measurement.units[trait.y], ")"), cex.lab=1.5, cex.axis=1.5)

###################################################################################################################
# 2.2) realizar un subconjunto de datos sólo con los caracteres fenotípicos para el análisis. La selección de los caracteres
# fue hecha a priori en bases en ideas de importancia de carátecteres para distinguir las especies y basado en la 
# monografíade Espeletiinae (Cuatrecasas, 2013). 

#seleccionar un subconjunto con los caracteres fenotípicos: de la columna 7 a 19
colnames(mean.phenodata) 
mean.phenodata.selected <- mean.phenodata[,7:19]

#examine the results
dim(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)
colnames(mean.phenodata.selected)
###################################################################################################################
# 2.3) Remover los especímenes con valores NA en algún rasgo fenotípico.

#determinar qué especimenes tiene  valores NA para ser excluídos del análisis
rows.with.na <- unique(which(is.na(mean.phenodata.selected), arr.ind = T)[,1])
rows.with.na # especímenes con valores NA
length(rows.with.na) #número de especímenes con NA:713
#correr las siguietes líneas en caso de existir NAs
mean.phenodata.selected <- mean.phenodata.selected[-rows.with.na,]
dim(mean.phenodata.selected) # 350 especímenes con datos en los 13 caracteres fenotípicos
class(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)

#guardar los la tabla seleccionada y filtrada 
#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
save(mean.phenodata.selected, file=paste("MeanPhenodataSelected_", 
                                         format(Sys.time(), "%Y%B%d_%H%M%S"), ".RData", sep=""))

 

#Tenga en cuenta que la referencia al dataframe original (mean.phenodata) se puede hacer por el nombre de fila:
rownames(mean.phenodata.selected)
as.numeric(rownames(mean.phenodata.selected))
#Usando el índice numérico puede saber el número de colector  y el colector de los especímenes analizados,por ejemplo:
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),c(1,2)]
#o las coordenadas y la elevación de los especímenes:
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),4:6]
#o filas con alguna información de los especímenes:
!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20])
mean.phenodata[as.numeric(
  rownames(mean.phenodata.selected)),20][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20])]
#o filas con información de especímenes citados en la monografía de Cuatrecasas
!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21])
mean.phenodata[as.numeric(
  rownames(mean.phenodata.selected)),21][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21])]


###################################################################################################################
# 2.4) Transfomación de los datos.

#caracteres continuos transformación log, dado que los caracteres fenotípicos frecuentemenente siguen distribución log-normal
#asegurarse agregar 1 a los caracteres que estén acotdos a la izquierda con cero.
mean.phenodata.selected.log <- log(data.frame(mean.phenodata.selected[,1:4], mean.phenodata.selected[,5]+1, 
                                              mean.phenodata.selected[,6:13]))
head(mean.phenodata.selected.log)

#editar el nombre de las variables
colnames(mean.phenodata.selected.log) <- paste("log", colnames(mean.phenodata.selected))
colnames(mean.phenodata.selected.log)[5] <- paste("log", paste(colnames(mean.phenodata.selected)[5], "+1", sep=""))
class(mean.phenodata.selected.log)
summary(mean.phenodata.selected.log)

###################################################################################################################
# 2.5) Análisis de componentes principales (PCA) a la matriz de covarianza de los caracteres continuos con 
#transformación logarítmica.

#PCA para la mariz de covarianza
mean.phenodata.selected.log.pca <- prcomp(
  mean.phenodata.selected.log, center = T, scale. = F) #PCA usando matriz de covarianza
View(mean.phenodata.selected.log.pca)

#examinar los resultados del PCA
attributes(mean.phenodata.selected.log.pca)
mean.phenodata.selected.log.pca$scale
mean.phenodata.selected.log.pca$center
summary(mean.phenodata.selected.log.pca) #varianza explicada en cada componente
summary(mean.phenodata.selected.log.pca$x) #resumen de los principales componentes
mean.phenodata.selected.log.pca$rotation # coeficientes (o "loadings") de cada rasgo en cada componente

#guardar el análisis de PCA
save(mean.phenodata.selected.log.pca, file=paste("MeanPhenodataSelectedLogPca_", 
                                                 format(Sys.time(), "%Y%B%d_%H%M%S"), ".RData", sep=""))
#cargar el análisis de PCA
load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")

#examinar las distribuciones univariadas de los principales componentes:
## seleccionar un componente principal
colnames(mean.phenodata.selected.log.pca$x) 
PCA.x <- 1
#revisar elnombre
colnames(mean.phenodata.selected.log.pca$x)[PCA.x]
#histograma PCA1
hist(mean.phenodata.selected.log.pca$x[,PCA.x], breaks=100, xlab=colnames(mean.phenodata.selected.log.pca$x)[PCA.x],
     ylab="Especímenes", main="", cex.lab=1.5, cex.axis=1.5)

## examinar relaciones bivariadas entre los principales componentes 
#seleccionar dos componentes:
colnames(mean.phenodata.selected.log.pca$x)
PCA.x <- 1
PCA.y <- 2
#revisar los nombres
dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.x]
dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.y]
#graficar las relaciones bivariadas
plot(mean.phenodata.selected.log.pca$x[,PCA.x], mean.phenodata.selected.log.pca$x[,PCA.y],
     xlab=dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.x], 
     ylab=dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.y],
     cex.lab=1.5, cex.axis=1.5)

###################################################################################################################
###################################################################################################################
# 3) Selección de variables para los modelos de mezclas normales.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 3.1) selección de variables hacia atrás usando PCA de la matriz de covarianza de los caracteres fenotípicos con
#       transformación logarítmica.

#ejecutar selección de variables con dirección hacia atrás para los diferentes valores de incialización, usando el
#argumento "hcUse"; revisar las opciones de MClust: hep("Mclust)

mclust.options(hcUse="PCS") #principal components computed using SVD on centered variables (i.e. using the covariance matrix)
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, 
                                                           search=c("greedy"), direction = c("backward"))
#examinar los resultados
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="VARS")# original variables
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, 
                                                           search=c("greedy"), direction = c("backward"))
#resultados examinados
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="STD")# standardized variables (centered and scaled)
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10,
                                                           search=c("greedy"), direction = c("backward"))
#resultados examinados
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="SPH")#sphered variables (centered, scaled and uncorrelated) computed using SVD
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, 
                                                           search=c("greedy"), direction = c("backward"))
#resultados examinados
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="PCR") # principal components computed using SVD on standardized (center and scaled) variables
# (i.e. using the correlation matrix)

mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10,
                                                           search=c("greedy"), direction = c("backward"))
#resultados examinados
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="SVD")#scaled SVD transformation (default)
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10,
                                                           search=c("greedy"), direction = c("backward"))
#resultados examinados
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

###################################################################################################################
# 3.2) selección de variables hacia adelante unsando PCA de la matriz de covarianza de los caracteres fenotípicos con
#       transformación logarítmica.

#ejecutar selección de variables con dirección hacia atrás para los diferentes valores de incialización, usando el
#argumento "hcUse"; revisar las opciones de MClust: hep("Mclust)


mclust.options(hcUse="PCS")
mean.phenodata.selected.log.pca.varsel.for <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10,
                                                          search=c("greedy"), direction = c("forward"))
#resultado:
attributes(mean.phenodata.selected.log.pca.varsel.for)
summary(mean.phenodata.selected.log.pca.varsel.for)
names(mean.phenodata.selected.log.pca.varsel.for$subset) 
mean.phenodata.selected.log.pca.varsel.for$steps.info
mean.phenodata.selected.log.pca.varsel.for$search
mean.phenodata.selected.log.pca.varsel.for$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,3,2,4,6,5,10,8,9,11,12,7

mclust.options(hcUse="VARS")
mean.phenodata.selected.log.pca.varsel.for <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10,
                                                          search=c("greedy"), direction = c("forward"))
#resultado:
attributes(mean.phenodata.selected.log.pca.varsel.for)
summary(mean.phenodata.selected.log.pca.varsel.for)
names(mean.phenodata.selected.log.pca.varsel.for$subset) 
mean.phenodata.selected.log.pca.varsel.for$steps.info
mean.phenodata.selected.log.pca.varsel.for$search
mean.phenodata.selected.log.pca.varsel.for$direction
#caracteres seleccionados en orden: 1,3,2,4,6,5,10,8,9,11,12,7

mclust.options(hcUse="STD")
mean.phenodata.selected.log.pca.varsel.for <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10,
                                                          search=c("greedy"), direction = c("forward"))
#resultados:
attributes(mean.phenodata.selected.log.pca.varsel.for)
summary(mean.phenodata.selected.log.pca.varsel.for)
names(mean.phenodata.selected.log.pca.varsel.for$subset) 
mean.phenodata.selected.log.pca.varsel.for$steps.info
mean.phenodata.selected.log.pca.varsel.for$search
mean.phenodata.selected.log.pca.varsel.for$direction
#éstos son los caracteres seleccionados en orden por el modelo: 1,3,2,4,6,5,10,8,9,11,12,7 

mclust.options(hcUse="SPH")
mean.phenodata.selected.log.pca.varsel.for <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, 
                                                          search=c("greedy"), direction = c("forward"))
#Resultados:
attributes(mean.phenodata.selected.log.pca.varsel.for)
summary(mean.phenodata.selected.log.pca.varsel.for)
names(mean.phenodata.selected.log.pca.varsel.for$subset) 
mean.phenodata.selected.log.pca.varsel.for$steps.info
mean.phenodata.selected.log.pca.varsel.for$search
mean.phenodata.selected.log.pca.varsel.for$direction
#Éstos son los caracteres seleccionados en orden por el modelo: 1,3,2,4,6,5,10,8,9,11,12,7

mclust.options(hcUse="PCR")
mean.phenodata.selected.log.pca.varsel.for <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, 
                                                          search=c("greedy"), direction = c("forward"))
#Resultados
attributes(mean.phenodata.selected.log.pca.varsel.for)
summary(mean.phenodata.selected.log.pca.varsel.for)
names(mean.phenodata.selected.log.pca.varsel.for$subset) 
mean.phenodata.selected.log.pca.varsel.for$steps.info
mean.phenodata.selected.log.pca.varsel.for$search
mean.phenodata.selected.log.pca.varsel.for$direction
#Éstos son los caracteres seleccionados en orden por el modelo: 1,3,2,4,6,5,10,8,9,11,12,7

mclust.options(hcUse="SVD")
mean.phenodata.selected.log.pca.varsel.for <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, 
                                                          search=c("greedy"), direction = c("forward"))
#Resultados
attributes(mean.phenodata.selected.log.pca.varsel.for)
summary(mean.phenodata.selected.log.pca.varsel.for)
names(mean.phenodata.selected.log.pca.varsel.for$subset) 
mean.phenodata.selected.log.pca.varsel.for$steps.info
mean.phenodata.selected.log.pca.varsel.for$search
mean.phenodata.selected.log.pca.varsel.for$direction
#Éstos son los caracteres seleccionados en orden por el modelo: 1,3,2,4,6,5,10,8,9,11,12,7

##la selección de las variables tanto hacia adelante como hacia atrás, escogieron los primeros 12 caracteres fenotípicos 


###################################################################################################################
###################################################################################################################
# 4) Ajuste de los modelos de mezlas normales
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 4.1) Seleccionar caracteres fenotípicos (PCA) para la inclusión del método de mezclas normales basado en los resultados
#     de las secciones 3.1 3.2.

data.for.GMM <- mean.phenodata.selected.log.pca$x[,1:12]

###################################################################################################################
# 4.2) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

#"PCS"
mclust.options(hcUse="PCS") 
Mcluster.phenodata <- Mclust(data.for.GMM, G=1:12)

#Resultados:
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#especímenes incluídos en el análisis
Mcluster.phenodata$classification #clasificación de los especímenes
Mcluster.phenodata$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components: 
#   
#   log-likelihood   n  df       BIC       ICL
#     295.0292      350 190     -522.9489 -526.2676
# 
# Clustering table:
#   1   2   3   4   5 
#   85 107  58  22  78 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

#graficar soportes empíricos para el mejor modelo a cada morfogrupo
BIC.Best.Model.Per.G <- apply(Mcluster.phenodata$BIC, 1, max, na.rm=T)
max.BIC <- max(BIC.Best.Model.Per.G)
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,6,4,2))
plot(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], type="n", bty="n", xlim=c(1,12), ylim=c(2000,0), yaxt="n", xaxt="n",
     xlab="Número de grupos morfológicos", ylab=expression(paste("Soporte empírico (",Delta, "BIC)", sep="")), 
     main="", cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
points(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], cex=2, pch=20, col="black", lwd=1)
#mostrar el mejor modelo
#agregar eje
axis(1, at=c(1,seq(2,12,1)), labels=T, tcl=-0.5, cex.axis=1.2)
axis(2, at=seq(2000,0,-100), tcl=-0.7, cex.axis=1.2)
abline(v=Mcluster.phenodata$G, lty=3) #para determinar el modelo con el mejor soporte


#guardar el mejor modelo en el directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Directorio de Iván Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun")
save(Mcluster.phenodata, file=paste("Mcluster.phenodata_", format(Sys.time(), "%Y%B%d"), ".RData", sep=""))
load("Mcluster.phenodata_2023agosto19.RData")

#"VARS"
mclust.options(hcUse="VARS")
Mcluster.phenodata <- Mclust(data.for.GMM, G=1:12)

#Resultados:
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#especímenes incluídos en el análisis
Mcluster.phenodata$classification #clasificación de los especímenes
Mcluster.phenodata$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata)
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components: 
#   
#   log-likelihood   n  df       BIC       ICL
#   295.0292        350 190     -522.9489 -526.2676
# 
# Clustering table:
#   1   2   3   4   5 
#   85 107  58  22  78 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

#"STD"
mclust.options(hcUse="STD")
Mcluster.phenodata <- Mclust(data.for.GMM, G=1:12)

#Resultados:
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#especímenes incluídos en el análisis
Mcluster.phenodata$classification #clasificación de los especímenes
Mcluster.phenodata$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata)
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components: 
#   
#   log-likelihood   n  df       BIC       ICL
#   160.1269        350 190   -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
#   23 163  76  27  61

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

#graficar soportes empíricos para el mejor modelo a cada morfogrupo
BIC.Best.Model.Per.G <- apply(Mcluster.phenodata$BIC, 1, max, na.rm=T)
max.BIC <- max(BIC.Best.Model.Per.G)
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,6,4,2))
plot(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], type="n", bty="n", xlim=c(1,12), ylim=c(2000,0), yaxt="n", xaxt="n",
     xlab="Número de grupos morfológicos", ylab=expression(paste("Soporte empírico (",Delta, "BIC)", sep="")), 
     main="", cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
points(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], cex=2, pch=20, col="black", lwd=1)
#mostrar el mejor modelo
#agregar eje
axis(1, at=c(1,seq(2,12,1)), labels=T, tcl=-0.5, cex.axis=1.2)
axis(2, at=seq(2000,0,-100), tcl=-0.7, cex.axis=1.2)
abline(v=Mcluster.phenodata$G, lty=3) #para determinar el modelo con el mejor soporte


#"SPH"
mclust.options(hcUse="SPH")
Mcluster.phenodata <- Mclust(data.for.GMM, G=1:12)

#Resultados:
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#especímenes incluídos en el análisis
Mcluster.phenodata$classification #clasificación de los especímenes
Mcluster.phenodata$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components: 
#   
#   log-likelihood   n  df       BIC       ICL
#     160.1269      350 190     -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
#   23 163  76  27  61 


#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

#graficar soportes empíricos para el mejor modelo a cada morfogrupo
BIC.Best.Model.Per.G <- apply(Mcluster.phenodata$BIC, 1, max, na.rm=T)
max.BIC <- max(BIC.Best.Model.Per.G)
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,6,4,2))
plot(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], type="n", bty="n", xlim=c(1,12), ylim=c(2000,0), yaxt="n", xaxt="n",
     xlab="Número de grupos morfológicos", ylab=expression(paste("Soporte empírico (",Delta, "BIC)", sep="")), 
     main="", cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
points(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], cex=2, pch=20, col="black", lwd=1)
#mostrar el mejor modelo
#agregar eje
axis(1, at=c(1,seq(2,12,1)), labels=T, tcl=-0.5, cex.axis=1.2)
axis(2, at=seq(2000,0,-100), tcl=-0.7, cex.axis=1.2)
abline(v=Mcluster.phenodata$G, lty=3) #para determinar el modelo con el mejor soporte

#"PCR"
mclust.options(hcUse="PCR")
Mcluster.phenodata <- Mclust(data.for.GMM, G=1:12)

#Resultados
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#the specimens included in the analysis
Mcluster.phenodata$classification #classification of specimens
Mcluster.phenodata$uncertainty #the uncertainty of the classification
attributes(Mcluster.phenodata)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components: 
#   
#   log-likelihood   n  df       BIC       ICL
#       160.1269    350 190     -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
#   23 163  76  27  61

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

#graficar soportes empíricos para el mejor modelo a cada morfogrupo
BIC.Best.Model.Per.G <- apply(Mcluster.phenodata$BIC, 1, max, na.rm=T)
max.BIC <- max(BIC.Best.Model.Per.G)
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,6,4,2))
plot(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], type="n", bty="n", xlim=c(1,12), ylim=c(2000,0), yaxt="n", xaxt="n",
     xlab="Número de grupos morfológicos", ylab=expression(paste("Soporte empírico (",Delta, "BIC)", sep="")), 
     main="", cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
points(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], cex=2, pch=20, col="black", lwd=1)
#mostrar el mejor modelo
#agregar eje
axis(1, at=c(1,seq(2,12,1)), labels=T, tcl=-0.5, cex.axis=1.2)
axis(2, at=seq(2000,0,-100), tcl=-0.7, cex.axis=1.2)
abline(v=Mcluster.phenodata$G, lty=3) #para determinar el modelo con el mejor soporte


#"SDV"
mclust.options(hcUse="SVD")
Mcluster.phenodata <- Mclust(data.for.GMM, G=1:12)

#Resultados:
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#the specimens included in the analysis
Mcluster.phenodata$classification #classification of specimens
Mcluster.phenodata$uncertainty #the uncertainty of the classification
attributes(Mcluster.phenodata)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components: 
#   
#   log-likelihood   n  df       BIC       ICL
#   160.1269        350 190    -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
#   23 163  76  27  61

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")


#graficar soportes empíricos para el mejor modelo a cada morfogrupo
BIC.Best.Model.Per.G <- apply(Mcluster.phenodata$BIC, 1, max, na.rm=T)
max.BIC <- max(BIC.Best.Model.Per.G)
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,6,4,2))
plot(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], type="n", bty="n", xlim=c(1,12), ylim=c(2000,0), yaxt="n", xaxt="n",
     xlab="Número de grupos morfológicos", ylab=expression(paste("Soporte empírico (",Delta, "BIC)", sep="")), 
     main="", cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
points(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], cex=2, pch=20, col="black", lwd=1)
#mostrar el mejor modelo
#agregar eje
axis(1, at=c(1,seq(2,12,1)), labels=T, tcl=-0.5, cex.axis=1.2)
axis(2, at=seq(2000,0,-100), tcl=-0.7, cex.axis=1.2)
abline(v=Mcluster.phenodata$G, lty=3) #para determinar el modelo con el mejor soporte


###################################################################################################################
###################################################################################################################
# 5) Examinar grupos fenotípicos en el mejor modelo de mezclas normales.
###################################################################################################################
###################################################################################################################

#cargar el mejor modelo de mezcla normal
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun")# directorio de Diana
load("Mcluster.phenodata_2023agosto19.RData")
load("MeanPhenodataSelected_2023septiembre07_052114.RData")
load("MeanPhenodata_2023septiembre07_050654.RData")

###################################################################################################################
# 5.1)Examinar y guardar en un documento para asignación de los especímenes a los grupos fenotípicos. 

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment <- data.frame(as.numeric(rownames(mean.phenodata.selected)),
                                          mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),c(1,4,5,6)], 
                                          Mcluster.phenodata$classification, 
                                          Mcluster.phenodata$uncertainty,
                                          Mcluster.phenodata$data)
colnames(phenotypic.group.assignment) <- 
  c("Rownames.Meanphenodata",
    "Collector.Collection.Number", 
    "Longitude",                                     
    "Latitude",                                      
    "Altitude",
    "Phenotypic.Group",
    "Uncertainty",
    "PC1",                                          
    "PC2",
    "PC3",                                          
    "PC4",                                          
    "PC5",                                         
    "PC6",                                          
    "PC7",                                         
    "PC8",                                          
    "PC9",                                          
    "PC10",                                        
    "PC11",                                         
    "PC12")
head(phenotypic.group.assignment)
setwd("C:/Users/usuario/Documents/Jardin_comun")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(phenotypic.group.assignment, 
          file=paste("PhenotypicGroupAssignment_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)
load("PhenotypicGroupAssignment_2023septiembre08_120644.csv")
#Examinar los grupos fenotípicps de as plantas del piloto

phenotypic.group.assignment.piloto<-phenotypic.group.assignment[308:350,]

View(phenotypic.group.assignment.piloto)##Las plantas del piloto se encuentran en cuatro de los cinco grupos fenotípicos

###################################################################################################################
# 5.2) Examinar los parámetros de la distribución normal multivariable definiendo cada grupo fenotípico de acuerdo
# al mejor modelo de mezcla de normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# Directorio de Diana
#Guardar las gráficas en un pdf
#pdf("Figuras_sección_5.2.pdf")

#jet.colors3 <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
jet.colors3 <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#jet.gray <- colorRampPalette(c("gray90", "black"))

xy.coo <- seq(0, 1, length.out=12)

# Calcular los rangos de valor en la matriz de correlación de los grupos fenotípicos
minmaxCO <- matrix(NA, nrow=5, ncol=2)
for(i in 1:5){
  CO <- cov2cor(Mcluster.phenodata$parameters$variance$sigma[,,i])
  diag(CO) <- NA
  CO[lower.tri(CO)] <- NA
  minmaxCO[i,] <- range(CO, na.rm=T) 
}
range(minmaxCO) # -0.8049053  0.6718863 para graficar usamos rango entre -0.9 a 0.6


#Seleccionar grupo fenotípico
P <- 5

#http://www.statistics4u.com/fundstat_eng/cc_scaling.html
# escalando rango para el promedio
M.Rmin <- 1.06 
M.Rmax <- 1.28 
M.Dmin <- apply(Mcluster.phenodata$parameters$mean, MARGIN=1, FUN=min)
M.Dmax <- apply(Mcluster.phenodata$parameters$mean, MARGIN=1, FUN=max)
#M.range.scaling <- (M.Rmax - M.Rmin)/(M.Dmax - M.Dmin) + (M.Rmin*M.Dmax - M.Rmax*M.Dmin)/(M.Dmax - M.Dmin)
M.rs <- Mcluster.phenodata$parameters$mean * (M.Rmax - M.Rmin)/(M.Dmax - M.Dmin) + (M.Rmin*M.Dmax - M.Rmax*M.Dmin)/(M.Dmax - M.Dmin)

#escalando rangos para la varianza
V.Rmin <- 0.5
V.Rmax <- 4 
V.Dmin <- diag(apply(Mcluster.phenodata$parameters$variance$sigma, MARGIN=c(1,2), FUN=min))
V.Dmax <- diag(apply(Mcluster.phenodata$parameters$variance$sigma, MARGIN=c(1,2), FUN=max))
V.raw <- matrix(NA, nrow=12, ncol=5)
for(i in 1:5){
  V.raw[,i] <- diag(Mcluster.phenodata$parameters$variance$sigma[,,i])
}
V.rs <- V.raw * (V.Rmax - V.Rmin)/(V.Dmax - V.Dmin) + (V.Rmin*V.Dmax - V.Rmax*V.Dmin)/(V.Dmax - V.Dmin)

# Obtener matriz de correlación
CO <- cov2cor(Mcluster.phenodata$parameters$variance$sigma[,,P])
diag(CO) <- NA
CO[lower.tri(CO)] <- NA

#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(1,1,1,1))
image(CO, col=jet.colors3(20), xaxt="n", yaxt="n", bty="n", xlim=c(-0.15, 1.3), ylim=c(-0.15, 1.3), zlim=c(-0.9,0.6))
#par(new=T)
#image(xy.coo, xy.coo-0.095, P1.SD, col=jet.gray(20), xaxt="n", yaxt="n", bty="n", xlab="", ylab="", xlim=c(-0.6, 1.3), ylim=c(-0.6, 1.3))
#par(new=T)
#image(xy.coo, xy.coo-0.275, P1.M, col=jet.gray(20), xaxt="n", yaxt="n", bty="n", xlab="", ylab="", xlim=c(-0.6, 1.3), ylim=c(-0.6, 1.3))
#points(xy.coo, xy.coo, pch=21, cex= 0.5, col="gray70")
#points(xy.coo, xy.coo, pch=21, cex= 4, col="gray70")
#graph variances
#points(xy.coo, xy.coo, pch=19, cex= 4, col="gray90")
#points(xy.coo, xy.coo, pch=19, cex= 0.5, col="white")
#points(xy.coo, xy.coo, pch=21, cex=V.rs[,P])
points(xy.coo, xy.coo, pch=19, cex=V.rs[,P])
#points(xy.coo, xy.coo-0.1, type="p", pch=as.character(1:12))
text(xy.coo, xy.coo-0.08, labels=as.character(1:12), adj=0.5, cex=1.2)
text(-0.1, xy.coo, labels=as.character(1:12), cex=1.2)
#points(xy.coo, rep(1.1, 12), type="l", lty=1, lwd=0.5, col="gray")
#points(xy.coo, rep(1.28, 12), type="l", lty=1, lwd=0.5, col="gray")
#points(xy.coo, rep(1.19, 12), type="l", lty=3, lwd=0.5, col="gray")
#segments(x0=-0.05, y0=1.1, x1=1.05, y1=1.1, lwd=0.5, col="gray")
#segments(x0=-0.05, y0=1.28, x1=1.05, y1=1.28, lwd=0.5, col="gray")
rect(xleft=-0.05, ybottom=1.06, xright=1.05, ytop=1.28, col = "gray90", border="gray90")
segments(x0=-0.05, y0=1.17, x1=1.05, y1=1.17, lwd=0.5, col="gray60", lty=3)
points(xy.coo, M.rs[,P], pch=19, cex=0.6)
segments(x0=xy.coo, y0=1.17, x1=xy.coo, y1=M.rs[,P], lty=1)
text(xy.coo[10], xy.coo[4], paste(letters[P], ") ", "P" , P, sep=""), cex=2)

#leyenda para el promedio y la varianza
par(mar=c(1,1,1,1))
image(CO, col="transparent", xaxt="n", yaxt="n", bty="n", xlim=c(-0.15, 1.3), ylim=c(-0.15, 1.3), zlim=c(-0.9,0.6))
rect(xleft=-0.05, ybottom=1.06, xright=0.09090909+0.05, ytop=1.28, col = "gray90", border="gray90")
segments(x0=-0.05, y0=1.17, x1=0.09090909+0.05, y1=1.17, lwd=0.5, col="gray60", lty=3)
points(xy.coo[1:2], c(1.06,1.28), pch=19, cex=0.6)
segments(x0=xy.coo[1:2], y0=1.17, x1=xy.coo[1:2], y1=c(M.Rmin, M.Rmax), lty=1)
text(mean(xy.coo[1:2]), 1, "Promedio", cex=1.2)
text(xy.coo[2]+0.3, 1.11, "mímino", cex=1.2)
text(xy.coo[2]+0.3, 1.24, "máximo", cex=1.2)
#leyenda par la varianza
points(xy.coo[c(8,8)]+0.045, c(1.11,1.24), pch=19, cex=c(V.Rmin, V.Rmax))
text(xy.coo[c(8,8)]+0.045, 1, "Varianza", cex=1.2)
#text(xy.coo[c(9,9)]+0.045, 1.16, "Varianza", cex=1.5, srt=90)
rect(xleft=-0.07, ybottom=0.95, xright=xy.coo[c(9,9)]+0.11, ytop=1.3, col = "transparent", border="black")

#add legend for correlation matrices
imagelegend <- function(xl, yt, width, nbox, bheight, bgap, col, border=NULL) 
{ 
  x <- c(xl,xl,xl+width,xl+width) 
  top <- 0 
  bottom <- bheight 
  y <- c(yt-bottom,yt-top,yt-top,yt-bottom) 
  polygon(x,y,border=border,col=col[1]) 
  for (i in 2:nbox) { 
    top <- top + bheight + bgap 
    bottom <- top + bheight 
    y <- c(yt-bottom,yt-top,yt-top,yt-bottom) 
    polygon(x,y,border=border,col=col[i]) 
  } 
}

#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(1,1,1,1))
plot(c(-0.9,0.6), c(-0.9,0.6), xaxt="n", yaxt="n", bty="n", type="n")
#imagelegend(xl=0, yt=1, width=0.05, nbox=29, bheight=0.05, bgap=0, col=jet.colors3(20)[20:1], border="transparent")
imagelegend(xl=0, yt=0.6, width=0.05, nbox=30, bheight=0.05, bgap=0, col=jet.colors3(30)[30:1], border="transparent")
axis(4, line=-9, at=seq(-0.9, 0.6, 0.05), labels=F)
axis(4, line=-9, at=seq(-0.9, 0.6, 0.1), labels=F, tcl=-1, cex.axis=1.5)
axis(4, line=-8.5, at=seq(-0.9, 0.6, 0.1), labels=T, tcl=-1, cex.axis=1.5, lwd=0)
mtext(side=4, "Correlación (Pearson's r)", cex=1.5, line=-5)
#text(-0.6, -0.1, labels="I) Correlation", cex=2, srt=90)

#par(mar=c(5,4.5,4,2)+0.1) #default
par(mar=c(7,1,7,1))
#image(CO, col="transparent", xaxt="n", yaxt="n", bty="n", xlim=c(-0.15, 1.3), ylim=c(-0.15, 1.3))
#plot(rep(1:12, 2), c(M.Dmin, M.Dmax), xaxt="n", yaxt="n", bty="n", type="n")
#plot(rep(1:12, 2), c(M.Dmin, M.Dmax), xaxt="n", xlab="", ylab="Mean", bty="n", type="n", cex.axis=1.5, cex.lab=1.5, ylim=c(-1.7,0.7))
plot(rep(xy.coo, 2), c(M.Dmin, M.Dmax), xaxt="n", yaxt="n", xlab="", ylab="", bty="n", type="n",
     cex.axis=1.5, cex.lab=1.2,  xlim=c(-0.15, 1.3), ylim=c(-1.7,0.7), xaxs="i")
points(xy.coo, M.Dmax, type="o", pch=19)
points(xy.coo, M.Dmin, type="o", pch=19)
axis(2, at=round(seq(-1.7,0.7,0.1),3), labels=F, tcl=-0.5, line=-2.2)
axis(2, at=c(-1.5,-1,-0.5,0,0.5), labels=F, tcl=-0.7, line=-2.2)
axis(2, at=c(-1.5,-1,-0.5,0, 0.5), labels=T, cex.axis=1.5, line=-2.2, lwd=0, las=2)
#abline(h=0, lty=3)
axis(1, at=xy.coo, labels=F, line=0, cex.axis=1.5)
axis(1, at=xy.coo[seq(2,12,2)], labels=seq(2,12,2), line=0, cex.axis=1.3, lwd=0)
axis(1, at=xy.coo[seq(1,12,2)], labels=seq(1,12,2), line=1, cex.axis=1.3, lwd=0)
text(xy.coo[8], -1, labels="G) promedio", cex=2)
mtext(side=1, "Componentes principales", cex=1.5, line=4, at=xy.coo[6]+0.05)


#par(mar=c(5,4.5,4,2)+0.1) #default
par(mar=c(7,1,7,1))
plot(rep(xy.coo, 2), c(V.Dmin, V.Dmax), xaxt="n", yaxt="n", xlab="", ylab="", bty="n", type="n",
     cex.axis=1.5, cex.lab=1.3,  xlim=c(-0.15, 1.3), xaxs="i")
points(xy.coo, V.Dmax, type="o", pch=19)
points(xy.coo, V.Dmin, type="o", pch=19)
axis(2, at=round(seq(0,1.5,0.1),3), labels=F, tcl=-0.5, line=-2.2)
axis(2, at=c(0,0.5,1,1.5), labels=F, tcl=-0.7, line=-2.2)
axis(2, at=c(0,0.5,1,1.5), labels=T, cex.axis=1.5, lwd=0, las=2,  line=-2.2)
axis(1, at=xy.coo, labels=F, line=0, cex.axis=1.5)
axis(1, at=xy.coo[seq(2,12,2)], labels=seq(2,12,2), line=0, cex.axis=1.3, lwd=0)
axis(1, at=xy.coo[seq(1,12,2)], labels=seq(1,12,2), line=1, cex.axis=1.3, lwd=0)
text(xy.coo[8], 1, labels="H) Varianza", cex=2)
mtext(side=1, "Componentes principales", cex=1.5, line=4, at=xy.coo[6]+0.05)
#dev.off()
###################################################################################################################
# 5.3) Examinar la tabulación cruzada de las variables en los datos originales (mean.phenodata) y grupos fenotípicos 
# particulares en el mejor modelo de mezcla normal.

#seleccionar un morfogrupo en el modelo de mezla normal.
pg.nmm <- 5
# Seleccionar una columna en los datos originales(mean.phenodata)
colnames(mean.phenodata)
col.phenodata <- 11 
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==pg.nmm],col.phenodata])

###################################################################################################################
# 5.4) Graficar el soporte empírico para el mejor modelo para cada grupo fenotípico.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# Directorio de Diana
#Guardar las gráficas en un pdf
#pdf("Figuras_sección_5.4.pdf")
#graficar soportes empíricos para el mejor modelo a cada morfogrupo
BIC.Best.Model.Per.G <- apply(Mcluster.phenodata$BIC, 1, max, na.rm=T)
max.BIC <- max(BIC.Best.Model.Per.G)
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,6,4,2))
plot(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], type="n", bty="n", xlim=c(1,12), ylim=c(2000,0), yaxt="n", xaxt="n",
     xlab="Número de grupos morfológicos", ylab=expression(paste("Soporte empírico (",Delta, "BIC)", sep="")), 
     main="", cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
points(1:12, max.BIC-BIC.Best.Model.Per.G[1:12], cex=2, pch=20, col="black", lwd=1)
#mostrar el mejor modelo
#agregar eje
axis(1, at=c(1,seq(2,12,1)), labels=T, tcl=-0.5, cex.axis=1.2)
axis(2, at=seq(2000,0,-100), tcl=-0.7, cex.axis=1.2)
abline(v=Mcluster.phenodata$G, lty=3) #para determinar el modelo con el mejor soporte
#dev.off()

###################################################################################################################
# 5.5) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.
setwd("C:/Users/usuario/Documents/Jardin_comun")# Diana's directory
load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")
summary(mean.phenodata.selected.log.pca)
# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# directorio de Diana
# pdf("Figuras_sección_5.5.pdf")

# 5.5.1)Todos los datos
#PC1 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,2), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC2 (14.81% varianza)", cex.axis=1.5, cex.lab=1.5)
legend("bottomleft", paste("P", 1:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
Mcluster.phenodata$parameters$mean[c(1,2),]
# [,1]              [,2]       [,3]       [,4]       [,5]
# PC1 1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC2 0.6643681 -0.3152533  0.3752883  0.2845935 0.00324688
text(1.81, 0.66, "P1")
text(-1.17, 0.3, "P4")
text(-0.37, -0.31, "P2")
text(-0.5, 0.37, "P3")
text(1.43, 0, "P5")
#mtext(side=2, "a)", at=2, las=2, line=2, cex=1.5)

#PC1 vs PC3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC3 (11.47% varianza)", cex.axis=1.5, cex.lab=1.5)
legend("bottomleft", paste("G", 1:5), col=mclust.options("classPlotColors"),horiz = T,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,3),]
#         [,1]       [,2]       [,3]       [,4]       [,5]
# PC1  1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC3 -0.1657277 -0.2814246  0.6479339 -0.1735923 0.08351875
text(1.81, -0.17, "G1", font=2)
text(-1.17, -0.17, "G4", font=2)
text(-0.37, -0.28, "G2", font=2)
text(-0.5, 0.65, "G3", font=2)
text(1.43, 0.08, "G5", font=2)

#PC3 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex.axis=1.5, cex.lab=1.5)
legend("bottomleft", paste("P", 1:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text( -0.17, 0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.31, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0.03, "P5")
#mtext(side=2, "b)", at=2, las=2, line=2, cex=1.5)

# 5.5.2) sólo las plantas madre
#PC1 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,2), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC2 (14.81% varianza)", cex.axis=1.5, cex.lab=1.5, cex=0)

for (i in c(2,3,4,5)) {
  points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==i,c(8,9)],
         col=mclust.options("classPlotColors")[i],pch=mclust.options("classPlotSymbols")[i])
}


legend("bottomleft", paste("P", 2:5), col=mclust.options("classPlotColors")[2:5],xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols")[2:5], pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,2),]
# [,1]              [,2]       [,3]       [,4]       [,5]
# PC1 1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC2 0.6643681 -0.3152533  0.3752883  0.2845935 0.00324688
#text(1.81, 0.66, "P1")
text(-1.17, 0.3, "P4")
text(-0.37, -0.31, "P2")
text(-0.5, 0.37, "P3")
text(1.43, 0, "P5")
text(1.81, 0.66, "P1")

#PC1 vs PC3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC3 (11.47% varianza)", cex.axis=1.5, cex.lab=1.5, cex=0)
for (i in c(2,3,4,5)) {
  points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==i,c(8,10)],
         col=mclust.options("classPlotColors")[i],pch=mclust.options("classPlotSymbols")[i])
}

legend("bottomleft", paste("P", 2:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,3),]
#         [,1]       [,2]       [,3]       [,4]       [,5]
# PC1  1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC3 -0.1657277 -0.2814246  0.6479339 -0.1735923 0.08351875
text(1.8156484,-0.1657277, "P1" )
text(-1.17, -0.17, "P4")
text(-0.37, -0.28, "P2")
text(-0.5, 0.65, "P3")
text(1.43, 0.08, "P5")


#PC3 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex.axis=1.5, cex.lab=1.5, cex=0)
for (i in c(2,3,4,5)) {
  points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==i,c(10,9)],
         col=mclust.options("classPlotColors")[i],pch=mclust.options("classPlotSymbols")[i])
}

legend("bottomleft", paste("P", 2:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688

text(-0.17, 0.28, "P4")
text(-0.28, -0.31, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0.03, "P5")
text(-0.1657277,0.6643681, "P1" )
#dev.off()

###################################################################################################################
# 5.6) Graficar coeficientes para diferentes caracteres fenotípicos en el espacio de los componentes principales
#Directorio para guardar las gráficas
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# directorio de Diana
#pdf("Figuras_sección_5.6.pdf")

# Grafica de los coeficientes del PC1 y PC2
#View(mean.phenodata.selected.log.pca$rotation[,c(1,2)])
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(0,0, xlim=c(-0.76,0.76), ylim=c(-0.76,0.76), asp=1, type="n", bty="n",
     xlab = "Coeficientes PC1", ylab = "Coeficientes PC2",
     cex.axis=1.5, cex.lab=1.5)
for (i in c(5,6,7,10))
{
  arrows(0, 0, x1 = mean.phenodata.selected.log.pca$rotation[,1][i], y1 = mean.phenodata.selected.log.pca$rotation[,2][i], 
         length = 0.1, angle = 20, code = 2, col = "black", lwd=1)
}
# Graficar círculo para igual contribubción de los caracteres.
x <- seq(-(2/13)^0.5, (2/13)^0.5, 1e-4)
x <- c(x, (2/13)^0.5)
equal_contribution <- ((2/13)-x^2)^0.5
points(x, equal_contribution, type="l", lty=1, col="gray70")
points(x, -equal_contribution, type="l", lty=1, col="gray70")
#Pairs of sterile leaves per synflorescence
text(mean.phenodata.selected.log.pca$rotation[,1][5]*1.1+0.12, mean.phenodata.selected.log.pca$rotation[,2][5]*1.1, "pares de hojas ")
text(mean.phenodata.selected.log.pca$rotation[,1][5]*1.1+0.12, mean.phenodata.selected.log.pca$rotation[,2][5]*1.1-0.06, "estériles por")
text(mean.phenodata.selected.log.pca$rotation[,1][5]*1.1+0.12, mean.phenodata.selected.log.pca$rotation[,2][5]*1.1-0.12, "sinflorescencia")
#Capitula per synflorescence
text(mean.phenodata.selected.log.pca$rotation[,1][6]*1.1-0.1, mean.phenodata.selected.log.pca$rotation[,2][6]*1.1, "Capitulo por")
text(mean.phenodata.selected.log.pca$rotation[,1][6]*1.1-0.1, mean.phenodata.selected.log.pca$rotation[,2][6]*1.1-0.06, "sinflorescencia")
#Terminal cyme peduncle length
text(mean.phenodata.selected.log.pca$rotation[,1][7]*1.1+0.08, mean.phenodata.selected.log.pca$rotation[,2][7]*1.1+0.06, "longitud del")
text(mean.phenodata.selected.log.pca$rotation[,1][7]*1.1+0.08, mean.phenodata.selected.log.pca$rotation[,2][7]*1.1, "Pedúnculo")
text(mean.phenodata.selected.log.pca$rotation[,1][7]*1.1+0.08, mean.phenodata.selected.log.pca$rotation[,2][7]*1.1-0.06, "de la cima")
#Sterile phyllary width
text(mean.phenodata.selected.log.pca$rotation[,1][10]*1.1, mean.phenodata.selected.log.pca$rotation[,2][10]*1.1, "Ancho de la")
text(mean.phenodata.selected.log.pca$rotation[,1][10]*1.1, mean.phenodata.selected.log.pca$rotation[,2][10]*1.1-0.06, "filaria estéril")


# Grafica de los coeficientes del PC1 y PC3: todos los caracteres
#View(mean.phenodata.selected.log.pca$rotation[,c(1,3)])
# par(mar=c(5,5,4,2)+0.1)
# plot(0,0, xlim=c(-0.76,0.76), ylim=c(-0.76,0.76), asp=1, type="n", bty="n",
#      xlab="Coeficientes PC1", ylab="Coeficientes PC3", cex.axis=1.5, cex.lab=1.5)
# for (i in 1:13)
# {
#   arrows(0, 0, x1 = mean.phenodata.selected.log.pca$rotation[,1][i], y1 = mean.phenodata.selected.log.pca$rotation[,3][i], 
#          length = 0.1, angle = 20, code = 2, col = "black", lwd=1)
# }
# # Graficar círculo para igual contribubción de los caracteres.
# x <- seq(-(2/13)^0.5, (2/13)^0.5, 1e-4) 
# x <- c(x, (2/13)^0.5)
# equal_contribution<-((2/13)-x^2)^0.5
# points(x, equal_contribution, type="l", lty=1, col="gray70")
# points(x, -equal_contribution, type="l", lty=1, col="gray70")
# for (i in 1:13)
# {
#   text(mean.phenodata.selected.log.pca$rotation[,1][i]*1.1, mean.phenodata.selected.log.pca$rotation[,3][i]*1.1, i, col="cyan3")
# }

# Grafica de los coeficientes del PC1 y PC3

par(mar=c(5,5,4,2)+0.1)

plot(0,0, xlim=c(-2,3), ylim=c(-0.76,0.76), asp=1, type="n", bty="n",
     xlab="Coeficientes PC1", ylab="Coeficientes PC3", cex.axis=1.5, cex.lab=1.5)
for (i in c(5,6,7))
{
  arrows(0, 0, x1 = mean.phenodata.selected.log.pca$rotation[,1][i], y1 = mean.phenodata.selected.log.pca$rotation[,3][i], 
         length = 0.1, angle = 20, code = 2, col = "black", lwd=1)
}
# Graficar círculo para igual contribubción de los caracteres.
x <- seq(-(2/13)^0.5, (2/13)^0.5, 1e-4)
x <- c(x, (2/13)^0.5) 
equal_contribution<-((2/13)-x^2)^0.5
points(x, equal_contribution, type="l", lty=1, col="gray70")
points(x, -equal_contribution, type="l", lty=1, col="gray70")

#Pairs of sterile leaves per synflorescence
text(mean.phenodata.selected.log.pca$rotation[,1][5]*1.1-0.7, mean.phenodata.selected.log.pca$rotation[,3][5]*1.1+0.2, 
     "pares de hojas \n estériles por  \n sinflorescencia" )

#Capitula per synflorescence

text(mean.phenodata.selected.log.pca$rotation[,1][6]*1.1, mean.phenodata.selected.log.pca$rotation[,3][6]*1.1+0.2,
     "Capítulo por \n sinflorescencia")

#Terminal cyme peduncle length
text(mean.phenodata.selected.log.pca$rotation[,1][7]*1.1+0.5, mean.phenodata.selected.log.pca$rotation[,3][7]*1.1+0.09, 
     "longitud del \n pedúnculo \n de la cima")



# Grafica de los coeficientes del PC3 y PC2
#View(mean.phenodata.selected.log.pca$rotation[,c(3,2)])
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)

plot(0,0, xlim=c(-0.76,0.76), ylim=c(-0.76,0.76), asp=1, type="n", bty="n",
     xlab="Coeficientes PC3", ylab="Coeficientes PC2", cex.axis=1.5, cex.lab=1.5)
for (i in c(2,5,6,10))
{
  arrows(0, 0, x1 = mean.phenodata.selected.log.pca$rotation[,3][i], y1 = mean.phenodata.selected.log.pca$rotation[,2][i], 
         length = 0.1, angle = 20, code = 2, col = "black", lwd=1)
}
# Graficar círculo para igual contribubción de los caracteres.
x <- seq(-(2/13)^0.5, (2/13)^0.5, 1e-4)
x <- c(x, (2/13)^0.5) 
equal_contribution<-((2/13)-x^2)^0.5
points(x, equal_contribution, type="l", lty=1, col="gray70")
points(x, -equal_contribution, type="l", lty=1, col="gray70")
#Lamina width
text(mean.phenodata.selected.log.pca$rotation[,3][2]*1.1+0.16, mean.phenodata.selected.log.pca$rotation[,2][2]*1.1+0.02, "Ancho lámina")
#Pairs of sterile leaves per synflorescence
text(mean.phenodata.selected.log.pca$rotation[,3][5]*1.1+0.1, mean.phenodata.selected.log.pca$rotation[,2][5]*1.1, "pares de hojas")
text(mean.phenodata.selected.log.pca$rotation[,3][5]*1.1+0.1, mean.phenodata.selected.log.pca$rotation[,2][5]*1.1-0.06, "estériles por")
text(mean.phenodata.selected.log.pca$rotation[,3][5]*1.1+0.1, mean.phenodata.selected.log.pca$rotation[,2][5]*1.1-0.12, "sinflorescencia")
#Capitula per synflorescence
text(mean.phenodata.selected.log.pca$rotation[,3][6]*1.1, mean.phenodata.selected.log.pca$rotation[,2][6]*1.1, "Capítulo por")
text(mean.phenodata.selected.log.pca$rotation[,3][6]*1.1, mean.phenodata.selected.log.pca$rotation[,2][6]*1.1-0.06, "sinflorescencia")
#Sterile phyllary width
text(mean.phenodata.selected.log.pca$rotation[,3][10]*1.1, mean.phenodata.selected.log.pca$rotation[,2][10]*1.1, "Ancho de filaria")
text(mean.phenodata.selected.log.pca$rotation[,3][10]*1.1, mean.phenodata.selected.log.pca$rotation[,2][10]*1.1-0.06, "estétil")


#Character name and number legend
rownames(mean.phenodata.selected.log.pca$rotation)
trait.names <- c("Log (longitud de la lámina)", "Log (ancho de la lámina)", "Log (longitud del eje de sinflorescencia)",
                 "Log (perímetro del eje de sinflorescencia)", "Log (pares de hojas estériles por sinflorescencia + 1)",
                 "Log (capítulo por sinflorescencia)", "Log (longitud del pedúnculo de cima terminal)",
                 "Log (diámetro del capítulo)", "Log (longitud de la filaria estéril)", "Log (ancho de la filaria estéril)",
                 "Log (longitud de la corola de la flor del radio)", "Log (longitud de la corola de la flor del disco)",
                 "Log (longitud del tubo de la flor del disco)") 
#par(mar=c(5,4,4,2)+0.1) #default
par (mfrow=c(1,1), mar=c(2,2,2,4)+0.1)
plot(c(-0.1,1), c(-0.1,1), type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
legend(x=0, y=1, paste(1:13, ". ", trait.names, sep=""), col=("black"), cex=1)

#dev.off()

###################################################################################################################
# 5.7) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata$uncertainty)
#       Min.  1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000010 0.0000355 0.0073859 0.0007149 0.4844362 

#De los 350 especímenes, 7 tiene valores de incertidumbre mayores a 0.1 
#esto es el 2% de los especímenes
sum(Mcluster.phenodata$uncertainty>0.1)
sum(Mcluster.phenodata$uncertainty>0.1)/length(Mcluster.phenodata$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata$classification[Mcluster.phenodata$uncertainty>0.1]
# 88  107  128  585  807  961 1039 
# 4    4    2    4    4    1    2 

#Resumen de los valores de incertidumbre para las plantas madre
summary(Mcluster.phenodata$uncertainty[308:350])
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000007 0.0000320 0.0129535 0.0003625 0.4816502

sum(Mcluster.phenodata$uncertainty[308:350]>0.1)# de las 43 plantas madre, 1 planta madre tuvo incertidumbre >0.1
sum(Mcluster.phenodata$uncertainty[308:350]>0.1)/length(Mcluster.phenodata$uncertainty[308:350])# 2.33%

Mcluster.phenodata$classification[308:350][Mcluster.phenodata$uncertainty[308:350]>0.1]
# 1039 
#    2

#Directorio para guardar las gráficas
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
#setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# directorio de Diana
#pdf("Figuras_sección_5.7.pdf")

#Gráfica de la función de distribución acumulativa de valores de incertidumbre
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(sort(Mcluster.phenodata$uncertainty),
     1:length(Mcluster.phenodata$uncertainty)/length(Mcluster.phenodata$uncertainty), 
     xlim=c(0,0.5), bty="n", xlab="Incertidumbre", ylab="F (Incertidumbre)",
     type="l", pch=19, cex=0.5, cex.axis=1.5, cex.lab=1.5)
abline(h=c(0,1), lty=3, col="gray70")

# gráfica, en PC 1 y PC2, de los especímenes con incertidumbre > 0.1 
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,2), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
#Agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar las etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,2),]
#       [,1]       [,2]       [,3]       [,4]       [,5]
# PC1 1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC2 0.6643681 -0.3152533  0.3752883  0.2845935 0.00324688
text(1.82, 0.66, "P1")
text(-1.17, 0.28, "P4")
text(-0.37, -0.32, "P2")
text(-0.5, 0.38, "P3")
text(1.44, 0, "P5")
for(i in 1:Mcluster.phenodata$G){
  points(Mcluster.phenodata$data[Mcluster.phenodata$uncertainty>0.1 & Mcluster.phenodata$classification==i,1:2], 
         pch=mclust.options("classPlotSymbols")[i],
         col=mclust.options("classPlotColors")[i])
}
legend("bottomright", paste("P", c(1,2,4)),
       col=mclust.options("classPlotColors")[c(1,2,4)],
       pch=mclust.options("classPlotSymbols")[c(1,2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")

#gráfica, en PC 2 y PC3, de los especímenes con incertidumbre > 0.1 
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}

#Agregar las etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#           [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text(-0.17,0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.32, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0, "P5")
#text(0.5, 0.8, "P1")
#text(-0.75, 1, "P4")
#text(0.4, -1.77, "P6")
#text(-0.55, -0.9, "P2")
#text(0.49, -0.45, "P3")
#text(-0.8, 0, "P5"
#arrows(x0=-0.95, y0=0.7, x1 =-0.6, y1 =0.3, length = 0.1, angle = 20, code = 2)
for(i in 1:Mcluster.phenodata$G){
  points(Mcluster.phenodata$data[Mcluster.phenodata$uncertainty>0.1 & Mcluster.phenodata$classification==i,3:2], 
         pch=mclust.options("classPlotSymbols")[i],
         col=mclust.options("classPlotColors")[i])
}
legend("bottomleft", paste("P", c(1,2,4)),
       col=mclust.options("classPlotColors")[c(1,2,4)],
       pch=mclust.options("classPlotSymbols")[c(1,2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
#dev.off()

###################################################################################################################
###################################################################################################################
# 6) Examinar la localización de los especímens citados en la monografía de Espeletiinae (Cuatrecasas 2013)
#    en el mejor de los modelos de la mezcla normales.
###################################################################################################################
###################################################################################################################

#cargar los datos datos fenotípicos crudos
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun")# directorio de Diana
load("MeanPhenodata_2023septiembre07_050654.RData")

#cargar el mejor modelo de mezcla normal
load("Mcluster.phenodata_2023agosto19.RData")

#cargar los datos fenotípicas seleccionados sin NA
load("MeanPhenodataSelected_2023septiembre07_052114.RData")

###################################################################################################################
# 6.1)  Examinar la distribución de los tipos  entre grupos fenotípicos.

colnames(mean.phenodata)
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==1],20])
# Espeletia.summapacis 
# 1 
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==2],20])
# Espeletia tapirophila   Espeletia.grandiflora.fma.multiflora 
# 1                                    1 
# Espeletia.grandiflora.fma.reducta 
# 1 
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==3],20])
#< table of extent 0 >
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==4],20])
# < table of extent 0 >
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==5],20])
# Espeletia.killipii 
# 1

unique(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20])
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20])]
table(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20])])

# crear marco de datos para los especímenes tipo de acuerdo con la clasificación, incertidumbre y coordenadas de los
# tres primeros componentes principales
type.classification <- data.frame(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),20], 
                                  Mcluster.phenodata$classification, Mcluster.phenodata$uncertainty, Mcluster.phenodata$data[,1:3]) 
type.classification <- type.classification[!is.na(type.classification[,1]),]
colnames(type.classification)[1] <- colnames(mean.phenodata)[20]
type.classification <- type.classification[order(type.classification[,1], type.classification[,2]),]

#guardar tabla de clasificación de los especímen tipo
setwd("C:/Users/usuario/Documents/Jardin_comun")# directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(type.classification, file=paste("TypeClassification_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)

###################################################################################################################
# 6.2)  Examinar la distribución de los especímenes citados en la monografía de Espeletiinae (Cuatrecasas 2013) 
# entre los grupos fenotípicos.

table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==1],21])
# Espeletia.summapacis 
# 4 
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==2],21])
# Espeletia.argentea.fma.phaneractis 
# 1 
# Espeletia.grandiflora.ssp.grandiflora.var.attenuata 
# 2 
# Espeletia.grandiflora.ssp.grandiflora.var.grandiflora 
# 7
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==3],21])
#< table of extent 0 >
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==4],21])
# Espeletia.grandiflora.ssp.grandiflora.var.attenuata 
# 1
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==5],21])
# Espeletia.killipii   Espeletia.killipii.var.chisacana 
# 3                                3 
# Espeletia.killipii.var.killipii 
# 1 

unique(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21])
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21])]
table(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21])])

#create data frame with the following information for specimens cited in the Espeletiinae monograph:
#classification, uncertainty and coordinates for first three principal components
cited.specimen.classification <- data.frame(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),21], 
                                            Mcluster.phenodata$classification,
                                            Mcluster.phenodata$uncertainty, 
                                            Mcluster.phenodata$data[,1:3]) 
cited.specimen.classification <- cited.specimen.classification[!is.na(cited.specimen.classification[,1]),]
colnames(cited.specimen.classification)[1] <- colnames(mean.phenodata)[21]
cited.specimen.classification <- cited.specimen.classification[order(cited.specimen.classification[,1], cited.specimen.classification[,2]),]
cited.specimen.classification

#guardar tabla de clasificación de los especímen citados en la monografía de Espeletiinae
setwd("C:/Users/usuario/Documents/Jardin_comun")# directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(cited.specimen.classification, file=paste("CitedSpecimenClassification_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)

###################################################################################################################
# 6.3) Graficar grupos fenotípicos de los especímenes citados en el mejor modelo de mezclas normales.
setwd("C:/Users/usuario/Documents/Jardin_comun")# Diana's directory
load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")
summary(mean.phenodata.selected.log.pca)
# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")

# setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# directorio de Diana
# pdf("Figuras_secciones_6.3_6.9.pdf")
#PC1 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)

plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,2), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC2 (14.81% varianza)",cex.axis=1.5, cex.lab=1.5, col="gray90")

for (i in 1:5) {
  points(cited.specimen.classification[cited.specimen.classification[,2]==i,4:5],
         col=mclust.options("classPlotColors")[i], pch=mclust.options("classPlotSymbols")[i])
  
}

legend("bottomleft", paste("P", c(1,2,4,5)),
       col=mclust.options("classPlotColors")[c(1,2,4,5)],
       pch=mclust.options("classPlotSymbols")[c(1,2,4,5)], bty="o",xpd=T,ncol=2)
#agregar elipses
for (i in c(1,2,4,5)){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,2),]
# [,1]              [,2]       [,3]       [,4]       [,5]
# PC1 1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC2 0.6643681 -0.3152533  0.3752883  0.2845935 0.00324688
text(1.81, 0.66, "P1")
text(-1.17, 0.3, "P4")
text(-0.37, -0.31, "P2")
text(1.43, 0, "P5")
#agregar puntos de los especímenes citados en Cuatrecasas
text(2,1.1,expression(italic("E. summapacis")), cex=0.8)
arrows(2,1,1.821640,0.5579996, length = 0.1, angle = 20, code = 2)
arrows(2,1,1.731608,0.7476140, length = 0.1, angle = 20, code = 2)
arrows(2,1,2.002485,0.7156814, length = 0.1, angle = 20, code = 2)
arrows(2,1,1.640706,0.6069505, length = 0.1, angle = 20, code = 2)

text(-2.1,0.4,expression(italic(" E. argentea\n fma phaneractis")),cex = 0.8)
arrows(-2.1,0.3,-1.4331972,0.03570394, length = 0.1, angle = 20, code = 2)

text(-0.3,-1.5,expression(italic("E. grandiflora\n ssp. grandiflora\n var attenuata")),cex = 0.8)
arrows(-0.3,-1.2,-0.3243095,-0.46988095,length = 0.1, angle = 20, code = 2)
arrows(-0.3,-1.2,-0.1678826,-0.12223327,length = 0.1, angle = 20, code = 2)
arrows(-0.3,-1.2,-0.7966127,-0.2626585,length = 0.1, angle = 20, code = 2)

text(0,0.3,expression(italic("E. grandiflora\n ssp. grandiflora\n var grandiflora")),cex = 0.8)
arrows(0,0.2,-0.5321339,-0.31476342,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,-0.1359422,-0.30544467,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,0.4763375,-0.88729838,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,0.3329577,-0.28560410,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,0.6518312,-0.56410990,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,1.2907129,-0.61568567,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,-0.2936392,-0.15498676,length = 0.1, angle = 20, code = 2)

text(0.8,0.2, expression(italic("E. killipii")),cex = 0.8)
arrows(1,0.2,1.348491,0.06131696,length = 0.1, angle = 20, code = 2)
arrows(1,0.2,1.253955,0.17729659,length = 0.1, angle = 20, code = 2)
arrows(1,0.2,1.461211,0.11295413,length = 0.1, angle = 20, code = 2)

text(2, -0.7,expression(italic("E. killipii\n var chisacana")),cex = 0.8)
arrows(2,-0.5,1.638730,0.04508356,length = 0.1, angle = 20, code = 2)
arrows(2,-0.5,1.307810,-0.06032423,length = 0.1, angle = 20, code = 2)
arrows(2,-0.5,1.498897,-0.15023074,length = 0.1, angle = 20, code = 2)

text(1.5,0.35,expression(italic("E. killipii\n var killipii")),cex = 0.8)
arrows(1.5,0.25,1.405207,0.14853116,length = 0.1, angle = 20, code = 2)

# PC3 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)

plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)",cex.axis=1.5, cex.lab=1.5, col="gray90")

for (i in 1:5) {
  points(cited.specimen.classification[cited.specimen.classification[,2]==i,6:5],
         col=mclust.options("classPlotColors")[i], pch=mclust.options("classPlotSymbols")[i])
  
}

legend("bottomleft", paste("P", c(1,2,4,5)),
       col=mclust.options("classPlotColors")[c(1,2,4,5)],
       pch=mclust.options("classPlotSymbols")[c(1,2,4,5)], bty="o",xpd=T,ncol=2)
#agregar elipses
for (i in c(1,2,4,5)){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Agregar las etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#           [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text(-0.17,0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.32, "P2")
text(0.08, 0, "P5")
#agregar puntos de los especímenes citados en Cuatrecasas
text(-0.3,1.1,expression(italic("E. summapacis")), cex=0.8)
arrows(-0.3,1.05,-0.1132887,0.5579996, length = 0.1, angle = 20, code = 2)
arrows(-0.3,1.05,-0.3449696,0.7476140, length = 0.1, angle = 20, code = 2)
arrows(-0.3,1.05,-0.3527511,0.7156814, length = 0.1, angle = 20, code = 2)
arrows(-0.3,1.05,-0.2712019,0.6069505, length = 0.1, angle = 20, code = 2)

text(-1,0,expression(italic(" E. argentea\n fma phaneractis")),cex = 0.8)
arrows(-0.85,0,-0.53537205,0.03570394, length = 0.1, angle = 20, code = 2)

text(-1,-1,expression(italic("E. grandiflora\n ssp. grandiflora\n var attenuata")),cex = 0.8)
arrows(-0.85,-0.9,-0.42362361,-0.46988095,length = 0.1, angle = 20, code = 2)
arrows(-0.85,-0.9,-0.67169532,-0.12223327,length = 0.1, angle = 20, code = 2)
arrows(-0.85,-0.9,-0.5859421,-0.2626585,length = 0.1, angle = 20, code = 2)

text(0.45,-1.2,expression(italic("E. grandiflora\n ssp. grandiflora\n var grandiflora")),cex = 0.8)
arrows(0.3,-1,-0.09257224,-0.31476342,length = 0.1, angle = 20, code = 2)
arrows(0.3,-1,-0.15250418,-0.30544467,length = 0.1, angle = 20, code = 2)
arrows(0.3,-1,0.05903328,-0.88729838,length = 0.1, angle = 20, code = 2)
arrows(0.3,-1,-0.35571893,-0.28560410,length = 0.1, angle = 20, code = 2)
arrows(0.3,-1,-0.05485819,-0.56410990,length = 0.1, angle = 20, code = 2)
arrows(0.3,-1,-0.43700040,-0.61568567,length = 0.1, angle = 20, code = 2)
arrows(0.3,-1,-0.24524329,-0.15498676,length = 0.1, angle = 20, code = 2)

text(0.3,0.2, expression(italic("E. killipii")),cex = 0.8)
arrows(0.2,0.2,0.14342759,0.06131696,length = 0.1, angle = 20, code = 2)
arrows(0.2,0.2,-0.02465401,0.17729659,length = 0.1, angle = 20, code = 2)
arrows(0.2,0.2,0.16546748,0.11295413,length = 0.1, angle = 20, code = 2)

text(0.45, -0.7,expression(italic("E. killipii\n var chisacana")),cex = 0.8)
arrows(0.3,-0.5,0.10590605,0.04508356,length = 0.1, angle = 20, code = 2)
arrows(0.3,-0.5,0.14989944,-0.06032423,length = 0.1, angle = 20, code = 2)
arrows(0.3,-0.5,-0.02007657,-0.15023074,length = 0.1, angle = 20, code = 2)

text(0.1,0.35,expression(italic("E. killipii\n var killipii")),cex = 0.8)
arrows(0.06,0.35,0.06260453,0.14853116,length = 0.1, angle = 20, code = 2)

###################################################################################################################
# 6.4)  Examinar la distribución de los especímenes tipo citados en la monografía de Espeletiinae (Cuatrecasas, 2013)

#PC1 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)

plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,2), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC2 (14.81% varianza)",cex.axis=1.5, cex.lab=1.5, col="gray90")

for (i in 1:5) {
  points(type.classification[type.classification[,2]==i,4:5],
         col=mclust.options("classPlotColors")[i], pch=mclust.options("classPlotSymbols")[i])
  
}

legend("bottomleft", paste("P", c(1,2,5)),
       col=mclust.options("classPlotColors")[c(1,2,5)],
       pch=mclust.options("classPlotSymbols")[c(1,2,5)], bty="o",xpd=T,ncol=3)
#agregar elipses
for (i in c(1,2,5)){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,2),]
# [,1]              [,2]       [,3]       [,4]       [,5]
# PC1 1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC2 0.6643681 -0.3152533  0.3752883  0.2845935 0.00324688
text(1.81, 0.66, "P1")
text(-0.37, -0.31, "P2")
text(1.43, 0, "P5")
#agregar puntos de los especímenes tipos en Cuatrecasas
text(2,1.1,expression(paste("tipo\n",italic("E. summapacis"))), cex=0.8)
arrows(2,1,1.9385224,0.5418004, length = 0.1, angle = 20, code = 2)

text(-1.5,-1,expression(paste("tipo\n ",italic("E. tapirophila"))), cex=0.8)
arrows(-1.5,-0.8,-0.7872908,-0.3998375, length = 0.1, angle = 20, code = 2) 

text(-0.3,-1.5,expression(paste("tipo\n ",italic("E. grandiflora\n fma multiflora"))), cex=0.8)
arrows(-0.3,-1.3,-0.4571326,-0.8111396, length = 0.1, angle = 20, code = 2) 

text(-0.3,1,expression(paste("tipo\n ",italic("E. grandiflora\n fma reducta"))), cex=0.8)
arrows(-0.3,0.9,-0.4120358,-0.2217099, length = 0.1, angle = 20, code = 2)

text(2.3,0,expression(paste("tipo\n ",italic("E. killipii"))), cex=0.8)
arrows(2,0,1.6933554,0.1164668, length = 0.1, angle = 20, code = 2)

###################################################################################################################
# 6.5)  Examinar la distribución de los especímenes tipo  y los citados en la monografía de Espeletiinae (Cuatrecasas, 2013)


# coordenadas PC1 y PC3
par(mar=c(5,5,4,2)+0.1)

plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC3 (11.47% varianza)",cex.axis=1.5, cex.lab=1.5, col="gray90")

for (i in 1:5) {
  points(cited.specimen.classification[cited.specimen.classification[,2]==i,4:5],
         col=mclust.options("classPlotColors")[i], pch=mclust.options("classPlotSymbols")[i])
  
  points(type.classification[type.classification[,2]==i,4:5],
         col=mclust.options("classPlotColors")[i], pch=mclust.options("classPlotSymbols")[i])
  
}

legend("bottomleft", paste("P", c(1,2,4,5)),
       col=mclust.options("classPlotColors")[c(1,2,4,5)],
       pch=mclust.options("classPlotSymbols")[c(1,2,4,5)], bty="o",xpd=T,ncol=2)
#agregar elipses
for (i in c(1,2,4,5)){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.phenodata$parameters$mean[c(1,2),]
# [,1]              [,2]       [,3]       [,4]       [,5]
# PC1 1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC2 0.6643681 -0.3152533  0.3752883  0.2845935 0.00324688
text(1.81, 0.66, "P1")
text(-1.17, 0.3, "P4")
text(-0.37, -0.31, "P2")
text(1.43, 0, "P5")
#agregar puntos de los especímenes citados en Cuatrecasas
text(2,1.1,expression(italic("E. summapacis")), cex=0.8)
arrows(2,1,1.821640,0.5579996, length = 0.1, angle = 20, code = 2)
arrows(2,1,1.731608,0.7476140, length = 0.1, angle = 20, code = 2)
arrows(2,1,2.002485,0.7156814, length = 0.1, angle = 20, code = 2)
arrows(2,1,1.640706,0.6069505, length = 0.1, angle = 20, code = 2)

text(-2.1,0.4,expression(italic(" E. argentea\n fma phaneractis")),cex = 0.8)
arrows(-2.1,0.3,-1.4331972,0.03570394, length = 0.1, angle = 20, code = 2)

text(-0.3,-1.5,expression(italic("E. grandiflora\n ssp. grandiflora\n var attenuata")),cex = 0.8)
arrows(-0.3,-1.2,-0.3243095,-0.46988095,length = 0.1, angle = 20, code = 2)
arrows(-0.3,-1.2,-0.1678826,-0.12223327,length = 0.1, angle = 20, code = 2)
arrows(-0.3,-1.2,-0.7966127,-0.2626585,length = 0.1, angle = 20, code = 2)

text(0,0.3,expression(italic("E. grandiflora\n ssp. grandiflora\n var grandiflora")),cex = 0.8)
arrows(0,0.2,-0.5321339,-0.31476342,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,-0.1359422,-0.30544467,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,0.4763375,-0.88729838,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,0.3329577,-0.28560410,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,0.6518312,-0.56410990,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,1.2907129,-0.61568567,length = 0.1, angle = 20, code = 2)
arrows(0,0.2,-0.2936392,-0.15498676,length = 0.1, angle = 20, code = 2)

text(0.8,0.2, expression(italic("E. killipii")),cex = 0.8)
arrows(1,0.2,1.348491,0.06131696,length = 0.1, angle = 20, code = 2)
arrows(1,0.2,1.253955,0.17729659,length = 0.1, angle = 20, code = 2)
arrows(1,0.2,1.461211,0.11295413,length = 0.1, angle = 20, code = 2)

text(2, -0.7,expression(italic("E. killipii\n var chisacana")),cex = 0.8)
arrows(2,-0.5,1.638730,0.04508356,length = 0.1, angle = 20, code = 2)
arrows(2,-0.5,1.307810,-0.06032423,length = 0.1, angle = 20, code = 2)
arrows(2,-0.5,1.498897,-0.15023074,length = 0.1, angle = 20, code = 2)

text(1.5,0.35,expression(italic("E. killipii\n var killipii")),cex = 0.8)
arrows(1.5,0.25,1.405207,0.14853116,length = 0.1, angle = 20, code = 2)

#agregar puntos de los especímenes tipos en Cuatrecasas
text(2.5,0.5,expression(paste("tipo ",italic("E. summapacis"))), cex=0.8)
arrows(2.3,0.5,1.9385224,0.5418004, length = 0.1, angle = 20, code = 2)

text(-1.5,-1.2,expression(paste("tipo ",italic("E. tapirophila"))), cex=0.8)
arrows(-1.5,-0.8,-0.7872908,-0.3998375, length = 0.1, angle = 20, code = 2) 

text(-1,-1.5,expression(paste("tipo ",italic("E. grandiflora\n fma multiflora"))), cex=0.8)
arrows(-1,-1.3,-0.4571326,-0.8111396, length = 0.1, angle = 20, code = 2) 

text(-0.3,1,expression(paste("tipo ",italic("E. grandiflora\n fma reducta"))), cex=0.8)
arrows(-0.3,0.9,-0.4120358,-0.2217099, length = 0.1, angle = 20, code = 2)

text(2.3,0,expression(paste("tipo ",italic("E. killipii"))), cex=0.8)
arrows(2.2,0,1.6933554,0.1164668, length = 0.1, angle = 20, code = 2)

###################################################################################################################
# 6.6)  Examinar la distribución de Espeletia argentea citado, Espeletia tapirophila  tipo y miradorensis
#plot coordenadas PC1 Vs PC3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="", ylab="PC3 (11.47% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)

  #agregar las elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}

#Agregar especíenes de E. argentea
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.argentea.fma.phaneractis",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.argentea.fma.phaneractis",6],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])

text(-2,-1, "E. argentea",font=3, cex=1.1)
arrows(-2,-0.8,cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.argentea.fma.phaneractis",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.argentea.fma.phaneractis",6], length = 0.1, angle = 20, code = 2)

#Agregar especímen tipo E. tapirophila
points(type.classification[type.classification[,1]=="Espeletia tapirophila",4],
       type.classification[type.classification[,1]=="Espeletia tapirophila",6],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])

text(-1,-1.8,expression(paste("tipo ",italic("E. tapirophila"))), cex=1.1)
arrows(-1,-1.6,type.classification[type.classification[,1]=="Espeletia tapirophila",4],
       type.classification[type.classification[,1]=="Espeletia tapirophila",6], length = 0.1, angle = 20, code = 2) 

# Agregar especímenes similares a E. miradorensis (correr sección 7.2)
points(Mcluster.phenodata$data[o.miradorensis[1:3],1],
       Mcluster.phenodata$data[o.miradorensis[1:3],3],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])

text(1.3,0.8, expression(paste("Especímenes\n más similares a ", italic("E. miradorensis"))),cex=1.1)
for (i in 1:3) {
  arrows(1,0.7,Mcluster.phenodata$data[o.miradorensis[i],1],
         Mcluster.phenodata$data[o.miradorensis[i],3],length = 0.1, angle = 20, code = 2)
}
 
  #Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(1,3),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC1  1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC3 -0.1657277 -0.2814246  0.6479339 -0.1735923 0.08351875
for (i in 1:Mcluster.phenodata$G) {
  text( Mcluster.phenodata$parameters$mean[c(1),i],  Mcluster.phenodata$parameters$mean[c(3),i], paste("G",i), font = 2)
}

#leyenda
legend("bottomright", "G2",col=mclust.options("classPlotColors")[2],pch=mclust.options("classPlotSymbols")[2] )

#agregar título
title(expression(paste("a) ", italic("E. argentea, "), italic("E. tapirophila"),  " y ", italic("E. miradorensis"))), adj=0)

# # Para las plantas madres
# #plot in PC3 and PC2
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
#      xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
# # Agregar los especímenes asignados al P2.
# points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==2,10],
#        phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==2,9],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col="gray90")
# #agregar las elipses
# for (i in 1:Mcluster.phenodata$G){
#   points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
#                  centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
#          type="l", col="black")
# }
# #Agregar etiquetas de las elipses
# # Mcluster.phenodata$parameters$mean[c(3,2),]
# #         [,1]       [,2]      [,3]       [,4]       [,5]
# # PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# # PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
# text( -0.17, 0.66, "P1")
# text(-0.17, 0.28, "P4")
# text(-0.28, -0.31, "P2")
# text(0.65, 0.38, "P3")
# text(0.08, 0.03, "P5")
# #Agregar especíenes de E. argentea
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.argentea.fma.phaneractis",6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.argentea.fma.phaneractis",5],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
# 
# text(-1.5,0, expression(paste(italic("E. argentea"),)), cex=1.1)
# arrows(-1.3,0,-0.53537205,0.03570394, length = 0.1, angle = 20, code = 2)
# 
# #Agregar especímen tipo E. tapirophila
# points(type.classification[type.classification[,1]=="Espeletia tapirophila",6],
#        type.classification[type.classification[,1]=="Espeletia tapirophila",5],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
# 
# text(-1.5,-0.8,expression(paste("tipo ",italic("E. tapirophila"))), cex=1.1)
# arrows(-1.3,-0.8,-0.56590729,-0.3998375, length = 0.1, angle = 20, code = 2) 
# 
# #leyenda
# 
# text(-1.81, -2, "P2", cex=1.3)
# points(-1.915, -2, col=mclust.options("classPlotColors")[2],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5)
# points(-2.02, -2, col="gray90",pch=mclust.options("classPlotSymbols")[2], cex=1.5)
# rect(xleft=-2.3, ybottom=-1.8, xright=-1.74, ytop=-2.2)
# #agregar título
# title(expression(paste("B) Especímenes citados como ", italic("E. argentea"), " y ", italic("E. tapirophila"),"de las plantas madre")), adj=0)
###################################################################################################################
# 6.7.)  especímenes citados y tipos Espeletia grandiflora

# 6.7.1) Para todas las plantas
#plot en PC1 y PC3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="", ylab="", cex=0, cex.lab=1.5, cex.axis=1.5)

#agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}

#add Espeletia.grandiflora.fma.reducta type specimen
points(type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.reducta",4],
       type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.reducta",6],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
#add Espeletia.grandiflora.fma.multiflora type specimen
points(type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.multiflora",4],
       type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.multiflora",6],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
#add specimens determined as Espeletia.grandiflora.ssp.grandiflora.var.grandiflora
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.grandiflora",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.grandiflora",6],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
#add specimens determined as Espeletia.grandiflora.ssp.grandiflora.var.attenuata
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata" & cited.specimen.classification[,2]==2,4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata"& cited.specimen.classification[,2]==2,6],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata" & cited.specimen.classification[,2]==4,4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata"& cited.specimen.classification[,2]==4,6],
       pch=mclust.options("classPlotSymbols")[4], cex=1.5, col=mclust.options("classPlotColors")[4])

#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(1,3),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC1  1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC3 -0.1657277 -0.2814246  0.6479339 -0.1735923 0.08351875
for (i in 1:Mcluster.phenodata$G) {
  text( Mcluster.phenodata$parameters$mean[c(1),i],  Mcluster.phenodata$parameters$mean[c(3),i], paste("G",i), font = 2)
}

#leyenda
legend("bottomright", paste("G",c(2,4)),col=mclust.options("classPlotColors")[c(2,4)], pch = mclust.options("classPlotSymbols")[c(2,4)], horiz = T)

# título
title(expression(paste("b)  ", italic("E. grandiflora"))), adj=0)

# # 6.7.2) Para las plantas madre
# #plot en PC3 y PC2
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
#      xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
# #Agregar los puntos de P2 y P4
# points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==2,10],
#        phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==2,9],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col="gray90")
# 
# points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==4,10],
#        phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==4,9],
#        pch=mclust.options("classPlotSymbols")[4], cex=1.5, col="gray90")
# 
# #agregar elipse
# for (i in 1:Mcluster.phenodata$G){
#   points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
#                  centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
#          type="l", col="black")
# }
# #Agregar etiquetas de las elipses
# # Mcluster.phenodata$parameters$mean[c(3,2),]
# #         [,1]       [,2]      [,3]       [,4]       [,5]
# # PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# # PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
# text( -0.17, 0.66, "P1")
# text(-0.17, 0.28, "P4")
# text(-0.28, -0.31, "P2")
# text(0.65, 0.38, "P3")
# text(0.08, 0.03, "P5")
# #add Espeletia.grandiflora.fma.reducta type specimen
# points(type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.reducta",6],
#        type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.reducta",5],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
# #add Espeletia.grandiflora.fma.multiflora type specimen
# points(type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.multiflora",6],
#        type.classification[type.classification[,1]=="Espeletia.grandiflora.fma.multiflora",5],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
# #add specimens determined as Espeletia.grandiflora.ssp.grandiflora.var.grandiflora
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.grandiflora",6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.grandiflora",5],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
# #add specimens determined as Espeletia.grandiflora.ssp.grandiflora.var.attenuata
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata" & cited.specimen.classification[,2]==2,6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata"& cited.specimen.classification[,2]==2,5],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2])
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata" & cited.specimen.classification[,2]==4,6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.grandiflora.ssp.grandiflora.var.attenuata"& cited.specimen.classification[,2]==4,5],
#        pch=mclust.options("classPlotSymbols")[4], cex=1.5, col=mclust.options("classPlotColors")[4])
# 
# #leyenda
# text(-1.81, -1.8, "P2", cex=1.3)
# text(-1.81, -2, "P4", cex=1.3)
# points(-1.915, -1.8, col=mclust.options("classPlotColors")[2],
#        pch=mclust.options("classPlotSymbols")[2], cex=1.5)
# points(-2.02, -1.8, col="gray90",pch=mclust.options("classPlotSymbols")[2], cex=1.5)
# 
# points(-1.915, -2, col=mclust.options("classPlotColors")[4],
#        pch=mclust.options("classPlotSymbols")[4], cex=1.5)
# points(-2.02, -2, col="gray90",
#        pch=mclust.options("classPlotSymbols")[4], cex=1.5)
# rect(xleft=-2.3, ybottom=-1.6, xright=-1.74, ytop=-2.2)
# 
# # título
# title(expression(paste("D) Especímenes citados como ", italic("E. grandiflora"), "en plantas madres")), adj=0)

###################################################################################################################
# 6.8) Especímenes tipos y citados de Espeletia killipii.

# 6.8.1) Para todas las plantas
#para coordenadas PC1 y PC3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="PC3 (11.47% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)

#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}

# agregar especímen tipo de Espeletia.killipii
points(type.classification[type.classification[,1]=="Espeletia.killipii",4],
       type.classification[type.classification[,1]=="Espeletia.killipii",6],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
#agregar los especímenes Espeletia.killipii
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii",6],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
# agregar especímenes Espeletia.killipii.var.chisacana
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.chisacana",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.chisacana",6],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
#agregar de los especímenes de Espeletia.killipii.var.killipii
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.killipii",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.killipii",6],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])

#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(1,3),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC1  1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC3 -0.1657277 -0.2814246  0.6479339 -0.1735923 0.08351875
for (i in 1:Mcluster.phenodata$G) {
  text( Mcluster.phenodata$parameters$mean[c(1),i],  Mcluster.phenodata$parameters$mean[c(3),i], paste("G",i), font = 2)
}

text(1.7, 1, "Tipo", cex=1.1)
arrows(1.7, 0.8, type.classification[type.classification[,1]=="Espeletia.killipii",4],
       type.classification[type.classification[,1]=="Espeletia.killipii",6], length = 0.1, angle = 20, code = 2, col="black")

#leyenda

legend("bottomright", "G5",col=mclust.options("classPlotColors")[5],
       pch=mclust.options("classPlotSymbols")[5])

#add title
title(expression(paste("c) ", italic("E. killipii"))), adj=0)

# # 6.8.2) Para las plantas
# #plot en PC3 y PC2
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
#      xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
# # Agregar especímenes de las plantas madre asignados al P5
# points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==5,10],
#        phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==5,9],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5, col="gray90")
# #add ellipses
# for (i in 1:Mcluster.phenodata$G){
#   points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
#                  centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
#          type="l", col="black")
# }
# 
# # agregar especímen tipo de Espeletia.killipii
# points(type.classification[type.classification[,1]=="Espeletia.killipii",6],
#        type.classification[type.classification[,1]=="Espeletia.killipii",5],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
# #agregar los especímenes Espeletia.killipii
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii",6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii",5],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
# # agregar especímenes Espeletia.killipii.var.chisacana
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.chisacana",6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.chisacana",5],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
# #agregar de los especímenes de Espeletia.killipii.var.killipii
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.killipii",6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.killipii.var.killipii",5],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5])
# #Agregar etiquetas de las elipses
# # Mcluster.phenodata$parameters$mean[c(3,2),]
# #         [,1]       [,2]      [,3]       [,4]       [,5]
# # PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# # PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
# text( -0.17, 0.66, "P1")
# text(-0.17, 0.28, "P4")
# text(-0.28, -0.31, "P2")
# text(0.65, 0.38, "P3")
# text(0.08, 0.03, "P5")
# 
# text(0, 1.4, expression(paste("tipo ", italic("E. killipii"))), cex=1.1)
# arrows(x0=0.02, y0=1.27, x1 =0.02528127, y1 =0.1164668, length = 0.1, angle = 20, code = 2, col="black")
# 
# #leyenda
# 
# text(-1.81, -2, "P5", cex=1.3)
# points(-1.915, -2, col=mclust.options("classPlotColors")[5],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5)
# points(-2.02, -2, col="gray90",pch=mclust.options("classPlotSymbols")[5], cex=1.5)
# rect(xleft=-2.3, ybottom=-1.8, xright=-1.74, ytop=-2.2)
# #add title
# title(expression(paste("F) Especímenes citados como ", italic("E. killipii"), "en las plantas madre")), adj=0)
###################################################################################################################
# 6.9) Espeletia summapacis tipo y citados y especímenes similares a E. cabrerensis

# 6.9.1) Para todas las plantas

#coordenadas PC1 y PC3

#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="PC1 (44.87% varianza)", ylab="", cex=0, cex.lab=1.5, cex.axis=1.5)

#agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}


#agregar especímenes tipo de Espeletia.summapacis
points(type.classification[type.classification[,1]=="Espeletia.summapacis",4],
       type.classification[type.classification[,1]=="Espeletia.summapacis",6],
       pch=mclust.options("classPlotSymbols")[1], cex=1.5, col=mclust.options("classPlotColors")[1])
text(1.7, -1, expression(paste("Tipo ",italic("E. summapacis"))), cex=1.1)
arrows(1.7, -0.8, type.classification[type.classification[,1]=="Espeletia.summapacis",4],
       type.classification[type.classification[,1]=="Espeletia.summapacis",6], length = 0.1, angle = 20, code = 2, col="black")
#agregar especímenes de Espeletia.summapacis
points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.summapacis",4],
       cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.summapacis",6],
       pch=mclust.options("classPlotSymbols")[1], cex=1.5, col=mclust.options("classPlotColors")[1])

 
#agregar especímenes de Espeletia cabrerensis      
points(Mcluster.phenodata$data[o.cabrerensis[1:3],1],
       Mcluster.phenodata$data[o.cabrerensis[1:3],3],
       pch=mclust.options("classPlotSymbols")[3], cex=1.5, col=mclust.options("classPlotColors")[3])
text(1.3,0.8, expression(paste("Especímenes\n más similares al tipo ", italic("E. cabrerensis"))),cex=1.1)
arrows(-0.4,1,-0.8,1 ,length = 0.08, angle = 25, code =2, col="black")

#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(1,3),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC1  1.8156484 -0.3675515 -0.4995887 -1.1710470 1.43878675
# PC3 -0.1657277 -0.2814246  0.6479339 -0.1735923 0.08351875
for (i in 1:Mcluster.phenodata$G) {
  text( Mcluster.phenodata$parameters$mean[c(1),i],  Mcluster.phenodata$parameters$mean[c(3),i], paste("G",i), font = 2)
}

#leyenda
legend("bottomright", paste("G", c(1,3)),col=mclust.options("classPlotColors")[c(1,3)],pch=mclust.options("classPlotSymbols")[c(1,3)], horiz = T)

#título
title(expression(paste("d) ", italic("E. summapacis"), " y ",italic("E. cabrerensis") )), adj=0)

# # 6.9.2) Para las plantas madre (no hay especímenes)
# #plot en PC3 y PC2
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
#      xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
# #agregar especímenes del P1
# points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==1,10],
#        phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==1,9],
#        pch=mclust.options("classPlotSymbols")[5], cex=1.5, col="gray90")
# # agregar elipses
# for (i in 1:Mcluster.phenodata$G){
#   points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
#                  centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
#          type="l", col="black")
# }
# #Agregar etiquetas de las elipses
# # Mcluster.phenodata$parameters$mean[c(3,2),]
# #         [,1]       [,2]      [,3]       [,4]       [,5]
# # PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# # PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
# text( -0.17, 0.66, "P1")
# text(-0.17, 0.28, "P4")
# text(-0.28, -0.31, "P2")
# text(0.65, 0.38, "P3")
# text(0.08, 0.03, "P5")
# #agregar especímenes tipo de Espeletia.summapacis
# points(type.classification[type.classification[,1]=="Espeletia.summapacis",6],
#        type.classification[type.classification[,1]=="Espeletia.summapacis",5],
#        pch=mclust.options("classPlotSymbols")[1], cex=1.5, col=mclust.options("classPlotColors")[1])
# #agregar especímenes de Espeletia.summapacis
# points(cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.summapacis",6],
#        cited.specimen.classification[cited.specimen.classification[,1]=="Espeletia.summapacis",5],
#        pch=mclust.options("classPlotSymbols")[1], cex=1.5, col=mclust.options("classPlotColors")[1])
# text(-0.2491367, 1.4, expression(paste("tipo",italic("E. summapacis"))), cex=1.1)
# arrows(x0=-0.2491367, y0=1.27, x1 =-0.12240609, y1 =0.5418004, length = 0.1, angle = 20, code = 2, col="black")
# 
# #leyenda
# 
# text(-1.81, -2, "P1", cex=1.3)
# points(-1.915, -2, col=mclust.options("classPlotColors")[1],
#        pch=mclust.options("classPlotSymbols")[1], cex=1.5)
# points(-2.02, -2, col="gray90",pch=mclust.options("classPlotSymbols")[1], cex=1.5)
# rect(xleft=-2.3, ybottom=-1.8, xright=-1.74, ytop=-2.2)
# #add title
# title(expression(paste("E) Especímenes citados como ", italic("E. summapacis"))), adj=0)
#dev.off()


###################################################################################################################
###################################################################################################################
# 7) Examinar la localización  delos especimes tipospeletia cabrerensis y Espeletia miradorensis en el mejor modelo
# de mezclas normales.
###################################################################################################################
###################################################################################################################
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
pdf("Figuras_sección_7.pdf")

###################################################################################################################
# 7.1) Espeletia cabrerensis
#7.1.1) Todas las planta
mean.phenodata.cabrerensis.log <- as.numeric(log(mean.phenodata[which(mean.phenodata[,20]=="Espeletia.cabrerensis"),7:17] + c(rep(0,4), 1, rep(0,6))))
diff.cabrerensis <- apply(as.matrix(mean.phenodata.selected.log[,-c(12,13)]), MARGIN=1, FUN=function(x) x-mean.phenodata.cabrerensis.log)
diff.cabrerensis[,1:10]
(diff.cabrerensis)[,1:10]

dist.Euclidian.cabrerensis <- sqrt(colSums(diff.cabrerensis^2))
sort(dist.Euclidian.cabrerensis)
o.cabrerensis <- order(dist.Euclidian.cabrerensis)
hist(dist.Euclidian.cabrerensis, breaks=25)
mean.phenodata[c("983","984","1009"),]
mean.phenodata[which(mean.phenodata[,20]=="Espeletia.cabrerensis"),]
mean.phenodata[c("983", "984", "1009", "348", "349", "671", "286", "791", "669", "806", "7", "982", "719", "340", "346"),]
mean.phenodata[which(mean.phenodata[,20]=="Espeletia.cabrerensis"),]
Mcluster.phenodata$classification[c("983", "984", "1009", "348", "349", "671", "286", "791", "669", "806", "7", "982", "719", "340", "346")] 
Mcluster.phenodata$classification[o.cabrerensis[1:15]] 

#plot en PC3 y PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
# Agregar especímenes asignados a P3
points(Mcluster.phenodata$data[Mcluster.phenodata$classification==3,3],
       Mcluster.phenodata$data[Mcluster.phenodata$classification==3,2],
       pch=mclust.options("classPlotSymbols")[3], cex=1.5, col="gray90")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text( -0.17, 0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.31, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0.03, "P5")
# Agregar especímenes similares a especímes tipo E. cabrerensis
points(Mcluster.phenodata$data[c("983","1060","984"),3],
       Mcluster.phenodata$data[c("983","1060","984"),2],
       pch=mclust.options("classPlotSymbols")[3], cex=1.5, col=mclust.options("classPlotColors")[3])
#points(Mcluster.phenodata$data[c("983", "984", "1009", "348", "349", "671", "286", "791", "669", "806", "7", "982", "719", "340", "346"),3],
#	Mcluster.phenodata$data[c("983", "984", "1009", "348", "349", "671", "286", "791", "669", "806", "7", "982", "719", "340", "346"),2],
#	pch=mclust.options("classPlotSymbols")[1], cex=1.5, col=mclust.options("classPlotColors")[1])
#add legend
text(0.8, -0.6, "más similares a ", cex=1.1)
text(0.8, -0.8, expression(paste(italic("E. cabrerensis"))), cex=1.1)
text(0.8, -1, "type", cex=1.1)
arrows(x0=0.8, y0=-0.5, x1 =0.94, y1 =0.09, length = 0.08, angle = 25, code =2, col="black")
#legend("bottomright", paste("P", c(1)),
#	col=mclust.options("classPlotColors")[1],
#	pch=mclust.options("classPlotSymbols")[1], pt.lwd=1, pt.cex=1.5, cex=1.3, bty="o")
xl <- -1.8
yt <- -1.75
legend(xl+0.09, yt-0.02, paste("P", c(3)), cex=1.3, bty="n")
points(xl+0.1, yt-0.23, col=mclust.options("classPlotColors")[3],
       pch=mclust.options("classPlotSymbols")[3], cex=1.5)
points(xl+0.22, yt-0.23, col="gray90",
       pch=mclust.options("classPlotSymbols")[3], cex=1.5)
rect(xleft=xl, ybottom=-2.2, xright=xl+0.56, ytop=yt)
#add title
title(expression(paste("Especímenes similares a", italic("E. cabrerensis"))), adj=0)

# 7.1.2) Plantas madre
diff.cabrerensis[,308:350]
sort(dist.Euclidian.cabrerensis[308:350])
hist(dist.Euclidian.cabrerensis[308:350], breaks=25)
mean.phenodata[c(names(sort(dist.Euclidian.cabrerensis[308:350]))[1:3]),]
mean.phenodata[which(mean.phenodata[,20]=="Espeletia.cabrerensis"),]
Mcluster.phenodata$classification[c(names(sort(dist.Euclidian.cabrerensis[308:350]))[1:15])]
# 1060 1059 1044 1058 1050 1049 1056 1021 1024 1061 1051 1054 1052 1039 1025 
#   3    3    3    3    3    3    3    3    3    3    3    3    4    2    3 

#plot en PC3 y PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
# Agregar especímenes asignados a P3
points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==3,10],
        phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==3,9],
        pch=mclust.options("classPlotSymbols")[3], cex=1.5, col="gray90")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text( -0.17, 0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.31, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0.03, "P5")
# Agregar especímenes similares a especímes tipo E. cabrerensis
points(Mcluster.phenodata$data[c(names(sort(dist.Euclidian.cabrerensis[308:350]))[1:3]),3],
       Mcluster.phenodata$data[c(names(sort(dist.Euclidian.cabrerensis[308:350]))[1:3]),2],
       pch=mclust.options("classPlotSymbols")[3], cex=1.5, col=mclust.options("classPlotColors")[3])
text(0.8, -0.6, "más similares a ", cex=1.1)
text(0.8, -0.8, expression(paste(italic("E. cabrerensis"))), cex=1.1)
arrows(x0=0.8, y0=-0.5, x1 =0.94, y1 =-0.1, length = 0.08, angle = 25, code =2, col="black")
#legend("bottomright", paste("P", c(1)),
#	col=mclust.options("classPlotColors")[1],
#	pch=mclust.options("classPlotSymbols")[1], pt.lwd=1, pt.cex=1.5, cex=1.3, bty="o")
xl <- -1.8
yt <- -1.75
legend(xl+0.09, yt-0.02, paste("P", c(3)), cex=1.3, bty="n")
points(xl+0.1, yt-0.23, col=mclust.options("classPlotColors")[3],
       pch=mclust.options("classPlotSymbols")[3], cex=1.5)
points(xl+0.22, yt-0.23, col="gray90",
       pch=mclust.options("classPlotSymbols")[3], cex=1.5)
rect(xleft=xl, ybottom=-2.2, xright=xl+0.56, ytop=yt)
#add title
title(expression(paste("Especímenes similares a", italic("E. cabrerensis"))), adj=0)



###################################################################################################################
# 7.2) Espeletia miradorensis
# 7.2.1) Todas las plantas
mean.phenodata.miradorensis.log <- as.numeric(log(mean.phenodata[which(mean.phenodata[,20]=="Espeletia.miradorensis"),7:17] + c(rep(0,4), 1, rep(0,6))))
diff.miradorensis <- apply(as.matrix(mean.phenodata.selected.log[,-c(12,13)]), MARGIN=1, FUN=function(x) x-mean.phenodata.miradorensis.log)
diff.miradorensis[,1:10]
(diff.miradorensis^2)[,1:10]

dist.Euclidian.miradorensis <- sqrt(colSums(diff.miradorensis^2))
sort(dist.Euclidian.miradorensis)
o.miradorensis<-order(dist.Euclidian.miradorensis)
hist(dist.Euclidian.miradorensis, breaks=25)
mean.phenodata[c("794","798","800"),]
mean.phenodata[which(mean.phenodata[,20]=="Espeletia.miradorensis"),]
Mcluster.phenodata$classification[c("794", "798", "800", "799", "793", "290", "1007", "880", "1011", "1010", "651", "689", "911", "796", "795")]
Mcluster.phenodata$classification[o.miradorensis[1:15]] 
#plot en PC3 y PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
#add specimens assigned to P2
points(Mcluster.phenodata$data[Mcluster.phenodata$classification==2,3],
       Mcluster.phenodata$data[Mcluster.phenodata$classification==2,2],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col="gray90")
#agregar elipses

for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text( -0.17, 0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.31, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0.03, "P5")
#agregar especímenes similares al tipo E. miradorensis 

points(Mcluster.phenodata$data[c("794","798","800"),3],
       Mcluster.phenodata$data[c("794","798","800"),2],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, col=mclust.options("classPlotColors")[2], lwd=2)
#points(Mcluster.phenodata$data[c("794", "798", "800", "799", "793", "290", "1007", "880", "1011", "1010", "651", "689", "911", "796", "795"),3],
#	Mcluster.phenodata$data[c("794", "798", "800", "799", "793", "290", "1007", "880", "1011", "1010", "651", "689", "911", "796", "795"),2],
#	pch=mclust.options("classPlotSymbols")[6], cex=1.5, col=mclust.options("classPlotColors")[6])
text(-0.75, -1.18, "más similar al ", cex=1.1)
text(-0.75, -1.38, expression(paste("tipo ",italic("E. miradorensis"))), cex=1.1)
arrows(x0=-0.45, y0=-1.73, x1 =-0.45, y1 =-0.95, length = 0.1, angle = 20, code = 0, col="black")
arrows(x0=-0.45, y0=-0.95, x1 =-0.40, y1 =-0.95, length = 0.1, angle = 20, code = 0, col="black")
arrows(x0=-0.45, y0=-1.73, x1 =-0.40, y1 =-1.73, length = 0.1, angle = 20, code = 0, col="black")
#add legend
#legend("bottomright", paste("P", c(1)),
#	col=mclust.options("classPlotColors")[1],
#	pch=mclust.options("classPlotSymbols")[1], pt.lwd=1, pt.cex=1.5, cex=1.3, bty="o")
xl <- -2.1
yt <- -1.75
text(xl+0.4, yt-0.23, paste("P", c(2)), cex=1.3, bty="n")
points(xl+0.1, yt-0.23, col=mclust.options("classPlotColors")[2],
       pch=mclust.options("classPlotSymbols")[2], cex=1.5, lwd=2)
points(xl+0.22, yt-0.23, col="gray90",
       pch=mclust.options("classPlotSymbols")[2], cex=1.5)
rect(xleft=xl, ybottom=-2.2, xright=xl+0.56, ytop=yt)
#add title
title(expression(paste("Especímenes más similar a ", italic("E. miradorensis"))), adj=0)

# 7.2.2) Plantas madre
diff.miradorensis[,308:350]
sort(dist.Euclidian.miradorensis[308:350])
hist(dist.Euclidian.miradorensis[308:350], breaks=25)
mean.phenodata[c(names(sort(dist.Euclidian.miradorensis[308:350]))[1:3]),]
mean.phenodata[which(mean.phenodata[,20]=="Espeletia.miradorensis"),]
Mcluster.phenodata$classification[c(names(sort(dist.Euclidian.miradorensis[308:350]))[1:15])]
# 1030 1045 1023 1022 1046 1031 1061 1047 1032 1058 1039 1038 1053 1060 1049 
#   4    5    5    2    5    2    3    5    2    3    2    2    5    3    3
#plot en PC3 y PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (11.47% varianza)", ylab="PC2 (14.81% varianza)", cex=0, cex.lab=1.5, cex.axis=1.5)
#agregar especímenes asignados a P4 y P5
points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==4,10],
              phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==4,9],
              pch=mclust.options("classPlotSymbols")[4], cex=1.5, col="gray90")
points(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==5,10],
       phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6]==5,9],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, col="gray90")
#agregar elipses

for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#Agregar etiquetas de las elipses
# Mcluster.phenodata$parameters$mean[c(3,2),]
#         [,1]       [,2]      [,3]       [,4]       [,5]
# PC3 -0.1657277 -0.2814246 0.6479339 -0.1735923 0.08351875
# PC2  0.6643681 -0.3152533 0.3752883  0.2845935 0.00324688
text( -0.17, 0.66, "P1")
text(-0.17, 0.28, "P4")
text(-0.28, -0.31, "P2")
text(0.65, 0.38, "P3")
text(0.08, 0.03, "P5")
#agregar especímenes similares al tipo E. miradorensis 

points(Mcluster.phenodata$data[c(names(sort(dist.Euclidian.miradorensis[308:350]))[1]),3],
       Mcluster.phenodata$data[c(names(sort(dist.Euclidian.miradorensis[308:350]))[1]),2],
       pch=mclust.options("classPlotSymbols")[4], cex=1.5, col=mclust.options("classPlotColors")[4], lwd=2)
points(Mcluster.phenodata$data[c(names(sort(dist.Euclidian.miradorensis[308:350]))[2:3]),3],
       Mcluster.phenodata$data[c(names(sort(dist.Euclidian.miradorensis[308:350]))[2:3]),2],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, col=mclust.options("classPlotColors")[5], lwd=2)

xl <- -2.1
yt <- -1.75
text(xl+0.4, yt-0.23, paste("P", c(4)), cex=1.3, bty="n")
points(xl+0.1, yt-0.23, col=mclust.options("classPlotColors")[4],
       pch=mclust.options("classPlotSymbols")[4], cex=1.5, lwd=2)
points(xl+0.22, yt-0.23, col="gray90",
       pch=mclust.options("classPlotSymbols")[4], cex=1.5)

text(xl+0.4, yt, paste("P", c(5)), cex=1.3, bty="n")
points(xl+0.1, yt, col=mclust.options("classPlotColors")[5],
       pch=mclust.options("classPlotSymbols")[5], cex=1.5, lwd=2)
points(xl+0.22, yt, col="gray90",
       pch=mclust.options("classPlotSymbols")[5], cex=1.5)
rect(xleft=xl, ybottom=-2.2, xright=xl+0.56, ytop=yt+0.2)
#add title
title(expression(paste("Especímenes de plantas madre más similar a ", italic("E. miradorensis"))), adj=0)

dev.off()

###################################################################################################################
###################################################################################################################
# 7) Histograma de de la altura a la que fueron colectado los especímenes
###################################################################################################################
###################################################################################################################
#cargar datos
load("MeanPhenodataSelected_2023septiembre07_052114.RData")
load("MeanPhenodata_2023septiembre07_050654.RData")

#Examinar la variación altitudinal de los especímenes usado en el modelo de mezclas
summary(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),6][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),6])])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2800    3599    3760    3727    3877    4147
#histograma de todas las plantas
hist(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),6]
     [!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),6])],
     xlab = "Altitud (m)",ylab = "Frecuencia especímenes", main = "")
#resumen de sólo las plantas madre
summary(mean.phenodata[as.numeric(rownames(mean.phenodata.selected[308:350,])),6])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 3560    3576    3618    3672    3798    3863
hist(mean.phenodata[as.numeric(rownames(mean.phenodata.selected[308:350,])),6],
     xlab = "Altitud (m)",ylab = "Frecuencia especímenes", main = "")

hist(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),6]
     [!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),6])],
     xlab = "Altitud (m)",ylab = "Frecuencia especímenes", main = "")
hist(mean.phenodata[as.numeric(rownames(mean.phenodata.selected[308:350,])),6],
     add=T, col="gray20")
legend("bottom", inset=c(, -25), legend = c("Todas las plantas", "Plantas madre"),horiz = T)

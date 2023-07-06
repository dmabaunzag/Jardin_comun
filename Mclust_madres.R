###################################################################################################################
###################################################################################################################
###################################################################################################################
#
# INTRODUCCIÓN
#
#
#
#
###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
# 1) Preliminares: cargar las librerías y los datos
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
setwd("C:/Users/usuario/Documents/Jardin_comun")#copiar su directorio de trabajo acá

#leer las tablas de datos, examinar y resumir los datos

mean.phenodata.pineda <- read.table("meanphenodata_2022Apr27_160817.csv", header=T, sep=",")#datos usados en Pineda 
  #et al.2020 para la formación de grupos fenotípicos: 21 variables y 1020 observaciones
summary(mean.phenodata.pineda)
head(mean.phenodata.pineda)
dim(mean.phenodata.pineda)

mean.phenodata.piloto<-read.table("meanphenodatapilot_2023Jun28_173331.csv", header = T, sep = ",")# datos de las
  #plantas madres de los con las mismas variables analizadas en Pineda et al.para la conformación de grupos
summary(mean.phenodata.piloto)
head(mean.phenodata.piloto)
dim(mean.phenodata.piloto)# 21 variables y 43 especímenes

#combinar las dos tablas para correr el modelo y agrupar los 43 especímenes del piloto al modelo

mean.phenodata<-rbind(mean.phenodata.pineda,mean.phenodata.piloto)
summary(mean.phenodata)
head(mean.phenodata)
dim(mean.phenodata) # con 1063 especímenes y 21 variables

#unidades de medida de cada variabes
measurement.units <- c(NA, NA, NA, "decimal.degrees", "decimal.degrees", "m", "cm",
                       "cm", "cm", "cm", "count", "count", "cm", "cm", "mm", "mm", "mm", "mm", "mm", NA, NA)
data.frame(colnames(mean.phenodata), measurement.units)

###################################################################################################################
###################################################################################################################
# 2) Examinar  ladistribución de cada raso fenotípico, edita los datos y transformación y rotación de los datos con
#    análisis de componentes principales.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 2.1) Examinar gráficamente la distribución de cada rasgo fenotípico en escala logarítmica y linear.


colnames(mean.phenodata) # nombre de las variables
trait.x <- 15 # número de columna/variable/rasgo fenotípico
colnames(mean.phenodata)[trait.x] # Qué variable
#distribución de la logintud de las filarias estériles en escala lineal.
hist(mean.phenodata[,trait.x], breaks=100, xlab=colnames(mean.phenodata)[trait.x], main="", col="gray80")
summary(mean.phenodata[,trait.x])
#distribución de la logintud de las filarias estériles en escala logarítmica.
hist(log(mean.phenodata[,trait.x]), breaks=100, xlab=colnames(mean.phenodata)[trait.x], main="", col="gray80")
summary(log(mean.phenodata[,trait.x]))
#distribución de la logintud de las filarias estériles en escala logarítmica agregándole uno; puede ser útil cuano 
#hay ceros en los datos crudos 
hist(log(mean.phenodata[,trait.x]+1), breaks=100, 
     xlab=paste(colnames(mean.phenodata)[trait.x], "(", measurement.units[trait.x], ")"), main="", col="gray80")
summary(log(mean.phenodata[,trait.x]+1))

#examinar gráficamente relaciones bivariables
#definir dos rasgos fenotípicos a examinar:
colnames(mean.phenodata)
trait.x <- 9
trait.y <- 10
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
# 2.2) realizar un subconjunto de datos sólo con los rasgos fenotípicos para el análisis. La selección de los rasgos
# fue hecha a priori en bases en ideas de importancia de carátecteres para distinguir las especies y basado en la 
# monografíade Espeletiinae (Cuatrecasas, 2013). 

#seleccionar un subconjunto con los rasgos fenotípicos: de la columna 7 a 19
colnames(mean.phenodata) 
mean.phenodata.selected <- mean.phenodata[,7:19]

#examine the results
dim(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)

###################################################################################################################
# 2.3) Remover los especímenes con valores NA en algún rasgo fenotípico.

#determinar qué especimenes tiene  valores NA para ser excluídos del análisis
rows.with.na <- unique(which(is.na(mean.phenodata.selected), arr.ind = T)[,1])
rows.with.na # especímenes con valores NA
length(rows.with.na) #número de especímenes con NA:713
#correr las siguietes líneas en caso de existir NAs
mean.phenodata.selected <- mean.phenodata.selected[-rows.with.na,]
dim(mean.phenodata.selected) # 350 especímenes con datos en los 13 rasgos fenotípicos
class(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)

#Tenga en cuenta que la referencia al dataframe original (mean.phenodata) se puede hacer por el nombre de fila:
rownames(mean.phenodata.selected)
as.numeric(rownames(mean.phenodata.selected))
#Usando el índice numérico puede saber el número de colector  y el colector de los especímenes analizados,por ejemplo:
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),c(1,2)]
#or las coordenadas y la elavción de los especímenes:
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

#rasgos continuos transformación log, dado que los raasos fenotípicos frecuentemenente siguen distribución log-normal
#asegurarse agregar 1 a los rasgos que estén acotdos a la izquierda con cero.
mean.phenodata.selected.log <- log(data.frame(mean.phenodata.selected[,1:4], mean.phenodata.selected[,5]+1, 
                                              mean.phenodata.selected[,6:13]))
head(mean.phenodata.selected.log)

#editar el nombre de las variables
colnames(mean.phenodata.selected.log) <- paste("log", colnames(mean.phenodata.selected))
colnames(mean.phenodata.selected.log)[5] <- paste("log", paste(colnames(mean.phenodata.selected)[5], "+1", sep=""))
class(mean.phenodata.selected.log)
summary(mean.phenodata.selected.log)

###################################################################################################################
# 2.5) Análisis de componentes principales (PCA) a la matriz de covarianza de los rasgos continuos con 
#transformación logarítmica.

#PCA para la mariz de covarianza
mean.phenodata.selected.log.pca <- prcomp(
  mean.phenodata.selected.log, center = T, scale. = F) #PCA usando matriz de covarianza
mean.phenodata.selected.log.pca

#examinar los resultados del PCA
attributes(mean.phenodata.selected.log.pca)
mean.phenodata.selected.log.pca$scale
mean.phenodata.selected.log.pca$center
summary(mean.phenodata.selected.log.pca) #varianza explicada en cada componente
summary(mean.phenodata.selected.log.pca$x) #resumen de los principales componentes
mean.phenodata.selected.log.pca$rotation # coeficinetes (o "loadings") de cada rasgo en cada componente

#guardar el análisis de PCA
save(mean.phenodata.selected.log.pca, file=paste("MeanPhenodataSelectedLogPca_", 
                                                 format(Sys.time(), "%Y%b%d_%H%M%S"), ".RData", sep=""))
#cargar el análisis de PCA
load("MeanPhenodataSelectedLogPca_2023jul.06_115221.RData")

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
# 3) selección de variables para los modelos de mezclas normales.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 3.1) selección de variables haia atrás unsando PCA de la matriz de covarianza de los rasgos fenotípicos con
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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="VARS")
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, search=c("greedy"), direction = c("backward"))
#examine results
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#these are the traits selected, in the order they were selected: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="STD")
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, search=c("greedy"), direction = c("backward"))
#examine results
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#these are the traits selected, in the order they were selected: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="SPH")
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, search=c("greedy"), direction = c("backward"))
#examine results
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#these are the traits selected, in the order they were selected: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="PCR")
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, search=c("greedy"), direction = c("backward"))
#examine results
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#these are the traits selected, in the order they were selected: 1,2,3,4,5,6,7,8,9,10,11,12

mclust.options(hcUse="SVD")
mean.phenodata.selected.log.pca.varsel.back <- clustvarsel(mean.phenodata.selected.log.pca$x, G=1:10, search=c("greedy"), direction = c("backward"))
#examine results
attributes(mean.phenodata.selected.log.pca.varsel.back)
summary(mean.phenodata.selected.log.pca.varsel.back)
names(mean.phenodata.selected.log.pca.varsel.back$subset) 
mean.phenodata.selected.log.pca.varsel.back$steps.info
mean.phenodata.selected.log.pca.varsel.back$search
mean.phenodata.selected.log.pca.varsel.back$direction
#these are the traits selected, in the order they were selected: 1,2,3,4,5,6,7,8,9,10,11,12


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
setwd("C:/Users/usuario/Documents/Jardin_comun")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

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
# 2) Examinar  ladistribución de cada rasgo fenotípico, edita los datos y transformación y rotación de los datos con
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
# 3.1) selección de variables hacia atrás usando PCA de la matriz de covarianza de los rasgos fenotípicos con
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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,3,4,5,6,7,8,9,10,11,12

###################################################################################################################
# 3.2) selección de variables haia adelante unsando PCA de la matriz de covarianza de los rasgos fenotípicos con
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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,4,3,6,7,5,8,12,9,11,10

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
#rasgos seleccionados en orden: 1,2,4,3,6,7,5,8,9,11,10

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
#éstos son los rasgos seleccionados en orden por el modelo: 1,2,4,3,6,7,5,8,12,9,11,10 

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
#Éstos son los rasgos seleccionados en orden por el modelo: 1,2,4,3,6,7,5,8,12,9,11,10

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
#Éstos son los rasgos seleccionados en orden por el modelo: 1,2,4,3,6,7,5,8,12,9,11,10

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
#Éstos son los rasgos seleccionados en orden por el modelo: 1,2,4,3,6,7,5,8,12,9,11,10

##la selección de las variables tanto hacia adelante como hacia atrás, escogieron los primeros 12 rasgos fenotípicos 


###################################################################################################################

###################################################################################################################
# 4) Ajuste de los modelosde mezlas normales
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 4.1) Seleccioinar rasgos fnotípicos (PCA) para la inclusión del método de meclas normales basado en los resultados
#     de las secciones 3.1 3.2.

data.for.GMM <- mean.phenodata.selected.log.pca$x[,1:12]

###################################################################################################################
# 4.2)ajuste de mezclas normales usandoo diferentes valores de inicialización, usando el argument "hcUse"

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
#   Mclust VVE (ellipsoidal, equal orientation) model with 8 components: 
#   
#   log-likelihood   n  df       BIC       ICL
#      186.1445     350 265   -1180.063   -1186.984
# 
# Clustering table:
#   1   2   3   4   5   6   7   8 
#   23 124  23  59  32  46  29  14 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata, what="BIC")

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


#guardar el mejor modelo en el directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Directorio de Iván Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun")
save(Mcluster.phenodata, file=paste("Mcluster.phenodata_", format(Sys.time(), "%Y%b%d"), ".RData", sep=""))
load("Mcluster.phenodata_2023jul.26.RData")


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
#   Mclust VVE (ellipsoidal, equal orientation) model with 8 components: 
#   
#   log-likelihood   n  df       BIC       ICL
#     186.1445      350 265     -1180.063 -1186.984
# 
# Clustering table:
#   1   2   3   4   5   6   7   8 
#   23 124  23  59  32  46  29  14 

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
#   Mclust VVE (ellipsoidal, equal orientation) model with 11 components: 
#   
#   log-likelihood   n  df      BIC      ICL
# 331.3036          350 340   -1329.09 -1341.62
# 
# Clustering table:
#   1  2  3  4  5  6  7  8  9 10 11 
#   23 12 41 25 59 48 16 39 46 25 16 

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
# Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 11 components: 
#   
#   log-likelihood   n  df      BIC      ICL
#   331.3036        350 340   -1329.09 -1341.62
# 
# Clustering table:
#   1  2  3  4  5  6  7  8  9 10 11 
#   23 12 41 25 59 48 16 39 46 25 16 

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
#   Mclust VVE (ellipsoidal, equal orientation) model with 11 components: 
#   
#   log-likelihood   n  df      BIC      ICL
#     331.3036     350  340   -1329.09   -1341.62
# 
# Clustering table:
#   1  2  3  4  5  6  7  8  9 10 11 
#  23 12 41 25 59 48 16 39 46 25 16 
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
#   Mclust VVE (ellipsoidal, equal orientation) model with 11 components: 
#   
#   log-likelihood   n  df      BIC      ICL
#     331.3036      350 340   -1329.09 -1341.62
# 
# Clustering table:
#   1  2  3  4  5  6  7  8  9 10 11 
#  23 12 41 25 59 48 16 39 46 25 16 

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
load("Mcluster.phenodata_2023jul.26.RData")

###################################################################################################################
# 5.1)Examinar y guardar en un documento para asignación de los especímenes a los grupos fenotípicos. 

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment <- data.frame(as.numeric(rownames(mean.phenodata.selected)),
                                          mean.phenodata[as.numeric(rownames(mean.phenodata.selected)),1], 
                                          Mcluster.phenodata$classification, Mcluster.phenodata$uncertainty)
colnames(phenotypic.group.assignment) <- 
  c("Rownames.Meanphenodata", "Collector.Collection.Number", "Phenotypic.Group", "Uncertainty")
head(phenotypic.group.assignment)
setwd("C:/Users/usuario/Documents/Jardin_comun")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(phenotypic.group.assignment, 
          file=paste("PhenotypicGroupAssignment_", format(Sys.time(), "%Y%b%d_%H%M%S"), ".csv", sep=""), row.names = F)

#Examinar los grupos fenotípicps de as plantas del piloto

phenotypic.group.assignment.piloto<-phenotypic.group.assignment[308:350,]

phenotypic.group.assignment.piloto##Las plantas del piloto se encuentran en tres de los 11 grupos fenotípicos: 4,10 y 11

###################################################################################################################
# 5.2) Examinar los parámetros de la distribución normal multivariable definiendo cada grupo fenotípico de acuerdo
# al mejor modelo de mezcla de normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# Diana's directory

#jet.colors3 <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
jet.colors3 <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#jet.gray <- colorRampPalette(c("gray90", "black"))

xy.coo <- seq(0, 1, length.out=12)

#Calcular rangos de valores en la correlación de matrices de los grupos fenotípicos
minmaxCO <- matrix(NA, nrow=11, ncol=2)
for(i in 1:11){
  CO <- cov2cor(Mcluster.phenodata$parameters$variance$sigma[,,i])
  diag(CO) <- NA
  CO[lower.tri(CO)] <- NA
  minmaxCO[i,] <- range(CO, na.rm=T) 
}
range(minmaxCO) #para graficar usar este rango: -0.9 to 0.6


#seleccionar grupo fenotípico
P <- 11

#http://www.statistics4u.com/fundstat_eng/cc_scaling.html
#escalando rano spara el promedio
M.Rmin <- 1.06
M.Rmax <- 1.28 
M.Dmin <- apply(Mcluster.phenodata$parameters$mean, MARGIN=1, FUN=min)
M.Dmax <- apply(Mcluster.phenodata$parameters$mean, MARGIN=1, FUN=max)
#M.range.scaling <- (M.Rmax - M.Rmin)/(M.Dmax - M.Dmin) + (M.Rmin*M.Dmax - M.Rmax*M.Dmin)/(M.Dmax - M.Dmin)
M.rs <- Mcluster.phenodata$parameters$mean * (M.Rmax - M.Rmin)/(M.Dmax - M.Dmin) + 
  (M.Rmin*M.Dmax - M.Rmax*M.Dmin)/(M.Dmax - M.Dmin)

# Escalando el rango para la varianza
V.Rmin <- 0.5
V.Rmax <- 4 
V.Dmin <- diag(apply(Mcluster.phenodata$parameters$variance$sigma, MARGIN=c(1,2), FUN=min))
V.Dmax <- diag(apply(Mcluster.phenodata$parameters$variance$sigma, MARGIN=c(1,2), FUN=max))
V.raw <- matrix(NA, nrow=12, ncol=11)
for(i in 1:11){
  V.raw[,i] <- diag(Mcluster.phenodata$parameters$variance$sigma[,,i])
}
V.rs <- V.raw * (V.Rmax - V.Rmin)/(V.Dmax - V.Dmin) + (V.Rmin*V.Dmax - V.Rmax*V.Dmin)/(V.Dmax - V.Dmin)

#Obtener matriz de correlación.
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
text(xy.coo, xy.coo-0.08, labels=as.character(1:12), adj=0.5, cex=1.5)
text(-0.1, xy.coo, labels=as.character(1:12), cex=1.5)
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

#Leyenda para el promedio y la varianza.
par(mar=c(1,1,1,1))
image(CO, col="transparent", xaxt="n", yaxt="n", bty="n", xlim=c(-0.15, 1.3), ylim=c(-0.15, 1.3), zlim=c(-0.9,0.6))
rect(xleft=-0.05, ybottom=1.06, xright=0.09090909+0.05, ytop=1.28, col = "gray90", border="gray90")
segments(x0=-0.05, y0=1.17, x1=0.09090909+0.05, y1=1.17, lwd=0.5, col="gray60", lty=3)
points(xy.coo[1:2], c(1.06,1.28), pch=19, cex=0.6)
segments(x0=xy.coo[1:2], y0=1.17, x1=xy.coo[1:2], y1=c(M.Rmin, M.Rmax), lty=1)
text(mean(xy.coo[1:2]), 1, "Mean", cex=1.5)
text(xy.coo[2]+0.3, 1.11, "minimun", cex=1.5)
text(xy.coo[2]+0.3, 1.24, "maximum", cex=1.5)
#leyenda para la varianza
points(xy.coo[c(8,8)]+0.045, c(1.11,1.24), pch=19, cex=c(V.Rmin, V.Rmax))
text(xy.coo[c(8,8)]+0.045, 1, "Variance", cex=1.5)
#text(xy.coo[c(9,9)]+0.045, 1.16, "Variance", cex=1.5, srt=90)
rect(xleft=-0.07, ybottom=0.95, xright=xy.coo[c(9,9)]+0.11, ytop=1.3, col = "transparent", border="black")

# agregar leyenda para la correlación de las matrices
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
#Desde R. Bivand:
#As far as I remenber, the arguments are: 
#xl x-location of legend panel left edge 
#yt y-location of legend panel top edge 
#width fill box width 
#nbox number of boxes 
#bheight fill box height 
#bgap vertical gap between boxes 
#col nbox colours 
#border colour to draw fill box boundaries 

#image(CO, col="transparent", xaxt="n", yaxt="n", bty="n", xlim=c(-0.15, 1.3), ylim=c(-0.15, 1.3))
#imagelegend(0.8, 0.7, 0.02, 20, 0.02, 0, jet.colors3(20), "black")
#imagelegend(xl=0.8, yt=0.7, width=0.02, nbox=20, bheight=0.02, bgap=0, col=jet.colors3(20)[20:1], border="transparent")
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(1,1,1,1))
plot(c(-0.9,0.6), c(-0.9,0.6), xaxt="n", yaxt="n", bty="n", type="n")
#imagelegend(xl=0, yt=1, width=0.05, nbox=29, bheight=0.05, bgap=0, col=jet.colors3(20)[20:1], border="transparent")
imagelegend(xl=0, yt=0.6, width=0.05, nbox=30, bheight=0.05, bgap=0, col=jet.colors3(30)[30:1], border="transparent")
axis(4, line=-9, at=seq(-0.9, 0.6, 0.05), labels=F)
axis(4, line=-9, at=seq(-0.9, 0.6, 0.1), labels=F, tcl=-1, cex.axis=1.5)
axis(4, line=-8.5, at=seq(-0.9, 0.6, 0.1), labels=T, tcl=-1, cex.axis=1.5, lwd=0)
mtext(side=4, "Correlation (Pearson's r)", cex=1.5, line=-5)
#text(-0.6, -0.1, labels="I) Correlation", cex=2, srt=90)

#par(mar=c(5,4.5,4,2)+0.1) #default
par(mar=c(7,1,7,1))
#image(CO, col="transparent", xaxt="n", yaxt="n", bty="n", xlim=c(-0.15, 1.3), ylim=c(-0.15, 1.3))
#plot(rep(1:12, 2), c(M.Dmin, M.Dmax), xaxt="n", yaxt="n", bty="n", type="n")
#plot(rep(1:12, 2), c(M.Dmin, M.Dmax), xaxt="n", xlab="", ylab="Mean", bty="n", type="n", cex.axis=1.5, cex.lab=1.5, ylim=c(-1.7,0.7))
plot(rep(xy.coo, 2), c(M.Dmin, M.Dmax), xaxt="n", yaxt="n", xlab="", ylab="", bty="n", type="n",
     cex.axis=1.5, cex.lab=1.5,  xlim=c(-0.15, 1.3), ylim=c(-1.7,0.7), xaxs="i")
points(xy.coo, M.Dmax, type="o", pch=19)
points(xy.coo, M.Dmin, type="o", pch=19)
axis(2, at=round(seq(-1.7,0.7,0.1),3), labels=F, tcl=-0.5, line=-2.2)
axis(2, at=c(-1.5,-1,-0.5,0,0.5), labels=F, tcl=-0.7, line=-2.2)
axis(2, at=c(-1.5,-1,-0.5,0, 0.5), labels=T, cex.axis=1.5, line=-2.2, lwd=0, las=2)
#abline(h=0, lty=3)
axis(1, at=xy.coo, labels=F, line=0, cex.axis=1.5)
axis(1, at=xy.coo[seq(2,12,2)], labels=seq(2,12,2), line=0, cex.axis=1.5, lwd=0)
axis(1, at=xy.coo[seq(1,12,2)], labels=seq(1,12,2), line=1, cex.axis=1.5, lwd=0)
text(xy.coo[8], -1, labels="G) Mean", cex=2)
mtext(side=1, "Principal component", cex=1.5, line=4, at=xy.coo[6]+0.05)


#par(mar=c(5,4.5,4,2)+0.1) #default
par(mar=c(7,1,7,1))
plot(rep(xy.coo, 2), c(V.Dmin, V.Dmax), xaxt="n", yaxt="n", xlab="", ylab="", bty="n", type="n",
     cex.axis=1.5, cex.lab=1.5,  xlim=c(-0.15, 1.3), xaxs="i")
points(xy.coo, V.Dmax, type="o", pch=19)
points(xy.coo, V.Dmin, type="o", pch=19)
axis(2, at=round(seq(0,1.5,0.1),3), labels=F, tcl=-0.5, line=-2.2)
axis(2, at=c(0,0.5,1,1.5), labels=F, tcl=-0.7, line=-2.2)
axis(2, at=c(0,0.5,1,1.5), labels=T, cex.axis=1.5, lwd=0, las=2,  line=-2.2)
axis(1, at=xy.coo, labels=F, line=0, cex.axis=1.5)
axis(1, at=xy.coo[seq(2,12,2)], labels=seq(2,12,2), line=0, cex.axis=1.5, lwd=0)
axis(1, at=xy.coo[seq(1,12,2)], labels=seq(1,12,2), line=1, cex.axis=1.5, lwd=0)
text(xy.coo[8], 1, labels="H) Variance", cex=2)
mtext(side=1, "Principal component", cex=1.5, line=4, at=xy.coo[6]+0.05)

###################################################################################################################
# 5.3) Examinar la tabulación cruzada de las variables en los datos originales (mean.phenodata) y grupos fenotípicos 
# particulares en el mejor modelo de mezcla normal.

#seleccionar un morfogrupo en el modelo de mezla normal.
pg.nmm <- 6
# Seleccionar una columna en los datos originales(mean.phenodata)
colnames(mean.phenodata)
col.phenodata <- 11 
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification==pg.nmm],col.phenodata])

###################################################################################################################
# 5.4) Graficar el soporte empírico para el mejor modelo para cada grupo fenotípico.

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
# 5.5) Graficar grupos fenotípicos en el mejr modelo de mezclas normales. 

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Figuras")# Diana's directory


#PC1 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,2), main="", addEllipses = F,
     xlab="PC1 (44.46% varianza)", ylab="PC2 (16.29% varianza)", cex.axis=1.5, cex.lab=1.5)
legend("bottomleft", paste("P", 1:11), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#add ellipse labels
text(0.9, 0.79, "P1")
text(-1.7, 0.95, "P4")
text(0.5, -1.7, "P6")
text(-0.82, -0.85, "P2")
text(-1.03, -0.24, "P3")
text(0.85, -0.4, "P5")
arrows(x0=0.7, y0=-0.4, x1 =-0.25, y1 =-0.1, length = 0.1, angle = 20, code = 2)
#mtext(side=2, "a)", at=2, las=2, line=2, cex=1.5)

#PC1 vs PC3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(1,3), main="", addEllipses = F,
     xlab="PC1 (44.46% variance)", ylab="PC3 (10.60% variance)", cex.axis=1.5, cex.lab=1.5)
legend("bottomright", paste("P", 1:6), col=mclust.options("classPlotColors"),
       pch=mclust.options("classPlotSymbols"), pt.lwd=1, pt.cex=1, cex=1.2, bty="o")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(1,3),c(1,3),i],
                 centre = Mcluster.phenodata$parameters$mean[c(1,3),i], level = pchisq(1, 2)),
         type="l", col="black")
}

#PC3 vs PC2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.phenodata, what=c("classification"), dimens=c(3,2), main="", addEllipses = F,
     xlab="PC3 (10.60% variance)", ylab="PC2 (16.29% variance)", cex.axis=1.5, cex.lab=1.5)
legend("bottomleft", paste("P", 1:6), col=mclust.options("classPlotColors"),
       pch=mclust.options("classPlotSymbols"), pt.lwd=1, pt.cex=1, cex=1.2, bty="o")
#add ellipses
for (i in 1:Mcluster.phenodata$G){
  points(ellipse(x = Mcluster.phenodata$parameters$variance$sigma[c(3,2),c(3,2),i],
                 centre = Mcluster.phenodata$parameters$mean[c(3,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#add ellipse labels
text(0.13, 0.82, "P1")
text(-0.83, 0.9, "P4")
text(0.32, -1.72, "P6")
text(-0.5, -0.89, "P2")
text(0.475, -0.27, "P3")
text(-0.76, -0.06, "P5")
#mtext(side=2, "b)", at=2, las=2, line=2, cex=1.5)


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

library(mclust) # librería para adaptar modelo de mezclas normales
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
#definir dos rasgos fenotípicos para exaimar:
colnames(mean.phenodata)
trait.x <- 9
trait.y <- 10
#revisar los nombres de las variables seleccionasdas
colnames(mean.phenodata)[trait.x]
summary(mean.phenodata[,trait.x])
colnames(mean.phenodata)[trait.y]
summary(mean.phenodata[,trait.y])
#graficar la relaciones bivariables
plot(mean.phenodata[,trait.x], mean.phenodata[,trait.y],
     xlab=paste(colnames(mean.phenodata)[trait.x], "(", measurement.units[trait.x], ")"), 
     ylab=paste(colnames(mean.phenodata)[trait.y], "(", measurement.units[trait.y], ")"), cex.lab=1.5, cex.axis=1.5)

###################################################################################################################
# 2.2) relizar un subconjunto de datos sólo con los rasgos fenotípicos para el análisis. La selección de los rasgos
# fue hecha a priori en bases en ideas de importancia de carátecteres para distinguir las especies y basado en la monografía
# de Espeletiinae (Cuatrecasas, 2013). 

#select a subset of phenotypic traits
colnames(mean.phenodata) #these are column names, showing the names of phenotypic traits
mean.phenodata.selected <- mean.phenodata[,7:19]

#examine the results
dim(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)

###################################################################################################################
# 2.3) Remove specimens that have NA values for any of the phenotypic traits selected in the previous section.

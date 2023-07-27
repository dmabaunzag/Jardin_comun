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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

#leer las tablas de datos, examinar y resumir los datos

phenodata.progenie <- read.table("PhenotypicDataProgeny_Quebradas_2023March20.csv", header=T, sep=";") 
#datos del jardín común de Quebradas tomados en 20/03/2020
summary(phenodata.progenie)
head(phenodata.progenie)
dim(phenodata.progenie) 236# 236 plantas del piloto con 11 variables

#unidades de medida de cada variabes
measurement.units <- c(NA, NA, NA,NA, NA, NA,NA, NA, "cm", "count", NA)
data.frame(colnames(phenodata.progenie), measurement.units)

#2) Examinar  la distribución de cada rasgo fenotípico, edita los datos y transformación y rotación de los datos con
#    análisis de componentes principales.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 2.1) Examinar gráficamente la distribución de cada rasgo fenotípico en escala logarítmica y linear.


colnames(phenodata.progenie) # nombre de las variables
trait.x <- 10 # número de columna/variable/rasgo fenotípico
colnames(phenodata.progenie)[trait.x] # Qué variable
#distribución del número de hojas en escala lineal.
hist(phenodata.progenie[,trait.x], breaks=30, xlab=colnames(phenodata.progenie)[trait.x], main="", col="gray80")
summary(phenodata.progenie[,trait.x])
#distribución del número de hojas en escala logarítmica.
hist(log(phenodata.progenie[,trait.x]), breaks=30, xlab=colnames(phenodata.progenie)[trait.x], main="", col="gray80")
summary(log(phenodata.progenie[,trait.x]))
#distribución del número de hojas en escala logarítmica agregándole uno; puede ser útil cuano 
#hay ceros en los datos crudos 
hist(log(phenodata.progenie[,trait.x]+1), breaks=100, 
     xlab=paste(colnames(phenodata.progenie)[trait.x], "(", measurement.units[trait.x], ")"), main="", col="gray80")
summary(log(phenodata.progenie[,trait.x]+1))


colnames(phenodata.progenie) # nombre de las variables
trait.x <- 9 # número de columna/variable/rasgo fenotípico
colnames(phenodata.progenie)[trait.x] # Qué variable
#distribución del número de hojas en escala lineal.
hist(phenodata.progenie[,trait.x], breaks=30, xlab=colnames(phenodata.progenie)[trait.x], main="", col="gray80")
summary(phenodata.progenie[,trait.x])
#distribución del número de hojas en escala logarítmica.
hist(log(phenodata.progenie[,trait.x]), breaks=30, xlab=colnames(phenodata.progenie)[trait.x], main="", col="gray80")
summary(log(phenodata.progenie[,trait.x]))
#distribución del número de hojas en escala logarítmica agregándole uno; puede ser útil cuano 
#hay ceros en los datos crudos 
hist(log(phenodata.progenie[,trait.x]+1), breaks=100, 
     xlab=paste(colnames(phenodata.progenie)[trait.x], "(", measurement.units[trait.x], ")"), main="", col="gray80")
summary(log(phenodata.progenie[,trait.x]+1))


#examinar gráficamente relaciones bivariables
#definir dos rasgos fenotípicos a examinar:
colnames(phenodata.progenie)
trait.x <- 9
trait.y <- 10
#revisar los nombres de las variables seleccionadas
colnames(phenodata.progenie)[trait.x]
summary(phenodata.progenie[,trait.x])
colnames(phenodata.progenie)[trait.y]
summary(phenodata.progenie[,trait.y])
#graficar la relaciones bivariables
plot(phenodata.progenie[,trait.x], phenodata.progenie[,trait.y],
     xlab=paste(colnames(phenodata.progenie)[trait.x], "(", measurement.units[trait.x], ")"), 
     ylab=paste(colnames(phenodata.progenie)[trait.y], "(", measurement.units[trait.y], ")"), cex.lab=1.5, cex.axis=1.5)

###################################################################################################################
# 2.2) realizar un subconjunto de datos sólo con los rasgos fenotípicos para el análisis. 


#seleccionar un subconjunto con los rasgos fenotípicos: de la columna 9 y 10
colnames(phenodata.progenie) 
phenodata.progenie.selected <- phenodata.progenie[,9:10]

#examine the results
dim(phenodata.progenie.selected)
summary(phenodata.progenie.selected)
head(phenodata.progenie.selected)

###################################################################################################################
# 2.3) Remover los especímenes con valores NA en algún rasgo fenotípico.

#determinar qué especimenes tiene  valores NA para ser excluídos del análisis
rows.with.na <- unique(which(is.na(phenodata.progenie.selected), arr.ind = T)[,1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 0

#correr las siguietes líneas en caso de existir NAs
#phenodata.progenie.selected <- phenodata.progenie.selected[-rows.with.na,]
#dim(phenodata.progenie.selected) # 350 especímenes con datos en los 13 rasgos fenotípicos
#class(phenodata.progenie.selected)
#summary(phenodata.progenie.selected)
#head(phenodata.progenie.selected)

#Tenga en cuenta que la referencia al dataframe original (phenodata.progenie) se puede hacer por el nombre de fila:
rownames(phenodata.progenie.selected)
as.numeric(rownames(phenodata.progenie.selected))
#Usando el índice numérico puede saber el número de colector  y el colector de los especímenes analizados,por ejemplo:
phenodata.progenie[as.numeric(rownames(phenodata.progenie.selected)),c(1,2)]
#or las coordenadas y la elavción de los especímenes:
phenodata.progenie[as.numeric(rownames(phenodata.progenie.selected)),4:6]
#o filas con alguna información de los especímenes:Observaciones
!is.na(phenodata.progenie[as.numeric(rownames(phenodata.progenie.selected)),11])
phenodata.progenie[as.numeric(
  rownames(phenodata.progenie.selected)),11][!is.na(phenodata.progenie[as.numeric(rownames(phenodata.progenie.selected)),11])]


###################################################################################################################
# 2.4) Transfomación de los datos.

#rasgos continuos transformación log, dado que los raasos fenotípicos frecuentemenente siguen distribución log-normal
#asegurarse agregar 1 a los rasgos que estén acotados a la izquierda con cero.
phenodata.progenie.selected.log <- log(data.frame(phenodata.progenie.selected[,1]+1, phenodata.progenie.selected[,2]))
head(phenodata.progenie.selected.log)

#editar el nombre de las variables
colnames(phenodata.progenie.selected.log) <- paste("log", colnames(phenodata.progenie.selected))
colnames(phenodata.progenie.selected.log)[1] <- paste("log", paste(colnames(phenodata.progenie.selected)[1], "+1", sep=""))
class(phenodata.progenie.selected.log)
summary(phenodata.progenie.selected.log)

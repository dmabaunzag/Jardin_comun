###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################
#INTRODUCCIÓN



###################################################################################################################
###################################################################################################################
# 1) Preliminares: cargar las librerías
###################################################################################################################
###################################################################################################################
# Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mexclas normales
library(ellipse)
library(ggplot2)

###################################################################################################################
###################################################################################################################
# 2) Leer los rasgos fenotípicos
###################################################################################################################
###################################################################################################################

#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

#leer la tabla de datos: datos de la progenie tomados en Quebradas en 20/03/2020

phenodata.progenie <- read.table("PhenotypicDataProgeny_Quebradas_2023March20.csv", header=T, sep=",") 

View(phenodata.progenie)
summary(phenodata.progenie)
head(phenodata.progenie)
dim(phenodata.progenie)# 250 plantas del piloto con 26 variables


###################################################################################################################
###################################################################################################################
# 3) Limpiar los rasgos fenotípicos: editar los datos y transformarlos.
###################################################################################################################
###################################################################################################################
# 3.1) Filtrar rasgos fenotípicos a usar

colnames(phenodata.progenie)#sólo se usará número de colección (1), longitud del tallo (9) y número de hojas (10)
phenodata.progenie.selected<-phenodata.progenie[,c(1,9,10)]
View(phenodata.progenie.selected)
colnames(phenodata.progenie.selected)<-c("Número de colección plantas madres", "Longitud del tallo", "Número de hojas")
dim(phenodata.progenie.selected) #250 plantas de la progenie y 3 variables

#unidades de medida de cada variables
measurement.units <- c(NA,  "cm", "count")
data.frame(colnames(phenodata.progenie.selected), measurement.units)

###################################################################################################################
# 3.2) Remover plantas de la progenie sin madre asignada y con algún faltante en los rasgos fenotípicos

# cuantas plantas madres están representadas
unique(phenodata.progenie.selected$`Número de colección plantas madres`)## hay dos plantas sin madre
length(unique(phenodata.progenie.selected$`Número de colección plantas madres`))## 39 plantas madres representadas

# excluir plantas de la progenie sin planta madre y que no tenga datos (excluir todos los NAs)

sapply(phenodata.progenie.selected, class)

phenodata.progenie.selected[,1]<-as.numeric(phenodata.progenie.selected[,1])

sapply(phenodata.progenie.selected, class)

rows.with.na <- unique(which(is.na(phenodata.progenie.selected), arr.ind = T)[,1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 14 plantas hijas con NA

#correr las siguietes líneas en caso de existir NAs
phenodata.progenie.selected <- phenodata.progenie.selected[-rows.with.na,]
dim(phenodata.progenie.selected) # 236 hijas con todos los datos
class(phenodata.progenie.selected)
summary(phenodata.progenie.selected)
head(phenodata.progenie.selected)

length(unique(phenodata.progenie.selected$`Número de colección plantas madres`))## hay representación de 37 plantas madres
unique(phenodata.progenie.selected$`Número de colección plantas madres`)##cuáles

###################################################################################################################
# 3.3) Examinar gráficamente la distribución de cada rasgo fenotípico en escala logarítmica y linear.

# Examinar cuántas hijas tiene cada planta madre
sort(summary(as.factor(phenodata.progenie.selected$`Número de colección plantas madres`)), decreasing = T)
#gráfica
barplot(sort(summary(as.factor(phenodata.progenie.selected$`Número de colección plantas madres`)), decreasing = T), 
        las=2, xlab =  "Plantas madres", ylab = "Plantas hijas", ylim = c(0,25))

colnames(phenodata.progenie.selected) # nombre de las variables
trait.x <- 2 # Longitud del tallo
colnames(phenodata.progenie.selected)[trait.x] # Qué variable
#distribución del número de hojas en escala lineal.
hist(phenodata.progenie.selected[,trait.x], breaks=20, 
     xlab=paste(colnames(phenodata.progenie.selected)[trait.x], "(", measurement.units[trait.x], ")"),
     ylab="Frecuencia",main="", col="gray80")
summary(phenodata.progenie.selected[,trait.x])
#distribución del número de hojas en escala logarítmica.
hist(log(phenodata.progenie.selected[,trait.x]), breaks=20, 
     xlab=paste("log (",colnames(phenodata.progenie.selected)[trait.x], "(", measurement.units[trait.x], "))"),
     ylab = "Frecuencia", main="", col="gray80")
  summary(log(phenodata.progenie.selected[,trait.x]))

trait.x <- 3 # número de hojas
colnames(phenodata.progenie.selected)[trait.x] # Qué variable
#distribución del número de hojas en escala lineal.
hist(phenodata.progenie.selected[,trait.x], breaks=10, 
     xlab=paste(colnames(phenodata.progenie.selected)[trait.x], "(", measurement.units[trait.x], ")"),
     ylab="Frecuencia",main="", col="gray80")
summary(phenodata.progenie.selected[,trait.x])
#distribución del número de hojas en escala logarítmica.
hist(log(phenodata.progenie.selected[,trait.x]), breaks=10, 
     xlab=paste("log (",colnames(phenodata.progenie.selected)[trait.x], "(", measurement.units[trait.x], "))"),
     ylab = "Frecuencia", main="", col="gray80")
summary(log(phenodata.progenie.selected[,trait.x]))

###################################################################################################################
# 3.4) Examinar gráficamente relaciones bivariables
#definir dos rasgos fenotípicos a examinar:
colnames(phenodata.progenie.selected)
trait.x <- 2
trait.y <- 3
#revisar los nombres de las variables seleccionadas
colnames(phenodata.progenie.selected)[trait.x]
summary(phenodata.progenie.selected[,trait.x])
colnames(phenodata.progenie.selected)[trait.y]
summary(phenodata.progenie.selected[,trait.y])

#graficar la relaciones bivariables
cor(phenodata.progenie.selected[,trait.x], phenodata.progenie.selected[,trait.y])# correlación:0.5300861
regresion<-lm(phenodata.progenie.selected[,trait.y]~phenodata.progenie.selected[,trait.x])
plot(phenodata.progenie.selected[,trait.x], phenodata.progenie.selected[,trait.y],
     xlab=paste(colnames(phenodata.progenie.selected)[trait.x], "(", measurement.units[trait.x], ")"), 
     ylab=paste(colnames(phenodata.progenie.selected)[trait.y], "(", measurement.units[trait.y], ")"), cex.lab=1.5, cex.axis=1.5)
abline(regresion)


###################################################################################################################
#  3.5) Transfomación de los datos.

 # Dado que los rasgos fenotípicos frecuentemenente siguen distribución log-normal se transforman a escala logarítmica

phenodata.progenie.selected.log <- data.frame(phenodata.progenie.selected[,1],log(phenodata.progenie.selected[,2:3]))
head(phenodata.progenie.selected.log)

#editar el nombre de las variables
colnames(phenodata.progenie.selected.log) <-colnames(phenodata.progenie.selected)
colnames(phenodata.progenie.selected.log)[2:3] <- paste("log", paste(colnames(phenodata.progenie.selected)[2:3], sep=""))
class(phenodata.progenie.selected.log)
summary(phenodata.progenie.selected.log)



###################################################################################################################
# 3.6) Promedios de los rasgos fenotípicos por planta madre


mean.phenodata.progenie.selected.log<-aggregate(phenodata.progenie.selected.log, by=list(phenodata.progenie.selected.log[,1]),
                                            FUN = function(x) mean(x, na.rm=T))
mean.phenodata.progenie.selected.log<-mean.phenodata.progenie.selected.log[,2:4]
View(mean.phenodata.progenie.selected.log)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
save(mean.phenodata.progenie.selected.log, file=paste("mean.phenodata.progenie.selected.log_", format(Sys.time(),"%Y%B%d_%H%M%S"), ".RData", sep=""))
load("mean.phenodata.progenie.selected.log_2023agosto03_140929.RData")


###################################################################################################################
# 3.7) graficar los rasgos fenotípicos

#  log longitud del tallo

boxplot(`log Longitud del tallo`~ `Número de colección plantas madres`, data = phenodata.progenie.selected.log, las=2 )
promedio.longitud.tallo<- tapply(phenodata.progenie.selected.log$`log Longitud del tallo`,
                                 phenodata.progenie.selected.log$`Número de colección plantas madres`, mean)
xi<-0.4 +seq(phenodata.progenie.selected.log$n)
points(promedio.longitud.tallo, col="blue", pch=19)
legend("bottomright", legend = "Promedio", pch=19, col="blue")

# log número de hojas 
boxplot(`log Número de hojas`~`Número de colección plantas madres`, data = phenodata.progenie.selected.log, las=2 )
promedio.numero.hojas<- tapply(phenodata.progenie.selected.log$`log Número de hojas`,
                                 phenodata.progenie.selected.log$`Número de colección plantas madres`, mean)
xi<-0.4 +seq(phenodata.progenie.selected.log$n)
points(promedio.numero.hojas, col="blue", pch=19)
legend("bottomright", legend = "Promedio", pch=19, col="blue")


## gráficas de dispersión entre la longitud del tallo y el número de hojas por planta madre

phenodata.progenie.selected.log[,1]<-as.factor(phenodata.progenie.selected.log[,1])

ggplot()+
  geom_point(data=phenodata.progenie.selected.log, aes(
    x=`log Longitud del tallo`, y=`log Número de hojas`, color=`Número de colección plantas madres`))+
  geom_smooth(data=phenodata.progenie.selected.log, aes(x=`log Longitud del tallo`, y=`log Número de hojas`),method = lm)+
  #theme(element_text(`Número de colección plantas madres`), legend.position = "left")+
  geom_point(data=mean.phenodata.progenie.selected.log, aes(x=`log Longitud del tallo`, y=`log Número de hojas`))+
  geom_text(data=mean.phenodata.progenie.selected.log, aes(x=`log Longitud del tallo`, y=`log Número de hojas`,
                                                           label=`Número de colección plantas madres`), cex=2.5, vjust=1.5)
cor(x=phenodata.progenie.selected.log$`log Longitud del tallo`, y=phenodata.progenie.selected.log$`log Número de hojas`)# 0.5605965
lm(`log Número de hojas`~`log Longitud del tallo`, data=phenodata.progenie.selected.log)


###################################################################################################################
###################################################################################################################
# 4) Ajuste de modelos de mezclas normales
###################################################################################################################
###################################################################################################################
# subset con sólo los rasgos fenotípicos
data.for.GMM<-mean.phenodata.progenie.selected.log[,2:3]

###################################################################################################################
# 4.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

#"PCS"
mclust.options(hcUse="PCS") 
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de lso especímenes
Mcluster.phenodata.progenie$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 2
# components: 
#   
#   log-likelihood  n   df      BIC       ICL
#   7.604783        37  9     -17.2887 -18.08209
# 
# Clustering table:
#   1  2 
#    2 35

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what="BIC")

#"VARS"
mclust.options(hcUse="VARS") 
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #classifiación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 2
# components: 
#   
#   log-likelihood  n df      BIC       ICL
#   7.604783       37  9    -17.2887 -18.08209
# 
# Clustering table:
#   1  2 
#   2 35 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what="BIC")

#"STD"
mclust.options(hcUse="STD") 
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)#
Mcluster.phenodata.progenie$classification #clasificación de los especíemenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components: 
#   
#   log-likelihood  n df       BIC       ICL
#   7.058085       37 8       -14.77117 -14.77144
# 
# Clustering table:
#   1  2 
#  36  1 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what="BIC")

#"SPH"
mclust.options(hcUse="SPH") 
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 2
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 7.604763          37 9  -17.28874   -18.09695
# 
# Clustering table:
#    1  2 
#   35  2 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what="BIC")

#"PCR"
mclust.options(hcUse="PCR") 
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificacion de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components: 
#   
#   log-likelihood  n df       BIC       ICL
#     7.058085      37  8    -14.77117 -14.77144
# 
# Clustering table:
#   1  2 
#   36  1

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what="BIC")

#"SDV"
mclust.options(hcUse="SDV") 
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de os especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 2
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 7.604768 37  9 -17.28872 -18.09609
# 
# Clustering table:
#   1  2 
#   35  2 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what="BIC")


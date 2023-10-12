#################################################################################################################
#################################################################################################################
#################################################################################################################
#INTRODUCCIÓN####
#
# A partir de proporciones de supervivencias de la progenie por planta madre para tres ocasiones (marzo y octubre
#de 2020 y junio de 2023, se realiza un agrupamiento morfológica de las plantas madre según superviviencia de su
#progenie

#REQUERIMIENTOS
#Tabla de datos de la supervivencia de las medidas tomadas en marzo y octubre de 202 y junio de 2023:
#     "supervivencia_piloto_2023agosto22_082700.csv"
#
#CONTENIDO
# 1) Datos preliminares.
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
#################################################################################################################
#################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mexclas normales
library(ellipse)
library(tidyverse)
#################################################################################################################
# 1.2) lectura de los datos de supervivencia de las plantas de la progenie
#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
#
#leer las tablas de datos, examinar y resumir los datos
supervivencia <-
  read.table(
    "supervivencia_piloto_2023agosto22_082700.csv",
    header = T,
    sep = ","
  )
head(supervivencia)
colnames(supervivencia)
dim(supervivencia)#250 plantas de la progenie
View(supervivencia)

#################################################################################################################
#################################################################################################################
# 2) crear tabla con número de sobrevivientes por planta madre
#################################################################################################################
#################################################################################################################
#
#################################################################################################################
# 2.1)Remover plantas madres sin número de colección
supervivencia$Número.colección.planta.madre <-
  as.numeric(supervivencia$Número.colección.planta.madre)
supervivencia <-
  supervivencia[!is.na(supervivencia$Número.colección.planta.madre), ]
dim(supervivencia) # 248 plantas de la progenie con madre asignada
#
#################################################################################################################
# 2.2) Crear tabla
vivas.supervivencia.1 <- as.data.frame(table(supervivencia[, c(1, 6)]))
vivas.supervivencia.1 <-
  vivas.supervivencia.1[vivas.supervivencia.1$supervivencia.1 == "V", c(1, 3)]
vivas.supervivencia.2 <- as.data.frame(table(supervivencia[, c(1, 8)]))
vivas.supervivencia.2 <-
  vivas.supervivencia.2[vivas.supervivencia.2$supervivencia.2 == "V", c(1, 3)]
vivas.supervivencia.3 <- as.data.frame(table(supervivencia[, c(1, 10)]))
vivas.supervivencia.3 <-
  vivas.supervivencia.3[vivas.supervivencia.3$supervivencia.3 == "V", c(1, 3)]

sobrevivientes<-data.frame(vivas.supervivencia.1,
                           vivas.supervivencia.2[,2],
                           vivas.supervivencia.3[,2])
View(sobrevivientes)
colnames(sobrevivientes) <-
  c(
    "Collector.Collection.Number",
    "vivas.1",
    "vivas.2",
    "vivas.3"
  )
#Agregar las plantas madre que no nacieron ninguna hija mediante la tabla de asignación de grupos de las plantas madre
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350, ]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)
#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))
sobrevivientes.all<-
  merge(x=phenotypic.group.assignment.madres[,c(2,3)],
      y=sobrevivientes, 
      all=T, row.names=NULL)
sobrevivientes.all[is.na(sobrevivientes.all)]<-0
sobrevivientes.all<-sobrevivientes.all[-2]
#Guardar tabla de datos
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# save(sobrevivientes.all, file=paste("sobrevivientes_",
#                                                       format(Sys.time(),"%Y%B%d_%H%M%S"), ".RData", sep=""))
# #################################################################################################################
# 2.3) Transfomación de los datos.
#seleccionar sólo datos de sobrevivencia
sobrevivientes.all.selected<-sobrevivientes.all[-1]
sobrevivientes.all.selected.log<-log(sobrevivientes.all.selected+1)
colnames(sobrevivientes.all.selected.log)<-paste("log(",colnames(sobrevivientes.all.selected),"+1)")
###################################################################################################################
###################################################################################################################
# 3) Ajuste de modelos de mezclas normales
###################################################################################################################
###################################################################################################################

###################################################################################################################
#3.1) Sobrevivientes 11 meses DDS

#subset con sólo las proporciones de supervivencia
data.for.GMM <- sobrevivientes.all.selected.log[,1]
###################################################################################################################
#3.1.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"
#
#"PCS"####
mclust.options(hcUse = "PCS")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 2 components: 
#   
#   log-likelihood  n df       BIC       ICL
#   -54.31204        43  4 -123.6689 -128.2803
# 
# Clustering table:
#   1  2 
# 18 25
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"SPH"####
mclust.options(hcUse = "SPH")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 2 components: 

# log-likelihood  n df       BIC       ICL
# -54.31204       43  4 -123.6689 -128.2803
# 
# Clustering table:
#   1  2 
#   18 25 
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"PCR"####
mclust.options(hcUse = "PCR")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 2 components: 

# log-likelihood  n df       BIC       ICL
# -54.31204       43  4 -123.6689 -128.2803
# 
# Clustering table:
#   1  2 
#   18 25
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"SVD"####
mclust.options(hcUse = "SVD")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 2 components: 
# 
# log-likelihood  n df       BIC       ICL
# -54.31204       43  4 -123.6689 -128.2803
# 
# Clustering table:
#   1  2 
#  18 25 
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 2 components: 

# log-likelihood  n df       BIC       ICL
# -54.31204       43  4 -123.6689 -128.2803
# 
# Clustering table:
#   1  2 
#   18 25  
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.sobreviviencia, file = "Mcluster.sobreviviencia(1)_2023October12.RData")
#load("Mcluster.sobreviviencia(1)_2023October12.RData")
#"STD"####
mclust.options(hcUse = "STD")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
#Mclust E (univariate, equal variance) model with 2 components: 

# log-likelihood  n df       BIC       ICL
# -54.31204       43  4 -123.6689 -128.2803
# 
# Clustering table:
#   1  2 
#   18 25  
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
###################################################################################################################
#3.2) Sobrevivientes un año y seis meses DDS

#subset con sólo las proporciones de supervivencia
data.for.GMM <- sobrevivientes.all.selected.log[,2]
###################################################################################################################
#3.2.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"
#
#"PCS"####
mclust.options(hcUse = "PCS")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 8 components: 

# log-likelihood  n df      BIC       ICL
# -27.98738       43 16   -116.154 -116.2036
# 
# Clustering table:
#   1 2 3 4 5 6 7 8 
#   8 8 3 5 6 6 5 2
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"SPH"####
mclust.options(hcUse = "SPH")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 8 components: 

# log-likelihood  n df      BIC       ICL
#    -27.98738    43 16 -116.154 -116.2036
# 
# Clustering table:
#   1 2 3 4 5 6 7 8 
#   8 8 3 5 6 6 5 2
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"PCR"####
mclust.options(hcUse = "PCR")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 8 components: 

# log-likelihood  n df      BIC       ICL
#   -27.98738     43 16   -116.154 -116.2036
# 
# Clustering table:
#   1 2 3 4 5 6 7 8 
#    8 8 3 5 6 6 5 2 
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"SVD"####
mclust.options(hcUse = "SVD")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 8 components: 

# log-likelihood  n df      BIC       ICL
# -27.98738       43 16 -116.154 -116.2036
# 
# Clustering table:
#   1 2 3 4 5 6 7 8 
#   8 8 3 5 6 6 5 2  
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 8 components: 

# log-likelihood  n df      BIC       ICL
#   -27.98738     43 16 -116.154 -116.2036
# 
# Clustering table:
#   1 2 3 4 5 6 7 8 
#   8 8 3 5 6 6 5 2  
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.sobreviviencia, file = "Mcluster.sobreviviencia(2)_2023October12.RData")
#load("Mcluster.sobreviviencia(2)_2023October12.RData")
#"STD"####
mclust.options(hcUse = "STD")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust E (univariate, equal variance) model with 8 components: 
#   
#   log-likelihood  n df      BIC       ICL
#  -27.98738        43 16 -116.154 -116.2036
# 
# Clustering table:
#   1 2 3 4 5 6 7 8 
#   8 8 3 5 6 6 5 2
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
###################################################################################################################
#3.3) Sobrevivientes cuatro años y dos meses DDS

#subset con sólo las proporciones de supervivencia
data.for.GMM <- sobrevivientes.all.selected.log[,3]
###################################################################################################################
#3.3.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"
#
#"PCS"####
mclust.options(hcUse = "PCS")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust X (univariate normal) model with 1 component: 

# log-likelihood  n df       BIC       ICL
#   -57.30554     43  2   -122.1335 -122.1335
# 
# Clustering table:
#   1 
#   43 
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"SPH"####
mclust.options(hcUse = "SPH")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust X (univariate normal) model with 1 component: 

# log-likelihood  n df       BIC       ICL
# -57.30554       43  2   -122.1335 -122.1335
# 
# Clustering table:
#   1 
# 43
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"PCR"####
mclust.options(hcUse = "PCR")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
#Mclust X (univariate normal) model with 1 component: 

# log-likelihood  n df       BIC       ICL
# -57.30554       43  2   -122.1335 -122.1335
# 
# Clustering table:
#   1 
#   43
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"SVD"####
mclust.options(hcUse = "SVD")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
#Mclust X (univariate normal) model with 1 component: 

# log-likelihood  n df       BIC       ICL
# -57.30554       43  2 -122.1335 -122.1335
# 
# Clustering table:
#   1 
#   43   
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
#Mclust X (univariate normal) model with 1 component: 

# log-likelihood  n df       BIC       ICL
# -57.30554      43  2 -122.1335 -122.1335
# 
# Clustering table:
#   1 
# 43 
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.sobreviviencia, file = "Mcluster.sobreviviencia(3)_2023October12.RData")
load("Mcluster.sobreviviencia(3)_2023October12.RData")
#"STD"####
mclust.options(hcUse = "STD")
Mcluster.sobreviviencia<- Mclust(data.for.GMM)
#examinando resultados
Mcluster.sobreviviencia
summary(Mcluster.sobreviviencia)
names(Mcluster.sobreviviencia$classification)
Mcluster.sobreviviencia$classification #clasificación de lso especímenes
Mcluster.sobreviviencia$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.sobreviviencia)
# Mclust X (univariate normal) model with 1 component: 

# log-likelihood  n df       BIC       ICL
# -57.30554       43  2 -122.1335 -122.1335
# 
# Clustering table:
#   1 
# 43
# #
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.sobreviviencia,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.sobreviviencia, what = "BIC")
#
###################################################################################################################
###################################################################################################################
# 4) Examinar los grupos de las plantas madres según sobrevivencia de su progeie en base al mejor modelo de mezclas normales, de accuerdo con las variables de
#inicialización
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
#4.1) 11 meses DDS
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.sobreviviencia(1)_2023October12.RData")#modelos VARS
load("sobrevivientes_2023octubre12_064200.RData")
#################################################################################################################
# 4.1.1)Examinar y guardar en un documento la asignación de grupos de las plantas madre de acuerdo con la sobrevivencia

phenotypic.group.assignment.sobrevivencia.1<-
  data.frame(
    as.numeric(rownames(sobrevivientes.all)),
    sobrevivientes.all[,1],
    Mcluster.sobreviviencia$classification,
    Mcluster.sobreviviencia$uncertainty
  )
colnames(phenotypic.group.assignment.sobrevivencia.1)<-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.sobrevivencia.1)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(
  phenotypic.group.assignment.sobrevivencia.1,
  file = paste(
    "PhenotypicGroupAssignment.sobrevivencia.1_",
    format(Sys.time(), "%Y%B%d_%H%M%S"),
    ".csv",
    sep = ""
  ),
  row.names = F
)
###################################################################################################################
###################################################################################################################
#4.2)  un año y seis meses DDS
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.sobreviviencia(2)_2023October12.RData")#modelos VARS
load("sobrevivientes_2023octubre12_064200.RData")
#################################################################################################################
# 4.2.1)Examinar y guardar en un documento la asignación de grupos de las plantas madre de acuerdo con la sobrevivencia

phenotypic.group.assignment.sobrevivencia.2<-
  data.frame(
    as.numeric(rownames(sobrevivientes.all)),
    sobrevivientes.all[,1],
    Mcluster.sobreviviencia$classification,
    Mcluster.sobreviviencia$uncertainty
  )
colnames(phenotypic.group.assignment.sobrevivencia.2)<-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.sobrevivencia.2)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(
  phenotypic.group.assignment.sobrevivencia.2,
  file = paste(
    "PhenotypicGroupAssignment.sobrevivencia.2_",
    format(Sys.time(), "%Y%B%d_%H%M%S"),
    ".csv",
    sep = ""
  ),
  row.names = F
)
###################################################################################################################
###################################################################################################################
#4.3)  cuatro años y dos meses DDS
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.sobreviviencia(3)_2023October12.RData")#modelos VARS
load("sobrevivientes_2023octubre12_064200.RData")
#################################################################################################################
# 4.3.1)Examinar y guardar en un documento la asignación de grupos de las plantas madre de acuerdo con la sobrevivencia

phenotypic.group.assignment.sobrevivencia.3<-
  data.frame(
    as.numeric(rownames(sobrevivientes.all)),
    sobrevivientes.all[,1],
    Mcluster.sobreviviencia$classification,
    Mcluster.sobreviviencia$uncertainty
  )
colnames(phenotypic.group.assignment.sobrevivencia.3)<-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.sobrevivencia.3)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(
  phenotypic.group.assignment.sobrevivencia.3,
  file = paste(
    "PhenotypicGroupAssignment.sobrevivencia.3_",
    format(Sys.time(), "%Y%B%d_%H%M%S"),
    ".csv",
    sep = ""
  ),
  row.names = F
)
###################################################################################################################
###################################################################################################################
#4.4) Tabulaciones cruzada entre la agrupación de las plantas madres según sobrevivencia para cada ocasión de medida 

#4.4.1) 11 meses DDS y 1 año y 2 meses DDS
tapply(phenotypic.group.assignment.sobrevivencia.1$Collector.Collection.Number,
       phenotypic.group.assignment.sobrevivencia.1$Phenotypic.Group,
       length)
# 1  2 
# 18 25 
tapply(phenotypic.group.assignment.sobrevivencia.2$Collector.Collection.Number,
       phenotypic.group.assignment.sobrevivencia.2$Phenotypic.Group,
       length)
# 1 2 3 4 5 6 7 8 
# 8 8 3 5 6 6 5 2 

table(phenotypic.group.assignment.sobrevivencia.1[,3],
      phenotypic.group.assignment.sobrevivencia.2[,3])

#   1 2 3 4 5 6 7 8
# 1 8 8 2 0 0 0 0 0
# 2 0 0 1 5 6 6 5 2

#4.4.2) 11 meses DDS y 4 años DDS
tapply(phenotypic.group.assignment.sobrevivencia.1$Collector.Collection.Number,
       phenotypic.group.assignment.sobrevivencia.1$Phenotypic.Group,
       length)
# 1  2 
# 18 25 
tapply(phenotypic.group.assignment.sobrevivencia.3$Collector.Collection.Number,
       phenotypic.group.assignment.sobrevivencia.3$Phenotypic.Group,
       length)
#  1 
# 43 

table(phenotypic.group.assignment.sobrevivencia.1[,3],
      phenotypic.group.assignment.sobrevivencia.3[,3])

#    1
# 1 18
# 2 25

#4.4.3) 1 año y 2 meses y 4 años DDS
tapply(phenotypic.group.assignment.sobrevivencia.2$Collector.Collection.Number,
       phenotypic.group.assignment.sobrevivencia.2$Phenotypic.Group,
       length)
# 1 2 3 4 5 6 7 8 
# 8 8 3 5 6 6 5 2  
tapply(phenotypic.group.assignment.sobrevivencia.3$Collector.Collection.Number,
       phenotypic.group.assignment.sobrevivencia.3$Phenotypic.Group,
       length)
#  1 
# 43 

table(phenotypic.group.assignment.sobrevivencia.2[,3],
      phenotypic.group.assignment.sobrevivencia.3[,3])

#   1
# 1 8
# 2 8
# 3 3
# 4 5
# 5 6
# 6 6
# 7 5
# 8 2
###################################################################################################################
###################################################################################################################
#4.5) Tabulaciones cruzada entre la agrupación de las plantas madres según sobrevivencia para cada ocasión de medida y agruaión de plantas madres segun morfología

#4.5.1) 11 meses DDS y morfología

phenotypic.group.assignment.madres.progenie <-
  merge(
    phenotypic.group.assignment.madres[, c(2, 6)],
    phenotypic.group.assignment.sobrevivencia.1[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie"))

table(phenotypic.group.assignment.madres.progenie[, c(2, 3)])
#                        Phenotypic.Group.progenie
# Phenotypic.Group.madres  1  2
                      # 2  2  4
                      # 3  7 11
                      # 4  2  3
                      # 5  7  7

#4.5.2) 1 año y dos meses DDS y morfología

phenotypic.group.assignment.madres.progenie <-
  merge(
    phenotypic.group.assignment.madres[, c(2, 6)],
    phenotypic.group.assignment.sobrevivencia.2[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie"))

table(phenotypic.group.assignment.madres.progenie[, c(2, 3)])
#                        Phenotypic.Group.progenie
#   Phenotypic.Group.madres 1 2 3 4 5 6 7 8
#                         2 1 1 0 0 2 1 1 0
#                         3 3 2 2 3 4 2 2 0
#                         4 0 2 0 1 0 0 1 1
#                         5 4 3 1 1 0 3 1 1


#4.5.3) 4 añoss DDS y morfología

phenotypic.group.assignment.madres.progenie <-
  merge(
    phenotypic.group.assignment.madres[, c(2, 6)],
    phenotypic.group.assignment.sobrevivencia.3[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie"))

table(phenotypic.group.assignment.madres.progenie[, c(2, 3)])
#                         Phenotypic.Group.progenie
# Phenotypic.Group.madres  1
                      # 2  6
                      # 3 18
                      # 4  5
                      # 5 14


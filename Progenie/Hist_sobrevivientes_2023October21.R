#################################################################################################################
#################################################################################################################
#################################################################################################################
#INTRODUCCIÓN####
#REQUERIMIENTOS
#Tabla de datos de la supervivencia de las medidas tomadas en marzo y octubre de 202 y junio de 2023:
#     "supervivencia_piloto_2023agosto22_082700.csv"
#
#CONTENIDO
# 1) Datos preliminares.
# 2) crear tabla con número de sobrevivientes por planta madre
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
#################################################################################################################
#################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos####
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
# 2) crear tabla con número de sobrevivientes por planta madre####
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
# 3) Examinar datos de sobrevivencia.
###################################################################################################################
###################################################################################################################

range(sobrevivientes.all$vivas.1)
hist(sobrevivientes.all$vivas.1,
     breaks=seq(-0.5,24.5,1),
     xlab=NA,
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,10)
)
title(expression("A) 11 meses DDS"),adj=0)
axis(side=1, at=1:25, labels = F, tcl=-0.3)
axis(side=2, at=1:10, labels = F, tcl=-0.3)


hist(sobrevivientes.all$vivas.2,
     breaks=seq(-0.5,24.5,1),
     xlab=NA,
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,10)
)
title(expression("B) un año y seis meses DDS"), adj=0)
axis(side=1, at=1:25, labels = F, tcl=-0.3)
axis(side=2, at=1:10, labels = F, tcl=-0.3)

hist(sobrevivientes.all$vivas.3,
     breaks=seq(-0.5,24.5,1),
     xlab="Número de plantas hijas",
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,10)
     )
title(expression("C) Cuatro años y dos meses DDS"), adj=0)
axis(side=1, at=1:25, labels = F, tcl=-0.3)
axis(side=2, at=1:10, labels = F, tcl=-0.3)

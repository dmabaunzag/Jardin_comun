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
# 2) Crear tabla con proporción de tasa de supervivencia.
# 3) Examinar tasa de supervivencia.
# 4)  Ajuste de modelos de mezclas normales.
# 5) Examinar los grupos morfológicos en base al mejor modelo de mezclas normales, de accuerdo con las variables
#de inicialización
#
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
dim(supervivencia) #250 plantas de la progenie
#
#################################################################################################################
#################################################################################################################
# 2) crear tabla con proporción de tasa de supervivencia
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
# 2.2) Crear tabla de proporción de supervivencia
nacimiento <- as.data.frame(table(supervivencia[, 1]))
vivas.supervivencia.1 <- as.data.frame(table(supervivencia[, c(1, 6)]))
vivas.supervivencia.1 <-
  vivas.supervivencia.1[vivas.supervivencia.1$supervivencia.1 == "V", c(1, 3)]
vivas.supervivencia.2 <- as.data.frame(table(supervivencia[, c(1, 8)]))
vivas.supervivencia.2 <-
  vivas.supervivencia.2[vivas.supervivencia.2$supervivencia.2 == "V", c(1, 3)]
vivas.supervivencia.3 <- as.data.frame(table(supervivencia[, c(1, 10)]))
vivas.supervivencia.3 <-
  vivas.supervivencia.3[vivas.supervivencia.3$supervivencia.3 == "V", c(1, 3)]
#
proporcion.supervivencia <- data.frame(
  nacimiento[, 1],
  vivas.supervivencia.1[, 2] / nacimiento[, 2],
  vivas.supervivencia.2[, 2] / nacimiento[, 2],
  vivas.supervivencia.3[, 2] / nacimiento[, 2]
)
colnames(proporcion.supervivencia) <-
  c(
    "Número.colección.planta.madre",
    "proporcion.supervivencia.1",
    "proporcion.supervivencia.2",
    "proporcion.supervivencia.3"
  )
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
#save(proporcion.supervivencia, file=paste("proporcion.supervivencia_",
#                                                      format(Sys.time(),"%Y%B%d_%H%M%S"), ".RData", sep=""))
#load("proporcion.supervivencia_2023septiembre04_095936.RData")
#
###################################################################################################################
###################################################################################################################
# 3) Examinar tasa de supervivencia
###################################################################################################################
###################################################################################################################
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")
#pdf("Figuras_supervivencia.pdf")
summary(proporcion.supervivencia)
dim(proporcion.supervivencia) #37 plantas madres representadas
proporcion.supervivencia$Número.colección.planta.madre <-
  as.factor(proporcion.supervivencia$Número.colección.planta.madre)
sapply(proporcion.supervivencia[, c(2:4)], sd)
# proporcion.supervivencia.1 proporcion.supervivencia.2 proporcion.supervivencia.3
# 0.9509240                  0.8703254                  0.6488025
barplot(sapply(proporcion.supervivencia[, c(2:4)], mean),
        ylim=(c(0,1)),
        ylab = "promedio proporción supervivencia")
#
barplot(
  cbind(
    proporcion.supervivencia.1,
    proporcion.supervivencia.2,
    proporcion.supervivencia.3
  ) ~ Número.colección.planta.madre,
  data = proporcion.supervivencia,
  beside = T,
  las = 2,
  ylab = "proporción de supervivencia",
  col = c("lightblue", "mistyrose", "lightcyan"),
  legend.text = c("supervivencia 1", "supervivencia 2", "Supervivencia 3"),
  args.legend = list(
    x = "top",
    bty = "n",
    horiz = TRUE,
    inset = -0.15
  )
)

###################################################################################################################
###################################################################################################################
# 4) Ajuste de modelos de mezclas normales
###################################################################################################################
###################################################################################################################
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
#load("proporcion.supervivencia_2023septiembre04_095936.RData")
#subset con sólo las proporciones de supervivencia
data.for.GMM <- proporcion.supervivencia[, 2:4]
###################################################################################################################
# 4.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"
#
#"PCS"
mclust.options(hcUse = "PCS")
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificación de lso especímenes
Mcluster.supervivencia.piloto$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 5 components:
#
#   log-likelihood  n   df      BIC      ICL
#         200.8311 37   37      268.0583 255.7391
#
# Clustering table:
#   1  2  3  4  5
#   3  3  5 24  2

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empiríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what = "BIC")
#
#"VARS"
mclust.options(hcUse = "VARS")
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #classifiación de los especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 5 components:
#
#   log-likelihood  n   df      BIC      ICL
#   200.8311        37  37  268.0583   255.7391
#
# Clustering table:
#   1  2  3  4  5
#   3  3  5 24  2
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what = "BIC")
#
#"STD"
mclust.options(hcUse = "STD")
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)#
Mcluster.supervivencia.piloto$classification #clasificación de los especíemenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 5 components:
#
#   log-likelihood  n   df      BIC      ICL
#   200.8311        37  37    268.0583   255.7391
#
# Clustering table:
#  1  2  3  4  5
#  3  3  5 24  2
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what = "BIC")
#
#"SPH"
mclust.options(hcUse = "SPH")
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificación de los especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 8 components:
#
#   log-likelihood  n df      BIC      ICL
#   265.2543        37 58   321.0754 309.1267
#
# Clustering table:
#   1  2  3  4  5  6  7  8
#   1  6  3  9 10  4  3  1
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what = "BIC")
#
#"PCR"
mclust.options(hcUse = "PCR")
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificacion de los especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 4 components:
#
#   log-likelihood  n   df      BIC       ICL
#   213.1409       37    30    317.9543   317.6814
#
# Clustering table:
#   1  2  3  4
#   3  5  3 26
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what = "BIC")
#
#"SVD"
mclust.options(hcUse = "SVD")
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificación de os especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#save(Mcluster.supervivencia.piloto, file = "Mcluster.supervivencia.piloto.SVD_2023September04.RData")
#load("Mcluster.supervivencia.piloto.SVD_2023September04.RData")
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 4 components:
#
#   log-likelihood  n df      BIC    ICL
#   221.3889        37 30   334.4502 334.31
#
# Clustering table:
#   1  2  3  4
#   2 25  4  6
#
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what = "BIC")
#
#dev.off()
#
###################################################################################################################
###################################################################################################################
# 5) Examinar los grupos morfológicos en base al mejor modelo de mezclas normales, de accuerdo con las variables de
#inicialización
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
# 5.1)SVD
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.supervivencia.piloto.SVD_2023September04.RData")#modelos VARS
load("proporcion.supervivencia_2023septiembre04_095936.RData")
#################################################################################################################
# 5.1.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.supervivencia <-
  data.frame(
    as.numeric(rownames(proporcion.supervivencia)),
    proporcion.supervivencia[, 1],
    Mcluster.supervivencia.piloto$classification,
    Mcluster.supervivencia.piloto$uncertainty
  )
colnames(phenotypic.group.assignment.supervivencia) <-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.supervivencia)
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
#write.csv(
#   phenotypic.group.assignment.supervivencia,
#   file = paste(
#     "PhenotypicGroupAssignment.supervivencia.SVD_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )
###################################################################################################################
# 5.1.2) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
# pdf("Figuras(supervivencia)_sección_5.1(SVD).pdf")

#Supervivencia.1 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.supervivencia.piloto,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  xlab=NA,
  ylab = "supervivencia 2",
  addEllipses = F,
  asp=1
)
# legend(
#   "bottomright",
#   paste("M", 1:4),
#   col = mclust.options("classPlotColors"),
#   xpd = T,
#   ncol = 2,
#   pch = mclust.options("classPlotSymbols"),
#   pt.lwd = 0.8,
#   pt.cex = 0.8,
#   cex = 0.8,
#   bty = "o"
# )
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G) {
  points(
    ellipse(
      x = Mcluster.supervivencia.piloto$parameters$variance$sigma[c(1,2), c(1,2), i],
      centre = Mcluster.supervivencia.piloto$parameters$mean[c(1, 2), i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
#agregar etiquetas de las elipses
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][1,1], 
     Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][2,1], "S1", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][1,2]+0.1,
     Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][2,2], "S2", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][1,3]+0.1,
     Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][2,3], "S3", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][1,4]-0.1,
     Mcluster.supervivencia.piloto$parameters$mean[c(1,2),][2,4], "S4", cex=0.8)
title("A)", adj=0)
#
#Supervivencia.1 vs supervivencia.3
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.supervivencia.piloto,
  what = c("classification"),
  dimens = c(1, 3),
  xlab="Supervivencia 1",
  ylab="Supervivencia 3",
  main = "",
  addEllipses = F,
  asp=1
)
legend(
  "bottomright",
  paste("S", 1:4),
  col = mclust.options("classPlotColors"),
  xpd = T,
  ncol = 2,
  pch = mclust.options("classPlotSymbols"),
  pt.lwd = 0.8,
  pt.cex = 0.8,
  cex = 0.8,
  bty = "o"
)
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G) {
  points(
    ellipse(
      x = Mcluster.supervivencia.piloto$parameters$variance$sigma[c(1,3), c(1,3), i],
      centre = Mcluster.supervivencia.piloto$parameters$mean[c(1, 3), i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
#agregar etiquetas de las elipses
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][1,1], 
     Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][2,1], "S1", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][1,2]+0.1, 
     Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][2,2], "S2", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][1,3]+0.1,
     Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][2,3], "S3", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][1,4]-0.1, 
     Mcluster.supervivencia.piloto$parameters$mean[c(1,3),][2,4], "S4", cex=0.8)
title("B)", adj=0)
#
#Supervivencia.3 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.supervivencia.piloto,
  what = c("classification"),
  dimens = c(3, 2),
  xlab="Supervivencia 3",
  ylab="supervivencia 2",
  main = "",
  addEllipses = F,
  asp=1
)
legend(
  "bottomright",
  paste("S", 1:4),
  col = mclust.options("classPlotColors"),
  xpd = T,
  ncol = 2,
  pch = mclust.options("classPlotSymbols"),
  pt.lwd = 0.8,
  pt.cex = 0.8,
  cex = 0.8,
  bty = "o"
)
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G) {
  points(
    ellipse(
      x = Mcluster.supervivencia.piloto$parameters$variance$sigma[c(3,2), c(3,2), i],
      centre = Mcluster.supervivencia.piloto$parameters$mean[c(3, 2), i],
      level = pchisq(1,2)
    ),
    type = "l",
    col = "black"
  )
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
text(Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][1,1]+0.1,
     Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][2,1], "S1", cex= 0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][1,2]+0.2,
     Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][2,2], "S2", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][1,3]-0.1, 
     Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][2,3], "S3", cex=0.8)
text(Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][1,4]+0.1,
     Mcluster.supervivencia.piloto$parameters$mean[c(3,2),][2,4], "S4", cex=0.8)

###################################################################################################################
# 5.1.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.supervivencia.piloto$uncertainty)

#De los 37 especímenes, ninguna de éstos tuvieron incertidumbre mayor a 0.1
sum(Mcluster.supervivencia.piloto$uncertainty > 0.1)
sum(Mcluster.supervivencia.piloto$uncertainty > 0.1) / length(Mcluster.supervivencia.piloto$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.supervivencia.piloto$classification[Mcluster.supervivencia.piloto$uncertainty > 0.1]
##################################################################################################################
#5.1.4) Examinar la tabulación cruzada de los grupos morfolóficos según las plantas madre del piloto y grupos
#morfológicos según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.
#
#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento
phenotypic.group.assignment.progenie <-
  read.table(
    "PhenotypicGroupAssignment.supervivencia.SVD_2023septiembre04_203727.csv",
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
phenotypic.group.assignment.madres$Phenotypic.Group <-
  as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(
  phenotypic.group.assignment.madres$Collector.Collection.Number,
  phenotypic.group.assignment.madres$Phenotypic.Group,
  length
)
# 2  3  4  5
# 6 18  5 14

tapply(
  phenotypic.group.assignment.progenie$Collector.Collection.Number,
  phenotypic.group.assignment.progenie$Phenotypic.Group,
  length
)
#  1  2  3  4
#  2 25  4  6

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
phenotypic.group.assignment.madres.progenie <-
  merge(
    phenotypic.group.assignment.madres[, c(2, 6)],
    phenotypic.group.assignment.progenie[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie")
  )


phenotypic.group.crosstab <-
  table(phenotypic.group.assignment.madres.progenie[, c(2, 3)])
phenotypic.group.crosstab
#                        Phenotypic.Group.progenie
# Phenotypic.Group.madres 1 2 3 4
                      # 2 0 5 1 0
                      # 3 0 9 2 4
                      # 4 1 3 0 1
                      # 5 1 8 1 1
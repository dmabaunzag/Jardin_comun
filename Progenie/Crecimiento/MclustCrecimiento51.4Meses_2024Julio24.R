###################################################################################################################
###################################################################################################################
###################################################################################################################

#####MODELO DE MEZCLAS NORMALES PARA CRECIMIENTO DE LA PROGENIE DE LAS PLANTAS MADRE: 51.4 meses#####

###################################################################################################################
###################################################################################################################
###################################################################################################################
#
#INTRODUCCIÓN:

#Los datos de crecimiento de la progenie (cantidad de hojas y longitud del tallo) fueron usados para asignar
#grupos morfológico de las plantas madre según e crecimiento de su progenie. Realizamos análisis separados para
#cada uno de los tres muestreos (11.4 meses, 19.6 meses y 51.4 meses después de la siembra). En cada uno de estos
#análisis, primero se obtuvo la media del logaritmo natural de la longitud del tallo y del número de hojas para
#la progenie de cada planta madre. Luego, se analizaron con modelos de mezclas bivariadas normales con el paquete
#estadístico de R mclust v. 6.0.0 (Fraley et al., 2024) para estimar de novo los grupos conformados por las
#plantas madre según el crecimiento de su progenie en el jardín común.

#REQUERIMIENTOS##
#"PhenotypicDataProgeny_Quebradas_2020Marzo.csv": datos de crecimiento hechos en junio de 2023

# "PhenotypicDataProgeny_Quebradas_2023Junio.csv"

#CONTENIDO##
# 1) Preliminares: cargar las librerías y lectura de datos

# 2) Examinar variables: editar los datos y transformarlos

# 3) Ajuste de modelos de mezclas normales

# 4) Examinar los grupos de crecimiento en base al mejor modelo de mezclas normales, de acuerdo con las variables
#de inicialización

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
# 1) Preliminares: cargar las librerías y lectura de datos
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 1.1) Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mezclas normales
library(ellipse)
library(ggplot2)

###################################################################################################################
# 1.2) Lectura de datos

#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

# Leer la tabla de datos: datos de la progenie tomados en Quebradas en 20/03/2020
phenodata.progenie <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2023Junio.csv",
    header = T,
    sep = ","
  )

View(phenodata.progenie)
summary(phenodata.progenie)
head(phenodata.progenie)
dim(phenodata.progenie)# 180 plantas del piloto con 29 variables

# Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
grupos.morfologicos <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )


###################################################################################################################
###################################################################################################################
# 2) Examinar variables: editar los datos y transformarlos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 2.1) Seleccionar variables de crecimiento

colnames(phenodata.progenie)#sólo se usará número de colección (2), longitud del tallo (9) y número de hojas (10)
phenodata.progenie.selected <- phenodata.progenie[, c(2, 9, 10)]
View(phenodata.progenie.selected)
colnames(phenodata.progenie.selected) <-
  c("Número de colección plantas madres",
    "Longitud del tallo",
    "Número de hojas")
dim(phenodata.progenie.selected) # 180 plantas de la progenie y 3 variables

#cambiar unidad de medida a cm
phenodata.progenie.selected[, 2] <- phenodata.progenie.selected[, 2] / 10

#unidades de medida de cada variables
measurement.units <- c(NA,  "cm", "count")
data.frame(colnames(phenodata.progenie.selected), measurement.units)

#################################################################################################################
# 2.2) Remover plantas de la progenie sin madre asignada o datos faltantes

# Cuántas plantas madre están representadas
unique(phenodata.progenie.selected$`Número de colección plantas madres`)## todos son números
length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))# 33 plantas madres representadas

# Excluir plantas de la progenie sin planta madre y que no tenga datos (excluir todos los NAs)
sapply(phenodata.progenie.selected, class)

phenodata.progenie.selected[, 1] <-
  as.numeric(phenodata.progenie.selected[, 1])

sapply(phenodata.progenie.selected, class)

rows.with.na <-
  unique(which(is.na(phenodata.progenie.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 6 plantas hija con NA

# Correr las siguientes líneas en caso de existir NAs
phenodata.progenie.selected <-
  phenodata.progenie.selected[-rows.with.na,]
dim(phenodata.progenie.selected) # 174 hijas con todos los datos
class(phenodata.progenie.selected)
summary(phenodata.progenie.selected)
head(phenodata.progenie.selected)

length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))## hay representación de 33 plantas madres
unique(phenodata.progenie.selected$`Número de colección plantas madres`)##cuáles

###################################################################################################################
#  2.3) Transformación de los datos.

# Dado que los rasgos fenotípicos frecuentemente siguen distribución log-normal se transforman a escala logarítmica

phenodata.progenie.selected.log <-
  data.frame(phenodata.progenie.selected[, 1],
             log(phenodata.progenie.selected[, 2:3]))
head(phenodata.progenie.selected.log)

# Editar el nombre de las variables
colnames(phenodata.progenie.selected.log) <-
  colnames(phenodata.progenie.selected)
colnames(phenodata.progenie.selected.log)[2:3] <-
  paste("log", paste(colnames(phenodata.progenie.selected)[2:3], sep = ""))
class(phenodata.progenie.selected.log)
summary(phenodata.progenie.selected.log)

###################################################################################################################
# 2.4) Promedio de las variables de crecimiento por planta madre

mean.phenodata.progenie.selected.log <-
  aggregate(
    phenodata.progenie.selected.log,
    by = list(phenodata.progenie.selected.log[, 1]),
    FUN = function(x)
      mean(x, na.rm = T)
  )
mean.phenodata.progenie.selected.log <-
  mean.phenodata.progenie.selected.log[, 2:4]
View(mean.phenodata.progenie.selected.log)

# Guardar la media de las cariables de crecimiento por planta madre
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# save(
#   mean.phenodata.progenie.selected.log,
#   file = paste(
#     "mean.phenodata.progenie.selected.log_(junio)_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".RData",
#     sep = ""
#   )
# )
#load("mean.phenodata.progenie.selected.log_(junio)_2023diciembre28_133842.RData")

###################################################################################################################
# 2.5) Graficar caracteres fenotípicos

# Histograma de la distribución de las variables de crecimiento en el promedio de las plantas madre
# Longitud del tallo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
load("mean.phenodata.progenie.selected.log_(junio)_2023diciembre28_133842.RData")
load("~/Jardin_comun/Especimenes/datos/Mcluster.phenodata_2023agosto19.RData")

# subconjunto de los especímenes de las plantas madres asignados a M3
phenotypic.group.assignment.madres <- 
  grupos.morfologicos[308:350,]

phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))
m3 <- as.numeric(phenotypic.group.assignment.madres$Collector.Collection.Number[phenotypic.group.assignment.madres$Phenotypic.Group==3])

mean.phenodata.progenie.selected.log.m3 <- 
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1] %in% m3[m3>1000],]
# Histograma de la Media log (longitud del tallo [cm])
par(mar = c(5, 5, 4, 1) + 0.1)
range(mean.phenodata.progenie.selected.log$`log Longitud del tallo`)
hist(
  mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
  breaks = seq(-1.4, 2.2, 0.2),
  ylim = c(0, 15),
  main = NA,
  xaxt = "n",
  yaxt = "n",
  xlab = "Media log (longitud del tallo [cm])",
  ylab = "Número de plantas madre",
  cex.lab = 1.5,
  col = "gray90"
)
title(expression("C) 51.4 meses"), adj = 0, cex.main =1.5)
axis(
  side = 1,
  at = seq(-1.4, 2.2, 0.2),
  labels = F,
  tcl = -0.5
)
axis(
  side = 1,
  at = seq(-1.4, 2.2, 0.6),
  labels = T,
  cex.axis =1.5,
  tcl = -0.7
)
axis(
  side = 1,
  at = seq(-1.4, 2.2, 0.6)[2],
  labels = T,
  cex.axis =1.5,
  tcl = F
)
axis(
  side = 2,
  at = 1:15,
  labels = F,
  tcl = -0.5
)
axis(
  side = 2,
  at = seq(0,15,3),
  labels = seq(0,15,3),
  cex.axis =1.5,
  las = 2,
  tcl = -0.7
)
hist(
  mean.phenodata.progenie.selected.log.m3$`log Longitud del tallo`,
  breaks = seq(-1.4, 2.2, 0.2),
  ylim = c(0, 15),
  main = NA,
  xaxt = "n",
  yaxt = "n",
  xlab = NA,
  ylab = NA,
  cex.lab = 1.5,
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)

# Histograma de la Media log (número de hojas)
par(mar = c(5, 5, 4, 1) + 0.1)
range(mean.phenodata.progenie.selected.log$`log Número de hojas`)# 1.386294 2.564949
hist(
  mean.phenodata.progenie.selected.log$`log Número de hojas`,
  breaks = seq(1, 3.4, 0.2),
  ylim = c(0, 15),
  main = NA,
  xlab = "Media log (número de hojas)",
  ylab = NA,
  xaxt = "n",
  yaxt = "n",
  cex.lab = 1.5
)
title(expression("F)"),  adj = 0, cex.main =1.5)
axis(
  side = 1,
  at = seq(1, 3.4, 0.2),
  labels = F,
  tcl = -0.5
)
axis(
  side = 1,
  at = seq(1, 3.4, 0.4),
  labels = T,
  cex.axis =1.5,
  tcl = -0.7
)

axis(
  side = 2,
  at = 1:15,
  labels = F,
  tcl = -0.5
)
axis(
  side = 2,
  at = seq(0,15,3),
  labels = seq(0,15,3),
  cex.axis =1.5,
  las = 2,
  tcl = -0.7
)

hist(
  mean.phenodata.progenie.selected.log.m3$`log Número de hojas`,
  breaks = seq(1, 3.4, 0.2),
  ylim = c(0, 15),
  main = NA,
  xlab = NA,
  ylab = NA,
  xaxt = "n",
  yaxt = "n",
  cex.lab = 1.5,
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)

 #################################################################################################################
#################################################################################################################
# 3) Ajuste de modelos de mezclas normales####
#################################################################################################################
#################################################################################################################

# Directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
load("mean.phenodata.progenie.selected.log_(junio)_2023diciembre28_133842.RData")

# Subconjunto con sólo los caracteres fenotípicos
data.for.GMM <- mean.phenodata.progenie.selected.log[, 2:3]

#################################################################################################################
# 3.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

#"PCS"####
mclust.options(hcUse = "PCS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de lso especímenes
Mcluster.phenodata.progenie$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust EVE (ellipsoidal, equal volume and orientation) model with 2 components: 
#   
#   log-likelihood  n df      BIC      ICL
# 22.07618 33  9 12.68378 11.49126
# 
# Clustering table:
#   1  2 
# 28  5

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#Examinar los resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# save(Mcluster.phenodata.progenie, file="Mcluster.phenodata.progenie.(junio).VARS_2023Diciembre28.RData")
# load("Mcluster.phenodata.progenie.(junio).VARS_2023Diciembre28.RData")
#
# Mclust EVE (ellipsoidal, equal volume and orientation) model with 2 components: 

# log-likelihood  n df      BIC      ICL
# 22.07618 33  9 12.68378 11.49126
# 
# Clustering table:
#   1  2 
# 28  5

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"STD"####
mclust.options(hcUse = "STD")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)#
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#Mclust EVE (ellipsoidal, equal volume and orientation) model with 2 components: 

# log-likelihood  n df      BIC      ICL
# 22.07618 33  9 12.68378 11.49126
# 
# Clustering table:
#   1  2 
# 28  5

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SPH"####
mclust.options(hcUse = "SPH")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinando resultados:
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust EVE (ellipsoidal, equal volume and orientation) model with 2 components: 
#   
#   log-likelihood  n df      BIC      ICL
# 22.07618 33  9 12.68379 11.48561
# 
# Clustering table:
#   1  2 
# 28  5

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"PCR"####
mclust.options(hcUse = "PCR")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificacion de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#
# Mclust EVE (ellipsoidal, equal volume and orientation) model with 2 components: 
# 
# log-likelihood  n df      BIC      ICL
# 22.07618 33  9 12.68379 11.48561
# 
# Clustering table:
#   1  2 
# 28  5

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SVD"####
mclust.options(hcUse = "SVD")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
# Resultados:
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de os especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust EVE (ellipsoidal, equal volume and orientation) model with 2 components: 
#   
#   log-likelihood  n df      BIC      ICL
# 22.07618 33  9 12.68379 11.48561
# 
# Clustering table:
#   1  2 
# 28  5

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#################################################################################################################
#################################################################################################################
# 4) Examinar los grupos fenotípicos en base al mejor modelo de mezclas normales, de acuerdo con las variables de
#inicialización
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1)VARS
# Cargar los mejores modelos de mezclas normales
# Directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
load("Mcluster.phenodata.progenie.(junio).VARS_2023Diciembre28.RData")#modelos VARS
load("mean.phenodata.progenie.selected.log_(junio)_2023diciembre28_133842.RData")

# 4.1.1)Examinar y guardar en un documento la asignación de grupos

# Crear y escribir archivo para la asignación de los grupos según crecimiento.
phenotypic.group.assignment.piloto <-
  data.frame(
    as.numeric(rownames(mean.phenodata.progenie.selected.log)),
    mean.phenodata.progenie.selected.log[, 1],
    Mcluster.phenodata.progenie$classification,
    Mcluster.phenodata.progenie$uncertainty
  )
colnames(phenotypic.group.assignment.piloto) <-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.piloto)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# write.csv(
#   phenotypic.group.assignment.piloto,
#   file = paste(
#     "grupos.crecimiento.(junio).VARS_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )
#################################################################################################################
# 4.1.2) Graficar grupos según crecimiento en el mejor modelo de mezclas normales.

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
load("Mcluster.phenodata.progenie.(junio).VARS_2023Diciembre28.RData")#modelos VARS
load("mean.phenodata.progenie.selected.log_(junio)_2023diciembre28_133842.RData")

#  Directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana

#Longitud del tallo vs número de hojas
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  addEllipses = F,
  asp = 1,
  xlab = "Media log (longitud del tallo [cm])",
  ylab = NA,
  cex.lab = 1.5
)
mtext(side = 2,
      "Media log (número de hojas)",
      line= 2.3,
      cex =1.5)
# legend(
#   "bottomright",
#   paste("C", 1:3),
#   col = mclust.options("classPlotColors"),
#   xpd = T,
#   ncol =3,
#   pch = mclust.options("classPlotSymbols"),
#   pt.lwd = 0.9,
#   pt.cex = 0.9,
#   cex = 0.9,
#   bty = "o"
# )
# Agregar elipses
for (i in 1:Mcluster.phenodata.progenie$G) {
  points(
    ellipse(
      x = Mcluster.phenodata.progenie$parameters$variance$sigma[1:2, 1:2, i],
      centre = Mcluster.phenodata.progenie$parameters$mean[c(1, 2), i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
# Agregar etiquetas de las elipses
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 1] - 0.5,
  Mcluster.phenodata.progenie$parameters$mean[2, 1],
  "C1",
  cex = 0.9
)
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 2]-0.4,
  Mcluster.phenodata.progenie$parameters$mean[2, 2],
  "C2",
  cex = 0.9
)
title(expression("E) 51.4 meses"), adj = 0)
#Identificando puntos
# identify(
#   mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
#   mean.phenodata.progenie.selected.log$`log Número de hojas`,
#   labels = as.character(
#     mean.phenodata.progenie.selected.log$`Número de colección plantas madres`
#   ),
#   cex=0.5
# )

#################################################################################################################
#Leyenda
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  addEllipses = F,
  asp = 1,
  xlab = NA,
  ylab = NA,
  cex.lab = 1.5,
  cex= 0,
  yaxt = "n",
  xaxt = "n",
  axes = F
)
legend(
  "center",
  paste("C", 1:3),
  col = mclust.options("classPlotColors"),
  pch = mclust.options("classPlotSymbols"),
  # pt.lwd = 0.9,
  # pt.cex = 0.9,
  # cex = 0.9,
  bty = "o"
)
title(expression("Grupos de crecimiento"))
#################################################################################################################



# 4.1.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata.progenie$uncertainty)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 1.000e-08 1.096e-04 1.663e-02 3.124e-03 1.959e-01

#De los 33 especímenes, dos de éstos tiene incertidumbre mayor a 0.1
sum(Mcluster.phenodata.progenie$uncertainty > 0.1)# 6%
sum(Mcluster.phenodata.progenie$uncertainty > 0.1) / length(Mcluster.phenodata.progenie$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata.progenie$classification[Mcluster.phenodata.progenie$uncertainty >
                                             0.1]
# 1 1

#################################################################################################################
# 4.1.4) Graficar soporte empírico para el mejor modelo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
load("Mcluster.phenodata.progenie.(junio).VARS_2023Diciembre28.RData")#modelos VARS
load("mean.phenodata.progenie.selected.log_(junio)_2023diciembre28_133842.RData")
BIC.Best.Model.Per.G<-
  apply(Mcluster.phenodata.progenie$BIC, 1, max, na.rm = T)
max.BIC <- max(BIC.Best.Model.Per.G)

#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 6, 4, 2))
plot(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  type = "n",
  bty = "n",
  xlim = c(1, 9),
  ylim = c(40, 0),
  yaxt = "n",
  xaxt = "n",
  xlab = NA,
  ylab = NA,
  main = "",
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex.main = 1.5
)
mtext(side = 1,
      "Número de grupos según el crecimiento\n a 51.4 meses después de la siembra ",
      line= 3.6,
      cex =1.5)
mtext(side = 2,
      expression(paste("Soporte empírico (", Delta, "BIC)", sep = "")),
      line= 3,
      cex =1.5)
points(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  cex = 2,
  pch = 19,
  col = "black",
  lwd = 1
)

# Mostrar el mejor modelo
# Agregar eje
axis(
  1,
  at = c(1, seq(2, 9, 1)),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(
  1,
  at = c(1, seq(2, 9, 2)),
  labels = T,
  tcl = F,
  cex.axis = 1.5
)

axis(
  1,
  at = 2,
  labels = T,
  tcl = F,
  cex.axis = 1.5
)
axis(2,
     at = seq(40, 0,-10),
     tcl = -0.7,
     cex.axis = 1.5,
     las =1)
abline(v = Mcluster.phenodata.progenie$G, lty = 3)# para determinar el modelo con el mejor soporte
title(expression("E)"), adj = 0)

#Acotando el soporte empírico 0-10
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 6, 4, 2))
plot(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  type = "n",
  bty = "n",
  xlim = c(1, 9),
  ylim = c(10, 0),
  yaxt = "n",
  xaxt = "n",
  xlab = NA,
  ylab = NA,
  main = "",
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex.main = 1.5
)
mtext(side = 1,
      "Número de grupos según el crecimiento\n a 51.4 meses después de la siembra ",
      line= 3.6,
      cex =1.5)
# mtext(side = 2,
#       expression(paste("Soporte empírico (", Delta, "BIC)", sep = "")),
#       line= 3,
#       cex =1.5)
points(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  cex = 2,
  pch = 19,
  col = "black",
  lwd = 1
)

# Mostrar el mejor modelo
# Agregar eje
axis(
  1,
  at = c(1, seq(2, 9, 1)),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(
  1,
  at = c(1, seq(2, 9, 2)),
  labels = T,
  tcl = F,
  cex.axis = 1.5
)

axis(
  1,
  at = 2,
  labels = T,
  tcl = F,
  cex.axis = 1.5
)
axis(2,
     at = seq(10, 0,-2),
     tcl = -0.7,
     cex.axis = 1.5,
     las =1)
abline(v = Mcluster.phenodata.progenie$G, lty = 3)# para determinar el modelo con el mejor soporte
title(expression("F)"), adj = 0)
#################################################################################################################

# 4.1.4) Examinar tablas de clasificación cruzada de los grupos morfológicos según las plantas madre del piloto y
#grupos de plantas madre según crecimiento.

# Directorio de trabajo de los especímenes(plantas madre)
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#directorio de los datos de las plantas madres

# Asignación de grupos de las plantas madre del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")

# Asignación de grupos de plantas madre según crecimiento tomados en junio de 2023 (51.4 meses después de la siembra)
phenotypic.group.assignment.progenie <-
  read.table(
    "grupos.crecimiento.(junio).VARS_2023diciembre28_140052.csv",
    header = T,
    sep = ","
  )

#Subconjunto de los grupos morfológicos con sólo las plantas madre del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350,]
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
#  1  2
# 28  5

# Unir los tablas con los grupos morfológicos y los grupos de plantas madre según crecimiento que tengan
# representación de progenie viva: 35 plantas
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
# Phenotypic.Group.progenie
# Phenotypic.Group.madres 1 2
                      # 2 5 0
                      # 3 9 5
                      # 4 5 0
                      # 5 9 0
###################################################################################################################
###################################################################################################################
###################################################################################################################

#####MODELO DE MEZCLAS NORMALES PARA CRECIMIENTO DE LA PROGENIE DE LAS PLANTAS MADRE: 11.4 meses#####

###################################################################################################################
###################################################################################################################
###################################################################################################################

#INTRODUCCIÓN:

#Los datos de crecimiento de la progenie (cantidad de hojas y longitud del tallo) fueron usados para asignar
#grupos morfológico de las plantas madre según e crecimiento de su progenie. Realizamos análisis separados para
#cada uno de los tres muestreos (11.4 meses, 19.6 meses y 51.4 meses después de la siembra). En cada uno de estos
#análisis, primero se obtuvo la media del logaritmo natural de la longitud del tallo y del número de hojas para
#la progenie de cada planta madre. Luego, se analizaron con modelos de mezclas bivariadas normales con el paquete
#estadístico de R mclust v. 6.0.0 (Fraley et al., 2024) para estimar de novo los grupos conformados por las
#plantas madre según el crecimiento de su progenie en el jardín común.

#REQUERIMIENTOS##

#"PhenotypicDataProgeny_Quebradas_2020Marzo.csv": datos de crecimiento hechos en marzo de 2020#

# "PhenotypicGroupAssignment_2023septiembre08_120644.csv": Asignación de grupos morfológicos#

#CONTENIDO##
# 1) Preliminares: cargar las librerías y lectura de datos

# 2) Examinar variables: editar los datos y transformarlos

# 3) Ajuste de modelos de mezclas normales

# 4) Examinar los grupos de crecimiento en base al mejor modelo de mezclas normales, de acuerdo con las variables
#de inicialización

# 5) Tabla de clasificación cruzada entre modelos

#################################################################################################################
#################################################################################################################
#################################################################################################################


#################################################################################################################
#################################################################################################################
# 1) Preliminares: cargar las librerías y lectura de datos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1)Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mexclas normales
library(ellipse)
library(ggplot2)


#################################################################################################################
# 1.2) Lectura de datos
# Directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/data") #Ivan's working directory Waterman

# Leer la tabla de datos: datos de la progenie tomados en Quebradas en 20/03/2020 (11.4 meses después de la siembra)
phenodata.progenie <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2020Marzo.csv",
    header = T,
    sep = ","
  )
View(phenodata.progenie)
summary(phenodata.progenie)
head(phenodata.progenie)
dim(phenodata.progenie) # 250 observaciones y 26 variables

#################################################################################################################
#################################################################################################################
# 2) Examinar variables: editar los datos y transformarlos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 2.1) Seleccionar variables de crecimiento

colnames(phenodata.progenie)#sólo se usará número de colección (1), longitud del tallo (9) y número de hojas (10)
phenodata.progenie.selected <- phenodata.progenie[, c(1, 9, 10)]
View(phenodata.progenie.selected)
colnames(phenodata.progenie.selected) <-
  c("Número de colección plantas madres",
    "Longitud del tallo",
    "Número de hojas")
dim(phenodata.progenie.selected) #250 plantas de la progenie y 3 variables

# Unidades de medida de cada variables
measurement.units <- c(NA,  "cm", "count")
data.frame(colnames(phenodata.progenie.selected), measurement.units)

#################################################################################################################
# 2.2) Remover plantas de la progenie sin madre asignada o datos faltantes

# Cuántas plantas madre están representadas
unique(phenodata.progenie.selected$`Número de colección plantas madres`)## hay dos plantas sin madre
length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))## 39 plantas madres representadas

# Excluir plantas de la progenie sin planta madre y que no tenga datos (excluir todos los NAs)

sapply(phenodata.progenie.selected, class)

phenodata.progenie.selected[, 1] <-
  as.numeric(phenodata.progenie.selected[, 1])

sapply(phenodata.progenie.selected, class)

rows.with.na <-
  unique(which(is.na(phenodata.progenie.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 14 plantas hijas con NA

# Correr las siguientes líneas en caso de existir NAs
phenodata.progenie.selected <-
  phenodata.progenie.selected[-rows.with.na,]
dim(phenodata.progenie.selected) # 236 hijas con todos los datos
class(phenodata.progenie.selected)
summary(phenodata.progenie.selected)
head(phenodata.progenie.selected)

length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))## hay representación de 37 plantas madres  
unique(phenodata.progenie.selected$`Número de colección plantas madres`)##cuáles

#################################################################################################################
#  2.3) Transformación de los datos.

# Dado que las variables fenotípicas frecuentemente siguen distribución log-normal se transforman a escala
# logarítmica

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

# Guardar la media de las variables de crecimiento por planta madre
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/data") #Ivan's working directory Waterman
# save(
#   mean.phenodata.progenie.selected.log,
#   file = paste(
#     "mean.phenodata.progenie.selected.log.(marzo)_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".RData",
#     sep = ""
#   )
# )
# load("mean.phenodata.progenie.selected.log.(marzo)_2023diciembre28_073516.RData")

###################################################################################################################
# 2.5) Graficar los caracteres fenotípicos

# Histograma de la distribución de las variables de crecimiento en el promedio de las plantas madre

# Seleccionar directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
load("mean.phenodata.progenie.selected.log.(marzo)_2023diciembre28_073516.RData")

# Seleccionar directorio de trabajo para guardar figuras
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")

par(mar = c(5, 5, 4, 5) + 0.1)
range(mean.phenodata.progenie.selected.log$`log Longitud del tallo`)#-1.2039728  0.5306283
hist(
  mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
  breaks = seq(-1.4, 2.2, 0.2),
  ylim = c(0, 15),
  main = NA,
  xlab = NA,
  xaxt = "n",
  yaxt = "n",
  ylab = "Número de plantas madre",
  cex.lab = 1.5
)
title(expression("A) 11.4 meses"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(-1.4, 2.2, 0.2),
  labels = F,
  tcl = -0.3
)
axis(
  side = 1,
  at = seq(-1.4, 2.2, 0.6),
  labels = T,
  cex.axis =1.5,
  tcl = -0.5
)
axis(
  side = 1,
  at = -0.8,
  labels = T,
  cex.axis =1.5
)
axis(
  side = 2,
  at = 1:15,
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0,15,3),
  labels = seq(0,15,3),
  cex.axis =1.5,
  las = 2,
  tcl = -0.5
)

par(mar = c(5, 5, 4, 5) + 0.1)
range(mean.phenodata.progenie.selected.log$`log Número de hojas`)# 1.386294 2.564949
hist(
  mean.phenodata.progenie.selected.log$`log Número de hojas`,
  breaks = seq(1, 3.4, 0.2),
  ylim = c(0, 15),
  main = NA,
  xlab = NA,
  ylab = NA,
  xaxt = "n",
  yaxt = "n",
  cex.lab = 1.5
)
title(expression("D)"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(1, 3.4, 0.2),
  labels = F,
  tcl = -0.3
)
axis(
  side = 1,
  at = seq(1, 3.4, 0.4),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
  axis(
  side = 2,
  at = 1:15,
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0,15,3),
  labels = seq(0,15,3),
  cex.axis = 1.5,
  las = 2,
  tcl = -0.5
)

#################################################################################################################
#################################################################################################################
# 3) Ajuste de modelos de mezclas normales
#################################################################################################################
#################################################################################################################

# Subconjunto con sólo los caracteres fenotípicos
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/data") #Ivan's working directory Waterman
# load("mean.phenodata.progenie.selected.log.(marzo)_2023diciembre28_073516.RData")
data.for.GMM <- mean.phenodata.progenie.selected.log[, 2:3]

#################################################################################################################
# 3.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

#"PCS"####
mclust.options(hcUse = "PCS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
# Examinando resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust EEE (ellipsoidal, equal volume, shape and orientation) model with 2 components: 
#   
#   log-likelihood  n df       BIC       ICL
#   6.891778        37  8   -15.10379 -15.10401
# 
# Clustering table:
#   1  2 
# 36  1

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
# Resultados:
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

#Guardar el mejor modelo de mezclas normales
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# save(Mcluster.phenodata.progenie, file = "Mcluster.phenodata.progenie.(marzo).VARS_2023diciembre28.RData")
#load("Mcluster.phenodata.progenie.(marzo).VARS_2023diciembre28.RData")
#
# Mclust EEE (ellipsoidal, equal volume, shape and orientation) model with 2 components: 
#   
#   log-likelihood  n df       BIC       ICL
# 6.891778 37  8 -15.10379 -15.10401
# 
# Clustering table:
#   1  2 
# 36  1

# Gráfica de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"STD"####
mclust.options(hcUse = "STD")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)#
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust EEE (ellipsoidal, equal volume, shape and orientation) model with 2 components: 
#   
#   log-likelihood  n df       BIC       ICL
# 6.891778 37  8 -15.10379 -15.10401
# 
# Clustering table:
#   1  2 
# 36  1

# Gráfica de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SPH"####
mclust.options(hcUse = "SPH")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
# Resultados:
summary(Mcluster.phenodata.progenie)
Mcluster.phenodata.progenie
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust EEV (ellipsoidal, equal volume and shape) model with 2 components: 
#   
#   log-likelihood  n df      BIC       ICL
# 7.37518 37  9 -17.7479 -18.70331
# 
# Clustering table:
#   1  2 
# 2 35

## Gráfica de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"PCR"####
mclust.options(hcUse = "PCR")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
# Resultados:
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# Guardar el mejor modelo de mezclas:
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# save(Mcluster.phenodata.progenie, file="Mcluster.phenodata.progenie.(marzo).PCR_2023diciembre28.RData")
#load("Mcluster.phenodata.progenie.(marzo).PCR_2023diciembre28.RData")
#
# Mclust EEV (ellipsoidal, equal volume and shape) model with 2 components: 
#   
#   log-likelihood  n df      BIC       ICL
# 7.375183 37  9 -17.7479 -18.72028
# 
# Clustering table:
#   1  2 
# 35  2
# Gráfica de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#  Gráfica del soporte empírico de los diferentes modelos
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

# Guardar el mejor modelo de mezclas:
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# save(Mcluster.phenodata.progenie, file="Mcluster.phenodata.progenie.(marzo).SVD_2023diciembre28.RData")
#load("Mcluster.phenodata.progenie.(marzo).SVD_2023diciembre28.RData")
#
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 2
# components:
#
#   log-likelihood  n df       BIC       ICL
#      7.604768     37  9   -17.28872 -18.09609
#
# Clustering table:
#   1  2
#   35  2

# Gráfica de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#################################################################################################################
#################################################################################################################
# 4) Examinar los grupos de crecimiento en base al mejor modelo de mezclas normales, de acuerdo con las variables
#de inicialización
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1)VARS####
# Cargar los mejores modelos de mezclas normales
# Directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
load("Mcluster.phenodata.progenie.(marzo).VARS_2023diciembre28.RData")#modelos VARS
load("mean.phenodata.progenie.selected.log.(marzo)_2023diciembre28_073516.RData")

# 4.1.1) Examinar y guardar en un documento la asignación de grupos para la progenie en el primer muestreo

#  Crear y escribir documento para la asignación de los grupos de crecimiento
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
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/data") #Ivan's working directory Waterman
# write.csv(
#   phenotypic.group.assignment.piloto,
#   file = paste(
#     "grupos.crecimiento.(marzo).VARS_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )


# 4.1.2) Graficar grupos según crecimiento en el mejor modelo de mezclas normales.

# Directorio para guardar figuras
# setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana

# Longitud del tallo vs número de hojas
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1) #FS_X
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  xlab = NA,
  main = "",
  addEllipses = F,
  asp = 1,
  ylab = "Media log (número de hojas)",
  cex.lab = 1.5
)
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
#Mcluster.phenodata.progenie$parameters$mean
# Agregar etiquetas de las elipses
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 1],
  Mcluster.phenodata.progenie$parameters$mean[2, 1],
  "C1",
  cex = 0.9
)
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 2] + 0.2,
  Mcluster.phenodata.progenie$parameters$mean[2, 2] + 0.02,
  "C2",
  cex = 0.9
)
title(expression("A) 11.4 meses") , adj = 0)
# Identificando puntos
# identify(
#   mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
#   mean.phenodata.progenie.selected.log$`log Número de hojas`,
#   labels = as.character(
#     mean.phenodata.progenie.selected.log$`Número de colección plantas madres`
#   ),
#   cex=0.5
# )
# Plantas madre discordantes
discordantes <-
  c(1003, 1004, 1008, 1018, 1031, 1032, 1033, 1034, 1036, 1041, 1022, 1006, 1013, 1037, 1038, 1040, 1039, 1030)
muertas <- c(1029, 1025, 1001, 1026)
text(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% discordantes, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% discordantes, 3],
  labels = as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% discordantes, 1]),
  cex = 0.6,
  pos = 4
)
text(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 3],
  labels = as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 1]),
  cex = 0.6,
  pos = 3,
  font = 2
)
points(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 3],
  pch = 4
)

# 4.1.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata.progenie$uncertainty)

#De los 37 especímenes, 1 tiene valores de incertidumbre mayores a 0.1
#esto es el 2.7% de los especímenes
sum(Mcluster.phenodata.progenie$uncertainty > 0.1)
sum(Mcluster.phenodata.progenie$uncertainty > 0.1) / length(Mcluster.phenodata.progenie$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata.progenie$classification[Mcluster.phenodata.progenie$uncertainty >
                                             0.1]
# 0
phenotypic.group.assignment.piloto$Collector.Collection.Number[phenotypic.group.assignment.piloto$Uncertainty >
                                                                 0.1]# planta madre 1022

# Gráfica de la función de distribución acumulativa de valores de incertidumbre
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  sort(Mcluster.phenodata.progenie$uncertainty),
  1:length(Mcluster.phenodata.progenie$uncertainty) / length(Mcluster.phenodata.progenie$uncertainty),
  xlim = c(0, 0.5),
  bty = "n",
  xlab = "Incertidumbre",
  ylab = "F (Incertidumbre)",
  type = "l",
  pch = 19,
  cex = 0.5,
  cex.axis = 1.5,
  cex.lab = 1.5
)
abline(h = c(0, 1),
       lty = 3,
       col = "gray70")

# Gráfica de los especímenes con incertidumbre > 0.1
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  addEllipses = F,
  cex = 0
)
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
text(-1.16, 1.62, "C1")
text(-0.33, 2.30, "C2")
for (i in 1:Mcluster.phenodata.progenie$G) {
  points(
    Mcluster.phenodata.progenie$data[Mcluster.phenodata.progenie$uncertainty > 0.1 &
                                       Mcluster.phenodata.progenie$classification == i][1],
    Mcluster.phenodata.progenie$data[Mcluster.phenodata.progenie$uncertainty > 0.1 &
                                       Mcluster.phenodata.progenie$classification == i][2],
    pch = mclust.options("classPlotSymbols")[i],
    col = mclust.options("classPlotColors")[i]
  )
}
legend(
  "bottomright",
  paste("C", c(1, 2)),
  col = mclust.options("classPlotColors")[c(1, 2)],
  pch = mclust.options("classPlotSymbols")[c(1, 2)],
  pt.lwd = 1,
  pt.cex = 1,
  cex = 1.5,
  bty = "o"
)

# 4.1.4) Examinar la tablas de clasificación cruzada de los grupos morfológicos según las plantas madre y grupos 
#según crecimiento de su progenie en el mejor modelo de mezcla normal.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
grupos.morfologicos <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en marzo de 2020
grupos.crecimiento.1 <-
  read.table(
    "grupos.crecimiento.(marzo).VARS_2023diciembre28_081348.csv",
    header = T,
    sep = ","
  )
#Subconjunto con sólo las plantas madres del piloto
grupos.morfologicos.madres <-
  grupos.morfologicos[308:350,]
head(grupos.morfologicos.madres)
head(grupos.crecimiento.1)

#Extracción del número de colección de las plantas madres
grupos.morfologicos.madres$Collector.Collection.Number <-
  as.numeric(substring(
    grupos.morfologicos.madres$Collector.Collection.Number,
    5
  ))
grupos.morfologicos.madres$Phenotypic.Group <-
  as.factor(grupos.morfologicos.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(
  grupos.morfologicos.madres$Collector.Collection.Number,
  grupos.morfologicos.madres$Phenotypic.Group,
  length
)
# 2  3  4  5
# 6 18  5 14

tapply(
  grupos.crecimiento.1$Collector.Collection.Number,
  grupos.crecimiento.1$Phenotypic.Group,
  length
)
#  1  2
#  36 1

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
grupos.crecimiento.madres.progenie <-
  merge(
    grupos.morfologicos.madres[, c(2, 6)],
    grupos.crecimiento.1[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie")
  )


phenotypic.group.crosstab <-
  table(grupos.crecimiento.madres.progenie[, c(2, 3)])
phenotypic.group.crosstab
# Phenotypic.Group.progenie
# Phenotypic.Group.madres  1  2
                      # 2  6  0
                      # 3 15  0
                      # 4  5  0
                      # 5 10  1

###################################################################################################################
# 4.2) PCR####
# Cargar los mejores modelos de mezclas normales
# Directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
load("Mcluster.phenodata.progenie.(marzo).PCR_2023diciembre28.RData")


# 4.2.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

# Crear y escribir documento para la asignación de los grupos fenotípicos.
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
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# guardar en directorio de Diana
# setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/data") #Ivan's working directory Waterman
# write.csv(
#   phenotypic.group.assignment.piloto,
#   file = paste(
#     "grupos.crecimiento.(marzo).PCR_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

# 4.2.2) Graficar grupos según ccrecimiento en el mejor modelo de mezclas normales.

#  Directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana

#Longitud del tallo vs número de hojas
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  xlab = NA,
  addEllipses = F,
  asp = 1,
  ylab = "Media log (número de hojas)",
  cex.lab = 1.5
)
legend(
  "bottomright",
  paste("C", 1:2),
  col = mclust.options("classPlotColors"),
  xpd = T,
  ncol = 2,
  pch = mclust.options("classPlotSymbols"),
  pt.lwd = 0.9,
  pt.cex = 0.9,
  cex = 0.9,
  bty = "o"
)
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
  Mcluster.phenodata.progenie$parameters$mean[1, 1] + 0.2,
  Mcluster.phenodata.progenie$parameters$mean[2, 1] + 0.02,
  "C1",
  cex = 0.9
)
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 2] + 0.1,
  Mcluster.phenodata.progenie$parameters$mean[2, 2] + 0.05,
  "C2",
  cex = 0.9
)
title(expression("11.4 meses"), adj = 0)
# Identificando puntos
# identify(
#   mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
#   mean.phenodata.progenie.selected.log$`log Número de hojas`,
#   labels = as.character(
#     mean.phenodata.progenie.selected.log$`Número de colección plantas madres`
#   ),
#   cex=0.5
# )
#Plantas madre discordantes
discordantes <-
  c(1004, 1022, 1006, 1013, 1032, 1037, 1040, 1030, 1038, 1039)
muertas <- c(1029, 1025, 1001, 1026)
text(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% discordantes, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% discordantes, 3],
  labels = as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% discordantes, 1]),
  cex = 0.6,
  pos = 4
)
text(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 3],
  labels = as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 1]),
  cex = 0.6,
  pos = 3,
  font = 2
)
points(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 3],
  pch = 4
)

# 4.2.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata.progenie$uncertainty)

#De los 37 especímenes, 1 tiene incertidumbre mayor a 0.1
sum(Mcluster.phenodata.progenie$uncertainty > 0.1)
sum(Mcluster.phenodata.progenie$uncertainty > 0.1) / length(Mcluster.phenodata.progenie$uncertainty) #0.02702703
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata.progenie$classification[Mcluster.phenodata.progenie$uncertainty >
                                             0.1]# grupo 2


# 4.2.4) Examinar la tabla de clasificación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos
#fenotípicos según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

# Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
grupos.morfologicos <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# Asignación de grupos de plantas progenie con datos de crecimiento tomados en marzo de 2020
grupos.crecimiento.2 <-
  read.table(
    "grupos.crecimiento.(marzo).PCR_2023diciembre28_082916.csv",
    header = T,
    sep = ","
  )
# Subconjunto con sólo las plantas madre del piloto
grupos.morfologicos.madres <-
  grupos.morfologicos[308:350,]
head(grupos.morfologicos.madres)
head(grupos.crecimiento.2)

# Extracción del número de colección de las plantas madres
grupos.morfologicos.madres$Collector.Collection.Number <-
  as.numeric(substring(
    grupos.morfologicos.madres$Collector.Collection.Number,
    5
  ))
grupos.morfologicos.madres$Phenotypic.Group <-
  as.factor(grupos.morfologicos.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(
  grupos.morfologicos.madres$Collector.Collection.Number,
  grupos.morfologicos.madres$Phenotypic.Group,
  length
)
# 2  3  4  5
# 6 18  5 14

tapply(
  grupos.crecimiento.2$Collector.Collection.Number,
  grupos.crecimiento.2$Phenotypic.Group,
  length
)
#  1  2
# 35  2

# Unir los dos agrupamientos para las plantas madre en común que tenga representación en la progenie: 35 plantas
grupos.morfologicos.madres.progenie <-
  merge(
    grupos.morfologicos.madres[, c(2, 6)],
    grupos.crecimiento.2[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie")
  )


phenotypic.group.crosstab <-
  table(grupos.morfologicos.madres.progenie[, c(2, 3)])
phenotypic.group.crosstab
# Phenotypic.Group.progenie
# Phenotypic.Group.madres  1  2
# 2  6  0
# 3 15  0
# 4  4  1
# 5 10  1

###################################################################################################################
# 4.3) SVD####
# Cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/data") #Directorio de Iván: Waterman
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
# load("Mcluster.phenodata.progenie.(marzo).SVD_2023diciembre28.RData")

# 4.3.1) Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

# Crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.piloto.SVD <-
  data.frame(
    as.numeric(rownames(mean.phenodata.progenie.selected.log)),
    mean.phenodata.progenie.selected.log[, 1],
    Mcluster.phenodata.progenie$classification,
    Mcluster.phenodata.progenie$uncertainty
  )
colnames(phenotypic.group.assignment.piloto.SVD) <-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.piloto.SVD)

# 4.3.2) Comparar si los modelos PCR y SVD tiene la misma asignación a pesar que el delta de BIC = 2e-05

identical(phenotypic.group.assignment.piloto$Collector.Collection.Number, phenotypic.group.assignment.piloto.SVD$Collector.Collection.Number) #TRUE
table(phenotypic.group.assignment.piloto$Phenotypic.Group, phenotypic.group.assignment.piloto.SVD$Phenotypic.Group)
# 1  2
# 1 35  0
# 2  0  2

# es decir tiene la misma asignación a grupo fenotípico
###################################################################################################################
###################################################################################################################
# 5) Tabla de clasificación cruzada entre modelos####
###################################################################################################################
###################################################################################################################

grupos.crecimiento <-
  merge(
    grupos.crecimiento.1[, c(2, 3)],
    grupos.crecimiento.2[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".1", ".2")
  )

table(
  grupos.crecimiento[, 2],
  grupos.crecimiento[, 3]
)
#   1  2
# 1 35  1
# 2  0  1

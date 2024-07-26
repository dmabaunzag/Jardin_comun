#################################################################################################################
#################################################################################################################
#################################################################################################################

#####MODELO DE MEZCLAS NORMALES PARA CRECIMIENTO DE LA PROGENIE DE LAS PLANTAS MADRE: 19.6 meses#####

#################################################################################################################
#################################################################################################################
#################################################################################################################

#INTRODUCCIÓN:

#Los datos de crecimiento de la progenie (cantidad de hojas y longitud del tallo) fueron usados para asignar
#grupos morfológico de las plantas madre según e crecimiento de su progenie. Realizamos análisis separados para
#cada uno de los tres muestreos (11.4 meses, 19.6 meses y 51.4 meses después de la siembra). En cada uno de estos
#análisis, primero se obtuvo la media del logaritmo natural de la longitud del tallo y del número de hojas para
#la progenie de cada planta madre. Luego, se analizaron con modelos de mezclas bivariadas normales con el paquete
#estadístico de R mclust v. 6.0.0 (Fraley et al., 2024) para estimar de novo los grupos conformados por las
#plantas madre según el crecimiento de su progenie en el jardín común.

#REQUERIMIENTOS##
#"PhenotypicDataProgeny_Quebradas_2020Octubre.csv": datos de crecimiento hechos en octubre de 2020

# "PhenotypicGroupAssignment_2023septiembre08_120644.csv": Asignación de grupos morfológicos#

#CONTENIDO##
# 1) Preliminares: cargar las librerías y lectura de datos

# 2) Examinar variables: editar los datos y transformarlos

# 3) Ajuste de modelos de mezclas normales

# 4) Examinar los grupos de crecimiento en base al mejor modelo de mezclas normales, de acuerdo con las variables
#de inicialización

#################################################################################################################
#################################################################################################################
#################################################################################################################


#################################################################################################################
#################################################################################################################
# 1) Preliminares: cargar las librerías y lectura de datos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mexclas normales
library(ellipse)
library(ggplot2)

#################################################################################################################
# 1.2) Lectura de datos

# Directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

# Leer la tabla de datos: datos de la progenie tomados en Quebradas en octubre de 2020 (19.6 meses después de la
# siembra)
phenodata.progenie <-
  read.table("PhenotypicDataProgeny_Quebradas_2020Octubre.csv", 
             header=T,
             sep = ",")
View(phenodata.progenie)
summary(phenodata.progenie)
head(phenodata.progenie)
dim(phenodata.progenie)# 250 plantas del piloto con 33 variables

#################################################################################################################
#################################################################################################################
# 2) Examinar variables: editar los datos y transformarlos.
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
# 2.2) Remover plantas de la progenie sin madre asignada y datos faltantes

# Cuántas plantas madre están representadas
unique(phenodata.progenie.selected$`Número de colección plantas madres`)## hay dos plantas sin madre
length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))## 39 plantas madre representadas

# Excluir plantas de la progenie sin planta madre y que no tenga datos (excluir todos los NAs)

sapply(phenodata.progenie.selected, class)

phenodata.progenie.selected[, 1] <-
  as.numeric(phenodata.progenie.selected[, 1])

sapply(phenodata.progenie.selected, class)

rows.with.na <-
  unique(which(is.na(phenodata.progenie.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 30 plantas hijas con NA

# Correr las siguientes líneas en caso de existir NAs
phenodata.progenie.selected <-
  phenodata.progenie.selected[-rows.with.na, ]
dim(phenodata.progenie.selected) # 220 hijas con todos los datos
class(phenodata.progenie.selected)
summary(phenodata.progenie.selected)
head(phenodata.progenie.selected)

length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))## hay representación de 35 plantas madres
unique(phenodata.progenie.selected$`Número de colección plantas madres`)##cuáles

##################################################################################################################
#  2.3) Transformación de los datos.

# Dado que los variables fenotípicas frecuentemente siguen distribución log-normal se transforman a escala
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
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# save(
#   mean.phenodata.progenie.selected.log,
#   file = paste(
#     "mean.phenodata.progenie.selected.log.(octubre)_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".RData",
#     sep = ""
#   )
# )
#load("mean.phenodata.progenie.selected.log.(octubre)_2023diciembre28_091832.RData")

###################################################################################################################
# 2.5) Graficar los caracteres fenotípicos

# Histograma de la distribución de las variables de crecimiento en el promedio de las plantas madre

# Seleccionar directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
load("mean.phenodata.progenie.selected.log.(octubre)_2023diciembre28_091832.RData")

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
title(expression("B) 19.6 meses"), adj = 0, cex.main =1.5)
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
title(expression("E)"), adj = 0, cex.main =1.5)
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
  cex.axis =1.5,
  las = 2,
  tcl = -0.5
)

###################################################################################################################
###################################################################################################################
# 3) Ajuste de modelos de mezclas normales
###################################################################################################################
###################################################################################################################

# Directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
load("mean.phenodata.progenie.selected.log.(octubre)_2023diciembre28_091832.RData")

# Subconjunto con sólo los caracteres fenotípicos
data.for.GMM <- mean.phenodata.progenie.selected.log[, 2:3]

###################################################################################################################
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
#
#Mclust VEI (diagonal, equal shape) model with 3
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 9.94742 35 12 -22.76934 -24.47416
# 
# Clustering table:
#   1  2  3 
# 17 15  3 
# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
# Examinar resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

# Guardar el mejor modelo
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# save(Mcluster.phenodata.progenie, file = "Mcluster.phenodata.progenie.(octubre)_2023Diciembre28.RData")
#load("Mcluster.phenodata.progenie.(octubre)_2023Diciembre28.RData")
# Mclust VEI (diagonal, equal shape) model with 3
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 9.94742 35 12 -22.76934 -24.47416
# 
# Clustering table:
#   1  2  3 
# 17 15  3

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
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
#Mclust VEI (diagonal, equal shape) model with 3
# components: 
#   
#   log-likelihood  n df       BIC      ICL
# 9.947433 35 12 -22.76931 -24.4686
# 
# Clustering table:
#   1  2  3 
# 17 15  3
# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SPH"
mclust.options(hcUse = "SPH")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinar resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#Mclust VEI (diagonal, equal shape) model with 3
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 9.94742 35 12 -22.76934 -24.47416
# 
# Clustering table:
#   1  2  3 
# 17 15  3

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"PCR"
mclust.options(hcUse = "PCR")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinar resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
# Mclust VEI (diagonal, equal shape) model with 3
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 9.94742 35 12 -22.76934 -24.47416
# 
# Clustering table:
#   1  2  3 
# 17 15  3

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SVD"
mclust.options(hcUse = "SVD")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinar resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de os especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#Mclust VEI (diagonal, equal shape) model with 3
# components: 
#   
#   log-likelihood  n df       BIC       ICL
# 9.94742 35 12 -22.76934 -24.47416
# 
# Clustering table:
#   1  2  3 
# 17 15  3

# Gráficas de los grupos según crecimiento, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
# Gráfica del soporte empírico de los diferentes modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

###################################################################################################################
###################################################################################################################
# 4) Examinar los grupos fenotípicos en base al mejor modelo de mezclas normales
###################################################################################################################
###################################################################################################################
# Cargar el mejor modelo de mezcla normal
# Directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")# directorio de Diana
load("Mcluster.phenodata.progenie.(octubre)_2023Diciembre28.RData")# Agrupación
load("mean.phenodata.progenie.selected.log.(octubre)_2023diciembre28_091832.RData") #datos morfológicos

VEI <- Mcluster.phenodata.progenie$classification
###################################################################################################################
# 4.1) Examinar y guardar en un documento la asignación de grupos

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
#     "grupos.crecimiento.(octubre)_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

###################################################################################################################
# 4.2) Graficar grupos según crecimiento en el mejor modelo de mezclas normales.

# Directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
#
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
  cex.lab = 1.2
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
  Mcluster.phenodata.progenie$parameters$mean[1, 1] - 0.15,
  Mcluster.phenodata.progenie$parameters$mean[2, 1] - 0.1,
  "C1",
  cex = 0.9
)
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 2] + 0.1,
  Mcluster.phenodata.progenie$parameters$mean[2, 2] - 0.05,
  "C2",
  cex = 0.9
)
text(
  Mcluster.phenodata.progenie$parameters$mean[1, 3],
  Mcluster.phenodata.progenie$parameters$mean[2, 3],
  "C3",
  cex = 0.9
)
title(expression("B) 19.6 meses"), adj = 0)
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
text(
muertas <- c(1029, 1025, 1001,1026),
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
  pos = 4,
  font = 2
)
points(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 3],
  pch = 4
)

#################################################################################################################
# 4.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata.progenie$uncertainty)

# De los 35 especímenes, 2 tiene valores de incertidumbre mayores a 0.1
# esto es el 5.7% de los especímenes
sum(Mcluster.phenodata.progenie$uncertainty > 0.1)
sum(Mcluster.phenodata.progenie$uncertainty > 0.1) / length(Mcluster.phenodata.progenie$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata.progenie$classification[Mcluster.phenodata.progenie$uncertainty >
                                             0.1]
# 1 2 

#################################################################################################################
# 4.4) Graficar soporte empírico para el mejor modelo
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
  ylim = c(30, 0),
  yaxt = "n",
  xaxt = "n",
  xlab = "Número de grupos según el crecimiento\n a 19.6 meses después de la siembra ",
  ylab = expression(paste("Soporte empírico (", Delta, "BIC)", sep = "")),
  main = "",
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex.main = 1.5
)
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
axis(2,
     at = seq(30, 0,-10),
     tcl = -0.7,
     cex.axis = 1.5,
     las =1)
abline(v = Mcluster.phenodata.progenie$G, lty = 3)# para determinar el modelo con el mejor soporte
title(expression("C)"), adj = 0)

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
  xlab = "Número de grupos según el crecimiento\n a 19.6 meses después de la siembra ",
  ylab = "",# expression(paste("Soporte empírico (", Delta, "BIC)", sep = "")),
  main = "",
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex.main = 1.5
)
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
axis(2,
     at = seq(10, 0,-2),
     tcl = -0.7,
     cex.axis = 1.5,
     las =1)
abline(v = Mcluster.phenodata.progenie$G, lty = 3)# para determinar el modelo con el mejor soporte
title(expression("D)"), adj = 0)


#Graficando modelo de Mclust con soporte empírico similar al de mejor BIC

#"VARS"####
mclust.options(hcUse = "VARS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM, modelNames = "VII")
# Examinar resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)

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
write.csv(
  phenotypic.group.assignment.piloto,
  file = paste(
    "grupos.crecimiento.(octubre)_VII_",
    format(Sys.time(), "%Y%B%d_%H%M%S"),
    ".csv",
    sep = ""
  ),
  row.names = F
)


#Gráfica
#Longitud del tallo vs número de hojas
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  xlab = "Media log (longitud del tallo [cm])",
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
title(expression("19.6 meses"), adj = 0)
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
text(
  muertas <- c(1029, 1025, 1001,1026),
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
  pos = 4,
  font = 2
)
points(
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 2],
  mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[, 1] %in% muertas, 3],
  pch = 4
)

#Tabla de calsificación cruzada entre clasificacion VII y VEI

VII <- Mcluster.phenodata.progenie$classification

table(VEI,VII)

###################################################################################################################
# 4.5) Examinar tablas de clasificación cruzada de los grupos morfológicos según las plantas madre del piloto y
#grupos de plantas madre según crecimiento.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#directorio de los datos de las plantas madres

#Asignación de grupos de las plantas madre del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")

# Asignación de grupos de plantas progenie con datos de crecimiento tomados en Octubre de 2020
phenotypic.group.assignment.progenie <-
  read.table(
    "grupos.crecimiento.(octubre)_2023diciembre28_132240.csv",
    header = T,
    sep = ","
  )
# Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350, ]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

# Extracción del número de colección de las plantas madres
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
# 1  2  3 
# 17 15  3 

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
Phenotypic.Group.progenie
# Phenotypic.Group.madres  1  2  3
                      # 2  4  1  0
                      # 3  4 10  1
                      # 4  3  2  0
                      # 5  6  2  2
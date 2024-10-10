#
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
####INTRODUCCIÓN####
#
#Este código  hace parte del material suplementario del artículo "La naturaleza de las especies de frailejones:
#un experimento de jardín común en Sumapaz", adaptado del código de Pineda et al. (en prep. The Nature of
#Espeletia Species). El objetivo es hacer un análisis de delimitación de los grupos morfológicos de las plantas
#madre basado en caracteres morfológicos de frailejones de del Páramo Sumapaz, cordillera Oriental de los Andes
#(Colombia).Los datos analizados corresponden a los 307 especímenes analizados en Pineda et al. (2020) junto con
#a 43 especímenes que fueron usadas como plantas madre dentro del experimento del jardín común. Primero,
#realizamos grupos morfológicos de los frailejones silvestres del páramo de Sumapaz sin información a priori. La
#variación morfológica expresada se analizó con modelos de mezclas normales utilizando el paquete de R mclust v.
#6.0.0.

#DATOS REQUERIDOS PARA CORRER ESTE CÓDIGO####

#Variables morfológicas usados en Pineda et al.:"meanphenodata_2022Apr27_160817.csv"

#Variables morfológicas de las plantas madre: "meanphenodatapilot_2023Aug02_095547.csv" 

#coordenadas plantas madres : "COORDENADAS_PLANTAS_MADRES_PILOTO.csv"

#Asignación de grupos Pineda e al: "PhenotypicGroupAssignmentPineda_2023junio01_095823".csv"

#CONTENIDO####

# 1) Datos preliminares: librerías y lectura de datos

# 2) Examinar  la distribución de cada carácter morfológico, editar los datos y transformación y rotación de los
#   datos con análisis de componentes principales

# 3) selección de variables para los modelos de mezclas normales

# 4) Ajuste de los modelos de mezclas normales

# 5)Examinar grupos morfológicos en el mejor modelo de mezclas normales

# 6)Examinar la localización  delos especímenes tipos E. cabrerensis y E. miradorensis en el mejor modelo
# de mezclas normales.

# 7) Examinar la distribución de los especímenes citados en la monografía de Espeletiinae (Cuatrecasas 2013)
#    en el en el espacio morfológico.

# 8)Distribución altitudinal de los especímenes en relación con la asignación de su grupo morfológico.

# 9) Tabla clasificación cruzada entre grupos morfológicos y resultados de Pineda et al. y estadístico tau para 
#encontrar grado de concordancia entre éstos

#################################################################################################################
#################################################################################################################
#################################################################################################################

#################################################################################################################
#################################################################################################################
# 1) Datos preliminares: librerías y lectura de datos####
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías

library(mclust) # librería para analizar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mezclas normales
library(ellipse)
library(GoodmanKruskal) # estadístico tau (τ) de Goodman y Kruskal

###################################################################################################################
# 1.2) lectura de los datos morfológicos. Estos datos son promedios de cada variable para cada espécimen que
#fueron promediados previamente.

# Seleccionar directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

# Leer datos morfológicos usados en Pineda et al.(2020)
mean.phenodata.pineda <-
  read.table("meanphenodata_2022Apr27_160817.csv",
             header = T,
             sep = ",")

summary(mean.phenodata.pineda)
head(mean.phenodata.pineda)
dim(mean.phenodata.pineda)# 1020 Especímenes y 21 variables

# Leer datos morfológicos de las 43 plantas madre
mean.phenodata.piloto <-
  read.table("meanphenodatapilot_2023Aug02_095547.csv",
             header = T,
             sep = ",")
summary(mean.phenodata.piloto)
head(mean.phenodata.piloto)
dim(mean.phenodata.piloto)# 21 variables y 43 especímenes

# Agregar coordenadas geográficas a las plantas madre
coordenadas.piloto <-
  read.table("COORDENADAS_PLANTAS _MADRES_PILOTO.csv",
             header = T,
             sep = ",")
mean.phenodata.piloto <-
  merge(mean.phenodata.piloto[, c(-4,-5,-6)], coordenadas.piloto[, c(-2,-3)],
        by = "Collector.Collection.Number")

# Combinar los datos morfológicos usados en Pineda et al. (2020) y los datos morfológicos de las 43 plantas madre
# en el jardín común.
mean.phenodata <-
  rbind(mean.phenodata.pineda, mean.phenodata.piloto)
summary(mean.phenodata)
head(mean.phenodata)
dim(mean.phenodata) # con 1063 especímenes y 21 variables

# Unidades de medida de cada variables
measurement.units <-
  c(
    NA,
    NA,
    NA,
    "decimal.degrees",
    "decimal.degrees",
    "m",
    "cm",
    "cm",
    "cm",
    "cm",
    "count",
    "count",
    "cm",
    "cm",
    "mm",
    "mm",
    "mm",
    "mm",
    "mm",
    NA,
    NA
  )

data.frame(colnames(mean.phenodata), measurement.units)

#Guardar los la tabla combinada
#Seleccionar directorio de trabajo
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# save(mean.phenodata,
#      file = paste(
#        "MeanPhenodata_",
#        format(Sys.time(), "%Y%B%d_%H%M%S"),
#        ".RData",
#        sep = ""
#      ))
# load("Mcluster.phenodata_2023agosto19.RData")

#################################################################################################################
#################################################################################################################
# 2) Examinar  la distribución de cada carácter morfológico, editar los datos y transformación y rotación de####
#los datos con análisis de componentes principales
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 2.1) Examinar gráficamente la distribución de cada carácter morfológico en escala logarítmica y linear.

#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/Figuras")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
colnames(mean.phenodata) # nombre de las variables

# Escoger variable fenotípica
trait <- 7 # eg., largo de la lámina

# Histogramas con la distribución de cada carácter morfológico
colnames(mean.phenodata)[trait] # Qué variable
# Distribución escala lineal
hist(
  mean.phenodata[, trait],
  breaks = 100,
  xlab = colnames(mean.phenodata)[trait],
  main = "",
  col = "gray80"
)
summary(mean.phenodata[, trait])
#Distribución en escala logarítmica
hist(
  log(mean.phenodata[, trait]),
  breaks = 100,
  xlab = paste(
    "log(",
    colnames(mean.phenodata)[trait],
    "(",
    measurement.units[trait],
    "))"
  ),
  main = "",
  col = "gray80"
)
summary(log(mean.phenodata[, trait]))

# Distribución en escala logarítmica +1; puede ser útil cuando hay ceros en los datos crudos
hist(
  log(mean.phenodata[, trait] + 1),
  breaks = 100,
  xlab = paste(
    "log(",
    colnames(mean.phenodata)[trait],
    "+1(",
    measurement.units[trait],
    "))"
  ),
  main = "",
  col = "gray80"
)
summary(log(mean.phenodata[, trait] + 1))


# Examinar gráficamente relaciones bivariables entre caracteres morfológicos
colnames(mean.phenodata)
trait.x <- 7 # eg., longitud de la lámina vs.,
trait.y <- 8 # ancho de la lámina

plot(
  mean.phenodata[, trait.x],
  mean.phenodata[, trait.y],
  xlab = paste(colnames(mean.phenodata)[trait.x], "(", measurement.units[trait.x], ")"),
  ylab = paste(colnames(mean.phenodata)[trait.y], "(", measurement.units[trait.y], ")"),
  cex.lab = 1.5,
  cex.axis = 1.5,
  asp = 1
)
#################################################################################################################
# 2.2) Realizar un subconjunto de datos sólo con los caracteres morfológicos para el análisis. La selección de los
#caracteres fue hecha a priori en bases en ideas de importancia de caracteres para distinguir las especies y
#basado en la monografía de Espeletiinae (Cuatrecasas, 2013).

# Seleccionar un subconjunto con los caracteres morfológicos: de la columna 7 a 19
colnames(mean.phenodata)
mean.phenodata.selected <- mean.phenodata[, 7:19]

# Examinar los resultados
dim(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)
colnames(mean.phenodata.selected)

#################################################################################################################
# 2.3) Remover los especímenes con valores NA en algún carácter morfológico.

# Determinar qué especímenes tiene  valores NA para ser excluidos del análisis
rows.with.na <-
  unique(which(is.na(mean.phenodata.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na) #número de especímenes con NA:713

# Correr las siguientes líneas en caso de existir NAs
mean.phenodata.selected <- mean.phenodata.selected[-rows.with.na,]
dim(mean.phenodata.selected) # 350 especímenes con datos en los 13 caracteres morfológicos
class(mean.phenodata.selected)
summary(mean.phenodata.selected)
head(mean.phenodata.selected)

#Guardar los la tabla seleccionada y filtrada

# Seleccionar directorio de trabajo
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data)#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# save(
#   mean.phenodata.selected,
#   file = paste(
#     "MeanPhenodataSelected_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".RData",
#     sep = ""
#   )
# )

# Tenga en cuenta que la referencia al marco de datos original (mean.phenodata) se puede hacer por el nombre de fila:
rownames(mean.phenodata.selected)
as.numeric(rownames(mean.phenodata.selected))

# Usando el índice numérico puede saber el número de colector  y el colector de los especímenes analizados,por
# Ejemplo:
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), c(1, 2)]
#o las coordenadas y la elevación de los especímenes:
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 4:6]
#o filas con alguna información de los especímenes:!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20])
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20])]
#o filas con información de especímenes citados en la monografía de Cuatrecasas: !is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21])
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21])]

#################################################################################################################
# 2.4) Transformación de los datos.

# Dado que los caracteres morfológicos frecuentemente siguen distribución log-normal, se transformaron los
#caracteres morfológicos con logaritmo natural, cerciorándose que a los caracteres que estén acotados a la
#izquierda con cero agregar +1, previamente.
mean.phenodata.selected.log <-
  log(
    data.frame(
      mean.phenodata.selected[, 1:4],
      mean.phenodata.selected[, 5] + 1, #varios especímenes no tienen brácteas estériles
      mean.phenodata.selected[, 6:13]
    )
  )
head(mean.phenodata.selected.log)

# Editar el nombre de las variables
colnames(mean.phenodata.selected.log) <-
  paste("log", colnames(mean.phenodata.selected))
colnames(mean.phenodata.selected.log)[5] <-
  paste("log", paste(colnames(mean.phenodata.selected)[5], "+1", sep = ""))
class(mean.phenodata.selected.log)
summary(mean.phenodata.selected.log)

#################################################################################################################
# 2.5) Análisis de componentes principales (CPs) a la matriz de varianza-covarianza de los caracteres continuos
#con transformación logarítmica.

# Análisis de componentes principales (CPs)
mean.phenodata.selected.log.pca <-
  prcomp(mean.phenodata.selected.log,
         center = T,
         scale. = F) #ACP usando matriz de varianza-covarianza
View(mean.phenodata.selected.log.pca)

# Examinar los resultados de los CPs
attributes(mean.phenodata.selected.log.pca)
mean.phenodata.selected.log.pca$scale
mean.phenodata.selected.log.pca$center
summary(mean.phenodata.selected.log.pca) # Varianza explicada en cada componente
summary(mean.phenodata.selected.log.pca$x) #Resumen de los principales componentes
mean.phenodata.selected.log.pca$rotation # Coeficientes (o "loadings") de cada carácter en cada componente

# Guardar el análisis de CPs

# Seleccionar directorio de trabajo
# setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data)# Directorio de Diana
# save(
#   mean.phenodata.selected.log.pca,
#   file = paste(
#     "MeanPhenodataSelectedLogPca_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".RData",
#     sep = ""
#   )
# )

# Cargar el análisis de CPs
# setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")# Directorio de Diana
# load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")
#
#Examinar las distribuciones univariadas de los principales componentes:
colnames(mean.phenodata.selected.log.pca$x)

# Revisar el nombre de los CPs
colnames(mean.phenodata.selected.log.pca$x)[PCA.x]

# Histogramas para cada CPs
for (PCA.x in 1:length(colnames(mean.phenodata.selected.log.pca$x))) {
  hist(
    mean.phenodata.selected.log.pca$x[, PCA.x],
    breaks = 100,
    xlab = colnames(mean.phenodata.selected.log.pca$x)[PCA.x],
    ylab = "Especímenes",
    main = "",
    cex.lab = 1.5,
    cex.axis = 1.5
  )
}

# Examinar relaciones bivariadas entre los principales componentes
# Seleccionar dos componentes:
colnames(mean.phenodata.selected.log.pca$x)
PCA.x <- 1
PCA.y <- 2

# Revisar los nombres
dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.x]
dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.y]

# Graficar las relaciones bivariadas
plot(
  mean.phenodata.selected.log.pca$x[, PCA.x],
  mean.phenodata.selected.log.pca$x[, PCA.y],
  xlab = dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.x],
  ylab = dimnames(mean.phenodata.selected.log.pca$x)[[2]][PCA.y],
  cex.lab = 1.5,
  cex.axis = 1.5,
  asp = 1
)
#################################################################################################################
#################################################################################################################
# 3) Selección de variables para los modelos de mezclas normales.#####
#################################################################################################################
#################################################################################################################
# Cargar el análisis de CPs
# setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")# Directorio de Diana
# load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")
#################################################################################################################
# 3.1) selección de variables hacia atrás usando CPs de la matriz de varianza-covarianza de los caracteres
#morfológicos con transformación logarítmica.

# Ejecutar selección de variables con dirección hacia atrás para los diferentes valores de incialización, usando
# el argumento "hcUse"; revisar las opciones de MClust: hep("Mclust)
for (option in c("PCS", "VARS", "STD", "SPH", "PCR", "SVD")) {
mclust.options(hcUse = option)
mean.phenodata.selected.log.pca.varsel.back <-
  clustvarsel(
    mean.phenodata.selected.log.pca$x,
    G = 1:9,
    search = c("greedy"),
    direction = c("backward")
  )
# Examinar los resultados
print(attributes(mean.phenodata.selected.log.pca.varsel.back))
print(summary(mean.phenodata.selected.log.pca.varsel.back))
print(names(mean.phenodata.selected.log.pca.varsel.back$subset))
print(mean.phenodata.selected.log.pca.varsel.back$steps.info)
print(mean.phenodata.selected.log.pca.varsel.back$search)
print(mean.phenodata.selected.log.pca.varsel.back$direction)
}
#Si existe concordancia entre cada uno de la selección de variables en los valores de inicialización, se prosigue
#a examinar si hay concordancia, según la dirección de la selección de variables, en caso contrario se utilizarán
#todas las variables para delimitar grupos de novo por mclust:
#################################################################################################################
#3.2) selección de variables hacia adelante usando CPs de la matriz de varianza-covarianza de los caracteres
#morfológicos con transformación logarítmica.

# Ejecutar selección de variables con dirección hacia atrás para los diferentes valores de incialización, usando el
#argumento "hcUse"; revisar las opciones de MClust: help("Mclust)

for (option in c("PCS", "VARS", "STD", "SPH", "PCR", "SVD")) {
  mclust.options(hcUse = option)
  mean.phenodata.selected.log.pca.varsel.for <-
    clustvarsel(
      mean.phenodata.selected.log.pca$x,
      G = 1:9,
      search = c("greedy"),
      direction = c("forward")
    )
  # Resultados:
  print(attributes(mean.phenodata.selected.log.pca.varsel.for))
  print(summary(mean.phenodata.selected.log.pca.varsel.for))
  print(names(mean.phenodata.selected.log.pca.varsel.for$subset))
  print(mean.phenodata.selected.log.pca.varsel.for$steps.info)
  print(mean.phenodata.selected.log.pca.varsel.for$search)
  print(mean.phenodata.selected.log.pca.varsel.for$direction)
}
# La selección de las variables tanto hacia adelante como hacia atrás, escogieron los primeros 12 componentes
# principales:

#################################################################################################################
#################################################################################################################
# 4) Ajuste de los modelos de mezclas normales####
#################################################################################################################
#################################################################################################################
# Cargar el análisis de CPs
# setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")# Directorio de Diana
# load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")
#################################################################################################################
# 4.1) Seleccionar caracteres morfológicos (PCs) para la inclusión del método de mezclas normales basado en los
#resultados de las secciones 3.1 3.2.
data.for.GMM <- mean.phenodata.selected.log.pca$x[, 1:12]

###################################################################################################################
# 4.2) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

for (option in c("PCS", "VARS", "STD", "SPH", "PCR", "SVD")) {
  mclust.options(hcUse = option)
  Mcluster.phenodata <- Mclust(data.for.GMM, G = 1:9)# con 1-9 componentes
  #Imprimir resultados:
  Mcluster.phenodata
  print(option)
  print(summary(Mcluster.phenodata))
}
# fitting ...
# |===============================================================================| 100%
# [1] "PCS"
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5 components: 
#   
#   log-likelihood   n  df       BIC       ICL
# 295.0292 350 190 -522.9489 -526.2676
# 
# Clustering table:
#   1   2   3   4   5 
# 85 107  58  22  78 
# fitting ...
# |===============================================================================| 100%
# [1] "VARS"
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5 components: 
#   
#   log-likelihood   n  df       BIC       ICL
# 295.0292 350 190 -522.9489 -526.2676
# 
# Clustering table:
#   1   2   3   4   5 
# 85 107  58  22  78 
# fitting ...
# |===============================================================================| 100%
# [1] "STD"
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5 components: 
#   
#   log-likelihood   n  df       BIC       ICL
# 160.1269 350 190 -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
# 23 163  76  27  61 
# fitting ...
# |===============================================================================| 100%
# [1] "SPH"
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5 components: 
#   
#   log-likelihood   n  df       BIC       ICL
# 160.1269 350 190 -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
# 23 163  76  27  61 
# fitting ...
# |===============================================================================| 100%
# [1] "PCR"
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5 components: 
#   
#   log-likelihood   n  df       BIC       ICL
# 160.1269 350 190 -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
# 23 163  76  27  61 
# fitting ...
# |===============================================================================| 100%
# [1] "SVD"
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVE (ellipsoidal, equal orientation) model with 5 components: 
#   
#   log-likelihood   n  df       BIC       ICL
# 160.1269 350 190 -792.7536 -798.8695
# 
# Clustering table:
#   1   2   3   4   5 
# 23 163  76  27  61 

# de acuerdo al BIC los modelos inicializados en PCS y/o VARS tuvieron mayor soporte emírico:
#"PCS"
mclust.options(hcUse = "PCS")
Mcluster.phenodata <- Mclust(data.for.GMM, G = 1:9)

#Resultados:
Mcluster.phenodata
summary(Mcluster.phenodata)
names(Mcluster.phenodata$classification)#especímenes incluidos en el análisis
Mcluster.phenodata$classification #clasificación de los especímenes
Mcluster.phenodata$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata)
#
#   Mclust VVE (ellipsoidal, equal orientation) model with 5
# components:
#
#   log-likelihood   n  df       BIC       ICL
#     295.0292      350 190     -522.9489 -526.2676
#
# Clustering table:
#   1   2   3   4   5
#   85 107  58  22  78


# Graficar de los grupos morfológicos, de acuerdo con el mejor modelo, eg. graficar el mejor modelo de mezclas
# normales en la dimension 1 vs 2 (PCs)
plot(Mcluster.phenodata, what = "classification", dimens = c(1, 2))

# Graficar el soporte empírico de los diferentes modelos
plot(Mcluster.phenodata, what = "BIC")

#Graficar soporte empírico para el mejor modelo
BIC.Best.Model.Per.G <-
  apply(Mcluster.phenodata$BIC, 1, max, na.rm = T)
max.BIC <- max(BIC.Best.Model.Per.G)

#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 6, 4, 2))
plot(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  type = "n",
  bty = "n",
  xlim = c(1, 9),
  ylim = c(2000, 0),
  yaxt = "n",
  xaxt = "n",
  xlab = "Número de grupos morfológicos",
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
  pch = 20,
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
     at = seq(2000, 0,-100),
     tcl = -0.7,
     cex.axis = 1.5)
abline(v = Mcluster.phenodata$G, lty = 3)# para determinar el modelo con el mejor soporte

# Guardar el mejor modelo en el directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Directorio de Iván Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván Waterman
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")
# save(Mcluster.phenodata,
#      file = paste(
#        "Mcluster.phenodata_",
#        format(Sys.time(), "%Y%B%d"),
#        ".RData",
#        sep = ""
#      ))
#load("Mcluster.phenodata_2023agosto19.RData")

#################################################################################################################
#################################################################################################################
# 5) Examinar grupos morfológicos en el mejor modelo de mezclas normales####
#################################################################################################################
#################################################################################################################

# Cargar el mejor modelo de mezcla normal
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")# directorio de Diana
load("Mcluster.phenodata_2023agosto19.RData")
load("MeanPhenodataSelected_2023septiembre07_052114.RData")
load("MeanPhenodata_2023septiembre07_050654.RData")

###################################################################################################################
# 5.1)Examinar y guardar en un documento para asignación de especímenes a los grupos grupos morfológicos.

# Crear y escribir archivo para la asignación de los grupos morfológicos.
phenotypic.group.assignment <-
  data.frame(
    as.numeric(rownames(mean.phenodata.selected)),
    mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), c(1, 4, 5, 6)],
    Mcluster.phenodata$classification,
    Mcluster.phenodata$uncertainty,
    Mcluster.phenodata$data
  )
colnames(phenotypic.group.assignment) <-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Longitude",
    "Latitude",
    "Altitude",
    "Phenotypic.Group",
    "Uncertainty",
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "PC11",
    "PC12"
  )
head(phenotypic.group.assignment)
View(phenotypic.group.assignment)
# Guardar archivo con la asignación de especímenes a grupos morfológicos
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# write.csv(
#   phenotypic.group.assignment,
#   file = paste(
#     "PhenotypicGroupAssignment_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )
#"PhenotypicGroupAssignment_2023septiembre08_120644.csv")

#Examinar los grupos morfológicos sólos de las plantas madre
phenotypic.group.assignment.piloto <-
  phenotypic.group.assignment[308:350,]
View(phenotypic.group.assignment.piloto)#Las plantas madre se encuentran en cuatro de los cinco grupos morfológicos

#################################################################################################################
# 5.2) Graficar el soporte empírico para el mejor modelo para cada grupo morfológico.

# Directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/Figuras")# Directorio de Diana

# Graficar soporte empírico para el mejor modelo de mezclas normales
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 6, 4, 2))
plot(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  type = "n",
  bty = "n",
  xlim = c(1, 9),
  ylim = c(2000, 0),
  yaxt = "n",
  xaxt = "n",
  xlab = "Número de grupos morfológicos",
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
  pch = 20,
  col = "black",
  lwd = 1
)
# Mostrar el mejor modelo
# Agregar ejes
axis(
  1,
  at = c(1, seq(2, 9, 1)),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(2,
     at = seq(2000, 0,-100),
     tcl = -0.7,
     cex.axis = 1.5)
abline(v = Mcluster.phenodata$G, lty = 3) #para determinar el modelo con el mejor soporte
title(expression("A)"), adj = 0)

# Acotando soporte empírico entre 0-200
par(mar = c(5, 6, 4, 2))
plot(
  1:9,
  max.BIC - BIC.Best.Model.Per.G[1:9],
  type = "n",
  bty = "n",
  xlim = c(1, 9),
  ylim = c(200, 0),
  yaxt = "n",
  xaxt = "n",
  xlab = "Número de grupos morfológicos",
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
  pch = 20,
  col = "black",
  lwd = 1
)
# Mostrar el mejor modelo
# Agregar ejes
axis(
  1,
  at = c(1, seq(2, 9, 1)),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(2,
     at = seq(200, 0,-50),
     tcl = -0.7,
     cex.axis = 1.5)
abline(v = Mcluster.phenodata$G, lty = 3) #para determinar el modelo con el mejor soporte
title(expression("B)"), adj = 0)

#################################################################################################################
# 5.3) Graficar grupos morfológicos en el mejor modelo de mezclas normales.
# Seleccionar directorio de trabajo : datos
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")# Diana's directory
#load("MeanPhenodataSelectedLogPca_2023agosto15_190054.RData")
#load("Mcluster.phenodata_2023agosto19.RData")
#summary(mean.phenodata.selected.log.pca)

# Directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/Figuras")# directorio de Diana

#################################################################################################################
# 5.3.1) Con todos los datos

#Escogerla dimensión para graficar la clasificación (x, y):
x <- 1
y <- 2
dimens <- c(x, y)
loadings <- summary(mean.phenodata.selected.log.pca)

# Puede usar la función plot de mclust, para examinar rápidamente:
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens)

# o puede hacer gratificarla manualmente:
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = "",
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex.axis = 1.5,
  cex.lab = 1.5 ,
  asp = 1
)
legend(
  "bottomleft",
  paste("M", 1:5),
  col = mclust.options("classPlotColors"),
  xpd = T,
  ncol = 3,
  pch = mclust.options("classPlotSymbols"),
  pt.lwd = 0.8,
  pt.cex = 0.9,
  cex = 0.9,
  bty = "o"
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
# Agregar etiquetas de las elipses
for (i in 1:Mcluster.phenodata$G) {
text(
  Mcluster.phenodata$parameters$mean[dimens,][1, i],
  Mcluster.phenodata$parameters$mean[dimens,][2, i],
  paste0("M",i),
  cex = 0.9
)
}
#################################################################################################################
# 5.3.2) sólo las plantas madre

#Escogerla dimensión para graficar la clasificación (x, y):
x <- 1
y <- 2
dimens <- c(x, y)

#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = "",
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = 0,
  asp = 1
)
#Agregar plantas madres
for (i in 1:Mcluster.phenodata$G) {
  points(
    phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[, 6] ==
                                         i, dimens+7],
    col = mclust.options("classPlotColors")[i],
    pch = mclust.options("classPlotSymbols")[i]
  )
}
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
#Agregar etiquetas de las elipses
for (i in 1:Mcluster.phenodata$G) {
  text(
    Mcluster.phenodata$parameters$mean[dimens,][1, i],
    Mcluster.phenodata$parameters$mean[dimens,][2, i],
    paste0("M",i),
    cex = 0.9
  )
}

#################################################################################################################
# 5.4) Graficar coeficientes para diferentes caracteres morfológicos en el espacio de los componentes principales
#Escogerla dimensión para graficar los coeficientes en "x" y "y" dimensiones (x, y):
x <- 1
y <- 2
dimens <- c(x, y)

#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  0,
  0,
  xlim = c(-0.5, 0.5),
  ylim = c(-0.6, 0.6),
  asp = 1,
  type = "n",
  bty = "n",
  axes = F,
  xlab = paste0("Coeficientes ", colnames(mean.phenodata.selected.log.pca$rotation)[x]),
  ylab = paste0("Coeficientes ", colnames(mean.phenodata.selected.log.pca$rotation)[y]),
  # cex.axis = 1.5,
  # cex.lab = 1.5
)
# Escala
l <- 0.2
p <- 0.02
segments(-l, 0, l, 0, lty = 3, lend = 0)
segments(0, -l, 0, l, lty = 3, lend = 0)
segments(-l, -p, -l, p, lty = 3, lend = 0)
segments(-p, -l, p, -l, lty = 3, lend = 0)
segments(l, -p, l, p, lty = 3, lend = 0)
segments(-p, l, p, l, lty = 3, lend = 0)

# Hallar círculo para igual contribución de los caracteres.
x.circ <- seq(-(2 / 13) ^ 0.5, (2 / 13) ^ 0.5, 1e-4)
x.circ <- c(x.circ, (2 / 13) ^ 0.5)
radio <- x.circ[length(x.circ)]
equal_contribution <- ((2 / 13) - x.circ ^ 2) ^ 0.5

# Encontrar las variables con maor contribución:
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
punto.0 <- data.frame(x=0, y=0)
coef.contribucion <- vector()
j <- 0
for(i in 1:dim(mean.phenodata.selected.log.pca$rotation)[1]){
  modulo <- euc.dist(punto.0, mean.phenodata.selected.log.pca$rotation[, dimens][i,])
  if(modulo>=radio){
  j <- j+1
  coef.contribucion[j] <- i
  }
}
coef.contribucion

#Graficar las variables con mayor contribución
for (i in coef.contribucion) {
  arrows(
    0,
    0,
    x1 = mean.phenodata.selected.log.pca$rotation[, x][i],
    y1 = mean.phenodata.selected.log.pca$rotation[, y][i],
    length = 0.1,
    angle = 20,
    code = 2,
    col = "black",
    lwd = 1
  )
}
# Graficar círculo para igual contribución de los caracteres.
points(
  x.circ,
  equal_contribution,
  type = "l",
  lty = 1,
  col = "gray70"
)
points(
  x.circ,-equal_contribution,
  type = "l",
  lty = 1,
  col = "gray70"
)

# Anotar las variables de mayor contribución:

trait.names <-
  c(
    "Longitud de la lámina",
    "Ancho de la lámina",
    "Longitud del eje de\n la sinflorescencia",
    "Perímetro del eje de\n la sinflorescencia",
    "Pares de hojas estériles\n por sinflorescencia",
    "Capítulos por sinflorescencia",
    "Longitud del pedúnculo\n de la cima terminal",
    "Diámetro del capítulo",
    "Longitud de la\n filaria estéril",
    "Ancho de la\n filaria estéril",
    "Longitud de la corola\n de la flor del radio",
    "Longitud de la corola\n de la flor del disco",
    "Longitud del tubo de\n la flor del disco")

for (i in coef.contribucion) {
text(
  mean.phenodata.selected.log.pca$rotation[, x][i]* 1.1,
  mean.phenodata.selected.log.pca$rotation[, y][i] * 1.1,
  trait.names[i],
  cex = 0.9
)
}

#Examinar los caracteres morfológicos con mayor contribución
cluster.mean.phenodata.selected<-
  data.frame(mean.phenodata.selected, Mcluster.phenodata$classification)

for (i in coef.contribucion) {
  print(trait.names[i])
  print(tapply(
    cluster.mean.phenodata.selected[,i],
    cluster.mean.phenodata.selected$Mcluster.phenodata.classification,
    mean
  ))
}
# [1] "Pares de hojas estériles\n por sinflorescencia"
# 1        2        3        4        5 
# 0.000000 1.959100 0.000000 1.858025 0.000000 
# [1] "Capítulos por sinflorescencia"
# 1         2         3         4         5 
# 3.123188 14.278119 16.893640 21.129630  5.663934 
# [1] "Longitud del pedúnculo\n de la cima terminal"
# 1         2         3         4         5 
# 10.691522  2.603414  2.250509  2.916457  6.060317 
# [1] "Ancho de la\n filaria estéril"
# 1         2         3         4         5 
# 6.458913  7.192669  4.617917  4.304815 12.867596 

#################################################################################################################
# 5.5) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata$uncertainty)
#       Min.  1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000000 0.0000010 0.0000355 0.0073859 0.0007149 0.4844362

#De los 350 especímenes, 7 tiene valores de incertidumbre mayores a 0.1
#esto es el 2% de los especímenes
sum(Mcluster.phenodata$uncertainty > 0.1)
sum(Mcluster.phenodata$uncertainty > 0.1) / length(Mcluster.phenodata$uncertainty)

# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata$classification[Mcluster.phenodata$uncertainty > 0.1]
# 88  107  128  585  807  961 1039
# 4    4    2    4    4    1    2

#Resumen de los valores de incertidumbre para las plantas madre
summary(Mcluster.phenodata$uncertainty[308:350])
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000000 0.0000007 0.0000320 0.0129535 0.0003625 0.4816502

sum(Mcluster.phenodata$uncertainty[308:350] > 0.1)# de las 43 plantas madre, 1 planta madre tuvo incertidumbre >0.1
sum(Mcluster.phenodata$uncertainty[308:350] > 0.1) / length(Mcluster.phenodata$uncertainty[308:350])# 2.33%

Mcluster.phenodata$classification[308:350][Mcluster.phenodata$uncertainty[308:350] >
                                             0.1]
# 1039
#    2

#Graficar la función de distribución acumulativa de valores de incertidumbre
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  sort(Mcluster.phenodata$uncertainty),
  1:length(Mcluster.phenodata$uncertainty) / length(Mcluster.phenodata$uncertainty),
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

# Graficar los especímenes con incertidumbre > 0.1
#Escogerla dimensión para graficar la clasificación (x, y):
x <- 1
y <- 2
dimens <- c(x, y)

#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = "",
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex = 0,
  cex.lab = 1.5,
  cex.axis = 1.5,
  asp = 1
)
#Agregar especímenes con incertidumbre >0.1
for (i in 1:Mcluster.phenodata$G) {
  points(
    Mcluster.phenodata$data[Mcluster.phenodata$uncertainty > 0.1 &
                              Mcluster.phenodata$classification == i, 1:2],
    pch = mclust.options("classPlotSymbols")[i],
    col = mclust.options("classPlotColors")[i]
  )
}
#Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}

#Agregar etiquetas de las elipses
for (i in 1:Mcluster.phenodata$G) {
  text(
    Mcluster.phenodata$parameters$mean[dimens,][1, i],
    Mcluster.phenodata$parameters$mean[dimens,][2, i],
    paste0("M",i),
    cex = 0.9
  )
}


legend(
  "bottomright",
  paste0("M", sort(unique(Mcluster.phenodata$classification[Mcluster.phenodata$uncertainty > 0.1]))),
  col = mclust.options("classPlotColors")[sort(unique(Mcluster.phenodata$classification[Mcluster.phenodata$uncertainty > 0.1]))],
  pch = mclust.options("classPlotSymbols")[sort(unique(Mcluster.phenodata$classification[Mcluster.phenodata$uncertainty > 0.1]))],
  pt.lwd = 1,
  pt.cex = 1,
  cex = 1.3,
  bty = "o"
)
#################################################################################################################
#################################################################################################################
# 6) Examinar la localización  delos especímenes tipos E. cabrerensis y E. miradorensis en el mejor modelo####
# de mezclas normales.
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 6.1) Espeletia cabrerensis
mean.phenodata.cabrerensis.log <-
  as.numeric(log(mean.phenodata[which(mean.phenodata[, 20] == "Espeletia.cabrerensis"), 7:17] + c(rep(0, 4), 1, rep(0, 6))))
diff.cabrerensis <-
  apply(
    as.matrix(mean.phenodata.selected.log[,-c(12, 13)]),
    MARGIN = 1,
    FUN = function(x)
      x - mean.phenodata.cabrerensis.log
  )
diff.cabrerensis[, 1:10]
(diff.cabrerensis)[, 1:10]

dist.Euclidian.cabrerensis <- sqrt(colSums(diff.cabrerensis ^ 2))
sort(dist.Euclidian.cabrerensis)
o.cabrerensis <- order(dist.Euclidian.cabrerensis)
hist(dist.Euclidian.cabrerensis, breaks = 25)
mean.phenodata[c("983", "984", "1009"),]
mean.phenodata[which(mean.phenodata[, 20] == "Espeletia.cabrerensis"),]
Mcluster.phenodata$classification[o.cabrerensis[1:15]]
# 983  984 1009  348  349  671  286  791  669  806    7  982  719  340
# 3    3    3    3    3    3    3    3    3    3    3    3    3    3
# 346
# 2

###################################################################################################################
# 6.2) Espeletia miradorensis
mean.phenodata.miradorensis.log <-
  as.numeric(log(mean.phenodata[which(mean.phenodata[, 20] == "Espeletia.miradorensis"), 7:17] + c(rep(0, 4), 1, rep(0, 6))))
diff.miradorensis <-
  apply(
    as.matrix(mean.phenodata.selected.log[,-c(12, 13)]),
    MARGIN = 1,
    FUN = function(x)
      x - mean.phenodata.miradorensis.log
  )
diff.miradorensis[, 1:10]
(diff.miradorensis ^ 2)[, 1:10]

dist.Euclidian.miradorensis <- sqrt(colSums(diff.miradorensis ^ 2))
sort(dist.Euclidian.miradorensis)
o.miradorensis <- order(dist.Euclidian.miradorensis)
hist(dist.Euclidian.miradorensis, breaks = 25)
mean.phenodata[c("794", "798", "800"),]
mean.phenodata[which(mean.phenodata[, 20] == "Espeletia.miradorensis"),]
Mcluster.phenodata$classification[o.miradorensis[1:15]]
# 794  798  800  799  793  290 1007  880 1011 1010  651  689  911  796
# 2    2    2    2    2    2    2    2    2    2    2    2    2    2
# 795
# 2

#################################################################################################################
#################################################################################################################
# 7) Examinar la distribución de los especímenes citados en la monografía de Espeletiinae (Cuatrecasas 2013)#####
#    en el espacio morfológico.
#################################################################################################################
#################################################################################################################

# Directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")# directorio de Diana

#Cargar los caracteres morfológicos crudos
#load("MeanPhenodata_2023septiembre07_050654.RData")

# Cargar el mejor modelo de mezcla normal
#load("Mcluster.phenodata_2023agosto19.RData")

# Cargar los datos morfolóficos seleccionados sin NA
#load("MeanPhenodataSelected_2023septiembre07_052114.RData")

#################################################################################################################
# 7.1)  Examinar la distribución de los tipos  entre grupos morfológicos.
#
colnames(mean.phenodata)
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            1], 20])
# Espeletia.summapacis
# 1
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            2], 20])
# Espeletia tapirophila   Espeletia.grandiflora.fma.multiflora
# 1                                    1
# Espeletia.grandiflora.fma.reducta
# 1
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            3], 20])
#< table of extent 0 >
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            4], 20])
# < table of extent 0 >
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            5], 20])
# Espeletia.killipii
# 1
#
unique(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20])
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20])]
table(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20])])

# Crear marco de datos para los especímenes tipo de acuerdo con la clasificación, incertidumbre y coordenadas de
# los tres primeros componentes principales
type.classification <-
  data.frame(
    mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 20],
    Mcluster.phenodata$classification,
    Mcluster.phenodata$uncertainty,
    Mcluster.phenodata$data[, 1:3]
  )
type.classification <-
  type.classification[!is.na(type.classification[, 1]),]
colnames(type.classification)[1] <- colnames(mean.phenodata)[20]
type.classification <-
  type.classification[order(type.classification[, 1], type.classification[, 2]),]

# Guardar tabla de clasificación de los especímen tipo
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")# directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# write.csv(
#   type.classification,
#   file = paste(
#     "TypeClassification_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#################################################################################################################
# 7.2)  Examinar la distribución de los especímenes citados en la monografía de Espeletiinae (Cuatrecasas 2013)
# entre los grupos morfológicos.
#
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            1], 21])
# Espeletia.summapacis
# 4
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            2], 21])
# Espeletia.argentea.fma.phaneractis
# 1
# Espeletia.grandiflora.ssp.grandiflora.var.attenuata
# 2
# Espeletia.grandiflora.ssp.grandiflora.var.grandiflora
# 7
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            3], 21])
#< table of extent 0 >
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            4], 21])
# Espeletia.grandiflora.ssp.grandiflora.var.attenuata
# 1
table(mean.phenodata[as.numeric(names(Mcluster.phenodata$classification))[Mcluster.phenodata$classification ==
                                                                            5], 21])
# Espeletia.killipii   Espeletia.killipii.var.chisacana
# 3                                3
# Espeletia.killipii.var.killipii
# 1

unique(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21])
mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21])]
table(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21][!is.na(mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21])])

# Crear marco de datos con esta información para los especímenes citado en la monografía de Espeletiinae
#(Cuatrecasas 2013) con la clasificación, incertidumbre y coordenadas de los tres principales componentes
cited.specimen.classification <-
  data.frame(
    mean.phenodata[as.numeric(rownames(mean.phenodata.selected)), 21],
    Mcluster.phenodata$classification,
    Mcluster.phenodata$uncertainty,
    Mcluster.phenodata$data[, 1:3]
  )
cited.specimen.classification <-
  cited.specimen.classification[!is.na(cited.specimen.classification[, 1]),]
colnames(cited.specimen.classification)[1] <-
  colnames(mean.phenodata)[21]
cited.specimen.classification <-
  cited.specimen.classification[order(cited.specimen.classification[, 1],
                                      cited.specimen.classification[, 2]),]
cited.specimen.classification

# Guardar tabla de clasificación de los especímen citados en la monografía de Espeletiinae
#setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")# directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
#write.csv( cited.specimen.classification, file = paste( "CitedSpecimenClassification_", format(Sys.time(),
#"%Y%B%d_%H%M%S"), ".csv", sep = "" ), row.names = F )

#################################################################################################################
#7.3)  Examinar la distribución de los especímenes tipo  y los citados en la monografía de Espeletiinae
#(Cuatrecasas, 2013) en el mejor modelo de mezclas normales

#Escogerla dimensión para graficar la clasificación (x, y):
x <- 1
y <- 3
dimens <- c(x, y)

# Escoger especímenes citados en la monografía de Cuatrecasas
unique(cited.specimen.classification$Cited.Cuatrecasas.Monograph.Taxa)
# [1] "Espeletia.argentea.fma.phaneractis"                   
# [2] "Espeletia.grandiflora.ssp.grandiflora.var.attenuata"  
# [3] "Espeletia.grandiflora.ssp.grandiflora.var.grandiflora"
# [4] "Espeletia.killipii"                                   
# [5] "Espeletia.killipii.var.chisacana"                     
# [6] "Espeletia.killipii.var.killipii"                      
# [7] "Espeletia.summapacis"

cited <- unique(cited.specimen.classification$Cited.Cuatrecasas.Monograph.Taxa)[c(2,3)]# uno o varios

par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = paste("Citados",cited),
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = 0
)
#Agregar especímenes citados
for (i in 1:Mcluster.phenodata$G) {
  #Agregar especímenes citados
  points(
    cited.specimen.classification[cited.specimen.classification[, 2] == i & cited.specimen.classification[, 1] == cited, dimens+3],
    col = mclust.options("classPlotColors")[i],
    pch = mclust.options("classPlotSymbols")[i]
  )
}
# Leyenda
legend(
  "bottomright",
  paste0("M", 1:Mcluster.phenodata$G),
  col = mclust.options("classPlotColors"),
  pch = mclust.options("classPlotSymbols"),
  bty = "o",
  horiz = T
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
# Agregar etiquetas de las elipses
for (j in 1:Mcluster.phenodata$G) {
  text(
    Mcluster.phenodata$parameters$mean[dimens,][1, j],
    Mcluster.phenodata$parameters$mean[dimens,][2, j],
    paste0("M", j),
    font = 2
  )
}
title(paste(cited, "citados"))


# Escoger especímenes tipo en la monografía de Cuatrecasas
unique(type.classification$Type.Taxa)
# [1] "Espeletia tapirophila"                "Espeletia.grandiflora.fma.multiflora"
# [3] "Espeletia.grandiflora.fma.reducta"    "Espeletia.killipii"                  
# [5] "Espeletia.summapacis"

type <- unique(type.classification$Type.Taxa)[c(4)]# uno o varios

par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = paste("Citados",cited),
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = 0
)
#Agregar especímenes citados
for (i in 1:Mcluster.phenodata$G) {
  #Agregar tipos citados
  points(
    type.classification[type.classification[, 2] == i & type.classification[,1] == type, dimens+3],
    col = mclust.options("classPlotColors")[i],
    pch = mclust.options("classPlotSymbols")[i]
  )
}
# Leyenda
legend(
  "bottomright",
  paste0("M", 1:Mcluster.phenodata$G),
  col = mclust.options("classPlotColors"),
  pch = mclust.options("classPlotSymbols"),
  bty = "o",
  horiz = T
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
# Agregar etiquetas de las elipses
for (j in 1:Mcluster.phenodata$G) {
  text(
    Mcluster.phenodata$parameters$mean[dimens,][1, j],
    Mcluster.phenodata$parameters$mean[dimens,][2, j],
    paste0("M", j),
    font = 2
  )
}
title(paste(type, "tipo"))


#################################################################################################################
# 7.7) Graficar especímenes similares a E. cabrerensis o E. miradorensis

#Escogerla dimensión para graficar la clasificación (x, y):
x <- 1
y <- 3
dimens <- c(x, y)

# Escoger qué especímenes graficar:
cabrerensis <- o.cabrerensis[1:3]
miradorensis <- o.miradorensis[1:3]

#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = "",
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex = 0,
  cex.lab = 1.5,
  cex.axis = 1.5,
  asp = 1
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}


# Agregar especímenes de Espeletia cabrerensis
points(
  Mcluster.phenodata$data[cabrerensis, x],
  Mcluster.phenodata$data[cabrerensis, y],
  pch = mclust.options("classPlotSymbols")[Mcluster.phenodata$classification[cabrerensis[1:3]]],
  cex = 1.5,
  col = mclust.options("classPlotColors")[Mcluster.phenodata$classification[cabrerensis[1:3]]]
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
# Agregar etiquetas de las elipses
for (j in 1:Mcluster.phenodata$G) {
  text(
    Mcluster.phenodata$parameters$mean[dimens,][1, j],
    Mcluster.phenodata$parameters$mean[dimens,][2, j],
    paste0("M", j),
    font = 2
  )
}
title("más similares al tipo E. cabrerensis")

# o graficar los especímenes más similares a E. miradorensis
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata,
  what = c("classification"),
  dimens = dimens,
  main = "",
  addEllipses = F,
  xlab = paste0(colnames(Mcluster.phenodata$data)[x], " (",round((loadings$importance[2,x]*100),1), " % varianza)"),
  ylab = paste0(colnames(Mcluster.phenodata$data)[y], " (",round((loadings$importance[2,y]*100),1), " % varianza)"),
  cex = 0,
  cex.lab = 1.5,
  cex.axis = 1.5,
  asp = 1
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}


# Agregar especímenes de Espeletia miradorensis
points(
  Mcluster.phenodata$data[miradorensis, x],
  Mcluster.phenodata$data[miradorensis, y],
  pch = mclust.options("classPlotSymbols")[Mcluster.phenodata$classification[miradorensis[1:3]]],
  cex = 1.5,
  col = mclust.options("classPlotColors")[Mcluster.phenodata$classification[miradorensis[1:3]]]
)
# Agregar elipses
for (i in 1:Mcluster.phenodata$G) {
  points(
    ellipse(
      x = Mcluster.phenodata$parameters$variance$sigma[dimens, dimens, i],
      centre = Mcluster.phenodata$parameters$mean[dimens, i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
# Agregar etiquetas de las elipses
for (j in 1:Mcluster.phenodata$G) {
  text(
    Mcluster.phenodata$parameters$mean[dimens,][1, j],
    Mcluster.phenodata$parameters$mean[dimens,][2, j],
    paste0("M", j),
    font = 2
  )
}
title("más similares al tipo E. miradorensis")

#################################################################################################################
#################################################################################################################
# 8) Distribución altitudinal de los especímenes en relación con la asignación de su grupo morfológico.####
#################################################################################################################
#################################################################################################################
#Histograma de cada uno de los grupos morfológicos

setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/Figuras")#Directorio de Diana

# Grupo 1
range(phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 1, 5], na.rm =
        T)#3877 4076
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 1, 5],
  ylab = "Especímenes",
  xlab = "",
  xaxt = "n",
  yaxt = "n",
  main = "",
  cex.lab=1.5,
  col = "gray90",
  breaks = seq(2800, 4200, 100),
  ylim = c(0, 80)
)
legend(
  2800,
  80,
  fill = c("gray90", "gray60"),
  legend = c("Todos los especímenes", "Plantas madre"),
  )
title(expression("A) M1"), adj = 0, cex.main =1.5)
axis(side=1, at=seq(2800, 4200, 400), labels = T, tcl=-0.5, cex.axis =1.5)
axis(side=1, at=seq(2800, 4200, 100), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,10), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,20), labels = T, tcl=-0.5, cex.axis = 1.5, las =2)

# Grupo 2
range(phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 2, 5], na.rm =
        T)#2800 4147
range(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6] == 2, 5], na.rm =
        T)#  3567 3797
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 2, 5],
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  main = "",
  cex.lab=1.5,
  col = "gray90",
  breaks = seq(2800, 4200, 100),
  ylim = c(0, 80)
)
hist(
  phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[, 6] ==
                                       2, 5],
  add = T,
  breaks = seq(2800, 4200, 100),
  col = "gray60"
)
title(expression("B) M2"), adj = 0, cex.main = 1.5)
axis(side=1, at=seq(2800, 4200, 400), labels = T, tcl=-0.5, cex.axis =1.5)
axis(side=1, at=seq(2800, 4200, 100), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,10), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,20), labels = T, tcl=-0.5, cex.axis = 1.5, las =2)

# Grupo 3
range(phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 3, 5], na.rm =
        T)#3394 4062
range(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6] == 3, 5], na.rm =
        T)#3567 3863
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 3, 5],
  #breaks=6,
  xlab = "",
  ylab = "Especímenes",
  xaxt = "n",
  yaxt = "n",
  main = "",
  cex.lab=1.5,
  col = "gray90",
  breaks = seq(2800, 4200, 100),
  ylim = c(0, 80)
)
hist(
  phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[, 6] ==
                                       3, 5],
  add = T,
  breaks = seq(2800, 4200, 100),
  col = "gray60"
)
title(expression("C) M3"), adj = 0, cex.main = 1.5)
axis(side=1, at=seq(2800, 4200, 400), labels = T, tcl=-0.5, cex.axis =1.5)
axis(side=1, at=seq(2800, 4200, 100), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,10), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,20), labels = T, tcl=-0.5, cex.axis = 1.5, las =2)

# Grupo 4
range(phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 4, 5], na.rm =
        T)#3394 3920
range(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6] == 4, 5], na.rm =
        T)#3560 3759
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 4, 5],
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  main = "",
  cex.lab=1.5,
  col = "gray90",
  breaks = seq(2800, 4200, 100),
  ylim = c(0, 80)
)
hist(
  phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[, 6] ==
                                       4, 5],
  add = T,
  breaks = seq(2800, 4200, 100),
  col = "gray60"
)
title(expression("D) M4"), adj = 0, cex.main = 1.5)
axis(side=1, at=seq(2800, 4200, 400), labels = T, tcl=-0.5, cex.axis =1.5)
axis(side=1, at=seq(2800, 4200, 100), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,10), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,20), labels = T, tcl=-0.5, cex.axis = 1.5, las =2)

# Grupo 5
range(phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 5, 5], na.rm =
        T)#3250 4062
range(phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[,6] == 5, 5], na.rm =
        T)#3591 3855
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  phenotypic.group.assignment[phenotypic.group.assignment[, 6] == 5, 5],
  xlab = "Altitud (m)",
  ylab = "Especímenes",
  xaxt = "n",
  yaxt = "n",
  main = "",
  cex.lab=1.5,
  col = "gray90",
  breaks = seq(2800, 4200, 100),
  ylim = c(0, 80)
)
hist(
  phenotypic.group.assignment.piloto[phenotypic.group.assignment.piloto[, 6] ==
                                       5, 5],
  add = T,
  breaks = seq(2800, 4200, 100),
  col = "gray60"
)
title(expression("E) M5"), adj = 0, cex.main = 1.5)
axis(side=1, at=seq(2800, 4200, 400), labels = T, tcl=-0.5, cex.axis =1.5)
axis(side=1, at=seq(2800, 4200, 100), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,10), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,20), labels = T, tcl=-0.5, cex.axis = 1.5, las =2)

# Todos los especímenes
range(phenotypic.group.assignment[, 5], na.rm =
        T)#2800 4147
range(phenotypic.group.assignment.piloto[, 5], na.rm =
        T)#  3560 3863
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  phenotypic.group.assignment$Altitude,
  xlab = "Altitud (m)",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  main = "",
  cex.lab=1.5,
  col = "gray90",
  breaks = seq(2800, 4200, 100),
  ylim = c(0, 80)
)
hist(
  phenotypic.group.assignment$Altitude[308:350],
  add = T,
  breaks = seq(2800, 4200, 100),
  col = "gray60"
)
title(expression("F) Todos los grupos morfológicos"), adj = 0, cex.main = 1.5)
axis(side=1, at=seq(2800, 4200, 400), labels = T, tcl=-0.5, cex.axis =1.5)
axis(side=1, at=seq(2800, 4200, 100), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,10), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,80,20), labels = T, tcl=-0.5, cex.axis = 1.5, las =2)

#################################################################################################################
#################################################################################################################
# 9) Tabla clasificación cruzada entre grupos morfológicos y resultados de Pineda et al. y el estadístico tau ####
# para encontrar grado de concordancia entre éstos####
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 9.1) lectura de tabla de asignación de grupos morfológicos según Pineda et al.

# Directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

# Leer tablas de datos morfológicos: examinar y resumir los datos
# Datos usados en Pineda et al.
phenotypic.group.pineda <-
  read.table("PhenotypicGroupAssignmentPineda_2023junio01_095823.csv",
             header = T,
             sep = ",")
head(phenotypic.group.pineda )
colnames(phenotypic.group.pineda )
dim(phenotypic.group.pineda )
View(phenotypic.group.pineda )

#################################################################################################################
# 9.2) Tabla de clasificación cruzada
morfologia.Pineda.all<-merge(
  phenotypic.group.assignment[, c(2,6)],
  phenotypic.group.pineda[, c(2,3)],
  by="Collector.Collection.Number",
  suffixes = c(".pineda", ".all")
)
head(morfologia.Pineda.all )
colnames(morfologia.Pineda.all )
dim(morfologia.Pineda.all )
View(morfologia.Pineda.all )

table(morfologia.Pineda.all[,3],
      morfologia.Pineda.all[,2])
#    1  2  3  4  5
# 1 23  0 58  2 47
# 2  0 46  0  2  0
# 3  0 49  0  3  0
# 4  0  4  0 13  0
# 5  0 37  0  2  0
# 6  0 21  0  0  0

#################################################################################################################
# 9.3) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de morfológicos de los
#especímenes con los encontrado en Pineda et al. (2020): tau(M, P) y tau(P, M)

colnames(morfologia.Pineda.all)

GKtau(
  morfologia.Pineda.all$Phenotypic.Group.all,
  morfologia.Pineda.all$Phenotypic.Group.pineda
)
# xName
# 1 morfologia.Pineda.all$Phenotypic.Group.all
# yName Nx Ny tauxy tauyx
# 1 morfologia.Pineda.all$Phenotypic.Group.pineda  6  5 0.501 0.431

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(morfologia.Pineda.all$Phenotypic.Group.pineda)
  GKtau.nulo <-
    GKtau(morfologia.Pineda.all$Phenotypic.Group.all,
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}
# Gráfica de la distribución nula de tau(M,P),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(M, P)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    morfologia.Pineda.all$Phenotypic.Group.all,
    morfologia.Pineda.all$Phenotypic.Group.pineda
  )[[5]],
  col = "red"
)
# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(S,M) al menos tan extremo como el observado):
sum(
  GKtau(
    morfologia.Pineda.all$Phenotypic.Group.all,
    morfologia.Pineda.all$Phenotypic.Group.pineda
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0

# Gráfica de la distribución nula de tau(P, M),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(P,S)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    morfologia.Pineda.all$Phenotypic.Group.all,
    morfologia.Pineda.all$Phenotypic.Group.pineda
  )[[6]],
  col = "red"
)
# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M,S) al menos tan extremo como el observado):
sum(
  GKtau(
    morfologia.Pineda.all$Phenotypic.Group.all,
    morfologia.Pineda.all$Phenotypic.Group.pineda
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0

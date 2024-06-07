#################################################################################################################
#################################################################################################################
#################################################################################################################

#CONCORDANCIA ENTRE GRUPOS DE PLANTAS MADRE SEGÚN MORFOLOGÍA Y SEGÚN EL CRECIMENTO DE LA PROGENIE EN EL JARDÍN
#COMÚN EN TRES MUESTREOS: 11.4 meses, 19.6 meses y 51.4 meses después de la siembra

#################################################################################################################
#################################################################################################################
#################################################################################################################

#INTRODUCCIÓN

#Este código determina la concordancia entre los grupos de plantas madre estimados según sus caracteres
#morfológicos y según el crecimiento de la progenie en el jardín común. El grado de concordancia indica la medida
#en la que los grupos morfológicos de frailejones silvestres corresponden a especies que difieren en el
#reclutamiento y crecimiento durante los primeros años de vida. Una alta concordancia apoyaría empíricamente una
#faceta de la predicción derivada de la hipótesis del singameón, según la cual los grupos morfológicos de
#frailejones no son demográficamente intercambiables porque difieren en la ecología de las etapas ontogenéticas
#tempranas.

#REQUERIMIENTOS

# Tablas de de asignación de grupos de plantas madre según crecimiento:

# "grupos.crecimiento.(marzo).VARS_2023diciembre28_081348.csv", Asignación de grupos de plantas madre en el
# primer muestreo (11.4 meses después de la siembra)

# "grupos.crecimiento.(marzo).PCR_2023diciembre28_082916.csv", asignación de grupos de plantas madre en el primer
# muestreo (11.4 meses después de la siembra)

# "grupos.crecimiento.(octubre)_2023diciembre28_132240.csv", asignación de grupos de plantas madre en el segundo
# muestreo (19.6 meses después de la siembra)

# "grupos.crecimiento.(junio).VARS_2023diciembre28_140052.csv", asignación de grupos de plantas madre en el
# tercer muestreo (51.4 meses después de la siembra)

# "PhenotypicGroupAssignment_2023septiembre08_120644.csv", asignación de grupo morfológico de los especímens de
# plantas madre.


# CONTENIDO

# 1) Preliminares: cargar las librerías y lectura de datos

# 2) Creación de tabla con la asignación de grupo para cada modelo por cada muestreo y por morfología

# 3) Tabla de clasificación cruzada entre modelos

# 4) Estadístico Goodman-Kruskal tau para la concordancia entre grupos de crecimiento entre muestreos y
#entre morfología

#################################################################################################################
#################################################################################################################
#################################################################################################################

#################################################################################################################
#################################################################################################################
# 1) Preliminares: cargar las librerías y lectura de datos#####
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías

library(GoodmanKruskal)

#################################################################################################################
# 1.2) Cargar tablas

#Tablas de asignación de grupos de las plantas madre según el crecimiento de su progenie medido en tres muestreos:
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
modelo.1.marzo <-
  read.table(
    "grupos.crecimiento.(marzo).VARS_2023diciembre28_081348.csv",
    header = T,
    sep = ","
  )
summary(modelo.1.marzo)
head(modelo.1.marzo)
dim(modelo.1.marzo)
modelo.2.marzo <-
  read.table(
    "grupos.crecimiento.(marzo).PCR_2023diciembre28_082916.csv",
    header = T,
    sep = ","
  )
summary(modelo.2.marzo)
head(modelo.2.marzo)
dim(modelo.2.marzo)
modelo.octubre <-
  read.table(
    "grupos.crecimiento.(octubre)_2023diciembre28_132240.csv",
    header = T,
    sep = ","
  )
summary(modelo.octubre)
head(modelo.octubre)
dim(modelo.octubre)


modelo.junio <-
  read.table(
    "grupos.crecimiento.(junio).VARS_2023diciembre28_140052.csv",
    header = T,
    sep = ","
  )
summary(modelo.junio)
head(modelo.junio)
dim(modelo.junio)

# y asignación de grupos de las plantas madre según su morfología
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
head(phenotypic.group.assignment)

#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350, ]
head(phenotypic.group.assignment.madres)

#################################################################################################################
#################################################################################################################
# 2) Creación de tabla con la asignación de grupo para cada modelo por cada muestreo y por morfología####
#################################################################################################################
#################################################################################################################

phenotypic.group.assignment.crecimiento <-
  merge(    modelo.1.marzo[, c(2, 3)],
    merge(
      modelo.2.marzo[, c(2, 3)],
      merge(
        modelo.octubre[, c(2, 3)],
        modelo.junio[, c(2, 3)],
        by = "Collector.Collection.Number",
        all = T,
        suffixes = c("modelo.octubre", ".modelo.junio")
      ),
      by = "Collector.Collection.Number",
      all = T
    ),
    by = "Collector.Collection.Number",
    all = T
  )

View(phenotypic.group.assignment.crecimiento)

colnames(phenotypic.group.assignment.crecimiento) <-
  c(
    "Collector.Collection.Number",
    "modelo.1.marzo",
    "modelo.2.marzo",
    "modelo.octubre",
    "modelo.junio"
  )

# Extracción del número de colección de las plantas madre
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.integer(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))

# Agregar asignación de grupos según morfología
phenotypic.group.assignment.crecimiento <-
  merge(
    phenotypic.group.assignment.crecimiento,
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number"
  )
head(phenotypic.group.assignment.crecimiento)

# Guardar tabla con asiganión de grupos de las plantas madre según crecimiento de su progenie y morfología
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# write.csv(
#   phenotypic.group.assignment.crecimiento,
#   file = paste(
#     "grupos.crecimiento.morfologicos_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#################################################################################################################
#################################################################################################################
# 3) Tabla de clasificación cruzada entre modelos#####
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 3.1) 11.4 meses
# 3.1.1) modelo1 (filas) vs modelo2 (columnas) marzo (11.4 meses)
colnames(phenotypic.group.assignment.crecimiento)
# [1] "Collector.Collection.Number" "modelo.1.marzo"              "modelo.2.marzo"
# [4] "modelo.octubre"              "modelo.junio"                "Phenotypic.Group"

# Tabla de clasificación cruzada entre los dos modelos a 11.4 meses después de la siembra
table(
  phenotypic.group.assignment.crecimiento[, 2],
  # Modelo 1 marzo
  phenotypic.group.assignment.crecimiento[, 3],
  # Modelo 2 marzo
  exclude = NULL
)
# 1  2
# 1 35  1
# 2  0  1

# 3.1.2) Modelo octubre (19.6 meses) vs modelo1 (columnas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  # Modelo octubre
  phenotypic.group.assignment.crecimiento[, 2],
  # Modelo 1 marzo
  exclude = NULL
)
# 1  2
# 1    17  0
# 2    15  0
# 3     3  0
# <NA>  1  1

# 3.1.3) Modelo junio (51.4 meses) vs modelo1 (columnas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 5],
  #Modelo junio
  phenotypic.group.assignment.crecimiento[, 2],
  # MOdelo 1 marzo
  exclude = NULL
)
# 1  2
# 1    28  0
# 2     5  0
# <NA>  3  1

# 3.1.4) Morfología (columnas) vs modelo1 (filas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  #modelo 1 marzo
  phenotypic.group.assignment.crecimiento[, 6],
  # modelo morfología
  exclude = NULL
)
# 2  3  4  5
# 1  6 15  5 10
# 2  0  0  0  1

###################################################################################################################
# 3.2) 19.6 meses

# 3.2.1) Modelo1 (filas) marzo (11.4 meses) vs modelo octubre (19.6 meses)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  #  Modelo 1 marzo
  phenotypic.group.assignment.crecimiento[, 4],
  # Modelo octubre
  exclude = NULL
)
#   1  2  3 <NA>
#   1 17 15  3    1
#   2  0  0  0    1

# 3.2.2) Modelo2 (filas) marzo (11.4 meses) vs modelo octubre (19.6 meses)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  # Modelo 2 marzo
  phenotypic.group.assignment.crecimiento[, 4],
  # Modelo octubre
  exclude = NULL
)
#   1  2  3 <NA>
#   1 17 14  3    1
#   2  0  1  0    1

# 3.2.3) Modelo (filas) junio (51.4) vs modelo octubre (19.6)
table(
  phenotypic.group.assignment.crecimiento[, 5],
  # Modelo junio
  phenotypic.group.assignment.crecimiento[, 4],
  # Modelo octubre
  exclude = NULL
)
#   1  2  3 <NA>
#   1  15 11  2    0
#   2   1  3  1    0
# <NA>  1  1  0    2

# 3.2.4) Morfología (columnas)  vs modelo octubre (19.6meses)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  # Modelo octubre
  phenotypic.group.assignment.crecimiento[, 6],
  # Modelo morfología
  exclude = NULL
)
#       2  3  4  5
# 1     4  4  3  6
# 2     1 10  2  2
# 3     0  1  0  2
# <NA>  1  0  0

#################################################################################################################
# 3.3) 51.4 meses

# 3.3.1) Modelo1 (filas) marzo (11.4 meses) vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  # Modelo 1 marzo
  phenotypic.group.assignment.crecimiento[, 5],
  # Modelo junio
  exclude = NULL
)
#     1  2 <NA>
#   1 28  5    3
#   2  0  0    1

# 3.3.2) Modelo2 (filas) marzo (11.4 meses) vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  # Modelo 2 marzo
  phenotypic.group.assignment.crecimiento[, 5],
  # Modelo junio
  exclude = NULL
)
#     1  2 <NA>
#   1 27  5    3
#   2  1  0    1

# 3.3.3) Modelo octubre (19.6 meses) vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  # Modelo octubre
  phenotypic.group.assignment.crecimiento[, 5],
  # Modelo junio
  exclude = NULL
)
#         1  2 <NA>
#   1    15  1    1
#   2    11  3    1
#   3     2  1    0
# <NA>    0  0    2

# 3.3.4) Morfología vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 5],
  # Modelo junio
  phenotypic.group.assignment.crecimiento[, 6],
  # Modelo morfología
  exclude = NULL
)
# 2 3 4 5
# 1    5 9 5 9
# 2    0 5 0 0
# <NA> 1 1 0 2

###################################################################################################################
# 3.4) Morfología
# 3.3.1) Modelo1 (filas) marzo (11.4 meses) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
# 2  3  4  5
# 1  6 15  5 10
# 2  0  0  0  1

# 3.3.2) Modelo2 (filas) marzo (11.4 meses) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
# 2  3  4  5
# 1  6 15  4 10
# 2  0  0  1

# 3.3.3) Modelo octubre (19.6) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#       2  3  4  5
# 1     4  4  3  6
# 2     1 10  2  2
# 3     0  1  0  2
# <NA>  1  0  0  1

# 3.3.4) Modelo junio (51.4 meses) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 5],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#      2 3 4 5
# 1    5 9 5 9
# 2    0 5 0 0
# <NA> 1 1 0 2

###################################################################################################################
###################################################################################################################
# 4) Estadístico Goodman-Kruskal tau para la concordancia entre grupos de crecimiento entre muestreos y
#entre morfología
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 4.1) 11.4 meses vs. 19.6 meses (modelo 1)

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(2, 4)]
head(grupos.crecimiento)# 37 plantas madre
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 2 filas con NA

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) # 35 filas con datos completos (plantas madre asignadas)
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y
# 19.6 meses y grupos morfológicos: tau(11.4,19.6) y tau(19.6,11.4)
GKtau(grupos.crecimiento$modelo.1.marzo,
      grupos.crecimiento$modelo.octubre)
#
# xName                             yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.1.marzo grupos.crecimiento$modelo.octubre  1  3     0   NaN

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.octubre)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.1.marzo,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(11.4, 19.6),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, 19.6)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$modelo.octubre
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$modelo.octubre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 1

# Gráfica de la distribución nula de tau(19.6, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(19.6, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$modelo.octubre
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$modelo.octubre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# NA

###################################################################################################################
# 4.2) 11.4 meses vs. 19.6 meses (modelo 2)

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(3, 4)]
head(grupos.crecimiento)# 37 plantas madre
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 2 filas con NA

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) # 35 filas con datos completos (plantas madre asignadas)
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y
#19.6 meses y grupos morfológicos: tau(11.4,19.6) y tau(19.6,11.4)
GKtau(grupos.crecimiento$modelo.2.marzo,
      grupos.crecimiento$modelo.octubre)
# xName                             yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.2.marzo grupos.crecimiento$modelo.octubre  2  3 0.029 0.039

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.octubre)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.2.marzo,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(11.4, 19.6),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, 19.6)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.51579

# Gráfica de la distribución nula de tau(19.6, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(19.6, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.51579

###################################################################################################################
# 4.3) 19.6 meses vs. 51.4 meses

#  Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(4, 5)]
head(grupos.crecimiento)# 37 filas
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 4 con NA

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) # 33 filas completas
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 19.6 meses y
#51.4 meses y grupos morfológicos: tau(19.6, 51.4) y tau(51.4, 19.6)
GKtau(grupos.crecimiento$modelo.octubre,
      grupos.crecimiento$modelo.junio)

# xName                           yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.octubre grupos.crecimiento$modelo.junio  3  2 0.066 0.038

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.junio)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.octubre,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(19.6, 51.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(19.6, 51.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.36162

# Gráfica de la distribución nula de tau(51.4, 19.6),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(51.4, 19.6)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.36162

###################################################################################################################
# 4.4) 51.4 meses vs. 11.4 meses (modelo 1)

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(5, 2)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 4 planta hija con NA

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) # 170 hijas con todos los datos
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)


# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 51.4 meses y
#11.4 meses y grupos morfológicos: tau(51.4, 11.4) y tau(11.4, 51.4)
GKtau(grupos.crecimiento$modelo.junio,
      grupos.crecimiento$modelo.1.marzo)
# xName                             yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.junio grupos.crecimiento$modelo.1.marzo  2  1   Inf     0

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.1.marzo)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.junio,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(51.4, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(51.4, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.1.marzo
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.1.marzo
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
#1

# Gráfica de la distribución nula de tau(11.4, 51.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, 51.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.1.marzo
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.1.marzo
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
#1

###################################################################################################################
# 4.5) 51.4 meses vs. 11.4 meses (modelo 2)

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(5, 3)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 4 planta hija con NA

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) # 170 hijas con todos los datos
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 51.4 meses y
#11.4 meses y grupos morfológicos: tau(51.4, 11.4) y tau(11.4, 51.4)
GKtau(grupos.crecimiento$modelo.junio,
      grupos.crecimiento$modelo.2.marzo)

# xName                             yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.junio grupos.crecimiento$modelo.2.marzo  2  2 0.006 0.006

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.2.marzo)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.junio,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(51.4, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(51.4, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
#1

# Gráfica de la distribución nula de tau(11.4, 51.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, 51.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
#1

###################################################################################################################
# 4.6) 11.4 meses (modelo 1) vs. morfología

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(2, 6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)# 37 filas

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 0

# Correr las siguientes líneas en caso de existir NAs
# grupos.crecimiento <-
#   grupos.crecimiento[-rows.with.na,]
# dim(grupos.crecimiento) # 170 hijas con todos los datos
# class(grupos.crecimiento)
# summary(grupos.crecimiento)
# head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y
#y grupos morfológicos: tau(11.4, M) y tau(M, 11.4)
GKtau(grupos.crecimiento$modelo.1.marzo,
      grupos.crecimiento$Phenotypic.Group)
# xName                               yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.1.marzo grupos.crecimiento$Phenotypic.Group  2  4 0.028 0.066

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.1.marzo,
          morfo.aleatorio,)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(11.4, M),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, M)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.59266

# Gráfica de la distribución nula de tau(M, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(M, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.1.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.59266

###################################################################################################################
# 4.6) 11.4 meses (modelo 2) vs. morfología

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(3, 6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)# 37 filas

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 0

# Correr las siguietes líneas en caso de existir NAs
# grupos.crecimiento <-
#   grupos.crecimiento[-rows.with.na,]
# dim(grupos.crecimiento) # 170 hijas con todos los datos
# class(grupos.crecimiento)
# summary(grupos.crecimiento)
# head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y
#y grupos morfológicos: tau(11.4, M) y tau(M, 11.4)
GKtau(grupos.crecimiento$modelo.2.marzo,
      grupos.crecimiento$Phenotypic.Group)
# xName                               yName Nx Ny
# 1 grupos.crecimiento$modelo.2.marzo grupos.crecimiento$Phenotypic.Group  2  4
# tauxy tauyx
# 1 0.028 0.066

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.2.marzo,
          morfo.aleatorio,)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(11.4, M),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, M)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.40511

# Gráfica de la distribución nula de tau(M, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(M, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.24785

###################################################################################################################
# 4.7) 19.6 meses vs. morfología

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(4, 6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 2 filas

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) # 170 hijas con todos los datos
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y 19.6 meses
#y grupos morfológicos: tau(19.6, M) y tau(M, 19.6)
GKtau(grupos.crecimiento$modelo.octubre,
      grupos.crecimiento$Phenotypic.Group)
# xName                               yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.octubre grupos.crecimiento$Phenotypic.Group  3  4 0.113 0.162

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.octubre,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(19.6,M),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(19.6, M)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6,M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.06973

# Gráfica de la distribución nula de tau(M,S),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(M, 19.6)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M,19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.098

###################################################################################################################
# 4.8) 51.4 meses vs. morfología

# Filtrar filas y columnas con NA
grupos.crecimiento <-
  phenotypic.group.assignment.crecimiento[, c(5, 6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na #
length(rows.with.na)# 4 filas con NA

# Correr las siguietes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na, ]
dim(grupos.crecimiento) #
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 51.4 meses
#y grupos morfológicos: tau(51.4, M) y tau(M, 51.4)
GKtau(grupos.crecimiento$modelo.junio,
      grupos.crecimiento$Phenotypic.Group)
# xName                               yName Nx Ny tauxy tauyx
# 1 grupos.crecimiento$modelo.junio grupos.crecimiento$Phenotypic.Group  2  4 0.115 0.242

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.junio,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(51.4,M),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(51.4, M)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(S,M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.01591

# Gráfica de la distribución nula de tau(M,51.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(M, 51.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M,51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
#  0.06146

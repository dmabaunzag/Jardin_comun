#################################################################################################################
#################################################################################################################
#################################################################################################################
#
#Concordacia entre tiempos de crecimientos: 11.4 meses, 19.6 meses y 51.4 meses despues de la siembra
#
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
#INTRODUCCIÓN ## Los datos de crecimiento de la progenie (cantidad de hojas y longitud del tallo) en cada tiempo
#de medida hizo modelo de mezclas para cada uno. El objetivo de este sript esexaminar el cambio de asignación de
#grupos en cada momento de medida
#
#REQUERIMIENTOS##
#Tablas de de asignación de grupos de plantas madre ún crecimiento

###################################################################################################################
###################################################################################################################
# 1) Preliminares: cargar las librerías y lectura de datos
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 1.1) Librerías

library(GoodmanKruskal)

###################################################################################################################
# 1.2) Cargar tablas 

#Tablas de asignación de grupos de las plantas madre según el creceimiento de su progenie mediedo en tres ocaciones
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/data")
modelo.1.marzo <-
  read.table(
    "PhenotypicGroupAssignment.piloto.(marzo).VARS_2023septiembre03_112042.csv",
    header = T,
    sep = ","
  )
summary(modelo.1.marzo)
head(modelo.1.marzo)
dim(modelo.1.marzo)
modelo.2.marzo <-
  read.table(
    "PhenotypicGroupAssignment.piloto.(marzo).STD_2023septiembre03_120715.csv",
    header = T,
    sep = ","
  )
summary(modelo.2.marzo)
head(modelo.2.marzo)
dim(modelo.2.marzo)
modelo.octubre <-
  read.table(
    "PhenotypicGroupAssignment.piloto.(octubre)_2023agosto30_082143.csv",
    header = T,
    sep = ","
  )
summary(modelo.octubre)
head(modelo.octubre)
dim(modelo.octubre)
modelo.junio <-
  read.table(
    "PhenotypicGroupAssignment.piloto.(junio).VARS_2023noviembre24_221700.csv",
    header = T,
    sep = ","
  )
summary(modelo.junio)
head(modelo.junio)
dim(modelo.junio)
#Asignación de grupos de las plantas madre según su morfología
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
head(phenotypic.group.assignment)
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350,]
head(phenotypic.group.assignment.madres)

###################################################################################################################
###################################################################################################################
# 2) Creación de tabla con la asignación de grupo para cada modelo por cada ocasión de medida y por morfología
###################################################################################################################
###################################################################################################################

phenotypic.group.assignment.crecimiento <-
  merge(
    modelo.1.marzo[, c(2, 3)],
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

#Extracción del número de colección de las plantas madre
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.integer(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))

#Agregar asiganció de grupos según morfología

phenotypic.group.assignment.crecimiento <-
  merge(
    phenotypic.group.assignment.crecimiento,
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number"
  )

#guardar tabla con asiganión de grupos de las plantas madre según crecimiento de su progenie y morfología

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/data")
# write.csv(
#   phenotypic.group.assignment.crecimiento,
#   file = paste(
#     "phenotypic.group.assignment.crecimiento_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

###################################################################################################################
###################################################################################################################
# 3) tabla de clasificación cruzada entre modelos
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 3.1) 11.4 meses
# 3.1.1) modelo1 (filas) vs modelo2 (columnas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  phenotypic.group.assignment.crecimiento[, 3],
  exclude = NULL
)
#    1  2
# 1  1  1
# 2 35  0

# 3.1.2) modelo octubre (19.6 meses) vs modelo2 (columnas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  phenotypic.group.assignment.crecimiento[, 3],
  exclude = NULL
)
#       1  2
# 1    29  0
# 2     6  0
# <NA>  1  1

# 3.1.3) modelo junio (51.4 meses) vs modelo2 (columnas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 5],
  phenotypic.group.assignment.crecimiento[, 3],
  exclude = NULL
)
#       1  2
# 1    28  0
# 2     5  0
# <NA>  3  1

# 3.1.4) morfología (fila) vs modelo2 (columnas) marzo (11.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#     2  3  4  5
# 1  6 15  5 10
# 2  0  0  0  1

###################################################################################################################
# 3.2) 19.6 meses

# 3.2.1) modelo1 (filas) marzo (11.4 meses) vs modelo octubre (19.6 meses)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  phenotypic.group.assignment.crecimiento[, 4],
  exclude = NULL
)
#       1  2 <NA>
#   1  0  1    1
#   2 29  5    1

# 3.2.2) modelo2 (filas) marzo (11.4 meses) vs modelo octubre (19.6 meses)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  phenotypic.group.assignment.crecimiento[, 4],
  exclude = NULL
)
#     1  2 <NA>
#   1 29  6    1
#   2  0  0    1

# 3.2.3) modelo1 (filas) junio (51.4) vs modelo octubre (19.6)
table(
  phenotypic.group.assignment.crecimiento[, 5],
  phenotypic.group.assignment.crecimiento[, 4],
  exclude = NULL
)
#         1  2 <NA>
#   1    23  5    0
#   2     4  1    0
#   <NA>  2  0    2

# 3.2.4) morfología (filas)  vs modelo octubre (19.6)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#       2  3  4  5
# 1     5 13  4  7
# 2     0  2  1  3
# <NA>  1  0  0  1

###################################################################################################################
# 3.3) 51.4 meses

# 3.3.1) modelo1 (filas) marzo (11.4 meses) vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  phenotypic.group.assignment.crecimiento[, 5],
  exclude = NULL
)
#      1  2 <NA>
#   1  1  0    1
#   2 27  5    3

# 3.3.2) modelo2 (filas) marzo (11.4 meses) vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  phenotypic.group.assignment.crecimiento[, 5],
  exclude = NULL
)
#     1  2 <NA>
#   1 28  5    3
#   2  0  0    1

# 3.3.3) modelo octubre (19.6 meses) vs modelo junio (51.4 meses)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  phenotypic.group.assignment.crecimiento[, 5],
  exclude = NULL
)
#         1  2 <NA>
#   1    23  4    2
#   2     5  1    0
# <NA>    0  0    2

# 3.3.4) morfología vs modelo junio (51.4 meses)
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
# 3.4) morfología
# 3.3.1) modelo1 (filas) marzo (11.4 meses) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 2],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#    2  3  4  5
# 1  0  0  1  1
# 2  6 15  4 10

# 3.3.2) modelo2 (filas) marzo (11.4 meses) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 3],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#    2  3  4  5
# 1  6 15  5 10
# 2  0  0  0  1

# 3.3.3) modelo octubre (19.6) vs morfología (columnas)
table(
  phenotypic.group.assignment.crecimiento[, 4],
  phenotypic.group.assignment.crecimiento[, 6],
  exclude = NULL
)
#       2  3  4  5
# 1     5 13  4  7
# 2     0  2  1  3
# <NA>  1  0  0  1

# 3.3.4) modelo junio (51.4 meses) vs morfología (columnas)
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
#4) Estadístico Goodman-Kruskal tau para la concordancia entre grupos de crecimiento entre tiempos de medida y
#entre morfología
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 4.1) 11.4 meses vs. 19.6 meses

# filtrar filas y columnas con NA
grupos.crecimiento <- 
  phenotypic.group.assignment.crecimiento[, c(3,4)]
head(grupos.crecimiento)# 37 plantas madre
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 2 filas con NA

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na,]
dim(grupos.crecimiento) # 35 filas con datos compeltos (plantas madre asignadas)
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

#calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento a 11.4 meses y
#19.6 meses y grupos morfológicos: tau(11.4,19.6) y tau(19.6,11.4)


GKtau(grupos.crecimiento$modelo.2.marzo,
      grupos.crecimiento$modelo.octubre) 
# xName                             yName Nx Ny tauxy
# 1 grupos.crecimiento$modelo.2.marzo grupos.crecimiento$modelo.octubre  1  2     0
# tauyx
# 1   NaN

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.octubre)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.2.marzo,
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(11.4, 19.6),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
    
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre)[[5]] <= GKtau.nulo.mat[, 1]
) / k


#grafica de la distribución nula de tau(19.6, 11.4),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$modelo.octubre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k


###################################################################################################################
# 4.2) 19.6 meses vs. 51.4 meses

# filtrar filas y columnas con NA
grupos.crecimiento <- 
  phenotypic.group.assignment.crecimiento[, c(4, 5)]
head(grupos.crecimiento)# 37 filas
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 4 con NA

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na,]
dim(grupos.crecimiento) # 33 filas completas
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

#calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 19.6 meses y
#51.4 meses y grupos morfológicos: tau(19.6, 51.4) y tau(51.4, 19.6)

GKtau(
  grupos.crecimiento$modelo.octubre,
  grupos.crecimiento$modelo.junio
)

# xName                           yName Nx Ny tauxy
# 1 grupos.crecimiento$modelo.octubre grupos.crecimiento$modelo.junio  2  2     0
# tauyx
# 1     0

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.junio)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.octubre,
          morfo.aleatorio 
          )
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(19.6, 51.4),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 1

#grafica de la distribución nula de tau(51.4, 19.6),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$modelo.junio
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 1

###################################################################################################################
# 4.3) 51.4 meses vs. 11.4 meses

# filtrar filas y columnas con NA
grupos.crecimiento <- 
  phenotypic.group.assignment.crecimiento[, c(5,3)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 9 planta hija con NA

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na,]
dim(grupos.crecimiento) # 170 hijas con todos los datos
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)


#calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 51.4 meses y
#11.4 meses y grupos morfológicos: tau(51.4, 11.4) y tau(11.4, 51.4)
GKtau(
  grupos.crecimiento$modelo.junio,
  grupos.crecimiento$modelo.2.marzo
)
# xName                                                         yName Nx Ny tauxy
# 1 grupos.crecimiento$modelo.junio grupos.crecimiento$modelo.2.marzo  2  1   Inf
# tauyx
# 1     0

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$modelo.2.marzo)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.junio,
          morfo.aleatorio
          )
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(51.4, 11.4),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k


#grafica de la distribución nula de tau(11.4, 51.4),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$modelo.2.marzo
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k



###################################################################################################################
# 4.4) 11.4 meses vs. morfología

# filtrar filas y columnas con NA
grupos.crecimiento <- 
  phenotypic.group.assignment.crecimiento[, c(3,6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)# 37 filas

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 0

#correr las siguietes líneas en caso de existir NAs
# grupos.crecimiento <-
#   grupos.crecimiento[-rows.with.na,]
# dim(grupos.crecimiento) # 170 hijas con todos los datos
# class(grupos.crecimiento)
# summary(grupos.crecimiento)
# head(grupos.crecimiento)

#calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y
#y grupos morfológicos: tau(11.4, M) y tau(M, 11.4)

GKtau(
  grupos.crecimiento$modelo.2.marzo,
  grupos.crecimiento$Phenotypic.Group
)
# xName                               yName Nx Ny
# 1 grupos.crecimiento$modelo.2.marzo grupos.crecimiento$Phenotypic.Group  2  4
# tauxy tauyx
# 1 0.028 0.066

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.2.marzo,
          morfo.aleatorio,
          )
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(11.4, M),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.59495

#grafica de la distribución nula de tau(M, 11.4),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.2.marzo,
    grupos.crecimiento$Phenotypic.Group
    )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.59495

###################################################################################################################
# 4.5) 19.6 meses vs. morfología

# filtrar filas y columnas con NA
grupos.crecimiento <- 
  phenotypic.group.assignment.crecimiento[, c(4,6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 2 filas

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na,]
dim(grupos.crecimiento) # 170 hijas con todos los datos
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)

#calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 11.4 meses y 19.6 meses
#y grupos morfológicos: tau(19.6, M) y tau(M, 19.6)
GKtau(
  grupos.crecimiento$modelo.octubre,
  grupos.crecimiento$Phenotypic.Group
)
# xName                               yName Nx Ny
# 1 grupos.crecimiento$modelo.octubre grupos.crecimiento$Phenotypic.Group  2  4
# tauxy tauyx
# 1 0.023 0.068

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.octubre,
          morfo.aleatorio
          )
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(19.6,M),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6,M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.60775

#grafica de la distribución nula de tau(M,S),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M,19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.octubre,
    grupos.crecimiento$Phenotypic.Group
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.73012

###################################################################################################################
# 4.6) 51.4 meses vs. morfología

# filtrar filas y columnas con NA
grupos.crecimiento <- 
  phenotypic.group.assignment.crecimiento[, c(5,6)]
head(grupos.crecimiento)
dim(grupos.crecimiento)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 4 filas con NA

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento <-
  grupos.crecimiento[-rows.with.na,]
dim(grupos.crecimiento) # 
class(grupos.crecimiento)
summary(grupos.crecimiento)
head(grupos.crecimiento)


#calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento a 51.4 meses
#y grupos morfológicos: tau(51.4, M) y tau(M, 51.4)
GKtau(
  grupos.crecimiento$modelo.junio,
  grupos.crecimiento$Phenotypic.Group
)
# xName                               yName Nx Ny tauxy
# 1 grupos.crecimiento$modelo.junio grupos.crecimiento$Phenotypic.Group  2  4 0.115
# tauyx
# 1 0.242

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento$Phenotypic.Group)
  GKtau.nulo <-
    GKtau(grupos.crecimiento$modelo.junio,
          morfo.aleatorio
          )
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(51.4,M),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(S,M) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.01609

#grafica de la distribución nula de tau(M,51.4),
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
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(M,51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento$modelo.junio,
    grupos.crecimiento$Phenotypic.Group
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.06126
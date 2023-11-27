#################################################################################################################
#################################################################################################################
#################################################################################################################
#
# INTRODUCCIÓN
#
# REQUERIMIENTOS: Tablas de asignación de grupos según crecimiento y superviviencia para cada ocasión de medida
# 
#  "asignacion.grupo.fenotipico.supervivencia.3.2_2023noviembre08_215124.csv"
#  "asignacion.grupo.fenotipico.supervivencia.3.3_2023noviembre08_215114.csv"
#  "asignacion.grupo.fenotipico.supervivencia.2_2023noviembre08_214937.csv"
#  "asignacion.grupo.fenotipico.supervivencia.1_2023noviembre08_214819.csv"
#
#
#
# CONTENIDO
#  1) Datos preliminares
#  2)
#  3) Tabla de clasificación cruzada según crecimiento y supervivencia para cada ocasión de medida
#  
#
#################################################################################################################
#################################################################################################################
#################################################################################################################


#################################################################################################################
#################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías

library(GoodmanKruskal)

#################################################################################################################
# 1.2) lectura de las asignaciones de grupos segúnsuperviviencia
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana

asignacion.grupo.fenotipico.supervivencia.1 <- 
  read.table(
    "grupo.supervivencia.1_2023noviembre08_214819.csv",
    header = T,
    sep = ","
  )
head(asignacion.grupo.fenotipico.supervivencia.1)
colnames(asignacion.grupo.fenotipico.supervivencia.1)
dim(asignacion.grupo.fenotipico.supervivencia.1)  

asignacion.grupo.fenotipico.supervivencia.2 <- 
  read.table(
    "grupo.supervivencia.2_2023noviembre08_214937.csv",
    header = T,
    sep = ","
  )
head(asignacion.grupo.fenotipico.supervivencia.2)
colnames(asignacion.grupo.fenotipico.supervivencia.2)
dim(asignacion.grupo.fenotipico.supervivencia.2)

asignacion.grupo.fenotipico.supervivencia.3.3 <- 
  read.table(
    "grupo.supervivencia.3.3_2023noviembre08_215114.csv",
    header = T,
    sep = ","
  )
head(asignacion.grupo.fenotipico.supervivencia.3.3)
colnames(asignacion.grupo.fenotipico.supervivencia.3.3)
dim(asignacion.grupo.fenotipico.supervivencia.3.3) 

#################################################################################################################
# 1.3) lectura de las asignaciones de grupos según crecimiento

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/data")
asignacion.grupo.fenotipico.crecimiento <- 
  read.table(
    "phenotypic.group.assignment.crecimiento_2023noviembre26_074551.csv",
    header = T,
    sep = ","
  )

#################################################################################################################
#################################################################################################################
# 2) Creación de tabla combinada con las asignación de grupos según crecimiento y supervivencia
#################################################################################################################
#################################################################################################################

asignacion.grupo.fenotipico.supervivencia <- 
  merge(
   asignacion.grupo.fenotipico.supervivencia.1[, c(1, 3)],
    merge(
      asignacion.grupo.fenotipico.supervivencia.2[, c(1, 3)],
      asignacion.grupo.fenotipico.supervivencia.3.3[, c(1, 3)],
        by = "Collector.Collection.Number",
        all = T),
        by="Collector.Collection.Number",
        all = T)
colnames(asignacion.grupo.fenotipico.supervivencia) <- 
  c("Collector.Collection.Number",
    "supervivencia.1.marzo",
    "supervivencia.2.octubre",
    "supervivencia.3.junio"
  )
colnames(asignacion.grupo.fenotipico.crecimiento)
colnames(asignacion.grupo.fenotipico.supervivencia)
asignacion.grupo.fenotipico.crecimiento.supervivencia <- 
  merge(asignacion.grupo.fenotipico.crecimiento[, c(1,3,4,5)],
        asignacion.grupo.fenotipico.supervivencia,
        all=T,
        by="Collector.Collection.Number")
head(asignacion.grupo.fenotipico.crecimiento.supervivencia)
View(asignacion.grupo.fenotipico.crecimiento.supervivencia)

#################################################################################################################
#################################################################################################################
# 3) Tabla de clasificación cruzada según crecimiento y supervivencia para cada ocasión de medida
#################################################################################################################
#################################################################################################################
colnames(asignacion.grupo.fenotipico.crecimiento.supervivencia)

# 3.1) 11.4 meses

table(asignacion.grupo.fenotipico.crecimiento.supervivencia[,2],# crecimiento
      asignacion.grupo.fenotipico.crecimiento.supervivencia[,5], # supervivencia  
      exclude= NULL)
#       1  2  3
# 1     5 18 13
# 2     0  0  1
# <NA>  0  0  6

# 3.2) 19.6 meses

table(asignacion.grupo.fenotipico.crecimiento.supervivencia[,3],
      asignacion.grupo.fenotipico.crecimiento.supervivencia[,6],
      exclude= NULL)
#       1  2  3
# 1     7  5 17
# 2     4  0  2
# <NA>  8  0  0
  
# 3.3) 51.4 meses

table(asignacion.grupo.fenotipico.crecimiento.supervivencia[,4],
      asignacion.grupo.fenotipico.crecimiento.supervivencia[,7],
      exclude= NULL)
#       1  2  3
# 1    12  9  7
# 2     2  3  0
# <NA>  0 10  0

#################################################################################################################
#################################################################################################################
#4) Estadístico Goodman-Kruskal tau para la concordancia entre grupos de crecimiento con grupos de superviviencia
#para tiempo de medida
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1) 11.4 meses

grupos.crecimiento.supervivencia<- 
  asignacion.grupo.fenotipico.crecimiento.supervivencia[, c(2,5)]
head(grupos.crecimiento.supervivencia)
dim(grupos.crecimiento.supervivencia)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento.supervivencia), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 6 filas con NA

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento.supervivencia <-
  grupos.crecimiento.supervivencia[-rows.with.na,]
dim(grupos.crecimiento.supervivencia) #
class(grupos.crecimiento.supervivencia)
summary(grupos.crecimiento.supervivencia)
head(grupos.crecimiento.supervivencia)

#calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento con grupos de
#supervivencia a 11.4: tau(C, S) y tau(S, C)


GKtau(grupos.crecimiento.supervivencia[,1],#Crecimiento
      grupos.crecimiento.supervivencia[,2]) # supervivencia
# xName                                 yName Nx
# 1 grupos.crecimiento.supervivencia[, 1] grupos.crecimiento.supervivencia[, 2]  2
# Ny tauxy tauyx
# 1  3  0.03 0.046

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau

k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.supervivencia[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.supervivencia[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(S, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(S, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
    
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(S, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.51167

#grafica de la distribución nula de tau(C, S),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, S)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, S) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.51167

#################################################################################################################
# 4.2) 19.6 meses

grupos.crecimiento.supervivencia<- 
  asignacion.grupo.fenotipico.crecimiento.supervivencia[, c(3,6)]
head(grupos.crecimiento.supervivencia)
dim(grupos.crecimiento.supervivencia)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento.supervivencia), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento.supervivencia <-
  grupos.crecimiento.supervivencia[-rows.with.na,]
dim(grupos.crecimiento.supervivencia) #
class(grupos.crecimiento.supervivencia)
summary(grupos.crecimiento.supervivencia)
head(grupos.crecimiento.supervivencia)

#calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento con grupos de
#supervivencia a 19.6 meses: tau(C, S) y tau(S, C)


GKtau(grupos.crecimiento.supervivencia[,1],
      grupos.crecimiento.supervivencia[,2]) 
# xName                                 yName Nx
# 1 grupos.crecimiento.supervivencia[, 1] grupos.crecimiento.supervivencia[, 2]  2
# Ny tauxy tauyx
# 1  3 0.067 0.128

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau

k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.supervivencia[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.supervivencia[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(S, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(S, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
    
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(S, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.12723

#grafica de la distribución nula de tau(C, S),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, S)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, S) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.12723

#################################################################################################################
# 4.3) 51.4 meses

grupos.crecimiento.supervivencia<- 
  asignacion.grupo.fenotipico.crecimiento.supervivencia[, c(4, 7)]
head(grupos.crecimiento.supervivencia)
dim(grupos.crecimiento.supervivencia)

rows.with.na <-
  unique(which(is.na(grupos.crecimiento.supervivencia), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 

#correr las siguietes líneas en caso de existir NAs
grupos.crecimiento.supervivencia <-
  grupos.crecimiento.supervivencia[-rows.with.na,]
dim(grupos.crecimiento.supervivencia) #
class(grupos.crecimiento.supervivencia)
summary(grupos.crecimiento.supervivencia)
head(grupos.crecimiento.supervivencia)

#calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento con grupoa de
#supervivencia a 51.4: tau(C, S) y tau(S, C)


GKtau(grupos.crecimiento.supervivencia[,1],
      grupos.crecimiento.supervivencia[,2]) 
# xName                                 yName Nx
# 1 grupos.crecimiento.supervivencia[, 1] grupos.crecimiento.supervivencia[, 2]  2
# Ny tauxy tauyx
# 1  3 0.028 0.066

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau

k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.supervivencia[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.supervivencia[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(S, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(S, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
    
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(S, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.41254

#grafica de la distribución nula de tau(C, S),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, S)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, S) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.supervivencia[,1],
    grupos.crecimiento.supervivencia[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.32143
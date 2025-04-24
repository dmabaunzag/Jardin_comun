#################################################################################################################
#################################################################################################################
#################################################################################################################

# CONCORDACIA ENTRE MUESTREOS ENTRE GRUPO DE PLANTAS MADRES SEGÚN RECLUTAMIENTO Y CRECIMIENTO####

# ###############################################################################################################
# ###############################################################################################################
# ###############################################################################################################
#
# INTRODUCCIÓN#### 

# Examinar la concordancia entre muestreos de grupos de plantas madre según reclutamiento y crecimiento de su
# progenie. El estadístico tau (τ) de Goodman y Kruskal (Goodman & Kruskal, 1979; Agresti, 2002) fue usado para
# estimar el grado de concordancia entre los grupos de plantas madre estimados de novo según i) su morfología,
# ii) el reclutamiento y iii) crecimiento de la progenie en el jardín común. El estadístico τ también se usó para
# evaluar el grado de concordancia entre grupos de plantas madre con progenie viva estimados según el
# reclutamiento y crecimiento de la progenie en los tres muestreos (11.4 meses, 19.6 meses y 51.4 meses después
# de la siembra).

# REQUERIMIENTOS: Tablas de asignación de grupos según crecimiento y reclutamiento para cada muestreo

#  "grupo.reclutamiento.3.1_2025abril20_114731.csv"
#  "grupo.reclutamiento.3_2025abril20_114604.csv"
#  "grupo.reclutamiento.2_2023diciembre26_143302.csv"
#  "grupo.reclutamiento.1_2023diciembre26_141927.csv"

#  "grupos.crecimiento.morfologicos_2024agosto01_092331.csv"

# CONTENIDO

#  1) Datos preliminares: Carga de librerías y lectura de datos

#  2) Creación de tabla combinada con las asignación de grupos según crecimiento y reclutamiento

#  3) Tabla de clasificación cruzada según crecimiento y reclutamiento para cada muestreo

#################################################################################################################
#################################################################################################################
#################################################################################################################


#################################################################################################################
#################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos####
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías

library(GoodmanKruskal)

# Seleccionar el directorio de trabajo 
project_dir <- "C:/Users/dmaba/OneDrive - Universidad Nacional de Colombia/PROYECTO JARDÍN COMUN/Jardin_comun" # Diana's directory

# Define sub directories
data_path <- file.path(project_dir, "Progenie", "datos") # data file
figures_path <- file.path(project_dir, "Progenie", "Figuras") # figures file
datos_reclutamiento_path <- file.path(project_dir, "Progenie", "reclutamiento", "datos")
datos_crecimiento_path <- file.path(project_dir, "Progenie", "Crecimiento", "datos")

#################################################################################################################
# 1.2) lectura de las asignaciones de grupos según reclutamiento

setwd(datos_reclutamiento_path)#Directorio de Diana
grupo.reclutamiento.1 <- 
  read.table(
    "grupo.reclutamiento.1_2023diciembre26_141927.csv",
    header = T,
    sep = ","
  )
head(grupo.reclutamiento.1)
colnames(grupo.reclutamiento.1)
dim(grupo.reclutamiento.1)  

grupo.reclutamiento.2 <- 
  read.table(
    "grupo.reclutamiento.2_2023diciembre26_143302.csv",
    header = T,
    sep = ","
  )
head(grupo.reclutamiento.2)
colnames(grupo.reclutamiento.2)
dim(grupo.reclutamiento.2)

grupo.reclutamiento.3<- 
  read.table(
    "grupo.reclutamiento.3_2025abril20_114604.csv",
    header = T,
    sep = ","
  )
head(grupo.reclutamiento.3)
colnames(grupo.reclutamiento.3)
dim(grupo.reclutamiento.3) 

grupo.reclutamiento.3.1<- 
  read.table(
    "grupo.reclutamiento.3.1_2025abril20_114731.csv",
    header = T,
    sep = ","
  )
head(grupo.reclutamiento.3.1)
colnames(grupo.reclutamiento.3.1)
dim(grupo.reclutamiento.3.1) 

#################################################################################################################
# 1.3) lectura de las asignaciones de grupos según crecimiento

setwd(datos_crecimiento_path)
grupo.crecimiento <- 
  read.table(
    "grupos.crecimiento.morfologicos_2024agosto01_092331.csv",
    header = T,
    sep = ","
  )

#################################################################################################################
#################################################################################################################
# 2) Creación de tabla combinada con las asignación de grupos según crecimiento y reclutamiento
#################################################################################################################
#################################################################################################################

grupo.reclutamiento <- 
  merge(
   grupo.reclutamiento.1[, c(1, 3)],
    merge(
      grupo.reclutamiento.2[, c(1, 3)],
      merge(
        grupo.reclutamiento.3[, c(1, 3)],
        grupo.reclutamiento.3.1[, c(1, 3)],
        by = "Collector.Collection.Number",
        all = T),
        by="Collector.Collection.Number",
        all = T),
by="Collector.Collection.Number",
all = T)
colnames(grupo.reclutamiento) <- 
  c("Collector.Collection.Number",
    "reclutamiento.1.marzo",
    "reclutamiento.2.octubre",
    "reclutamiento.3.junio",
    "reclutamiento.3.1.junio"
  )
colnames(grupo.crecimiento)
colnames(grupo.reclutamiento)
grupo.crecimiento.reclutamiento <- 
  merge(grupo.crecimiento,
        grupo.reclutamiento,
        all=T,
        by="Collector.Collection.Number")
head(grupo.crecimiento.reclutamiento)
View(grupo.crecimiento.reclutamiento)

#################################################################################################################
#################################################################################################################
# 3) Tabla de clasificación cruzada según crecimiento y reclutamiento para cada ocasión de medida
#################################################################################################################
#################################################################################################################
colnames(grupo.crecimiento.reclutamiento)

# 3.1) 11.4 meses (modelo1)

table(grupo.crecimiento.reclutamiento[,2],# crecimiento
      grupo.crecimiento.reclutamiento[,8], # reclutamiento  
      exclude= NULL)
# 1  2  3
# 1    19  4 13
# 2     0  0  1
# <NA>  0  0  6

# 3.2) 11.4 meses (modelo2)

table(grupo.crecimiento.reclutamiento[,3],# crecimiento
      grupo.crecimiento.reclutamiento[,8], # reclutamiento  
      exclude= NULL)

# 1  2  3
# 1    19  4 12
# 2     0  0  2
# <NA>  0  0  6

# 3.3) 19.6 meses

table(grupo.crecimiento.reclutamiento[,4],
      grupo.crecimiento.reclutamiento[,9],
      exclude= NULL)
# 1  2  3
# 1     4 11  2
# 2     6  7  2
# 3     3  0  0
# <NA>  8  0  0
  
# 3.4) 19.6 meses (2)

table(grupo.crecimiento.reclutamiento[,5],
      grupo.crecimiento.reclutamiento[,9],
      exclude= NULL)
      # 1  2  3
# 1     3  8  1
# 2    10 10  3
# <NA>  8  0  0

# 3.4) 51.4 meses (modelo1)

table(grupo.crecimiento.reclutamiento[,6],
      grupo.crecimiento.reclutamiento[,10],
      exclude= NULL)
#       1  2  3
# 1     2 11 15
# 2     0  4  1
# <NA>  0 10  0


# 3.5) 51.4 meses (modelo2)
table(grupo.crecimiento.reclutamiento[,6],
      grupo.crecimiento.reclutamiento[,11],
      exclude= NULL)

#       1  2
# 1    15 13
# 2     0  5
# <NA>  0 10

#################################################################################################################
#################################################################################################################
# 4) Estadístico Goodman-Kruskal tau para la concordancia entre grupos de crecimiento con grupos de reclutamiento
#para cada muestreo
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1) 11.4 meses (modelo 1)
colnames(grupo.crecimiento.reclutamiento)
grupos.crecimiento.reclutamiento<- 
  grupo.crecimiento.reclutamiento[, c(2,8)]
head(grupos.crecimiento.reclutamiento)
dim(grupos.crecimiento.reclutamiento)
# Quitar NAs
rows.with.na <-
  unique(which(is.na(grupos.crecimiento.reclutamiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 6 filas con NA

#correr las siguientes líneas en caso de existir NAs
grupos.crecimiento.reclutamiento <-
  grupos.crecimiento.reclutamiento[-rows.with.na,]
dim(grupos.crecimiento.reclutamiento) #
class(grupos.crecimiento.reclutamiento)
summary(grupos.crecimiento.reclutamiento)
head(grupos.crecimiento.reclutamiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento con grupos de
#reclutamiento a 11.4: tau(C, R) y tau(R, C)
GKtau(grupos.crecimiento.reclutamiento[,1],#Crecimiento
      grupos.crecimiento.reclutamiento[,2]) # reclutamiento
# xName                                 yName Nx Ny tauxy
# 1 grupos.crecimiento.reclutamiento[, 1] grupos.crecimiento.reclutamiento[, 2]  2  3 0.032
# tauyx
# 1 0.046

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.reclutamiento[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.reclutamiento[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}
# Gráfica de la distribución nula de tau(R, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(R, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
    
  )[[5]],
  col = "red"
)
# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(R, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.48779

# Gráfica de la distribución nula de tau(C, R),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, R)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, R) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.48779

#################################################################################################################
# 4.2) 11.4 meses (modelo 2)

grupos.crecimiento.reclutamiento<- 
  grupo.crecimiento.reclutamiento[, c(3,8)]
head(grupos.crecimiento.reclutamiento)
dim(grupos.crecimiento.reclutamiento)
# Eliminar NAs
rows.with.na <-
  unique(which(is.na(grupos.crecimiento.reclutamiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 6 filas con NA

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento.reclutamiento <-
  grupos.crecimiento.reclutamiento[-rows.with.na,]
dim(grupos.crecimiento.reclutamiento) #
class(grupos.crecimiento.reclutamiento)
summary(grupos.crecimiento.reclutamiento)
head(grupos.crecimiento.reclutamiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento con grupos de
#reclutamiento a 11.4: tau(C, R) y tau(R, C)
GKtau(grupos.crecimiento.reclutamiento[,1],#Crecimiento
      grupos.crecimiento.reclutamiento[,2]) # reclutamiento
# xName                                 yName Nx Ny tauxy
# 1 grupos.crecimiento.reclutamiento[, 1] grupos.crecimiento.reclutamiento[, 2]  2  3 0.065
# tauyx
# 1 0.094

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.reclutamiento[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.reclutamiento[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(R, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(R, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(R, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.14643

# Gráfica de la distribución nula de tau(C, R),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, R)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, R) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.34357

#################################################################################################################
# 4.3) 19.6 meses (1)

grupos.crecimiento.reclutamiento<- 
  grupo.crecimiento.reclutamiento[, c(4,9)]
head(grupos.crecimiento.reclutamiento)
dim(grupos.crecimiento.reclutamiento)
# Eliminar NAs
rows.with.na <-
  unique(which(is.na(grupos.crecimiento.reclutamiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 8 filas con NAs

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento.reclutamiento <-
  grupos.crecimiento.reclutamiento[-rows.with.na,]
dim(grupos.crecimiento.reclutamiento) #
class(grupos.crecimiento.reclutamiento)
summary(grupos.crecimiento.reclutamiento)
head(grupos.crecimiento.reclutamiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento con grupos de
#reclutamiento a 19.6 meses: tau(C, R) y tau(R, C)
GKtau(grupos.crecimiento.reclutamiento[,1],
      grupos.crecimiento.reclutamiento[,2]) 
# xName                                 yName Nx Ny tauxy
# 1 grupos.crecimiento.reclutamiento[, 1] grupos.crecimiento.reclutamiento[, 2]  3  3 0.131
# tauyx
# 1  0.06

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.reclutamiento[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.reclutamiento[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(R, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(R, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(R, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.0634

# Gráfica de la distribución nula de tau(C, R),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, R)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, R) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
#  0.38234

#################################################################################################################
# 4.4) 19.6 meses (2)

grupos.crecimiento.reclutamiento<- 
  grupo.crecimiento.reclutamiento[, c(5,9)]
head(grupos.crecimiento.reclutamiento)
dim(grupos.crecimiento.reclutamiento)
# Eliminar NAs
rows.with.na <-
  unique(which(is.na(grupos.crecimiento.reclutamiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 8 filas con NAs

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento.reclutamiento <-
  grupos.crecimiento.reclutamiento[-rows.with.na,]
dim(grupos.crecimiento.reclutamiento) #
class(grupos.crecimiento.reclutamiento)
summary(grupos.crecimiento.reclutamiento)
head(grupos.crecimiento.reclutamiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de crecimiento con grupos de
#reclutamiento a 19.6 meses: tau(C, R) y tau(R, C)
GKtau(grupos.crecimiento.reclutamiento[,1],
      grupos.crecimiento.reclutamiento[,2]) 
# xName                                 yName
# 1 grupos.crecimiento.reclutamiento[, 1] grupos.crecimiento.reclutamiento[, 2]
# Nx Ny tauxy tauyx
# 1  2  3 0.035 0.049

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.reclutamiento[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.reclutamiento[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(R, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(R, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(R, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.28605

# Gráfica de la distribución nula de tau(C, R),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, R)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, R) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
#  0.4657

#################################################################################################################
# 4.4) 51.4 meses

grupos.crecimiento.reclutamiento<- 
  grupo.crecimiento.reclutamiento[, c(6, 10)]
head(grupos.crecimiento.reclutamiento)
dim(grupos.crecimiento.reclutamiento)
# Eliminar NAs
rows.with.na <-
  unique(which(is.na(grupos.crecimiento.reclutamiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento.reclutamiento <-
  grupos.crecimiento.reclutamiento[-rows.with.na,]
dim(grupos.crecimiento.reclutamiento) #
class(grupos.crecimiento.reclutamiento)
summary(grupos.crecimiento.reclutamiento)
head(grupos.crecimiento.reclutamiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento con grupos de
#reclutamiento a 51.4: tau(C, R) y tau(R, C)
GKtau(grupos.crecimiento.reclutamiento[,1],
      grupos.crecimiento.reclutamiento[,2]) 
# xName                                 yName Nx Ny tauxy
# 1 grupos.crecimiento.reclutamiento[, 1] grupos.crecimiento.reclutamiento[, 2]  2  2 0.149
# tauyx
# 1 0.149

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.reclutamiento[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.reclutamiento[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(R, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(R, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(R, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.15407

# Gráfica de la distribución nula de tau(C, R),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, R)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, R) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.30144

#################################################################################################################
# 4.5) 51.4 meses (modelo 2)

grupos.crecimiento.reclutamiento<- 
  grupo.crecimiento.reclutamiento[, c(6, 11)]
head(grupos.crecimiento.reclutamiento)
dim(grupos.crecimiento.reclutamiento)
# Eliminar NAs
rows.with.na <-
  unique(which(is.na(grupos.crecimiento.reclutamiento), arr.ind = T)[, 1])
rows.with.na # 
length(rows.with.na)# 

# Correr las siguientes líneas en caso de existir NAs
grupos.crecimiento.reclutamiento <-
  grupos.crecimiento.reclutamiento[-rows.with.na,]
dim(grupos.crecimiento.reclutamiento) #
class(grupos.crecimiento.reclutamiento)
summary(grupos.crecimiento.reclutamiento)
head(grupos.crecimiento.reclutamiento)

# Calcular los estadísticos Goodman-Kruskal tau para la concordanc ia entre grupos de crecimiento con grupos de
#reclutamiento a 51.4: tau(C, R) y tau(R, C)
GKtau(grupos.crecimiento.reclutamiento[,1],
      grupos.crecimiento.reclutamiento[,2]) 
# xName                                 yName Nx Ny
# 1 grupos.crecimiento.reclutamiento[, 1] grupos.crecimiento.reclutamiento[, 2]  2  3
# tauxy tauyx
# 1 0.052 0.091

# Modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupos.crecimiento.reclutamiento[,2])
  GKtau.nulo <-
    GKtau(grupos.crecimiento.reclutamiento[,1],
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

# Gráfica de la distribución nula de tau(R, C),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(R, C)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
    
  )[[5]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(R, C) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2])[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.04803

# Gráfica de la distribución nula de tau(C, R),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(C, R)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
# Mostrar el valor observado
abline(
  v = GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]],
  col = "red"
)

# Calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(C, R) al menos tan extremo como el observado):
sum(
  GKtau(
    grupos.crecimiento.reclutamiento[,1],
    grupos.crecimiento.reclutamiento[,2]
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.04803
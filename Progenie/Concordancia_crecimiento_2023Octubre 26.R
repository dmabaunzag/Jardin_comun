#################################################################################################################
#################################################################################################################
#################################################################################################################
#Concordacia entre tiempos de crecimientos: 11 meses, un año y seis meses y cuatro años y dos meses DDS
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
# 1) Leer tablas de asignación de grupos
###################################################################################################################
###################################################################################################################
#Tablas de asignación de grupos de las plantas madre según el creceimiento de su progenie mediedo en tres ocaciones
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
modelo.1.marzo<-read.table("PhenotypicGroupAssignment.piloto.(marzo).VARS_2023septiembre03_112042.csv",
                           header = T,
                           sep = ","
                           )
summary(modelo.1.marzo)
head(modelo.1.marzo)
dim(modelo.1.marzo)
modelo.2.marzo<-read.table("PhenotypicGroupAssignment.piloto.(marzo).STD_2023septiembre03_120715.csv",
                           header = T,
                           sep = ","
                           )
summary(modelo.2.marzo)
head(modelo.2.marzo)
dim(modelo.2.marzo)
modelo.octubre<-read.table("PhenotypicGroupAssignment.piloto.(octubre)_2023agosto30_082143.csv",
                           header = T,
                           sep = ","
                           )
summary(modelo.octubre)
head(modelo.octubre)
dim(modelo.octubre)
modelo.1.junio<-read.table("PhenotypicGroupAssignment.piloto.(junio).VARS_2023septiembre04_083353.csv",
                           header = T,
                           sep = ","
                           )
summary(modelo.1.junio)
head(modelo.1.junio)
dim(modelo.1.junio)
modelo.2.junio<-read.table("PhenotypicGroupAssignment.piloto.(junio).SPH_2023septiembre04_090428.csv",
                           header = T,
                           sep = ","
                           )
summary(modelo.2.junio)
head(modelo.2.junio)
dim(modelo.2.junio)

#Asignación de grupos de las plantas madre según su morfología
setwd("C:/Users/usuario/Documents/Jardin_comun")
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

###################################################################################################################
###################################################################################################################
# 2) creación de tabla con la asignación de grupo para cada modelo por cada ocasión de medida y por morfología
###################################################################################################################
###################################################################################################################

phenotypic.group.assignment.crecimiento <-
  merge(
  modelo.1.marzo[, c(2, 3)],
  merge(
    modelo.2.marzo[, c(2, 3)],
    merge(
      modelo.octubre[, c(2, 3)],
      merge(
        modelo.1.junio[, c(2, 3)],
        modelo.2.junio[, c(2, 3)],
        by = "Collector.Collection.Number",
        all = T,
        suffixes = c("modelo.1.junio", ".modelo.2.junio")),
  by="Collector.Collection.Number",
  all = T),
  by="Collector.Collection.Number",
  all = T),
  by="Collector.Collection.Number",
  all = T)

View(phenotypic.group.assignment.crecimiento)

colnames(phenotypic.group.assignment.crecimiento) <-
  c(
    "Collector.Collection.Number",
    "modelo.1.marzo",
    "modelo.2.marzo",
    "modelo.octubre",
    "modelo.1.junio",
    "modelo.2.junio"
  )

#Extracción del número de colección de las plantas madre
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))

#Agregar asiganció de grupos según morfología

phenotypic.group.assignment.crecimiento<-
  merge(phenotypic.group.assignment.crecimiento,
        phenotypic.group.assignment.madres[, c(2,6)],
        by="Collector.Collection.Number")

#guardar tabla con asiganión de grupos de las plantas madre según crecimiento de su progenie y morfología

# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
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
# 3.1) 11 meses DDS
# 3.1.1) modelo1 (filas) vs modelo2 (columnas) marzo (11 meses DDS)
table(phenotypic.group.assignment.crecimiento[,2],
      phenotypic.group.assignment.crecimiento[,3],
      exclude=NULL)
#   1  2
# 1  1  1
# 2 35  0

# 3.1.2) modelo octubre (1 año y c4 meses DDs) vs modelo2 (columnas) marzo (11 meses DDS)
table(phenotypic.group.assignment.crecimiento[,4],
      phenotypic.group.assignment.crecimiento[,3], 
      exclude=NULL)
#       1  2
# 1    29  0
# 2     6  0
# <NA>  1  1

# 3.1.3) modelo1 junio (4 años y 2 meses DDS) vs modelo2 (columnas) marzo (11 meses DDS)
table(phenotypic.group.assignment.crecimiento[,5],
      phenotypic.group.assignment.crecimiento[,3], 
      exclude=NULL)
#       1  2
# 1    30  0
# 2     3  0
# <NA>  3  1

# 3.1.4) modelo2 junio (4 años y 2 meses DDS) vs modelo2 (columnas) marzo (11 meses DDS)
table(phenotypic.group.assignment.crecimiento[,6],
      phenotypic.group.assignment.crecimiento[,3], 
      exclude=NULL)
#       1  2
# 1    28  0
# 2     5  0
# <NA>  3  1

# 3.1.5) morfología (fila) vs modelo2 (columnas) marzo (11 meses DDS)
table(phenotypic.group.assignment.crecimiento[,3],
      phenotypic.group.assignment.crecimiento[,7], 
      exclude=NULL)
# 2  3  4  5
# 1  6 15  5 10
# 2  0  0  0  1

###################################################################################################################
# 3.2) un año y cuatro meses DDS
# 3.2.1) modelo1 (filas) marzo (11 meses) vs modelo octubre (1 año y 4 meses DDS)
table(phenotypic.group.assignment.crecimiento[,2],
      phenotypic.group.assignment.crecimiento[,4],
      exclude=NULL)
#     1  2   <NA>
#   1  0  1    1
#   2 29  5    1

# 3.2.2) modelo2 (filas) marzo (11 meses) vs modelo octubre (1 año y 4 meses DDS)
table(phenotypic.group.assignment.crecimiento[,3],
      phenotypic.group.assignment.crecimiento[,4],
      exclude=NULL)
#     1  2    <NA>
#   1 29  6    1
#   2  0  0    1

# 3.2.3) modelo1 (filas) junio (4 años y 2 meses DDS) vs modelo octubre (1 año y 4 meses DDS)
table(phenotypic.group.assignment.crecimiento[,5],
      phenotypic.group.assignment.crecimiento[,4],
      exclude=NULL)
#           1  2  <NA>
#     1    25  5    0
#     2     2  1    0
#   <NA>    2  0    2

# 3.2.4) modelo2 (filas) junio (4 años y 2 meses DDS) vs modelo octubre (1 año y 4 meses DDS)
table(phenotypic.group.assignment.crecimiento[,6],
      phenotypic.group.assignment.crecimiento[,4],
      exclude=NULL)
#         1  2 <NA>
#   1    23  5    0
#   2     4  1    0
#   <NA>  2  0    2

# 3.2.5) morfología (filas)  vs modelo octubre (1 año y 4 meses DDS)
table(phenotypic.group.assignment.crecimiento[,4],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)
#       2  3  4  5
# 1     5 13  4  7
# 2     0  2  1  3
# <NA>  1  0  0  1

###################################################################################################################
# 3.3) cuatro años y dos meses DDS
# 3.3.1) modelo1 (filas) marzo (11 meses) vs modelo1 junio (4 años y 2 meses DDS)
table(phenotypic.group.assignment.crecimiento[,2],
      phenotypic.group.assignment.crecimiento[,5],
      exclude=NULL)
#       1  2 <NA>
#   1  1  0    1
#   2 29  3    3

# 3.3.2) modelo2 (filas) marzo (11 meses) vs modelo1 junio (4 años y 2 meses DDS)
table(phenotypic.group.assignment.crecimiento[,3],
      phenotypic.group.assignment.crecimiento[,5],
      exclude=NULL)
#       1  2 <NA>
#   1 30  3    3
#   2  0  0    1

# 3.3.3) modelo octubre (1 año y 4 meses) vs modelo1 junio (4 años y 2 meses DDS)
table(phenotypic.group.assignment.crecimiento[,4],
      phenotypic.group.assignment.crecimiento[,5],
      exclude=NULL)
#       1  2 <NA>
# 1    25  2    2
# 2     5  1    0
# <NA>  0  0    2

# 3.3.4) modelo2 junio (4 años y 2 meses) vs modelo1 junio (4 años y 2 meses DDS)
table(phenotypic.group.assignment.crecimiento[,6],
      phenotypic.group.assignment.crecimiento[,5],
      exclude=NULL)

#       1  2 <NA>
# 1    28  0    0
# 2     2  3    0
# <NA>  0  0    4

# 3.3.5) morfología vs modelo1 junio (4 años y 2 meses DDS)
table(phenotypic.group.assignment.crecimiento[,5],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)

#       2  3  4  5
# 1     5 11  5  9
# 2     0  3  0  0
# <NA>  1  1  0  2

###################################################################################################################
# 3.4) morfología
# 3.3.1) modelo1 (filas) marzo (11 meses) vs morfología (columnas)
table(phenotypic.group.assignment.crecimiento[,2],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)
#    2  3  4  5
# 1  0  0  1  1
# 2  6 15  4 10

# 3.3.2) modelo2 (filas) marzo (11 meses) vs morfología (columnas)
table(phenotypic.group.assignment.crecimiento[,3],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)
#    2  3  4  5
# 1  6 15  5 10
# 2  0  0  0  1

# 3.3.3) modelo octubre (1 año y 4 meses) vs morfología (columnas)
table(phenotypic.group.assignment.crecimiento[,4],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)
#       2  3  4  5
# 1     5 13  4  7
# 2     0  2  1  3
# <NA>  1  0  0  1

# 3.3.4) modelo1 junio (4 años y 2 meses) vs morfología (columnas)
table(phenotypic.group.assignment.crecimiento[,5],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)
#       2  3  4  5
# 1     5 11  5  9
# 2     0  3  0  0
# <NA>  1  1  0  2

# 3.3.5) modelo2 junio (4 años y 2 meses) vs morfología (columnas)
table(phenotypic.group.assignment.crecimiento[,6],
      phenotypic.group.assignment.crecimiento[,7],
      exclude=NULL)
#      2 3 4 5
# 1    5 9 5 9
# 2    0 5 0 0
# <NA> 1 1 0 2
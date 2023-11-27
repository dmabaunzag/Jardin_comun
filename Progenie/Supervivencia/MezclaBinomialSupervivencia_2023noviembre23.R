#################################################################################################################
#################################################################################################################
#################################################################################################################
#
##### MODELO DE MEZCLAS BINOMIALES PARA SUPERVIVENCIA DE LA PROGENIE DE LAS PLANTAS MADRE#######
#
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
# INTRODUCCIÓN:
# Con los datos de las platnas que sobrevivieron en los tres tiempos de medidia se realizó modelos de mezclas
#binomiales que agrupan a las ´plantas madre según la superviviencia de la progenie
#
# REQUERIMIENTOS
# "PhenotypicDataProgeny_Quebradas_2020Marzo.csv"
# "PhenotypicDataProgeny_Quebradas_2020Octubre.csv"
# "PhenotypicDataProgeny_Quebradas_2023Junio.csv"
# "PhenotypicGroupAssignment_2023septiembre08_120644.csv"
#
#
# CONTENIDO
# 1) Datos preliminares: Carga de librerías y lectura de datos
# 2) Seleccionar las variables a usar en cada tabla
# 3) Crear marco de datos de la supervivencia
# 4) Guardar marco de datos
# 5) Crear tabla con número de sobrevivientes por planta madre
# 6) Examinar la supervivencia en cada ocasión de medida
# 7) Ajustar modelos de mezclas binomiales
# 8) Tablas de clasificación cruzadas entre modelos de supervivencia y entre morfología
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
# 1.1) Librerías.

#install.packages("flemix")
library(flexmix)# Modelos de mezclas binomiales
library(tidyverse) #limpiar y organizar datos
library(GoodmanKruskal)# paquete GoodmanKruskal


#seleccionar directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/data")

#################################################################################################################
# 1.2)leer las tablas

datos.fenotipicos.marzo <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2020Marzo.csv",
    header = T,
    sep = ","
  )
datos.fenotipicos.octubre <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2020Octubre.csv",
    header = T,
    sep = ","
  )
datos.fenotipicos.junio <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2023Junio.csv",
    header = T,
    sep = ","
  )

#################################################################################################################
#################################################################################################################
# 2) Seleccionar las variables a usar en cada tabla
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 2.1)primera medida (11.4 meses DS): marzo de 2020
summary(datos.fenotipicos.marzo)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables

# 2.1.1)subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.marzo)
datos.fenotipicos.marzo.selected <- datos.fenotipicos.marzo %>%
  unite("nombre.progenie", c(Bandeja, Fila, Columna)) %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    Longitud.tallo..cm.,
    Número.de.hojas,
    OBSERVACIONES
  ) %>%
  mutate(
    Fecha.trasplante = dmy(Fecha.trasplante),
    Fecha.siembra = dmy(Fecha.siembra),
    Fecha.medición = dmy(Fecha.medición)
  )
# 2.1.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de medida
supervivencia.1 <- datos.fenotipicos.marzo.selected %>%
  mutate(supervivencia.1 = if_else(
    is.na(Longitud.tallo..cm.) & is.na(Número.de.hojas),
    "M",
    "V",
    "M"
  ))
view(supervivencia.1)

supervivencia.1 <- supervivencia.1 %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    supervivencia.1
  )

#################################################################################################################
# 2.2) segunda medida (19.6 meses DS): octubre de 2020
summary(datos.fenotipicos.octubre)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables


# 2.2.1) subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.marzo)
datos.fenotipicos.octubre.selected <- datos.fenotipicos.octubre %>%
  unite("nombre.progenie", c(Bandeja, Fila, Columna)) %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    Longitud.tallo..cm.,
    Número.de.hojas,
    OBSERVACIONES
  ) %>%
  mutate(
    Fecha.trasplante = dmy(Fecha.trasplante),
    Fecha.siembra = dmy(Fecha.siembra),
    Fecha.medición = dmy(Fecha.medición)
  )
view(datos.fenotipicos.octubre.selected)

# 2.2.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.2 <- datos.fenotipicos.octubre.selected %>%
  mutate(supervivencia.2 = if_else(is.na(Fecha.medición), "M", "V", "M"))
view(supervivencia.2)

supervivencia.2 <- supervivencia.2 %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    supervivencia.2
  )
View(supervivencia.2)
#################################################################################################################
# 2.3) tercera medida (51.4 meses DS): junio de 2023
summary(datos.fenotipicos.junio)
head(datos.fenotipicos.junio)
dim(datos.fenotipicos.junio)# 179 plantas hijas sembradas y 26 variables

# 2.3.1)subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.junio)
datos.fenotipicos.junio.selected <- datos.fenotipicos.junio %>%
  unite("nombre.progenie", c(Bandeja, Fila, Columna)) %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.transplante,
    Fecha.medición,
    Longitud.tallo..mm.,
    Número.de.hojas,
    OBSERVACIONES
  ) %>%
  rename(Fecha.trasplante = Fecha.transplante) %>%
  mutate(
    Fecha.trasplante = mdy(Fecha.trasplante),
    Fecha.siembra = mdy(Fecha.siembra),
    Fecha.medición = mdy(Fecha.medición),
    Número.colección.planta.madre = as.character(Número.colección.planta.madre)
  )
view(datos.fenotipicos.junio.selected)

# 2.3.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.3 <- datos.fenotipicos.junio.selected %>%
  mutate(supervivencia.3 = if_else(is.na(Fecha.medición), "M", "V", "M"))
view(supervivencia.3)

supervivencia.3 <- supervivencia.3 %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    supervivencia.3
  )
View(supervivencia.3)
#################################################################################################################
#################################################################################################################
# 3) Crear marco de datos de la supervivencia
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 3.1) combinar la tabla supervivencia 1 y supervivencia 2

supervivencia <-
  left_join(
    supervivencia.1,
    supervivencia.2,
    by = join_by(
      Número.colección.planta.madre,
      nombre.progenie,
      Fecha.siembra,
      Fecha.trasplante
    )
  )
#################################################################################################################
# 3.2)combinar supervivencia con supervivencia 3

supervivencia <-
  left_join(
    supervivencia,
    supervivencia.3,
    by = join_by(
      Número.colección.planta.madre,
      nombre.progenie,
      Fecha.siembra,
      Fecha.trasplante
    )
  )
view(supervivencia)
#################################################################################################################
# 3.3) Cambiar los NAS del muestreo 3 por "M"
supervivencia <-
  supervivencia %>%  replace_na(list(supervivencia.3 = "M"))
#################################################################################################################
# 3.4 )renombrar las variable

supervivencia <- supervivencia %>%
  rename(
    Fecha.medición.1 = Fecha.medición.x,
    Fecha.medición.2 = Fecha.medición.y,
    Fecha.medición.3 = Fecha.medición
  )
view(supervivencia)
#################################################################################################################
#################################################################################################################
# 4) Guardar marco de datos
#################################################################################################################
#################################################################################################################

# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")
#
# #RData
# save(supervivencia, file= paste("Supervivencia_piloto_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".RData", sep=""))
#
# #.csv
# write.csv(supervivencia, file= paste("supervivencia_piloto_",format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)

#load ("Supervivencia_piloto_2023noviembre20_021834.RData")

#################################################################################################################
#################################################################################################################
# 5) Crear tabla con número de sobrevivientes por planta madre.
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 5.1)Remover plantas madres sin número de colección.

supervivencia$Número.colección.planta.madre <-
  as.numeric(supervivencia$Número.colección.planta.madre)
supervivencia <-
  supervivencia[!is.na(supervivencia$Número.colección.planta.madre), ]
dim(supervivencia) # 248 plantas de la progenie con madre asignada

#################################################################################################################
# 5.2) Crear tabla.

vivas.supervivencia.1 <-
  as.data.frame(table(supervivencia[, c(1, 6)]))
vivas.supervivencia.1 <-
  vivas.supervivencia.1[vivas.supervivencia.1$supervivencia.1 == "V", c(1, 3)]
vivas.supervivencia.2 <-
  as.data.frame(table(supervivencia[, c(1, 8)]))
vivas.supervivencia.2 <-
  vivas.supervivencia.2[vivas.supervivencia.2$supervivencia.2 == "V", c(1, 3)]
vivas.supervivencia.3 <-
  as.data.frame(table(supervivencia[, c(1, 10)]))
vivas.supervivencia.3 <-
  vivas.supervivencia.3[vivas.supervivencia.3$supervivencia.3 == "V", c(1, 3)]

sobrevivientes <- data.frame(vivas.supervivencia.1,
                             vivas.supervivencia.2[, 2],
                             vivas.supervivencia.3[, 2])
View(sobrevivientes)
colnames(sobrevivientes) <-
  c("Collector.Collection.Number",
    "vivas.1",
    "vivas.2",
    "vivas.3")

#Agregar las plantas madre que no nacieron ninguna hija mediante la tabla de asignación de grupos de las plantas madre
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/data")#directorio de los datos de las plantas madres
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/MedicionesPlantasMadre") #Ivan's working directory Waterman
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )

#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350, ]
head(phenotypic.group.assignment.madres)
dim(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))
sobrevivientes.all <-
  merge(
    x = phenotypic.group.assignment.madres[, c(2, 3)],
    y = sobrevivientes,
    all = T,
    row.names = NULL
  )
sobrevivientes.all[is.na(sobrevivientes.all)] <- 0
sobrevivientes.all <- sobrevivientes.all[-2]
class(sobrevivientes.all)
dim(sobrevivientes.all)
head(sobrevivientes.all)

#################################################################################################################
#################################################################################################################
# 6) Examinar la supervivencia
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 6.1) crear marco de datos para analizar sobreviviencia
sobrevivientes.tiempo <-
  sobrevivientes.all
colnames(sobrevivientes.tiempo) <-
  c("Collector.Collection.Number", "11.4", "19.6", "51.4")
sobrevivientes.tiempo <-
  sobrevivientes.tiempo %>%
  gather(time, sobrevivientes, "11.4":"51.4")

# gráfica por tiempo de medida
promedio.supervivencia <-
  sobrevivientes.tiempo %>%
  group_by(time) %>%
  summarise(
    promedio = mean(sobrevivientes),
    mediana = median(sobrevivientes),
    Q5 = quantile(sobrevivientes, 0.05),
    Q95 = quantile(sobrevivientes, 0.95),
  ) %>%
  ungroup()
promedio.supervivencia
# time promedio mediana    Q5   Q95
# <dbl>    <dbl>   <dbl> <dbl> <dbl>
#   1  11.4     5.51       4     0  15.9
# 2  19.6     5.14       4     0  14
# 3  51.4     4.07       2     0  11
sobrevivientes.tiempo %>%
  group_by(time) %>%
  summarise(suma = sum(sobrevivientes))
# time   suma
# <chr> <dbl>
#   1 11.4    237
# 2 19.6    221
# 3 51.4    175
promedio.supervivencia$time <-
  as.numeric(promedio.supervivencia$time)
#gráfica promedio en el tiempo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
plot(
  promedio.supervivencia[, 1:2],
  axes = F,
  type = "o",
  pch = 19,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 16),
  xlim = c(11.4, 60),
  xaxt = "n",
  bty = "n",
  xlab = "meses después de la siembra",
  ylab = "% supervivencia"
)
title(expression("A) "), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = c(11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 16, 1),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 16, 0.5),
  labels = F,
  tcl = -0.3
)
lines(
  promedio.supervivencia$time,
  promedio.supervivencia$mediana,
  type = "o",
  pch = 24
)
lines(
  promedio.supervivencia$time,
  promedio.supervivencia$Q5,
  lty = 3,
  col = "gray60"
)

lines(
  promedio.supervivencia$time,
  promedio.supervivencia$Q95,
  lty = 3,
  col = "gray60"
)
text(57.5, 4.07, "Promedio")
text(57.5, 2, "Mediana")
text(57, 0, "Q5%")
text(57, 11, "Q95%")


# gráfica por tiempo de medida y planta madre
promedio.supervivencia.madre <-
  sobrevivientes.tiempo %>%
  group_by(Collector.Collection.Number, time) %>%
  summarise(promedio = mean(sobrevivientes)) %>%
  spread(Collector.Collection.Number, promedio) %>%
  ungroup()


promedio.supervivencia.madre$time <-
  as.numeric(promedio.supervivencia.madre$time)
#gráfica promedio en el tiempo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  promedio.supervivencia.madre$time,
  promedio.supervivencia.madre[,-1],
  axes = F,
  type = "l",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 25),
  xlim = c(11.4, 60),
  xaxt = "n",
  bty = "n",
  col = "gray60",
  xlab = "meses después de la siembra",
  ylab = "% supervivencia"
)
title(expression("B)"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = c(11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 25, 5),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 25, 0.5),
  labels = F,
  tcl = -0.3
)


#################################################################################################################
#################################################################################################################
# 7) Ajustar modelos de mezclas binomiales.
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 7.1) primera medición: 11.4 meses después de la siembra.

vivas1 <- sobrevivientes.all$vivas.1
mode <- function(x) {
  return(as.numeric(names(which.max(table(
    x
  )))))
}
mode(vivas1)
max(vivas1)
mean(vivas1)#5.511628
sd(vivas1) # 5.856913
sum(vivas1)
Conc  <- FLXPmultinom(~ 1)
Mod.fam <- FLXglm(~ 1, family = "binomial")
siembra <-
  100 # number of trials of the binomial components (max number of credits)
Modelos1 <-
  stepFlexmix(
    cbind(vivas1, siembra - vivas1) ~ 1,
    model = Mod.fam,
    k = 1:6,
    concomitant = Conc,
    nrep = 5
  )
show(Modelos1)
# Call:
#   stepFlexmix(cbind(vivas1, siembra - vivas1) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 1:6, nrep = 5)
#
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -190.7586 383.5173 385.2785 385.2785
# 2   11      TRUE 2  2 -129.9783 265.9565 271.2401 275.3472
# 3   24      TRUE 3  3 -118.7065 247.4131 256.2191 263.1446
# 4   97      TRUE 4  4 -117.7791 249.5583 261.8867 275.1548
# 5   68      TRUE 5  5 -117.7791 253.5582 269.4090 298.0531
# 6   53      TRUE 6  6 -117.7807 257.5613 276.9345 320.2890

sort(BIC(Modelos1))
#        3        4        5        2        6        1
# 256.2191 261.8866 269.4090 271.2400 276.9311 385.2785
plot(Modelos1)
getModel(Modelos1, which = "BIC")
# Call:
#   stepFlexmix(cbind(vivas1, siembra - vivas1) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 3, nrep = 5)
#
# Cluster sizes:
#   1  2  3
# 5 18 20
#
# convergence after 26 iterations


#then we keep the best model in terms of BIC
MejorModelo1  <- getModel(Modelos1, "BIC")
summary(MejorModelo1)
MejorModelo1@df #número de parámetros en el modelo
str(MejorModelo1)

plot(MejorModelo1)
BIC(MejorModelo1)

# parámetro binomial para cada componente
fitted(MejorModelo1)
p.MejorModelo1 <- fitted(MejorModelo1)[1,]
#otra forma de obtener el parámetro binomial para cada componente
MejorModelo1@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(MejorModelo1) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(MejorModelo1, component = 1)
exp(parameters(MejorModelo1)[1]) / (1 + exp(parameters(MejorModelo1)[1]))
exp(parameters(MejorModelo1)[2]) / (1 + exp(parameters(MejorModelo1)[2]))
exp(parameters(MejorModelo1)[3]) / (1 + exp(parameters(MejorModelo1)[3]))

MejorModelo1@prior #probabilidad previa
MejorModelo1@posterior #probabilidad posterior
MejorModelo1@cluster #asignación grupo


grupo.supervivencia.1 <-
  data.frame(sobrevivientes.all[, c(1, 2)],
             MejorModelo1@cluster)
colnames(grupo.supervivencia.1) <-
  c("Collector.Collection.Number",
    "Vivas.1",
    "Phenotypic.Group")

head(grupo.supervivencia.1)

# guardar tabla de asignación de grupos
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#     grupo.supervivencia.1,
#     file = paste(
#       "grupo.supervivencia.1_",
#       format(Sys.time(), "%Y%B%d_%H%M%S"),
#       ".csv",
#       sep = ""
#          ),
#       row.names = F
#     )

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo
p.comp.1 <-
  exp(parameters(MejorModelo1)[1]) / (1 + exp(parameters(MejorModelo1)[1])) #parametro p grupo 1
pi.comp.1 <-
  sum(MejorModelo1@cluster == 1) / length(MejorModelo1@cluster) #parametro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(MejorModelo1)[2]) / (1 + exp(parameters(MejorModelo1)[2])) #parametro p grupo 2
pi.comp.2 <-
  sum(MejorModelo1@cluster == 2) / length(MejorModelo1@cluster) #parametro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2
p.comp.3 <-
  exp(parameters(MejorModelo1)[3]) / (1 + exp(parameters(MejorModelo1)[3])) #parametro p grupo 1
pi.comp.3 <-
  sum(MejorModelo1@cluster == 3) / length(MejorModelo1@cluster) #parametro pi grupo 3
pm.comp.3 <-
  pi.comp.3 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.3,
                     log = FALSE)#masa de probabilidad grupo 3

#representación gráfica del mejor modelo: gráficos de barras representando
#la asignación de cada planta madre a cada componente del mejor modelo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  vivas1[MejorModelo1@cluster == 2],
  breaks = seq(-0.5, 24.5, 1),
  xlab = "Número de plantas hijas",
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xaxt = "n",
  yaxt = "n",
  bty = "n"
)
title(expression("A) 11.4 meses"),
      adj = 0,
      cex.main = 1.5)
axis(
  side = 1,
  at = 0:24,
  labels = F,
  tcl = -0.5,
  cex.axis = 1.3
)
axis(
  side = 1,
  at = seq(0, 24, 4),
  labels = T,
  tcl = -0.7,
  cex.axis = 1.3
)
axis(
  side = 2,
  at = 0:10,
  labels = T,
  tcl = -0.5,
  cex.axis = 1.3
)
hist(
  vivas1[MejorModelo1@cluster == 3],
  breaks = seq(-0.5, 24.5, 1),
  add = T,
  density = 15,
  col = "black"
)
hist(vivas1[MejorModelo1@cluster == 1],
     breaks = seq(-0.5, 24.5, 1),
     add = T,
     col = "transparent")
#hist(vivas1[MejorModelo1@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=-45, col="black")
#hist(vivas1[MejorModelo1@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=45, col="black")
#leyenda
legend(
  16,
  9,
  c("S1", "S2", "S3"),
  pch = c(24, 4, 19),
  lty = 1,
  pt.cex = c(1.5, 1, 1),
  cex = 1.5
)
legend(
  8,
  9,
  c("S1", "S2", "S3"),
  density = c(NA, NA, 15),
  fill = c("transparent", "gray90", "black"),
  cex = 1.5
)
#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.3,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 19,
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  bty = "n"
)
points(
  0:24,
  pm.comp.2,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 4
)
points(
  0:24,
  pm.comp.1,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 24,
  cex = 1.5
)
axis(side = 4, cex.axis = 1.5)
mtext(side = 4,
      "Probabilidad",
      cex = 1.5,
      line = 2.8)


#################################################################################################################
# 7.2) segunda medición: 19.6 meses después de la siembra.
vivas2 <- sobrevivientes.all$vivas.2
mode(vivas2)# 0
max(vivas2)
sum(vivas2)#221 plantas hijas
mean(vivas2)#5.511628
sd(vivas2) # 5.856913
Conc  <- FLXPmultinom(~ 1)
Mod.fam <- FLXglm(~ 1, family = "binomial")
siembra <-
  100 # number of trials of the binomial components (max number of credits)
Modelos2 <-
  stepFlexmix(
    cbind(vivas2, siembra - vivas2) ~ 1,
    model = Mod.fam,
    k = 1:6,
    concomitant = Conc,
    nrep = 5
  )
show(Modelos2)
# Call:
#   stepFlexmix(cbind(vivas2, siembra - vivas2) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 1:6, nrep = 5)
#
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -184.6619 371.3237 373.0849 373.0849
# 2   10      TRUE 2  2 -125.9278 257.8557 263.1393 269.3732
# 3   30      TRUE 3  3 -116.7673 243.5347 252.3407 259.2866
# 4   28      TRUE 4  4 -115.4161 244.8323 257.1607 270.2430
# 5   55      TRUE 5  5 -115.4166 248.8333 264.6841 290.1837
# 6   60      TRUE 6  6 -115.4165 252.8330 272.2062 305.2969
plot(Modelos2)
getModel(Modelos2, which = "BIC")
#Call:
# stepFlexmix(cbind(vivas2, siembra - vivas2) ~ 1, model = Mod.fam,
#             concomitant = Conc, k = 3, nrep = 5)
#
# Cluster sizes:
#   1  2  3
# 19  5 19
#
# convergence after 26 iterations
sort(BIC(Modelos2))
# 3        4        2        5        6        1
# 252.3407 257.1607 263.1393 264.6841 272.2062 373.0849

#then we keep the best model in terms of BIC
MejorModelo2  <- getModel(Modelos2, "BIC")
summary(MejorModelo2)
# Call:
#   stepFlexmix(cbind(vivas2, siembra - vivas2) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 3, nrep = 5)
#
# prior size post>0 ratio
# Comp.1 0.454   19     30 0.633
# Comp.2 0.125    5     22 0.227
# Comp.3 0.422   19     42 0.452
#
# 'log Lik.' -116.7673 (df=5)
# AIC: 243.5346   BIC: 252.3406
MejorModelo2@df #número de parámetros en el modelo
str(MejorModelo2)

plot(MejorModelo2)
BIC(MejorModelo2)

# parámetro binomial para cada componente
fitted(MejorModelo2)
p.MejorModelo2 <- fitted(MejorModelo2)[1,]
#otra forma de obtener el parámetro binomial para cada componente
MejorModelo2@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(MejorModelo2) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(MejorModelo2, component = 1)
exp(parameters(MejorModelo2)[1]) / (1 + exp(parameters(MejorModelo2)[1]))
exp(parameters(MejorModelo2)[2]) / (1 + exp(parameters(MejorModelo2)[2]))
exp(parameters(MejorModelo2)[3]) / (1 + exp(parameters(MejorModelo2)[3]))

MejorModelo2@prior #probabilidad previa
MejorModelo2@posterior #probabilidad posterior
MejorModelo2@cluster #asignación grupo


grupo.supervivencia.2 <-
  data.frame(sobrevivientes.all[, c(1, 3)],
             MejorModelo2@cluster)
colnames(grupo.supervivencia.2) <-
  c("Collector.Collection.Number",
    "Vivas.2",
    "Phenotypic.Group")

head(grupo.supervivencia.2)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#   grupo.supervivencia.2,
#   file = paste(
#     "grupo.supervivencia.2_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo

p.comp.1 <-
  exp(parameters(MejorModelo2)[1]) / (1 + exp(parameters(MejorModelo2)[1])) #parametro p grupo 1
pi.comp.1 <-
  sum(MejorModelo2@cluster == 1) / length(MejorModelo2@cluster) #parametro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(MejorModelo2)[2]) / (1 + exp(parameters(MejorModelo2)[2])) #parametro p grupo 2
pi.comp.2 <-
  sum(MejorModelo2@cluster == 2) / length(MejorModelo2@cluster) #parametro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2
p.comp.3 <-
  exp(parameters(MejorModelo2)[3]) / (1 + exp(parameters(MejorModelo2)[3])) #parametro p grupo 1
pi.comp.3 <-
  sum(MejorModelo2@cluster == 3) / length(MejorModelo2@cluster) #parametro pi grupo 3
pm.comp.3 <-
  pi.comp.3 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.3,
                     log = FALSE)#masa de probabilidad grupo 3
#representación gráfica del mejor modelo: gráficos de barras representando
#la asignación de cada planta madre a cada componente del mejor modelo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  vivas2[MejorModelo2@cluster == 3],
  breaks = seq(-0.5, 24.5, 1),
  xlab = "Número de plantas hijas",
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xaxt = "n",
  yaxt = "n",
  bty = "n"
)
title(expression("B) 19.6 meses"),
      adj = 0,
      cex.main = 1.5)
axis(
  side = 1,
  at = 0:24,
  labels = F,
  tcl = -0.5,
  cex.axis = 1.3
)
axis(
  side = 1,
  at = seq(0, 24, 4),
  labels = T,
  tcl = -0.7,
  cex.axis = 1.3
)
axis(
  side = 2,
  at = 0:10,
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
hist(
  vivas2[MejorModelo2@cluster == 1],
  breaks = seq(-0.5, 24.5, 1),
  add = T,
  density = 15,
  col = "black"
)
hist(vivas2[MejorModelo2@cluster == 2],
     breaks = seq(-0.5, 24.5, 1),
     add = T,
     col = "transparent")
#hist(vivas2[MejorModelo2@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=-45, col="black")
#hist(vivas2[MejorModelo2@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=45, col="black")
# #leyenda
# legend(16, 9, c("S1", "S2", "S3"), pch=c(24,4,19), lty=1, pt.cex=c(1.5, 1, 1), cex=1.5)
# legend(8,9, c("S1", "S2", "S3"), density=c(NA, NA, 15), fill=c("transparent", "gray90","black"), cex=1.5)
#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.1,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 19,
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  bty = "n"
)
points(
  0:24,
  pm.comp.3,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 4
)
points(
  0:24,
  pm.comp.2,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 24,
  cex = 1.5
)
axis(side = 4, cex.axis = 1.5)
mtext(side = 4,
      "Probabilidad",
      cex = 1.5,
      line = 2.8)

#################################################################################################################
# 7.3) Tercera medición: 51.4 meses después de la siembra.

vivas3 <- sobrevivientes.all$vivas.3
mode(vivas3)# 0
max(vivas3)
sum(vivas3)#175 plantas hijas
mean(vivas3)#4.069767
sd(vivas3) # 4.86188
Conc  <- FLXPmultinom(~ 1)
Mod.fam <- FLXglm(~ 1, family = "binomial")
siembra <-
  100 # number of trials of the binomial components (max number of credits)
Modelos3 <-
  stepFlexmix(
    cbind(vivas3, siembra - vivas3) ~ 1,
    model = Mod.fam,
    k = 1:6,
    concomitant = Conc,
    nrep = 5
  )
show(Modelos3)
plot(Modelos3)

# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 1:6, nrep = 5)
#
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -166.7607 335.5213 337.2825 337.2825
# 2   14      TRUE 2  2 -113.5369 233.0737 238.3573 241.8678
# 3   21      TRUE 3  3 -108.8010 227.6019 236.4079 246.5732
# 4   91      TRUE 3  4 -108.8032 227.6065 236.4125 246.1628
# 5   92      TRUE 3  5 -108.8032 227.6064 236.4124 246.1645
# 6  138      TRUE 3  6 -108.8032 227.6064 236.4124 246.1644

sort(BIC(Modelos3))# 3 Modelos con igual apoyo empírico: k=3,5,4,2
# 3        5        6        4        2        1
# 236.4079 236.4124 236.4124 236.4125 238.3573 337.2825

#then we keep the best model in terms of BIC
Modelo3.3  <- getModel(Modelos3, 3)
summary(Modelo3.3)
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 3, nrep = 5)
#
# prior size post>0 ratio
# Comp.1 0.145    7     24 0.292
# Comp.2 0.336   14     42 0.333
# Comp.3 0.520   22     35 0.629
#
# 'log Lik.' -108.801 (df=5)
# AIC: 227.6019   BIC: 236.4079
Modelo3.3@df #número de parámetros en el modelo: 5

plot(Modelo3.3)
BIC(Modelo3.3)


# parámetro binomial para cada componente
fitted(Modelo3.3)
p.Modelo3.3 <- fitted(Modelo3.3)[1,]
#otra forma de obtener el parámetro binomial para cada componente
Modelo3.3@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.3) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.3, component = 1)
exp(parameters(Modelo3.3)[1]) / (1 + exp(parameters(Modelo3.3)[1]))
exp(parameters(Modelo3.3)[2]) / (1 + exp(parameters(Modelo3.3)[2]))
exp(parameters(Modelo3.3)[3]) / (1 + exp(parameters(Modelo3.3)[3]))

Modelo3.3@prior #probabilidad previa
Modelo3.3@posterior #probabilidad posterior
Modelo3.3@cluster #asignación grupo

grupo.supervivencia.3.3 <-
  data.frame(sobrevivientes.all[, c(1, 4)],
             Modelo3.3@cluster)
colnames(grupo.supervivencia.3.3) <-
  c("Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group")

head(grupo.supervivencia.3.3)

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo
p.comp.1 <-
  exp(parameters(Modelo3.3)[1]) / (1 + exp(parameters(Modelo3.3)[1])) #parametro p grupo 1
pi.comp.1 <-
  sum(Modelo3.3@cluster == 1) / length(Modelo3.3@cluster) #parametro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(Modelo3.3)[2]) / (1 + exp(parameters(Modelo3.3)[2])) #parametro p grupo 2
pi.comp.2 <-
  sum(Modelo3.3@cluster == 2) / length(Modelo3.3@cluster) #parametro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2
p.comp.3 <-
  exp(parameters(Modelo3.3)[3]) / (1 + exp(parameters(Modelo3.3)[3])) #parametro p grupo 1
pi.comp.3 <-
  sum(Modelo3.3@cluster == 3) / length(Modelo3.3@cluster) #parametro pi grupo 3
pm.comp.3 <-
  pi.comp.3 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.3,
                     log = FALSE)#masa de probabilidad grupo 3

#representación gráfica del mejor modelo: gráficos de barras representando
#la asignación de cada planta madre a cada componente del mejor modelo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  vivas3[Modelo3.3@cluster == 3],
  breaks = seq(-0.5, 24.5, 1),
  xlab = "Número de plantas hijas",
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xaxt = "n",
  yaxt = "n",
  bty = "n"
)
title(expression("C) 51.4 meses"),
      adj = 0,
      cex.main = 1.5)
axis(
  side = 1,
  at = 0:24,
  labels = F,
  tcl = -0.5,
  cex.axis = 1.3
)
axis(
  side = 1,
  at = seq(0, 24, 4),
  labels = T,
  tcl = -0.7,
  cex.axis = 1.3
)
axis(
  side = 2,
  at = 0:10,
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
hist(
  vivas3[Modelo3.3@cluster == 2],
  breaks = seq(-0.5, 24.5, 1),
  add = T,
  density = 15,
  col = "black"
)
hist(vivas3[Modelo3.3@cluster == 1],
     breaks = seq(-0.5, 24.5, 1),
     add = T,
     col = "transparent")
#
# #leyenda
# legend(16, 9, c("S1", "S2", "S3"), pch=c(24,4,19), lty=1, pt.cex=c(1.5, 1, 1), cex=1.5)
# legend(8,9, c("S1", "S2", "S3"), density=c(NA, NA, 15), fill=c("transparent", "gray90","black"), cex=1.5)

#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.2,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 19,
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  bty = "n"
)
points(
  0:24,
  pm.comp.1,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 4
)
points(
  0:24,
  pm.comp.3,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 24,
  cex = 1.5
)
axis(side = 4, cex.axis = 1.5)
mtext(side = 4,
      "Probabilidad",
      cex = 1.5,
      line = 2.8)

#then we keep the best model in terms of BIC: K=5
Modelo3.5  <- getModel(Modelos3, 5)
summary(Modelo3.5)
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 5, nrep = 5)
#
# prior size post>0 ratio
# Comp.1 0.152    7     24 0.292
# Comp.2 0.336   14     42 0.333
# Comp.3 0.512   22     35 0.629
#
# 'log Lik.' -108.8032 (df=5)
# AIC: 227.6065   BIC: 236.4125
Modelo3.5@df #número de parámetros en el modelo
str(Modelo3.5)

plot(Modelo3.5)
BIC(Modelo3.5)

# parámetro binomial para cada componente
fitted(Modelo3.5)
p.Modelo3.5 <- fitted(Modelo3.5)[1,]
#otra forma de obtener el parámetro binomial para cada componente
Modelo3.5@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.5) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.5, component = 1)
exp(parameters(Modelo3.5)[1]) / (1 + exp(parameters(Modelo3.5)[1]))
exp(parameters(Modelo3.5)[2]) / (1 + exp(parameters(Modelo3.5)[2]))
exp(parameters(Modelo3.5)[3]) / (1 + exp(parameters(Modelo3.5)[3]))

Modelo3.5@prior #probabilidad previa
Modelo3.5@posterior #probabilidad posterior
Modelo3.5@cluster #asignación grupo

table(Modelo3.3@cluster, Modelo3.5@cluster)# modelos con igual asignación de grupos
#    1  2  3
# 1  0 14  0
# 2  0  0 22
# 3  7  0  0

grupo.supervivencia.3.5 <-
  data.frame(sobrevivientes.all[, c(1, 4)],
             Modelo3.5@cluster)
colnames(grupo.supervivencia.3.5) <-
  c("Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group")
head(grupo.supervivencia.3.5)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#   grupo.supervivencia.3.3,
#   file = paste(
#     "grupo.supervivencia.3.3_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )


#then we keep the best model in terms of BIC: K=4
Modelo3.4  <- getModel(Modelos3, 4)
summary(Modelo3.4)
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 4, nrep = 5)
#
# prior size post>0 ratio
# Comp.1 0.336   14     42 0.333
# Comp.2 0.512   22     35 0.629
# Comp.3 0.152    7     24 0.292
#
# 'log Lik.' -108.8032 (df=5)
# AIC: 227.6065   BIC: 236.4125

# AIC: 227.6065   BIC: 236.4125
Modelo3.4@df #número de parámetros en el modelo
str(Modelo3.4)

plot(Modelo3.4)
BIC(Modelo3.4)

# parámetro binomial para cada componente
fitted(Modelo3.4)
p.Modelo3.4 <- fitted(Modelo3.4)[1,]
#otra forma de obtener el parámetro binomial para cada componente
Modelo3.4@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.4) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.4, component = 1)
exp(parameters(Modelo3.4)[1]) / (1 + exp(parameters(Modelo3.4)[1]))
exp(parameters(Modelo3.4)[2]) / (1 + exp(parameters(Modelo3.4)[2]))
exp(parameters(Modelo3.4)[3]) / (1 + exp(parameters(Modelo3.4)[3]))

Modelo3.4@prior #probabilidad previa
Modelo3.4@posterior #probabilidad posterior
Modelo3.4@cluster #asignación grupo

grupo.supervivencia.3.4 <-
  data.frame(sobrevivientes.all[, c(1, 4)],
             Modelo3.4@cluster)
colnames(grupo.supervivencia.3.4) <-
  c("Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group")
head(grupo.supervivencia.3.4)

table(Modelo3.3@cluster, Modelo3.4@cluster)
#    1  2  3
# 1 14  0  0
# 2  0 22  0
# 3  0  0  7

#then we keep the best model in terms of BIC: K=2
Modelo3.2  <- getModel(Modelos3, 2)
summary(Modelo3.2)
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam,
#               concomitant = Conc, k = 2, nrep = 5)
#
# prior size post>0 ratio
# Comp.1 0.674   29     36 0.806
# Comp.2 0.326   14     33 0.424
#
# 'log Lik.' -113.5369 (df=3)
# AIC: 233.0737   BIC: 238.3573
#
# 'log Lik.' -113.5369 (df=3)
# AIC: 233.0737   BIC: 238.3573
Modelo3.2@df #número de parámetros en el modelo
str(Modelo3.2)

plot(Modelo3.2)
BIC(Modelo3.2)

# parámetro binomial para cada componente
fitted(Modelo3.2)
p.Modelo3.2 <- fitted(Modelo3.2)[1,]
#otra forma de obtener el parámetro binomial para cada componente
Modelo3.2@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.2) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.2, component = 1)
exp(parameters(Modelo3.2)[1]) / (1 + exp(parameters(Modelo3.2)[1]))
exp(parameters(Modelo3.2)[2]) / (1 + exp(parameters(Modelo3.2)[2]))
exp(parameters(Modelo3.2)[3]) / (1 + exp(parameters(Modelo3.2)[3]))

Modelo3.2@prior #probabilidad previa
Modelo3.2@posterior #probabilidad posterior
Modelo3.2@cluster #asignación grupo

table(Modelo3.2@cluster, Modelo3.3@cluster)
#    1  2  3
# 1  7 22  0
# 2  7  0  7

grupo.supervivencia.3.2 <-
  data.frame(sobrevivientes.all[, c(1, 4)],
             Modelo3.2@cluster)
colnames(grupo.supervivencia.3.2) <-
  c("Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group")
head(grupo.supervivencia.3.2)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#   grupo.supervivencia.3.2,
#   file = paste(
#     "grupo.supervivencia.3.2_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo
p.comp.1 <-
  exp(parameters(Modelo3.2)[1]) / (1 + exp(parameters(Modelo3.2)[1])) #parametro p grupo 1
pi.comp.1 <-
  sum(Modelo3.2@cluster == 1) / length(Modelo3.2@cluster) #parametro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(Modelo3.2)[2]) / (1 + exp(parameters(Modelo3.2)[2])) #parametro p grupo 2
pi.comp.2 <-
  sum(Modelo3.2@cluster == 2) / length(Modelo3.2@cluster) #parametro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2

par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  vivas3[Modelo3.2@cluster == 2],
  breaks = seq(-0.5, 24.5, 1),
  xlab = "Número de plantas hijas",
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xaxt = "n",
  yaxt = "n",
  bty = "n"
)
title(expression("51.4 meses"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = 0:24,
  labels = F,
  tcl = -0.5,
  cex.axis = 1.3
)
axis(
  side = 1,
  at = seq(0, 24, 4),
  labels = T,
  tcl = -0.7,
  cex.axis = 1.3
)
axis(
  side = 2,
  at = 0:10,
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
hist(
  vivas3[Modelo3.2@cluster == 1],
  breaks = seq(-0.5, 24.5, 1),
  add = T,
  col = "transparent"
)
# #leyenda      
legend(
  16,
  9,
  c("S1", "S2"),
  pch = c(24, 4),
  lty = 1,
  pt.cex = c(1.5, 1),
  cex = 1.5
)
legend(
  8,
  9,
  c("S1", "S2"),
  fill = c("transparent", "gray90"),
  cex = 1.5
)
#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.2,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 4,
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  bty = "n"
)
points(
  0:24,
  pm.comp.1,
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 24,
  cex = 1.5
)
axis(side = 4, cex.axis = 1.5)
mtext(side = 4,
      "Probabilidad",
      cex = 1.5,
      line = 2.8)

#################################################################################################################
#################################################################################################################
#8) Tablas de clasificación cruzadas entre modelos de supervivencia y entre morfología y estadísticos
#Goodman-Kruskal tau para la concordancia entre grupos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 8.1) 11.4 meses después de la siembra vs. 19.6 meses después de la siembra
grupo.supervivencia.1.vs.2 <-
  merge(
    grupo.supervivencia.1[, c(1, 3)],
    grupo.supervivencia.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    all = T,
    suffixes = c(".11.4", ".19.6")
  )
table(grupo.supervivencia.1.vs.2[, 3],
      grupo.supervivencia.1.vs.2[, 2])
#    1  2  3
# 1  0  0 19
# 2  5  0  0
# 3  0 18  1

# 8.1.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de supervivencia a 11.4 y
#19.6: tau(11.4, 19.6) y tau(19.6, 11.4)

colnames(grupo.supervivencia.1.vs.2)
GKtau(
  grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4,
  grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6
)
# xName
# 1 grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4
# yName Nx Ny tauxy tauyx
# 1 grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6  3  3 0.926 0.926

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6)
  GKtau.nulo <-
    GKtau(grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4,
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
    grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4,
    grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4 ,
    grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0
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
    grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4,
    grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6,11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.1.vs.2$Phenotypic.Group.11.4,
    grupo.supervivencia.1.vs.2$Phenotypic.Group.19.6
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0

#################################################################################################################
# 8.2) 19.6 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 1)
grupo.supervivencia.2.vs.3.3 <-
  merge(
    grupo.supervivencia.2[, c(1, 3)],
    grupo.supervivencia.3.3[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".51.4")
  )
table(grupo.supervivencia.2.vs.3.3[, 3],
      grupo.supervivencia.2.vs.3.3[, 2])
#    1  2  3
# 1  0  0 14
# 2 19  0  3
# 3  0  5  2

# 8.2.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de supervivencia a 19.6 y
#51.4: tau(19.6, 51.4) y tau(51.4, 19.6)

colnames(grupo.supervivencia.2.vs.3.3)
GKtau(
  grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6,
  grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4
)
# xName
# 1 grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6
# yName Nx Ny tauxy tauyx
# 1 grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4  3  3 0.693 0.686

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4)
  GKtau.nulo <-
    GKtau(grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6,
          morfo.aleatorio)
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
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6,
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6 ,
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0
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
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6,
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4,19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.19.6,
    grupo.supervivencia.2.vs.3.3$Phenotypic.Group.51.4
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0


# 19.6 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 2)
grupo.supervivencia.2.vs.3.2 <-
  merge(
    grupo.supervivencia.2[, c(1, 3)],
    grupo.supervivencia.3.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".51.4")
  )
table(grupo.supervivencia.2.vs.3.2[, 2],
      grupo.supervivencia.2.vs.3.2[, 3])
#    1  2
# 1  5  0
# 2  0 19
# 3  9 10

#################################################################################################################
# 8.3) 11.4 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 1)

grupo.supervivencia.1.vs.3.3 <-
  merge(
    grupo.supervivencia.1[, c(1, 3)],
    grupo.supervivencia.3.3[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".51.4")
  )
table(grupo.supervivencia.1.vs.3.3[, 2],
      grupo.supervivencia.1.vs.3.3[, 3])
# 1  2  3
# 1  0  0  5
# 2 14  2  2
# 3  0 20  0

# 8.3.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de supervivencia a 51.4 y
#11.4: tau(51.4, 11.4) y tau(11.4, 51.4)

colnames(grupo.supervivencia.1.vs.3.3)
GKtau(
  grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4,
  grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4
)
# xName
# 1 grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4
# yName Nx Ny tauxy tauyx
# 1 grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4  3  3 0.746 0.744

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4)
  GKtau.nulo <-
    GKtau(grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4,
          morfo.aleatorio)
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
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4,
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4 ,
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0
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
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4,
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4,51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.51.4,
    grupo.supervivencia.1.vs.3.3$Phenotypic.Group.11.4
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0

# 11.4 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 2)

grupo.supervivencia.1.vs.3.2 <-
  merge(
    grupo.supervivencia.1[, c(1, 3)],
    grupo.supervivencia.3.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".51.4")
  )
table(grupo.supervivencia.1.vs.3.2[, 2],
      grupo.supervivencia.1.vs.3.2[, 3])
# 1  2
# 1  9  9
# 2  5  0
# 3  0 20


#################################################################################################################
# 8.4) 11.4 meses después de la siembra vs. morfología

grupo.supervivencia.1.vs.morfologia <-
  merge(
    grupo.supervivencia.1[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".madre")
  )

table(grupo.supervivencia.1.vs.morfologia[, 2],
      grupo.supervivencia.1.vs.morfologia[, 3])
#   2  3  4  5
# 1  0  1  2  2
# 2  4 10  0  4
# 3  2  7  3  8

# 8.4.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de supervivencia a 11.4 y
#Group.madre: tau(11.4, Group.madre) y tau(Group.madre, 11.4)

colnames(grupo.supervivencia.1.vs.morfologia)
GKtau(
  grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4,
  grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre
)
#                                                     xName
# 1 grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4
# yName Nx Ny tauxy tauyx
# 1 grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre  3  4 0.065 0.111

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre)
  GKtau.nulo <-
    GKtau(grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4,
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(11.4, Group.madre),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(11.4, Group.madre)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)

#mostrar el valor observado
abline(
  v = GKtau(
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4,
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, Group.madre) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4 ,
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.22722
#grafica de la distribución nula de tau(Group.madre, 11.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(Group.madre, 11.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4,
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(Group.madre,11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.11.4,
    grupo.supervivencia.1.vs.morfologia$Phenotypic.Group.madre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.16693


#################################################################################################################
# 8.5) 19.6 meses después de la siembra vs. morfología

grupo.supervivencia.2.vs.morfologia <-
  merge(
    grupo.supervivencia.2[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".madre")
  )

table(grupo.supervivencia.2.vs.morfologia[, 2],
      grupo.supervivencia.2.vs.morfologia[, 3])
# 2  3  4  5
# 1  2  7  2  8
# 2  0  1  2  2
# 3  4 10  1  4

# 8.5.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de supervivencia a 19.6 y
#Group.madre: tau(19.6, Group.madre) y tau(Group.madre, 19.6)

colnames(grupo.supervivencia.2.vs.morfologia)
GKtau(
  grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6,
  grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre
)
#                                                      xName
# 1 grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6
# yName Nx Ny tauxy tauyx
# 1 grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre  3  4 0.056 0.082

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre)
  GKtau.nulo <-
    GKtau(grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6,
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(19.6, Group.madre),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(19.6, Group.madre)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)

#mostrar el valor observado
abline(
  v = GKtau(
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6,
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, Group.madre) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6 ,
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.32112
#grafica de la distribución nula de tau(Group.madre, 19.6),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(Group.madre, 19.6)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6,
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(Group.madre,19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.19.6,
    grupo.supervivencia.2.vs.morfologia$Phenotypic.Group.madre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.33412

#################################################################################################################
# 8.6) 51.4 meses después de la siembra (modelo 1) vs. morfología

grupo.supervivencia.3.3.vs.morfologia <-
  merge(
    grupo.supervivencia.3.3[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".51.4", ".madre")
  )

table(grupo.supervivencia.3.3.vs.morfologia[, 2],
      grupo.supervivencia.3.3.vs.morfologia[, 3])
# 2 3 4 5
# 1 3 7 0 4
# 2 2 9 3 8
# 3 1 2 2 2

# 8.6.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de supervivencia a 51.4 y
#Group.madre: tau(51.4, Group.madre) y tau(Group.madre, 51.4)

colnames(grupo.supervivencia.3.3.vs.morfologia)
GKtau(
  grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4,
  grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre
)
# xName
# 1 grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4
# yName Nx Ny tauxy tauyx
# 1 grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre  3  4 0.028 0.055

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre)
  GKtau.nulo <-
    GKtau(grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4,
          morfo.aleatorio)
  GKtau.nulo.mat[i, ] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
}

#grafica de la distribución nula de tau(51.4, Group.madre),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(51.4, Group.madre)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)

#mostrar el valor observado
abline(
  v = GKtau(
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4,
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, Group.madre) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4 ,
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.73781
#grafica de la distribución nula de tau(Group.madre, 51.4),
par(mar = c(5, 5, 2, 2) + 0.1)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
hist(
  GKtau.nulo.mat[, 1],
  xlim = c(0, 1),
  col = "gray90",
  main = "",
  xlab = expression(tau(Group.madre, 51.4)),
  ylab = "Iteraciones del modelo nulo",
  cex.main = 1,
  cex.lab = 1.5,
  cex.axis = 1.5
)
#mostrar el valor observado
abline(
  v = GKtau(
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4,
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(Group.madre,51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.51.4,
    grupo.supervivencia.3.3.vs.morfologia$Phenotypic.Group.madre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.6033
#################################################################################################################
# 8.7) 51.4 meses después de la siembra (modelo 2) vs. morfología

grupo.supervivencia.3.2.vs.morfologia <-
  merge(
    grupo.supervivencia.3.2[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".madre")
  )

table(grupo.supervivencia.3.2.vs.morfologia[, 2],
      grupo.supervivencia.3.2.vs.morfologia[, 3])

#    2  3  4  5
# 1  3  4  2  5
# 2  3 14  3  9

#################################################################################################################
#################################################################################################################
#################################################################################################################
#
##### MODELO DE MEZCLAS BINOMIALES PARA RECLUTAMIENTO DE LA PROGENIE DE LAS PLANTAS MADRE#######
#
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
# INTRODUCCIÓN:
# Con los datos de las plantas que sobrevivieron en los tres muestreos se realizó modelos de mezclas
#binomiales que agrupan a las plantas madre según el reclutamiento de la progenie
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
# 3) Crear marco de datos del reclutamiento
# 4) Guardar marco de datos
# 5) Crear tabla con número de sobrevivientes por planta madre
# 6) Examinar el reclutamiento
# 7) Ajustar modelos de mezclas binomiales
# 8) Tablas de clasificación cruzadas entre modelos de reclutamiento y entre morfología y estadísticos
#Goodman-Kruskal tau para la concordancia entre grupos
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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/datos")

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
# nombre.progenie; seleccionar las variables para analizar reclutamiento
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
# 2.1.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de medida en base a si
# tiene información de número de hojas y longitud del tallo
reclutamiento.1 <- datos.fenotipicos.marzo.selected %>%
  mutate(reclutamiento.1 = if_else(
    is.na(Longitud.tallo..cm.) & is.na(Número.de.hojas),
    "M",
    "V",
    "M"
  ))
view(reclutamiento.1)

reclutamiento.1 <- reclutamiento.1 %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    reclutamiento.1
  )

#################################################################################################################
# 2.2) segunda medida (19.6 meses DS): octubre de 2020
summary(datos.fenotipicos.octubre)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables


# 2.2.1) subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables para analizar reclutamiento
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
reclutamiento.2 <- datos.fenotipicos.octubre.selected %>%
  mutate(reclutamiento.2 = if_else(is.na(Fecha.medición), "M", "V", "M"))
view(reclutamiento.2)

reclutamiento.2 <- reclutamiento.2 %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    reclutamiento.2
  )
View(reclutamiento.2)
#################################################################################################################
# 2.3) tercera medida (51.4 meses DS): junio de 2023
summary(datos.fenotipicos.junio)
head(datos.fenotipicos.junio)
dim(datos.fenotipicos.junio)# 180 progenie y 29 variables

# 2.3.1)subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables para analizar reclutamiento
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
reclutamiento.3 <- datos.fenotipicos.junio.selected %>%
  mutate(reclutamiento.3 = if_else(is.na(Fecha.medición), "M", "V", "M"))
view(reclutamiento.3)

reclutamiento.3 <- reclutamiento.3 %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Fecha.siembra,
    Fecha.trasplante,
    Fecha.medición,
    reclutamiento.3
  )
View(reclutamiento.3)
#################################################################################################################
#################################################################################################################
# 3) Crear marco de datos del reclutamiento
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 3.1) combinar la tabla reclutamiento 1 y reclutamiento 2


reclutamiento <-
  left_join(
    reclutamiento.1,
    reclutamiento.2,
    by = join_by(
      Número.colección.planta.madre,
      nombre.progenie,
      Fecha.siembra,
      Fecha.trasplante
    )
  )
#################################################################################################################
# 3.2)combinar reclutamiento con reclutamiento 3

reclutamiento <-
  left_join(
    reclutamiento,
    reclutamiento.3,
    by = join_by(
      Número.colección.planta.madre,
      nombre.progenie,
      Fecha.siembra,
      Fecha.trasplante
    )
  )
view(reclutamiento)
#################################################################################################################
# 3.3) Cambiar los NAS del muestreo 3 por "M"
reclutamiento <-
  reclutamiento %>%  replace_na(list(reclutamiento.3 = "M"))
#################################################################################################################
# 3.4 )renombrar las variable

reclutamiento <- reclutamiento %>%
  rename(
    Fecha.medición.1 = Fecha.medición.x,
    Fecha.medición.2 = Fecha.medición.y,
    Fecha.medición.3 = Fecha.medición
  )
view(reclutamiento)
#################################################################################################################
#################################################################################################################
# 4) Guardar marco de datos
#################################################################################################################
#################################################################################################################

# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/reclutamiento/datos")
# 
# #RData
# save(reclutamiento,
#      file = paste(
#        "reclutamiento_piloto_",
#        format(Sys.time(), "%Y%B%d_%H%M%S"),
#        ".RData",
#        sep = ""
#      ))
# 
# #.csv
# write.csv(
#   reclutamiento,
#   file = paste(
#     "reclutamiento_piloto_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

# load ("reclutamiento_piloto_2023diciembre26_134304.RData")

################################################################################################################
#################################################################################################################
# 5) Crear tabla con número de sobrevivientes por planta madre.
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 5.1)Remover plantas madres sin número de colección.

reclutamiento$Número.colección.planta.madre <-
  as.numeric(reclutamiento$Número.colección.planta.madre)
reclutamiento <-
  reclutamiento[!is.na(reclutamiento$Número.colección.planta.madre),]
dim(reclutamiento) # 248 plantas de la progenie con madre asignada

#################################################################################################################
# 5.2) Crear tabla que suma progenie viva por plantas madre

vivas.reclutamiento.1 <-
  as.data.frame(table(reclutamiento[, c(1, 6)]))
vivas.reclutamiento.1 <-
  vivas.reclutamiento.1[vivas.reclutamiento.1$reclutamiento.1 == "V", c(1, 3)]
vivas.reclutamiento.2 <-
  as.data.frame(table(reclutamiento[, c(1, 8)]))
vivas.reclutamiento.2 <-
  vivas.reclutamiento.2[vivas.reclutamiento.2$reclutamiento.2 == "V", c(1, 3)]
vivas.reclutamiento.3 <-
  as.data.frame(table(reclutamiento[, c(1, 10)]))
vivas.reclutamiento.3 <-
  vivas.reclutamiento.3[vivas.reclutamiento.3$reclutamiento.3 == "V", c(1, 3)]

sobrevivientes <- data.frame(vivas.reclutamiento.1,
                             vivas.reclutamiento.2[, 2],
                             vivas.reclutamiento.3[, 2])
View(sobrevivientes)
colnames(sobrevivientes) <-
  c("Collector.Collection.Number",
    "vivas.1",
    "vivas.2",
    "vivas.3")

#Agregar las plantas madre que no nacieron ninguna hija mediante la tabla de asignación de grupos de las plantas madre
setwd("C:/Users/usuario/Documents/Jardin_comun/Especimenes/datos")#directorio de los datos de las plantas madres
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/datos/MedicionesPlantasMadre") #Ivan's working directory Waterman
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )

#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350,]
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
# 6) Examinar el reclutamiento
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 6.1) crear marco de datos para analizar reclutamiento
sobrevivientes.tiempo <-
  sobrevivientes.all
colnames(sobrevivientes.tiempo) <-
  c("Collector.Collection.Number", "11.4", "19.6", "51.4")
sobrevivientes.tiempo <-
  sobrevivientes.tiempo %>%
  gather(time, sobrevivientes, "11.4":"51.4")

# gráfica por tiempo de medida
promedio.reclutamiento <-
  sobrevivientes.tiempo %>%
  group_by(time) %>%
  summarise(
    promedio = mean(sobrevivientes),
    mediana = median(sobrevivientes),
    Q5 = quantile(sobrevivientes, 0.05),
    Q95 = quantile(sobrevivientes, 0.95),
  ) %>%
  ungroup()
promedio.reclutamiento  #medidas de tendencia central del reclutamiento de las plantas madre por cada muestreo

# # A tibble: 3 × 5
# time  promedio mediana    Q5   Q95
# <chr>    <dbl>   <dbl> <dbl> <dbl>
#   1 11.4      5.51       4     0  15.9
# 2 19.6      5.14       4     0  14  
# 3 51.4      4.19       2     0  11

sobrevivientes.tiempo %>%  #Cantidad de reclutas por cada tiempo de muestreo
  group_by(time) %>%
  summarise(suma = sum(sobrevivientes))
# time   suma
# <chr> <dbl>
#   1 11.4    237
# 2 19.6    221
# 3 51.4    180
promedio.reclutamiento$time <-
  as.numeric(promedio.reclutamiento$time)

#Gráfica de las medidas de tendencia central del reclutamiento promedio por muestreo # Fig. S7A

#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
plot(
  promedio.reclutamiento[, 1:2],
  axes = F,
  type = "o",
  pch = 19,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 16),
  xlim = c(11.4, 60),
  xaxt = "n",
  bty = "n",
  xlab = "Meses después de la siembra",
  ylab = "Plantas vivas"
)
title(expression("A) "), adj = 0, cex.main = 1.5)
# ejes por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels =F,
  col = "gray60"
)

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
  promedio.reclutamiento$time,
  promedio.reclutamiento$mediana,
  type = "o",
  pch = 24
)
lines(
  promedio.reclutamiento$time,
  promedio.reclutamiento$Q5,
  lty = 3,
  col = "gray60"
)

lines(
  promedio.reclutamiento$time,
  promedio.reclutamiento$Q95,
  lty = 3,
  col = "gray60"
)
text(57.5, 4.07, "Promedio")
text(57.5, 2, "Mediana")
text(57, 0, "Q5%")
text(57, 11, "Q95%")


# Gráfica del reclutamiento por planta madre: Fig. S7B
promedio.reclutamiento.madre <-
  sobrevivientes.tiempo %>%
  group_by(Collector.Collection.Number, time) %>%
  summarise(promedio = mean(sobrevivientes)) %>%
  spread(Collector.Collection.Number, promedio) %>%
  ungroup()


promedio.reclutamiento.madre$time <-
  as.numeric(promedio.reclutamiento.madre$time)
#gráfica promedio en el tiempo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  promedio.reclutamiento.madre$time,
  promedio.reclutamiento.madre[, -1],
  axes = F,
  type = "l",
  lty = 1,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 25),
  xlim = c(11.4, 60),
  xaxt = "n",
  bty = "n",
  col = "gray20",
  xlab = "Meses después de la siembra",
  ylab = "Plantas vivas"
)
title(expression("B)"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels =F,
  col = "gray60"
)
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
# 7.1) primer muestreo: 11.4 meses después de la siembra.

vivas1 <- sobrevivientes.all$vivas.1
mode <- function(x) {
  return(as.numeric(names(which.max(table(
    x
  )))))
}
mode(vivas1) #moda= 1
max(vivas1) # reclutamiento máximo por planta madre = 24
mean(vivas1)#promedio de reclutamiento = 5.511628
sd(vivas1) # desviación = 5.787392
sum(vivas1) # 237 reclutas a 11.4 meses
Conc  <- FLXPmultinom( ~ 1)
Mod.fam <- FLXglm( ~ 1, family = "binomial")
siembra <-
  100 # número de ensayos de los componentes binomiales (máximo número de créditos)
Modelos1 <-
  stepFlexmix(
    cbind(vivas1, siembra - vivas1) ~ 1,
    model = Mod.fam,
    k = 1:6,
    concomitant = Conc,
    nrep = 5
  )
show(Modelos1)# modelos
# Call:
#   stepFlexmix(cbind(vivas1, siembra - vivas1) ~ 1, model = Mod.fam, concomitant = Conc, 
#               k = 1:6, nrep = 5)
# 
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -189.3003 380.6006 382.3618 382.3618
# 2   18      TRUE 2  2 -128.5333 263.0665 268.3501 272.4879
# 3   33      TRUE 3  3 -118.6317 247.2635 256.0695 261.4749
# 4   63      TRUE 4  4 -117.7583 249.5165 261.8449 277.2500
# 5   30      TRUE 5  5 -117.7580 253.5160 269.3668 297.8632
# 6   55      TRUE 6  6 -117.7583 257.5165 276.8897 325.6568

sort(BIC(Modelos1)) # organizar según BIC delta = 5.7
# 3        4        2        5        6        1 
# 256.0695 261.8449 268.3501 269.3668 276.8897 382.3618

plot(Modelos1)
getModel(Modelos1, which = "BIC")

# Call:
#   stepFlexmix(cbind(vivas1, siembra - vivas1) ~ 1, model = Mod.fam, concomitant = Conc, 
#               k = 3, nrep = 5)
# 
# Cluster sizes:
#   1  2  3 
# 19  4 20 
# 
# convergence after 33 iterations


#then we keep the best model in terms of BIC
MejorModelo1  <- getModel(Modelos1, "BIC")
summary(MejorModelo1)
MejorModelo1@df #número de parámetros en el modelo = 5
str(MejorModelo1)

plot(MejorModelo1)
BIC(MejorModelo1) # BIC = 256.0695

# parámetro binomial para cada componente
fitted(MejorModelo1)
p.MejorModelo1 <- fitted(MejorModelo1)[1, ]
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


grupo.reclutamiento.1 <-
  data.frame(sobrevivientes.all[, c(1, 2)],
             MejorModelo1@cluster)
colnames(grupo.reclutamiento.1) <-
  c("Collector.Collection.Number",
    "Vivas.1",
    "Phenotypic.Group")

head(grupo.reclutamiento.1)

# guardar tabla de asignación de grupos
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/reclutamiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/datos/reclutamiento") #Ivan's working directory Waterman
# write.csv(
#     grupo.reclutamiento.1,
#     file = paste(
#       "grupo.reclutamiento.1_",
#       format(Sys.time(), "%Y%B%d_%H%M%S"),
#       ".csv",
#       sep = ""
#          ),
#       row.names = F
#     )

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo
p.comp.1 <-
  exp(parameters(MejorModelo1)[1]) / (1 + exp(parameters(MejorModelo1)[1])) #parámetro p grupo 1
pi.comp.1 <-
  sum(MejorModelo1@cluster == 1) / length(MejorModelo1@cluster) #parámetro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(MejorModelo1)[2]) / (1 + exp(parameters(MejorModelo1)[2])) #parámetro p grupo 2
pi.comp.2 <-
  sum(MejorModelo1@cluster == 2) / length(MejorModelo1@cluster) #parámetro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2
p.comp.3 <-
  exp(parameters(MejorModelo1)[3]) / (1 + exp(parameters(MejorModelo1)[3])) #parámetro p grupo 1
pi.comp.3 <-
  sum(MejorModelo1@cluster == 3) / length(MejorModelo1@cluster) #parámetro pi grupo 3
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
  vivas1[MejorModelo1@cluster == 1],#R2
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
  vivas1[MejorModelo1@cluster == 3],#R3
  breaks = seq(-0.5, 24.5, 1),
  add = T,
  density = 15,
  col = "black"
)
hist(vivas1[MejorModelo1@cluster == 2],# R1
     breaks = seq(-0.5, 24.5, 1),
     add = T,
     col = "transparent")
#hist(vivas1[MejorModelo1@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=-45, col="black")
#hist(vivas1[MejorModelo1@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=45, col="black")
#leyenda
legend(
  16,
  10,
  c("R1", "R2", "R3"),
  pch = c(24, 4, 19),
  lty = 1,
  pt.cex = c(1.5, 1, 1),
  cex = 1.5
)
legend(
  7,
  10,
  c("R1", "R2", "R3"),
  density = c(NA, NA, 15),
  fill = c("transparent", "gray90", "black"),
  cex = 1.5
)
#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.3,# R3
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
  pm.comp.1,#R2
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 4
)
points(
  0:24,
  pm.comp.2,#R1
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
# 7.2) segundo muestreo: 19.6 meses después de la siembra.
vivas2 <- sobrevivientes.all$vivas.2
mode(vivas2)#moda = 0
max(vivas2) #reclutamiento máximo = 23
sum(vivas2)# 221 reclutas
mean(vivas2)# reclutamiento promedio = 5.139535
sd(vivas2) # desviación = 5.466702
Conc  <- FLXPmultinom( ~ 1)
Mod.fam <- FLXglm( ~ 1, family = "binomial")
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
show(Modelos2) #modelos
# Call:
#   stepFlexmix(cbind(vivas2, siembra - vivas2) ~ 1, model = Mod.fam, concomitant = Conc, 
#               k = 1:6, nrep = 5)
# 
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -183.0510 368.1020 369.8632 369.8632
# 2    8      TRUE 2  2 -124.4231 254.8462 260.1298 266.1241
# 3   49      TRUE 3  3 -116.6566 243.3132 252.1192 259.0522
# 4   43      TRUE 4  4 -115.2756 244.5511 256.8795 270.5391
# 5   42      TRUE 5  5 -115.2756 248.5511 264.4019 299.1808
# 6   46      TRUE 6  6 -115.2757 252.5514 271.9246 300.7972

sort(BIC(Modelos2)) # delta = 4.7603
# 3        4        2        5        6        1 
# 252.1192 256.8795 260.1298 264.4019 271.9246 369.8632

plot(Modelos2)
getModel(Modelos2, which = "BIC")

# Call:
#   stepFlexmix(cbind(vivas2, siembra - vivas2) ~ 1, model = Mod.fam, concomitant = Conc, 
#               k = 3, nrep = 5)
# 
# Cluster sizes:
#   1  2  3 
# 21 18  4 
# 
# convergence after 49 iterations

#luego, nosotros mantenemos el mejor BIC
MejorModelo2  <- getModel(Modelos2, "BIC")
summary(MejorModelo2)
# #Call:
# stepFlexmix(cbind(vivas2, siembra - vivas2) ~ 1, model = Mod.fam, concomitant = Conc, 
#             k = 3, nrep = 5)
# 
# prior size post>0 ratio
# Comp.1 0.4677   21     30 0.700
# Comp.2 0.4344   18     42 0.429
# Comp.3 0.0979    4     22 0.182

# 'log Lik.' -116.6566 (df=5)
# AIC: 243.3132   BIC: 252.1192 
#
# 'log Lik.' -116.7673 (df=5)
# AIC: 243.5346   BIC: 252.3406

MejorModelo2@df #número de parámetros en el modelo = 5 
str(MejorModelo2)

plot(MejorModelo2)
BIC(MejorModelo2) #252.119

# parámetro binomial para cada componente
fitted(MejorModelo2)
p.MejorModelo2 <- fitted(MejorModelo2)[1, ]
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


grupo.reclutamiento.2 <-
  data.frame(sobrevivientes.all[, c(1, 3)],
             MejorModelo2@cluster)
colnames(grupo.reclutamiento.2) <-
  c("Collector.Collection.Number",
    "Vivas.2",
    "Phenotypic.Group")

head(grupo.reclutamiento.2)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/reclutamiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/datos/reclutamiento") #Ivan's working directory Waterman
# write.csv(
#   grupo.reclutamiento.2,
#   file = paste(
#     "grupo.reclutamiento.2_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo

p.comp.1 <-
  exp(parameters(MejorModelo2)[1]) / (1 + exp(parameters(MejorModelo2)[1])) #parámetro p grupo 1
pi.comp.1 <-
  sum(MejorModelo2@cluster == 1) / length(MejorModelo2@cluster) #parámetro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(MejorModelo2)[2]) / (1 + exp(parameters(MejorModelo2)[2])) #parámetro p grupo 2
pi.comp.2 <-
  sum(MejorModelo2@cluster == 2) / length(MejorModelo2@cluster) #parámetro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2
p.comp.3 <-
  exp(parameters(MejorModelo2)[3]) / (1 + exp(parameters(MejorModelo2)[3])) #parámetro p grupo 1
pi.comp.3 <-
  sum(MejorModelo2@cluster == 3) / length(MejorModelo2@cluster) #parámetro pi grupo 3
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
  vivas2[MejorModelo2@cluster == 2],#R2
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
  vivas2[MejorModelo2@cluster == 1], #R3
  breaks = seq(-0.5, 24.5, 1),
  add = T,
  density = 15,
  col = "black"
)
hist(vivas2[MejorModelo2@cluster == 3], #R1
     breaks = seq(-0.5, 24.5, 1),
     add = T,
     col = "transparent")
#hist(vivas2[MejorModelo2@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=-45, col="black")
#hist(vivas2[MejorModelo2@cluster==3], breaks=seq(-0.5, 24.5, 1), add=T, density=20, angle=45, col="black")
# #leyenda
# legend(16, 9, c("R1", "R2", "R3"), pch=c(24,4,19), lty=1, pt.cex=c(1.5, 1, 1), cex=1.5)
# legend(8,9, c("R1", "R2", "R3"), density=c(NA, NA, 15), fill=c("transparent", "gray90","black"), cex=1.5)
#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.1, #R3
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
  pm.comp.2, #R2
  type = "o",
  xlim = c(-0.5, 24.5),
  pch = 4
)
points(
  0:24,
  pm.comp.3, #R1
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
# 7.3) Tercer muestreo: 51.4 meses después de la siembra.

vivas3 <- sobrevivientes.all$vivas.3
mode(vivas3)#moda =  0
max(vivas3) #reclutamiento máximo = 21
sum(vivas3)# 180 reclutas
mean(vivas3)#promedio = 4.186047
sd(vivas3) # desviación = 4.807018
Conc  <- FLXPmultinom( ~ 1)
Mod.fam <- FLXglm( ~ 1, family = "binomial")
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

# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -166.5155 335.0309 336.7921 336.7921
# 2    5      TRUE 2  2 -112.1443 230.2887 235.5723 239.5691
# 3   53      TRUE 2  3 -112.1444 230.2887 235.5723 239.5884
# 4   99      TRUE 2  4 -112.1444 230.2887 235.5723 239.5885
# 5   55      TRUE 3  5 -112.1444 234.2887 243.0947 283.9240
# 6  129      TRUE 2  6 -112.1444 230.2887 235.5723 239.5886

sort(BIC(Modelos3))# delta = 7.522397

#mantenemos el modelo con mejor BIC
MejorModelo3  <- getModel(Modelos3, "BIC")

summary(MejorModelo3)
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam, concomitant = Conc, 
#               k = 2, nrep = 5)
# 
# prior size post>0 ratio
# Comp.1 0.622   28     35 0.800
# Comp.2 0.378   15     43 0.349
# 
# 'log Lik.' -112.1443 (df=3)
# AIC: 230.2887   BIC: 235.5723
MejorModelo3@df #número de parámetros en el modelo: 3

plot(MejorModelo3)
BIC(MejorModelo3) # 235.5723
getModel(Modelos3, which = "BIC")
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam, concomitant = Conc, 
#               k = 2, nrep = 5)
# 
# Cluster sizes:
#   1  2 
# 28 15 
# 
# convergence after 5 iterations

# parámetro binomial para cada componente
fitted(MejorModelo3)
p.MejorModelo3 <- fitted(MejorModelo3)[1, ]
#otra forma de obtener el parámetro binomial para cada componente
MejorModelo3@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(MejorModelo3) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(MejorModelo3, component = 1)
exp(parameters(MejorModelo3)[1]) / (1 + exp(parameters(MejorModelo3)[1]))
exp(parameters(MejorModelo3)[2]) / (1 + exp(parameters(MejorModelo3)[2]))
exp(parameters(MejorModelo3)[3]) / (1 + exp(parameters(MejorModelo3)[3]))

MejorModelo3@prior #probabilidad previa
MejorModelo3@posterior #probabilidad posterior
MejorModelo3@cluster #asignación grupo

grupo.reclutamiento.3 <-
  data.frame(sobrevivientes.all[, c(1, 4)],
             MejorModelo3@cluster)
colnames(grupo.reclutamiento.3) <-
  c("Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group")

head(grupo.reclutamiento.3)

#para representar el mejor modelo gráficamente, aquí se calcula la masa de probabilidad
#de la distribución binomial correspondiente a cada componente del mejor modelo
p.comp.1 <-
  exp(parameters(MejorModelo3)[1]) / (1 + exp(parameters(MejorModelo3)[1])) #parámetro p grupo 1
pi.comp.1 <-
  sum(MejorModelo3@cluster == 1) / length(MejorModelo3@cluster) #parámetro pi grupo 1
pm.comp.1 <-
  pi.comp.1 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.1,
                     log = FALSE) #masa de probabilidad grupo 1
p.comp.2 <-
  exp(parameters(MejorModelo3)[2]) / (1 + exp(parameters(MejorModelo3)[2])) #parámetro p grupo 2
pi.comp.2 <-
  sum(MejorModelo3@cluster == 2) / length(MejorModelo3@cluster) #parámetro pi grupo 2
pm.comp.2 <-
  pi.comp.2 * dbinom(seq(0, 24, 1),
                     size = siembra,
                     prob = p.comp.2,
                     log = FALSE) #masa de probabilidad grupo 2


#representación gráfica del mejor modelo: gráficos de barras representando
#la asignación de cada planta madre a cada componente del mejor modelo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(
  vivas3[MejorModelo3@cluster == 1],#R2
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
hist(vivas3[MejorModelo3@cluster == 2],#R1
     breaks = seq(-0.5, 24.5, 1),
     add = T,
     col = "transparent")
#
# #leyenda
# legend(16, 9, c("R1", "R2", "R3"), pch=c(24,4,19), lty=1, pt.cex=c(1.5, 1, 1), cex=1.5)
# legend(8,9, c("R1", "R2", "R3"), density=c(NA, NA, 15), fill=c("transparent", "gray90","black"), cex=1.5)

#representación gráfica del mejor modelo: adicionar al gráfico de barras anterior
#la distribución binomial correspondiente a cada componente del mejor modelo
par(new = T)
plot(
  0:24,
  pm.comp.1,
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


#Guardar asignació de grupos de reclutamiento a 51.4 meses

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/reclutamiento/datos")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/datos/reclutamiento") #Ivan's working directory Waterman
# write.csv(
#   grupo.reclutamiento.3,
#   file = paste(
#     "grupo.reclutamiento.3_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#################################################################################################################
#################################################################################################################
#8) Tablas de clasificación cruzadas entre modelos de reclutamiento y entre morfología y estadísticos
#Goodman-Kruskal tau para la concordancia entre grupos
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 8.1) 11.4 meses después de la siembra vs. 19.6 meses después de la siembra
grupo.reclutamiento.1.vs.2 <-
  merge(
    grupo.reclutamiento.1[, c(1, 3)],
    grupo.reclutamiento.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    all = T,
    suffixes = c(".11.4", ".19.6")
  )
table(grupo.reclutamiento.1.vs.2[, 3],
      grupo.reclutamiento.1.vs.2[, 2])
#    1  2  3
# 1  1  0 20
# 2 18  0  0
# 3  0  4  0

# 8.1.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de reclutamiento a 11.4 y
#19.6: tau(11.4, 19.6) y tau(19.6, 11.4)

colnames(grupo.reclutamiento.1.vs.2)
GKtau(
  grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4,
  grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6
)
# xName
# 1 grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4
# yName Nx Ny tauxy tauyx
# 1 grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6  3  3 0.924 0.924

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6)
  GKtau.nulo <-
    GKtau(grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
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
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4,
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, 19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4 ,
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6
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
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4,
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6,11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.11.4,
    grupo.reclutamiento.1.vs.2$Phenotypic.Group.19.6
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0

#################################################################################################################
# 8.2) 19.6 meses después de la siembra vs. 51.4 meses después de la siembra
grupo.reclutamiento.2.vs.3 <-
  merge(
    grupo.reclutamiento.2[, c(1, 3)],
    grupo.reclutamiento.3[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".51.4")
  )
table(grupo.reclutamiento.2.vs.3[, 3],
      grupo.reclutamiento.2.vs.3[, 2])
#    1  2  3
# 1 21  7  0
# 2  0 11  4

# 8.2.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de reclutamiento a 19.6 y
#51.4: tau(19.6, 51.4) y tau(51.4, 19.6)

colnames(grupo.reclutamiento.2.vs.3)
GKtau(
  grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6,
  grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4
)
# xName
# 1 grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6
# yName Nx Ny tauxy tauyx
# 1 grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4  3  2 0.562 0.341

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4)
  GKtau.nulo <-
    GKtau(grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
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
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6,
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, 51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6 ,
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4
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
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6,
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4,19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.19.6,
    grupo.reclutamiento.2.vs.3$Phenotypic.Group.51.4
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 3e-05

#################################################################################################################
# 8.3) 11.4 meses después de la siembra vs. 51.4 meses después de la siembra

grupo.reclutamiento.1.vs.3 <-
  merge(
    grupo.reclutamiento.1[, c(1, 3)],
    grupo.reclutamiento.3[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".51.4")
  )
table(grupo.reclutamiento.1.vs.3[, 2],
      grupo.reclutamiento.1.vs.3[, 3])
#    1  2
# 1  8 11
# 2  0  4
# 3 20  0

# 8.3.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de reclutamiento a 51.4 y
#11.4: tau(51.4, 11.4) y tau(11.4, 51.4)

colnames(grupo.reclutamiento.1.vs.3)
GKtau(
  grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4,
  grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4
)
# xName
# 1 grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4
# yName Nx Ny tauxy tauyx
# 1 grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4  2  3 0.306 0.526

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4)
  GKtau.nulo <-
    GKtau(grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
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
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4,
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, 11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4 ,
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 5e-05
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
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4,
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4,51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.51.4,
    grupo.reclutamiento.1.vs.3$Phenotypic.Group.11.4
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0

#################################################################################################################
# 8.4) 11.4 meses después de la siembra vs. morfología

grupo.reclutamiento.1.vs.morfologia <-
  merge(
    grupo.reclutamiento.1[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".madre")
  )

table(grupo.reclutamiento.1.vs.morfologia[, 2],
      grupo.reclutamiento.1.vs.morfologia[, 3])
#    2  3  4  5
# 1  4 10  0  5
# 2  0  1  2  1
# 3  2  7  3  8

# 8.4.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de reclutamiento a 11.4 y
#Group.madre: tau(11.4, Group.madre) y tau(Group.madre, 11.4)

colnames(grupo.reclutamiento.1.vs.morfologia)
GKtau(
  grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4,
  grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre
)
# xName
# 1 grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4
# yName Nx Ny tauxy tauyx
# 1 grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre  3  4 0.058 0.105

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre)
  GKtau.nulo <-
    GKtau(grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
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
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4,
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(11.4, Group.madre) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4 ,
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.29771
#gráfica de la distribución nula de tau(Group.madre, 11.4),
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
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4,
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(Group.madre,11.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.11.4,
    grupo.reclutamiento.1.vs.morfologia$Phenotypic.Group.madre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.16693


#################################################################################################################
# 8.5) 19.6 meses después de la siembra vs. morfología

grupo.reclutamiento.2.vs.morfologia <-
  merge(
    grupo.reclutamiento.2[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".madre")
  )

table(grupo.reclutamiento.2.vs.morfologia[, 2],
      grupo.reclutamiento.2.vs.morfologia[, 3])
#   2 3 4 5
# 1 2 8 3 8
# 2 4 9 0 5
# 3 0 1 2 1

# 8.5.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de reclutamiento a 19.6 y
#Group.madre: tau(19.6, Group.madre) y tau(Group.madre, 19.6)

colnames(grupo.reclutamiento.2.vs.morfologia)
GKtau(
  grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6,
  grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre
)
# xName
# 1 grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6
# yName Nx Ny tauxy tauyx
# 1 grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre  3  4  0.05 0.093

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre)
  GKtau.nulo <-
    GKtau(grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
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
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6,
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(19.6, Group.madre) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6 ,
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
#0.39728
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
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6,
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(Group.madre,19.6) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.19.6,
    grupo.reclutamiento.2.vs.morfologia$Phenotypic.Group.madre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
#  0.2589

#################################################################################################################
# 8.6) 51.4 meses después de la siembra vs. morfología

grupo.reclutamiento.3.vs.morfologia <-
  merge(
    grupo.reclutamiento.3[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".51.4", ".madre")
  )

table(grupo.reclutamiento.3.vs.morfologia[, 2],
      grupo.reclutamiento.3.vs.morfologia[, 3])
#    2  3  4  5
# 1  3 13  3  9
# 2  3  5  2  5

# 8.6.1) Calcular los estadísticos Goodman-Kruskal tau para la concordancia entre grupos de reclutamiento a 51.4 y
#Group.madre: tau(51.4, Group.madre) y tau(Group.madre, 51.4)

colnames(grupo.reclutamiento.3.vs.morfologia)
GKtau(
  grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4,
  grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre
)
# xName
# 1 grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4
# yName Nx Ny tauxy tauyx
# 1 grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre  2  4 0.009 0.025

#modelo nulo para medir la significancia de los valores de los estadísticos Goodman-Kruskal tau
k <- 100000 #numero de iteraciones del modelo nulo
GKtau.nulo.mat <- matrix(NA, ncol = 2, nrow = k)
for (i in 1:k) {
  morfo.aleatorio <-
    sample(grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre)
  GKtau.nulo <-
    GKtau(grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4,
          morfo.aleatorio)
  GKtau.nulo.mat[i,] <- c(GKtau.nulo[[5]], GKtau.nulo[[6]])
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
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4,
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre
  )[[5]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(51.4, Group.madre) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4 ,
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre
  )[[5]] <= GKtau.nulo.mat[, 1]
) / k
# 0.78066
#gráfica de la distribución nula de tau(Group.madre, 51.4),
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
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4,
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre
  )[[6]],
  col = "red"
)

#calcular la significancia estadística (i.e., la probabilidad de que el modelo nulo genere
#un valor de tau(Group.madre,51.4) al menos tan extremo como el observado):
sum(
  GKtau(
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.51.4,
    grupo.reclutamiento.3.vs.morfologia$Phenotypic.Group.madre
  )[[6]] <= GKtau.nulo.mat[, 2]
) / k
# 0.82266

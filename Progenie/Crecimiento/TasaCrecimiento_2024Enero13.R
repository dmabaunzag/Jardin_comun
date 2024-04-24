################################################################################################################
#################################################################################################################
#################################################################################################################
#
##### TASA DE CRECIMIENTO DE LA PROGENIE DE LAS PLANTAS MADRE#######
#
#################################################################################################################
#################################################################################################################
#################################################################################################################
#
# INTRODUCCIÓN:
#
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
# 3) Crear marco de datos del crecimiento
# 4) Examinar el crecimiento

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

library(tidyverse) #limpiar y organizar datos

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
# nombre.progenie; seleccionar las variables para analizar tasa de crecimiento
colnames(datos.fenotipicos.marzo)
datos.fenotipicos.marzo.selected <- datos.fenotipicos.marzo %>%
  unite("nombre.progenie", c(Bandeja, Fila, Columna)) %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Longitud.tallo..cm.,
    Número.de.hojas
  ) %>%
  mutate(
    Número.colección.planta.madre=as.numeric(Número.colección.planta.madre),
    tiempo= 11.4
  )
head(datos.fenotipicos.marzo.selected)

# 2.1.2) Remover plantas de la progenie sin madre asignada o datos faltantes

# cuantas plantas madres están representadas
unique(datos.fenotipicos.marzo.selected$Número.colección.planta.madre)
length(unique(
  datos.fenotipicos.marzo.selected$Número.colección.planta.madre))## 38 plantas madres representadas

rows.with.na <-
  unique(which(is.na(datos.fenotipicos.marzo.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 14 plántulas hijas con NA

#correr las siguientes líneas en caso de existir NAs
datos.fenotipicos.marzo.selected <-
  datos.fenotipicos.marzo.selected[-rows.with.na,]
dim(datos.fenotipicos.marzo.selected) # 236 hijas con todos los datos
class(datos.fenotipicos.marzo.selected)
summary(datos.fenotipicos.marzo.selected)
head(datos.fenotipicos.marzo.selected)

length(unique(
  datos.fenotipicos.marzo.selected$Número.colección.planta.madre
))## hay representación de 37 plantas madres
unique(datos.fenotipicos.marzo.selected$Número.colección.planta.madre)##cuáles

#################################################################################################################
# 2.2) segunda medida (19.6 meses DS): octubre de 2020
summary(datos.fenotipicos.octubre)
head(datos.fenotipicos.octubre)
dim(datos.fenotipicos.octubre)# 250 plantas hijas sembradas y 33 variables


# 2.2.1) subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables de crecimiento

colnames(datos.fenotipicos.octubre)
datos.fenotipicos.octubre.selected <- 
  datos.fenotipicos.octubre %>%
  unite("nombre.progenie", c(Bandeja, Fila, Columna)) %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Longitud.tallo..cm.,
    Número.de.hojas
  ) %>%
  mutate(
    Número.colección.planta.madre = as.numeric(Número.colección.planta.madre),
    tiempo= 19.6)

head(datos.fenotipicos.octubre.selected)

unique(datos.fenotipicos.octubre.selected$Número.colección.planta.madre)
length(unique(
  datos.fenotipicos.octubre.selected$Número.colección.planta.madre))## 38 plantas madres representadas

#Remover filas sin madre asignada

datos.fenotipicos.octubre.selected <- 
  datos.fenotipicos.octubre.selected %>% 
  filter(!is.na(Número.colección.planta.madre))

#################################################################################################################
# 2.3) tercera medida (51.4 meses DS): junio de 2023
summary(datos.fenotipicos.junio)
head(datos.fenotipicos.junio)
dim(datos.fenotipicos.junio)# 180 plantas hijas sembradas y 29 variables

# 2.3.1)subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
# nombre.progenie; seleccionar las variables para analizar supervivencia

colnames(datos.fenotipicos.junio)
datos.fenotipicos.junio.selected <- datos.fenotipicos.junio %>%
  unite("nombre.progenie", c(Bandeja, Fila, Columna)) %>%
  select(
    Número.colección.planta.madre,
    nombre.progenie,
    Longitud.tallo..mm.,
    Número.de.hojas
  ) %>%
  mutate(
    Longitud.tallo..cm.= Longitud.tallo..mm./10,.keep="unused",# Pasar a cm la longitud del tallo
    Número.colección.planta.madre = as.numeric(Número.colección.planta.madre),
    tiempo= 51.4
  ) 
#Remover filas sin madre asignada

datos.fenotipicos.octubre.selected <- 
  datos.fenotipicos.octubre.selected %>% 
  filter(!is.na(Número.colección.planta.madre))

head(datos.fenotipicos.junio.selected)

#################################################################################################################
#################################################################################################################
# 3) Crear marco de datos del crecimiento
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 3.1) combinar la tablas

crecimiento <-
  bind_rows(
    datos.fenotipicos.marzo.selected,
    datos.fenotipicos.junio.selected,
    datos.fenotipicos.octubre.selected
  )
View(crecimiento)

crecimiento <- 
  crecimiento %>% 
  rename(Collector.Collection.Number= Número.colección.planta.madre,
         Longitud.tallo= Longitud.tallo..cm.,
         Numero.hojas =Número.de.hojas)


setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")
# 
# # #RData
# save(crecimiento,
#      file = paste(
#        "Crecimiento_piloto_",
#        format(Sys.time(), "%Y%B%d_%H%M%S"),
#        ".RData",
#        sep = ""
#      ))


#################################################################################################################
#################################################################################################################
# 4) Examinar el crecimiento
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1) Número de hojas

# 4.1.1) Gráfica del promedio de hojas por planta madre

numero.hojas.madre <- 
  crecimiento %>% 
  select(Collector.Collection.Number,
         Numero.hojas,
         tiempo) %>% 
  filter(!is.na(Numero.hojas)) %>% 
  group_by(tiempo,Collector.Collection.Number) %>% 
  summarise(promedio= list(mean_se(Numero.hojas)) ) %>% 
  unnest(cols = c(promedio))


promedio.numero.hojas.madre <- 
  numero.hojas.madre%>% 
  select(tiempo, Collector.Collection.Number,y) %>% 
  spread(Collector.Collection.Number, y)
dim(promedio.numero.hojas.madre)

tiempo.0 <- data.frame(matrix(0, ncol=38, nrow=1))
colnames(tiempo.0) <- colnames(promedio.numero.hojas.madre)

promedio.numero.hojas.madre <- 
  bind_rows(tiempo.0,
            promedio.numero.hojas.madre)
head(promedio.numero.hojas.madre)
#gráfica promedio en el tiempo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  promedio.numero.hojas.madre$tiempo,
  promedio.numero.hojas.madre[, -1],
  axes = F,
  type = "l",
  lty=1,
  col="black",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 30),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  xlab = "Meses después de la siembra",
  ylab = "Número de hojas promedio"
)
title(expression("A)"), adj = 0, cex.main = 1.5)
# eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels =F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 30, 5),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 25, 1),
  labels = F,
  tcl = -0.3
)
  # Si barras de error....
# matlines(numero.hojas.madre$tiempo,
#         numero.hojas.madre[, 4:5],
#         col="gray60",
#         lty= 1)
# 
# barras.error.1 <- 
#   numero.hojas.madre %>% 
#   ungroup() %>% 
#   mutate(tiempo= tiempo-0.8) %>% 
#   select(tiempo, ymin, ymax, Collector.Collection.Number)
# barras.error.2 <- 
#   numero.hojas.madre %>% 
#   ungroup() %>% 
#   mutate(tiempo= tiempo+0.8) %>% 
#   select(tiempo, ymin, ymax, Collector.Collection.Number)
# barras.error <- 
#   bind_rows(barras.error.1,barras.error.2)
# 
# barras.min <- 
#   barras.error %>% 
#   select(-ymax) %>% 
#   spread(Collector.Collection.Number,ymin)
# matlines(barras.min[1:2,1], 
#          barras.min[1:2,-1],
#          col= "gray60",
#          lty=1)
# matlines(barras.min[3:4,1], 
#          barras.min[3:4,-1],
#          col= "gray60",
#          lty =1)
# matlines(barras.min[5:6,1], 
#          barras.min[5:6,-1],
#          col= "gray60",
#          lty=1)
# 
# barras.max <- 
#   barras.error %>% 
#   select(-ymin) %>% 
#   spread(Collector.Collection.Number,ymax)
# matlines(barras.max[1:2,1], 
#          barras.max[1:2,-1],
#          col= "gray60",
#          lty=1)
# matlines(barras.max[3:4,1], 
#          barras.max[3:4,-1],
#          col= "gray60",
#          lty =1)
# matlines(barras.max[5:6,1], 
#          barras.max[5:6,-1],
#          col= "gray60",
#          lty=1)
# matlines(
#   promedio.numero.hojas.madre$tiempo,
#   promedio.numero.hojas.madre[, -1],
#   axes = F,
#   type = "l",
#   lty=1,
#   main = NA,
#   cex.lab = 1.5,
#   ylim = c(0, 30),
#   xlim = c(0, 55),
#   xaxt = "n",
#   bty = "n",
#   col = "black"
# )

# 4.1.2) Gráfica de número hojas por plántula

numero.hojas<- 
  crecimiento %>% 
  select(Numero.hojas,
         tiempo, nombre.progenie) %>% 
  filter(!is.na(Numero.hojas)) %>% 
  spread(nombre.progenie, Numero.hojas)

dim(numero.hojas) # 238 hojas

tiempo.0 <- data.frame(matrix(0, ncol=238, nrow=1))
colnames(tiempo.0) <- colnames(numero.hojas)

numero.hojas <- 
  bind_rows(tiempo.0,
            numero.hojas)

head(numero.hojas)

#Gráfica
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  numero.hojas$tiempo,
  numero.hojas[, -1],
  axes = F,
  type = "l",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 40),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black",
  lty=1,
  xlab = "Meses después de la siembra",
  ylab = "Número de hojas"
)
title(expression("B)"), adj = 0, cex.main = 1.5)

# eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels =F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 40, 5),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 40, 1),
  labels = F,
  tcl = -0.3
)

# 4.1.3) Promedio de número de hojas en el tiempo

hojas <- crecimiento %>% 
  select(Numero.hojas,
         tiempo, nombre.progenie, Collector.Collection.Number) %>% 
  filter(!is.na(Numero.hojas)) %>% 
  mutate(tiempo= tiempo/12)

# Número de plántulas con hojas contadas
hojas %>% 
  group_by(tiempo) %>% 
  summarise(n=length(Numero.hojas))
# A tibble: 3 × 2
# tiempo     n
# <dbl> <int>
#   1   0.95   236
# 2   1.63   220
# 3   4.28   175

# promedio número de hojas y mediana
hojas %>% 
  summarise (promedio = mean(Numero.hojas),
             mediana=median(Numero.hojas),
             desviacion=sd(Numero.hojas))

# promedio mediana desviacion
# 1 12.76545      11   5.613375

# Numero de hojas promedio por tiempo
promedio.hojas.tiempo <- 
  hojas%>%
  group_by(tiempo) %>% 
  summarise(promedio = mean(Numero.hojas),
            desviacion= sd(Numero.hojas))
promedio.hojas.tiempo
# tiempo promedio desviacion
# <dbl>    <dbl>      <dbl>
#   1   0.95     10.6       2.35
# 2   1.63     11.6       4.58
# 3   4.28     17.3       7.16

# 4.1.4) Promedios del número de hojas por planta madre

promedio.hojas.madre <- 
  hojas %>% 
  group_by(Collector.Collection.Number, tiempo) %>% 
  summarise(promedio = mean(Numero.hojas),
            desviacion= sd(Numero.hojas), 
            mediana=median(Numero.hojas)) %>% 
  ungroup()

promedio.hojas.madre %>% 
  group_by(tiempo) %>% 
  summarise(promedio.1=mean(promedio),
            desviacion= sd(promedio))
# tiempo promedio.1 desviacion
# <dbl>      <dbl>      <dbl>
#   1   0.95       9.92       1.82
# 2   1.63      10.9        2.73
# 3   4.28      16.4        4.67


# 4.1.5) Histogramas de la pendiente del crecimiento en términos de número de hojas

#Para cada muestreo existe un delta de numero de hojas y un delta de tiempo entre el muestreo anterior.,la
#formula de la pendiente sería (hojas 11.4-hojas 0) / (11.4 meses-o meses)

# 4.1.5.1) POR PLANTA MADRE

 pendiente.promedio.hojas.madre <- 
   promedio.hojas.madre %>% 
   group_by(Collector.Collection.Number) %>% 
   arrange(tiempo, Collector.Collection.Number) %>% 
   mutate(diff.tiempo= tiempo-lag(tiempo, default = 0)) %>% 
   mutate(diff.hojas= promedio-lag(promedio, default = 0)) %>% 
   mutate(pendiente=diff.hojas/diff.tiempo)
   
view(pendiente.promedio.hojas.madre)
range(pendiente.promedio.hojas.madre$pendiente)# -4.390244 13.684211

# Graficar histogramas de número de hojas promedio

m.marzo <- 
  pendiente.promedio.hojas.madre %>% 
  filter(diff.tiempo=="0.95") %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.marzo$pendiente,
     breaks=seq(-5,15,1),
     xlab=NA,
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,15)
)
title(expression("A) 11.4 meses"),adj=0)
axis(side=1, at=-5:15, labels = F, tcl=-0.3)
axis(side=2, at=1:15, labels = F, tcl=-0.3)

m.octubre <- 
  pendiente.promedio.hojas.madre %>% 
  filter(diff.tiempo<0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.octubre$pendiente,
     breaks=seq(-5,15,1),
     xlab=NA,
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,15)
)
title(expression("B) 19.6 meses"),adj=0)
axis(side=1, at=-5:15, labels = F, tcl=-0.3)
axis(side=2, at=1:15, labels = F, tcl=-0.3)

m.junio <- 
  pendiente.promedio.hojas.madre %>% 
  filter(diff.tiempo>0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.junio$pendiente,
     breaks=seq(-5,15,1),
     xlab="Número de hojas promedio / año",
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,15)
)
title(expression("C) 51.4 meses"),adj=0)
axis(side=1, at=-5:15, labels = F, tcl=-0.3)
axis(side=2, at=1:15, labels = F, tcl=-0.3)

# 4.1.5.2) POR PLÁNTULA
pendiente.hojas<- 
  hojas %>% 
  group_by(nombre.progenie) %>% 
  arrange(tiempo, nombre.progenie) %>% 
  mutate(diff.tiempo= tiempo-lag(tiempo, default = 0)) %>% 
  mutate(diff.hojas= Numero.hojas-lag(Numero.hojas, default = 0)) %>% 
  mutate(pendiente=diff.hojas/diff.tiempo)
View(pendiente.hojas)
range(pendiente.hojas$pendiente)#  -11.70732  17.89474

pendiente.hojas %>% 
  group_by(tiempo) %>% 
  summarise(promedio=mean(pendiente),
            desviacion= sd(pendiente),
            mediana = median(pendiente))
# tiempo promedio desviacion mediana
# <dbl>    <dbl>      <dbl>   <dbl>
#   1   0.95    11.1        2.48   10.5 
# 2   1.63     1.47       5.77    1.46
# 3   4.28     2.04       2.79    1.89

# Graficar histogramas de número de hojas por plántula

m.marzo <- 
  pendiente.hojas %>% 
  filter(diff.tiempo=="0.95") %>% 
  select(pendiente)
#par(mar=c (5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.marzo$pendiente,
     breaks=seq(-12,18,2),
     xlab=NA,
     ylab="Número de plantas hijas",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,90)
)
title(expression("D)"),adj=0)
axis(side=1, at=-12:18, labels = F, tcl=-0.3)
axis(side=2, at=seq(0,90,5), labels = F, tcl=-0.3)

m.octubre <- 
  pendiente.hojas %>% 
  filter(diff.tiempo<0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.octubre$pendiente,
     breaks=seq(-12,18,2),
     xlab=NA,
     ylab="Número de plantas hijas",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,90)
)
title(expression("E)"),adj=0)
axis(side=1, at=-12:18, labels = F, tcl=-0.3)
axis(side=2, at=seq(0,90,5), labels = F, tcl=-0.3)

m.junio <- 
  pendiente.hojas %>% 
  filter(diff.tiempo>0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.junio$pendiente,
     breaks=seq(-12,18,2),
     xlab="Número de hojas / año",
     ylab="Número de plantas hijas",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,90)
)
title(expression("F)"),adj=0)
axis(side=1, at=-12:18, labels = F, tcl=-0.3)
axis(side=2, at=seq(0,90,5), labels = F, tcl=-0.3)


#################################################################################################################
# 4.2) Longitud del tallo.

# 4.2.1) Gráfica del promedio longitud de tallo por planta madre

longitud.tallo.madre <- 
  crecimiento %>% 
  select(Collector.Collection.Number,
                         Longitud.tallo,
                         tiempo) %>% 
  filter(!is.na(Longitud.tallo)) %>% 
  group_by(tiempo, Collector.Collection.Number) %>% 
  summarise(promedio= list(mean_se(Longitud.tallo)) ) %>% 
  unnest(cols = c(promedio))

promedio.longitud.tallo.madre <- 
  longitud.tallo.madre%>% 
  select(tiempo, Collector.Collection.Number,y) %>% 
  spread(Collector.Collection.Number, y)
dim(promedio.longitud.tallo.madre)

tiempo.0 <- data.frame(matrix(0, ncol=38, nrow=1))
colnames(tiempo.0) <- colnames(promedio.longitud.tallo.madre)

promedio.longitud.tallo.madre <- 
  bind_rows(tiempo.0,
            promedio.longitud.tallo.madre)

#gráfica promedio en el tiempo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  promedio.longitud.tallo.madre$tiempo,
  promedio.longitud.tallo.madre[, -1],
  axes = F,
  type = "l",
  lty=1,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black", 
  xlab = "Meses después de la siembra",
  ylab = "Longitud del tallo promedio (cm)"
)
title(expression("C)"), adj = 0, cex.main = 1.5)
# eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels =F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 10, 2),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 10, 0.5),
  labels = F,
  tcl = -0.3
)

#con barras de error...
# matlines(longitud.tallo.madre$tiempo,
#          longitud.tallo.madre[, 4:5],
#          col="gray60",
#          lty= 1)
# 
# barras.error.1 <- 
#   longitud.tallo.madre %>% 
#   ungroup() %>% 
#   mutate(tiempo= tiempo-0.8) %>% 
#   select(tiempo, ymin, ymax, Collector.Collection.Number)
# barras.error.2 <- 
#   longitud.tallo.madre %>% 
#   ungroup() %>% 
#   mutate(tiempo= tiempo+0.8) %>% 
#   select(tiempo, ymin, ymax, Collector.Collection.Number)
# barras.error <- 
#   bind_rows(barras.error.1,barras.error.2)
# 
# barras.min <- 
#   barras.error %>% 
#   select(-ymax) %>% 
#   spread(Collector.Collection.Number,ymin)
# matlines(barras.min[1:2,1], 
#          barras.min[1:2,-1],
#          col= "gray60",
#          lty=1)
# matlines(barras.min[3:4,1], 
#          barras.min[3:4,-1],
#          col= "gray60",
#          lty =1)
# matlines(barras.min[5:6,1], 
#          barras.min[5:6,-1],
#          col= "gray60",
#          lty=1)
# 
# barras.max <- 
#   barras.error %>% 
#   select(-ymin) %>% 
#   spread(Collector.Collection.Number,ymax)
# matlines(barras.max[1:2,1], 
#          barras.max[1:2,-1],
#          col= "gray60",
#          lty=1)
# matlines(barras.max[3:4,1], 
#          barras.max[3:4,-1],
#          col= "gray60",
#          lty =1)
# matlines(barras.max[5:6,1], 
#          barras.max[5:6,-1],
#          col= "gray60",
#          lty=1)
# matlines(
#   promedio.longitud.tallo.madre$tiempo,
#   promedio.longitud.tallo.madre[, -1],
#   axes = F,
#   type = "o",
#   pch= 1:37,
#   main = NA,
#   cex.lab = 1.5,
#   ylim = c(0, 10),
#   xlim = c(0, 55),
#   xaxt = "n",
#   bty = "n",
#   col = "black"
# )

# 4.2.2) Gráfica longitud de tallo por plántula

longitud.tallo<- 
  crecimiento %>% 
  select(Longitud.tallo,
         tiempo, nombre.progenie) %>% 
  filter(!is.na(Longitud.tallo)) %>% 
  spread(nombre.progenie, Longitud.tallo)

dim(longitud.tallo)

tiempo.0 <- data.frame(matrix(0, ncol=238, nrow=1))
colnames(tiempo.0) <- colnames(longitud.tallo)

longitud.tallo <- 
  bind_rows(tiempo.0,
            longitud.tallo)

#Gráfica
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  longitud.tallo$tiempo,
  longitud.tallo[, -1],
  axes = F,
  type = "l",
  lty=1,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black",
  xlab = "Meses después de la siembra",
  ylab = "Longitud del tallo (cm)"
)
title(expression("D)"), adj = 0, cex.main = 1.5)
# eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels =F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 10, 2),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.2
)
axis(
  side = 2,
  at = seq(0, 10, 0.5),
  labels = F,
  tcl = -0.3
)

# 4.2.3) Promedio de longitud del tallos en el tiempo

tallos <- crecimiento %>% 
  select(Longitud.tallo,
         tiempo, nombre.progenie, Collector.Collection.Number) %>% 
  filter(!is.na(Longitud.tallo)) %>% 
  mutate(tiempo= tiempo/12)

# Número de plántulas con longitud de tallos medidas
tallos %>% 
  group_by(tiempo) %>% 
  summarise(n=length(Longitud.tallo))
# tiempo     n
# <dbl> <int>
#   1   0.95   236
# 2   1.63   220
# 3   4.28   176

# promedio longitud de tallos y mediana
tallos %>% 
  summarise (promedio = mean(Longitud.tallo),
             mediana=median(Longitud.tallo),
             desviacion=sd(Longitud.tallo))

# promedio mediana desviacion
# 1  2.47478     1.3   2.346488

#   Longitud de tallo promedio por tiempo
promedio.tallo.tiempo <- 
  tallos%>%
  group_by(tiempo) %>% 
  summarise(promedio = mean(Longitud.tallo),
            desviacion= sd(Longitud.tallo))
promedio.tallo.tiempo
# tiempo promedio desviacion
# <dbl>    <dbl>      <dbl>
#   1   0.95    0.846      0.376
#   2   1.63    1.42       0.578
#   3   4.28    5.98       1.39


# 4.2.4) Promedios del número de hojas por planta madre

  promedio.tallos.madre <- 
  tallos %>% 
  group_by(Collector.Collection.Number, tiempo) %>% 
  summarise(promedio = mean(Longitud.tallo),
            desviacion= sd(Longitud.tallo))
View(promedio.tallos.madre)

# 4.2.5) Histogramas de la pendiente del crecimiento en términos de longitud del tallo

#Para cada muestreo existe un delta de la longitud del tallo y un delta de tiempo entre el muestreo anterior.,la
#formula de la pendiente sería (tallo 11.4-tallo 0) / (11.4 meses-o meses)

# 4.2.5.1) POR PLANTA MADRE
pendiente.promedio.tallos.madre <- 
  promedio.tallos.madre %>% 
  group_by(Collector.Collection.Number) %>% 
  arrange(tiempo, Collector.Collection.Number) %>% 
  mutate(diff.tiempo= tiempo-lag(tiempo, default = 0)) %>% 
  mutate(diff.tallos= promedio-lag(promedio, default = 0)) %>% 
  mutate(pendiente=diff.tallos/diff.tiempo)

view(pendiente.promedio.tallos.madre)
range(pendiente.promedio.tallos.madre$pendiente)# 0.1463415 2.7871698

# Graficar histogramas longitud de tallos promedio

m.marzo <- 
  pendiente.promedio.tallos.madre %>% 
  filter(diff.tiempo=="0.95") %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.marzo$pendiente,
     breaks=seq(0,2.8,0.2),
     xlab=NA,
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,12)
)
title(expression("A) 11.4 meses"),adj=0)
axis(side=1, at=seq(0,2.8,0.1), labels = F, tcl=-0.3)
axis(side=2, at=1:12, labels = F, tcl=-0.3)

m.octubre <- 
  pendiente.promedio.tallos.madre %>% 
  filter(diff.tiempo<0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.octubre$pendiente,
     breaks=seq(0,2.8,0.2),
     xlab=NA,
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,12)
)
title(expression("B) 19.6 meses"),adj=0)
axis(side=1, at=seq(0,2.8,0.1), labels = F, tcl=-0.3)
axis(side=2, at=1:12, labels = F, tcl=-0.3)

m.junio <- 
  pendiente.promedio.tallos.madre %>% 
  filter(diff.tiempo>0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.junio$pendiente,
     breaks=seq(0,2.8,0.2),
     xlab="Longitud del tallo promedio (cm) / año",
     ylab="Número de plantas madre",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,12)
)
title(expression("C) 51.4 meses"),adj=0)
axis(side=1, at=seq(0,2.8,0.1), labels = F, tcl=-0.3)
axis(side=2, at=1:12, labels = F, tcl=-0.3)

# 4.1.5.2) POR PLÁNTULA
pendiente.tallos<- 
  tallos %>% 
  group_by(nombre.progenie) %>% 
  arrange(tiempo, nombre.progenie) %>% 
  mutate(diff.tiempo= tiempo-lag(tiempo, default = 0)) %>% 
  mutate(diff.tallos= Longitud.tallo-lag(Longitud.tallo, default = 0)) %>% 
  mutate(pendiente=diff.tallos/diff.tiempo)
View(pendiente.tallos)
range(pendiente.tallos$pendiente)#-0.7317073  3.5121951

pendiente.tallos %>% 
  group_by(tiempo) %>% 
  summarise(promedio=mean(pendiente),
            desviacion= sd(pendiente),
            mediana = median(pendiente))
# tiempo promedio desviacion mediana
# <dbl>    <dbl>      <dbl>   <dbl>
#   1   0.95    0.890      0.396   0.842
# 2   1.63    0.829      0.732   0.732
# 3   4.28    1.71       0.525   1.69

# Graficar histogramas de número de tallos por plántula

m.marzo <- 
  pendiente.tallos %>% 
  filter(diff.tiempo=="0.95") %>% 
  select(pendiente)
#par(mar=c (5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.marzo$pendiente,
     breaks=seq(-0.8,3.6,0.2),
     xlab=NA,
     ylab="Número de plantas hijas",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,60)
)
title(expression("D)"),adj=0)
axis(side=1, at=seq(-0.8,3.6,0.1), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,60,5), labels = F, tcl=-0.3)

m.octubre <- 
  pendiente.tallos %>% 
  filter(diff.tiempo<0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.octubre$pendiente,
     breaks=seq(-0.8,3.6,0.2),
     xlab=NA,
     ylab="Número de plantas hijas",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,60)
)
title(expression("E)"),adj=0)
axis(side=1, at=seq(-0.8,3.6,0.1), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,60,5), labels = F, tcl=-0.3)

m.junio <- 
  pendiente.tallos %>% 
  filter(diff.tiempo>0.95) %>% 
  select(pendiente)
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
hist(m.junio$pendiente,
     breaks=seq(-0.8,3.6,0.2),
     xlab="Longitud de tallos (cm) / año",
     ylab="Número de plantas hijas",
     main=NA,
     cex.lab=1.3,
     ylim = c(0,60)
)
title(expression("F)"),adj=0)
axis(side=1, at=seq(-0.8,3.6,0.1), labels = F, tcl=-0.3)
axis(side=2, at=seq(0,60,5), labels = F, tcl=-0.3)
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

#correr las siguietes líneas en caso de existir NAs
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
dim(datos.fenotipicos.junio)# 179 plantas hijas sembradas y 26 variables

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
#gráfica promedio en el tiempo
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 5, 4, 5) + 0.1)
matplot(
  promedio.numero.hojas.madre$tiempo,
  promedio.numero.hojas.madre[, -1],
  axes = F,
  type = "n",
  pch= 1:37,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 30),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "gray20",
  xlab = "meses después de la siembra",
  ylab = "promedio de número de hojas"
)
title(expression("A)"), adj = 0, cex.main = 1.5)
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
matlines(numero.hojas.madre$tiempo,
        numero.hojas.madre[, 4:5],
        col="gray60",
        lty= 1)

barras.error.1 <- 
  numero.hojas.madre %>% 
  ungroup() %>% 
  mutate(tiempo= tiempo-0.8) %>% 
  select(tiempo, ymin, ymax, Collector.Collection.Number)
barras.error.2 <- 
  numero.hojas.madre %>% 
  ungroup() %>% 
  mutate(tiempo= tiempo+0.8) %>% 
  select(tiempo, ymin, ymax, Collector.Collection.Number)
barras.error <- 
  bind_rows(barras.error.1,barras.error.2)

barras.min <- 
  barras.error %>% 
  select(-ymax) %>% 
  spread(Collector.Collection.Number,ymin)
matlines(barras.min[1:2,1], 
         barras.min[1:2,-1],
         col= "gray60",
         lty=1)
matlines(barras.min[3:4,1], 
         barras.min[3:4,-1],
         col= "gray60",
         lty =1)
matlines(barras.min[5:6,1], 
         barras.min[5:6,-1],
         col= "gray60",
         lty=1)

barras.max <- 
  barras.error %>% 
  select(-ymin) %>% 
  spread(Collector.Collection.Number,ymax)
matlines(barras.max[1:2,1], 
         barras.max[1:2,-1],
         col= "gray60",
         lty=1)
matlines(barras.max[3:4,1], 
         barras.max[3:4,-1],
         col= "gray60",
         lty =1)
matlines(barras.max[5:6,1], 
         barras.max[5:6,-1],
         col= "gray60",
         lty=1)
matlines(
  promedio.numero.hojas.madre$tiempo,
  promedio.numero.hojas.madre[, -1],
  axes = F,
  type = "o",
  pch= 1:37,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 30),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black"
)

# 4.1.2) Gráfica de número hojas por plántula

numero.hojas<- 
  crecimiento %>% 
  select(Numero.hojas,
         tiempo, nombre.progenie) %>% 
  filter(!is.na(Numero.hojas)) %>% 
  spread(nombre.progenie, Numero.hojas)

dim(numero.hojas)

tiempo.0 <- data.frame(matrix(0, ncol=238, nrow=1))
colnames(tiempo.0) <- colnames(numero.hojas)

numero.hojas <- 
  bind_rows(tiempo.0,
            numero.hojas)

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
  col = "gray20",
  xlab = "meses después de la siembra",
  ylab = "Número de hojas"
)
title(expression("B)"), adj = 0, cex.main = 1.5)
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

# promedio número de hojas y mediana
hojas %>% 
  summarise (promedio = mean(Numero.hojas),
             mediana=median(Numero.hojas),
             desviacion=sd(Numero.hojas))

# promedio mediana desviacion
# 1 12.75199      11   5.616075

# Numero de hojas promedio por tiempo
promedio.hojas.tiempo <- 
  hojas%>%
  group_by(tiempo) %>% 
  summarise(promedio = mean(Numero.hojas),
            desviacion= sd(Numero.hojas))
# tiempo promedio desviacion
# <dbl>    <dbl>      <dbl>
#   1   0.95     10.6       2.35
# 2   1.63     11.6       4.58
# 3   4.28     17.3       7.20

#summary(lm(promedio~log(tiempo), data= promedio.hojas.tiempo))  
# Call:
#   lm(formula = promedio ~ tiempo, data = promedio.hojas.tiempo)
# 
# Residuals:
#   1        2        3 
# 0.17549 -0.22074  0.04525 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   8.4228     0.3100   27.17   0.0234 *
#   tiempo        2.0553     0.1147   17.92   0.0355 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2856 on 1 degrees of freedom
# Multiple R-squared:  0.9969,	Adjusted R-squared:  0.9938 
# F-statistic: 321.1 on 1 and 1 DF,  p-value: 0.03549

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



# 4.1.5) Histogramas de la pendiente del crecimiento en términos de número de hojas

 pendiente.promedio.hojas.madre <-
   promedio.hojas.madre %>% 
   mutate(m.11.4=)

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
  type = "n",
  pch= 1:37,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "gray20",
  xlab = "meses después de la siembra",
  ylab = "promedio longitud tallo (cm)"
)
title(expression("C)"), adj = 0, cex.main = 1.5)
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
matlines(longitud.tallo.madre$tiempo,
         longitud.tallo.madre[, 4:5],
         col="gray60",
         lty= 1)

barras.error.1 <- 
  longitud.tallo.madre %>% 
  ungroup() %>% 
  mutate(tiempo= tiempo-0.8) %>% 
  select(tiempo, ymin, ymax, Collector.Collection.Number)
barras.error.2 <- 
  longitud.tallo.madre %>% 
  ungroup() %>% 
  mutate(tiempo= tiempo+0.8) %>% 
  select(tiempo, ymin, ymax, Collector.Collection.Number)
barras.error <- 
  bind_rows(barras.error.1,barras.error.2)

barras.min <- 
  barras.error %>% 
  select(-ymax) %>% 
  spread(Collector.Collection.Number,ymin)
matlines(barras.min[1:2,1], 
         barras.min[1:2,-1],
         col= "gray60",
         lty=1)
matlines(barras.min[3:4,1], 
         barras.min[3:4,-1],
         col= "gray60",
         lty =1)
matlines(barras.min[5:6,1], 
         barras.min[5:6,-1],
         col= "gray60",
         lty=1)

barras.max <- 
  barras.error %>% 
  select(-ymin) %>% 
  spread(Collector.Collection.Number,ymax)
matlines(barras.max[1:2,1], 
         barras.max[1:2,-1],
         col= "gray60",
         lty=1)
matlines(barras.max[3:4,1], 
         barras.max[3:4,-1],
         col= "gray60",
         lty =1)
matlines(barras.max[5:6,1], 
         barras.max[5:6,-1],
         col= "gray60",
         lty=1)
matlines(
  promedio.longitud.tallo.madre$tiempo,
  promedio.longitud.tallo.madre[, -1],
  axes = F,
  type = "o",
  pch= 1:37,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black"
)

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
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "gray20",
  xlab = "meses después de la siembra",
  ylab = "Longitud del tallo (cm)"
)
title(expression("D)"), adj = 0, cex.main = 1.5)
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
# 3   4.28   173

# promedio longtud de tallos y mediana
tallos %>% 
  summarise (promedio = mean(Longitud.tallo),
             mediana=median(Longitud.tallo),
             desviacion=sd(Longitud.tallo))

# promedio mediana desviacion
# 1 2.452221     1.3   2.328058

#   Longitud de tallo promedio por tiempo
promedio.tallo.tiempo <- 
  tallos%>%
  group_by(tiempo) %>% 
  summarise(promedio = mean(Longitud.tallo),
            desviacion= sd(Longitud.tallo))
# tiempo promedio desviacion
# <dbl>    <dbl>      <dbl>
#   1   0.95    0.846      0.376
# 2   1.63    1.42       0.578
# 3   4.28    5.96       1.38

#summary(lm(promedio~log(tiempo), data= promedio.hojas.tiempo))  
# Call:
#   lm(formula = promedio ~ tiempo, data = promedio.hojas.tiempo)
# 
# Residuals:
#   1        2        3 
# 0.17549 -0.22074  0.04525 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   8.4228     0.3100   27.17   0.0234 *
#   tiempo        2.0553     0.1147   17.92   0.0355 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2856 on 1 degrees of freedom
# Multiple R-squared:  0.9969,	Adjusted R-squared:  0.9938 
# F-statistic: 321.1 on 1 and 1 DF,  p-value: 0.03549

# 4.2.4) Promedios del número de hojas por planta madre

promedio.tallos.madre <- 
  tallos %>% 
  group_by(Collector.Collection.Number) %>% 
  summarise(promedio = mean(Longitud.tallo),
            desviacion= sd(Longitud.tallo))
View(promedio.tallos.madre)
summary(lm(Longitud.tallo~as.factor(Collector.Collection.Number), data=tallos))
plot(as.factor(tallos$Collector.Collection.Number), tallos$Numero.hojas)
  
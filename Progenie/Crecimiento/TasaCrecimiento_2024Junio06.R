################################################################################################################
#################################################################################################################
#################################################################################################################

##### TASA DE CRECIMIENTO DE LA PROGENIE DE LAS PLANTAS MADRE#######

#################################################################################################################
#################################################################################################################
#################################################################################################################

#INTRODUCCIÓN

#Descripción de las variables de crecimiento y su tasa de crecimiento en términos de longitud del
#tallo en frailejones a cuatro años después de su siembra en un jardín común

#REQUERIMIENTOS

#"PhenotypicDataProgeny_Quebradas_2020Marzo.csv", datos fenotípicos de la progenie a 11.4 meses después de la
#siembra

#"PhenotypicDataProgeny_Quebradas_2020Octubre.csv", datos fenotípicos de la progenie a 19.6 meses después de la
#siembra

#"PhenotypicDataProgeny_Quebradas_2023Junio.csv", datos fenotípicos de la progenie a 51.4 meses después de la
#siembra

#"PhenotypicGroupAssignment_2023septiembre08_120644.csv", datos de la clasificación de los grupos según
#morfología para las plantas madres + 307 especímenes usados en Pineda et al. (2020)

#CONTENIDO

# 1) Datos preliminares: Carga de librerías y lectura de datos

# 2) Seleccionar las variables a usar en cada tabla

# 3) Crear marco de datos del crecimiento

# 4) Examinar el crecimiento

# 5) Tasa de crecimiento en términos de la pendiente de la longitud del tallo por año

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

# Seleccionar directorio de trabajo
# Seleccionar el directorio de trabajo 
project_dir <- "C:/Users/dmaba/OneDrive - Universidad Nacional de Colombia/PROYECTO JARDÍN COMUN/Jardin_comun" # Diana's directory

# Define sub directories
data_path <- file.path(project_dir, "Progenie", "datos") # data file
figures_path <- file.path(project_dir, "Progenie", "Figuras") # figures file
datos_crecimiento_path <- file.path(project_dir, "Progenie", "Crecimiento", "datos")
setwd(data_path)

#################################################################################################################
# 1.2)leer las tablas

# Leer datos fenotípicos de la progenie a 11.4 meses después de la siembra
datos.fenotipicos.marzo <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2020Marzo.csv",
    header = T,
    sep = ","
  )
summary(datos.fenotipicos.marzo)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables

# Leer datos fenotípicos de la progenie a 19.6 meses después de la siembra
datos.fenotipicos.octubre <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2020Octubre.csv",
    header = T,
    sep = ","
  )
summary(datos.fenotipicos.octubre)
head(datos.fenotipicos.octubre)
dim(datos.fenotipicos.octubre)# 250 plantas hijas sembradas y 33 variables

# Leer datos fenotípicos de la progenie a 51.4 meses después de la siembra
datos.fenotipicos.junio <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2023Junio.csv",
    header = T,
    sep = ","
  )
summary(datos.fenotipicos.junio)
head(datos.fenotipicos.junio)
dim(datos.fenotipicos.junio)# 180 plantas hijas sembradas y 29 variables

# directorio de los datos fenotípicos según su morfología:

setwd(file.path(project_dir, "Especimenes","datos"))

phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
summary(phenotypic.group.assignment)
head(phenotypic.group.assignment)
dim(phenotypic.group.assignment)

#################################################################################################################
#################################################################################################################
# 2) Seleccionar las variables a usar en cada tabla
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 2.1) Subconjunto de los datos fenotípicos de la progenie a 11.4 meses después de la siembra (marzo de
#      2020)

# Seleccionar las variables para analizar tasa de crecimiento
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
    Número.colección.planta.madre = as.numeric(Número.colección.planta.madre),
    tiempo = 11.4
  )
head(datos.fenotipicos.marzo.selected)

# Remover plantas de la progenie sin madre asignada o datos faltantes
unique(datos.fenotipicos.marzo.selected$Número.colección.planta.madre)
length(unique(
  datos.fenotipicos.marzo.selected$Número.colección.planta.madre
))## 38 plantas madres representadas

rows.with.na <-
  unique(which(is.na(datos.fenotipicos.marzo.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 14 plántulas hijas con NA

# Correr las siguientes líneas en caso de existir NAs
datos.fenotipicos.marzo.selected <-
  datos.fenotipicos.marzo.selected[-rows.with.na, ]
dim(datos.fenotipicos.marzo.selected) # 236 hijas con todos los datos
class(datos.fenotipicos.marzo.selected)
summary(datos.fenotipicos.marzo.selected)
head(datos.fenotipicos.marzo.selected)

length(unique(
  datos.fenotipicos.marzo.selected$Número.colección.planta.madre
))## hay representación de 37 plantas madres
unique(datos.fenotipicos.marzo.selected$Número.colección.planta.madre)##cuáles

#################################################################################################################
# 2.2) Subconjunto de los datos fenotípicos de la progenie a 19.6 meses después de la siembra (octubre de 2020)

# Seleccionar las variables de crecimiento
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
    tiempo = 19.6
  )

head(datos.fenotipicos.octubre.selected)

unique(datos.fenotipicos.octubre.selected$Número.colección.planta.madre)
length(unique(
  datos.fenotipicos.octubre.selected$Número.colección.planta.madre
))## 38 plantas madres representadas

# Remover filas sin madre asignada
datos.fenotipicos.octubre.selected <-
  datos.fenotipicos.octubre.selected %>%
  filter(!is.na(Número.colección.planta.madre))

#################################################################################################################
# 2.3) Subconjunto de los datos fenotípicos de la progenie a 51.4 meses después de la siembra (junio de 2023)

# Seleccionar las variables para analizar supervivencia
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
    Longitud.tallo..cm. = Longitud.tallo..mm. / 10,
    .keep = "unused",
    # Pasar a cm la longitud del tallo
    Número.colección.planta.madre = as.numeric(Número.colección.planta.madre),
    tiempo = 51.4
  )

# Remover filas sin madre asignada
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
  rename(
    Collector.Collection.Number = Número.colección.planta.madre,
    Longitud.tallo = Longitud.tallo..cm.,
    Numero.hojas = Número.de.hojas
  )

# Guardar marco de datos de datos para el crecimiento
# setwd(datos_crecimiento_path)
#
# # #RData
# save(crecimiento,
#   file = paste(
#    "Crecimiento_piloto_",
#    format(Sys.time(), "%Y%B%d_%H%M%S"),
#    ".RData",
#    sep = ""
#   ))

#################################################################################################################
#################################################################################################################
# 4) Examinar el crecimiento
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1)  Examinar el crecimiento de la progenie por el Número de hojas

# Promedio de número de hojas en el tiempo
hojas <- crecimiento %>%
  select(Numero.hojas,
         tiempo,
         nombre.progenie,
         Collector.Collection.Number) %>%
  filter(!is.na(Numero.hojas)) %>%
  mutate(tiempo = tiempo / 12)

# Número de plántulas con hojas contadas
hojas %>%
  group_by(tiempo) %>%
  summarise(n = length(Numero.hojas))
# A tibble: 3 × 2
# tiempo   n
# <dbl> <int>
#  1  0.95  236
# 2  1.63  220
# 3  4.28  175

# Promedio número de hojas y mediana en total
hojas %>%
  summarise (
    promedio = mean(Numero.hojas),
    mediana = median(Numero.hojas),
    desviacion = sd(Numero.hojas)
  )

# promedio mediana desviacion
# 1 12.76545   11  5.613375

# Numero de hojas promedio por tiempo
promedio.hojas.tiempo <-
  hojas %>%
  group_by(tiempo) %>%
  summarise(promedio = mean(Numero.hojas),
            desviacion = sd(Numero.hojas))
hojas %>%
  group_by(tiempo) %>%
  summarise(
    promedio = mean(Numero.hojas),
    desviacion = sd(Numero.hojas),
    minimo = min(Numero.hojas),
    maximo = max(Numero.hojas)
  )
# tiempo promedio desviacion minimo maximo
# <dbl>  <dbl>   <dbl> <int> <int>
# 1  0.95   10.6    2.35   4   17
# 2  1.63   11.6    4.58   5   24
# 3  4.28   17.3    7.16   2   37

# Promedio del número de hojas por planta madre
promedio.hojas.madre <-
  hojas %>%
  group_by(Collector.Collection.Number, tiempo) %>%
  summarise(
    promedio = mean(Numero.hojas),
    desviacion = sd(Numero.hojas),
    mediana = median(Numero.hojas)
  ) %>%
  ungroup()

promedio.hojas.madre %>%
  group_by(tiempo) %>%
  summarise(
    promedio.1 = mean(promedio),
    desviacion = sd(promedio),
    minimo = min(promedio),
    maximo = max(promedio)
  )
# tiempo promedio.1 desviacion minimo maximo
# <dbl>   <dbl>   <dbl> <dbl> <dbl>
#  1  0.95    9.92    1.82  4   13
# 2  1.63   10.9    2.73  6   15.5
# 3  4.28   16.4    4.67  3.5  22.1

# 4.1.1) Gráficas del número de hojas por plantas madre en el tiempo medido
numero.hojas.madre <-
  crecimiento %>%
  select(Collector.Collection.Number,
         Numero.hojas,
         tiempo) %>%
  filter(!is.na(Numero.hojas)) %>%
  group_by(tiempo, Collector.Collection.Number) %>%
  summarise(promedio = list(mean_se(Numero.hojas))) %>%
  unnest(cols = c(promedio))

promedio.numero.hojas.madre <-
  numero.hojas.madre %>%
  select(tiempo, Collector.Collection.Number, y) %>%
  spread(Collector.Collection.Number, y)
dim(promedio.numero.hojas.madre)

tiempo.0 <- data.frame(matrix(0, ncol = 38, nrow = 1))
colnames(tiempo.0) <- colnames(promedio.numero.hojas.madre)

promedio.numero.hojas.madre <-
  bind_rows(tiempo.0,
            promedio.numero.hojas.madre)
head(promedio.numero.hojas.madre)

# subconjunto del número de hojas por plantas madre medido en el tiempo sólo para las plantas asignadas a M3
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350,]

phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))


promedio.numero.hojas.madre.m3 <-
  numero.hojas.madre %>%
  select(tiempo, Collector.Collection.Number, y) %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>% 
  filter(Phenotypic.Group == 3)
  
promedio.numero.hojas.madre.m3 <-
  promedio.numero.hojas.madre.m3 %>% 
  select(tiempo, Collector.Collection.Number, y) %>%
  spread(Collector.Collection.Number, y)
dim(promedio.numero.hojas.madre.m3)

tiempo.0 <- data.frame(matrix(0, ncol = dim(promedio.numero.hojas.madre.m3)[2], nrow = 1))
colnames(tiempo.0) <- colnames(promedio.numero.hojas.madre.m3)

promedio.numero.hojas.madre.m3 <-
  bind_rows(tiempo.0,
            promedio.numero.hojas.madre.m3)
head(promedio.numero.hojas.madre.m3)

# Gráfica Media del número de hojas en el tiempo####
par(mar = c(5, 6, 4, 1) + 0.1)
matplot(
  promedio.numero.hojas.madre$tiempo,
  promedio.numero.hojas.madre[,-1],
  axes = F,
  type = "l",
  lty = 1,
  col = "black",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 30),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  xlab = NA,
  ylab = "Media del número\n de hojas"
)
title(expression("A)"), adj = 0, cex.main = 1.5)
# Eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels = F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = 19.6,
  labels = T,
  cex.axis = 1.5,
  tcl= F
  )
axis(
  side = 2,
  at = seq(0, 30, 5),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = seq(0, 30, 1),
  labels = F,
  tcl = -0.3
)
#Agregar sólo a las plantas madres asignadas a M3
matplot(
  promedio.numero.hojas.madre.m3$tiempo,
  promedio.numero.hojas.madre.m3[,-1],
  axes = F,
  type = "l",
  lty = 1,
  col = "green3",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 30),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  add= T
)
#################################################################################################################
# 4.1.2) Gráfica del Número hojas por plántula en el tiempo medido

numero.hojas <-
  crecimiento %>%
  select(Numero.hojas,
         tiempo, nombre.progenie) %>%
  filter(!is.na(Numero.hojas)) %>%
  spread(nombre.progenie, Numero.hojas)

dim(numero.hojas) # 238 hojas

tiempo.0 <- data.frame(matrix(0, ncol = 238, nrow = 1))
colnames(tiempo.0) <- colnames(numero.hojas)

numero.hojas <-
  bind_rows(tiempo.0,
            numero.hojas)

head(numero.hojas)

# Solo plantas madre de M3
numero.hojas.madre.m3 <-
  crecimiento %>%
  select(Numero.hojas,
         tiempo, nombre.progenie, Collector.Collection.Number) %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  select(Numero.hojas,
         tiempo, nombre.progenie) %>%
  filter(!is.na(Numero.hojas)) %>%
  spread(nombre.progenie, Numero.hojas)

dim(numero.hojas.madre.m3) #  3 93

tiempo.0 <- data.frame(matrix(0, ncol = dim(numero.hojas.madre.m3)[2], nrow = 1))
colnames(tiempo.0) <- colnames(numero.hojas.madre.m3)

numero.hojas.madre.m3 <-
  bind_rows(tiempo.0,
            numero.hojas.madre.m3)

head(numero.hojas.madre.m3)

#Gráfica del número de hojas por plántula en el tiempo:####
par(mar = c(5, 6, 4, 1) + 0.1)
matplot(
  numero.hojas$tiempo,
  numero.hojas[,-1],
  axes = F,
  type = "l",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 40),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black",
  lty = 1,
  xlab = NA,
  ylab = "Número de hojas"
)
title(expression("B)"), adj = 0, cex.main = 1.5)

# Eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels = F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = 19.6,
  labels = T,
  cex.axis = 1.5,
  tcl = F
)
axis(
  side = 2,
  at = seq(0, 40, 10),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = seq(0, 40, 2),
  labels = F,
  tcl = -0.3
)
# Agregas datos de número de hojas de la progenie de las plantas madre asignadas a M3
matplot(
  numero.hojas.madre.m3$tiempo,
  numero.hojas.madre.m3[,-1],
  axes = F,
  type = "l",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 40),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "green3",
  lty = 1,
  add= T
)

#################################################################################################################
# 4.2) Examinar el crecimiento de la progenie por la longitud del tallo.

# Promedio de longitud del tallo en el tiempo

tallos <- crecimiento %>%
  select(Longitud.tallo,
         tiempo,
         nombre.progenie,
         Collector.Collection.Number) %>%
  filter(!is.na(Longitud.tallo)) %>%
  mutate(tiempo = tiempo / 12)

# Número de plántulas con longitud de tallos medidas
tallos %>%
  group_by(tiempo) %>%
  summarise(n = length(Longitud.tallo))
# tiempo   n
# <dbl> <int>
#  1  0.95  236
# 2  1.63  220
# 3  4.28  176

# Promedio longitud del tallo y mediana
tallos %>%
  summarise (
    promedio = mean(Longitud.tallo),
    mediana = median(Longitud.tallo),
    desviacion = sd(Longitud.tallo)
  )
# promedio mediana desviacion
# 1 2.47478   1.3  2.346488

# Promedio longitud de tallos y mediana por tiempo
tallos %>%
  group_by(tiempo) %>%
  summarise(
    promedio = mean(Longitud.tallo),
    desviacion = sd(Longitud.tallo),
    minimo = min(Longitud.tallo),
    maximo = max(Longitud.tallo)
  )
# tiempo promedio desviacion minimo maximo
# <dbl>  <dbl>   <dbl> <dbl> <dbl>
#  1  0.95  0.846   0.376  0.1  2.1
# 2  1.63  1.42    0.578  0.3  2.9
# 3  4.28  5.98    1.39  2.40  9.43

#  Longitud de tallo promedio por tiempo
promedio.tallo.tiempo <-
  tallos %>%
  group_by(tiempo) %>%
  summarise(promedio = mean(Longitud.tallo),
            desviacion = sd(Longitud.tallo))
promedio.tallo.tiempo
# tiempo promedio desviacion
# <dbl>  <dbl>   <dbl>
#  1  0.95  0.846   0.376
#  2  1.63  1.42    0.578
#  3  4.28  5.98    1.39

# Promedio de la longitud del tallo por planta madre
promedio.tallos.madre <-
  tallos %>%
  group_by(Collector.Collection.Number, tiempo) %>%
  summarise(promedio = mean(Longitud.tallo),
            desviacion = sd(Longitud.tallo))

View(promedio.tallos.madre)

promedio.tallos.madre %>%
  group_by(tiempo) %>%
  summarise(
    avg = mean(promedio),
    desviacion = sd(promedio),
    minimo = min(promedio),
    maximo = max(promedio),
    rango = maximo - minimo
  )
# A tibble: 3 × 6
# tiempo   avg desviacion minimo maximo rango
# <dbl> <dbl>      <dbl>  <dbl>  <dbl> <dbl>
#   1   0.95 0.788      0.303   0.3    1.7   1.4 
# 2   1.63 1.38       0.372   0.6    2     1.4 
# 3   4.28 5.83       1.06    3.12   7.99  4.86
 
# 4.2.1) Gráfica del promedio longitud de tallo por planta madre en el tiempo medido

longitud.tallo.madre <-
  crecimiento %>%
  select(Collector.Collection.Number,
         Longitud.tallo,
         tiempo) %>%
  filter(!is.na(Longitud.tallo)) %>%
  group_by(tiempo, Collector.Collection.Number) %>%
  summarise(promedio = list(mean_se(Longitud.tallo))) %>%
  unnest(cols = c(promedio))

promedio.longitud.tallo.madre <-
  longitud.tallo.madre %>%
  select(tiempo, Collector.Collection.Number, y) %>%
  spread(Collector.Collection.Number, y)
dim(promedio.longitud.tallo.madre)

tiempo.0 <- data.frame(matrix(0, ncol = 38, nrow = 1))
colnames(tiempo.0) <- colnames(promedio.longitud.tallo.madre)

promedio.longitud.tallo.madre <-
  bind_rows(tiempo.0,
            promedio.longitud.tallo.madre)

# Longitud del tallo promedio para plantas madre asignadas a M3
longitud.tallo.madre.m3 <-
  crecimiento %>%
  select(Collector.Collection.Number,
         Longitud.tallo,
         tiempo) %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  filter(!is.na(Longitud.tallo)) %>%
  group_by(tiempo, Collector.Collection.Number) %>%
  summarise(promedio = list(mean_se(Longitud.tallo))) %>%
  unnest(cols = c(promedio))

promedio.longitud.tallo.madre.m3 <-
  longitud.tallo.madre.m3 %>%
  select(tiempo, Collector.Collection.Number, y) %>%
  spread(Collector.Collection.Number, y)
dim(promedio.longitud.tallo.madre.m3)

tiempo.0 <- data.frame(matrix(0, ncol = dim(promedio.longitud.tallo.madre.m3)[2], nrow = 1))
colnames(tiempo.0) <- colnames(promedio.longitud.tallo.madre.m3)

promedio.longitud.tallo.madre.m3 <-
  bind_rows(tiempo.0,
            promedio.longitud.tallo.madre.m3)

# Gráfica promedio de la longitud del tallo en el tiempo:####
#par(mar=c(5, 4, 4, 2) + 0.1) #valor por defecto
par(mar = c(5, 6, 4, 1) + 0.1)
matplot(
  promedio.longitud.tallo.madre$tiempo,
  promedio.longitud.tallo.madre[,-1],
  axes = F,
  type = "l",
  lty = 1,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "black",
  xlab = "Meses después de la siembra",
  ylab = "Media de la longitud\n del tallo (cm)"
)
title(expression("C)"), adj = 0, cex.main = 1.5)
# Eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels = F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = 19.6,
  labels = T,
  cex.axis = 1.5,
  tcl = F
)
axis(
  side = 2,
  at = seq(0, 10, 2),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = seq(0, 10, 0.5),
  labels = F,
  tcl = -0.3
)

matplot(
  promedio.longitud.tallo.madre.m3$tiempo,
  promedio.longitud.tallo.madre.m3[,-1],
  axes = F,
  type = "l",
  lty = 1,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "green3",
  add = T
)

# 4.2.2) Gráfica longitud de tallo por plántula en el tiempo medido

longitud.tallo <-
  crecimiento %>%
  select(Longitud.tallo,
         tiempo, nombre.progenie) %>%
  filter(!is.na(Longitud.tallo)) %>%
  spread(nombre.progenie, Longitud.tallo)

dim(longitud.tallo)

tiempo.0 <- data.frame(matrix(0, ncol = 238, nrow = 1))
colnames(tiempo.0) <- colnames(longitud.tallo)

longitud.tallo <-
  bind_rows(tiempo.0,
            longitud.tallo)

# Longitud del tallo para la progenie del as plantas madre asignadas a M3
longitud.tallo.m3 <-
  crecimiento %>%
  select(Longitud.tallo,
         tiempo, nombre.progenie, Collector.Collection.Number) %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  select(Longitud.tallo,
         tiempo, nombre.progenie) %>%
  filter(!is.na(Longitud.tallo)) %>%
  spread(nombre.progenie, Longitud.tallo)

dim(longitud.tallo.m3)

tiempo.0 <- data.frame(matrix(0, ncol = dim(longitud.tallo.m3)[2], nrow = 1))
colnames(tiempo.0) <- colnames(longitud.tallo.m3)

longitud.tallo.m3 <-
  bind_rows(tiempo.0,
            longitud.tallo.m3)


# Gráfica de la longitud del tallo de las plántulas en el tiempo medido:####
par(mar = c(5, 6, 4, 1) + 0.1)
matplot(
  longitud.tallo$tiempo,
  longitud.tallo[,-1],
  axes = F,
  type = "l",
  lty = 1,
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
# Eje por año
axis(
  side = 1,
  at = seq(12, 48, 12),
  labels = F,
  col = "gray60"
)
axis(
  side = 1,
  at = c(0, 11.4, 19.6, 51.4),
  labels = T,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = 19.6,
  labels = T,
  cex.axis = 1.5,
  tcl =F
)
axis(
  side = 2,
  at = seq(0, 10, 2),
  labels = T,
  tcl = -0.5,
  las = 1,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = seq(0, 10, 0.5),
  labels = F,
  tcl = -0.3
)
# Agregar longitud del tallo para la progenie de las plantas madre asignadas a M3
matplot(
  longitud.tallo.m3$tiempo,
  longitud.tallo.m3[,-1],
  axes = F,
  type = "l",
  lty = 1,
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 10),
  xlim = c(0, 55),
  xaxt = "n",
  bty = "n",
  col = "green3",
  add = T
)

#################################################################################################################
#################################################################################################################
# 5) Tasa de crecimiento en términos de la pendiente de la longitud del tallo por año
#################################################################################################################
#################################################################################################################
# Para cada muestreo existe un delta de la longitud del tallo y un delta de tiempo entre el muestreo anterior.La
#formula de la pendiente sería (tallo 11.4-tallo 0) / (11.4 meses-o meses)

#################################################################################################################
# 5.1) Pendiente del la longitud del tallo por año por planta madre

pendiente.promedio.tallos.madre <-
  promedio.tallos.madre %>%
  group_by(Collector.Collection.Number) %>%
  arrange(tiempo, Collector.Collection.Number) %>%
  mutate(diff.tiempo = tiempo - lag(tiempo, default = 0)) %>%
  mutate(diff.tallos = promedio - lag(promedio, default = 0)) %>%
  mutate(pendiente = diff.tallos / diff.tiempo)

view(pendiente.promedio.tallos.madre)
# Examinar la pendiente por de la longitud del tallo por año por planta madre
pendiente.promedio.tallos.madre %>%
  group_by(tiempo) %>%
  summarise(
    promedio = mean(pendiente),
    desviacion = sd(pendiente),
    mediana = median(pendiente),
    minimo = min(pendiente),
    maximo = max (pendiente)
  )
# tiempo promedio desviacion mediana minimo maximo
# <dbl>  <dbl>   <dbl>  <dbl> <dbl> <dbl>
#  1  0.95  0.829   0.319  0.8  0.316  1.79
#  2  1.63  0.838   0.396  0.806 0.146  1.76
#  3  4.28  1.68    0.395  1.68  0.819  2.79

range(pendiente.promedio.tallos.madre$pendiente)# 0.1463415 2.7871698

# Pendiente por de la longitud del tallo por año por planta madre a 11.4 meses después de la siembra
m.marzo <-
  pendiente.promedio.tallos.madre %>%
  filter(tiempo == "0.95") %>%
  select(pendiente)

m.marzo.m3 <-
  pendiente.promedio.tallos.madre %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  filter(tiempo == "0.95") %>%
  select(pendiente)



# Seleccionar directorio de trabajo para guardar figuras
# setwd(figures_path)
# Gráfica de la distribución de la pendiente  de la longitud del tallo por año a 11.4 meses después de la siembra: Fig. S14A
par(mar = c(5, 5, 4, 1) + 0.1)
hist(
  m.marzo$pendiente,
  breaks = seq(0, 3.0, 0.2),
  xlab = NA,
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 12),
  xaxt = "n",
  yaxt = "n"
)
title(expression("A) 11.4 meses"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(0, 3, 0.5),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = seq(0, 3, 0.1),
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0, 12, 2),
  labels = T,
  tcl = -0.5,
  las = 2,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = 1:12,
  labels = F,
  tcl = -0.3
)

hist(
  m.marzo.m3$pendiente,
  breaks = seq(0, 3.0, 0.2),
  xlab = NA,
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 12),
  xaxt = "n",
  yaxt = "n",
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)

# Pendiente por de la longitud del tallo por año por planta madre a 19.6 meses después de la siembra
m.octubre <-
  pendiente.promedio.tallos.madre %>%
  filter(tiempo == "1.63333333333333") %>%
  select(pendiente)

m.octubre.m3 <-
  pendiente.promedio.tallos.madre %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  filter(tiempo == "1.63333333333333") %>%
  select(pendiente)

# Gráfica de la distribución de la pendiente  de la longitud del tallo por año a 19.6 meses después de la
# siembra: Fig. S14B
par(mar = c(5, 5, 4, 1) + 0.1)
hist(
  m.octubre$pendiente,
  breaks = seq(0, 3, 0.2),
  xlab = NA,
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 12),
  xaxt = "n",
  yaxt = "n"
)
title(expression("B) 19.6 meses"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(0, 3, 0.5),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = seq(0, 3, 0.1),
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0, 12, 2),
  labels = T,
  tcl = -0.5,
  las = 2,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = 1:12,
  labels = F,
  tcl = -0.3
)

hist(
  m.octubre.m3$pendiente,
  breaks = seq(0, 3, 0.2),
  xlab = NA,
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 12),
  xaxt = "n",
  yaxt = "n",
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)

# Pendiente por de la longitud del tallo por año por planta madre a 51.4 meses después de la siembra
m.junio <-
  pendiente.promedio.tallos.madre %>%
  filter(tiempo == "4.28333333333333") %>%
  select(pendiente)

m.junio.m3 <-
  pendiente.promedio.tallos.madre %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  filter(tiempo == "4.28333333333333") %>%
  select(pendiente)

# Gráfica de la distribución de la pendiente  de la longitud del tallo por año a 51.4 meses después de la siembra: Fig. S14C
par(mar = c(5, 5, 4, 1) + 0.1)
hist(
  m.junio$pendiente,
  breaks = seq(0, 3.0, 0.2),
  xlab = "Media de la longitud del tallo (cm) / año",
  ylab = "Número de plantas madre",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 12),
  xaxt = "n",
  yaxt = "n"
)
title(expression("C) 51.4 meses"), adj = 0, cex.main =1.5)
axis(
  side = 1,
  at = seq(0, 3, 0.5),
  labels = T,
  tcl = -0.5,
  cex.axis = 1.5
)
axis(
  side = 1,
  at = seq(0, 3, 0.1),
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0, 12, 2),
  labels = T,
  tcl = -0.5,
  las = 2,
  cex.axis = 1.5
)
axis(
  side = 2,
  at = 1:12,
  labels = F,
  tcl = -0.3
)
hist(
  m.junio.m3$pendiente,
  breaks = seq(0, 3.0, 0.2),
  cex.lab = 1.5,
  ylim = c(0, 12),
  xaxt = "n",
  yaxt = "n",
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)

#################################################################################################################
# 5.2) Pendiente del la longitud del tallo por año (por plántula)

pendiente.tallos <-
  tallos %>%
  group_by(nombre.progenie) %>%
  arrange(tiempo, nombre.progenie) %>%
  mutate(diff.tiempo = tiempo - lag(tiempo, default = 0)) %>%
  mutate(diff.tallos = Longitud.tallo - lag(Longitud.tallo, default = 0)) %>%
  mutate(pendiente = diff.tallos / diff.tiempo)
View(pendiente.tallos)
range(pendiente.tallos$pendiente)#-0.7317073 3.5121951

pendiente.tallos %>%
  group_by(tiempo) %>%
  summarise(
    promedio = mean(pendiente),
    desviacion = sd(pendiente),
    mediana = median(pendiente),
    minimo = min(pendiente),
    maximo = max(pendiente)
  )
# tiempo promedio desviacion mediana minimo maximo
# <dbl>  <dbl>   <dbl>  <dbl> <dbl> <dbl>
#  1  0.95  0.890   0.396  0.842 0.1c105  2.21
# 2  1.63  0.829   0.732  0.732 -0.732  3.51
# 3  4.28  1.71    0.525  1.69  0.378  2.95

# Pendiente por de la longitud del tallo por año a 11.4 meses después de la siembra
m.plantula.marzo <-
  pendiente.tallos %>%
  filter(tiempo == "0.95") %>%
  select(pendiente)
m.plantula.marzo.m3 <-
  pendiente.tallos %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  filter(tiempo == "0.95") %>%
  select(pendiente)
# Gráfica de la distribución de la pendiente  de la longitud del tallo por año a 11.4 meses después de la siembra: Fig. S14D
par(mar = c(5, 5, 4, 1) + 0.1)
hist(
  m.plantula.marzo$pendiente,
  breaks = seq(-0.8, 3.6, 0.2),
  xlab = NA,
  ylab = "Número de plantas hijas",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 60),
  xaxt = "n",
  yaxt = "n"
)
title(expression("D)"), adj = 0, cex.main =1.5)
axis(
  side = 1,
  at = seq(-0.8, 3.6, 0.2),
  labels = F,
  tcl = -0.3
)
axis(
  side = 1,
  at = seq(-0.8, 3.6, 0.4),
  labels = T,
  tcl = -0.5,
  cex.axis =1.5
)
axis(
  side = 2,
  at = seq(0, 60, 5),
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0, 60, 10),
  labels = T,
  tcl = -0.5,
  las =2,
  cex.axis = 1.5
)
hist(
  m.plantula.marzo.m3$pendiente,
  breaks = seq(-0.8, 3.6, 0.2),
  xlab = NA,
  ylab = "Número de plantas hijas",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 60),
  xaxt = "n",
  yaxt = "n",
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)

# Pendiente por de la longitud del tallo por año a 19.6 meses después de la siembra
m.plantula.octubre <-
  pendiente.tallos %>%
  filter(tiempo == "1.63333333333333") %>%
  select(pendiente)

m.plantula.octubre.m3 <-
  pendiente.tallos %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
    filter(tiempo =="1.63333333333333") %>%
  select(pendiente)
# Gráfica de la distribución de la pendiente  de la longitud del tallo por año a 19.6 meses después de la siembra: Fig. S14E
par(mar = c(5, 5, 4, 1) + 0.1)
hist(
  m.plantula.octubre$pendiente,
  breaks = seq(-0.8, 3.6, 0.2),
  xlab = NA,
  ylab = "Número de plantas hijas",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 60),
  xaxt = "n",
  yaxt = "n"
)
title(expression("E)"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(-0.8, 3.6, 0.2),
  labels = F,
  tcl = -0.3
)
axis(
  side = 1,
  at = seq(-0.8, 3.6, 0.4),
  labels = T,
  tcl = -0.5,
  cex.axis =1.5
)
axis(
  side = 2,
  at = seq(0, 60, 5),
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0, 60, 10),
  labels = T,
  tcl = -0.5,
  las =2,
  cex.axis = 1.5
)

hist(
  m.plantula.octubre.m3$pendiente,
  breaks = seq(-0.8, 3.6, 0.2),
  xlab = NA,
  ylab = "Número de plantas hijas",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 60),
  xaxt = "n",
  yaxt = "n",
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)
# Pendiente por de la longitud del tallo por año a 51.4 meses después de la siembra
m.plantula.junio <-
  pendiente.tallos %>%
  filter(tiempo == "4.28333333333333") %>%
  select(pendiente)

m.plantula.junio.m3 <-
  pendiente.tallos %>%
  inner_join(phenotypic.group.assignment.madres[, c(2,6)], by= "Collector.Collection.Number") %>%
  filter(Phenotypic.Group == 3) %>% 
  filter(tiempo== "4.28333333333333") %>%
  select(pendiente)
# Gráfica de la distribución de la pendiente  de la longitud del tallo por año a 51.4 meses después de la
# siembra: Fig. S14F
par(mar = c(5, 5, 4, 1) + 0.1)
hist(
  m.plantula.junio$pendiente,
  breaks = seq(-0.8, 3.6, 0.2),
  xlab = "Longitud del tallo (cm) / año",
  ylab = "Número de plantas hijas",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 60),
  xaxt = "n",
  yaxt = "n"
)
title(expression("F)"), adj = 0, cex.main = 1.5)
axis(
  side = 1,
  at = seq(-0.8, 3.6, 0.2),
  labels = F,
  tcl = -0.3
)
axis(
  side = 1,
  at = seq(-0.8, 3.6, 0.4),
  labels = T,
  tcl = -0.5,
  cex.axis =1.5
)
axis(
  side = 2,
  at = seq(0, 60, 5),
  labels = F,
  tcl = -0.3
)
axis(
  side = 2,
  at = seq(0, 60, 10),
  labels = T,
  tcl = -0.5,
  las =2,
  cex.axis = 1.5
)
hist(
  m.plantula.junio.m3$pendiente,
  breaks = seq(-0.8, 3.6, 0.2),
  xlab = "Longitud del tallo (cm) / año",
  ylab = "Número de plantas hijas",
  main = NA,
  cex.lab = 1.5,
  ylim = c(0, 60),
  xaxt = "n",
  yaxt = "n",
  density = 30,
  angle = 36,
  col= "green3",
  border = "black",
  add =T
)
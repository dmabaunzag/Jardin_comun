###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################
#INTRODUCCIÓN



###################################################################################################################
###################################################################################################################
# 1) Preliminares: cargar las librerías
###################################################################################################################
###################################################################################################################
# Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mezclas normales
library(ellipse)
library(ggplot2)

###################################################################################################################
###################################################################################################################
# 2) Leer los caracteres morfológicos
###################################################################################################################
###################################################################################################################

#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

#leer la tabla de datos: datos de la progenie tomados en Quebradas en 20/03/2020

phenodata.progenie <-
  read.table(
    "PhenotypicDataProgeny_Quebradas_2023Junio.csv",
    header = T,
    sep = ","
  )

View(phenodata.progenie)
summary(phenodata.progenie)
head(phenodata.progenie)
dim(phenodata.progenie)# 177 plantas del piloto con 26 variables


###################################################################################################################
###################################################################################################################
# 3) Limpiar los caracteres morfológicos: editar los datos y transformarlos.
#################################################################################################################
#################################################################################################################
# 3.1) Filtrar caracteres morfológicoss a usar

colnames(phenodata.progenie)#sólo se usará número de colección (2), longitud del tallo (9) y número de hojas (10)
phenodata.progenie.selected <- phenodata.progenie[, c(2, 9, 10)]
View(phenodata.progenie.selected)
colnames(phenodata.progenie.selected) <-
  c("Número de colección plantas madres",
    "Longitud del tallo",
    "Número de hojas")
dim(phenodata.progenie.selected) # 177 plantas de la progenie y 3 variables
#cambiar unidad de medida a cm
phenodata.progenie.selected[,2]<-phenodata.progenie.selected[,2]/10
#unidades de medida de cada variables
measurement.units <- c(NA,  "cm", "count")
data.frame(colnames(phenodata.progenie.selected), measurement.units)

#################################################################################################################
# 3.2) Remover plantas de la progenie sin madre asignada y con algún faltante en los caracteres morfológicos

# cuantas plantas madre están representadas
unique(phenodata.progenie.selected$`Número de colección plantas madres`)## todos son números
length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))# 33 plantas madres representadas

# excluir plantas de la progenie sin planta madre y que no tenga datos (excluir todos los NAs)

sapply(phenodata.progenie.selected, class)

phenodata.progenie.selected[, 1] <-
  as.numeric(phenodata.progenie.selected[, 1])

sapply(phenodata.progenie.selected, class)

rows.with.na <-
  unique(which(is.na(phenodata.progenie.selected), arr.ind = T)[, 1])
rows.with.na # especímenes con valores NA
length(rows.with.na)# 1 planta hija con NA

#correr las siguietes líneas en caso de existir NAs
phenodata.progenie.selected <-
  phenodata.progenie.selected[-rows.with.na, ]
dim(phenodata.progenie.selected) # 176 hijas con todos los datos
class(phenodata.progenie.selected)
summary(phenodata.progenie.selected)
head(phenodata.progenie.selected)

length(unique(
  phenodata.progenie.selected$`Número de colección plantas madres`
))## hay representación de 33 plantas madres
unique(phenodata.progenie.selected$`Número de colección plantas madres`)##cuáles

###################################################################################################################
# 3.3) Examinar gráficamente la distribución de cada caracter morfológico en escala logarítmica y linear.

# Examinar cuántas hijas tiene cada planta madre
sort(summary(
  as.factor(
    phenodata.progenie.selected$`Número de colección plantas madres`
  )
), decreasing = T)
#gráfica
barplot(
  sort(summary(
    as.factor(
      phenodata.progenie.selected$`Número de colección plantas madres`
    )
  ), decreasing = T),
  las = 2,
  xlab =  "Plantas madres",
  ylab = "Plantas hijas",
  ylim = c(0, 25)
)

colnames(phenodata.progenie.selected) # nombre de las variables
trait.x <- 2 # Longitud del tallo
colnames(phenodata.progenie.selected)[trait.x] # Qué variable
#distribución del número de hojas en escala lineal.
hist(
  phenodata.progenie.selected[, trait.x],
  breaks = 20,
  xlab = paste(
    colnames(phenodata.progenie.selected)[trait.x],
    "(",
    measurement.units[trait.x],
    ")"
  ),
  ylab = "Frecuencia",
  main = "",
  col = "gray80"
)
summary(phenodata.progenie.selected[, trait.x])
#distribución del número de hojas en escala logarítmica.
hist(
  log(phenodata.progenie.selected[, trait.x]),
  breaks = 20,
  xlab = paste(
    "log (",
    colnames(phenodata.progenie.selected)[trait.x],
    "(",
    measurement.units[trait.x],
    "))"
  ),
  ylab = "Frecuencia",
  main = "",
  col = "gray80"
)
summary(log(phenodata.progenie.selected[, trait.x]))

trait.x <- 3 # número de hojas
colnames(phenodata.progenie.selected)[trait.x] # Qué variable
#distribución del número de hojas en escala lineal.
hist(
  phenodata.progenie.selected[, trait.x],
  breaks = 10,
  xlab = paste(
    colnames(phenodata.progenie.selected)[trait.x],
    "(",
    measurement.units[trait.x],
    ")"
  ),
  ylab = "Frecuencia",
  main = "",
  col = "gray80"
)
summary(phenodata.progenie.selected[, trait.x])
#distribución del número de hojas en escala logarítmica.
hist(
  log(phenodata.progenie.selected[, trait.x]),
  breaks = 10,
  xlab = paste(
    "log (",
    colnames(phenodata.progenie.selected)[trait.x],
    "(",
    measurement.units[trait.x],
    "))"
  ),
  ylab = "Frecuencia",
  main = "",
  col = "gray80"
)
summary(log(phenodata.progenie.selected[, trait.x]))

###################################################################################################################
# 3.4) Examinar gráficamente relaciones bivariables
#definir dos caracteres morfológicos a examinar:
colnames(phenodata.progenie.selected)
trait.x <- 2
trait.y <- 3
#revisar los nombres de las variables seleccionadas
colnames(phenodata.progenie.selected)[trait.x]
summary(phenodata.progenie.selected[, trait.x])
colnames(phenodata.progenie.selected)[trait.y]
summary(phenodata.progenie.selected[, trait.y])

#graficar la relaciones bivariables
cor(phenodata.progenie.selected[, trait.x], phenodata.progenie.selected[, trait.y])# correlación:0.0632134
regresion <-
  lm(phenodata.progenie.selected[, trait.y] ~ phenodata.progenie.selected[, trait.x])
plot(
  phenodata.progenie.selected[, trait.x],
  phenodata.progenie.selected[, trait.y],
  xlab = paste(
    colnames(phenodata.progenie.selected)[trait.x],
    "(",
    measurement.units[trait.x],
    ")"
  ),
  ylab = paste(
    colnames(phenodata.progenie.selected)[trait.y],
    "(",
    measurement.units[trait.y],
    ")"
  ),
  cex.lab = 1,
  cex.axis = 1
)
abline(regresion)


###################################################################################################################
#  3.5) Transfomación de los datos.

# Dado que los rasgos fenotípicos frecuentemenente siguen distribución log-normal se transforman a escala logarítmica

phenodata.progenie.selected.log <-
  data.frame(phenodata.progenie.selected[, 1],
             log(phenodata.progenie.selected[, 2:3]))
head(phenodata.progenie.selected.log)

#editar el nombre de las variables
colnames(phenodata.progenie.selected.log) <-
  colnames(phenodata.progenie.selected)
colnames(phenodata.progenie.selected.log)[2:3] <-
  paste("log", paste(colnames(phenodata.progenie.selected)[2:3], sep = ""))
class(phenodata.progenie.selected.log)
summary(phenodata.progenie.selected.log)



###################################################################################################################
# 3.6) Promedios de los caracteres morfológicos por planta madre

mean.phenodata.progenie.selected.log <-
  aggregate(
    phenodata.progenie.selected.log,
    by = list(phenodata.progenie.selected.log[, 1]),
    FUN = function(x)
      mean(x, na.rm = T)
  )
mean.phenodata.progenie.selected.log <-
  mean.phenodata.progenie.selected.log[, 2:4]
View(mean.phenodata.progenie.selected.log)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
save(
  mean.phenodata.progenie.selected.log,
  file = paste(
    "mean.phenodata.progenie.selected.log_ (junio)_",
    format(Sys.time(), "%Y%B%d_%H%M%S"),
    ".RData",
    sep = ""
  )
)
load("mean.phenodata.progenie.selected.log_ (junio)_2023octubre24_194100.RData")


###################################################################################################################
# 3.7) graficar los caracteres morfológios

#  log longitud del tallo

boxplot(
  `log Longitud del tallo` ~ `Número de colección plantas madres`,
  data = phenodata.progenie.selected.log,
  las = 2
)
promedio.longitud.tallo <-
  tapply(
    phenodata.progenie.selected.log$`log Longitud del tallo`,
    phenodata.progenie.selected.log$`Número de colección plantas madres`,
    mean
  )
xi <- 0.4 + seq(phenodata.progenie.selected.log$n)
points(promedio.longitud.tallo, col = "blue", pch = 19)
legend("bottomright",
       legend = "Promedio",
       pch = 19,
       col = "blue")

# log número de hojas
boxplot(
  `log Número de hojas` ~ `Número de colección plantas madres`,
  data = phenodata.progenie.selected.log,
  las = 2
)
promedio.numero.hojas <-
  tapply(
    phenodata.progenie.selected.log$`log Número de hojas`,
    phenodata.progenie.selected.log$`Número de colección plantas madres`,
    mean
  )
xi <- 0.4 + seq(phenodata.progenie.selected.log$n)
points(promedio.numero.hojas, col = "blue", pch = 19)
legend("bottomright",
       legend = "Promedio",
       pch = 19,
       col = "blue")


## gráficas de dispersión entre la longitud del tallo y el número de hojas por planta madre

phenodata.progenie.selected.log[, 1] <-
  as.factor(phenodata.progenie.selected.log[, 1])

ggplot() +
  geom_point(
    data = phenodata.progenie.selected.log,
    aes(x = `log Longitud del tallo`, y = `log Número de hojas`, color = `Número de colección plantas madres`)
  ) +
  geom_smooth(
    data = phenodata.progenie.selected.log,
    aes(x = `log Longitud del tallo`,
        y = `log Número de hojas`),
    method = lm
  ) +
  #theme(element_text(`Número de colección plantas madres`), legend.position = "left")+
  geom_point(data = mean.phenodata.progenie.selected.log, aes(x = `log Longitud del tallo`,
                                                              y = `log Número de hojas`)) +
  geom_text(
    data = mean.phenodata.progenie.selected.log,
    aes(x = `log Longitud del tallo`,
        y = `log Número de hojas`,
        label = `Número de colección plantas madres`),
    cex = 2.5,
    vjust = 1.5
  )
cor(x = phenodata.progenie.selected.log$`log Longitud del tallo`,
    y = phenodata.progenie.selected.log$`log Número de hojas`)#  0.3939574
lm(`log Número de hojas` ~ `log Longitud del tallo`, data = phenodata.progenie.selected.log)

#Histograma de la distribución de las variables de crecimento en el promedio de las plantas madre
load("mean.phenodata.progenie.selected.log_ (junio)_2023octubre24_194100.RData")
range(mean.phenodata.progenie.selected.log$`log Longitud del tallo`)#3.413289 4.380275
hist(mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
     breaks = seq(-1.4,2.2,0.2),
     ylim = c(0,15),
     main=NA,
     xlab = "promedio del log de la longitud del tallo",
     ylab = "Número de plantas madre",
     cex.lab=1.3)
title(expression("C) cuatro años y dos meses DDS"), adj=0)
axis(side=1, at=seq(-1.4,2.2,0.1), labels = F, tcl=-0.3)
axis(side=2, at=1:15, labels = F, tcl=-0.3)

range(mean.phenodata.progenie.selected.log$`log Número de hojas`)# 1.151293 3.365753
hist(mean.phenodata.progenie.selected.log$`log Número de hojas`,
     breaks = seq(1,3.4,0.2),
     ylim = c(0,15),
     main=NA,
     xlab = "promedio del log de número de hojas",
     ylab = NA,
     cex.lab=1.3)
title(expression("F)"), adj=0)
axis(side=1, at=seq(1,3.4,0.1), labels = F, tcl=-0.3)
axis(side=2, at=1:15, labels = F, tcl=-0.3)
#################################################################################################################
#################################################################################################################
# 4) Ajuste de modelos de mezclas normales
#################################################################################################################
#################################################################################################################
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
load("mean.phenodata.progenie.selected.log_ (junio)_2023octubre24_194100.RData")
# subset con sólo los rasgos fenotípicos
data.for.GMM <- mean.phenodata.progenie.selected.log[, 2:3]

#################################################################################################################
# 4.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

#"PCS"
mclust.options(hcUse = "PCS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de lso especímenes
Mcluster.phenodata.progenie$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components:
#
#   log-likelihood  n  df       BIC      ICL
#       8.292518    33  8     -11.38703 -11.5128
#
# Clustering table:
#    1  2
#   30  3

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"VARS"
mclust.options(hcUse = "VARS")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #classifiación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#save(Mcluster.phenodata.progenie, file="Mcluster.phenodata.progenie.(junio).VARS_2023September04.RData")
#load("Mcluster.phenodata.progenie.(junio).VARS_2023September04.RData")
#
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components:
#
#   log-likelihood  n   df       BIC      ICL
#       8.292518    33  8     -11.38703 -11.5128
#
# Clustering table:
#    1  2
#   30  3
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"STD"
mclust.options(hcUse = "STD")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)#
Mcluster.phenodata.progenie$classification #clasificación de los especíemenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components:
#
#   log-likelihood  n   df       BIC      ICL
#      8.292518     33  8     -11.38703 -11.5128
#
# Clustering table:
#   1  2
#   30 3

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SPH"
mclust.options(hcUse = "SPH")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
# save(Mcluster.phenodata.progenie, file="Mcluster.phenodata.progenie.(junio).SPH_2023September04.RData")
# load("Mcluster.phenodata.progenie.(junio).SPH_2023September04.RData")
#
#   Mclust EVE (ellipsoidal, equal volume and orientation) model with
# 2 components:
#
#   log-likelihood  n df       BIC       ICL
#   9.754228       33  9      -11.96011 -12.22683
#
# Clustering table:
#   1  2
#   28  5

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"PCR"
mclust.options(hcUse = "PCR")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificacion de los especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components:
#
#   log-likelihood  n df       BIC      ICL
#         8.292518 33  8      -11.38703 -11.5128
#
# Clustering table:
#   1  2
#   30  3

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#"SVD"
mclust.options(hcUse = "SVD")
Mcluster.phenodata.progenie <- Mclust(data.for.GMM)
#examine results
Mcluster.phenodata.progenie
summary(Mcluster.phenodata.progenie)
names(Mcluster.phenodata.progenie$classification)
Mcluster.phenodata.progenie$classification #clasificación de os especímenes
Mcluster.phenodata.progenie$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.phenodata.progenie)
#
#   Mclust EEE (ellipsoidal, equal volume, shape and orientation)
# model with 2 components:
#
#   log-likelihood  n df       BIC      ICL
#       8.292518    33  8   -11.38703 -11.5128
#
# Clustering table:
#   1  2
#  30  3

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.phenodata.progenie,
     what = "classification",
     dimens = c(1, 2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.phenodata.progenie, what = "BIC")

#################################################################################################################
#################################################################################################################
#5) Examinar los grupos fenotípicos en base al mejor modelo de mezclas normales, de accuerdo con las variables de
#inicialización
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 5.1)VARS
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.phenodata.progenie.(junio).VARS_2023September04.RData")#modelos VARS
load("mean.phenodata.progenie.selected.log_ (junio)_2023octubre24_194100.RData")
#################################################################################################################
# 5.1.1)Examinar y guardar en un documento la asignación de grupos

#crear y escribir documento para la asignación de los grupos morfológicos.
phenotypic.group.assignment.piloto <-
  data.frame(
    as.numeric(rownames(mean.phenodata.progenie.selected.log)),
    mean.phenodata.progenie.selected.log[, 1],
    Mcluster.phenodata.progenie$classification,
    Mcluster.phenodata.progenie$uncertainty
  )
colnames(phenotypic.group.assignment.piloto) <-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.piloto)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# write.csv(
#   phenotypic.group.assignment.piloto,
#   file = paste(
#     "PhenotypicGroupAssignment.piloto.(junio).VARS_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )
#################################################################################################################
# 5.1.2) Graficar grupos morfológicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
#pdf("Figuras(junio)_sección_5.1(VARS).pdf")
#Longitud del tallo vs número de hojas
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  addEllipses = F,
  asp=1,
  xlab = "Promedio del log de la longitud del tallo",
  ylab="Promedio del log del número de hojas",
  cex.lab=1.2
)
legend(
  "bottomright",
  paste("C", 1:2),
  col = mclust.options("classPlotColors"),
  xpd = T,
  ncol = 2,
  pch = mclust.options("classPlotSymbols"),
  pt.lwd = 0.9,
  pt.cex = 0.9,
  cex = 0.9,
  bty = "o"
)
#agregar elipses
for (i in 1:Mcluster.phenodata.progenie$G) {
  points(
    ellipse(
      x = Mcluster.phenodata.progenie$parameters$variance$sigma[1:2, 1:2, i],
      centre = Mcluster.phenodata.progenie$parameters$mean[c(1, 2), i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
#agregar etiquetas de las elipses
#Mcluster.phenodata.progenie$parameters$mean
#                             [,1]       [,2]
# log Longitud del tallo 4.064562 3.616586
# log Número de hojas    2.812979 1.424830
text(Mcluster.phenodata.progenie$parameters$mean[1,1]-0.3,
     Mcluster.phenodata.progenie$parameters$mean[2,1], "C1",cex= 0.9)
text(Mcluster.phenodata.progenie$parameters$mean[1,2], 
     Mcluster.phenodata.progenie$parameters$mean[2,2], "C2", cex= 0.9)
title(expression("C) cuatro años y dos meses DDS"), adj=0)
#identificando puntos
# identify(
#   mean.phenodata.progenie.selected.log$`log Longitud del tallo`,
#   mean.phenodata.progenie.selected.log$`log Número de hojas`,
#   labels = as.character(
#     mean.phenodata.progenie.selected.log$`Número de colección plantas madres`
#   ),
#   cex=0.5
# )
#Plantas madre discordantes
discordantes<- c(1004, 1022, 1006, 1013, 1032, 1037, 1040, 1030, 1038, 1039)
muertas<- c(1029, 1025, 1001, 1026)
text( mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% discordantes,2],
      mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% discordantes,3],
      labels=as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% discordantes,1]),
      cex=0.6,
      pos=4)
text( mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% muertas,2],
      mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% muertas,3],
      labels=as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% muertas,1]),
      cex=0.6,
      pos=4,
      col="red")

  ###################################################################################################################
# 5.1.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata.progenie$uncertainty)

#De los 33 especímenes, ninguno de éstos tiene incertidumbre mayor a 0.1
sum(Mcluster.phenodata.progenie$uncertainty > 0.1)
sum(Mcluster.phenodata.progenie$uncertainty > 0.1) / length(Mcluster.phenodata.progenie$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata.progenie$classification[Mcluster.phenodata.progenie$uncertainty >
                                             0.1]

###################################################################################################################
# 5.1.4) Examinar la tabulación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos fenotípicos
#según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en junio de 2023
phenotypic.group.assignment.progenie <-
  read.table(
    "PhenotypicGroupAssignment.piloto.(junio).VARS_2023septiembre04_083353.csv",
    header = T,
    sep = ","
  )
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350, ]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))
phenotypic.group.assignment.madres$Phenotypic.Group <-
  as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(
  phenotypic.group.assignment.madres$Collector.Collection.Number,
  phenotypic.group.assignment.madres$Phenotypic.Group,
  length
)
# 2  3  4  5
# 6 18  5 14

tapply(
  phenotypic.group.assignment.progenie$Collector.Collection.Number,
  phenotypic.group.assignment.progenie$Phenotypic.Group,
  length
)
# 1  2
#30  3

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
phenotypic.group.assignment.madres.progenie <-
  merge(
    phenotypic.group.assignment.madres[, c(2, 6)],
    phenotypic.group.assignment.progenie[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie")
  )

phenotypic.group.crosstab <-
  table(phenotypic.group.assignment.madres.progenie[, c(2, 3)])
phenotypic.group.crosstab
# Phenotypic.Group.progenie
# Phenotypic.Group.madres  1  2
# 2  5  0
# 3 11  3
# 4  5  0
# 5  9  0
###################################################################################################################
###################################################################################################################
# 5.2)SPH
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.phenodata.progenie.(junio).SPH_2023September04.RData")

###################################################################################################################
# 5.2.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.piloto <-
  data.frame(
    as.numeric(rownames(mean.phenodata.progenie.selected.log)),
    mean.phenodata.progenie.selected.log[, 1],
    Mcluster.phenodata.progenie$classification,
    Mcluster.phenodata.progenie$uncertainty
  )
colnames(phenotypic.group.assignment.piloto) <-
  c(
    "Rownames.Meanphenodata",
    "Collector.Collection.Number",
    "Phenotypic.Group",
    "Uncertainty"
  )
head(phenotypic.group.assignment.piloto)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
# write.csv(
#   phenotypic.group.assignment.piloto,
#   file = paste(
#     "PhenotypicGroupAssignment.piloto.(junio).SPH_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )
###################################################################################################################
# 5.2.2) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
# pdf("Figuras(junio)_sección_5.2(SPH).pdf")
#Longitud del tallo vs número de hojas
#par(mar=c(5,4,4,2)+0.1) #default
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  Mcluster.phenodata.progenie,
  what = c("classification"),
  dimens = c(1, 2),
  main = "",
  addEllipses = F,
  asp=1,
  xlab = "Promedio del log de la longitud del tallo",
  ylab="Promedio del log del número de hojas",
  cex.lab=1.2
)
legend(
  "bottomright",
  paste("C", 1:2),
  col = mclust.options("classPlotColors"),
  xpd = T,
  ncol = 2,
  pch = mclust.options("classPlotSymbols"),
  pt.lwd = 0.9,
  pt.cex = 0.9,
  cex = 0.9,
  bty = "o"
)
#agregar elipses
for (i in 1:Mcluster.phenodata.progenie$G) {
  points(
    ellipse(
      x = Mcluster.phenodata.progenie$parameters$variance$sigma[1:2, 1:2, i],
      centre = Mcluster.phenodata.progenie$parameters$mean[c(1, 2), i],
      level = pchisq(1, 2)
    ),
    type = "l",
    col = "black"
  )
}
#agregar etiquetas de las elipses
#Mcluster.phenodata.progenie$parameters$mean
#                             [,1]       [,2]
# log Longitud del tallo 4.083514 3.682657
# log Número de hojas    2.853579 1.731799
text(Mcluster.phenodata.progenie$parameters$mean[1,1]-0.3,
     Mcluster.phenodata.progenie$parameters$mean[2,1]-0.05, "C1", cex= 0.9)
text(Mcluster.phenodata.progenie$parameters$mean[1,2]-0.2, 
     Mcluster.phenodata.progenie$parameters$mean[2,2], "C2", cex= 0.9)
title(expression("B) Cuatro años y dos meses DDS"), adj=0)

#Plantas madre discordantes
discordantes<- c(1004, 1022, 1006, 1013, 1032, 1037, 1040, 1030, 1038, 1039)
muertas<- c(1029, 1025, 1001, 1026)
text( mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% discordantes,2],
      mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% discordantes,3],
      labels=as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% discordantes,1]),
      cex=0.6,
      pos=4)
text( mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% muertas,2],
      mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% muertas,3],
      labels=as.character(mean.phenodata.progenie.selected.log[mean.phenodata.progenie.selected.log[,1]%in% muertas,1]),
      cex=0.6,
      pos=4,
      col="red")

###################################################################################################################
# 5.2.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.phenodata.progenie$uncertainty)

#De los 33 especímenes, ninguno tiene incertidumbre mayor a 0.1
sum(Mcluster.phenodata.progenie$uncertainty > 0.1)
sum(Mcluster.phenodata.progenie$uncertainty > 0.1) / length(Mcluster.phenodata.progenie$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.phenodata.progenie$classification[Mcluster.phenodata.progenie$uncertainty >
                                             0.1]

###################################################################################################################
# 5.2.4) Examinar la tabulación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos fenotípicos
#según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment <-
  read.table(
    "PhenotypicGroupAssignment_2023septiembre08_120644.csv",
    header = T,
    sep = ","
  )
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en marzo de 2020
phenotypic.group.assignment.progenie <-
  read.table(
    "PhenotypicGroupAssignment.piloto.(junio).SPH_2023septiembre04_090428.csv",
    header = T,
    sep = ","
  )
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres <-
  phenotypic.group.assignment[308:350, ]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number <-
  as.numeric(substring(
    phenotypic.group.assignment.madres$Collector.Collection.Number,
    5
  ))
phenotypic.group.assignment.madres$Phenotypic.Group <-
  as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(
  phenotypic.group.assignment.madres$Collector.Collection.Number,
  phenotypic.group.assignment.madres$Phenotypic.Group,
  length
)
# 2  3  4  5
# 6 18  5 14

tapply(
  phenotypic.group.assignment.progenie$Collector.Collection.Number,
  phenotypic.group.assignment.progenie$Phenotypic.Group,
  length
)
#   1  2
#  28  5

#Asignación de grupos de plantas progenie con datos de crecimiento tomados en junio de 2023
phenotypic.group.assignment.madres.progenie <-
  merge(
    phenotypic.group.assignment.madres[, c(2, 6)],
    phenotypic.group.assignment.progenie[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".madres", ".progenie")
  )


phenotypic.group.crosstab <-
  table(phenotypic.group.assignment.madres.progenie[, c(2, 3)])
phenotypic.group.crosstab
#                        Phenotypic.Group.progenie
# Phenotypic.Group.madres 1 2
# 2 5 0
# 3 9 5
# 4 5 0
# 5 9 0

###################################################################################################################
###################################################################################################################
# 5.3) Examinar la tabulación cruzada de los grupos fenotípicos según la progenie de los modelos
#de mezcla normal de VARS Y SPH.

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en marzo de 2020
phenotypic.group.assignment.progenie.SPH <- read.table(
  "PhenotypicGroupAssignment.piloto.(junio).SPH_2023septiembre04_090428.csv",
  header = T,
  sep = ","
)
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en junio de 2023
phenotypic.group.assignment.progenie.VARS <- read.table(
  "PhenotypicGroupAssignment.piloto.(junio).VARS_2023septiembre04_083353.csv",
  header = T,
  sep = ","
)

phenotypic.group.assignment.progenie.VARS.SPH <-
  merge(
    phenotypic.group.assignment.progenie.VARS[, c(2, 3)],
    phenotypic.group.assignment.progenie.SPH[, c(2, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".VARS", ".SPH")
  )

phenotypic.group.crosstab <-
  table(phenotypic.group.assignment.progenie.VARS.SPH[, c(2, 3)])
phenotypic.group.crosstab
#                       Phenotypic.Group.SPH
# Phenotypic.Group.VARS  1  2
# 1 28  2
# 2  0  3
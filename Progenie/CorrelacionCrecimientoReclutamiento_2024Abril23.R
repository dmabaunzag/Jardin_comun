#################################################################################################################
#################################################################################################################
# CORRELACIÓN DE PEARSON DEL RECLUTAMIENTO Y LAS VARIABLES DE CRECIMIENTO EN CADA UNO DE LOS MUESTREOS

#################################################################################################################
#################################################################################################################
# INTRODUCCIÓN

# Examinar si existe correlacion entre el reclutamiento y las variables de creciemiento (eg. número de hojas y
# longitud del tallo) en cada muestreo

#REQUERIMIENTOS

# "Crecimiento_piloto_2024abril23_110203" Tabla con la longitud del tallo y número de hojas vivas por progenie

# "sobrevivientes_piloto_2024abril23_110334" Tabla con el reclutamiento por planta madre

#
#CONTENIDO


#################################################################################################################
#################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos####
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 1.1) Librerías.

library(tidyverse)

#################################################################################################################
# 1.2)leer las tablas
# leer los datos de reclutas
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/reclutamiento/datos")#Directorio de Diana
load("sobrevivientes_piloto_2024abril23_110334.RData")
head(sobrevivientes)
colnames(sobrevivientes)
dim(sobrevivientes) 

# Leer los datos de crecimiento de la progenie
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Crecimiento/datos")#Directorio de Diana
load("Crecimiento_piloto_2024abril23_110203.RData")
head(crecimiento)
colnames(crecimiento)
dim(crecimiento)

#################################################################################################################
#################################################################################################################
# 2) Combinar tablas de crecimento promedio y reclutamiento por planta madre por muestreo
#################################################################################################################
#################################################################################################################

promedio.crecimiento <- 
  crecimiento %>% 
  select(!nombre.progenie) %>% 
  filter(!is.na(Numero.hojas)) %>% 
  group_by(tiempo,Collector.Collection.Number) %>% 
  summarise(promedio.numero.hojas= list(mean(Numero.hojas)),
            promedio.longitud.tallo = list(mean(Longitud.tallo))) %>% 
  unnest(cols = c(promedio.numero.hojas,promedio.longitud.tallo))


muestreo.1 <- 
  merge(promedio.crecimiento[promedio.crecimiento[,1]==11.4, 2:4],
      sobrevivientes[,1:2],
      by= "Collector.Collection.Number")
head(muestreo.1)
colnames(muestreo.1)
dim(muestreo.1)
muestreo.2 <- 
  merge(promedio.crecimiento[promedio.crecimiento[,1]==19.6, 2:4],
        sobrevivientes[,c(1,3)],
        by= "Collector.Collection.Number")
head(muestreo.2)
colnames(muestreo.2)
dim(muestreo.2)

muestreo.3 <- 
  merge(promedio.crecimiento[promedio.crecimiento[,1]==51.4, 2:4],
        sobrevivientes[,c(1,4)],
        by= "Collector.Collection.Number")
head(muestreo.3)
colnames(muestreo.3)
dim(muestreo.3)

#################################################################################################################
#################################################################################################################
# 3) Correlación
#################################################################################################################
#################################################################################################################
# 3.1) muestreo 1

#Shapiro test a cada variable

shapiro.test(muestreo.1$promedio.numero.hojas)
# Shapiro-Wilk normality test
# 
# data:  muestreo.1$promedio.numero.hojas
# W = 0.94689, p-value = 0.07664

shapiro.test(muestreo.1$promedio.longitud.tallo)
# Shapiro-Wilk normality test
# 
# data:  muestreo.1$promedio.longitud.tallo
# W = 0.94576, p-value = 0.0705

shapiro.test(muestreo.1$vivas.1)
# Shapiro-Wilk normality test
# 
# data:  muestreo.1$vivas.1
# W = 0.85316, p-value = 0.0001867

#correlación entre el número de hojas y longitud del tallo a 11.4 meses después de siembra
cor.test(muestreo.1$promedio.numero.hojas, muestreo.1$promedio.longitud.tallo)

# Pearson's product-moment correlation
# 
# data:  muestreo.1$promedio.numero.hojas and muestreo.1$promedio.longitud.tallo
# t = 3.954, df = 35, p-value = 0.0003565
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2825164 0.7454685
# sample estimates:
#       cor 
# 0.5556691

cor.test(muestreo.1$vivas.1, muestreo.1$promedio.numero.hojas, method = "spearman")

# Spearman's rank correlation rho
# 
# data:  muestreo.1$vivas.1 and muestreo.1$promedio.numero.hojas
# S = 4456.1, p-value = 0.003201
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.4717706 

cor.test(muestreo.1$vivas.1, muestreo.1$promedio.longitud.tallo, method = "spearman")
# Spearman's rank correlation rho
# 
# data:  muestreo.1$vivas.1 and muestreo.1$promedio.longitud.tallo
# S = 4803.9, p-value = 0.007814
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.4305423
#################################################################################################################
# 3.2) muestreo 2

#Shapiro test a cada variable

shapiro.test(muestreo.2$promedio.numero.hojas)
# Shapiro-Wilk normality test
# 
# data:  muestreo.2$promedio.numero.hojas
# W = 0.96209, p-value = 0.264

shapiro.test(muestreo.2$promedio.longitud.tallo)
# Shapiro-Wilk normality test
# 
# data:  muestreo.2$promedio.longitud.tallo
# W = 0.95579, p-value = 0.1705

shapiro.test(muestreo.2$vivas.2)
# Shapiro-Wilk normality test
# 
# data:  muestreo.2$vivas.2
# W = 0.86831, p-value = 0.0006176

#correlación entre el número de hojas y longitud del tallo a 19.6 meses después de siembra
cor.test(muestreo.2$promedio.numero.hojas, muestreo.2$promedio.longitud.tallo)

# Pearson's product-moment correlation
# 
# data:  muestreo.2$promedio.numero.hojas and muestreo.2$promedio.longitud.tallo
# t = 3.0471, df = 33, p-value = 0.004523
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.160386 0.693535
# sample estimates:
#       cor 
# 0.4685875 

cor.test(muestreo.2$vivas.2, muestreo.2$promedio.numero.hojas, method = "spearman")

# Spearman's rank correlation rho
# 
# data:  muestreo.2$vivas.2 and muestreo.2$promedio.numero.hojas
# S = 4945.5, p-value = 0.07249
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3073482 

cor.test(muestreo.2$vivas.2, muestreo.2$promedio.longitud.tallo, method = "spearman")

# Spearman's rank correlation rho
# 
# data:  muestreo.2$vivas.2 and muestreo.2$promedio.longitud.tallo
# S = 6445.3, p-value = 0.5782
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.09729654 
#################################################################################################################
# 3.3) muestreo 3

#Shapiro test a cada variable

shapiro.test(muestreo.3$promedio.numero.hojas)
# Shapiro-Wilk normality test
# 
# data:  muestreo.3$promedio.numero.hojas
# W = 0.90943, p-value = 0.009441

shapiro.test(muestreo.3$promedio.longitud.tallo)
# Shapiro-Wilk normality test
# 
# data:  muestreo.3$promedio.longitud.tallo
# W = 0.97147, p-value = 0.5603

shapiro.test(muestreo.3$vivas.3)
# Shapiro-Wilk normality test
# 
# data:  muestreo.3$vivas.3
# W = 0.84697, p-value = 0.0002922

#correlación entre el número de hojas y longitud del tallo a 19.6 meses después de siembra
cor.test(muestreo.3$promedio.numero.hojas, muestreo.3$promedio.longitud.tallo, method = "spearman")

# Spearman's rank correlation rho
# 
# data:  muestreo.3$promedio.numero.hojas and muestreo.3$promedio.longitud.tallo
# S = 1749.1, p-value = 8.28e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.6473506

cor.test(muestreo.3$vivas.3, muestreo.3$promedio.numero.hojas, method = "spearman")

# Spearman's rank correlation rho
# 
# data:  muestreo.3$vivas.3 and muestreo.3$promedio.numero.hojas
# S = 5040.9, p-value = 0.3811
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1576028  

cor.test(muestreo.3$vivas.3, muestreo.3$promedio.longitud.tallo, method = "spearman")

# Spearman's rank correlation rho
# 
# data:  muestreo.3$vivas.3 and muestreo.3$promedio.longitud.tallo
# S = 4109.1, p-value = 0.3561
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1715569 
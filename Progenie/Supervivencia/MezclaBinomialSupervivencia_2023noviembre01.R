#################################################################################################################
#################################################################################################################
#################################################################################################################
#
# INTRODUCCIÓN
#
# REQUERIMIENTOS
#
#
# CONTENIDO
#  1) 
#  2)
#  3)
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
library(flexmix)

#################################################################################################################
# 1.2) Lectura de los datos de supervivencia de las plantas de la progenie
#      directorio de trabajo.

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman

supervivencia <-
  read.table(
    "supervivencia_piloto_2023agosto22_082700.csv",
    header = T,
    sep = ","
  )

head(supervivencia)
colnames(supervivencia)
dim(supervivencia)#250 plantas de la progenie


#################################################################################################################
#################################################################################################################
# 2) Crear tabla con número de sobrevivientes por planta madre.
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 2.1)Remover plantas madres sin número de colección.

supervivencia$Número.colección.planta.madre <-
  as.numeric(supervivencia$Número.colección.planta.madre)
supervivencia <-
  supervivencia[!is.na(supervivencia$Número.colección.planta.madre), ]
dim(supervivencia) # 248 plantas de la progenie con madre asignada

#################################################################################################################
# 2.2) Crear tabla.

vivas.supervivencia.1 <- as.data.frame(table(supervivencia[, c(1, 6)]))
vivas.supervivencia.1 <-
  vivas.supervivencia.1[vivas.supervivencia.1$supervivencia.1 == "V", c(1, 3)]
vivas.supervivencia.2 <- as.data.frame(table(supervivencia[, c(1, 8)]))
vivas.supervivencia.2 <-
  vivas.supervivencia.2[vivas.supervivencia.2$supervivencia.2 == "V", c(1, 3)]
vivas.supervivencia.3 <- as.data.frame(table(supervivencia[, c(1, 10)]))
vivas.supervivencia.3 <-
  vivas.supervivencia.3[vivas.supervivencia.3$supervivencia.3 == "V", c(1, 3)]

sobrevivientes<-data.frame(vivas.supervivencia.1,
                           vivas.supervivencia.2[,2],
                           vivas.supervivencia.3[,2])
View(sobrevivientes)
colnames(sobrevivientes) <-
  c(
    "Collector.Collection.Number",
    "vivas.1",
    "vivas.2",
    "vivas.3"
  )
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
sobrevivientes.all<-
  merge(x=phenotypic.group.assignment.madres[,c(2,3)],
      y=sobrevivientes, 
      all=T, row.names=NULL)
sobrevivientes.all[is.na(sobrevivientes.all)]<-0
sobrevivientes.all<-sobrevivientes.all[-2]
class(sobrevivientes.all)
dim(sobrevivientes.all)
head(sobrevivientes.all)


#################################################################################################################
#################################################################################################################
# 3) Ajustar modelos de mezclas binomiales.
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 3.1) primera medición: 11.4 meses después de la siembra.

vivas1 <- sobrevivientes.all$vivas.1
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}
mode(vivas1)
max(vivas1)
mean(vivas1)#5.511628
sd(vivas1) # 5.856913
sum(vivas1)
Conc  <- FLXPmultinom(~1)
Mod.fam <- FLXglm(~1, family="binomial")
siembra <- 100 # number of trials of the binomial components (max number of credits)
  Modelos1 <- stepFlexmix(cbind(vivas1, siembra - vivas1)~1, model=Mod.fam, k=1:6, concomitant=Conc, nrep=5)
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
getModel(Modelos1, which= "BIC")
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
parameters(MejorModelo1, component=1)
exp(parameters(MejorModelo1)[1])/(1+exp(parameters(MejorModelo1)[1]))
exp(parameters(MejorModelo1)[2])/(1+exp(parameters(MejorModelo1)[2]))
exp(parameters(MejorModelo1)[3])/(1+exp(parameters(MejorModelo1)[3]))

MejorModelo1@prior #probabilidad previa
MejorModelo1@posterior #probabilidad posterior
MejorModelo1@cluster #asignación grupo


 asignacion.grupo.fenotipico.supervivencia.1 <-
     data.frame(sobrevivientes.all[, c(1,2)],
                               MejorModelo1@cluster)
 colnames(asignacion.grupo.fenotipico.supervivencia.1) <-
     c(
         "Collector.Collection.Number",
         "Vivas.1",
         "Phenotypic.Group"
       )
 
head(asignacion.grupo.fenotipico.supervivencia.1)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#     asignacion.grupo.fenotipico.supervivencia.1,
#     file = paste(
#       "asignacion.grupo.fenotipico.supervivencia.1_",
#       format(Sys.time(), "%Y%B%d_%H%M%S"),
#       ".csv",
#       sep = ""
#          ),
#       row.names = F
#     )

#################################################################################################################
# 3.2) segunda medición: 19.6 meses después de la siembra.
vivas2 <- sobrevivientes.all$vivas.2
mode(vivas2)# 0
max(vivas2)
sum(vivas2)#221 plantas hijas
mean(vivas2)#5.511628
sd(vivas2) # 5.856913
Conc  <- FLXPmultinom(~1)
Mod.fam <- FLXglm(~1, family="binomial")
siembra <- 100 # number of trials of the binomial components (max number of credits)
Modelos2 <- stepFlexmix(cbind(vivas2, siembra - vivas2)~1, model=Mod.fam, k=1:6, concomitant=Conc, nrep=5)
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
getModel(Modelos2, which="BIC")
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
parameters(MejorModelo2, component=1)
exp(parameters(MejorModelo2)[1])/(1+exp(parameters(MejorModelo2)[1]))
exp(parameters(MejorModelo2)[2])/(1+exp(parameters(MejorModelo2)[2]))
exp(parameters(MejorModelo2)[3])/(1+exp(parameters(MejorModelo2)[3]))

MejorModelo2@prior #probabilidad previa
MejorModelo2@posterior #probabilidad posterior
MejorModelo2@cluster #asignación grupo


asignacion.grupo.fenotipico.supervivencia.2 <-
  data.frame(sobrevivientes.all[, c(1,3)],
             MejorModelo2@cluster)
colnames(asignacion.grupo.fenotipico.supervivencia.2) <-
  c(
    "Collector.Collection.Number",
    "Vivas.2",
    "Phenotypic.Group"
  )

head(asignacion.grupo.fenotipico.supervivencia.2)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#   asignacion.grupo.fenotipico.supervivencia.2,
#   file = paste(
#     "asignacion.grupo.fenotipico.supervivencia.2_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#################################################################################################################
# 3.3) Tercera medición: 51.4 meses después de la siembra.

vivas3 <- sobrevivientes.all$vivas.3
mode(vivas3)# 0
max(vivas3)
sum(vivas3)#175 plantas hijas
mean(vivas3)#4.069767
sd(vivas3) # 4.86188
Conc  <- FLXPmultinom(~1)
Mod.fam <- FLXglm(~1, family="binomial")
siembra <- 100 # number of trials of the binomial components (max number of credits)
Modelos3 <- stepFlexmix(cbind(vivas3, siembra - vivas3)~1, model=Mod.fam, k=1:6, concomitant=Conc, nrep=5)
show(Modelos3)
plot(Modelos3)

# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam, 
#               concomitant = Conc, k = 1:6, nrep = 5)
# 
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -166.7607 335.5213 337.2825 337.2825
# 2   15      TRUE 2  2 -113.5369 233.0737 238.3573 241.8677
# 3   25      TRUE 3  3 -108.8032 227.6063 236.4124 246.1706
# 4  127      TRUE 3  4 -108.8033 227.6065 236.4125 246.1608
# 5  129      TRUE 3  5 -108.8032 227.6064 236.4124 246.1647
# 6  108      TRUE 4  6 -108.8027 231.6055 243.9339 278.2066

sort(BIC(Modelos3))# 3 Modelos con igual apoyo empírico: k=3,5,4,2
#       3        5        4        2        6        1 
# 236.4124 236.4124 236.4125 238.3573 243.9339 337.2825 

#then we keep the best model in terms of BIC
Modelo3.3  <- getModel(Modelos3, 3)
summary(Modelo3.3)
# Call:
#   stepFlexmix(cbind(vivas3, siembra - vivas3) ~ 1, model = Mod.fam, 
#               concomitant = Conc, k = 3, nrep = 5)
# 
# prior size post>0 ratio
# Comp.1 0.335   14     42 0.333
# Comp.2 0.514   22     35 0.629
# Comp.3 0.151    7     24 0.292
# 
# 'log Lik.' -108.8026 (df=5)
# AIC: 227.6052   BIC: 236.4112 
Modelo3.3@df #número de parámetros en el modelo
str(Modelo3.3)

plot(Modelo3.3)
BIC(Modelo3.3)


# parámetro binomial para cada componente
fitted(Modelo3.3)
p.Modelo3.3 <- fitted(Modelo3.3)[1,]
#otra forma de obtener el parámetro binomial para cada componente
Modelo3.3@components ##parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.3) #parámetro binomial en escala del logaritmo del cociente de probabilidades: log(p/(1-p)
parameters(Modelo3.3, component=1)
exp(parameters(Modelo3.3)[1])/(1+exp(parameters(Modelo3.3)[1]))
exp(parameters(Modelo3.3)[2])/(1+exp(parameters(Modelo3.3)[2]))
exp(parameters(Modelo3.3)[3])/(1+exp(parameters(Modelo3.3)[3]))

Modelo3.3@prior #probabilidad previa
Modelo3.3@posterior #probabilidad posterior
Modelo3.3@cluster #asignación grupo

asignacion.grupo.fenotipico.supervivencia.3.3 <-
  data.frame(sobrevivientes.all[, c(1,4)],
             Modelo3.3@cluster)
colnames(asignacion.grupo.fenotipico.supervivencia.3.3) <-
  c(
    "Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group"
  )

head(asignacion.grupo.fenotipico.supervivencia.3.3)

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
parameters(Modelo3.5, component=1)
exp(parameters(Modelo3.5)[1])/(1+exp(parameters(Modelo3.5)[1]))
exp(parameters(Modelo3.5)[2])/(1+exp(parameters(Modelo3.5)[2]))
exp(parameters(Modelo3.5)[3])/(1+exp(parameters(Modelo3.5)[3]))

Modelo3.5@prior #probabilidad previa
Modelo3.5@posterior #probabilidad posterior
Modelo3.5@cluster #asignación grupo

table(Modelo3.3@cluster,Modelo3.5@cluster)# modelos con igual asignación de grupos
#    1  2  3
# 1  0 14  0
# 2  0  0 22
# 3  7  0  0

asignacion.grupo.fenotipico.supervivencia.3.5 <-
  data.frame(sobrevivientes.all[, c(1,4)],
             Modelo3.5@cluster)
colnames(asignacion.grupo.fenotipico.supervivencia.3.5) <-
  c(
    "Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group"
  )
head(asignacion.grupo.fenotipico.supervivencia.3.5)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#   asignacion.grupo.fenotipico.supervivencia.3.3,
#   file = paste(
#     "asignacion.grupo.fenotipico.supervivencia.3.3_",
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
parameters(Modelo3.4, component=1)
exp(parameters(Modelo3.4)[1])/(1+exp(parameters(Modelo3.4)[1]))
exp(parameters(Modelo3.4)[2])/(1+exp(parameters(Modelo3.4)[2]))
exp(parameters(Modelo3.4)[3])/(1+exp(parameters(Modelo3.4)[3]))

Modelo3.4@prior #probabilidad previa
Modelo3.4@posterior #probabilidad posterior
Modelo3.4@cluster #asignación grupo

asignacion.grupo.fenotipico.supervivencia.3.4 <-
  data.frame(sobrevivientes.all[, c(1,4)],
             Modelo3.4@cluster)
colnames(asignacion.grupo.fenotipico.supervivencia.3.4) <-
  c(
    "Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group"
  )
head(asignacion.grupo.fenotipico.supervivencia.3.4)

table(Modelo3.3@cluster,Modelo3.4@cluster)
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
parameters(Modelo3.2, component=1)
exp(parameters(Modelo3.2)[1])/(1+exp(parameters(Modelo3.2)[1]))
exp(parameters(Modelo3.2)[2])/(1+exp(parameters(Modelo3.2)[2]))
exp(parameters(Modelo3.2)[3])/(1+exp(parameters(Modelo3.2)[3]))

Modelo3.2@prior #probabilidad previa
Modelo3.2@posterior #probabilidad posterior
Modelo3.2@cluster #asignación grupo

table(Modelo3.2@cluster, Modelo3.3@cluster)
#    1  2  3
# 1  7 22  0
# 2  7  0  7

asignacion.grupo.fenotipico.supervivencia.3.2 <-
  data.frame(sobrevivientes.all[, c(1,4)],
             Modelo3.2@cluster)
colnames(asignacion.grupo.fenotipico.supervivencia.3.2) <-
  c(
    "Collector.Collection.Number",
    "Vivas.3",
    "Phenotypic.Group"
  )
head(asignacion.grupo.fenotipico.supervivencia.3.2)

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia/data")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Papers/EspeletiaSumapazCommonGarden/Data/Supervivencia") #Ivan's working directory Waterman
# write.csv(
#   asignacion.grupo.fenotipico.supervivencia.3.2,
#   file = paste(
#     "asignacion.grupo.fenotipico.supervivencia.3.2_",
#     format(Sys.time(), "%Y%B%d_%H%M%S"),
#     ".csv",
#     sep = ""
#   ),
#   row.names = F
# )

#################################################################################################################
#################################################################################################################
# 4) Tablas de clasificación cruzadas entre modelos de supervivencia y entre morfología
#################################################################################################################
#################################################################################################################

#################################################################################################################
# 4.1) 11.6 meses después de la siembra vs. 19.6 meses después de la siembra
asignacion.grupo.fenotipico.supervivencia.1.vs.2 <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.1[, c(1, 3)],
    asignacion.grupo.fenotipico.supervivencia.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    all=T,
    suffixes = c(".11.6", ".19.6")
  )
table(asignacion.grupo.fenotipico.supervivencia.1.vs.2[,3],
      asignacion.grupo.fenotipico.supervivencia.1.vs.2[,2])
#    1  2  3
# 1  0  0 19
# 2  5  0  0
# 3  0 18  1

#################################################################################################################
# 4.2) 19.6 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 1)
asignacion.grupo.fenotipico.supervivencia.2.vs.3.3 <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.2[, c(1, 3)],
    asignacion.grupo.fenotipico.supervivencia.3.3[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".51.4")
  )
table(asignacion.grupo.fenotipico.supervivencia.2.vs.3.3[,3],
      asignacion.grupo.fenotipico.supervivencia.2.vs.3.3[,2])
#    1  2  3
# 1  0  0 14
# 2 19  0  3
# 3  0  5  2

# 19.6 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 2)
asignacion.grupo.fenotipico.supervivencia.2.vs.3.2 <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.2[, c(1, 3)],
    asignacion.grupo.fenotipico.supervivencia.3.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".51.4")
  )
table(asignacion.grupo.fenotipico.supervivencia.2.vs.3.2[,2],
      asignacion.grupo.fenotipico.supervivencia.2.vs.3.2[,3])
#    1  2
# 1  5  0
# 2  0 19
# 3  9 10

#################################################################################################################
# 4.3) 11.4 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 1)

asignacion.grupo.fenotipico.supervivencia.1.vs.3.3 <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.1[, c(1, 3)],
    asignacion.grupo.fenotipico.supervivencia.3.3[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".51.4")
  )
table(asignacion.grupo.fenotipico.supervivencia.1.vs.3.3[,2],
      asignacion.grupo.fenotipico.supervivencia.1.vs.3.3[,3])
# 1  2  3
# 1  0  0  5
# 2 14  2  2
# 3  0 20  0

# 11.4 meses después de la siembra vs. 51.4 meses después de la siembra (modelo 2)

asignacion.grupo.fenotipico.supervivencia.1.vs.3.2 <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.1[, c(1, 3)],
    asignacion.grupo.fenotipico.supervivencia.3.2[, c(1, 3)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".51.4")
  )
table(asignacion.grupo.fenotipico.supervivencia.1.vs.3.2[,2],
      asignacion.grupo.fenotipico.supervivencia.1.vs.3.2[,3])
# 1  2
# 1  9  9
# 2  5  0
# 3  0 20

#################################################################################################################
# 4.4) 11.4 meses después de la siembra vs. morfología

asignacion.grupo.fenotipico.supervivencia.1.vs.morfologia <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.1[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".11.4", ".madre")
  )

table(asignacion.grupo.fenotipico.supervivencia.1.vs.morfologia[,2],
      asignacion.grupo.fenotipico.supervivencia.1.vs.morfologia[,3])
#   2  3  4  5
# 1  0  1  2  2
# 2  4 10  0  4
# 3  2  7  3  8

#################################################################################################################
# 4.5) 19.6 meses después de la siembra vs. morfología

asignacion.grupo.fenotipico.supervivencia.2.vs.morfologia <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.2[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".madre")
  )

table(asignacion.grupo.fenotipico.supervivencia.2.vs.morfologia[,2],
      asignacion.grupo.fenotipico.supervivencia.2.vs.morfologia[,3])
# 2  3  4  5
# 1  2  7  2  8
# 2  0  1  2  2
# 3  4 10  1  4

#################################################################################################################
# 4.6) 51.4 meses después de la siembra (modelo 1) vs. morfología

asignacion.grupo.fenotipico.supervivencia.3.3.vs.morfologia <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.3.3[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".madre")
  )

table(asignacion.grupo.fenotipico.supervivencia.3.3.vs.morfologia[,2],
      asignacion.grupo.fenotipico.supervivencia.3.3.vs.morfologia[,3])
# 2 3 4 5
# 1 3 7 0 4
# 2 2 9 3 8
# 3 1 2 2 2

#################################################################################################################
# 4.7) 51.4 meses después de la siembra (modelo 2) vs. morfología

asignacion.grupo.fenotipico.supervivencia.3.2.vs.morfologia <-
  merge(
    asignacion.grupo.fenotipico.supervivencia.3.2[, c(1, 3)],
    phenotypic.group.assignment.madres[, c(2, 6)],
    by = "Collector.Collection.Number",
    suffixes = c(".19.6", ".madre")
  )

table(asignacion.grupo.fenotipico.supervivencia.3.2.vs.morfologia[,2],
      asignacion.grupo.fenotipico.supervivencia.3.2.vs.morfologia[,3])

#    2  3  4  5
# 1  3  4  2  5
# 2  3 14  3  9

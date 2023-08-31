###################################################################################################################
###################################################################################################################
###################################################################################################################
#INTRODUCCIÓN
# A partir de proporciones de supervivencias por planta madre de las tres medidas que se han realizado a la progenie
#del piloto, se realiza un agrupamiento de la tasa de supervivencia de la progenie de los 43 plantas sembradas en el
#piloto

#REQUERIMIENTOS
#Tabla de datos de la supervivencia de las medidas tomadas en marzo y octubre de 202 y junio de 2023:
#     "supervivencia_piloto_2023agosto22_082700.csv"
###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
# 1) Datos preliminares: Carga de librerías y lectura de datos
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 1.1) Librerías:

library(mclust) # librería para adaptar modelos de mezclas normales
library(clustvarsel) #librería para la selección de variables para el modelos de mexclas normales
library(ellipse)
###################################################################################################################
# 1.2) lectura de los datos de supervivencia de las plantas de la progenie
#directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman

#leer las tablas de datos, examinar y resumir los datos

supervivencia <- read.table("supervivencia_piloto_2023agosto22_082700.csv",header = T, sep=",")

head(supervivencia)
colnames(supervivencia)
dim(supervivencia) #250 plantas de la progenie

###################################################################################################################
###################################################################################################################
# 2) crear tabla con proporción de tasa de supervivencia
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 2.1)Remover plantas madres sin número de colección

supervivencia$Número.colección.planta.madre<-as.numeric(supervivencia$Número.colección.planta.madre)
supervivencia<-supervivencia[!is.na(supervivencia$Número.colección.planta.madre),]
dim(supervivencia) # 248 plantas de la progenie con madre asignada

###################################################################################################################
# 2.2) Crear tabla de proporción de supervivencia

nacimiento<-as.data.frame(table(supervivencia[,1]))
vivas.supervivencia.1<-as.data.frame(table(supervivencia[,c(1,6)]))
vivas.supervivencia.1<-vivas.supervivencia.1[vivas.supervivencia.1$supervivencia.1=="V",c(1,3)]
vivas.supervivencia.2<-as.data.frame(table(supervivencia[,c(1,8)]))
vivas.supervivencia.2<-vivas.supervivencia.2[vivas.supervivencia.2$supervivencia.2=="V",c(1,3)]
vivas.supervivencia.3<-as.data.frame(table(supervivencia[,c(1,10)]))
vivas.supervivencia.3<-vivas.supervivencia.3[vivas.supervivencia.3$supervivencia.3=="V",c(1,3)]

proporcion.supervivencia<-data.frame(nacimiento[,1],
                                     vivas.supervivencia.1[,2]/nacimiento[,2],
                                     vivas.supervivencia.2[,2]/nacimiento[,2],
                                     vivas.supervivencia.3[,2]/nacimiento[,2])
colnames(proporcion.supervivencia)<-c("Número.colección.planta.madre","proporcion.supervivencia.1",
                                      "proporcion.supervivencia.2","proporcion.supervivencia.3")
###################################################################################################################
###################################################################################################################
# 3) examinar tasa de supervivencia
###################################################################################################################
###################################################################################################################
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")
#pdf("Figuras_supervivencia.pdf")
summary(proporcion.supervivencia)
dim(proporcion.supervivencia) #37 plantas madres representadas
proporcion.supervivencia$Número.colección.planta.madre<-as.factor(proporcion.supervivencia$Número.colección.planta.madre)
sapply(proporcion.supervivencia[,c(2:4)], mean)
# proporcion.supervivencia.1 proporcion.supervivencia.2 proporcion.supervivencia.3 
# 0.9509240                  0.8703254                  0.6488025 
barplot(sapply(proporcion.supervivencia[,c(2:4)], mean), xlab = "medidas", ylab="promedio proporción supervivencia")


barplot(
  cbind(proporcion.supervivencia.1,proporcion.supervivencia.2,proporcion.supervivencia.3)~Número.colección.planta.madre, 
        data = proporcion.supervivencia, beside=T, las=2, ylab = "proporción de supervivencia", 
  col=c("lightblue", "mistyrose", "lightcyan"), legend.text=c("supervivencia 1", "supervivencia 2", "Supervivencia 3"),
  args.legend=list(x="top", bty = "n",horiz=TRUE, inset=-0.15) )

###################################################################################################################
###################################################################################################################
# 4) Ajuste de modelos de mezclas normales
###################################################################################################################
###################################################################################################################

#subset con sólo las proporciones de supervivencia
data.for.GMM<-proporcion.supervivencia[,2:4]
###################################################################################################################
# 4.1) Ajuste de mezclas normales usando diferentes valores de inicialización, usando el argument "hcUse"

#"PCS"
mclust.options(hcUse="PCS") 
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examinando resultados
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificación de lso especímenes
Mcluster.supervivencia.piloto$uncertainty # incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 5 components: 
#   
#   log-likelihood  n   df      BIC      ICL
#         200.8311 37   37      268.0583 255.7391
# 
# Clustering table:
#   1  2  3  4  5 
#   3  3  5 24  2

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#"VARS"
mclust.options(hcUse="VARS") 
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #classifiación de los especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)
#setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#save(Mcluster.supervivencia.piloto, file="Mcluster.supervivencia.piloto_2023August30.RData")
#load("Mcluster.supervivencia.piloto_2023August30.RData")

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 5 components: 
#   
#   log-likelihood  n   df      BIC      ICL
#   200.8311        37  37  268.0583   255.7391
# 
# Clustering table:
#   1  2  3  4  5 
#   3  3  5 24  2
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#"STD"
mclust.options(hcUse="STD") 
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)#
Mcluster.supervivencia.piloto$classification #clasificación de los especíemenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 5 components: 
#   
#   log-likelihood  n   df      BIC      ICL
#   200.8311        37  37    268.0583   255.7391
# 
# Clustering table:
#  1  2  3  4  5 
#  3  3  5 24  2

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#"SPH"
mclust.options(hcUse="SPH") 
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificación de los especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 8 components: 
#   
#   log-likelihood  n df      BIC      ICL
#   265.2543        37 58   321.0754 309.1267
# 
# Clustering table:
#   1  2  3  4  5  6  7  8 
#   1  6  3  9 10  4  3  1
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#"PCR"
mclust.options(hcUse="PCR") 
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificacion de los especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 4 components: 
#   
#   log-likelihood  n   df      BIC       ICL
#   213.1409       37    30    317.9543   317.6814
# 
# Clustering table:
#   1  2  3  4 
#   3  5  3 26 

#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#"SVD"
mclust.options(hcUse="SVD") 
Mcluster.supervivencia.piloto <- Mclust(data.for.GMM)
#examine results
Mcluster.supervivencia.piloto
summary(Mcluster.supervivencia.piloto)
names(Mcluster.supervivencia.piloto$classification)
Mcluster.supervivencia.piloto$classification #clasificación de os especímenes
Mcluster.supervivencia.piloto$uncertainty #incertidumbre de la clasificación
attributes(Mcluster.supervivencia.piloto)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EEV (ellipsoidal, equal volume and shape) model with 4 components: 
#   
#   log-likelihood  n df      BIC    ICL
#     221.3889      37 30   334.4502 334.31
# 
# Clustering table:
#  1 1  2  3  4 
#    2 25  4  6 
 
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#dev.off()

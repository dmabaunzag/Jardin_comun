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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
save(proporcion.supervivencia, file=paste("proporcion.supervivencia_",
                                                      format(Sys.time(),"%Y%B%d_%H%M%S"), ".RData", sep=""))
load("proporcion.supervivencia_2023septiembre04_095936.RData")

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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")#Directorio de Diana
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #Ivan's working directory Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
load("proporcion.supervivencia_2023septiembre04_095936.RData")
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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.supervivencia.piloto, file="Mcluster.supervivencia.piloto.VARS_2023September04.RData")
#load("Mcluster.supervivencia.piloto.VARS_2023September04.RData")

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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.supervivencia.piloto, file="Mcluster.supervivencia.piloto.SPH_2023September04.RData")
#load("Mcluster.supervivencia.piloto.SPH_2023September04.RData")

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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.supervivencia.piloto, file="Mcluster.supervivencia.piloto.PCR_2023September04.RData")
#load("Mcluster.supervivencia.piloto.PCR_2023September04.RData")

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
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
save(Mcluster.supervivencia.piloto, file="Mcluster.supervivencia.piloto.SVD_2023September04.RData")
#load("Mcluster.supervivencia.piloto.SVD_2023September04.RData")

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
#   1  2  3  4 
#   2 25  4  6 
 
#gráficas de los morfogrupos, de acuerdo con el mejor modelo
plot(Mcluster.supervivencia.piloto, what="classification", dimens=c(1,2))
#gráfica del soporte empríco de los diferentess modelos
plot(Mcluster.supervivencia.piloto, what="BIC")

#dev.off()

###################################################################################################################
###################################################################################################################
# 5) Examinar los grupos fenotípicos en base al mejor modelo de mezclas normales, de accuerdo con las variables de 
#inicialización
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 5.1)VARS
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.supervivencia.piloto.VARS_2023September04.RData")#modelos VARS
load("proporcion.supervivencia_2023septiembre04_095936.RData")
###################################################################################################################
# 5.1.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.supervivencia <- data.frame(as.numeric(rownames(proporcion.supervivencia)),
                                                 proporcion.supervivencia[,1], 
                                                 Mcluster.supervivencia.piloto$classification, Mcluster.supervivencia.piloto$uncertainty)
colnames(phenotypic.group.assignment.supervivencia) <- 
  c("Rownames.Meanphenodata", "Collector.Collection.Number", "Phenotypic.Group", "Uncertainty")
head(phenotypic.group.assignment.supervivencia)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(phenotypic.group.assignment.supervivencia, 
          file=paste("PhenotypicGroupAssignment.supervivencia.VARS_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)
###################################################################################################################
# 5.1.2) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
# pdf("Figuras(supervivencia)_sección_5.1(VARS).pdf")

#Supervivencia.1 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
#                                [,1]      [,2]      [,3]      [,4]      [,5]
# proporcion.supervivencia.1 0.6106961 0.9903580 0.8868995 1.0000000 1.0000000
# proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
text(0.61, 0.17, "G1")
text(0.99, 0.99, "G2")
text(0.89, 0.81, "G3")
text(1, 0.94, "G4")
text(1, 1, "G5")

#Supervivencia.3 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
#                                [,1]      [,2]      [,3]      [,4]      [,5]
# proporcion.supervivencia.3 0.1668445 0.7452894 0.5697141 0.7722337 0.0000000
# proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
text(0.17, 0.17, "G1")
text(0.75, 0.99, "G2")
text(0.57, 0.81, "G3")
text(0.77, 0.94, "G4")
text(0, 1, "G5")
###################################################################################################################
# 5.1.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.supervivencia.piloto$uncertainty)

#De los 37 especímenes, 19 de éstos tiene incertidumbre mayor a 0.1, es decir el 51.3%
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)/length(Mcluster.supervivencia.piloto$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.supervivencia.piloto$classification[Mcluster.supervivencia.piloto$uncertainty>0.1]


#Gráfica de la función de distribución acumulativa de valores de incertidumbre
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(sort(Mcluster.supervivencia.piloto$uncertainty),
     1:length(Mcluster.supervivencia.piloto$uncertainty)/length(Mcluster.supervivencia.piloto$uncertainty),
     xlim=c(0,0.5), bty="n", xlab="Incertidumbre", ylab="F (Incertidumbre)",
     type="l", pch=19, cex=0.5, cex.axis=1.5, cex.lab=1.5)
abline(h=c(0,1), lty=3, col="gray70")

#Supervivencia 1 vs supervivencia 2
# gráfica de los especímenes con incertidumbre > 0.1
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F, cex=0)
#Agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
#                                [,1]      [,2]      [,3]      [,4]      [,5]
# proporcion.supervivencia.1 0.6106961 0.9903580 0.8868995 1.0000000 1.0000000
# proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
text(0.61, 0.17, "G1")
text(0.99, 0.99, "G2")
text(0.89, 0.81, "G3")
text(1, 0.94, "G4")
text(1, 1, "G5")
for(i in 1:Mcluster.supervivencia.piloto$G){
  points(Mcluster.supervivencia.piloto$data[
    Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,1:2],
    pch=mclust.options("classPlotSymbols")[i],
    col=mclust.options("classPlotColors")[i])
}
legend("topleft", paste("G", c(2,4)),
       col=mclust.options("classPlotColors")[c(2,4)],
       pch=mclust.options("classPlotSymbols")[c(2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")

#Supervivencia 3 vs supervivencia 2
# gráfica de los especímenes con incertidumbre > 0.1
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F, cex=0)
#Agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
         type="l", col="black")
}

#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
#                                [,1]      [,2]      [,3]      [,4]      [,5]
# proporcion.supervivencia.3 0.1668445 0.7452894 0.5697141 0.7722337 0.0000000
# proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
text(0.17, 0.17, "G1")
text(0.75, 0.99, "G2")
text(0.57, 0.81, "G3")
text(0.77, 0.94, "G4")
text(0, 1, "G5")
for(i in 1:Mcluster.supervivencia.piloto$G){
  points(Mcluster.supervivencia.piloto$data[
    Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,2:3],
    pch=mclust.options("classPlotSymbols")[i],
    col=mclust.options("classPlotColors")[i])
}
legend("bottomright", paste("G", c(2,4)),
       col=mclust.options("classPlotColors")[c(2,4)],
       pch=mclust.options("classPlotSymbols")[c(2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
#dev.off()
###################################################################################################################
# 5.1.4) Examinar la tabulación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos fenotípicos 
#según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment<-read.table("PhenotypicGroupAssignment_2023agosto19_072633.csv",header=T, sep=",")
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en junio de 2023
phenotypic.group.assignment.progenie<-read.table("PhenotypicGroupAssignment.supervivencia.VARS_2023septiembre04_103037.csv",
                                                 header=T, sep=",")
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres<-phenotypic.group.assignment[308:350,]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number<-as.numeric(substring(phenotypic.group.assignment.madres$Collector.Collection.Number,5))
phenotypic.group.assignment.madres$Phenotypic.Group<-as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(phenotypic.group.assignment.madres$Collector.Collection.Number, 
       phenotypic.group.assignment.madres$Phenotypic.Group,length)
# 2  3  4  5 
# 6 18  5 14  

tapply(phenotypic.group.assignment.progenie$Collector.Collection.Number, 
       phenotypic.group.assignment.progenie$Phenotypic.Group,length)
# 1  2  3  4  5 
# 3  3  5 24  2   

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
phenotypic.group.assignment.madres.progenie<-merge(phenotypic.group.assignment.madres[,c(2,3)], phenotypic.group.assignment.progenie[,c(2,3)],
                                                   by="Collector.Collection.Number", suffixes = c(".madres",".progenie"))


phenotypic.group.crosstab<-table(phenotypic.group.assignment.madres.progenie[,c(2,3)])
phenotypic.group.crosstab
#                         Phenotypic.Group.progenie
# Phenotypic.Group.madres 1 2 3 4 5
                      # 2 1 0 0 5 0
                      # 3 0 1 4 9 1
                      # 4 1 2 0 2 0
                      # 5 1 0 1 8 1
###################################################################################################################
###################################################################################################################

# 5.2)SPH
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.supervivencia.piloto.SPH_2023September04.RData")#modelos VARS
load("proporcion.supervivencia_2023septiembre04_095936.RData")
###################################################################################################################
# 5.2.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.supervivencia <- data.frame(as.numeric(rownames(proporcion.supervivencia)),
                                                        proporcion.supervivencia[,1], 
                                                        Mcluster.supervivencia.piloto$classification, Mcluster.supervivencia.piloto$uncertainty)
colnames(phenotypic.group.assignment.supervivencia) <- 
  c("Rownames.Meanphenodata", "Collector.Collection.Number", "Phenotypic.Group", "Uncertainty")
head(phenotypic.group.assignment.supervivencia)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(phenotypic.group.assignment.supervivencia, 
          file=paste("PhenotypicGroupAssignment.supervivencia.SPH_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)
###################################################################################################################
# 5.2.2) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
# pdf("Figuras(supervivencia)_sección_5.2(SPH).pdf")

#Supervivencia.1 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F)
legend("topleft", paste("G", 1:8), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
#                                [,1]      [,2]      [,3] [,4]      [,5]      [,6]
# proporcion.supervivencia.1 0.9999203 1.0000000 0.7916407    1 0.9869843 0.8881944
# proporcion.supervivencia.2 0.0000000 0.8746094 0.5972136    1 0.9617223 0.8569444
#                             [,7]         [,8]
# proporcion.supervivencia.1    1 3.333434e-01
# proporcion.supervivencia.2    1 3.027634e-05
text(0.9999203, 0, "G1")
text(1, 0.8746094, "G2")
text(0.7916407, 0.5972136, "G3")
text(1, 1, "G4")
text(0.9869843, 0.9617223, "G5")
text(0.8881944, 0.8569444, "G6")
text(1, 1, "G7")
text(3.333434e-01, 3.027634e-05, "G8")

#Supervivencia.3 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:5), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
#                             [,1]      [,2]      [,3]     [,4]      [,5]      [,6]
# proporcion.supervivencia.3    0 0.6655295 0.4027864 0.794026 0.9552144 0.6055555
# proporcion.supervivencia.2    0 0.8746094 0.5972136 1.000000 0.9617223 0.8569444
                            # [,7]         [,8]
# proporcion.supervivencia.3 0.1084394 3.027634e-05
# proporcion.supervivencia.2 1.0000000 3.027634e-05
text(0, 0, "G1")
text(0.6655295, 0.8746094, "G2")
text(0.4027864, 0.5972136, "G3")
text(0.794026, 1, "G4")
text(0.9552144, 0.9617223, "G5")
text(0.6055555, 0.8569444, "G6")
text(0.1084394, 1, "G7")
text(3.027634e-05, 3.027634e-05, "G8")
###################################################################################################################
# 5.2.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.supervivencia.piloto$uncertainty)

#De los 37 especímenes, 9 de éstos tiene incertidumbre mayor a 0.1, es decir el 24.3%
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)/length(Mcluster.supervivencia.piloto$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.supervivencia.piloto$classification[Mcluster.supervivencia.piloto$uncertainty>0.1]


#Gráfica de la función de distribución acumulativa de valores de incertidumbre
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(sort(Mcluster.supervivencia.piloto$uncertainty),
     1:length(Mcluster.supervivencia.piloto$uncertainty)/length(Mcluster.supervivencia.piloto$uncertainty),
     xlim=c(0,0.5), bty="n", xlab="Incertidumbre", ylab="F (Incertidumbre)",
     type="l", pch=19, cex=0.5, cex.axis=1.5, cex.lab=1.5)
abline(h=c(0,1), lty=3, col="gray70")

#Supervivencia 1 vs supervivencia 2
# gráfica de los especímenes con incertidumbre > 0.1
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F, cex=0)
#Agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
#                                [,1]      [,2]      [,3] [,4]      [,5]      [,6]
# proporcion.supervivencia.1 0.9999203 1.0000000 0.7916407    1 0.9869843 0.8881944
# proporcion.supervivencia.2 0.0000000 0.8746094 0.5972136    1 0.9617223 0.8569444
#                             [,7]         [,8]
# proporcion.supervivencia.1    1 3.333434e-01
# proporcion.supervivencia.2    1 3.027634e-05
text(0.9999203, 0, "G1")
text(1, 0.8746094, "G2")
text(0.7916407, 0.5972136, "G3")
text(1, 1, "G4")
text(0.9869843, 0.9617223, "G5")
text(0.8881944, 0.8569444, "G6")
text(1, 1, "G7")
text(3.333434e-01, 3.027634e-05, "G8")
for(i in 1:Mcluster.supervivencia.piloto$G){
  points(Mcluster.supervivencia.piloto$data[
    Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,1:2],
    pch=mclust.options("classPlotSymbols")[i],
    col=mclust.options("classPlotColors")[i])
}
legend("topleft", paste("G", c(5,7)),
       col=mclust.options("classPlotColors")[c(5,7)],
       pch=mclust.options("classPlotSymbols")[c(5,7)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")

#Supervivencia 3 vs supervivencia 2
# gráfica de los especímenes con incertidumbre > 0.1
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F, cex=0)
#Agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
         type="l", col="black")
}

#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
#                             [,1]      [,2]      [,3]     [,4]      [,5]      [,6]
# proporcion.supervivencia.3    0 0.6655295 0.4027864 0.794026 0.9552144 0.6055555
# proporcion.supervivencia.2    0 0.8746094 0.5972136 1.000000 0.9617223 0.8569444
# [,7]         [,8]
# proporcion.supervivencia.3 0.1084394 3.027634e-05
# proporcion.supervivencia.2 1.0000000 3.027634e-05
text(0, 0, "G1")
text(0.6655295, 0.8746094, "G2")
text(0.4027864, 0.5972136, "G3")
text(0.794026, 1, "G4")
text(0.9552144, 0.9617223, "G5")
text(0.6055555, 0.8569444, "G6")
text(0.1084394, 1, "G7")
text(3.027634e-05, 3.027634e-05, "G8")
for(i in 1:Mcluster.supervivencia.piloto$G){
  points(Mcluster.supervivencia.piloto$data[
    Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,2:3],
    pch=mclust.options("classPlotSymbols")[i],
    col=mclust.options("classPlotColors")[i])
}
legend("bottomright", paste("G", c(5,7)),
       col=mclust.options("classPlotColors")[c(5,7)],
       pch=mclust.options("classPlotSymbols")[c(5,7)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
#dev.off()
###################################################################################################################
# 5.2.4) Examinar la tabulación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos fenotípicos 
#según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment<-read.table("PhenotypicGroupAssignment_2023agosto19_072633.csv",header=T, sep=",")
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de supervivencia
phenotypic.group.assignment.progenie<-read.table("PhenotypicGroupAssignment.supervivencia.SPH_2023septiembre04_115629.csv",
                                                 header=T, sep=",")
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres<-phenotypic.group.assignment[308:350,]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number<-as.numeric(substring(phenotypic.group.assignment.madres$Collector.Collection.Number,5))
phenotypic.group.assignment.madres$Phenotypic.Group<-as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(phenotypic.group.assignment.madres$Collector.Collection.Number, 
       phenotypic.group.assignment.madres$Phenotypic.Group,length)
# 2  3  4  5 
# 6 18  5 14  

tapply(phenotypic.group.assignment.progenie$Collector.Collection.Number, 
       phenotypic.group.assignment.progenie$Phenotypic.Group,length)
# 1  2  3  4  5  6  7  8 
# 1  6  3  9 10  4  3  1  

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
phenotypic.group.assignment.madres.progenie<-merge(phenotypic.group.assignment.madres[,c(2,3)], phenotypic.group.assignment.progenie[,c(2,3)],
                                                   by="Collector.Collection.Number", suffixes = c(".madres",".progenie"))


phenotypic.group.crosstab<-table(phenotypic.group.assignment.madres.progenie[,c(2,3)])
phenotypic.group.crosstab
#                   Phenotypic.Group.progenie
# Phenotypic.Group.madres 1 2 3 4 5 6 7 8
                      # 2 1 0 0 2 3 0 0 0
                      # 3 0 4 1 4 2 3 1 0
                      # 4 0 1 1 0 2 0 1 0
                      # 5 0 1 1 3 3 1 1 1

###################################################################################################################
###################################################################################################################
# 5.3)PCR
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.supervivencia.piloto.PCR_2023September04.RData")#modelos VARS
load("proporcion.supervivencia_2023septiembre04_095936.RData")
###################################################################################################################
# 5.3.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.supervivencia <- data.frame(as.numeric(rownames(proporcion.supervivencia)),
                                                        proporcion.supervivencia[,1], 
                                                        Mcluster.supervivencia.piloto$classification, Mcluster.supervivencia.piloto$uncertainty)
colnames(phenotypic.group.assignment.supervivencia) <- 
  c("Rownames.Meanphenodata", "Collector.Collection.Number", "Phenotypic.Group", "Uncertainty")
head(phenotypic.group.assignment.supervivencia)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(phenotypic.group.assignment.supervivencia, 
          file=paste("PhenotypicGroupAssignment.supervivencia.PCR_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)
###################################################################################################################
# 5.3.2) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
# pdf("Figuras(supervivencia)_sección_5.1(PCR).pdf")

#Supervivencia.1 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:4), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
#                                [,1]      [,2]      [,3]      [,4]      
# proporcion.supervivencia.1 0.6111111 0.8951709 0.9598746 1.000000
# proporcion.supervivencia.2 0.1666667 0.8624786 0.8712310 0.953285
text(0.61, 0.17, "G1")
text(0.89, 0.86, "G2")
text(0.96, 0.87, "G3")
text(1, 0.95, "G4")

#Supervivencia.3 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:4), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
#                                [,1]      [,2]      [,3]      [,4]     
# proporcion.supervivencia.3 0.1666667 0.6536752 0.137162 0.765316
# proporcion.supervivencia.2 0.1666667 0.8624786 0.871231 0.953285
text(0.17, 0.17, "G1")
text(0.65, 0.86, "G2")
text(0.14, 0.87, "G3")
text(0.77, 0.95, "G4")
###################################################################################################################
# 5.3.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.supervivencia.piloto$uncertainty)

#De los 37 especímenes, ninguna de éstos tuvieron incertidumbre mayor a 0.1
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)/length(Mcluster.supervivencia.piloto$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.supervivencia.piloto$classification[Mcluster.supervivencia.piloto$uncertainty>0.1]


# #Gráfica de la función de distribución acumulativa de valores de incertidumbre
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(sort(Mcluster.supervivencia.piloto$uncertainty),
#      1:length(Mcluster.supervivencia.piloto$uncertainty)/length(Mcluster.supervivencia.piloto$uncertainty),
#      xlim=c(0,0.5), bty="n", xlab="Incertidumbre", ylab="F (Incertidumbre)",
#      type="l", pch=19, cex=0.5, cex.axis=1.5, cex.lab=1.5)
# abline(h=c(0,1), lty=3, col="gray70")
# 
# #Supervivencia 1 vs supervivencia 2
# # gráfica de los especímenes con incertidumbre > 0.1
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F, cex=0)
# #Agregar elipses
# for (i in 1:Mcluster.supervivencia.piloto$G){
#   points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
#                  centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
#          type="l", col="black")
# }
# #agregar etiquetas de las elipses
# #Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
# #                                [,1]      [,2]      [,3]      [,4]      [,5]
# # proporcion.supervivencia.1 0.6106961 0.9903580 0.8868995 1.0000000 1.0000000
# # proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
# text(0.61, 0.17, "G1")
# text(0.99, 0.99, "G2")
# text(0.89, 0.81, "G3")
# text(1, 0.94, "G4")
# text(1, 1, "G5")
# for(i in 1:Mcluster.supervivencia.piloto$G){
#   points(Mcluster.supervivencia.piloto$data[
#     Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,1:2],
#     pch=mclust.options("classPlotSymbols")[i],
#     col=mclust.options("classPlotColors")[i])
# }
# legend("topleft", paste("G", c(2,4)),
#        col=mclust.options("classPlotColors")[c(2,4)],
#        pch=mclust.options("classPlotSymbols")[c(2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
# 
# #Supervivencia 3 vs supervivencia 2
# # gráfica de los especímenes con incertidumbre > 0.1
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F, cex=0)
# #Agregar elipses
# for (i in 1:Mcluster.supervivencia.piloto$G){
#   points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
#                  centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
#          type="l", col="black")
# }
# 
# #agregar etiquetas de las elipses
# #Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
# #                                [,1]      [,2]      [,3]      [,4]      [,5]
# # proporcion.supervivencia.3 0.1668445 0.7452894 0.5697141 0.7722337 0.0000000
# # proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
# text(0.17, 0.17, "G1")
# text(0.75, 0.99, "G2")
# text(0.57, 0.81, "G3")
# text(0.77, 0.94, "G4")
# text(0, 1, "G5")
# for(i in 1:Mcluster.supervivencia.piloto$G){
#   points(Mcluster.supervivencia.piloto$data[
#     Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,2:3],
#     pch=mclust.options("classPlotSymbols")[i],
#     col=mclust.options("classPlotColors")[i])
# }
# legend("bottomright", paste("G", c(2,4)),
#        col=mclust.options("classPlotColors")[c(2,4)],
#        pch=mclust.options("classPlotSymbols")[c(2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
#dev.off()
###################################################################################################################
# 5.3.4) Examinar la tabulación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos fenotípicos 
#según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment<-read.table("PhenotypicGroupAssignment_2023agosto19_072633.csv",header=T, sep=",")
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en junio de 2023
phenotypic.group.assignment.progenie<-read.table("PhenotypicGroupAssignment.supervivencia.PCR_2023septiembre04_201210.csv",
                                                 header=T, sep=",")
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres<-phenotypic.group.assignment[308:350,]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number<-as.numeric(substring(phenotypic.group.assignment.madres$Collector.Collection.Number,5))
phenotypic.group.assignment.madres$Phenotypic.Group<-as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(phenotypic.group.assignment.madres$Collector.Collection.Number, 
       phenotypic.group.assignment.madres$Phenotypic.Group,length)
# 2  3  4  5 
# 6 18  5 14  

tapply(phenotypic.group.assignment.progenie$Collector.Collection.Number, 
       phenotypic.group.assignment.progenie$Phenotypic.Group,length)
#  1  2  3  4 
#  3  5  3 26  

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
phenotypic.group.assignment.madres.progenie<-merge(phenotypic.group.assignment.madres[,c(2,3)], phenotypic.group.assignment.progenie[,c(2,3)],
                                                   by="Collector.Collection.Number", suffixes = c(".madres",".progenie"))


phenotypic.group.crosstab<-table(phenotypic.group.assignment.madres.progenie[,c(2,3)])
phenotypic.group.crosstab
#                         Phenotypic.Group.progenie
# Phenotypic.Group.madres  1  2  3  4
                      # 2  1  0  0  5
                      # 3  0  3  2 10
                      # 4  1  1  0  3
                      # 5  1  1  1  8

###################################################################################################################
###################################################################################################################
# 5.4)SVD
#cargar los mejores modelos de mezclas normales
#directorio de trabajo
#setwd("C:/_transfer/Review/MelissaPineda/Data_Melissa") #directorio de Iván: Lehmann
#setwd("C:/_transfer/Proposals/Espeletia/TesisMelissa/Data") #Directorio de Iván: Waterman
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# directorio de Diana
load("Mcluster.supervivencia.piloto.SVD_2023September04.RData")#modelos VARS
load("proporcion.supervivencia_2023septiembre04_095936.RData")
###################################################################################################################
# 5.4.1)Examinar y guardar en un documento la asignación de grupos para la progenie en el segundo muestreo

#crear y escribir documento para la asignación de los grupos fenotípicos.
phenotypic.group.assignment.supervivencia <- data.frame(as.numeric(rownames(proporcion.supervivencia)),
                                                        proporcion.supervivencia[,1], 
                                                        Mcluster.supervivencia.piloto$classification, Mcluster.supervivencia.piloto$uncertainty)
colnames(phenotypic.group.assignment.supervivencia) <- 
  c("Rownames.Meanphenodata", "Collector.Collection.Number", "Phenotypic.Group", "Uncertainty")
head(phenotypic.group.assignment.supervivencia)
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")# guardar en directorio de Diana
#setwd("C:/_transfer/Projects/Proposals/Espeletia/TesisMelissa/Data") #Ivan's working directory Waterman
write.csv(phenotypic.group.assignment.supervivencia, 
          file=paste("PhenotypicGroupAssignment.supervivencia.SVD_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)
###################################################################################################################
# 5.4.2) Graficar grupos fenotípicos en el mejor modelo de mezclas normales.

# directorio para guardar figuras
#setwd("C:/_transfer/Review/MelissaPineda/Figures")
# setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Figuras")# directorio de Diana
# pdf("Figuras(supervivencia)_sección_5.4(SVD).pdf")

#Supervivencia.1 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:4), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
#                                [,1]      [,2]      [,3]      [,4]      
# proporcion.supervivencia.1 0.4166667 1.0000000 1.0000000 0.8918091
# proporcion.supervivencia.2 0.2500000 0.9818676 0.5616383 0.8228989
text(0.42, 0.25, "G1")
text(1, 0.98, "G2")
text(1, 56, "G3")
text(0.89, 0.82, "G4")

#Supervivencia.3 vs supervivencia.2
#par(mar=c(5,4,4,2)+0.1) #default
par(mar=c(5,5,4,2)+0.1)
plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F)
legend("bottomright", paste("G", 1:4), col=mclust.options("classPlotColors"),xpd=T,ncol=3,
       pch=mclust.options("classPlotSymbols"), pt.lwd=0.8, pt.cex=0.8, cex=0.8, bty="o")
#agregar elipses
for (i in 1:Mcluster.supervivencia.piloto$G){
  points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
                 centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
         type="l", col="black")
}
#agregar etiquetas de las elipses
#Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
#                             [,1]      [,2]      [,3]      [,4]     
# proporcion.supervivencia.3 0.25 0.7456376 0.3127092 0.6072293
# proporcion.supervivencia.2 0.25 0.9818676 0.5616383 0.8228989
text(0.25, 0.25, "G1")
text(0.75, 0.98, "G2")
text(0.31, 0.56, "G3")
text(0.60, 0.82, "G4")
###################################################################################################################
# 5.4.3) Examinar la incertidumbre de la clasificación.

# Resumen de los valores de incertidumbre
summary(Mcluster.supervivencia.piloto$uncertainty)

#De los 37 especímenes, ninguna de éstos tuvieron incertidumbre mayor a 0.1
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)
sum(Mcluster.supervivencia.piloto$uncertainty>0.1)/length(Mcluster.supervivencia.piloto$uncertainty)
# Clasificación de los especímenes  con incertidumbre mayor a  0.1
Mcluster.supervivencia.piloto$classification[Mcluster.supervivencia.piloto$uncertainty>0.1]


# #Gráfica de la función de distribución acumulativa de valores de incertidumbre
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(sort(Mcluster.supervivencia.piloto$uncertainty),
#      1:length(Mcluster.supervivencia.piloto$uncertainty)/length(Mcluster.supervivencia.piloto$uncertainty),
#      xlim=c(0,0.5), bty="n", xlab="Incertidumbre", ylab="F (Incertidumbre)",
#      type="l", pch=19, cex=0.5, cex.axis=1.5, cex.lab=1.5)
# abline(h=c(0,1), lty=3, col="gray70")
# 
# #Supervivencia 1 vs supervivencia 2
# # gráfica de los especímenes con incertidumbre > 0.1
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(1,2), main="", addEllipses = F, cex=0)
# #Agregar elipses
# for (i in 1:Mcluster.supervivencia.piloto$G){
#   points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
#                  centre = Mcluster.supervivencia.piloto$parameters$mean[c(1,2),i], level = pchisq(1, 2)),
#          type="l", col="black")
# }
# #agregar etiquetas de las elipses
# #Mcluster.supervivencia.piloto$parameters$mean[c(1,2),]
# #                                [,1]      [,2]      [,3]      [,4]      [,5]
# # proporcion.supervivencia.1 0.6106961 0.9903580 0.8868995 1.0000000 1.0000000
# # proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
# text(0.61, 0.17, "G1")
# text(0.99, 0.99, "G2")
# text(0.89, 0.81, "G3")
# text(1, 0.94, "G4")
# text(1, 1, "G5")
# for(i in 1:Mcluster.supervivencia.piloto$G){
#   points(Mcluster.supervivencia.piloto$data[
#     Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,1:2],
#     pch=mclust.options("classPlotSymbols")[i],
#     col=mclust.options("classPlotColors")[i])
# }
# legend("topleft", paste("G", c(2,4)),
#        col=mclust.options("classPlotColors")[c(2,4)],
#        pch=mclust.options("classPlotSymbols")[c(2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
# 
# #Supervivencia 3 vs supervivencia 2
# # gráfica de los especímenes con incertidumbre > 0.1
# #par(mar=c(5,4,4,2)+0.1) #default
# par(mar=c(5,5,4,2)+0.1)
# plot(Mcluster.supervivencia.piloto, what=c("classification"), dimens=c(3,2), main="", addEllipses = F, cex=0)
# #Agregar elipses
# for (i in 1:Mcluster.supervivencia.piloto$G){
#   points(ellipse(x = Mcluster.supervivencia.piloto$parameters$variance$sigma[1:2,1:2,i],
#                  centre = Mcluster.supervivencia.piloto$parameters$mean[c(3,2),i], level = pchisq(3, 2)),
#          type="l", col="black")
# }
# 
# #agregar etiquetas de las elipses
# #Mcluster.supervivencia.piloto$parameters$mean[c(3,2),]
# #                                [,1]      [,2]      [,3]      [,4]      [,5]
# # proporcion.supervivencia.3 0.1668445 0.7452894 0.5697141 0.7722337 0.0000000
# # proporcion.supervivencia.2 0.1668445 0.9855370 0.8132083 0.9392751 0.9983833
# text(0.17, 0.17, "G1")
# text(0.75, 0.99, "G2")
# text(0.57, 0.81, "G3")
# text(0.77, 0.94, "G4")
# text(0, 1, "G5")
# for(i in 1:Mcluster.supervivencia.piloto$G){
#   points(Mcluster.supervivencia.piloto$data[
#     Mcluster.supervivencia.piloto$uncertainty>0.1 & Mcluster.supervivencia.piloto$classification==i,2:3],
#     pch=mclust.options("classPlotSymbols")[i],
#     col=mclust.options("classPlotColors")[i])
# }
# legend("bottomright", paste("G", c(2,4)),
#        col=mclust.options("classPlotColors")[c(2,4)],
#        pch=mclust.options("classPlotSymbols")[c(2,4)], pt.lwd=1, pt.cex=1, cex=1.3, bty="o")
#dev.off()
###################################################################################################################
# 5.4.4) Examinar la tabulación cruzada de los grupos fenotípicos según las plantas madres del piloto y grupos fenotípicos 
#según plantas de progenie del piloto el mejor modelo de mezcla normal de cada una.

#Lectura de las tablas con los grupos asignados de las madres e hijas
setwd("C:/Users/usuario/Documents/Jardin_comun")#directorio de los datos de las plantas madres
#Asignación de grupos de las plantas madres del piloto junto con los datos de Pineda et al.
phenotypic.group.assignment<-read.table("PhenotypicGroupAssignment_2023agosto19_072633.csv",header=T, sep=",")
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie")
#Asignación de grupos de plantas progenie con datos de crecimiento tomados en junio de 2023
phenotypic.group.assignment.progenie<-read.table("PhenotypicGroupAssignment.supervivencia.SVD_2023septiembre04_203727.csv",
                                                 header=T, sep=",")
#Subconjunto con sólo las plantas madres del piloto
phenotypic.group.assignment.madres<-phenotypic.group.assignment[308:350,]
head(phenotypic.group.assignment.madres)
head(phenotypic.group.assignment.progenie)

#Extracción del número de colección de las plantas madres
phenotypic.group.assignment.madres$Collector.Collection.Number<-as.numeric(substring(phenotypic.group.assignment.madres$Collector.Collection.Number,5))
phenotypic.group.assignment.madres$Phenotypic.Group<-as.factor(phenotypic.group.assignment.madres$Phenotypic.Group)
# Cantidad de plantas en cada grupo tanto de las madres como de la progenie
tapply(phenotypic.group.assignment.madres$Collector.Collection.Number, 
       phenotypic.group.assignment.madres$Phenotypic.Group,length)
# 2  3  4  5 
# 6 18  5 14  

tapply(phenotypic.group.assignment.progenie$Collector.Collection.Number, 
       phenotypic.group.assignment.progenie$Phenotypic.Group,length)
#  1  2  3  4 
#  2 25  4  6   

#unir los dos agrupamientos para las plantas madres en común que tenga representación en la progenie: 35 plantas
phenotypic.group.assignment.madres.progenie<-merge(phenotypic.group.assignment.madres[,c(2,3)], phenotypic.group.assignment.progenie[,c(2,3)],
                                                   by="Collector.Collection.Number", suffixes = c(".madres",".progenie"))


phenotypic.group.crosstab<-table(phenotypic.group.assignment.madres.progenie[,c(2,3)])
phenotypic.group.crosstab
#                        Phenotypic.Group.progenie
# Phenotypic.Group.madres 1 2 3 4
                      # 2 0 5 1 0
                      # 3 0 9 2 4
                      # 4 1 3 0 1
                      # 5 1 8 1 1

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
#
# INTRODUCCIÓN
#
# Archivos de datos requeridos:
# "NWSA_30s_elev.tif", "SumapazElevSR.tif", "SumapazStudyRegion.shp", "PhenotypicGroupAssignment_2023septiembre08_120644.csv"
#
# Paquetes de R requeridos:
# terra y rgdal
#
# Contenido:
# 1) Preliminares: caragar paquetes y leer y examinar datos.
# 2) Mapa de la región de estudio y las localidades de colecta de las plantas madre del jardín común.
# 3) Mapa de las localidades de colecta de todas las plantas incluidas en el análisis morfológico.
# 4) Mapas de la distribución geográfica de cada grupo morfológico. 
#
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


####################################################################################################################################
# 1) Preliminares: caragar paquetes y leer y examinar datos.
####################################################################################################################################

#cargar paquetes
library(terra)
library(rgdal)

#leer archivo con contorno del área del Sumapaz por encima de 3000 m de altitud.
#setwd("C:/_transfer/proposals/Espeletia/Sumapaz_data/REgionEstudioLimite") #Ivan en Lehmann
region.estudio <- vect("SumapazStudyRegion.shp")
class(region.estudio)
region.estudio

#leer modelo digital de elevación para el noroccidente de Sur América, con resolución de 30 m,
#obtenido de https://asterweb.jpl.nasa.gov/gdem.asp 
#setwd("C:/_transfer/SpatialData_General/DEM_NWSA")#Ivan at Lehmann
NWSA_ele <- rast("NWSA_30s_elev.tif")
par(mar=c(5,4,1,3),mfrow=c(1,1))
class(NWSA_ele)
NWSA_ele

#leer modelo digital de elevación, con resolución de 30 m, del área del Sumapaz por encima de 3000 m de altitud,
#obtenido de https://asterweb.jpl.nasa.gov/gdem.asp 
#setwd("C:/_transfer/proposals/Espeletia/Sumapaz_data/REgionEstudioLimite") #Ivan en Lehmann
region.estudio.ele <- rast("SumapazElevSR.tif")
class(region.estudio.ele)
region.estudio.ele

#leer archivo con datos de especímenes
#setwd("C:/_transfer/Papers/EspeletiaPilotCommonGarden/Data") #Ivan en Lehmann
datos.especimenes <- read.table("PhenotypicGroupAssignment_2023septiembre08_120644.csv", header=T, sep=",")
class(datos.especimenes)
dim(datos.especimenes)
head(datos.especimenes)

#crear un objeto de clase SpaVector
sv.especimenes <- vect(datos.especimenes, geom=c("Longitude", "Latitude"), crs=crs(region.estudio))
class(sv.especimenes)
sv.especimenes


####################################################################################################################################
# 2) Mapa de la región de estudio y las localidades de colecta de las plantas madre del jardín común.
####################################################################################################################################

#directorio para guardar gráficas
#setwd("C:/_transfer/Papers/EspeletiaPilotCommonGarden/Figures") #Ivan en Lehmann

#mapa del norte de Sur América
par(mar=c(5,4,1,3),mfrow=c(1,1))
plot(NWSA_ele, col="gray90", xlim=c(-78.5,-70), ylim=c(0,12.5), legend=F, axes=F, mar=c(6,3,1,3))
plot(NWSA_ele, range=c(0,1000), col="gray", add=T, xlim=c(-78.5,-70), ylim=c(0,12.5), legend=F, axes=F)
plot(region.estudio, add=T)
axis(1, at=seq(-78,-70,2), cex.axis=1.5)
axis(2, at=seq(0,12,2), line=-4.6, cex.axis=1.5)
mtext(side=1, "Longitud (grados decimales)", cex=1.5, line=3)
mtext(side=2, "Latitud (grados decimales)", cex=1.5, line=-1.6)
text(-78.1, 5, "Oceano Pacífico", srt=90, cex=1.3, col="gray70")
text(-76.3, 11, "Mar Caribe", srt=45, cex=1.3, col="gray70")
text(-71.6, 4, "Alto Sumapaz", cex=1.3, col="black")
arrows(x0=-73.16, y0=4, x1 =-73.8, y1 = 4, length = 0.07, angle = 30, code = 2, col = "black")
#text(-77.5, 12, "a)", cex=1.5, col="black") 
mtext(side=2, "a)", cex=1.5, line=-1.6, at=12.5, las=2)

#mapa de la región del Alto Sumapaz
par(mar=c(5,4,1,3),mfrow=c(1,1))
plot(region.estudio, asp=1, axes=F, legend=F, mar=c(6,3,1,3))
axis(1, at=seq(-74.6, -74, 0.2), cex.axis=1.2, pos=3.392917)
axis(2, cex.axis=1.2, pos=-74.69625)
mtext(side=1, "Longitud (grados decimales)", cex=1.5, line=3.5)
mtext(side=2, "Latitud (grados decimales)", cex=1.5, line=-2.5)
#escala espacial
sbar(d=30, xy = c(-74.6, 4.6), type = "bar", divs = 2, below = "", lonlat=T, label=c("0", "", "30"), adj=c(0.5,-1))
sbar(d=30, xy = c(-74.6, 4.6), type = "bar", divs = 2, below = "km", lonlat=T, label=c("", "", ""), adj=c(0.5,-1.5))
rect(-74.65, 4.52, -74.28, 4.7)
#adicionar localidades de colecta de las plantas madre del jardín común
ipm <- rep(c(F,T), times=c(307,43)) #crea índice plantas madre del jardín común
points(sv.especimenes[ipm,], col=morfogrupo.color[sv.especimenes$Phenotypic.Group[ipm]], pch=morfogrupo.simbolo[sv.especimenes$Phenotypic.Group[ipm]], cex=1.5)

#mapa de las localidades de colecta de las plantas madre del jardín común
morfogrupo.simbolo <- c(16,0,17,3,15,4) #definir simbolos para cada grupo morfológico, de acuerdo a la figuras de Mclust
morfogrupo.color <- c("dodgerblue2","red3","green3","slateblue","darkorange","skyblue1") #definir colores para cada grupo morfológico, de acuerdo a la figuras de Mclust
windows(14,7) #definir ventana gráfica
plot(region.estudio.ele, axes=F, legend=T, xlim=c(-74.30, -74.24),
	ylim=c(4.188, 4.223), mar=c(5,4,2,7), plg=list(cex=1.5),
	col=hcl.colors(n=8, palette="Light Grays"), range=c(3250, 4050))
axis(1, seq(-74.29, -74.25, 0.01), cex.axis=1.5, lwd=2)
axis(2, at=seq(4.19, 4.22, 0.01), cex.axis=1.5, lwd=2, line=-5.6)
mtext(side=1, "Longitud (grados decimales)", cex=1.5, line=2.9)
mtext(side=2, "Latitud (grados decimales)", cex=1.5, line=-2.5)
mtext(side=4, "Altitud (m)", cex=1.5)
sbar(d=1, xy = c(-74.295, 4.195), type = "bar", divs = 2, below = "", lonlat=T, label=c("0", "", "1"), adj=c(0.5,-1))
sbar(d=1, xy = c(-74.295, 4.195), type = "bar", divs = 2, below = "km", lonlat=T, label=c("", "", ""), adj=c(0.5,-1.5))
rect(-74.2965, 4.192, -74.2845, 4.1985)
#adicionar localidades de colecta de las plantas madre del jardín común
ipm <- rep(c(F,T), times=c(307,43)) #crea índice plantas madre del jardín común
plot(sv.especimenes[ipm,], col=morfogrupo.color[sv.especimenes$Phenotypic.Group[ipm]],
	pch=morfogrupo.simbolo[sv.especimenes$Phenotypic.Group[ipm]], cex=1.5, add=T)
mtext(side=2, "c)", cex=1.5, line=-1.6, at=4.223, las=2)


####################################################################################################################################
# 3) Mapa de las localidades de colecta de todas las plantas incluidas en el análisis morfológico.
####################################################################################################################################

#directorio para guardar gráficas
#setwd("C:/_transfer/Papers/EspeletiaPilotCommonGarden/Figures") #Ivan en Lehmann

#mapa de toda el área del Sumapaz por encima de 3000 m de altitud
par(mar=c(5,4,1,3),mfrow=c(1,1))
plot(region.estudio, asp=1, axes=F, legend=F, mar=c(6,3,1,3))
plot(sv.especimenes, col=morfogrupo.color[sv.especimenes$Phenotypic.Group],
	pch=morfogrupo.simbolo[sv.especimenes$Phenotypic.Group], cex=1.5, add=T)
axis(1, at=seq(-74.6, -74, 0.2), cex.axis=1.2, pos=3.392917)
axis(2, cex.axis=1.2, pos=-74.69625)
mtext(side=1, "Longitud (grados decimales)", cex=1.5, line=3.5)
mtext(side=2, "Latitud (grados decimales)", cex=1.5, line=-2.5)
sbar(d=30, xy = c(-74.6, 4.6), type = "bar", divs = 2, below = "", lonlat=T, label=c("0", "", "30"), adj=c(0.5,-1))
sbar(d=30, xy = c(-74.6, 4.6), type = "bar", divs = 2, below = "km", lonlat=T, label=c("", "", ""), adj=c(0.5,-1.5))
rect(-74.65, 4.52, -74.28, 4.7)
mtext(side=2, "a)", cex=1.5, line=-1.6, at=4.7, las=2)

#mapa restringido al área de colecta de plantas madre del jardín común,
#pero mostrando todas la localidad de colecta de todas las plantas
#incluidas en el análisis morfológico
windows(14,7)
plot(region.estudio.ele, axes=F, legend=T, xlim=c(-74.30, -74.24),
	ylim=c(4.188, 4.223), mar=c(5,4,2,7), plg=list(cex=1.5),
	col=hcl.colors(n=7, palette="Light Grays"))
axis(1, seq(-74.29, -74.25, 0.01), cex.axis=1.5, lwd=2)
axis(2, at=seq(4.19, 4.22, 0.01), cex.axis=1.5, lwd=2, line=-5.6)
mtext(side=1, "Longitud (grados decimales)", cex=1.5, line=2.9)
mtext(side=2, "Latitud (grados decimales)", cex=1.5, line=-2.5)
mtext(side=4, "Altitud (m)", cex=1.5)
sbar(d=1, xy = c(-74.295, 4.195), type = "bar", divs = 2, below = "", lonlat=T, label=c("0", "", "1"), adj=c(0.5,-1))
sbar(d=1, xy = c(-74.295, 4.195), type = "bar", divs = 2, below = "km", lonlat=T, label=c("", "", ""), adj=c(0.5,-1.5))
rect(-74.2965, 4.192, -74.2845, 4.1985)
#adicionar localidades de colecta de las plantas madre del jardín común
ipm <- rep(c(F,T), times=c(307,43)) #crea índice plantas madre del jardín común
plot(sv.especimenes, col=morfogrupo.color[sv.especimenes$Phenotypic.Group],
	pch=morfogrupo.simbolo[sv.especimenes$Phenotypic.Group], cex=1.5, add=T)
mtext(side=2, "b)", cex=1.5, line=-1.6, at=4.223, las=2)


####################################################################################################################################
# 4) Mapas de la distribución geográfica de cada grupo morfológico.
####################################################################################################################################

#directorio para guardar gráficas
#setwd("C:/_transfer/Papers/EspeletiaPilotCommonGarden/Figures") #Ivan en Lehmann

#definir simbolos y colores para cada grupo morfológico,
#de acuerdo con figuras de Mclust
morfogrupo.color <- c("dodgerblue2","red3","green3","slateblue","darkorange","skyblue1")
morfogrupo.simbolo <- c(16,0,17,3,15,4)
par(mar=c(5,4,1,3),mfrow=c(1,1))
#mapa del área del Sumapaz por encima de 3000 m de altitud.
plot(region.estudio, asp=1, axes=F, legend=F, mar=c(6,3,1,3))
axis(1, at=seq(-74.6, -74, 0.2), cex.axis=1.2, pos=3.392917)
axis(2, cex.axis=1.2, pos=-74.69625)
mtext(side=1, "Longitud (grados decimales)", cex=1.5, line=3.5)
mtext(side=2, "Latitud (grados decimales)", cex=1.5, line=-2.5)
#escala espacial
sbar(d=30, xy = c(-74.6, 4.6), type = "bar", divs = 2, below = "", lonlat=T, label=c("0", "", "30"), adj=c(0.5,-1))
sbar(d=30, xy = c(-74.6, 4.6), type = "bar", divs = 2, below = "km", lonlat=T, label=c("", "", ""), adj=c(0.5,-1.5))
rect(-74.65, 4.52, -74.28, 4.7)
#adicionar localidades de colecta de los especímenes de un grupo fenotípico
gm <- 5 #escoger el grupo morfológico
igm <- datos.especimenes[,6] == gm #crea índice de los especímenes del grupo morfológico escogido
points(sv.especimenes[igm,], col=morfogrupo.color[gm], pch=morfogrupo.simbolo[gm], cex=1.5)
#plot(convHull(sv.especimenes[igf,]), add=T, border=morfogrupo.color[gm]) #polígono mínimo convexo
text(-74.15, 3.45, paste("Grupo morfológico", gm, sep=" "), cex=1.3)
mtext(side=2, paste(letters[gm], ")", sep=""), at=4.7, cex=1.5, las=2)




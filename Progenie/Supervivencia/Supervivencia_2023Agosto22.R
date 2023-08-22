###################################################################################################################
###################################################################################################################
# MARCO DE DATOS PARA REALIZAR ANÁLISIS DE SUPERVIVENCIA
#
#Durante el 2020 se realizaron dos tomas de datos a las plantas de la progenie piloto en marzo y en octubre y en el 2023 
#se realizó otra toma de datos en junio. Estos muestreos midieron el crecimiento de estas plantas: longitud de tallo, 
#número de hojas y la filotaxis. Implícitamente es posible medir el grado de supervivencia con estos tres muestreo.
#El objetivo de este código es extrar un marco de datos para realizar análisis de supervivencia
###################################################################################################################
###################################################################################################################
# 1) Preliminares: cargar librerías y leer las tres tablas de datos
# 1.1)librerías
library(tidyverse)

#seleccionar directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia")

# 1.2)leer las tablas
datos.fenotipicos.marzo<-read.table("PhenotypicDataProgeny_Quebradas_2020Marzo.csv",header=T, sep=",")
datos.fenotipicos.octubre<-read.table("PhenotypicDataProgeny_Quebradas_2020Octubre.csv", header=T, sep = ",")
datos.fenotipicos.junio<-read.table("PhenotypicDataProgeny_Quebradas_2023Junio.csv", header = T, sep=",")

###################################################################################################################
# 2)seleccionar las variables a usar en cada tabla en cada tabla

# 2.1)primer muestro: marzo de 2020
summary(datos.fenotipicos.marzo)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables


# 2.1.1)subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
#nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.marzo)
datos.fenotipicos.marzo.selected<-datos.fenotipicos.marzo%>% 
  unite("nombre.progenie",c(Bandeja,Fila,Columna))%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición,
         Longitud.tallo..cm.,Número.de.hojas, OBSERVACIONES)%>%
  mutate(Fecha.trasplante=dmy(Fecha.trasplante),
         Fecha.siembra=dmy(Fecha.siembra),
         Fecha.medición=dmy(Fecha.medición))
# 2.1.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.1<-datos.fenotipicos.marzo.selected%>%
  mutate(supervivencia.1=if_else(is.na(Longitud.tallo..cm.) & is.na(Número.de.hojas),"M", "V", "M"))
view(supervivencia.1)

supervivencia.1<-supervivencia.1%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición, supervivencia.1)

# 2.2) segundo muestro: octubre de 2020
summary(datos.fenotipicos.octubre)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables


# 2.2.1) subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
#nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.marzo)
datos.fenotipicos.octubre.selected<-datos.fenotipicos.octubre%>% 
  unite("nombre.progenie",c(Bandeja,Fila,Columna))%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición,
         Longitud.tallo..cm.,Número.de.hojas, OBSERVACIONES)%>%
  mutate(Fecha.trasplante=dmy(Fecha.trasplante),
         Fecha.siembra=dmy(Fecha.siembra),
         Fecha.medición=dmy(Fecha.medición))
view(datos.fenotipicos.octubre.selected)
# 2.2.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.2<-datos.fenotipicos.octubre.selected%>%
  mutate(supervivencia.2=if_else(is.na(Fecha.medición),"M", "V", "M"))
view(supervivencia.2)

supervivencia.2<-supervivencia.2%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición, supervivencia.2)


# 2.3) tercer muestro: junio de 2023
summary(datos.fenotipicos.junio)
head(datos.fenotipicos.junio)
dim(datos.fenotipicos.junio)# 177 plantas hijas sembradas y 26 variables


# 2.3.1)subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
#nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.junio)
datos.fenotipicos.junio.selected<-datos.fenotipicos.junio%>% 
  unite("nombre.progenie",c(Bandeja,Fila,Columna))%>%
  select(Número.colección.planta.madre,nombre.progenie,Fecha.siembra,Fecha.transplante,Fecha.medición,
         Longitud.tallo..mm.,Número.de.hojas,OBSERVACIONES)%>%
  rename(Fecha.trasplante=Fecha.transplante)%>%
  mutate(Fecha.trasplante=dmy(Fecha.trasplante),
         Fecha.siembra=dmy(Fecha.siembra),
         Fecha.medición=dmy(Fecha.medición),
         Número.colección.planta.madre=as.character(Número.colección.planta.madre))
view(datos.fenotipicos.junio.selected)
# 2.3.2) crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.3<-datos.fenotipicos.junio.selected%>%
  mutate(supervivencia.3=if_else(is.na(Fecha.medición),"M", "V", "M"))
view(supervivencia.3)

supervivencia.3<-supervivencia.3%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición, supervivencia.3)

###################################################################################################################
# 3) Crear marco de datos de la supervivencia

# 3.1) combinar la tabla supervivencia 1 y supervivencia 2

supervivencia<-left_join(supervivencia.1,supervivencia.2, by = join_by(Número.colección.planta.madre, nombre.progenie, 
                                                                       Fecha.siembra, Fecha.trasplante))
# 3.2)combinar supervivencia con supervivencia 3

supervivencia<-left_join(supervivencia,supervivencia.3, by = join_by(Número.colección.planta.madre, nombre.progenie, 
                                                                       Fecha.siembra, Fecha.trasplante))
view(supervivencia)

# 3.3 )cambiar los NAS del muestreo 3 por "M"
supervivencia<-
  supervivencia%>%  replace_na(list(supervivencia.3="M"))

# 3.4 )renombrar las variable

supervivencia<-supervivencia%>%
  rename(Fecha.medición.1=Fecha.medición.x,
         Fecha.medición.2=Fecha.medición.y,
         Fecha.medición.3=Fecha.medición)
view(supervivencia)

###################################################################################################################
#4) Guardar marco de datos

setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia")

# 4.1). RData
save(supervivencia, file= paste("Supervivencia_piloto_", format(Sys.time(), "%Y%B%d_%H%M%S"), ".RData", sep=""))

# 4.2) .csv
write.csv(supervivencia, file= paste("supervivencia_piloto_",format(Sys.time(), "%Y%B%d_%H%M%S"), ".csv", sep=""), row.names = F)

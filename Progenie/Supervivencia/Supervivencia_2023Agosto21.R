#librerías
library(tidyverse)

#seleccionar directorio de trabajo
setwd("C:/Users/usuario/Documents/Jardin_comun/Progenie/Supervivencia")
#leer las tablas
datos.fenotipicos.marzo<-read.table("PhenotypicDataProgeny_Quebradas_2020Marzo.csv",header=T, sep=",")
datos.fenotipicos.octubre<-read.table("PhenotypicDataProgeny_Quebradas_2020Octubre.csv", header=T, sep = ",")
datos.fenotipicos.junio<-read.table("PhenotypicDataProgeny_Quebradas_2023Junio.csv", header = T, sep=",")

#seleccionar las variables a usar en cada tabla
#primer muestro: marzo de 2020
summary(datos.fenotipicos.marzo)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables


#subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
#nombre.progenie; seleccionar las variables para analizar supervivencia
colnames(datos.fenotipicos.marzo)
datos.fenotipicos.marzo.selected<-datos.fenotipicos.marzo%>% 
  unite("nombre.progenie",c(Bandeja,Fila,Columna))%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición,
         Longitud.tallo..cm.,Número.de.hojas, OBSERVACIONES)%>%
  mutate(Fecha.trasplante=dmy(Fecha.trasplante),
         Fecha.siembra=dmy(Fecha.siembra),
         Fecha.medición=dmy(Fecha.medición))
#crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.1<-datos.fenotipicos.marzo.selected%>%
  mutate(supervivencia.1=if_else(is.na(Longitud.tallo..cm.) & is.na(Número.de.hojas),"M", "V", "M"))
view(supervivencia.1)

supervivencia.1<-supervivencia.1%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición, supervivencia.1)

#segundo muestro: octubre de 2020
summary(datos.fenotipicos.octubre)
head(datos.fenotipicos.marzo)
dim(datos.fenotipicos.marzo)# 250 plantas hijas sembradas y 26 variables


#subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
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
#crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.2<-datos.fenotipicos.octubre.selected%>%
  mutate(supervivencia.2=if_else(is.na(Fecha.medición),"M", "V", "M"))
view(supervivencia.2)

supervivencia.2<-supervivencia.2%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición, supervivencia.2)


#tercer muestro: junio de 2023
summary(datos.fenotipicos.junio)
head(datos.fenotipicos.junio)
dim(datos.fenotipicos.junio)# 177 plantas hijas sembradas y 26 variables


#subconjunto de las columnas: unir las variables Bandeja, fila y columna para crear variable
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
#crear nueva variable que valide si la progenie está viva o muerta para el fecha de muestreo
supervivencia.3<-datos.fenotipicos.junio.selected%>%
  mutate(supervivencia.3=if_else(is.na(Fecha.medición),"M", "V", "M"))
view(supervivencia.3)

supervivencia.3<-supervivencia.3%>%
  select(Número.colección.planta.madre, nombre.progenie, Fecha.siembra,Fecha.trasplante, Fecha.medición, supervivencia.3)




## combinar la tabla supervivencia 1 y supervivencia 2

supervivencia<-left_join(supervivencia.1,supervivencia.2, by = join_by(Número.colección.planta.madre, nombre.progenie, 
                                                                       Fecha.siembra, Fecha.trasplante))
#combinar supervivencia con supervivencia 3

supervivencia<-left_join(supervivencia,supervivencia.3, by = join_by(Número.colección.planta.madre, nombre.progenie, 
                                                                       Fecha.siembra, Fecha.trasplante))
view(supervivencia)

#cambiar los NAS del muestreo 3 por "M"
supervivencia<-
  supervivencia%>%  replace_na(list(supervivencia.3="M"))

#renombrar las variable

supervivencia<-supervivencia%>%
  rename(Fecha.medición.1=Fecha.medición.x,
         Fecha.medición.2=Fecha.medición.y,
         Fecha.medición.3=Fecha.medición)
view(supervivencia)

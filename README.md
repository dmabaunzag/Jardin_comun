# La naturaleza de las especies de frailejones: un experimento de jardín común en Sumapaz

## Introducción

Este repositorio hace parte del material suplementario del artículo **"La naturaleza de las especies de
frailejones: un experimento de jardín común en Sumapaz"**.

El objetivo fue hacer un análisis de delimitación de especies *de novo* basado en caracteres morfológicos de frailejones de del Páramo Sumapaz, cordillera Oriental de los Andes (Colombia) y determinar si los grupos morfológicos de las plantas madre corresponden a grupos con diferentes nichos ecológicos durante periodos tempranos de la ontogenia, utilizando métodos de delimitación *de novo*, diseñados para el descubrimiento de grupos sin información *a priori*. Esta correspondencia indicaría que los grupos morfológicos de frailejones silvestres corresponden a especies que difieren en reclutamiento y tasas de crecimiento durante los primeros años de vida, es decir, los grupos morfológicos de frailejones no son demográficamente intercambiables porque difieren en la ecología de las etapas ontogenéticas tempranas.

## CONTENIDO
El repositorio tiene dos carpetas principales:

*  [**Especímenes**](https://github.com/dmabaunzag/Jardin_comun/tree/main/Especimenes)**:** Código y datos sobre la conformación de grupos morfológicos de los frailejones silvestres y el área de colecta de éstos:
    + _Mclust_madres_2024Junio06.R_: Código de R para estimar _de novo_ los grupos morfológicos.
    + [Elevacion_Limite_RegionEstudio](https://github.com/dmabaunzag/Jardin_comun/tree/main/Especimenes/Elevacion_Limite_RegionEstudio): Carpeta con los datos y código de R para realizar mapas con la distribución geográfica del muestreo de las plantas madre en el Páramo del Sumapaz.
    +[datos](https://github.com/dmabaunzag/Jardin_comun/tree/main/Especimenes/datos): Archivos con la información de los caracteres morfológicos de plantas madre y los datos de Pineda et. al (2020).
	

*  [**Progenie**](https://github.com/dmabaunzag/Jardin_comun/tree/main/Progenie)**:** Código y datos sobre la conformación de grupos según reclutamiento y crecimiento de la progenie de las plantas madre sembrados en el jardín común.
    +  [datos](https://github.com/dmabaunzag/Jardin_comun/tree/main/Progenie/datos): Datos de crecimiento de la progenie tomados a 11.4, 19.6 y 51.4 meses después de la siembra en el jardín común.
    +  [Crecimiento](https://github.com/dmabaunzag/Jardin_comun/tree/main/Progenie/Crecimiento): Código y análisis de modelos de mezclas normales bivariados para cada muestreo y datos asociados
    +  [reclutamiento](https://github.com/dmabaunzag/Jardin_comun/tree/main/Progenie/reclutamiento): Código y análisis de modelos mezclas binomiales del reclutamiento de la progenie para cada muestreo y datos asociados.
    + _ConcordanciaCrecimientoReclutamiento_2024Junio06.R_: Código de R que examina la concordancia entre muestreos de grupos de plantas madre según reclutamiento y crecimiento de su progenie.
    + _CorrelacionCrecimientoReclutamiento_2024Junio06.R_: Código de R que examina la correlación entre el reclutamiento y las variables de crecimiento (eg. número de hojas y longitud del tallo) en cada muestreo.

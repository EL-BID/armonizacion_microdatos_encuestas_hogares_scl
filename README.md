# Banco de Datos Armonizado de las Encuestas de Hogares

## Descripción

El repositorio armonizacion_microdatos_encuestas_hogares_scl contiene los scripts de transformación de las 
encuestas originales de forma tal que proporcionan información comparable a lo 
largo del tiempo y entre países. 

Las variables de estas bases son construidas bajo un enfoque y estructura común,
con nombres, definiciones y desagregaciones estandarizadas y almacenadas en un 
solo archivo para cada año (o ronda) de un país. Actualmente, se disponen de 377
bases armonizadas correspondiente a las encuestas de hogares de 20 países de América
Latina y el Caribe desde 1986. 


## Estructura 
Harmonized ha sido organizada para distinguir dos niveles de desagregación: país y tipo de encuesta 

* La primera desagregación, PAÍS (acrónimo), tiene un total de  23 subcarpetas 
correspondientes a los países de la región que tienen armonizadas sus bases de datos, 
cada una nombrada con el acrónimo del país según la nomenclatura estándar 
ISO 3166-1 alpha-3.	

* La segunda desagregación, ENCUESTA (acrónimo), tiene tantas carpetas como tipos 
de encuestas tiene un país cuyas bases han sido armonizadas. Están nombradas según 
el acrónimo del tipo de encuesta de cada país. 

Dentro de cada una de estas carpetas se encuentran tres subcarpetas (i) data_arm, 
(ii) program, y (iii) log:

* En la **subcarpeta program** se guardan dos archivos de programación en STATA (archivos con extensión do): **(i)** PAIS_añoronda_mergeBID.do y **(ii)** PAIS_añoronda_variablesBID.do). El primer archivo corresponde a la rutina que permite juntar los módulos y, en algunos casos, los meses/trimestres de las encuestas originales en un archivo único (guardados en las carpetas [\\sdssrv03\surveys\survey\PAIS\ENCUESTA\año\ronda](\\sdssrv03\surveys\survey\PAIS\ENCUESTA\año\ronda) ) y el segundo a la rutina que permite generar las variables de las bases armonizadas a partir del archivo único.
*  Finalmente, la **subcarpeta log** contiene los archivos de tipo ASCII donde aparecen los cálculos en formato de texto, generados automáticamente por STATA a partir de los archivos do para la generación de variables y unión de las bases. Estos archivos mantienen el mismo nombre del archivo do pero con extensión .log. Es decir, PAIS_añoronda_variablesBID.log.

La armonización de las bases de datos de las encuestas de hogares es un proceso complejo debido que requiere tomar en consideración varios criterios de homogenización para asegurar la calidad y comparabilidad de los datos, dada las restricciones impuestas por la disponibilidad de información levantada en las mismas y las particularidades propias de cada país. En particular, debe tenerse en cuenta lo siguiente:

*	Los cambios metodológicos introducidos en ciertos años por las oficinas de estadística nacionales: en cobertura geográfica, cobertura temática, definición y opciones de respuesta de preguntas específicas, código/nombre de las variables en las bases de datos, marco muestral de viviendas asociados a la ejecución de nuevos censos de población, actualización retrospectiva de factores de ponderación, etc. 
* Las diferencias entre encuestas nacionales para capturar la información en los módulos/secciones comunes: edad en educación, empleo, migración, etnia/raza; periodos de referencia (diaria, semanal, mensual, trimestral) en empleo, ingresos, gastos; miembros informantes (jefe, jefe-cónyuge, todos) en etnia/raza; unidad monetaria (local, dólar u otras) para capturar parte de los ingresos relacionados con las remesas, pensiones, etc. 
*	Las diferencias entre encuestas nacionales respecto a la estructura de las bases de datos, en particular en la forma en que son almacenadas: horizontalmente (donde una línea es la información de un hogar o personas) o verticalmente (donde una línea es la información de ítems como los del equipamiento del hogar); con una variable por pregunta (como la edad, sexo) o con una variable para varias preguntas (como los ingresos de varias fuentes guardadas en una sola variable). En el marco de las bases de datos armonizadas, cada una es guardada de manera horizontal en una sola base de datos para cada año de cada país a nivel de individuos. 
*	La ausencia de documentación completa asociadas a las encuestas de hogares (cuestionarios, manuales, diccionario de variables, informe del diseño muestral, entre otros) que dificulta  el entendimiento, manejo y armonización de las bases de datos. 

---

## Cómo citar el uso del banco de datos armonizado

La información tomada de este banco de datos debe ser citada como:
> "Fuente: Banco Interamericano de Desarrollo: Banco de Datos Armonizado de las Encuestas de Hogares". 
Se sugiere hacer referencia a la fecha en que las bases de datos fueron consultadas, dado que la información contenida en ellas podría cambiar. Asimismo, se agradece una copia de las publicaciones o informes que utilicen la información contenida en este banco de datos para nuestros registros.



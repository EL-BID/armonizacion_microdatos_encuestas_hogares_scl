# Banco de Datos Armonizado de las Encuestas de Hogares

## Descripción

Las Encuestas de Hogares Armonizadas del Sector Social (SCL) contiene las bases de datos de las encuestas de hogares de América Latina y el Caribe (ALC) transformadas de forma tal que proporcionan información comparable a lo largo del tiempo y entre países. Con el objetivo de realizar el seguimiento y estandarizar del proceso de armonización de las bases originales, entregadas por cada uno de los institutos o departamentos nacionales de estadística, se cuenta con el repositorio “armonizacion_microdatos_encuestas_hogares_scl” que contiene los scripts de la programación para la transformación de las variables originales a variables estandarizadas. Las variables de estas bases armonizadas son construidas bajo un enfoque y estructura común, con nombres, definiciones y desagregaciones estandarizadas y almacenadas en un solo archivo para cada año (o ronda) de un país. Actualmente, las Encuestas de Hogares Armonizadas tiene bases de datos de 26 países de ALC desde 1986.  

## Estructura de trabajo

El repositorio “armonizacion_microdatos_encuestas_hogares_scl” cuenta con una estructura estandarizada tanto para el flujo de trabajo como para el almacenamiento de los scripts:

### Almacenamiento

El repositorio cuenta con 26 carpetas correspondientes a los países de la región, las cuales están nombradas con el acrónimo de país (ISO 3166-1 alpha-3) y una carpeta que hace referencia a documentación adicional (_DOCS).

**1)	Carpetas de país:** Cada una de las carpetas cuenta con un segundo nivel de desagregación la cual hace referencia a la ENCUESTA (el acrónimo estándar que la identifica). Existen tantas carpetas como tipos de encuestas tiene un país cuyas bases han sido armonizadas. 
A su vez, cada una de las carpetas de encuestas cuenta con un tercer nivel de desagregación la cual es denominada “program”. En esta carpeta se guardan dos archivos de programación en STATA (archivos con extensión do): **(i)** PAIS_añoronda_mergeBID.do y **(ii)** PAIS_añoronda_variablesBID.do. El primer archivo corresponde a la rutina que permite juntar los módulos y, en algunos casos, los meses/trimestres de las encuestas originales en un archivo único (las bases generadas se guardan en el servidor del Banco: \\sdssrv03\surveys) y el segundo a la rutina que permite generar las variables de las bases armonizadas a partir del archivo único.

**2)	Carpeta _DOCS:** En esta carpeta se encuentran documentos y scripts adicionales que ayudan al proceso de armonización. Los principales scripts a tener en cuenta son: **i) Labels&ExternalVars_Harmonized_DataBank.do** el cual contiene procesos adicionales que se generan dentro de la armonización (por ejemplo, introducir de las líneas de pobreza, agregar algunas etiquetas, entre otros) y **ii) CorreDoFilesArmonizacion.do** la armonización de todos los países.  

La armonización de las bases de datos de las encuestas de hogares requiere tomar en consideración varios criterios de homogenización para asegurar la calidad y comparabilidad de los datos, dada las restricciones impuestas por la disponibilidad de información levantada en las mismas y las particularidades propias de cada país. En particular, debe tenerse en cuenta lo siguiente:

*	Los cambios metodológicos introducidos en ciertos años por las oficinas de estadística nacionales: en cobertura geográfica, cobertura temática, definición y opciones de respuesta de preguntas específicas, código/nombre de las variables en las bases de datos, marco muestral de viviendas asociados a la ejecución de nuevos censos de población, actualización retrospectiva de factores de ponderación, etc. 
*	Las diferencias entre encuestas nacionales para capturar la información en los módulos/secciones comunes: edad en educación, empleo, migración, etnia/raza; periodos de referencia (diaria, semanal, mensual, trimestral) en empleo, ingresos, gastos; miembros informantes (jefe, jefe-cónyuge, todos) en etnia/raza; unidad monetaria (local, dólar u otras) para capturar parte de los ingresos relacionados con las remesas, pensiones, etc. 
*	Las diferencias entre encuestas nacionales respecto a la estructura de las bases de datos, en particular en la forma en que son almacenadas: horizontalmente (donde una línea es la información de un hogar o personas) o verticalmente (donde una línea es la información de ítems como los del equipamiento del hogar); con una variable por pregunta (como la edad, sexo) o con una variable para varias preguntas (como los ingresos de varias fuentes guardadas en una sola variable). En el marco de las bases de datos armonizadas, cada una es guardada de manera horizontal en una sola base de datos para cada año de cada país a nivel de individuos. 
*	La ausencia de documentación completa asociadas a las encuestas de hogares (cuestionarios, manuales, diccionario de variables, informe del diseño muestral, entre otros) que dificulta el entendimiento, manejo y armonización de las bases de datos. 

Para más información sobre la definición de variables, periodos, encuestas armonizadas revisar el documento **“D.1.1 Documentación armonización microdatos encuestas de hogares”**.

### Guía de usuario


#### Cómo contribuir

Antes de empezar a trabajar o contribuir en la armonización de las encuestas de hogares se deben tener en cuenta los siguientes puntos: 

**1.	Clonar repositorio**

Se debe clonar el repositorio **“armonizacion_microdatos_encuestas_hogares_scl”** a nivel local cada desktop personal. Para esto, se debe utilizar el programa GitHub Desktop. 

1.	Una vez instalado el programa GitHub Desktop se va al icono **“File”** y se selecciona **“Clone a repository”**.
2.	En **“Clone a repository”** se selecciona el repositorio “armonizacion_microdatos_encuestas_hogares_scl” y se escoge el path local donde se quiera guardar la información. Hacer clic en Clone. 

Una vez se tenga el repositorio sincronizado, se puede avanzar a generar las Branch de trabajo. 

**2.	Definir path de datos**

Para el trabajo con la armonización se deben precisar dos global principales con el objetivo de definir los path necesarios para generar la armonización de las bases. Los global se deben definir a nivel local. Se recomienda que los global se definan en el Do-File **“profile.do”** alojado en la carpeta local **“PERSONAL”** para que una vez se inicie el programa de STATA ya los global estén definidos:

1.	Para encontrar la carpeta **“PERSONAL”** utilizar el comando sysdir en Stata. En general, la carpeta está ubicada en **C:\Users\NAME\ado\personal\**
2.	Es muy probable que la carpeta no esté creada. Debido a esto, se debe crear bajo el nombre **“personal”**.
3.	Una vez se ubique la carpeta, se debe crear el Do-file bajo **“profile.do”** que debe ir alojado en la carpeta mencionada anteriormente. Dentro de Do-file se van a generar los global:

* global surveysFolder "\\sdssrv03\surveys"
* global gitFolder "C:\Users\LINAA\OneDrive - Inter-American Development Bank Group\Documents\GitHub"

Se debe tener en cuenta que los path pueden cambiar de acuerdo con el directorio local de cada uno de los desktops. 
Recuerde que **NO** se deben modificar los paths ya establecidos en cada uno de los scripts de Git.

#### GIT Workflow

Con el objetivo de mantener estructurado y estandarizado el proceso de armonización de las bases de las encuestas de hogares, el repositorio cuenta con dos branches principales: 1) Master y 2) Development que ayudan a minimizar los errores y hacer más efectivo el trabajo:

**1)	Master:** La versión contenida en esta Branch siempre es la versión más actualizada y la cual está revisada y aprobada para ejecutarse. Esta Branch no se debe modificar a menos que todos los cambios sean aprobados en la Branch de Development. 

**2)	Depeloment:** Esta Branch es la designada para testear y probar los cambios o adiciones que se realicen en los scripts. De esta Branch se desligan las Branch de cada una de lo feature que se generen para el trabajo en los scripts. 

Debido a que el trabajo de armonización principalmente se realiza de forma paralela entre varios desarrolladores, se requiere que cada uno trabaje con una branch personal donde se solucione o trabaje en el feature requerido y se deben seguir los siguientes pasos: 

1)	Para trabajar en el feature, se debe crear una Branch que sea la copia de la versión de Development. La Branch debe tener el nombre estandarizado “type-task-division”. 

    **a.	Type:** hace referencia a el proceso que se va a llevar a cabo (un feature, fix, refactor, test, docs, chore)
    
    **b.	Task:** Hace referencia a una breve descripción de la tarea a realizar
    
    **c.	Division:** El nombre de la división a la que pertenece el desarrollador que trabaja en el proceso. 
    
2)	Una vez terminado el proceso de modificación o ajuste de los scripts se debe realizar el pull request para realizar el merge. Se debe tener en cuenta que el merge siempre se debe solicitar para realiza en la Branch de Development. 
3)	Una vez se realiza la solitud de merge, se revisa y verifica que no existan errores en el nuevo pull antes de aceptar el merge a la brach principal. 

Para ampliar la explicación del flujo de trabajo en el repositorio ver **“M.2.1.2 Git Conventions.pptx”**.

---

## Cómo citar el uso del banco de datos armonizado

>"Fuente: Banco Interamericano de Desarrollo (año de consulta), Encuestas de Hogares Armonizadas de América Latina y el Caribe" 

>“Source: Inter-American Development Bank (year of consultation), Harmonized Household Surveys of Latin America and the Caribbean”

Se sugiere hacer referencia al año de consulta, dado que la información contenida en ellas podría cambiar. Asimismo, se agradece una copia de las publicaciones o informes que utilicen la información contenida en este banco de datos para nuestros registros.


### Limitation of responsibilities
---
The IDB is not responsible, under any circumstance, for damage or compensation, moral or patrimonial; direct or indirect; accessory or special; or by way of consequence, foreseen or unforeseen, that could arise:

I. Under any concept of intellectual property, negligence or detriment of another part theory; I
ii. Following the use of the Digital Tool, including, but not limited to defects in the Digital Tool, or the loss or inaccuracy of data of any kind. The foregoing includes expenses or damages associated with communication failures and / or malfunctions of computers, linked to the use of the Digital Tool.
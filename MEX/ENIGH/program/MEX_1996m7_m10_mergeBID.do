* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\MEX\ENIGH\1996\m7_m10\data_orig"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "1996"
local ronda m7_m10

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (Nueva construcción)
Round: Julio-Octubre
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 6 de Agosto de 2015

							SCL/LMK - IADB
****************************************************************************/



*Mayra Sáenz - Agosto 2015: Se realiza el merge con base en la sintaxis de CONEVAL, 
*pero con algunas modificaciones, y generando nuevas variables.



/*Marzo 7 de 2006
Arreglo horastot: Se definia como horas en la actividad secuandaria y no como la sumatoria 
del total de horas trabajadas en la semana

programa original:
gen horastot_ci=hr_sem_s  if emp3_ci==1 

Abril 10, 2006 (Analia)
The following line:
egen ynlm_ci=rsum( utilidades  ayuda otros remesasext)
replace ynlm_ci=. if emp3_ci==0
was replaced with
egen ynlm_ci=rsum(utilidades  ayuda otros remesasext)
replace ynlm_ci=. if utilidades==. & ayuda==. & otros==. & remesasext==.

June 20, 2006 (Analia)
replace pared_ch=1 if muros1>=9 & muros1<=14
was replaced with
replace pared_ch=1 if muros1>=5 & muros1<=14

Agosto 1, 2006 (Victoria)
La variable de anios de educacion fue modificada de modo de ser comparbale y coherente entre los distintos anios.
El codigo anterior se puede ver en la seccion de educacion con * adelante del comando
El criterio utilizado para asignar a cada nivel un determinado numero de anios de estudio se puede ver en el
siguiente documento: ${surveysFolder}\Sociometro_2005\documentation\Years of education in Mexico. 
Tambien se cambiaron las varianles edupi, edupc, edusi, edusc y etc de modo de ser coherente con los siguientes
anios: Primaria 6, Secundaria 6 (12 acumulado) y Terciaria 5 (17 acumulado).  


Febrero 8, 2007 (Victoria)
Si nom_empr==3 tambien se es empleado publico por lo que se debe cambiar el codigo de la varibale spublico
OLD CODE:
gen spublico_ci=.
replace spublico_ci=1 if nom_empr=="1" & emp3_ci==1
replace spublico_ci=0 if nom_empr~="1" & emp3_ci==1
replace spublico_ci=. if nom_empr =="."

*/

****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
************************* PARA MEXICO 1996 *********************************
****************************************************************************

*********************************************************

/*Este programa debe ser utilizado con el Software Stata 
versión 8 o superior. 

Todas las bases de datos de la ENIGH pueden ser obtenidas 
en la página de Internet del INEGI, www.inegi.gob.mx, y 
deben estar convertidas a formato *.dta. 

Es importante renombrar las bases que se descargan del 
INEGI de acuerdo a la siguiente estructura, con la finalidad
de que el programa no tenga problemas al identificar las bases
que se descargan del INEGI: 

Base de ingresos: ingreso96.dta
Base de gasto monetario: gasto96.dta
Base de gasto no monetario: nomonetario96.dta
Base de concentrado: concentrado96.dta

En este programa se utilizan tres tipos de archivos, los 
cuales están ubicados en las siguientes carpetas:

1) Bases originales: "${surveysFolder}\pobreza ingresos\1996\ENIGH"
2) Bitácoras: "${surveysFolder}\pobreza ingresos\1996\Log"
3) Bases generadas: "${surveysFolder}\pobreza ingresos\1996\Resultados"

Para cambiar estas ubicaciones, se modifican los siguientes
globals  

gl data="${surveysFolder}\pobreza ingresos\1996\ENIGH" 
gl log="${surveysFolder}\pobreza ingresos\1996\Log" 
gl bases="${surveysFolder}\pobreza ingresos\1996\Resultados" 


log using "$log\Pobreza por ingresos 1996.txt", text replace */

*********************************************************
*
*	PROGRAMA PARA LA MEDICIÓN DE LA POBREZA 1996
*
*********************************************************

*CONEVAL Última modificación: 24 de julio del 2013

*********************************************************

*Parte I

/*Creación del ingreso monetario deflactado a pesos de 
agosto del 1996.

Los deflactores se crean previamente a partir del INPC 
general (ver Nota Técnica).

Se crea la base: "$ruta\ingreso_deflactado96.dta" a nivel de folio.*/

******************************************************** 


*Ingresos 

use "$ruta\ingresos.dta", clear

/*La variable meses define el mes de levantamiento de la 
información para el último mes y los 5 anteriores, ésta toma cuatro valores:

070605040302
080706050403
090807060504
100908070605

indicando que la encuesta fue levantada entre los meses de
julio y octubre, y por tanto, al preguntarse por los ingresos
de los últimos seis meses, se recolectó información 
correspondiente a febrero-julio, marzo-agosto, abril-septiembre,
y mayo-octubre */

*Definición de los deflactores 1996 

scalar ene96= 0.87422962  
scalar feb96= 0.89463378  
scalar mar96= 0.91432801  
scalar abr96= 0.94031998  
scalar may96= 0.95745986  
scalar jun96= 0.97305058  
scalar jul96= 0.98688287  
scalar ago96= 1.00000000  
scalar sep96= 1.01598869  
scalar oct96= 1.02867018  
scalar nov96= 1.04425566  
scalar dic96= 1.07769075  

*Así, se procede a dividir las columnas de ingreso por su deflactor correspondiente 

rename ing1 ing_1  
rename ing2 ing_2  
rename ing3 ing_3  
rename ing4 ing_4  
rename ing5 ing_5  
rename ing6 ing_6  

replace ing_6=ing_6/feb96 if meses=="070605040302" 
replace ing_5=ing_5/mar96 if meses=="070605040302" 
replace ing_4=ing_4/abr96 if meses=="070605040302" 
replace ing_3=ing_3/may96 if meses=="070605040302" 
replace ing_2=ing_2/jun96 if meses=="070605040302" 
replace ing_1=ing_1/jul96 if meses=="070605040302" 

replace ing_6=ing_6/mar96 if meses=="080706050403" 
replace ing_5=ing_5/abr96 if meses=="080706050403" 
replace ing_4=ing_4/may96 if meses=="080706050403" 
replace ing_3=ing_3/jun96 if meses=="080706050403" 
replace ing_2=ing_2/jul96 if meses=="080706050403" 
replace ing_1=ing_1/ago96 if meses=="080706050403" 

replace ing_6=ing_6/abr96 if meses=="090807060504" 
replace ing_5=ing_5/may96 if meses=="090807060504" 
replace ing_4=ing_4/jun96 if meses=="090807060504" 
replace ing_3=ing_3/jul96 if meses=="090807060504" 
replace ing_2=ing_2/ago96 if meses=="090807060504" 
replace ing_1=ing_1/sep96 if meses=="090807060504" 

replace ing_6=ing_6/may96 if meses=="100908070605" 
replace ing_5=ing_5/jun96 if meses=="100908070605" 
replace ing_4=ing_4/jul96 if meses=="100908070605" 
replace ing_3=ing_3/ago96 if meses=="100908070605" 
replace ing_2=ing_2/sep96 if meses=="100908070605" 
replace ing_1=ing_1/oct96 if meses=="100908070605" 

replace ing_6=ing_6/jun96 if meses=="111009080706" 
replace ing_5=ing_5/jul96 if meses=="111009080706" 
replace ing_4=ing_4/ago96 if meses=="111009080706" 
replace ing_3=ing_3/sep96 if meses=="111009080706" 
replace ing_2=ing_2/oct96 if meses=="111009080706" 
replace ing_1=ing_1/nov96 if meses=="111009080706" 

*Mayra Sáenz Julio 2015- Mes de referencia

g mes_ref = substr(meses, 1, 2)
destring mes_ref, replace

/*Una vez realizada la deflactación, se procede a obtener el 
promedio de ingreso mensual en los últimos 6 meses para cada
persona y clave de ingreso */

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6) 

gen double ing_mon =ing_mens  if clave>="P001" & clave<="P029" 
gen double ing_lab =ing_mens  if clave>="P001" & clave<="P015" 

**Modificación Mayra Sáenz - Julio 2015 : Son ingresos laborales de P001-P014, se desagrega por actividad principal y secundaria
gen     ocuprisec = 1 if empleo =="1" 
replace ocuprisec = 2 if empleo !="1"

forval j = 1/2 {
gen double ing_trab`j'=ing_mens if (clave>="P001" & clave<="P005") & ocuprisec == `j'
gen double ing_negp`j'=ing_mens if (clave>="P006" & clave<="P015") & ocuprisec == `j'
}

gen double ing_trab=ing_mens  if clave>="P001" & clave<="P005" 
gen double ing_negp=ing_mens  if clave>="P006" & clave<="P015" 
gen double ing_rent=ing_mens  if clave>="P016" & clave<="P022" 
gen double ing_tran=ing_mens  if clave>="P023" & clave<="P029" 

*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens  if  (clave=="P023")                                      //Jubilaciones y pensiones
g double trat_pr  = ing_mens  if  ((clave>="P024" & clave<="P025") | clave=="P028")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por despido y accidentes de trabajo, Ingresos provenientes de otros paises
g double trat_pu  = ing_mens  if  (clave=="P029")                                      //Desde 1996 aparece PROCAMPO
g double dona_pu  = 0                                                                 
g double dona_pr  = ing_mens if  (clave=="P026" | clave=="P027")                      //Becas y donaciones provenientes de Instituciones, Regalos y donativos originados dentro del país
g double otros   = ing_mens  if ((clave>="P030" & clave<="P031"))                     //Venta de automóviles, aparatos electricos de segunda mano, etc.  Otros ingresos corrientes no considerados en los anteriores, (viÃ¡ticos, etc.).
g double remesas = ing_mens  if  (clave=="P028")                                      //Ingresos provenientes de otros paises

*Modificación Mayra Sáenz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}


*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.


/*Así, una vez obtenida esta descomposición del ingreso según las
fuentes por las que se recibe, se procede a estimar el total de ingresos
que cada hogar recibe por cada una de ellas, y se guarda la base
en un archivo aparte */

collapse (sum) ing_mens ing_mon ing_lab ing_trab* ing_negp* ing_rent ing_tran ypension trat_pr trat_pu dona_pu dona_pr otros remesas P* (mean) mes_ref, by(folio num_ren)

label var folio     "Identificador del hogar"
label var ing_mon   "Ingreso corriente monetario del individuo"  //Antes era ingreso del hogar.
label var ing_lab   "Ingreso laboral act. princ. y sec"
label var ing_trab1 "Ingreso por remuneraciones al trabajo act. princ."
label var ing_negp1 "Ingresos por negocios propios act. princ."
label var ing_trab2 "Ingreso por remuneraciones al trabajo act. sec."
label var ing_negp2 "Ingresos por negocios propios act. sec."
label var ing_rent  "Ingresos por renta de la propiedad"
label var ing_tran  "Ingresos por transferencias"
label var ypension  "Ingresos por Jubilacion"
label var trat_pr   "Transferencias monetarias y no monetarias privadas" 
label var trat_pu   "Transferencias monetarias y no monetarias publicas"
label var dona_pu   "Donaciones Publicas"
label var dona_pr   "Donaciones Privadas"
label var remesas   "Remesas"
label var otros     "Otros"


sort folio num_ren, stable

rename folio folio1              //Modificado Mayra Sáenz - Julio 2015
gen folio = substr(folio1,4,100) //Modificado Mayra Sáenz - Julio 2015

saveold "$ruta\ingreso_deflactado96_per.dta", replace 

*********************************************************

/*Parte II

Creación del gasto monetario deflactado a pesos de agosto 
de 1996.

Los deflactores se crean previamente a partir del INPC 
según aparece en la Nota Técnica.

"$ruta\gastomonetario_def96.dta"

a nivel de folio.*/

********************************************************* 

*Gastos 

use "$ruta\gastos96.dta", clear

/*En el caso de la información de gasto, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual está dada dentro
del folio de identificación del hogar en la última posición. En 
primer lugar se obtiene una variable que identifique la decena de
levantamiento */

gen decena=real(substr(folio,7,1)) 
tab decena 

/*Así, se puede observar que se levantó información en 9 decenas,
correspondientes a:

Decena                           Periodo de Levantamiento

0	 |	del 12 al 18 de agosto
1	 |	del 22 al 28 de agosto 
2	 |	del 01 al 07 de septiembre
3	 |	del 11 al 17 de septiembre
4	 |	del 21 al 27 de septiembre
5	 |	del 01 al 07 de octubre
6	 |	del 11 al 17 de octubre
7	 |	del 21 al 27 de octubre 
8	 |	del 31 de octubre al 7 de noviembre
9	 |	del 10 al 16 de noviembre

Así, para realizar el análisis, y conforme a lo establecido en la
información de la ENIGH2000, se tomará como periodos de referencia

              Periodo de Referencia
Decena|	/Semanal     /Mensual     /Trimestral           /Semestral
	
0     |	/Agosto	     /Julio       /Mayo a Julio         /Febrero a Julio
1     |	/Agosto      /Julio       /Mayo a Julio         /Febrero a Julio
2     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
3     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
4     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
5     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
6     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
7     |	/Octubre     /Septiemre   /Julio a Septiembre   /Abril a Septiembre
8     |	/Octubre     /Octubre     /Agosto a Octubre     /Mayo a Octubre
9     |	/Noviembre   /Octubre     /Agosto a Octubre     /Mayo a Octubre
	

Así, se utilizarán los deflactores mensuales para el caso de las referencias semanales y mensuales
y los promedios de los meses de referencia en las referencias Trimestrales y Semestrales 

*Los rubros a considerar, según la metodología del CTMP, serán:

Rubro                                  /Periodicidad     /Nombre del deflactor
Alimentos y Bebidas No Alcohólicas     /Semanal          /d11wmes
Bebidas alcohólicas y tabaco           /Semanal          /d12wmes
Vestido y Calzado                      /Trimestral       /d2tmesini
Vivienda, servicios de conservación    /Mensual          /d3mmes
energía y combustible
Estimación del Alquiler                /No se deflactara /No se deflactara
Artículos y Servicios de Limpieza      /Mensual          /d42mmes
Cristalería, utensilios domésticos     /Trimestral       /d42tmesini
y blancos
Enseres domésticos y muebles           /Semestral        /d41smesini
Cuidados de la salud	               /Trimestral       /d51tmesini
Transporte público                     /Semanal          /d611wmes
Transporte foráneo, vehículos          /Semestral        /d6smesini
Comunicaciones                         /Mensual          /d6mmes
Educación básica                       /Mensual          /d7mmes
Artículos y servicios para el cuidado  /Mensual          /d23mmes
personal
Accesorios personales                  /Trimestral       /d23tmesini
Otros gastos diversos y transf.        /Semestral        /dINPCsmesini
Regalos otorgados                      /Semestral        /dINPCsmesini

A continuación se definen escalares que contienen cada uno de estos deflactores, a partir del
archivo de excel anexo (no se requieren deflactores mensuales antes de julio, por lo que no se
incorporan, si bien sí fueron calculados) */

*Rubro 1.1 semanal 

scalar d11w08	= 1.0000000  
scalar d11w09	= 1.0115161  
scalar d11w10	= 1.0229494  
scalar d11w11	= 1.0411878  

*Rubro 1.2 semanal 

scalar d12w08	= 1.0000000  
scalar d12w09	= 1.0179260  
scalar d12w10	= 1.0232341  
scalar d12w11	= 1.0257628  

*Rubro 2 trimestral 

scalar d2t05	= 0.9679863  
scalar d2t06	= 0.9847844  
scalar d2t07	= 1.0000112  
scalar d2t08    = 1.0163106  

*Rubro 3 mensual 

scalar d3m07	= 0.9955227  
scalar d3m08	= 1.0000000  
scalar d3m09	= 1.0111630  
scalar d3m10	= 1.0318597  
scalar d3m11	= 1.0518675  

*Rubro 4.2 mensual 

scalar d42m07	= 0.9753953  
scalar d42m08	= 1.0000000  
scalar d42m09	= 1.0164480  
scalar d42m10	= 1.0309184  
scalar d42m11	= 1.0514951  

*Rubro 4.2 trimestral 

scalar d42t05	= 0.9540421  
scalar d42t06	= 0.9763979  
scalar d42t07   = 0.9972811  
scalar d42t08	= 1.0157888  

*Rubro 4.1 semestral 

scalar d41s02	= 0.9417798  
scalar d41s03	= 0.9585164  
scalar d41s04	= 0.9744704  
scalar d41s05	= 0.9898757  

*Rubro 5.1 trimestral 

scalar d51t05	= 0.9732526  
scalar d51t06	= 0.9891866  
scalar d51t07	= 1.0014853  
scalar d51t08	= 1.0146306  

*Rubro 6.1.1 semanal 

scalar d611w07	= 0.9889061  
scalar d611w08  = 1.0000000  
scalar d611w09  = 1.0084400  
scalar d611w10  = 1.0109365  
scalar d611w11  = 1.0196352  

*Rubro 6 mensual 

scalar d6m07	= 0.9897885  
scalar d6m08	= 1.0000000  
scalar d6m09    = 1.0114321  
scalar d6m10    = 1.0208988  
scalar d6m11    = 1.0339737  

*Rubro 6 semestral 

scalar d6s02 	= 0.9512171  
scalar d6s03	= 0.9673956  
scalar d6s04	= 0.9823688  
scalar d6s05	= 0.9937884  

*Rubro 7 mensual 

scalar d7m07	= 0.9851652  
scalar d7m08	= 1.0000000  
scalar d7m09	= 1.0626332  
scalar d7m10	= 1.0677936  
scalar d7m11	= 1.0736744  

*Rubro 2.3 mensual 

scalar d23m07	= 0.9935548  
scalar d23m08	= 1.0000000  
scalar d23m09	= 1.0105628  
scalar d23m10	= 1.0154785  
scalar d23m11	= 1.0246668  

*Rubro 2.3 trimestral 

scalar d23t05	= 0.9862840  
scalar d23t06	= 0.9941306  
scalar d23t07	= 1.0013725  
scalar d23t08	= 1.0086804  

*INPC semestral 

scalar dINPCs02= 0.9444458  
scalar dINPCs03= 0.9620069  
scalar dINPCs04= 0.9789503  
scalar dINPCs05= 0.9936754  

*Una vez definidos los deflactores, se procede a limpiar la base de gasto a fin de incorporar sólo
*los rubros de gasto solicitados, y agregarlos en los rubros más generales definidos por el CTMP 

drop if (clave=="G002") 

gen double gasm=gas_tri/3 

*Gasto en alimentos deflactado 

gen ali_m=gasm if (clave>="A001" & clave<="A194") | (clave>="A205" & clave<="A208") 

replace ali_m=ali_m/d11w08 if decena==0 
replace ali_m=ali_m/d11w08 if decena==1 
replace ali_m=ali_m/d11w09 if decena==2 
replace ali_m=ali_m/d11w09 if decena==3 
replace ali_m=ali_m/d11w09 if decena==4 
replace ali_m=ali_m/d11w10 if decena==5 
replace ali_m=ali_m/d11w10 if decena==6 
replace ali_m=ali_m/d11w10 if decena==7 
replace ali_m=ali_m/d11w10 if decena==8 
replace ali_m=ali_m/d11w11 if decena==9 

*Gasto en Alcohol y Tabaco deflactado 

gen alta_m=gasm if (clave>="A195" & clave<="A204") | (clave>="A209" & clave<="A211") 

replace alta_m=alta_m/d12w08 if decena==0 
replace alta_m=alta_m/d12w08 if decena==1 
replace alta_m=alta_m/d12w09 if decena==2 
replace alta_m=alta_m/d12w09 if decena==3 
replace alta_m=alta_m/d12w09 if decena==4 
replace alta_m=alta_m/d12w10 if decena==5 
replace alta_m=alta_m/d12w10 if decena==6 
replace alta_m=alta_m/d12w10 if decena==7 
replace alta_m=alta_m/d12w10 if decena==8 
replace alta_m=alta_m/d12w11 if decena==9 

*Gasto en Vestido y Calzado deflactado 

gen veca_m=gasm if (clave>="H001" & clave<="H028") | (clave>="H031" & clave<="H055") 

replace veca_m=veca_m/d2t05 if decena==0 
replace veca_m=veca_m/d2t05 if decena==1 
replace veca_m=veca_m/d2t06 if decena==2 
replace veca_m=veca_m/d2t06 if decena==3 
replace veca_m=veca_m/d2t06 if decena==4 
replace veca_m=veca_m/d2t07 if decena==5 
replace veca_m=veca_m/d2t07 if decena==6 
replace veca_m=veca_m/d2t07 if decena==7 
replace veca_m=veca_m/d2t08 if decena==8 
replace veca_m=veca_m/d2t08 if decena==9 

*Gasto en Vivienda deflactado 

gen viv_m=gasm if (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G014") | (clave>="G016" & clave<="G033")  

replace viv_m=viv_m/d3m07 if decena==0 
replace viv_m=viv_m/d3m07 if decena==1 
replace viv_m=viv_m/d3m08 if decena==2 
replace viv_m=viv_m/d3m08 if decena==3 
replace viv_m=viv_m/d3m08 if decena==4 
replace viv_m=viv_m/d3m09 if decena==5 
replace viv_m=viv_m/d3m09 if decena==6 
replace viv_m=viv_m/d3m09 if decena==7 
replace viv_m=viv_m/d3m10 if decena==8 
replace viv_m=viv_m/d3m10 if decena==9 

*Gasto en Artículos de Limpieza deflactado 

gen lim_m=gasm if (clave>="C001" & clave<="C024") 

replace lim_m=lim_m/d42m07 if decena==0 
replace lim_m=lim_m/d42m07 if decena==1 
replace lim_m=lim_m/d42m08 if decena==2 
replace lim_m=lim_m/d42m08 if decena==3 
replace lim_m=lim_m/d42m08 if decena==4 
replace lim_m=lim_m/d42m09 if decena==5 
replace lim_m=lim_m/d42m09 if decena==6 
replace lim_m=lim_m/d42m09 if decena==7 
replace lim_m=lim_m/d42m10 if decena==8 
replace lim_m=lim_m/d42m10 if decena==9 

*Gasto en Cristalería y Blancos deflactado 

gen cris_m=gasm if (clave>="I001" & clave<="I026") 

replace cris_m=cris_m/d42t05 if decena==0 
replace cris_m=cris_m/d42t05 if decena==1 
replace cris_m=cris_m/d42t06 if decena==2 
replace cris_m=cris_m/d42t06 if decena==3 
replace cris_m=cris_m/d42t06 if decena==4 
replace cris_m=cris_m/d42t07 if decena==5 
replace cris_m=cris_m/d42t07 if decena==6 
replace cris_m=cris_m/d42t07 if decena==7 
replace cris_m=cris_m/d42t08 if decena==8 
replace cris_m=cris_m/d42t08 if decena==9 

*Gasto en Enseres deflactado 

gen ens_m=gasm if (clave>="K001" & clave<="K030") 

replace ens_m=ens_m/d41s02 if decena==0 
replace ens_m=ens_m/d41s02 if decena==1 
replace ens_m=ens_m/d41s03 if decena==2 
replace ens_m=ens_m/d41s03 if decena==3 
replace ens_m=ens_m/d41s03 if decena==4 
replace ens_m=ens_m/d41s04 if decena==5 
replace ens_m=ens_m/d41s04 if decena==6 
replace ens_m=ens_m/d41s04 if decena==7 
replace ens_m=ens_m/d41s05 if decena==8 
replace ens_m=ens_m/d41s05 if decena==9 

*Gasto en Salud deflactado 

gen sal_m=gasm if (clave>="J001" & clave<="J045") 

replace sal_m=sal_m/d51t05 if decena==0 
replace sal_m=sal_m/d51t05 if decena==1 
replace sal_m=sal_m/d51t06 if decena==2 
replace sal_m=sal_m/d51t06 if decena==3 
replace sal_m=sal_m/d51t06 if decena==4 
replace sal_m=sal_m/d51t07 if decena==5 
replace sal_m=sal_m/d51t07 if decena==6 
replace sal_m=sal_m/d51t07 if decena==7 
replace sal_m=sal_m/d51t08 if decena==8 
replace sal_m=sal_m/d51t08 if decena==9 

*Gasto en Transporte Público deflactado 

gen tpub_m=gasm if (clave>="B001" & clave<="B007") 

replace tpub_m=tpub_m/d611w08 if decena==0 
replace tpub_m=tpub_m/d611w08 if decena==1 
replace tpub_m=tpub_m/d611w09 if decena==2 
replace tpub_m=tpub_m/d611w09 if decena==3 
replace tpub_m=tpub_m/d611w09 if decena==4 
replace tpub_m=tpub_m/d611w10 if decena==5 
replace tpub_m=tpub_m/d611w10 if decena==6 
replace tpub_m=tpub_m/d611w10 if decena==7 
replace tpub_m=tpub_m/d611w10 if decena==8 
replace tpub_m=tpub_m/d611w11 if decena==9 

*Gasto en Transporte Foráneo deflactado 

gen tfor_m=gasm if (clave>="M001" & clave<="M018") | (clave>="F006" & clave<="F010") 

replace tfor_m=tfor_m/d6s02 if decena==0 
replace tfor_m=tfor_m/d6s02 if decena==1 
replace tfor_m=tfor_m/d6s03 if decena==2 
replace tfor_m=tfor_m/d6s03 if decena==3 
replace tfor_m=tfor_m/d6s03 if decena==4 
replace tfor_m=tfor_m/d6s04 if decena==5 
replace tfor_m=tfor_m/d6s04 if decena==6 
replace tfor_m=tfor_m/d6s04 if decena==7 
replace tfor_m=tfor_m/d6s05 if decena==8 
replace tfor_m=tfor_m/d6s05 if decena==9 

*Gasto en Comunicaciones deflactado 

gen com_m=gasm if (clave>="F001" & clave<="F005") 

replace com_m=com_m/d6m07 if decena==0 
replace com_m=com_m/d6m07 if decena==1 
replace com_m=com_m/d6m08 if decena==2 
replace com_m=com_m/d6m08 if decena==3 
replace com_m=com_m/d6m08 if decena==4 
replace com_m=com_m/d6m09 if decena==5 
replace com_m=com_m/d6m09 if decena==6 
replace com_m=com_m/d6m09 if decena==7 
replace com_m=com_m/d6m10 if decena==8 
replace com_m=com_m/d6m10 if decena==9 

*Gasto en Educación y Recreación deflactado 

gen edre_m=gasm if (clave>="E001" & clave<="E034") | (clave>="H029" & clave<="H030") | (clave>="L001" & clave<="L027") | (clave>="N003" & clave<="N005") 

replace edre_m=edre_m/d7m07 if decena==0 
replace edre_m=edre_m/d7m07 if decena==1 
replace edre_m=edre_m/d7m08 if decena==2 
replace edre_m=edre_m/d7m08 if decena==3 
replace edre_m=edre_m/d7m08 if decena==4 
replace edre_m=edre_m/d7m09 if decena==5 
replace edre_m=edre_m/d7m09 if decena==6 
replace edre_m=edre_m/d7m09 if decena==7 
replace edre_m=edre_m/d7m10 if decena==8 
replace edre_m=edre_m/d7m10 if decena==9 

*Gasto en Educación Básica deflactado 

gen edba_m=gasm if (clave>="E002" & clave<="E003") | (clave=="E015") | (clave=="H029") 


replace edba_m=edba_m/d7m07 if decena==0 
replace edba_m=edba_m/d7m07 if decena==1 
replace edba_m=edba_m/d7m08 if decena==2 
replace edba_m=edba_m/d7m08 if decena==3 
replace edba_m=edba_m/d7m08 if decena==4 
replace edba_m=edba_m/d7m09 if decena==5 
replace edba_m=edba_m/d7m09 if decena==6 
replace edba_m=edba_m/d7m09 if decena==7 
replace edba_m=edba_m/d7m10 if decena==8 
replace edba_m=edba_m/d7m10 if decena==9 

*Modificación Mayra Saenz - Abril 2017: Se desagrega el gasto unicamente en educación (se excluye recreación)

*Gasto monetario sólo educación
gen edu_gtosm=gasm if (clave>="E001" & clave<="E012") | (clave =="E035" ) | (clave=="E013")  | (clave>="E021" & clave<="E025")  

replace edu_gtosm=edu_gtosm/d7m07 if decena==0 
replace edu_gtosm=edu_gtosm/d7m07 if decena==1 
replace edu_gtosm=edu_gtosm/d7m08 if decena==2 
replace edu_gtosm=edu_gtosm/d7m08 if decena==3 
replace edu_gtosm=edu_gtosm/d7m08 if decena==4 
replace edu_gtosm=edu_gtosm/d7m09 if decena==5 
replace edu_gtosm=edu_gtosm/d7m09 if decena==6 
replace edu_gtosm=edu_gtosm/d7m09 if decena==7 
replace edu_gtosm=edu_gtosm/d7m10 if decena==8 
replace edu_gtosm=edu_gtosm/d7m10 if decena==9 

gen cuip_m=gasm if (clave>="D001" & clave<="D024") | (clave=="H064") 

replace cuip_m=cuip_m/d23m07 if decena==0 
replace cuip_m=cuip_m/d23m07 if decena==1 
replace cuip_m=cuip_m/d23m08 if decena==2 
replace cuip_m=cuip_m/d23m08 if decena==3 
replace cuip_m=cuip_m/d23m08 if decena==4 
replace cuip_m=cuip_m/d23m09 if decena==5 
replace cuip_m=cuip_m/d23m09 if decena==6 
replace cuip_m=cuip_m/d23m09 if decena==7 
replace cuip_m=cuip_m/d23m10 if decena==8 
replace cuip_m=cuip_m/d23m10 if decena==9 

*Gasto en Accesorios Personales deflactado 

gen accp_m=gasm if (clave>="H056" & clave<="H063") | (clave=="H065") 

replace accp_m=accp_m/d23t05 if decena==0 
replace accp_m=accp_m/d23t05 if decena==1 
replace accp_m=accp_m/d23t06 if decena==2 
replace accp_m=accp_m/d23t06 if decena==3 
replace accp_m=accp_m/d23t06 if decena==4 
replace accp_m=accp_m/d23t07 if decena==5 
replace accp_m=accp_m/d23t07 if decena==6 
replace accp_m=accp_m/d23t07 if decena==7 
replace accp_m=accp_m/d23t08 if decena==8 
replace accp_m=accp_m/d23t08 if decena==9 

*Gasto en Otros Gastos y Transferencias deflactado 

gen otr_m=gasm if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") 

replace otr_m=otr_m/dINPCs02 if decena==0 
replace otr_m=otr_m/dINPCs02 if decena==1 
replace otr_m=otr_m/dINPCs03 if decena==2 
replace otr_m=otr_m/dINPCs03 if decena==3 
replace otr_m=otr_m/dINPCs03 if decena==4 
replace otr_m=otr_m/dINPCs04 if decena==5 
replace otr_m=otr_m/dINPCs04 if decena==6 
replace otr_m=otr_m/dINPCs04 if decena==7 
replace otr_m=otr_m/dINPCs05 if decena==8 
replace otr_m=otr_m/dINPCs05 if decena==9 

*Gasto en Regalos Otorgados deflactado 

gen reda_m=gasm if (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") | (clave=="N013") 


replace reda_m=reda_m/dINPCs02 if decena==0 
replace reda_m=reda_m/dINPCs02 if decena==1 
replace reda_m=reda_m/dINPCs03 if decena==2 
replace reda_m=reda_m/dINPCs03 if decena==3 
replace reda_m=reda_m/dINPCs03 if decena==4 
replace reda_m=reda_m/dINPCs04 if decena==5 
replace reda_m=reda_m/dINPCs04 if decena==6 
replace reda_m=reda_m/dINPCs04 if decena==7 
replace reda_m=reda_m/dINPCs05 if decena==8 
replace reda_m=reda_m/dINPCs05 if decena==9 

collapse (sum) *_m edu_gtosm, by(folio) 
egen gmontot=rsum(*_m) 
sort folio 

rename edu_gtosm edu_gtosmh


save "$ruta\gastomonetario_def96.dta", replace 

*********************************************************

/*Parte III

Creación del Gasto No Monetario deflactado a pesos de 
Agosto del 1996.

Los deflactores se crean previamente a partir del INPC 
desagregado, según archivo de excel anexo.

Se crean las bases:
	"$ruta\gastonomonetario_def96.dta"
a nivel de clave de gasto, y
	"$ruta\auto_def96.dta"
	"$ruta\esp_def96.dta"
	"$ruta\reg_def96.dta"
a nivel de folio.*/

********************************************************* 

*No Monetario 

use "$ruta\nomon.dta", clear

/*En el caso de la información no meonetario, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual está dada dentro
del folio de identificación del hogar en la última posición. En 
primer lugar se obtiene una variable que identifique la decena de
levantamiento */


gen decena=real(substr(folio,7,1)) 
tab decena 

/*Así, se puede observar que se levantó información en 9 decenas,
correspondientes a:

Decena                           Periodo de Levantamiento

0	 |	del 12 al 18 de agosto
1	 |	del 22 al 28 de agosto 
2	 |	del 01 al 07 de septiembre
3	 |	del 11 al 17 de septiembre
4	 |	del 21 al 27 de septiembre
5	 |	del 01 al 07 de octubre
6	 |	del 11 al 17 de octubre
7	 |	del 21 al 27 de octubre 
8	 |	del 31 de octubre al 7 de noviembre
9	 |	del 10 al 16 de noviembre

Así, para realizar el análisis, y conforme a lo establecido en la
información de la ENIGH1998, se tomará como periodos de referencia

              Periodo de Referencia
Decena|	/Semanal     /Mensual     /Trimestral           /Semestral
	
0     |	/Agosto	     /Julio       /Mayo a Julio         /Febrero a Julio
1     |	/Agosto      /Julio       /Mayo a Julio         /Febrero a Julio
2     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
3     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
4     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
5     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
6     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
7     |	/Octubre     /Septiemre   /Julio a Septiembre   /Abril a Septiembre
8     |	/Octubre     /Octubre     /Agosto a Octubre     /Mayo a Octubre
9     |	/Noviembre   /Octubre     /Agosto a Octubre     /Mayo a Octubre

Así, se utilizarán los deflactores mensuales para el caso de las referencias semanales y mensuales
y los promedios de los meses de referencia en las referencias Trimestrales y Semestrales 

*Los rubros a considerar, según la metodología del CTMP, serán:

Rubro                                  /Periodicidad     /Nombre del deflactor
Alimentos y Bebidas No Alcohólicas     /Semanal          /d11wmes
Bebidas alcohólicas y tabaco           /Semanal          /d12wmes
Vestido y Calzado                      /Trimestral       /d2tmesini
Vivienda, servicios de conservación    /Mensual          /d3mmes
energía y combustible
Estimación del Alquiler                /No se deflactara /No se deflactara
Artículos y Servicios de Limpieza      /Mensual          /d42mmes
Cristalería, utensilios domésticos     /Trimestral       /d42tmesini
y blancos
Enseres domésticos y muebles           /Semestral        /d41smesini
Cuidados de la salud	               /Trimestral       /d51tmesini
Transporte público                     /Semanal          /d611wmes
Transporte foráneo, vehículos          /Semestral        /d6smesini
Comunicaciones                         /Mensual          /d6mmes
Educación básica                       /Mensual          /d7mmes
Artículos y servicios para el cuidado  /Mensual          /d23mmes
personal
Accesorios personales                  /Trimestral       /d23tmesini
Otros gastos diversos y transf.        /Semestral        /dINPCsmesini
Regalos otorgados                      /Semestral        /dINPCsmesini
*/
*Definición de los deflactores 

*Rubro 1.1 semanal 

scalar d11w08	= 1.0000000  
scalar d11w09	= 1.0115161  
scalar d11w10	= 1.0229494  
scalar d11w11	= 1.0411878  

*Rubro 1.2 semanal 

scalar d12w08	= 1.0000000  
scalar d12w09	= 1.0179260  
scalar d12w10	= 1.0232341  
scalar d12w11	= 1.0257628  

*Rubro 2 trimestral 

scalar d2t05	= 0.9679863  
scalar d2t06	= 0.9847844  
scalar d2t07	= 1.0000112  
scalar d2t08    = 1.0163106  

*Rubro 3 mensual 

scalar d3m07	= 0.9955227  
scalar d3m08	= 1.0000000  
scalar d3m09	= 1.0111630  
scalar d3m10	= 1.0318597  
scalar d3m11	= 1.0518675  

*Rubro 4.2 mensual 

scalar d42m07	= 0.9753953  
scalar d42m08	= 1.0000000  
scalar d42m09	= 1.0164480  
scalar d42m10	= 1.0309184  
scalar d42m11	= 1.0514951  

*Rubro 4.2 trimestral 

scalar d42t05	= 0.9540421  
scalar d42t06	= 0.9763979  
scalar d42t07   = 0.9972811  
scalar d42t08	= 1.0157888  

*Rubro 4.1 semestral 

scalar d41s02	= 0.9417798  
scalar d41s03	= 0.9585164  
scalar d41s04	= 0.9744704  
scalar d41s05	= 0.9898757  

*Rubro 5.1 trimestral 

scalar d51t05	= 0.9732526  
scalar d51t06	= 0.9891866  
scalar d51t07	= 1.0014853  
scalar d51t08	= 1.0146306  

*Rubro 6.1.1 semanal 

scalar d611w07	= 0.9889061  
scalar d611w08  = 1.0000000  
scalar d611w09  = 1.0084400  
scalar d611w10  = 1.0109365  
scalar d611w11  = 1.0196352  

*Rubro 6 mensual 

scalar d6m07	= 0.9897885  
scalar d6m08	= 1.0000000  
scalar d6m09    = 1.0114321  
scalar d6m10    = 1.0208988  
scalar d6m11    = 1.0339737  

*Rubro 6 semestral 

scalar d6s02 	= 0.9512171  
scalar d6s03	= 0.9673956  
scalar d6s04	= 0.9823688  
scalar d6s05	= 0.9937884  

*Rubro 7 mensual 

scalar d7m07	= 0.9851652  
scalar d7m08	= 1.0000000  
scalar d7m09	= 1.0626332  
scalar d7m10	= 1.0677936  
scalar d7m11	= 1.0736744  

*Rubro 2.3 mensual 

scalar d23m07	= 0.9935548  
scalar d23m08	= 1.0000000  
scalar d23m09	= 1.0105628  
scalar d23m10	= 1.0154785  
scalar d23m11	= 1.0246668  

*Rubro 2.3 trimestral 

scalar d23t05	= 0.9862840  
scalar d23t06	= 0.9941306  
scalar d23t07	= 1.0013725  
scalar d23t08	= 1.0086804  

*INPC semestral 

scalar dINPCs02= 0.9444458  
scalar dINPCs03= 0.9620069  
scalar dINPCs04= 0.9789503  
scalar dINPCs05= 0.9936754  

*Una vez definidos los deflactores, se procede a limpiar la base de gasto a fin de incorporar sólo
*los rubros de gasto solicitados, y agregarlos en los rubros más generales definidos por el CTMP 

drop if (clave=="G002") 
drop if (clave>="K031" & clave<="K034") 
drop if (clave>="Q000" & clave<="Q015") 
drop if (clave=="T906") 

gen double gasnomon=gas_tri/3 

destring tipo_gas, replace 
gen auto=1 if tipo_gas==1 
gen esp=1 if tipo_gas==2 
gen reg=1 if tipo_gas==3 
gen alq=1 if tipo_gas==0 

*Gasto en alimentos deflactado 

gen ali_nm=gasnomon if (clave>="A001" & clave<="A194") | (clave>="A205" & clave<="A208") 

replace ali_nm=ali_nm/d11w08 if decena==0 
replace ali_nm=ali_nm/d11w08 if decena==1 
replace ali_nm=ali_nm/d11w09 if decena==2 
replace ali_nm=ali_nm/d11w09 if decena==3 
replace ali_nm=ali_nm/d11w09 if decena==4 
replace ali_nm=ali_nm/d11w10 if decena==5 
replace ali_nm=ali_nm/d11w10 if decena==6 
replace ali_nm=ali_nm/d11w10 if decena==7 
replace ali_nm=ali_nm/d11w10 if decena==8 
replace ali_nm=ali_nm/d11w11 if decena==9 

*Gasto en Alcohol y Tabaco deflactado 

gen alta_nm=gasnomon if (clave>="A195" & clave<="A204") | (clave>="A209" & clave<="A211") 

replace alta_nm=alta_nm/d12w08 if decena==0 
replace alta_nm=alta_nm/d12w08 if decena==1 
replace alta_nm=alta_nm/d12w09 if decena==2 
replace alta_nm=alta_nm/d12w09 if decena==3 
replace alta_nm=alta_nm/d12w09 if decena==4 
replace alta_nm=alta_nm/d12w10 if decena==5 
replace alta_nm=alta_nm/d12w10 if decena==6 
replace alta_nm=alta_nm/d12w10 if decena==7 
replace alta_nm=alta_nm/d12w10 if decena==8 
replace alta_nm=alta_nm/d12w11 if decena==9 

*Gasto en Vestido y Calzado deflactado 

gen veca_nm=gasnomon if (clave>="H001" & clave<="H028") | (clave>="H031" & clave<="H055") 

replace veca_nm=veca_nm/d2t05 if decena==0 
replace veca_nm=veca_nm/d2t05 if decena==1 
replace veca_nm=veca_nm/d2t06 if decena==2 
replace veca_nm=veca_nm/d2t06 if decena==3 
replace veca_nm=veca_nm/d2t06 if decena==4 
replace veca_nm=veca_nm/d2t07 if decena==5 
replace veca_nm=veca_nm/d2t07 if decena==6 
replace veca_nm=veca_nm/d2t07 if decena==7 
replace veca_nm=veca_nm/d2t08 if decena==8 
replace veca_nm=veca_nm/d2t08 if decena==9 

*Gasto en Vivienda deflactado 

gen viv_nm=gasnomon if  (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G014") | (clave>="G016" & clave<="G033") 

replace viv_nm=viv_nm/d3m07 if decena==0 
replace viv_nm=viv_nm/d3m07 if decena==1 
replace viv_nm=viv_nm/d3m08 if decena==2 
replace viv_nm=viv_nm/d3m08 if decena==3 
replace viv_nm=viv_nm/d3m08 if decena==4 
replace viv_nm=viv_nm/d3m09 if decena==5 
replace viv_nm=viv_nm/d3m09 if decena==6 
replace viv_nm=viv_nm/d3m09 if decena==7 
replace viv_nm=viv_nm/d3m10 if decena==8 
replace viv_nm=viv_nm/d3m10 if decena==9 

*Gasto en Artículos de Limpieza deflactado 

gen lim_nm = gasnomon if (clave>="C001" & clave<="C024") 

replace lim_nm=lim_nm/d42m07 if decena==0 
replace lim_nm=lim_nm/d42m07 if decena==1 
replace lim_nm=lim_nm/d42m08 if decena==2 
replace lim_nm=lim_nm/d42m08 if decena==3 
replace lim_nm=lim_nm/d42m08 if decena==4 
replace lim_nm=lim_nm/d42m09 if decena==5 
replace lim_nm=lim_nm/d42m09 if decena==6 
replace lim_nm=lim_nm/d42m09 if decena==7 
replace lim_nm=lim_nm/d42m10 if decena==8 
replace lim_nm=lim_nm/d42m10 if decena==9 

*Gasto en Cristalería y Blancos deflactado 

gen cris_nm=gasnomon if (clave>="I001" & clave<="I026") 

replace cris_nm=cris_nm/d42t05 if decena==0 
replace cris_nm=cris_nm/d42t05 if decena==1 
replace cris_nm=cris_nm/d42t06 if decena==2 
replace cris_nm=cris_nm/d42t06 if decena==3 
replace cris_nm=cris_nm/d42t06 if decena==4 
replace cris_nm=cris_nm/d42t07 if decena==5 
replace cris_nm=cris_nm/d42t07 if decena==6 
replace cris_nm=cris_nm/d42t07 if decena==7 
replace cris_nm=cris_nm/d42t08 if decena==8 
replace cris_nm=cris_nm/d42t08 if decena==9 

*Gasto en Enseres deflactado 

gen ens_nm=gasnomon if (clave>="K001" & clave<="K03") 

replace ens_nm=ens_nm/d41s02 if decena==0 
replace ens_nm=ens_nm/d41s02 if decena==1 
replace ens_nm=ens_nm/d41s03 if decena==2 
replace ens_nm=ens_nm/d41s03 if decena==3 
replace ens_nm=ens_nm/d41s03 if decena==4 
replace ens_nm=ens_nm/d41s04 if decena==5 
replace ens_nm=ens_nm/d41s04 if decena==6 
replace ens_nm=ens_nm/d41s04 if decena==7 
replace ens_nm=ens_nm/d41s05 if decena==8 
replace ens_nm=ens_nm/d41s05 if decena==9 

*Gasto en Salud deflactado 

gen sal_nm=gasnomon if (clave>="J001" & clave<="J045") 

replace sal_nm=sal_nm/d51t05 if decena==0 
replace sal_nm=sal_nm/d51t05 if decena==1 
replace sal_nm=sal_nm/d51t06 if decena==2 
replace sal_nm=sal_nm/d51t06 if decena==3 
replace sal_nm=sal_nm/d51t06 if decena==4 
replace sal_nm=sal_nm/d51t07 if decena==5 
replace sal_nm=sal_nm/d51t07 if decena==6 
replace sal_nm=sal_nm/d51t07 if decena==7 
replace sal_nm=sal_nm/d51t08 if decena==8 
replace sal_nm=sal_nm/d51t08 if decena==9 

*Gasto en Transporte Público deflactado 

gen tpub_nm=gasnomon if (clave>="B001" & clave<="B007") 

replace tpub_nm=tpub_nm/d611w08 if decena==0 
replace tpub_nm=tpub_nm/d611w08 if decena==1 
replace tpub_nm=tpub_nm/d611w08 if decena==2 
replace tpub_nm=tpub_nm/d611w09 if decena==3 
replace tpub_nm=tpub_nm/d611w09 if decena==4 
replace tpub_nm=tpub_nm/d611w09 if decena==5 
replace tpub_nm=tpub_nm/d611w10 if decena==6 
replace tpub_nm=tpub_nm/d611w10 if decena==7 
replace tpub_nm=tpub_nm/d611w10 if decena==8 
replace tpub_nm=tpub_nm/d611w11 if decena==9 


*Gasto en Transporte Foráneo deflactado 

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M018") | (clave>="F010" & clave<="F010") 

replace tfor_nm=tfor_nm/d6s02 if decena==0 
replace tfor_nm=tfor_nm/d6s02 if decena==1 
replace tfor_nm=tfor_nm/d6s03 if decena==2 
replace tfor_nm=tfor_nm/d6s03 if decena==3 
replace tfor_nm=tfor_nm/d6s03 if decena==4 
replace tfor_nm=tfor_nm/d6s04 if decena==5 
replace tfor_nm=tfor_nm/d6s04 if decena==6 
replace tfor_nm=tfor_nm/d6s04 if decena==7 
replace tfor_nm=tfor_nm/d6s05 if decena==8 
replace tfor_nm=tfor_nm/d6s05 if decena==9 

*Gasto en Comunicaciones deflactado 

gen com_nm=gasnomon if (clave>="F001" & clave<="F005") 

replace com_nm=com_nm/d6m07 if decena==0 
replace com_nm=com_nm/d6m07 if decena==1 
replace com_nm=com_nm/d6m08 if decena==2 
replace com_nm=com_nm/d6m08 if decena==3 
replace com_nm=com_nm/d6m08 if decena==4 
replace com_nm=com_nm/d6m09 if decena==5 
replace com_nm=com_nm/d6m09 if decena==6 
replace com_nm=com_nm/d6m09 if decena==7 
replace com_nm=com_nm/d6m10 if decena==8 
replace com_nm=com_nm/d6m10 if decena==9 

*Gasto en Educación y Recreación deflactado 

gen edre_nm=gasnomon if (clave>="E001" & clave<="E034") | (clave>="H029" & clave<="H030") | (clave>="L001" & clave<="L027") | (clave>="N003" & clave<="N005") 

replace edre_nm=edre_nm/d7m07 if decena==0 
replace edre_nm=edre_nm/d7m07 if decena==1 
replace edre_nm=edre_nm/d7m08 if decena==2 
replace edre_nm=edre_nm/d7m08 if decena==3 
replace edre_nm=edre_nm/d7m08 if decena==4 
replace edre_nm=edre_nm/d7m09 if decena==5 
replace edre_nm=edre_nm/d7m09 if decena==6 
replace edre_nm=edre_nm/d7m09 if decena==7 
replace edre_nm=edre_nm/d7m10 if decena==8 
replace edre_nm=edre_nm/d7m10 if decena==9 

*Gasto en Educación Básica deflactado 

gen edba_nm=gasnomon if (clave>="E002" & clave<="E003") | (clave=="E015") | (clave=="E029") 

replace edba_nm=edba_nm/d7m07 if decena==0 
replace edba_nm=edba_nm/d7m07 if decena==1 
replace edba_nm=edba_nm/d7m08 if decena==2 
replace edba_nm=edba_nm/d7m08 if decena==3 
replace edba_nm=edba_nm/d7m08 if decena==4 
replace edba_nm=edba_nm/d7m09 if decena==5 
replace edba_nm=edba_nm/d7m09 if decena==6 
replace edba_nm=edba_nm/d7m09 if decena==7 
replace edba_nm=edba_nm/d7m10 if decena==8 
replace edba_nm=edba_nm/d7m10 if decena==9 

*Gasto en Cuidado Personal deflactado 

gen cuip_nm=gasnomon if (clave>="D001" & clave<="D024") | (clave=="H064") 

replace cuip_nm=cuip_nm/d23m07 if decena==0 
replace cuip_nm=cuip_nm/d23m07 if decena==1 
replace cuip_nm=cuip_nm/d23m08 if decena==2 
replace cuip_nm=cuip_nm/d23m08 if decena==3 
replace cuip_nm=cuip_nm/d23m08 if decena==4 
replace cuip_nm=cuip_nm/d23m09 if decena==5 
replace cuip_nm=cuip_nm/d23m09 if decena==6 
replace cuip_nm=cuip_nm/d23m09 if decena==7 
replace cuip_nm=cuip_nm/d23m10 if decena==8 
replace cuip_nm=cuip_nm/d23m10 if decena==9 

*Gasto en Accesorios Personales deflactado 

gen accp_nm=gasnomon if (clave>="H056" & clave<="H063") | (clave=="H065") 

replace accp_nm=accp_nm/d23t05 if decena==0 
replace accp_nm=accp_nm/d23t05 if decena==1 
replace accp_nm=accp_nm/d23t06 if decena==2 
replace accp_nm=accp_nm/d23t06 if decena==3 
replace accp_nm=accp_nm/d23t06 if decena==4 
replace accp_nm=accp_nm/d23t07 if decena==5 
replace accp_nm=accp_nm/d23t07 if decena==6 
replace accp_nm=accp_nm/d23t07 if decena==7 
replace accp_nm=accp_nm/d23t08 if decena==8 
replace accp_nm=accp_nm/d23t08 if decena==9 

*Gasto en Otros Gastos y Transferencias deflactado 

gen otr_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") 

replace otr_nm=otr_nm/dINPCs02 if decena==0 
replace otr_nm=otr_nm/dINPCs02 if decena==1 
replace otr_nm=otr_nm/dINPCs03 if decena==2 
replace otr_nm=otr_nm/dINPCs03 if decena==3 
replace otr_nm=otr_nm/dINPCs03 if decena==4 
replace otr_nm=otr_nm/dINPCs04 if decena==5 
replace otr_nm=otr_nm/dINPCs04 if decena==6 
replace otr_nm=otr_nm/dINPCs04 if decena==7 
replace otr_nm=otr_nm/dINPCs05 if decena==8 
replace otr_nm=otr_nm/dINPCs05 if decena==9 

*Gasto en Regalos Otorgados deflactado 

gen reda_nm=gasnomon if (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") | (clave=="N013") 

replace reda_nm=reda_nm/dINPCs02 if decena==0 
replace reda_nm=reda_nm/dINPCs02 if decena==1 
replace reda_nm=reda_nm/dINPCs03 if decena==2 
replace reda_nm=reda_nm/dINPCs03 if decena==3 
replace reda_nm=reda_nm/dINPCs03 if decena==4 
replace reda_nm=reda_nm/dINPCs04 if decena==5 
replace reda_nm=reda_nm/dINPCs04 if decena==6 
replace reda_nm=reda_nm/dINPCs04 if decena==7 
replace reda_nm=reda_nm/dINPCs05 if decena==8 
replace reda_nm=reda_nm/dINPCs05 if decena==9 

*Estimación del Alquiler 

gen vivprob=gasnomon if (clave=="G001") 
gen vivrecib=gasnomon if (clave=="G007") 
gen vivprest=gasnomon if (clave=="G010") 
gen vivotra=gasnomon if (clave=="G015") 
gen double est_alq=alq*gasnomon 
egen ren_imp=rsum(vivprob vivrecib vivprest vivotra) 

save "$ruta\gastonomonetario_def96.dta", replace 

*Construccion de la base de autoconsumo a partir de la base de gasto no monetario 

keep if auto==1 

collapse (sum) *_nm, by (folio) 

egen gasnma=rsum(*_nm) 

rename  ali_nm ali_nma 
rename  alta_nm alta_nma 
rename  veca_nm veca_nma 
rename  viv_nm viv_nma 
rename  lim_nm lim_nma 
rename  cris_nm cris_nma 
rename  ens_nm ens_nma 
rename  sal_nm sal_nma 
rename  tpub_nm tpub_nma 
rename  tfor_nm tfor_nma 
rename  com_nm com_nma  
rename  edre_nm edre_nma 
rename  edba_nm edba_nma 
rename  cuip_nm cuip_nma 
rename  accp_nm accp_nma 
rename  otr_nm otr_nma 
rename  reda_nm reda_nma 

sort folio 

save "$ruta\auto_def96.dta", replace 

use "$ruta\gastonomonetario_def96.dta", clear 

keep if esp==1 

collapse (sum) *_nm, by (folio) 

egen gasnme=rsum(*_nm) 

rename  ali_nm ali_nme 
rename  alta_nm alta_nme 
rename  veca_nm veca_nme 
rename  viv_nm viv_nme 
rename  lim_nm lim_nme 
rename  cris_nm cris_nme 
rename  ens_nm ens_nme 
rename  sal_nm sal_nme 
rename  tpub_nm tpub_nme 
rename  tfor_nm tfor_nme 
rename  com_nm com_nme  
rename  edre_nm edre_nme 
rename  edba_nm edba_nme 
rename  cuip_nm cuip_nme 
rename  accp_nm accp_nme 
rename  otr_nm otr_nme 
rename  reda_nm reda_nme 

sort folio 

save "$ruta\esp_def96.dta", replace 

use "$ruta\gastonomonetario_def96.dta", clear 

keep if (reg==1 | alq==1) 

collapse (sum) *_nm est_alq ren_imp vivprob vivrecib vivprest vivotra, by (folio) 

egen gasnmr=rsum(*_nm est_alq) 

rename  ali_nm ali_nmr 
rename  alta_nm alta_nmr 
rename  veca_nm veca_nmr 
rename  viv_nm viv_nmr 
rename  lim_nm lim_nmr 
rename  cris_nm cris_nmr 
rename  ens_nm ens_nmr 
rename  sal_nm sal_nmr 
rename  tpub_nm tpub_nmr 
rename  tfor_nm tfor_nmr 
rename  com_nm com_nmr  
rename  edre_nm edre_nmr 
rename  edba_nm edba_nmr 
rename  cuip_nm cuip_nmr 
rename  accp_nm accp_nmr 
rename  otr_nm otr_nmr 
rename  reda_nm reda_nmr 

sort folio 

save "$ruta\reg_def96.dta", replace 

*********************************************************

/*Parte IV

Cálculo de la incidencia 1996

Con el propósito de obtener cifras representativas a nivel 
nacional, rural y urbano, se obtiene una base con las 
variables de tamaño del hogar, estrato y factor de 
expansión, y a ésta se le unen las bases anteriormente 
generadas. Se estiman los ingresos corriente total y 
corriente neto, se genera el factor de expansión para 
personas, y se calcula la incidencia de la pobreza de 
acuerdo con el cálculo oficial de la Sedesol.


Se genera la base "$ruta\basefinal_96.dta" a nivel de folio.*/

********************************************************* 

use "$ruta\concen.dta", clear 
keep folio hog estrato tam_hog educacion 
rename hog factor 
sort folio 

*Modificado Mayra Sáenz Julio 2015 - Incluyo la base de hogares

merge 1:1 folio using "$ruta\hogares.dta" 
tab _merge 
drop _merge 
sort folio 

*Modificado Mayra Sáenz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*merge folio using "$ruta\ingreso_deflactado96.dta" 
tab _merge 
drop _merge 
sort folio */

merge 1:1 folio using "$ruta\gastomonetario_def96.dta" 
tab _merge 
drop _merge 
sort folio 

merge 1:1 folio using "$ruta\auto_def96.dta" 
tab _merge 
drop _merge 
sort folio 

merge 1:1 folio using "$ruta\esp_def96.dta" 
tab _merge 
drop _merge 
sort folio 

merge 1:1 folio using "$ruta\reg_def96.dta" 
tab _merge 
drop _merge 
sort folio 

*Construcción de rural y urbano, criterio de 15000 habitantes  

destring estrato, replace 
gen rururb=1 if (estrato>2 & estrato!=.) 
replace rururb=0 if estrato<=2 
label define rururb 1 "Rural" 0 "Urbano" 
label value rururb rururb 

*Se calculan los gastos totales 

egen double gasmon=rsum(ali_m alta_m veca_m viv_m lim_m ens_m cris_m sal_m tpub_m tfor_m com_m edre_m cuip_m accp_m otr_m) 

egen double autocons=rsum( ali_nma alta_nma veca_nma viv_nma lim_nma ens_nma cris_nma sal_nma tpub_nma tfor_nma com_nma edre_nma cuip_nma accp_nma otr_nma) 

egen double pago_esp=rsum(ali_nme alta_nme veca_nme viv_nme lim_nme ens_nme cris_nme sal_nme tpub_nme tfor_nme com_nme edre_nme cuip_nme accp_nme otr_nme) 

egen double reg_esp=rsum(ali_nmr alta_nmr veca_nmr viv_nmr lim_nmr ens_nmr cris_nmr sal_nmr tpub_nmr tfor_nmr com_nmr edre_nmr cuip_nmr accp_nmr otr_nmr) 

egen double nomon=rsum(autocons pago_esp reg_esp est_alq) 

egen double reda=rsum(reda_m reda_nma reda_nme) 

gen double redan= -1 * reda 
gen double reg_espn = -1 * reg_esp 


rename folio folio1              //Modificado Mayra Sáenz - Julio 2015
gen folio = substr(folio1,4,100) //Modificado Mayra Sáenz - Julio 2015

saveold "$ruta\gtos_autoc96.dta", replace //Mayra Sáenz Julio 2015

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*

*use "$ruta\pobla96.dta", clear
use "$ruta\person96.dta", clear

*rename folio folio1              //Modificado Mayra Sáenz - Julio 2015
*gen folio = substr(folio1,4,100) //Modificado Mayra Sáenz - Julio 2015

sort folio num_ren, stable

merge 1:1 folio num_ren using "$ruta\ingreso_deflactado96_per.dta"
rename _merge _merge_ing
sort folio num_ren, stable

merge m:1 folio using "$ruta\gtos_autoc96.dta"

*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict=rsum(ing_monh nomon) if paren=="01" | paren=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gct=rsum(gasmon nomon)   if paren=="01" | paren=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if paren=="01" | paren=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gnt=rsum(gasmon nomon redan reg_espn) if paren=="01" | paren=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas

label var  ict  "Ingreso corriente total del hogar"
label var  gct  "Gasto corriente total del hogar"
label var  intt "Ingreso neto total del hogar"
label var  gnt  "Gasto neto total del hogar"


*Información per cápita 

gen double ictpc= ict/tam_hog 
gen double gctpc= gct/tam_hog 
gen double intpc= intt/tam_hog 
gen double gntpc= gnt/tam_hog 

label var  ictpc "Ingreso corriente total per cápita" 
label var  gctpc "Gasto corriente totalper cápita" 
label var  intpc "Ingreso neto total per cápita" 
label var  gntpc "Gasto neto total per cápita" 

summ  gasmon autocons pago_esp reg_esp nomon reda ict gct intt gnt intpc gntpc ictpc gctpc 



saveold "`base_out'", replace


/*
use "$ruta\vivi96.dta",clear
sort folio
save "$ruta\MEX1996EA_BID.dta",replace
use "$ruta\person96.dta", clear
sort folio
merge folio using "$ruta\MEX1996EA_BID.dta"
drop _merge
sort folio num_ren
gen id=_n
save "$ruta\MEX1996EA_BID.dta",replace
use "$ruta\ingres96.dta", clear
tab  ocupa,gen(ocup) /*se genera para saber la numero de ocupacion*/
gen clave1=substr(clave,1,1)
gen clave2=real(substr(clave,2,3))
sort folio num_ren
by folio num_ren:egen ring1a_ml=sum(ingreso_) if clave2>0 & clave2<=5 & ocupa=="1" 
by folio num_ren:egen ring1b_ml=sum(ingreso_) if clave2==14 & ocupa=="1" 
replace ring1a_ml=0 if ring1a_ml==.
replace ring1b_ml=0 if ring1b_ml==.
by folio num_ren:gen ring1_ml=ring1a_ml+ring1b_ml
by folio num_ren:egen ing1_ml=max(ring1_ml)
 
by folio num_ren:egen ring2a_ml=sum(ingreso_) if clave2>0 & clave2<=5 & ocupa~="1" 
by folio num_ren:egen ring2b_ml=sum(ingreso_) if clave2==14 & ocupa~="1" 
replace ring2a_ml=0 if ring2a_ml==.
replace ring2b_ml=0 if ring2b_ml==.
by folio num_ren:gen ring2_ml=ring2a_ml+ring2b_ml
by folio num_ren:egen ing2_ml=max(ring2_ml)

by folio num_ren:egen ring1a_npm=sum(ingreso_) if clave2>5 & clave2<=13 & ocupa=="1" 
by folio num_ren:egen ring1b_npm=sum(ingreso_) if clave2==15 & ocupa=="1" 
replace ring1a_npm=0 if ring1a_npm==.
replace ring1b_npm=0 if ring1b_npm==.
by folio num_ren:gen ring1_npm=ring1a_npm+ring1b_npm
by folio num_ren:egen ing1_npm=max(ring1_npm)
 
by folio num_ren:egen double ring2a_npm=sum(ingreso_) if clave2>5 & clave2<=13 & ocupa~="1" 
by folio num_ren:egen double ring2b_npm=sum(ingreso_) if clave2==15 & ocupa~="1" 
replace ring2a_npm=0 if ring2a_npm==.
replace ring2b_npm=0 if ring2b_npm==.
by folio num_ren:gen ring2_npm=ring2a_npm+ring2b_npm
by folio num_ren:egen ing2_npm=max(ring2_npm)

/*renta*/
by folio num_ren:egen ring_rpm=sum(ingreso_) if clave2>=16 & clave2<=22 
replace ring_rpm=0 if ring_rpm==.
by folio num_ren:egen ing_rpm=max(ring_rpm)
/*transferencias*/
by folio num_ren:egen ring_tm=sum(ingreso_) if  clave2>=23 & clave2<=27 
replace ring_tm=0 if ring_tm==.
by folio num_ren:egen ing_tm=max(ring_tm)

*transferencias por jublaciones y/o pensiones*
by folio num_ren:egen ring_jub=sum(ingreso_) if  clave2==23
replace ring_jub=0 if ring_jub==.
by folio num_ren:egen ing_jub=max(ring_jub)

/*regalos*/
by folio num_ren:egen ring_otm=sum(ingreso_) if clave2>=29 & clave2<=31 
replace ring_otm=0 if ring_otm==.
by folio num_ren:egen ing_otm=max(ring_otm)
/*remesas*/
by folio num_ren:egen ring_otrosp=sum(ingreso_) if clave2==28  
replace ring_otrosp=0 if ring_otrosp==.
by folio num_ren:egen ing_otrosp=max(ring_otrosp)
/*financieras*/
by folio num_ren:egen ring_otm1=sum(ingreso_) if clave2>=32 &  clave2<=43
replace ring_otm1=0 if ring_otm1==.
by folio num_ren:egen ing_otm1=max(ring_otm1)
by folio num_ren:gen cont=(_n) 
drop if cont>1

drop  ocupa clave ingreso_ ing_tri ingreso1 ingreso2 ingreso3 ingreso4 ingreso5 cont
sort folio num_ren
merge folio num_ren using "$ruta\MEX1996EA_BID.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge
sort folio num_ren
save "$ruta\MEX1996EA_BID.dta",replace
use "$ruta\nomon96.dta", clear
sort folio num_ren
by folio num_ren:egen ing_nml=sum(gasto) 

by folio num_ren:gen cont=(_n) /*elimino repetidos*/
drop if cont>1
tostring  folio,replace
keep folio num_ren ing_nml
sort folio num_ren
merge folio num_ren using "$ruta\MEX1996EA_BID.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge 




saveold "`base_out'", replace


log close

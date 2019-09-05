* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENIGH\1998\m7_m12\data_orig"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "1998"
local ronda m7_m12

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (Nueva construcción)
Round: Julio- Diciembre
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013

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
siguiente documento: X:\Sociometro_2005\documentation\Years of education in Mexico. 
Tambien se cambiaron las varianles edupi, edupc, edusi, edusc y etc de modo de ser coherente con los siguientes
anios: Primaria 6, Secundaria 6 (12 acumulado) y Terciaria 5 (17 acumulado).  

Febrero 8, 2007 (Victoria)
Si nom_empr==3 tambien se es empleado publico por lo que se debe cambiar el codigo de la varibale spublico
OLD CODE:
gen spublico_ci=.
replace spublico_ci=1 if nom_empr=="1" & emp3_ci==1
replace spublico_ci=0 if nom_empr~="1" & emp3_ci==1
replace spublico_ci=. if nom_empr =="."

Marzo 20 de 2007
rama estaba mal clasificado. Ver diccionario.
se dejo el porograma anterior y se adicionaron excepciones


*/
****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
************************* PARA MEXICO 1998 *********************************
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

Base de ingresos: ingreso98.dta
Base de gasto monetario: gasto998.dta
Base de gasto no monetario: nomonetario98.dta
Base de concentrado: concentrado98.dta

En este programa se utilizan tres tipos de archivos, los 
cuales están ubicados en las siguientes carpetas:

1) Bases originales: "C:\pobreza ingresos\1998\ENIGH"
2) Bitácoras: "C:\pobreza ingresos\1998\Log"
3) Bases generadas: "C:\pobreza ingresos\1998\Resultados"


Para cambiar estas ubicaciones, se modifican los siguientes
globals 

gl data="C:\pobreza ingresos\1998\ENIGH"
gl log="C:\pobreza ingresos\1998\Log"
gl bases="C:\pobreza ingresos\1998\Resultados"

log using "$log\Pobreza por ingresos 1998.txt", text replace
*/
*********************************************************
*
*	PROGRAMA PARA LA MEDICIÓN DE LA POBREZA 1998
*
*********************************************************

*Última modificación: 24 de julio del 2013

*********************************************************

*Parte I


/*Creacion del Ingreso Monetario deflactado a pesos de 
Agosto del 1998.

Los deflactores se crean previamente a partir del INPC 
general (ver Nota Técnica).

En esta parte se crea la base:
"$ruta\ingreso_deflactado98.dta"
a nivel de folio.*/

********************************************************

*Ingresos

use "$ruta\\ingresos.dta", clear


/*La variable meses define el mes de levantamiento de la 
informacion para el ultimo mes y los 5 anteriores, esta 
toma seis valores:

060504030201               
070605040302             
080706050403         
090807060504           
100908070605             
111009080706           

indicando que la encuesta fue levantada entre los meses de
julio y diciembre, y por tanto, al preguntarse por los ingresos
de los ultimos seis meses, se recolecto informacion 
correspondiente a enero-junio, febrero-julio, marzo-agosto,
abril-septiembre, mayo-octubre y junio-noviembre */

*Definicion de los deflactores 1998

scalar ene98= 0.92573949 
scalar feb98= 0.94194657 
scalar mar98= 0.95298067 
scalar abr98= 0.96189694 
scalar may98= 0.96955896 
scalar jun98= 0.98101885 
scalar jul98= 0.99047811 
scalar ago98= 1.00000000 
scalar sep98= 1.01621893 
scalar oct98= 1.03078108 
scalar nov98= 1.04903547 
scalar dic98= 1.07463158 

/*La estrategia para deflactar sera dividir cada columna 
de ingreso por el deflactor correspondiente a su mes.

Asi se procede a dividir las columnas de ingreso por su
deflactor correspondiente*/

replace ing_6=ing_6/ene98 if meses=="060504030201"
replace ing_5=ing_6/feb98 if meses=="060504030201"
replace ing_4=ing_5/mar98 if meses=="060504030201"
replace ing_3=ing_4/abr98 if meses=="060504030201"
replace ing_2=ing_3/may98 if meses=="060504030201"
replace ing_1=ing_2/jun98 if meses=="060504030201"

replace ing_6=ing_6/feb98 if meses=="070605040302"
replace ing_5=ing_5/mar98 if meses=="070605040302"
replace ing_4=ing_4/abr98 if meses=="070605040302"
replace ing_3=ing_3/may98 if meses=="070605040302"
replace ing_2=ing_2/jun98 if meses=="070605040302"
replace ing_1=ing_1/jul98 if meses=="070605040302"

replace ing_6=ing_6/mar98 if meses=="080706050403"
replace ing_5=ing_5/abr98 if meses=="080706050403"
replace ing_4=ing_4/may98 if meses=="080706050403"
replace ing_3=ing_3/jun98 if meses=="080706050403"
replace ing_2=ing_2/jul98 if meses=="080706050403"
replace ing_1=ing_1/ago98 if meses=="080706050403"

replace ing_6=ing_6/abr98 if meses=="090807060504"
replace ing_5=ing_5/may98 if meses=="090807060504"
replace ing_4=ing_4/jun98 if meses=="090807060504"
replace ing_3=ing_3/jul98 if meses=="090807060504"
replace ing_2=ing_2/ago98 if meses=="090807060504"
replace ing_1=ing_1/sep98 if meses=="090807060504"

replace ing_6=ing_6/may98 if meses=="100908070605"
replace ing_5=ing_5/jun98 if meses=="100908070605"
replace ing_4=ing_4/jul98 if meses=="100908070605"
replace ing_3=ing_3/ago98 if meses=="100908070605"
replace ing_2=ing_2/sep98 if meses=="100908070605"
replace ing_1=ing_1/oct98 if meses=="100908070605"

replace ing_6=ing_5/jun98 if meses=="111009080706"
replace ing_5=ing_4/jul98 if meses=="111009080706"
replace ing_4=ing_3/ago98 if meses=="111009080706"
replace ing_3=ing_2/sep98 if meses=="111009080706"
replace ing_2=ing_1/oct98 if meses=="111009080706"
replace ing_1=ing_1/nov98 if meses=="111009080706"

*Mayra Sáenz Julio 2015- Mes de referencia

g mes_ref = substr(meses, 1, 2)
destring mes_ref, replace



/*Una vez realizada la deflactacion, se procede a obtener el 
promedio de ingreso mensual en los ultimos 6 meses para cada
persona y clave de ingreso*/

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)

gen double ing_mon=ing_mens if clave>="P001" & clave<="P034"
gen double ing_lab=ing_mens if clave>="P001" & clave<="P019"

**Modificación Mayra Sáenz - Julio 2015 : Son ingresos laborales de P001-P014, se desagrega por actividad principal y secundaria
gen     ocuprisec = 1 if empleo =="1" 
replace ocuprisec = 2 if empleo !="1"

forval j = 1/2 {
gen double ing_trab`j'=ing_mens if (clave>="P001" & clave<="P009") & ocuprisec == `j'
gen double ing_negp`j'=ing_mens if (clave>="P010" & clave<="P019") & ocuprisec == `j'
}

gen double ing_trab=ing_mens if clave>="P001" & clave<="P009"
gen double ing_negp=ing_mens if clave>="P010" & clave<="P019"
gen double ing_rent=ing_mens if clave>="P020" & clave<="P027"
gen double ing_tran=ing_mens if clave>="P028" & clave<="P034"

*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens  if  (clave=="P028")                                      //Jubilaciones y pensiones
g double trat_pr  = ing_mens  if  ((clave>="P029" & clave<="P030") | clave=="P033")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por despido y accidentes de trabajo, Ingresos provenientes de otros paises
g double trat_pu  = ing_mens  if  (clave=="P034")                                      //Desde 1996 aparece PROCAMPO
g double dona_pu  = 0                                                                 
g double dona_pr  = ing_mens if  (clave=="P031" | clave=="P032")                      //Becas y donaciones provenientes de Instituciones, Regalos y donativos originados dentro del país
g double otros   = ing_mens  if  ((clave>="P035" & clave<="P036"))                     //Venta de automóviles, aparatos electricos de segunda mano, etc.  Otros ingresos corrientes no considerados en los anteriores, (viÃ¡ticos, etc.).
g double remesas = ing_mens  if  (clave=="P033")                                      //Ingresos provenientes de otros paises

*Modificación Mayra Sáenz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}


*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.

/*A continuacion, se estima el total de ingresos
que cada hogar recibe, y se guarda la base
en un archivo aparte*/


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

rename folio folio1              //Modificado Mayra Sáenz - Julio 2015
gen folio = substr(folio1,4,100) //Modificado Mayra Sáenz - Julio 2015

sort folio num_ren, stable

saveold "$ruta\ingreso_deflactado98_per.dta", replace

*********************************************************

/*Parte II

Creación del gasto monetario deflactado a pesos de agosto 
de 2002.

Los deflactores se crean previamente a partir del INPC 
según aparece en la Nota Técnica.

Se crea la base:
	"$ruta\gastomonetario_def98.dta"
la cual se encuentra a nivel de folio.*/

*********************************************************

*Gastos

use "$ruta\\gastos98.dta", clear

/*En el caso de la informacion de gasto, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual esta dada dentro
del folio de identificacion del hogar en las posiciones septima y octava.
 En primer lugar se obtiene una variable que identifique la decena de
levantamiento*/

gen decena=real(substr(folio,7,1))
tab decena

/*Asi, se puede observar que se levanto informacion en 9 decenas,
correspondientes a:

Decena                           Periodo de Levantamiento

1	 |	Del 20 al 26 de agosto
2	 |	Del 30 de agosto al 05 de septiembre
3	 |	Del 09 al 15 de septiembre
4	 |	Del 19 al 25 de septiembre
5	 |	Del 29 de septiembre al 05 de octubre
6	 |	Del 09 al 15 de octubre
7	 |	Del 19 al 25 de octubre
8	 |	Del 29 de octubre al 04 de noviembre
9	 |	Del 08 al 14 de noviembre

Asi, para realizar el analisis, y conforme a lo establecido en la
informacion de la ENIGH1998, se tomara como periodos de referencia

              Periodo de Referencia
Decena|	/Semanal     /Mensual     /Trimestral           /Semestral
	
1     |	/Agosto	     /Julio       /Mayo a Julio         /Febrero a Julio
2     |	/Agosto  /Agosto      /Junio a Agosto       /Marzo a Agosto
3     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
4     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
5     |	/Septiembre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
6     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
7     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
8     |	/Octubre     /Octubre     /Agosto a Octubre     /Mayo a Octubre
9     |	/Noviembre   /Octubre     /Agosto a Octubre     /Mayo a Octubre

Asi, se utilizaran los deflactores mensuales para el caso de las referencias semanales y mensuales
y los promedios de los meses de referencia en las referencias Trimestrales y Semestrales

*Los rubros a considerar, segun la metodologia del CTMP, seran:

Rubro                                  /Periodicidad     /Nombre del deflactor
ali_ms y Bebidas No Alcoholicas     /Semanal          /d11wmes
Bebidas alcoholicas y tabaco           /Semanal          /d12wmes
Vestido y Calzado                      /Trimestral       /d2tmesini
vivienda, servicios de conservacion    /Mensual          /d3mmes
energia y combustible
Estimacion del Alquiler                /No se deflactara /No se deflactara
Articulos y Servicios de limpieza      /Mensual          /d42mmes
cristaleria, utensilios domesticos     /Trimestral       /d42tmesini
y blancos
Enseres domesticos y muebles           /Semestral        /d41smesini
Cuidados de la salud	               /Trimestral       /d51tmesini
Transporte publico                     /Semanal          /d611wmes
Transporte foraneo, vehiculos          /Semestral        /d6smesini
Comunicaciones                        /Mensual          /d6mmes
Educacion basica                       /Mensual          /d7mmes
Articulos y servicios para el cuidado  /Mensual          /d23mmes
personal
Accesorios personales                  /Trimestral       /d23tmesini
Otros gastos diversos y transf.        /Semestral        /dINPCsmesini
Regalos otorgados                      /Semestral        /dINPCsmesini
*/
*Definición de los deflactores

*Rubro 1.1 semanal

scalar d11w08	= 1.00000000 
scalar d11w09	= 1.01678362 
scalar d11w10	= 1.03756259 
scalar d11w11	= 1.05580747 

*Rubro 1.2 semanal

scalar d12w08	= 1.00000000 
scalar d12w09	= 1.02023398 
scalar d12w10	= 1.02658608 
scalar d12w11	= 1.03121068 

*Rubro 2 trimestral

scalar d2t05	= 0.97913945 
scalar d2t06	= 0.98901711 
scalar d2t07	= 0.99967392 
scalar d2t08    = 1.01214561 

*Rubro 3 mensual

scalar d3m07	= 0.99238572 
scalar d3m08	= 1.00000000 
scalar d3m09	= 1.01163363 
scalar d3m10	= 1.02969903 
scalar d3m11	= 1.04920842 

*Rubro 4.2 mensual

scalar d42m07	= 0.98808248 
scalar d42m08	= 1.00000000 
scalar d42m09	= 1.01274869 
scalar d42m10	= 1.03533472 
scalar d42m11	= 1.05300221 

*Rubro 4.2 trimestral

scalar d42t05	= 0.97399447 
scalar d42t06	= 0.98665169 
scalar d42t07   = 1.00027706 
scalar d42t08	= 1.01602780 

*Rubro 4.1 semestral

scalar d41s02	= 0.97005801 
scalar d41s03	= 0.97923486 
scalar d41s04	= 0.98951059 
scalar d41s05	= 1.00020301 

*Rubro 5.1 trimestral

scalar d51t05	= 0.98024053 
scalar d51t06	= 0.98999429 
scalar d51t07	= 1.00050380 
scalar d51t08	= 1.01202871 

*Rubro 6.1.1 semanal

scalar d611w08  = 1.00000000 
scalar d611w09  = 1.00798900 
scalar d611w10  = 1.01028722 
scalar d611w11  = 1.01339853 

*Rubro 6 mensual

scalar d6m07	= 0.99130729 
scalar d6m08	= 1.00000000 
scalar d6m09    = 1.00895463 
scalar d6m10    = 1.01559312 
scalar d6m11    = 1.04535206 

*Rubro 6 semestral

scalar d6s02 	= 0.97331747 
scalar d6s03	= 0.98087063 
scalar d6s04	= 0.98827503 
scalar d6s05	= 0.99562411 

*Rubro 7 mensual

scalar d7m07	= 0.98724017 
scalar d7m08	= 1.00000000 
scalar d7m09	= 1.05623874 
scalar d7m10	= 1.06369495 
scalar d7m11	= 1.07099144 

*Rubro 2.3 mensual

scalar d23m07	= 0.98708185 
scalar d23m08	= 1.00000000 
scalar d23m09	= 1.01151848 
scalar d23m10	= 1.02050485 
scalar d23m11	= 1.03919676 

*Rubro 2.3 trimestral

scalar d23t05	= 0.98151380 
scalar d23t06	= 0.98917654 
scalar d23t07	= 0.99953344 
scalar d23t08	= 1.01067444 

*INPC semestral

scalar dINPCs02= 0.96631335 
scalar dINPCs03= 0.97598892 
scalar dINPCs04= 0.98652863 
scalar dINPCs05= 0.99800932 

/*Una vez definidos los deflactores, se procede a limpiar la base de gasto a fin de incorporar solo
los rubros de gasto solicitados, y agregarlos en los rubros mas generales definidos por el CTMP*/

drop if (clave=="G002")


gen double gasm=gas_tri/3

*Gasto en alimentos deflactado

gen ali_m=gasm if (clave>="A001" & clave<="A194") | (clave>="A205" & clave<="A208")

replace ali_m=ali_m/d11w08 if decena==1
replace ali_m=ali_m/d11w08 if decena==2
replace ali_m=ali_m/d11w09 if decena==3
replace ali_m=ali_m/d11w09 if decena==4
replace ali_m=ali_m/d11w09 if decena==5
replace ali_m=ali_m/d11w10 if decena==6
replace ali_m=ali_m/d11w10 if decena==7
replace ali_m=ali_m/d11w10 if decena==8
replace ali_m=ali_m/d11w11 if decena==9

*Gasto en Alcohol y Tabaco deflactado

gen alta_m=gasm if (clave>="A195" & clave<="A204") | (clave>="A209" & clave<="A211")

replace alta_m=alta_m/d12w08 if decena==1
replace alta_m=alta_m/d12w08 if decena==2
replace alta_m=alta_m/d12w09 if decena==3
replace alta_m=alta_m/d12w09 if decena==4
replace alta_m=alta_m/d12w09 if decena==5
replace alta_m=alta_m/d12w10 if decena==6
replace alta_m=alta_m/d12w10 if decena==7
replace alta_m=alta_m/d12w10 if decena==8
replace alta_m=alta_m/d12w11 if decena==9

*Gasto en Vestido y Calzado deflactado

gen veca_m=gasm if (clave>="H001" & clave<="H028") | (clave>="H031" & clave<="H055")

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

replace viv_m=viv_m/d3m07 if decena==1
replace viv_m=viv_m/d3m08 if decena==2
replace viv_m=viv_m/d3m08 if decena==3
replace viv_m=viv_m/d3m08 if decena==4
replace viv_m=viv_m/d3m09 if decena==5
replace viv_m=viv_m/d3m09 if decena==6
replace viv_m=viv_m/d3m09 if decena==7
replace viv_m=viv_m/d3m10 if decena==8
replace viv_m=viv_m/d3m10 if decena==9

*Gasto en Articulos de Limpieza deflactado

gen lim_m=gasm if (clave>="C001" & clave<="C024")

replace lim_m=lim_m/d42m07 if decena==1
replace lim_m=lim_m/d42m08 if decena==2
replace lim_m=lim_m/d42m08 if decena==3
replace lim_m=lim_m/d42m08 if decena==4
replace lim_m=lim_m/d42m09 if decena==5
replace lim_m=lim_m/d42m09 if decena==6
replace lim_m=lim_m/d42m09 if decena==7
replace lim_m=lim_m/d42m10 if decena==8
replace lim_m=lim_m/d42m10 if decena==9

*Gasto en Cristaleria y Blancos deflactado

gen cris_m=gasm if (clave>="I001" & clave<="I026")

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

gen ens_m=gasm if (clave>="K001" & clave<="K033")

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

replace sal_m=sal_m/d51t05 if decena==1
replace sal_m=sal_m/d51t06 if decena==2
replace sal_m=sal_m/d51t06 if decena==3
replace sal_m=sal_m/d51t06 if decena==4
replace sal_m=sal_m/d51t07 if decena==5
replace sal_m=sal_m/d51t07 if decena==6
replace sal_m=sal_m/d51t07 if decena==7
replace sal_m=sal_m/d51t08 if decena==8
replace sal_m=sal_m/d51t08 if decena==9

*Gasto en Transporte Publico deflactado

gen tpub_m=gasm if (clave>="B001" & clave<="B007")

replace tpub_m=tpub_m/d611w08 if decena==1
replace tpub_m=tpub_m/d611w08 if decena==2
replace tpub_m=tpub_m/d611w09 if decena==3
replace tpub_m=tpub_m/d611w09 if decena==4
replace tpub_m=tpub_m/d611w09 if decena==5
replace tpub_m=tpub_m/d611w10 if decena==6
replace tpub_m=tpub_m/d611w10 if decena==7
replace tpub_m=tpub_m/d611w10 if decena==8
replace tpub_m=tpub_m/d611w11 if decena==9

*Gasto en Transporte Foraneo deflactado

gen tfor_m=gasm if (clave>="M001" & clave<="M018") | (clave>="F007" & clave<="F011")

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

gen com_m=gasm if (clave>="F001" & clave<="F006")

replace com_m=com_m/d6m07 if decena==1
replace com_m=com_m/d6m08 if decena==2
replace com_m=com_m/d6m08 if decena==3
replace com_m=com_m/d6m08 if decena==4
replace com_m=com_m/d6m09 if decena==5
replace com_m=com_m/d6m09 if decena==6
replace com_m=com_m/d6m09 if decena==7
replace com_m=com_m/d6m10 if decena==8
replace com_m=com_m/d6m10 if decena==9

*Gasto en Educacion y Recreacion deflactado

gen edre_m=gasm if (clave>="E001" & clave<="E034") | (clave>="H029" & clave<="H030") | (clave>="L001" & clave<="L027") | (clave>="N003" & clave<="N005")

replace edre_m=edre_m/d7m07 if decena==1
replace edre_m=edre_m/d7m08 if decena==2
replace edre_m=edre_m/d7m08 if decena==3
replace edre_m=edre_m/d7m08 if decena==4
replace edre_m=edre_m/d7m09 if decena==5
replace edre_m=edre_m/d7m09 if decena==6
replace edre_m=edre_m/d7m09 if decena==7
replace edre_m=edre_m/d7m10 if decena==8
replace edre_m=edre_m/d7m10 if decena==9

*Gasto en Educacion Basica deflactado

gen edba_m=gasm if (clave>="E002" & clave<="E003") | (clave=="E015") | (clave=="H029")

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
gen edu_gtosm=gasm if (clave>="E001" & clave<="E020")   

replace edu_gtosm=edu_gtosm/d7m07 if decena==1
replace edu_gtosm=edu_gtosm/d7m08 if decena==2
replace edu_gtosm=edu_gtosm/d7m08 if decena==3
replace edu_gtosm=edu_gtosm/d7m08 if decena==4
replace edu_gtosm=edu_gtosm/d7m09 if decena==5
replace edu_gtosm=edu_gtosm/d7m09 if decena==6
replace edu_gtosm=edu_gtosm/d7m09 if decena==7
replace edu_gtosm=edu_gtosm/d7m10 if decena==8
replace edu_gtosm=edu_gtosm/d7m10 if decena==9


*Gasto en Cuidado Personal deflactado

gen cuip_m=gasm if (clave>="D001" & clave<="D024") | (clave=="H064")

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

saveold "$ruta\gastomonetario_def98.dta", replace

*********************************************************

/*Parte III

Creación del Gasto no monetario deflactado a pesos de 
agosto del 1998.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crean las bases:
	"$ruta\gastonomonetario_def98.dta"
a nivel de clave de gasto, y
	"$ruta\auto_def98.dta"
	"$ruta\esp_def98.dta"
	"$ruta\reg_def98.dta"
a nivel de folio.*/

*********************************************************

*No Monetario

use "$ruta\\nomon.dta", clear

/*En el caso de la informacion no meonetario, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual esta dada dentro
del folio de identificacion del hogar en las posiciones septima y octava. 
En primer lugar se obtiene una variable que identifique la decena de
levantamiento*/

gen decena=real(substr(folio,7,1))
tab decena

/*Asi, se puede observar que se levanto informacion en 9 decenas,
correspondientes a:

Decena                           Periodo de levantamiento

1	 |	Del 20 al 26 de agosto
2	 |	Del 30 de agosto al 05 de septiembre
3	 |	Del 09 al 15 de septiembre
4	 |	Del 19 al 25 de septiembre
5	 |	Del 29 de septiembre al 05 de octubre
6	 |	Del 09 al 15 de octubre
7	 |	Del 19 al 25 de octubre
8	 |	Del 29 de octubre al 04 de oviembre
9	 |	Del 08 al 14 de noviembre

Asi, para realizar el analisis, y conforme a lo establecido en la
informacion de la ENIGH1998, se tomara como periodos de referencia

              Periodo de Referencia
Decena|	/Semanal     /Mensual     /Trimestral           /Semestral
	
1     |	/Agosto	     /Julio       /Mayo a Julio         /Febrero a Julio
2     |	/Agosto  /Agosto      /Junio a Agosto       /Marzo a Agosto
3     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
4     |	/Septiembre  /Agosto      /Junio a Agosto       /Marzo a Agosto
5     |	/Septiembre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
6     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
7     |	/Octubre     /Septiembre  /Julio a Septiembre   /Abril a Septiembre
8     |	/Octubre     /Octubre     /Agosto a Octubre     /Mayo a Octubre
9     |	/Noviembre   /Octubre     /Agosto a Octubre     /Mayo a Octubre

Asi, se utilizaran los deflactores mensuales para el caso de las referencias semanales y mensuales
y los promedios de los meses de referencia en las referencias Trimestrales y Semestrales

*Los rubros a considerar, segun la metodologia del CTMP, seran:

Rubro                                  /Periodicidad     /Nombre del deflactor
ali_ms y Bebidas No Alcoholicas     /Semanal          /d11wmes
Bebidas alcoholicas y tabaco           /Semanal          /d12wmes
Vestido y Calzado                      /Trimestral       /d2tmesini
vivienda, servicios de conservacion    /Mensual          /d3mmes
energia y combustible
Estimacion del Alquiler                /No se deflactara /No se deflactara
Articulos y Servicios de limpieza      /Mensual          /d42mmes
cristaleria, utensilios domesticos     /Trimestral       /d42tmesini
y blancos
Enseres domesticos y muebles           /Semestral        /d41smesini
Cuidados de la salud	               /Trimestral       /d51tmesini
Transporte publico                     /Semanal          /d611wmes
Transporte foraneo, vehiculos          /Semestral        /d6smesini
Comunicaciones                        /Mensual          /d6mmes
Educacion basica                       /Mensual          /d7mmes
Articulos y servicios para el cuidado  /Mensual          /d23mmes
personal
Accesorios personales                  /Trimestral       /d23tmesini
Otros gastos diversos y transf.        /Semestral        /dINPCsmesini
Regalos otorgados                      /Semestral        /dINPCsmesini
*/
*Definición de los deflactores

*Rubro 1.1 semanal

scalar d11w08	= 1.00000000 
scalar d11w09	= 1.01678362 
scalar d11w10	= 1.03756259 
scalar d11w11	= 1.05580747 

*Rubro 1.2 semanal

scalar d12w08	= 1.00000000 
scalar d12w09	= 1.02023398 
scalar d12w10	= 1.02658608 
scalar d12w11	= 1.03121068 

*Rubro 2 trimestral

scalar d2t05	= 0.97913945 
scalar d2t06	= 0.98901711 
scalar d2t07	= 0.99967392 
scalar d2t08    = 1.01214561 

*Rubro 3 mensual

scalar d3m07	= 0.99238572 
scalar d3m08	= 1.00000000 
scalar d3m09	= 1.01163363 
scalar d3m10	= 1.02969903 
scalar d3m11	= 1.04920842 

*Rubro 4.2 mensual

scalar d42m07	= 0.98808248 
scalar d42m08	= 1.00000000 
scalar d42m09	= 1.01274869 
scalar d42m10	= 1.03533472 
scalar d42m11	= 1.05300221 

*Rubro 4.2 trimestral

scalar d42t05	= 0.97399447 
scalar d42t06	= 0.98665169 
scalar d42t07   = 1.00027706 
scalar d42t08	= 1.01602780 

*Rubro 4.1 semestral

scalar d41s02	= 0.97005801 
scalar d41s03	= 0.97923486 
scalar d41s04	= 0.98951059 
scalar d41s05	= 1.00020301 

*Rubro 5.1 trimestral

scalar d51t05	= 0.98024053 
scalar d51t06	= 0.98999429 
scalar d51t07	= 1.00050380 
scalar d51t08	= 1.01202871 

*Rubro 6.1.1 semanal

scalar d611w08  = 1.00000000 
scalar d611w09  = 1.00798900 
scalar d611w10  = 1.01028722 
scalar d611w11  = 1.01339853 

*Rubro 6 mensual

scalar d6m07	= 0.99130729 
scalar d6m08	= 1.00000000 
scalar d6m09    = 1.00895463 
scalar d6m10    = 1.01559312 
scalar d6m11    = 1.04535206 

*Rubro 6 semestral

scalar d6s02 	= 0.97331747 
scalar d6s03	= 0.98087063 
scalar d6s04	= 0.98827503 
scalar d6s05	= 0.99562411 

*Rubro 7 mensual

scalar d7m07	= 0.98724017 
scalar d7m08	= 1.00000000 
scalar d7m09	= 1.05623874 
scalar d7m10	= 1.06369495 
scalar d7m11	= 1.07099144 

*Rubro 2.3 mensual

scalar d23m07	= 0.98708185 
scalar d23m08	= 1.00000000 
scalar d23m09	= 1.01151848 
scalar d23m10	= 1.02050485 
scalar d23m11	= 1.03919676 

*Rubro 2.3 trimestral

scalar d23t05	= 0.98151380 
scalar d23t06	= 0.98917654 
scalar d23t07	= 0.99953344 
scalar d23t08	= 1.01067444 

*INPC semestral

scalar dINPCs02= 0.96631335 
scalar dINPCs03= 0.97598892 
scalar dINPCs04= 0.98652863 
scalar dINPCs05= 0.99800932 

*Una vez definidos los deflactores, se procede a limpiar la base de gasto a fin de incorporar solo
*los rubros de gasto solicitados, y agregarlos en los rubros mas generales definidos por el CTMP

drop if (clave=="G002")
drop if (clave>="K034" & clave<"K037")
drop if (clave>="Q001" & clave<="Q015")
drop if (clave=="T906")

gen double gasnomon=gas_tri/3
destring tipo_gas, replace
drop if (tipo_gas==4)
gen auto=1 if tipo_gas==1
gen esp=1 if tipo_gas==2
gen reg=1 if tipo_gas==3
gen alq=1 if tipo_gas==0

*Gasto en alimentos deflactado

gen ali_nm=gasnomon if (clave>="A001" & clave<="A194") | (clave>="A205" & clave<="A208")

replace ali_nm=ali_nm/d11w08 if decena==1
replace ali_nm=ali_nm/d11w08 if decena==2
replace ali_nm=ali_nm/d11w09 if decena==3
replace ali_nm=ali_nm/d11w09 if decena==4
replace ali_nm=ali_nm/d11w09 if decena==5
replace ali_nm=ali_nm/d11w10 if decena==6
replace ali_nm=ali_nm/d11w10 if decena==7
replace ali_nm=ali_nm/d11w10 if decena==8
replace ali_nm=ali_nm/d11w11 if decena==9

*Gasto en Alcohol y Tabaco deflactado

gen alta_nm=gasnomon if (clave>="A195" & clave<="A204") | (clave>="A209" & clave<="A211")

replace alta_nm=alta_nm/d12w08 if decena==1
replace alta_nm=alta_nm/d12w08 if decena==2
replace alta_nm=alta_nm/d12w09 if decena==3
replace alta_nm=alta_nm/d12w09 if decena==4
replace alta_nm=alta_nm/d12w09 if decena==5
replace alta_nm=alta_nm/d12w10 if decena==6
replace alta_nm=alta_nm/d12w10 if decena==7
replace alta_nm=alta_nm/d12w10 if decena==8
replace alta_nm=alta_nm/d12w11 if decena==9

*Gasto en Vestido y Calzado deflactado

gen veca_nm=gasnomon if (clave>="H001" & clave<="H028") | (clave>="H031" & clave<="H055")

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

gen viv_nm=gasnomon if (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G014") | (clave>="G016" & clave<="G033")

replace viv_nm=viv_nm/d3m07 if decena==1
replace viv_nm=viv_nm/d3m08 if decena==2
replace viv_nm=viv_nm/d3m08 if decena==3
replace viv_nm=viv_nm/d3m08 if decena==4
replace viv_nm=viv_nm/d3m09 if decena==5
replace viv_nm=viv_nm/d3m09 if decena==6
replace viv_nm=viv_nm/d3m09 if decena==7
replace viv_nm=viv_nm/d3m10 if decena==8
replace viv_nm=viv_nm/d3m10 if decena==9

*Gasto en Articulos de Limpieza deflactado

gen lim_nm = gasnomon if (clave>="C001" & clave<="C024")

replace lim_nm=lim_nm/d42m07 if decena==1
replace lim_nm=lim_nm/d42m08 if decena==2
replace lim_nm=lim_nm/d42m08 if decena==3
replace lim_nm=lim_nm/d42m08 if decena==4
replace lim_nm=lim_nm/d42m09 if decena==5
replace lim_nm=lim_nm/d42m09 if decena==6
replace lim_nm=lim_nm/d42m09 if decena==7
replace lim_nm=lim_nm/d42m10 if decena==8
replace lim_nm=lim_nm/d42m10 if decena==9

*Gasto en Cristaleria y Blancos deflactado

gen cris_nm=gasnomon if (clave>="I001" & clave<="I026")

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

gen ens_nm=gasnomon if (clave>="K001" & clave<="K033")

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

replace sal_nm=sal_nm/d51t05 if decena==1
replace sal_nm=sal_nm/d51t06 if decena==2
replace sal_nm=sal_nm/d51t06 if decena==3
replace sal_nm=sal_nm/d51t06 if decena==4
replace sal_nm=sal_nm/d51t07 if decena==5
replace sal_nm=sal_nm/d51t07 if decena==6
replace sal_nm=sal_nm/d51t07 if decena==7
replace sal_nm=sal_nm/d51t08 if decena==8
replace sal_nm=sal_nm/d51t08 if decena==9

*Gasto en Transporte Publico deflactado

gen tpub_nm=gasnomon if (clave>="B001" & clave<="B007")

replace tpub_nm=tpub_nm/d611w08 if decena==1
replace tpub_nm=tpub_nm/d611w08 if decena==2
replace tpub_nm=tpub_nm/d611w09 if decena==3
replace tpub_nm=tpub_nm/d611w09 if decena==4
replace tpub_nm=tpub_nm/d611w09 if decena==5
replace tpub_nm=tpub_nm/d611w10 if decena==6
replace tpub_nm=tpub_nm/d611w10 if decena==7
replace tpub_nm=tpub_nm/d611w10 if decena==8
replace tpub_nm=tpub_nm/d611w11 if decena==9

*Gasto en Transporte Foraneo deflactado

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M018") | (clave>="F007" & clave<="F011")

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

gen com_nm=gasnomon if (clave>="F001" & clave<="F006")

replace com_nm=com_nm/d6m07 if decena==1
replace com_nm=com_nm/d6m08 if decena==2
replace com_nm=com_nm/d6m08 if decena==3
replace com_nm=com_nm/d6m08 if decena==4
replace com_nm=com_nm/d6m09 if decena==5
replace com_nm=com_nm/d6m09 if decena==6
replace com_nm=com_nm/d6m09 if decena==7
replace com_nm=com_nm/d6m10 if decena==8
replace com_nm=com_nm/d6m10 if decena==9

*Gasto en Educacion y Recreacion deflactado

gen edre_nm=gasnomon if (clave>="E001" & clave<="E034") | (clave>="H029" & clave<="H030") | (clave>="L001" & clave<="L027") | (clave>="N003" & clave<="N005")

replace edre_nm=edre_nm/d7m07 if decena==1
replace edre_nm=edre_nm/d7m08 if decena==2
replace edre_nm=edre_nm/d7m08 if decena==3
replace edre_nm=edre_nm/d7m08 if decena==4
replace edre_nm=edre_nm/d7m09 if decena==5
replace edre_nm=edre_nm/d7m09 if decena==6
replace edre_nm=edre_nm/d7m09 if decena==7
replace edre_nm=edre_nm/d7m10 if decena==8
replace edre_nm=edre_nm/d7m10 if decena==9


*Gasto en Educacion Basica deflactado

gen edba_nm=gasnomon if (clave>="E002" & clave<="E003") | (clave=="E015") | (clave=="H029")

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

replace reda_nm=reda_nm/dINPCs02 if decena==1
replace reda_nm=reda_nm/dINPCs03 if decena==2
replace reda_nm=reda_nm/dINPCs03 if decena==3
replace reda_nm=reda_nm/dINPCs03 if decena==4
replace reda_nm=reda_nm/dINPCs04 if decena==5
replace reda_nm=reda_nm/dINPCs04 if decena==6
replace reda_nm=reda_nm/dINPCs04 if decena==7
replace reda_nm=reda_nm/dINPCs05 if decena==8
replace reda_nm=reda_nm/dINPCs05 if decena==9

*Estimacion del Alquiler

gen vivprob=gasnomon if (clave=="G001")
gen vivrecib=gasnomon if (clave=="G007")
gen vivprest=gasnomon if (clave=="G010")
gen vivotra=gasnomon if (clave=="G015")
gen double est_alq=alq*gasnomon
egen ren_imp=rsum(vivprob vivrecib vivprest vivotra)

saveold "$ruta\gastonomonetario_def98.dta", replace

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

saveold "$ruta\auto_def98.dta", replace

use "$ruta\gastonomonetario_def98.dta", clear


*Construccion de la base de pagos en especie a partir de la base de gasto no monetario

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

saveold "$ruta\esp_def98.dta", replace

use "$ruta\gastonomonetario_def98.dta", clear


*Construccion de base de regalos a partir de la base no monetaria 

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

saveold "$ruta\reg_def98.dta", replace

*********************************************************

/*Parte IV

Cálculo de la incidencia 1998

Con el propósito de obtener cifras representativas a nivel 
nacional, rural y urbano, se obtiene una base con las 
variables de tamaño del hogar, estrato y factor de 
expansión, y a ésta se le unen las bases anteriormente 
generadas. Se estiman los ingresos corriente total y 
corriente neto, se genera el factor de expansión para 
personas, y se calcula la incidencia de la pobreza de 
acuerdo con el cálculo oficial de la Sedesol.

Se genera la base 
	"$ruta\basefinal_98.dta" 
la cual se encuentra a nivel de folio.*/




*********************************************************

use "$ruta\concen.dta", clear
rename hog factor

keep folio tam_hog  factor estrato educacion
sort folio


*Modificado Mayra Sáenz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*merge folio using "$ruta\ingreso_deflactado98.dta"
tab _merge
drop _merge
sort folio*/

merge 1:1 folio using "$ruta\gastomonetario_def98.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\auto_def98.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\esp_def98.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\reg_def98.dta"
tab _merge
drop _merge
sort folio

*Construccion de rural y urbano, criterio de 15000 habitantes 

destring estrato, replace
gen rururb=1 if (estrato>2 & estrato!=.)
replace rururb=0 if estrato<=2
label define rururb 1 "Rural" 0 "Urbano"
label value rururb rururb

*Se calcula los gastos totales

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


*Modificado Mayra Sáenz Julio 2015 - Incluyo la base de hogares

merge 1:1 folio using "$ruta\vivi98.dta", force
tab _merge 
drop _merge 
sort folio



saveold "$ruta\gtos_autoc98.dta", replace //Mayra Sáenz Julio 2015

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*


use "$ruta\person98.dta", clear

*rename folio folio1              //Modificado Mayra Sáenz - Julio 2015
*gen folio = substr(folio1,4,100) //Modificado Mayra Sáenz - Julio 2015


sort folio num_ren, stable

merge 1:1 folio num_ren using "$ruta\ingreso_deflactado98_per.dta"
rename _merge _merge_ing
sort folio num_ren, stable

merge m:1 folio using "$ruta\gtos_autoc98.dta"

*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict=rsum(ing_monh nomon)                 if parentes=="01" | parentes=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gct=rsum(gasmon nomon)                   if parentes=="01" | parentes=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gnt=rsum(gasmon nomon redan reg_espn)    if parentes=="01" | parentes=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if parentes=="01" | parentes=="02" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas


label var  ict "Ingreso corriente total"
label var  gct "Gasto corriente total"
label var  intt "Ingreso neto total"
label var  gnt "Gasto neto total"

*Informacion per capita

gen double ictpc= ict/tam_hog
gen double gctpc= gct/tam_hog
gen double intpc= intt/tam_hog
gen double gntpc= gnt/tam_hog

label var  ictpc "Ingreso corriente total per capita"
label var  gctpc "Gasto corriente totalper capita"
label var  intpc "Ingreso neto total per capita"
label var  gntpc "Gasto neto total per capita"

summ  gasmon autocons pago_esp reg_esp nomon reda ict gct intt gnt intpc gntpc ictpc gctpc


saveold "`base_out'", replace


log close

/*
use "$ruta\vivi98.dta",clear
sort folio
saveold "$ruta\MEX1998EA_BID.dta",replace
use "$ruta\person98.dta", clear
sort folio
merge folio using "$ruta\MEX1998EA_BID.dta"
drop _merge
sort folio num_ren
gen id=_n
saveold "$ruta\MEX1998EA_BID.dta",replace
use "$ruta\ingres98.dta", clear
gen ocupa=empleo
tab  empleo,gen(ocup) /*se genera para saber la numero de ocupacion*/
gen clave1=substr(clave,1,1)
gen clave2=real(substr(clave,2,3))
sort folio num_ren
by folio num_ren:egen ring1a_ml=sum(ing_1) if clave2>0 & clave2<=9 & ocupa=="1" 
by folio num_ren:egen ring1b_ml=sum(ing_1) if clave2==18 & ocupa=="1" 
replace ring1a_ml=0 if ring1a_ml==.
replace ring1b_ml=0 if ring1b_ml==.
by folio num_ren:gen ring1_ml=ring1a_ml+ring1b_ml
by folio num_ren:egen ing1_ml=max(ring1_ml)
 
by folio num_ren:egen ring2a_ml=sum(ing_1) if clave2>0 & clave2<=9 & ocupa~="1" 
by folio num_ren:egen ring2b_ml=sum(ing_1) if clave2==18 & ocupa~="1" 
replace ring2a_ml=0 if ring2a_ml==.
replace ring2b_ml=0 if ring2b_ml==.
by folio num_ren:gen ring2_ml=ring2a_ml+ring2b_ml
by folio num_ren:egen ing2_ml=max(ring2_ml)

by folio num_ren:egen ring1a_npm=sum(ing_1) if clave2>9 & clave2<=17 & ocupa=="1" 
by folio num_ren:egen ring1b_npm=sum(ing_1) if clave2==19 & ocupa=="1" 
replace ring1a_npm=0 if ring1a_npm==.
replace ring1b_npm=0 if ring1b_npm==.
by folio num_ren:gen ring1_npm=ring1a_npm+ring1b_npm
by folio num_ren:egen ing1_npm=max(ring1_npm)
 
by folio num_ren:egen double ring2a_npm=sum(ing_1) if clave2>9 & clave2<=17 & ocupa~="1" 
by folio num_ren:egen double ring2b_npm=sum(ing_1) if clave2==19 & ocupa~="1" 
replace ring2a_npm=0 if ring2a_npm==.
replace ring2b_npm=0 if ring2b_npm==.
by folio num_ren:gen ring2_npm=ring2a_npm+ring2b_npm
by folio num_ren:egen ing2_npm=max(ring2_npm)

/*renta*/
by folio num_ren:egen ring_rpm=sum(ing_1) if clave2>=20 & clave2<=27 
replace ring_rpm=0 if ring_rpm==.
by folio num_ren:egen ing_rpm=max(ring_rpm)
/*transferencias*/
by folio num_ren:egen ring_tm=sum(ing_1) if  clave2>=28 & clave2<=32 
replace ring_tm=0 if ring_tm==.
by folio num_ren:egen ing_tm=max(ring_tm)

*transferencias por jublaciones y/o pensiones*
by folio num_ren:egen ring_jub=sum(ing_1) if  clave2==28
replace ring_jub=0 if ring_jub==.
by folio num_ren:egen ing_jub=max(ring_jub)

/*regalos*/
by folio num_ren:egen ring_otm=sum(ing_1) if clave2==34
replace ring_otm=0 if ring_otm==.
by folio num_ren:egen ing_otm=max(ring_otm)
/*remesas*/
by folio num_ren:egen ring_otrosp=sum(ing_1) if clave2==33  
replace ring_otrosp=0 if ring_otrosp==.
by folio num_ren:egen ing_otrosp=max(ring_otrosp)
/*financieras*/
by folio num_ren:egen ring_otm1=sum(ing_1) if clave2>=35 &  clave2<=36
replace ring_otm1=0 if ring_otm1==.
by folio num_ren:egen ing_otm1=max(ring_otm1)
by folio num_ren:gen cont=(_n) 
drop if cont>1
drop  rin* ocupa empleo clave ing_1  ing_tri ing_2 ing_3 ing_4 ing_5 ing_6 cont
sort folio num_ren
merge folio num_ren using "$ruta\MEX1998EA_BID.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge
sort folio num_ren
saveold "$ruta\MEX1998EA_BID.dta",replace
use "$ruta\nomon98.dta", clear
sort folio num_ren
by folio num_ren:egen ing_nml=sum(gasto) 

by folio num_ren:gen cont=(_n) /*elimino repetidos*/
drop if cont>1
tostring  folio,replace
keep folio num_ren ing_nml
sort folio num_ren
merge folio num_ren using "$ruta\MEX1998EA_BID.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge 



saveold "`base_out'", replace


log close


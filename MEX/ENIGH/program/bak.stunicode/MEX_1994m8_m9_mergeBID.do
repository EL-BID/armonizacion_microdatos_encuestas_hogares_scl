* (Versi�n Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\MEX\ENIGH\1994\m8_m9\data_orig"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "1994"
local ronda m8_m9

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Mexico
Encuesta: ENIGH (Nueva construcci�n)
Round: Agosto-Septiembre
Autores:
Versi�n 2013: Mayra S�enz
�ltima versi�n: Mayra S�enz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha �ltima modificaci�n: 6 de Agosto de 2015

							SCL/LMK - IADB
****************************************************************************/


*Mayra S�enz - Agosto 2015: Se realiza el merge con base en la sintaxis de CONEVAL, 
*pero con algunas modificaciones, y generando nuevas variables.


****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
************************* PARA MEXICO 1994 *********************************
****************************************************************************


*********************************************************

/*Este programa debe ser utilizado con el Software Stata 
versi�n 8 o superior. 

Todas las bases de datos de la ENIGH pueden ser obtenidas 
en la p�gina de Internet del INEGI, www.inegi.gob.mx, y 
deben estar convertidas a formato *.dta. 

Es importante renombrar las bases que se descargan del 
INEGI de acuerdo a la siguiente estructura, con la finalidad
de que el programa no tenga problemas al identificar las bases
que se descargan del INEGI: 

Base de ingresos: ingreso94.dta
Base de gasto monetario: gasto94.dta
Base de gasto no monetario: nomonetario94.dta
Base de concentrado: concentrado94.dta

En este programa se utilizan tres tipos de archivos, los 
cuales est�n ubicados en las siguientes carpetas:

1) Bases originales: "${surveysFolder}\pobreza ingresos\1994\ENIGH"
2) Bit�coras: "${surveysFolder}\pobreza ingresos\1994\Log"
3) Bases generadas: "${surveysFolder}\pobreza ingresos\1994\Resultados" 
  */

*********************************************************
*
*	PROGRAMA PARA LA MEDICI�N DE LA POBREZA 1994
*
*********************************************************

*CONEVAL: �ltima modificaci�n: 24 de julio del 2013

*********************************************************

*Parte I
/*
Creacion del Ingreso Monetario deflactado a pesos de 
Agosto del 1994.

Los deflactores se crean previamente a partir del INPC 
general (ver Nota T�cnica).

En esta parte se crea la base:
"$ruta\ingreso_deflactado94.dta"
a nivel de folio.*/

******************************************************** 

*Ingresos 

use "$ruta\ingresos.dta" , clear

/*La variable �meses� define los meses a los que corresponden
cada uno de los ingresos de la persona en los seis meses 
anteriores al levantamiento de la informaci�n. Esta variable
toma cuatro valores:

080706050403
090807060504
100908070605
111009080706

indicando que la encuesta fue levantada entre los meses de
agosto y septiembre, y por tanto, al preguntarse por los ingresos
de los ultimos seis meses, se recolecto informacion 
correspondiente a marzo-agosto, abril-septiembre,
mayo-octubre y junio-noviembre */

*Definicion de los deflactores 1994 

scalar ene94=	0.96654191	 
scalar feb94=	0.97151275	 
scalar mar94=	0.97650834	 
scalar abr94=	0.98129074	 
scalar may94=	0.98603185	 
scalar jun94=	0.99096607	 
scalar jul94=	0.99536082	 
scalar ago94=	1.00000000	 
scalar sep94=	1.00711221	 
scalar oct94=	1.01239890	 
scalar nov94=	1.01781122	 
scalar dic94=	1.02673818	 

rename mes2 ing2 
rename mes3 ing3 
rename mes4 ing4 
rename mes5 ing5 
rename mes6 ing6 

*La estrategia para deflactar sera dividir cada columna de ingreso por el deflactor correspondiente a su mes

replace ing6=ing6/mar94 if meses=="080706050403" 
replace ing5=ing5/abr94 if meses=="080706050403" 
replace ing4=ing4/may94 if meses=="080706050403" 
replace ing3=ing3/jun94 if meses=="080706050403" 
replace ing2=ing2/jul94 if meses=="080706050403" 
replace ing1=ing1/ago94 if meses=="080706050403" 

replace ing6=ing6/abr94 if meses=="090807060504" 
replace ing5=ing5/may94 if meses=="090807060504" 
replace ing4=ing4/jun94 if meses=="090807060504" 
replace ing3=ing3/jul94 if meses=="090807060504" 
replace ing2=ing2/ago94 if meses=="090807060504" 
replace ing1=ing1/sep94 if meses=="090807060504" 

replace ing6=ing6/may94 if meses=="100908070605" 
replace ing5=ing5/jun94 if meses=="100908070605" 
replace ing4=ing4/jul94 if meses=="100908070605" 
replace ing3=ing3/ago94 if meses=="100908070605" 
replace ing2=ing2/sep94 if meses=="100908070605" 
replace ing1=ing1/oct94 if meses=="100908070605" 

replace ing6=ing6/jun94 if meses=="111009080706" 
replace ing5=ing5/jul94 if meses=="111009080706" 
replace ing4=ing4/ago94 if meses=="111009080706" 
replace ing3=ing3/sep94 if meses=="111009080706" 
replace ing2=ing2/oct94 if meses=="111009080706" 
replace ing1=ing1/nov94 if meses=="111009080706" 

*Mayra S�enz Julio 2015- Mes de referencia

g mes_ref = substr(meses, 1, 2)
destring mes_ref, replace


*Una vez realizada la deflactacion, se procede a obtener el promedio de ingreso mensual en los ultimos 6 meses para cada persona y clave de ingreso 

egen double ing_mens=rmean(ing1 ing2 ing3 ing4 ing5 ing6) 

gen double ing_mon=ing_mens if clave>="P001" & clave<="P028" 
gen double ing_lab=ing_mens if clave>="P001" & clave<="P015" 

**Modificaci�n Mayra S�enz - Julio 2015 : Son ingresos laborales de P001-P014, se desagrega por actividad principal y secundaria
gen     ocuprisec = 1 if empleo =="1" 
replace ocuprisec = 2 if (empleo !="1")

forval j = 1/2 {
gen double ing_trab`j'=ing_mens if (clave>="P001" & clave<="P005") & ocuprisec == `j'
gen double ing_negp`j'=ing_mens if (clave>="P006" & clave<="P015") & ocuprisec == `j'
}

gen double ing_trab=ing_mens if clave>="P001" & clave<="P005" 
gen double ing_negp=ing_mens if clave>="P006" & clave<="P015" 
gen double ing_rent=ing_mens if clave>="P016" & clave<="P022" 
gen double ing_tran=ing_mens if clave>="P023" & clave<="P028" 



*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens if  (clave=="P023")                                      //Jubilaciones y pensiones
g double trat_pr = ing_mens  if  ((clave>="P024" & clave<="P025") | clave=="P028")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por despido y accidentes de trabajo, Ingresos provenientes de otros paises
g double trat_pu  = 0
g double dona_pu  = 0                                                                 
g double dona_pr  = ing_mens if  (clave=="P026" | clave=="P027")                      //Becas y donaciones provenientes de Instituciones, Regalos y donativos originados dentro del pa�s
g double otros   = ing_mens  if ((clave>="P029" & clave<="P030"))                     //Venta de autom�viles, aparatos electricos de segunda mano, etc.  Otros ingresos corrientes no considerados en los anteriores, (viáticos, etc.).
g double remesas = ing_mens  if  (clave=="P028")                                      //Ingresos provenientes de otros paises

*Modificaci�n Mayra S�enz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}

*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.

*A continuacion, se estima el total de ingresos que cada hogar recibe, y se guarda la base en un archivo aparte 

*collapse (sum) ing_mon ing_lab ing_trab ing_negp ing_rent ing_tran, by(folio) 

*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.
*A continuacionn se estima el total de ingresos de cada individuo, y se guarda la base en un archivo aparte

collapse (sum) ing_mens ing_mon ing_lab ing_trab* ing_negp* ing_rent ing_tran ypension trat_pr trat_pu dona_pu dona_pr otros remesas P* (mean) mes_ref, by(folio num_ren)

label var folio     "Identificador del hogar"
label var ing_mon   "Ingreso corriente monetario del hogar"
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

rename folio folio1              //Modificado Mayra S�enz - Julio 2015
gen folio = substr(folio1,4,100) //Modificado Mayra S�enz - Julio 2015

saveold "$ruta\ingreso_deflactado94_per.dta", replace 

*********************************************************

/*Parte II

Creacion del Gasto Monetario deflactado a pesos de 
Agosto del 1994.

Los deflactores se crean previamente a partir del INPC 
desagregado, segun archivo de excel anexo.

En esta parte se crea la base:
"$ruta\gastomonetario_def94.dta"
a nivel de folio.*/

********************************************************* 

*Gastos monetario 

use "$ruta\gastos94.dta", clear

/*En el caso de la informacion de gasto, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual esta dada dentro
del folio de identificacion del hogar en la cuarta posicion. En primer lugar se obtiene una variable que identifique la decena de
levantamiento */

gen decena=real(substr(folio,7,1)) 
tab decena 

/*Asi, se puede observar que se levanto informacion en 9 decenas,
correspondientes a:

Decena                           Periodo de Levantamiento

1	 |	del 22 al 28 de Septiembre
2	 |	del 02 al 08 de Octubre
3	 |	del 12 al 18 de octubre
4	 |	del 22 al 28 de octubre
5	 |	del 01 al 07 de noviembre
6	 |	del 11 al 17 de noviembre
7	 |	del 21 al 27 de noviembre
8	 |	del 01 al 07 de diciembre
9	 |	del 11 al 17 de diciembre

Asi, para realizar el analisis, y conforme a lo establecido en la
informacion de la ENIGH1994, se tomara como periodos de referencia

              Periodo de Referencia
Decena|	/Semanal     /Mensual     /Trimestral           /Semestral
	
1     |	/Septiembre	     /Agosto       /Junio a Agosto         /Marzo a Agosto
2     |	/Octubre  /Septiembre      /Julio a Septiembre       /Abril a Septiembre
3     |	/Octubre  /Septiembre      /Julio a Septiembre       /Abril a Septiembre
4     |	/Octubre  /Septiembre      /Julio a Septiembre       /Abril a Septiembre
5     |	/Noviembre     /Octubre  /Agosto a Octubre   /Mayo a Octubre
6     |	/Noviembre     /Octubre  /Agosto a Octubre   /Mayo a Octubre
7     |	/Noviembre     /Octubre  /Agosto a Octubre   /Mayo a Octubre
8     |	/Diciembre     /Noviembre     /Septiembre a Noviembre     /Junio a Noviembre
9     |	/Diciembre   /Noviembre     /Septiembre a Noviembre    /Junio a Noviembre

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

A continuacion se definen escalares que contienen cada uno de estos deflactores, a partir del
archivo de excel anexo (no se requieren deflactores mensuales antes de agosto, por lo que no se
incorporan, si bien si fueron calculados) */

*Rubro 1.1 semanal 			
			
scalar d11w09	= 	1.0066591	 
scalar d11w10	= 	1.0120936	 
scalar d11w11	= 	1.0189161	 
scalar d11w12	= 	1.030306	 
			
*Rubro 1.2 semanal 			
			
scalar d12w09	= 	1.0283309	 
scalar d12w10	= 	1.0381586	 
scalar d12w11	= 	1.0439539	 
scalar d12w12	= 	1.0476356	 
			
*Rubro 2 trimestral 			
			
scalar d2t06	= 	0.995164	 
scalar d2t07	= 	0.9972957	 
scalar d2t08	= 	1.0028385	 
scalar d2t09	= 	1.0066064	 
			
*Rubro 3 mensual 			
			
scalar d3m08	= 	1	 
scalar d3m09	= 	1.0047596	 
scalar d3m10	= 	1.0103664	 
scalar d3m11	= 	1.0099404	 
			
*Rubro 4.2 mensual 			
			
scalar d42m08	= 	1	 
scalar d42m09	= 	1.0041214	 
scalar d42m10	= 	1.0071464	 
scalar d42m11	= 	1.0149518	 
			
*Rubro 4.2 trimestral 			
			
scalar d42t06	= 	0.9925574	 
scalar d42t07	= 	0.9982169	 
scalar d42t08	= 	1.0037559	 
scalar d42t09	= 	1.0087399	 
			
*Rubro 4.1 semestral 			
			
scalar d41s03	= 	0.9772681	 
scalar d41s04	= 	0.9853673	 
scalar d41s05	= 	0.9911522	 
scalar d41s06	= 	0.9965657	 
			
*Rubro 5.1 trimestral 			
			
scalar d51t06	= 	0.9869536	 
scalar d51t07	= 	1.000522	 
scalar d51t08	= 	1.0152349	 
scalar d51t09	= 	1.028321	 
			
*Rubro 6.1.1 semanal 			
			
scalar d611w09	= 	0.9999127	 
scalar d611w10	= 	1.0019927	 
scalar d611w11	= 	1.0054119	 
scalar d611w12	= 	1.0128794	 
			
*Rubro 6 mensual 			
			
scalar d6m08	= 	1	 
scalar d6m09	= 	1.0030515	 
scalar d6m10	= 	1.0093798	 
scalar d6m11	= 	1.0170424	 
			
*Rubro 6 semestral 			
			
scalar d6s03	= 	0.9824005	 
scalar d6s04	= 	0.9887934	 
scalar d6s05	= 	0.9949719	 
scalar d6s06 	= 	1.0015035	 
			
*Rubro 7 mensual 			
			
scalar d7m08	= 	1	 
scalar d7m09	= 	1.0622282	 
scalar d7m10	= 	1.0769648	 
scalar d7m11	= 	1.0926897	 
			
*Rubro 2.3 mensual 			
			
scalar d23m08	= 	1	 
scalar d23m09	= 	1.0084757	 
scalar d23m10	= 	1.0178758	 
scalar d23m11	= 	1.0226111	 
			
*Rubro 2.3 trimestral 			
			
scalar d23t06	= 	0.9941166	 
scalar d23t07	= 	1.0004248	 
scalar d23t08	= 	1.0087838	 
scalar d23t09	= 	1.0163208	 
			
*INPC semestral 			
			
scalar dINPCs03	= 	0.9883596	 
scalar dINPCs04	= 	0.9934603	 
scalar dINPCs05	= 	0.998645	 
scalar dINPCs06	= 	1.0039415	 


/*Una vez definidos los deflactores, se procede a limpiar la base de gasto a fin de incorporar solo
los rubros de gasto solicitados, y agregarlos en los rubros mas generales definidos por el CTMP */

drop if (clave=="G002") 

gen double gasm=gas_tri/3 

*Gasto en alimentos deflactado 

gen ali_m=gasm if (clave>="A001" & clave<="A193") | (clave>="A204" & clave<="A207") 

replace ali_m=ali_m/d11w09 if decena==1 
replace ali_m=ali_m/d11w10 if decena==2 
replace ali_m=ali_m/d11w10 if decena==3 
replace ali_m=ali_m/d11w10 if decena==4 
replace ali_m=ali_m/d11w11 if decena==5 
replace ali_m=ali_m/d11w11 if decena==6 
replace ali_m=ali_m/d11w11 if decena==7 
replace ali_m=ali_m/d11w12 if decena==8 
replace ali_m=ali_m/d11w12 if decena==9 

*Gasto en Alcohol y Tabaco deflactado 

gen alta_m=gasm if (clave>="A194" & clave<="A203") | (clave>="A208" & clave<="A210") 

replace alta_m=alta_m/d12w09 if decena==1 
replace alta_m=alta_m/d12w10 if decena==2 
replace alta_m=alta_m/d12w10 if decena==3 
replace alta_m=alta_m/d12w10 if decena==4 
replace alta_m=alta_m/d12w11 if decena==5 
replace alta_m=alta_m/d12w11 if decena==6 
replace alta_m=alta_m/d12w11 if decena==7 
replace alta_m=alta_m/d12w12 if decena==8 
replace alta_m=alta_m/d12w12 if decena==9 

*Gasto en Vestido y Calzado deflactado 

gen veca_m=gasm if (clave>="H001" & clave<="H028") | (clave>="H031" & clave<="H055") 

replace veca_m=veca_m/d2t06 if decena==1 
replace veca_m=veca_m/d2t07 if decena==2 
replace veca_m=veca_m/d2t07 if decena==3 
replace veca_m=veca_m/d2t07 if decena==4 
replace veca_m=veca_m/d2t08 if decena==5 
replace veca_m=veca_m/d2t08 if decena==6 
replace veca_m=veca_m/d2t08 if decena==7 
replace veca_m=veca_m/d2t09 if decena==8 
replace veca_m=veca_m/d2t09 if decena==9 

*Gasto en Vivienda deflactado 

gen viv_m=gasm if (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G014") | (clave>="G016" & clave<="G033") 

replace viv_m=viv_m/d3m08 if decena==1 
replace viv_m=viv_m/d3m09 if decena==2 
replace viv_m=viv_m/d3m09 if decena==3 
replace viv_m=viv_m/d3m09 if decena==4 
replace viv_m=viv_m/d3m10 if decena==5 
replace viv_m=viv_m/d3m10 if decena==6 
replace viv_m=viv_m/d3m10 if decena==7 
replace viv_m=viv_m/d3m11 if decena==8 
replace viv_m=viv_m/d3m11 if decena==9 

*Gasto en Articulos de Limpieza deflactado 

gen lim_m=gasm if (clave>="C001" & clave<="C024") 

replace lim_m=lim_m/d42m08 if decena==1 
replace lim_m=lim_m/d42m09 if decena==2 
replace lim_m=lim_m/d42m09 if decena==3 
replace lim_m=lim_m/d42m09 if decena==4 
replace lim_m=lim_m/d42m10 if decena==5 
replace lim_m=lim_m/d42m10 if decena==6 
replace lim_m=lim_m/d42m10 if decena==7 
replace lim_m=lim_m/d42m11 if decena==8 
replace lim_m=lim_m/d42m11 if decena==9 

*Gasto en Cristaleria y Blancos deflactado 

gen cris_m=gasm if (clave>="I001" & clave<="I026") 

replace cris_m=cris_m/d42t06 if decena==1 
replace cris_m=cris_m/d42t07 if decena==2 
replace cris_m=cris_m/d42t07 if decena==3 
replace cris_m=cris_m/d42t07 if decena==4 
replace cris_m=cris_m/d42t08 if decena==5 
replace cris_m=cris_m/d42t08 if decena==6 
replace cris_m=cris_m/d42t08 if decena==7 
replace cris_m=cris_m/d42t09 if decena==8 
replace cris_m=cris_m/d42t09 if decena==9 

*Gasto en Enseres deflactado 

gen ens_m=gasm if (clave>="K001" & clave<="K029") 

replace ens_m=ens_m/d41s03 if decena==1 
replace ens_m=ens_m/d41s04 if decena==2 
replace ens_m=ens_m/d41s04 if decena==3 
replace ens_m=ens_m/d41s04 if decena==4 
replace ens_m=ens_m/d41s05 if decena==5 
replace ens_m=ens_m/d41s05 if decena==6 
replace ens_m=ens_m/d41s05 if decena==7 
replace ens_m=ens_m/d41s06 if decena==8 
replace ens_m=ens_m/d41s06 if decena==9 

*Gasto en Salud deflactado 

gen sal_m=gasm if (clave>="J001" & clave<="J043") 

replace sal_m=sal_m/d51t06 if decena==1 
replace sal_m=sal_m/d51t07 if decena==2 
replace sal_m=sal_m/d51t07 if decena==3 
replace sal_m=sal_m/d51t07 if decena==4 
replace sal_m=sal_m/d51t08 if decena==5 
replace sal_m=sal_m/d51t08 if decena==6 
replace sal_m=sal_m/d51t08 if decena==7 
replace sal_m=sal_m/d51t09 if decena==8 
replace sal_m=sal_m/d51t09 if decena==9 

*Gasto en Transporte Publico deflactado 

gen tpub_m=gasm if (clave>="B001" & clave<="B007") 

replace tpub_m=tpub_m/d611w09 if decena==1 
replace tpub_m=tpub_m/d611w10 if decena==2 
replace tpub_m=tpub_m/d611w10 if decena==3 
replace tpub_m=tpub_m/d611w10 if decena==4 
replace tpub_m=tpub_m/d611w11 if decena==5 
replace tpub_m=tpub_m/d611w11 if decena==6 
replace tpub_m=tpub_m/d611w11 if decena==7 
replace tpub_m=tpub_m/d611w12 if decena==8 
replace tpub_m=tpub_m/d611w12 if decena==9 

*Gasto en Transporte Foraneo deflactado 

gen tfor_m=gasm if (clave>="M001" & clave<="M018") | (clave>="F006" & clave<="F010") 

replace tfor_m=tfor_m/d6s03 if decena==1 
replace tfor_m=tfor_m/d6s04 if decena==2 
replace tfor_m=tfor_m/d6s04 if decena==3 
replace tfor_m=tfor_m/d6s04 if decena==4 
replace tfor_m=tfor_m/d6s05 if decena==5 
replace tfor_m=tfor_m/d6s05 if decena==6 
replace tfor_m=tfor_m/d6s05 if decena==7 
replace tfor_m=tfor_m/d6s06 if decena==8 
replace tfor_m=tfor_m/d6s06 if decena==9 

*Gasto en Comunicaciones deflactado 

gen com_m=gasm if (clave>="F001" & clave<="F005") 

replace com_m=com_m/d6m08 if decena==1 
replace com_m=com_m/d6m09 if decena==2 
replace com_m=com_m/d6m09 if decena==3 
replace com_m=com_m/d6m09 if decena==4 
replace com_m=com_m/d6m10 if decena==5 
replace com_m=com_m/d6m10 if decena==6 
replace com_m=com_m/d6m10 if decena==7 
replace com_m=com_m/d6m11 if decena==8 
replace com_m=com_m/d6m11 if decena==9 

*Gasto en Educacion y Recreacion deflactado 

gen edre_m=gasm if (clave>="E001" & clave<="E031") | (clave>="H029" & clave<="H030") | (clave>="L001" & clave<="L027") | (clave>="N003" & clave<="N005") 

replace edre_m=edre_m/d7m08 if decena==1 
replace edre_m=edre_m/d7m09 if decena==2 
replace edre_m=edre_m/d7m09 if decena==3 
replace edre_m=edre_m/d7m09 if decena==4 
replace edre_m=edre_m/d7m10 if decena==5 
replace edre_m=edre_m/d7m10 if decena==6 
replace edre_m=edre_m/d7m10 if decena==7 
replace edre_m=edre_m/d7m11 if decena==8 
replace edre_m=edre_m/d7m11 if decena==9 

*Gasto en Educacion Basica deflactado 

gen edba_m=gasm if (clave>="E003" & clave<="E004") | (clave == "E014") | (clave =="H029" ) 

replace edba_m=edba_m/d7m08 if decena==1 
replace edba_m=edba_m/d7m09 if decena==2 
replace edba_m=edba_m/d7m09 if decena==3 
replace edba_m=edba_m/d7m09 if decena==4 
replace edba_m=edba_m/d7m10 if decena==5 
replace edba_m=edba_m/d7m10 if decena==6 
replace edba_m=edba_m/d7m10 if decena==7 
replace edba_m=edba_m/d7m11 if decena==8 
replace edba_m=edba_m/d7m11 if decena==9 


*Modificaci�n Mayra Saenz - Abril 2017: Se desagrega el gasto unicamente en educaci�n (se excluye recreaci�n)

*Gasto monetario s�lo educaci�n
gen edu_gtosm=gasm if (clave>="E001" & clave<="E012") | (clave>="E013" & clave<="E017")  | (clave =="H029" ) | (clave =="H030") 

replace edu_gtosm=edu_gtosm/d7m08 if decena==1 
replace edu_gtosm=edu_gtosm/d7m09 if decena==2 
replace edu_gtosm=edu_gtosm/d7m09 if decena==3 
replace edu_gtosm=edu_gtosm/d7m09 if decena==4 
replace edu_gtosm=edu_gtosm/d7m10 if decena==5 
replace edu_gtosm=edu_gtosm/d7m10 if decena==6 
replace edu_gtosm=edu_gtosm/d7m10 if decena==7 
replace edu_gtosm=edu_gtosm/d7m11 if decena==8 
replace edu_gtosm=edu_gtosm/d7m11 if decena==9 



*Gasto en Cuidado Personal deflactado 

gen cuip_m=gasm if (clave>="D001" & clave<="D022") | (clave=="H064")  

replace cuip_m=cuip_m/d23m08 if decena==1 
replace cuip_m=cuip_m/d23m09 if decena==2 
replace cuip_m=cuip_m/d23m09 if decena==3 
replace cuip_m=cuip_m/d23m09 if decena==4 
replace cuip_m=cuip_m/d23m10 if decena==5 
replace cuip_m=cuip_m/d23m10 if decena==6 
replace cuip_m=cuip_m/d23m10 if decena==7 
replace cuip_m=cuip_m/d23m11 if decena==8 
replace cuip_m=cuip_m/d23m11 if decena==9 

*Gasto en Accesorios Personales deflactado 

gen accp_m=gasm if (clave>="H056" & clave<="H063") | (clave=="H065") 

replace accp_m=accp_m/d23t06 if decena==1 
replace accp_m=accp_m/d23t07 if decena==2 
replace accp_m=accp_m/d23t07 if decena==3 
replace accp_m=accp_m/d23t07 if decena==4 
replace accp_m=accp_m/d23t08 if decena==5 
replace accp_m=accp_m/d23t08 if decena==6 
replace accp_m=accp_m/d23t08 if decena==7 
replace accp_m=accp_m/d23t09 if decena==8 
replace accp_m=accp_m/d23t09 if decena==9 

*Gasto en Otros Gastos y Transferencias deflactado 

gen otr_m=gasm if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") 

replace otr_m=otr_m/dINPCs03 if decena==1 
replace otr_m=otr_m/dINPCs04 if decena==2 
replace otr_m=otr_m/dINPCs04 if decena==3 
replace otr_m=otr_m/dINPCs04 if decena==4 
replace otr_m=otr_m/dINPCs05 if decena==5 
replace otr_m=otr_m/dINPCs05 if decena==6 
replace otr_m=otr_m/dINPCs05 if decena==7 
replace otr_m=otr_m/dINPCs06 if decena==8 
replace otr_m=otr_m/dINPCs06 if decena==9 

*Gasto en Regalos Otorgados deflactado 

gen reda_m=gasm if (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") | (clave=="N013") 

replace reda_m=reda_m/dINPCs03 if decena==1 
replace reda_m=reda_m/dINPCs04 if decena==2 
replace reda_m=reda_m/dINPCs04 if decena==3 
replace reda_m=reda_m/dINPCs04 if decena==4 
replace reda_m=reda_m/dINPCs05 if decena==5 
replace reda_m=reda_m/dINPCs05 if decena==6 
replace reda_m=reda_m/dINPCs05 if decena==7 
replace reda_m=reda_m/dINPCs06 if decena==8 
replace reda_m=reda_m/dINPCs06 if decena==9 

collapse (sum) *_m edu_gtosm, by(folio) 
egen gmontot=rsum(*_m) 
sort folio 
rename edu_gtosm edu_gtosmh

saveold "$ruta\gastomonetario_def94.dta", replace 

*********************************************************

/*Parte III

Creacion del Gasto No Monetario deflactado a pesos de 
agosto del 1994.

Los deflactores se crean previamente a partir del INPC 
desagregado, segun archivo de excel anexo.

En esta parte se crean las bases:
"$ruta\gastonomonetario_def94.dta"
a nivel de clave de gasto, y
"$ruta\auto_def94.dta"
"$ruta\esp_def94.dta"
"$ruta\reg_def94.dta"
a nivel de folio.*/

********************************************************* 

*No Monetario 

use "$ruta\nomon.dta", clear

/*En el caso de la informacion no meonetario, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual esta dada dentro
del folio de identificacion del hogar en las posiciones septima y octava. 
En primer lugar se obtiene una variable que identifique la decena de
levantamiento */

gen decena=real(substr(folio,7,1)) 
tab decena 

/*Asi, se puede observar que se levanto informacion en 9 decenas,
correspondientes a:

Decena                           Periodo de Levantamiento

1	 |	del 22 al 28 de Septiembre
2	 |	del 02 al 08 de Octubre
3	 |	del 12 al 18 de octubre
4	 |	del 22 al 28 de octubre
5	 |	del 01 al 07 de noviembre
6	 |	del 11 al 17 de noviembre
7	 |	del 21 al 27 de noviembre
8	 |	del 01 al 07 de diciembre
9	 |	del 11 al 17 de diciembre

Asi, para realizar el analisis, y conforme a lo establecido en la
informacion de la ENIGH1994, se tomara como periodos de referencia


              Periodo de Referencia
Decena|	/Semanal     /Mensual     /Trimestral           /Semestral
	
1     |	/Septiembre	     /Agosto       /Junio a Agosto         /Marzo a Agosto
2     |	/Octubre  /Septiembre      /Julio a Septiembre       /Abril a Septiembre
3     |	/Octubre  /Septiembre      /Julio a Septiembre       /Abril a Septiembre
4     |	/Octubre  /Septiembre      /Julio a Septiembre       /Abril a Septiembre
5     |	/Noviembre     /Octubre  /Agosto a Octubre   /Mayo a Octubre
6     |	/Noviembre     /Octubre  /Agosto a Octubre   /Mayo a Octubre
7     |	/Noviembre     /Octubre  /Agosto a Octubre   /Mayo a Octubre
8     |	/Diciembre     /Noviembre     /Septiembre a Noviembre     /Junio a Noviembre
9     |	/Diciembre   /Noviembre     /Septiembre a Noviembre    /Junio a Noviembre

Asi, se utilizaran los deflactores mensuales para el caso de las referencias semanales y mensuales
y los promedios de los meses de referencia en las referencias Trimestrales y Semestrales 

*Los rubros a considerar, segun la metodologia del CTMP, seran:

Rubro                                  /Periodicidad     /Nombre del deflactor
alimentos y Bebidas No Alcoholicas     /Semanal          /d11wmes
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

A continuacion se definen escalares que contienen cada uno de estos deflactores, a partir del
archivo de excel anexo (no se requieren deflactores mensuales antes de agosto, por lo que no se
incorporan, si bien si fueron calculados) */

*Rubro 1.1 semanal 			
			
scalar d11w09	= 	1.0066591	 
scalar d11w10	= 	1.0120936	 
scalar d11w11	= 	1.0189161	 
scalar d11w12	= 	1.030306	 
			
*Rubro 1.2 semanal 			
			
scalar d12w09	= 	1.0283309	 
scalar d12w10	= 	1.0381586	 
scalar d12w11	= 	1.0439539	 
scalar d12w12	= 	1.0476356	 
			
*Rubro 2 trimestral 			
			
scalar d2t06	= 	0.995164	 
scalar d2t07	= 	0.9972957	 
scalar d2t08	= 	1.0028385	 
scalar d2t09	= 	1.0066064	 
			
*Rubro 3 mensual 			
			
scalar d3m08	= 	1.0000000	 
scalar d3m09	= 	1.0047596	 
scalar d3m10	= 	1.0103664	 
scalar d3m11	= 	1.0099404	 
			
*Rubro 4.2 mensual 			
			
scalar d42m08	= 	1.0000000	 
scalar d42m09	= 	1.0041214	 
scalar d42m10	= 	1.0071464	 
scalar d42m11	= 	1.0149518	 
			
*Rubro 4.2 trimestral 			
			
scalar d42t06	= 	0.9925574	 
scalar d42t07	= 	0.9982169	 
scalar d42t08	= 	1.0037559	 
scalar d42t09	= 	1.0087399	 
			
*Rubro 4.1 semestral 			
			
scalar d41s03	= 	0.9772681	 
scalar d41s04	= 	0.9853673	 
scalar d41s05	= 	0.9911522	 
scalar d41s06	= 	0.9965657	 
			
*Rubro 5.1 trimestral 			
			
scalar d51t06	= 	0.9869536	 
scalar d51t07	= 	1.000522	 
scalar d51t08	= 	1.0152349	 
scalar d51t09	= 	1.028321	 
			
*Rubro 6.1.1 semanal 			
			
scalar d611w09	= 	0.9999127	 
scalar d611w10	= 	1.0019927	 
scalar d611w11	= 	1.0054119	 
scalar d611w12	= 	1.0128794	 
			
*Rubro 6 mensual 			
			
scalar d6m08	= 	1.0000000	 
scalar d6m09	= 	1.0030515	 
scalar d6m10	= 	1.0093798	 
scalar d6m11	= 	1.0170424	 
			
*Rubro 6 semestral 			
			
scalar d6s03	= 	0.9824005	 
scalar d6s04	= 	0.9887934	 
scalar d6s05	= 	0.9949719	 
scalar d6s06 	= 	1.0015035	 
			
*Rubro 7 mensual 			
			
scalar d7m08	= 	1.0000000	 
scalar d7m09	= 	1.0622282	 
scalar d7m10	= 	1.0769648	 
scalar d7m11	= 	1.0926897	 
			
*Rubro 2.3 mensual 			
			
scalar d23m08	= 	1.0000000	 
scalar d23m09	= 	1.0084757	 
scalar d23m10	= 	1.0178758	 
scalar d23m11	= 	1.0226111	 
			
*Rubro 2.3 trimestral 			
			
scalar d23t06	= 	0.9941166	 
scalar d23t07	= 	1.0004248	 
scalar d23t08	= 	1.0087838	 
scalar d23t09	= 	1.0163208	 
			
*INPC semestral 			
			
scalar dINPCs03	= 	0.9883596	 
scalar dINPCs04	= 	0.9934603	 
scalar dINPCs05	= 	0.998645	 
scalar dINPCs06	= 	1.0039415	 

/*Una vez definidos los deflactores, se procede a limpiar la base de gasto a fin de incorporar solo
los rubros de gasto solicitados, y agregarlos en los rubros mas generales definidos por el CTMP */

drop if (clave=="G002") 
drop if (clave>="K030" & clave<="K033") 
drop if (clave>="Q001" & clave<="Q015") 
drop if (clave=="T905") 

gen double gasnomon=gas_tri/3 
destring tipo_gas, replace 
drop if (tipo_gas==4) 
gen auto=1 if tipo_gas==1 
gen esp=1 if tipo_gas==2 
gen reg=1 if tipo_gas==3 
gen alq=1 if tipo_gas==0 

*Gasto en alimentos deflactado 

gen ali_nm=gasnomon if (clave>="A001" & clave<="A193") | (clave>="A204" & clave<="A207") 

replace ali_nm=ali_nm/d11w09 if decena==1 
replace ali_nm=ali_nm/d11w10 if decena==2 
replace ali_nm=ali_nm/d11w10 if decena==3 
replace ali_nm=ali_nm/d11w10 if decena==4 
replace ali_nm=ali_nm/d11w11 if decena==5 
replace ali_nm=ali_nm/d11w11 if decena==6 
replace ali_nm=ali_nm/d11w11 if decena==7 
replace ali_nm=ali_nm/d11w12 if decena==8 
replace ali_nm=ali_nm/d11w12 if decena==9 

*Gasto en Alcohol y Tabaco deflactado 

gen alta_nm=gasnomon if (clave>="A194" & clave<="A203") | (clave>="A208" & clave<="A210") 

replace alta_nm=alta_nm/d12w09 if decena==1 
replace alta_nm=alta_nm/d12w10 if decena==2 
replace alta_nm=alta_nm/d12w10 if decena==3 
replace alta_nm=alta_nm/d12w10 if decena==4 
replace alta_nm=alta_nm/d12w11 if decena==5 
replace alta_nm=alta_nm/d12w11 if decena==6 
replace alta_nm=alta_nm/d12w11 if decena==7 
replace alta_nm=alta_nm/d12w12 if decena==8 
replace alta_nm=alta_nm/d12w12 if decena==9 

*Gasto en Vestido y Calzado deflactado 

gen veca_nm=gasnomon if (clave>="H001" & clave<="H028") | (clave>="H031" & clave<="H055") 

replace veca_nm=veca_nm/d2t06 if decena==1 
replace veca_nm=veca_nm/d2t07 if decena==2 
replace veca_nm=veca_nm/d2t07 if decena==3 
replace veca_nm=veca_nm/d2t07 if decena==4 
replace veca_nm=veca_nm/d2t08 if decena==5 
replace veca_nm=veca_nm/d2t08 if decena==6 
replace veca_nm=veca_nm/d2t08 if decena==7 
replace veca_nm=veca_nm/d2t09 if decena==8 
replace veca_nm=veca_nm/d2t09 if decena==9 

*Gasto en Vivienda deflactado 

gen viv_nm=gasnomon if (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G014") | (clave>="G016" & clave<="G033") 

replace viv_nm=viv_nm/d3m08 if decena==1 
replace viv_nm=viv_nm/d3m09 if decena==2 
replace viv_nm=viv_nm/d3m09 if decena==3 
replace viv_nm=viv_nm/d3m09 if decena==4 
replace viv_nm=viv_nm/d3m10 if decena==5 
replace viv_nm=viv_nm/d3m10 if decena==6 
replace viv_nm=viv_nm/d3m10 if decena==7 
replace viv_nm=viv_nm/d3m11 if decena==8 
replace viv_nm=viv_nm/d3m11 if decena==9 

*Gasto en Articulos de Limpieza deflactado 

gen lim_nm=gasnomon if (clave>="C001" & clave<="C024") 

replace lim_nm=lim_nm/d42m08 if decena==1 
replace lim_nm=lim_nm/d42m09 if decena==2 
replace lim_nm=lim_nm/d42m09 if decena==3 
replace lim_nm=lim_nm/d42m09 if decena==4 
replace lim_nm=lim_nm/d42m10 if decena==5 
replace lim_nm=lim_nm/d42m10 if decena==6 
replace lim_nm=lim_nm/d42m10 if decena==7 
replace lim_nm=lim_nm/d42m11 if decena==8 
replace lim_nm=lim_nm/d42m11 if decena==9 

*Gasto en Cristaleria y Blancos deflactado 

gen cris_nm=gasnomon if (clave>="I001" & clave<="I026") 

replace cris_nm=cris_nm/d42t06 if decena==1 
replace cris_nm=cris_nm/d42t07 if decena==2 
replace cris_nm=cris_nm/d42t07 if decena==3 
replace cris_nm=cris_nm/d42t07 if decena==4 
replace cris_nm=cris_nm/d42t08 if decena==5 
replace cris_nm=cris_nm/d42t08 if decena==6 
replace cris_nm=cris_nm/d42t08 if decena==7 
replace cris_nm=cris_nm/d42t09 if decena==8 
replace cris_nm=cris_nm/d42t09 if decena==9 

*Gasto en Enseres deflactado 

gen ens_nm=gasnomon if (clave>="K001" & clave<="K029") 

replace ens_nm=ens_nm/d41s03 if decena==1 
replace ens_nm=ens_nm/d41s04 if decena==2 
replace ens_nm=ens_nm/d41s04 if decena==3 
replace ens_nm=ens_nm/d41s04 if decena==4 
replace ens_nm=ens_nm/d41s05 if decena==5 
replace ens_nm=ens_nm/d41s05 if decena==6 
replace ens_nm=ens_nm/d41s05 if decena==7 
replace ens_nm=ens_nm/d41s06 if decena==8 
replace ens_nm=ens_nm/d41s06 if decena==9 

*Gasto en Salud deflactado 

gen sal_nm=gasnomon if (clave>="J001" & clave<="J043") 

replace sal_nm=sal_nm/d51t06 if decena==1 
replace sal_nm=sal_nm/d51t07 if decena==2 
replace sal_nm=sal_nm/d51t07 if decena==3 
replace sal_nm=sal_nm/d51t07 if decena==4 
replace sal_nm=sal_nm/d51t08 if decena==5 
replace sal_nm=sal_nm/d51t08 if decena==6 
replace sal_nm=sal_nm/d51t08 if decena==7 
replace sal_nm=sal_nm/d51t09 if decena==8 
replace sal_nm=sal_nm/d51t09 if decena==9 

*Gasto en Transporte Publico deflactado 

gen tpub_nm=gasnomon if (clave>="B001" & clave<="B007") 

replace tpub_nm=tpub_nm/d611w09 if decena==1 
replace tpub_nm=tpub_nm/d611w10 if decena==2 
replace tpub_nm=tpub_nm/d611w10 if decena==3 
replace tpub_nm=tpub_nm/d611w10 if decena==4 
replace tpub_nm=tpub_nm/d611w11 if decena==5 
replace tpub_nm=tpub_nm/d611w11 if decena==6 
replace tpub_nm=tpub_nm/d611w11 if decena==7 
replace tpub_nm=tpub_nm/d611w12 if decena==8 
replace tpub_nm=tpub_nm/d611w12 if decena==9 

*Gasto en Transporte Foraneo deflactado 

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M018") | (clave>="F006" & clave<="F010") 

replace tfor_nm=tfor_nm/d6s03 if decena==1 
replace tfor_nm=tfor_nm/d6s04 if decena==2 
replace tfor_nm=tfor_nm/d6s04 if decena==3 
replace tfor_nm=tfor_nm/d6s04 if decena==4 
replace tfor_nm=tfor_nm/d6s05 if decena==5 
replace tfor_nm=tfor_nm/d6s05 if decena==6 
replace tfor_nm=tfor_nm/d6s05 if decena==7 
replace tfor_nm=tfor_nm/d6s06 if decena==8 
replace tfor_nm=tfor_nm/d6s06 if decena==9 

*Gasto en Comunicaciones deflactado 

gen com_nm=gasnomon if (clave>="F001" & clave<="F005") 

replace com_nm=com_nm/d6m08 if decena==1 
replace com_nm=com_nm/d6m09 if decena==2 
replace com_nm=com_nm/d6m09 if decena==3 
replace com_nm=com_nm/d6m09 if decena==4 
replace com_nm=com_nm/d6m10 if decena==5 
replace com_nm=com_nm/d6m10 if decena==6 
replace com_nm=com_nm/d6m10 if decena==7 
replace com_nm=com_nm/d6m11 if decena==8 
replace com_nm=com_nm/d6m11 if decena==9 

*Gasto en Educacion y Recreacion deflactado 

gen edre_nm=gasnomon if (clave>="E001" & clave<="E031") | (clave>="H029" & clave<="H030") | (clave>="L001" & clave<="L027") | (clave>="N003" & clave<="N005") 

replace edre_nm=edre_nm/d7m08 if decena==1 
replace edre_nm=edre_nm/d7m09 if decena==2 
replace edre_nm=edre_nm/d7m09 if decena==3 
replace edre_nm=edre_nm/d7m09 if decena==4 
replace edre_nm=edre_nm/d7m10 if decena==5 
replace edre_nm=edre_nm/d7m10 if decena==6 
replace edre_nm=edre_nm/d7m10 if decena==7 
replace edre_nm=edre_nm/d7m11 if decena==8 
replace edre_nm=edre_nm/d7m11 if decena==9 

*Gasto en Educacion Basica deflactado 

gen edba_nm=gasnomon if (clave>="E003" & clave<="E004") | (clave == "E014") | (clave>="H029" & clave<="H030") 

replace edba_nm=edba_nm/d7m08 if decena==1 
replace edba_nm=edba_nm/d7m09 if decena==2 
replace edba_nm=edba_nm/d7m09 if decena==3 
replace edba_nm=edba_nm/d7m09 if decena==4 
replace edba_nm=edba_nm/d7m10 if decena==5 
replace edba_nm=edba_nm/d7m10 if decena==6 
replace edba_nm=edba_nm/d7m10 if decena==7 
replace edba_nm=edba_nm/d7m11 if decena==8 
replace edba_nm=edba_nm/d7m11 if decena==9 

*Gasto en Cuidado Personal deflactado 

gen cuip_nm=gasnomon if (clave>="D001" & clave<="D022") | (clave=="H064") 

replace cuip_nm=cuip_nm/d23m08 if decena==1 
replace cuip_nm=cuip_nm/d23m09 if decena==2 
replace cuip_nm=cuip_nm/d23m09 if decena==3 
replace cuip_nm=cuip_nm/d23m09 if decena==4 
replace cuip_nm=cuip_nm/d23m10 if decena==5 
replace cuip_nm=cuip_nm/d23m10 if decena==6 
replace cuip_nm=cuip_nm/d23m10 if decena==7 
replace cuip_nm=cuip_nm/d23m11 if decena==8 
replace cuip_nm=cuip_nm/d23m11 if decena==9 

*Gasto en Accesorios Personales deflactado 

gen accp_nm=gasnomon if (clave>="H056" & clave<="H063") | (clave=="H065") 

replace accp_nm=accp_nm/d23t06 if decena==1 
replace accp_nm=accp_nm/d23t07 if decena==2 
replace accp_nm=accp_nm/d23t07 if decena==3 
replace accp_nm=accp_nm/d23t07 if decena==4 
replace accp_nm=accp_nm/d23t08 if decena==5 
replace accp_nm=accp_nm/d23t08 if decena==6 
replace accp_nm=accp_nm/d23t08 if decena==7 
replace accp_nm=accp_nm/d23t09 if decena==8 
replace accp_nm=accp_nm/d23t09 if decena==9 

*Gasto en Otros Gastos y Transferencias deflactado 

gen otr_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") 

replace otr_nm=otr_nm/dINPCs03 if decena==1 
replace otr_nm=otr_nm/dINPCs04 if decena==2 
replace otr_nm=otr_nm/dINPCs04 if decena==3 
replace otr_nm=otr_nm/dINPCs04 if decena==4 
replace otr_nm=otr_nm/dINPCs05 if decena==5 
replace otr_nm=otr_nm/dINPCs05 if decena==6 
replace otr_nm=otr_nm/dINPCs05 if decena==7 
replace otr_nm=otr_nm/dINPCs06 if decena==8 
replace otr_nm=otr_nm/dINPCs06 if decena==9 

*Gasto en Regalos Otorgados deflactado 

gen reda_nm=gasnomon if (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905") | (clave=="N013") 

replace reda_nm=reda_nm/dINPCs03 if decena==1 
replace reda_nm=reda_nm/dINPCs04 if decena==2 
replace reda_nm=reda_nm/dINPCs04 if decena==3 
replace reda_nm=reda_nm/dINPCs04 if decena==4 
replace reda_nm=reda_nm/dINPCs05 if decena==5 
replace reda_nm=reda_nm/dINPCs05 if decena==6 
replace reda_nm=reda_nm/dINPCs05 if decena==7 
replace reda_nm=reda_nm/dINPCs06 if decena==8 
replace reda_nm=reda_nm/dINPCs06 if decena==9 

*Estimacion del Alquiler 

gen vivprob=gasnomon if (clave=="G001") 
gen vivrecib=gasnomon if (clave=="G007") 
gen vivprest=gasnomon if (clave=="G010") 
gen vivotra=gasnomon if (clave=="G015") 
gen double est_alq=alq*gasnomon 

saveold "$ruta\gastonomonetario_def94.dta", replace 

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

saveold "$ruta\auto_def94.dta", replace 

use "$ruta\gastonomonetario_def94.dta", clear 


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

saveold "$ruta\esp_def94.dta", replace 

use "$ruta\gastonomonetario_def94.dta", clear 


*Construccion de base de regalos a partir de la base no monetaria  

keep if (reg==1 | alq==1) 

collapse (sum) *_nm est_alq vivprob vivrecib vivprest vivotra, by (folio) 

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

saveold "$ruta\reg_def94.dta", replace 
*********************************************************

/*Parte IV

C�lculo de la incidencia 1994

Con el prop�sito de obtener cifras representativas a nivel 
nacional, rural y urbano, se obtiene una base con las 
variables de tama�o del hogar, estrato y factor de 
expansi�n, y a �sta se le unen las bases anteriormente 
generadas. Se estiman los ingresos corriente total y 
corriente neto, se genera el factor de expansi�n para 
personas, y se calcula la incidencia de la pobreza de 
acuerdo con el c�lculo oficial de la Sedesol.

En esta parte se genera la base 
"$ruta\basefinal_94.dta" 
a nivel de folio.*/

********************************************************* 

use "$ruta\hogares.dta", clear 
*keep folio tam_hog 
sort folio 
saveold "$ruta\hogares94.dta", replace 

use "$ruta\concen.dta", clear 
keep folio hog estrato educacion
rename hog factor 
sort folio 

merge folio using "$ruta\hogares94.dta" 
tab _merge 
drop _merge 
sort folio 

*Modificado Mayra S�enz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*merge folio using "$ruta\ingreso_deflactado94.dta" 
tab _merge 
drop _merge 
sort folio */

merge folio using "$ruta\gastomonetario_def94.dta" 
tab _merge 
drop _merge 
sort folio 

merge folio using "$ruta\auto_def94.dta" 
tab _merge 
drop _merge 
sort folio 

merge folio using "$ruta\esp_def94.dta" 
tab _merge 
drop _merge 
sort folio 

merge folio using "$ruta\reg_def94.dta" 
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

rename folio folio1              //Modificado Mayra S�enz - Julio 2015
gen folio = substr(folio1,4,100) //Modificado Mayra S�enz - Julio 2015

saveold "$ruta\gtos_autoc94.dta", replace //Mayra S�enz Julio 2015

*_________________________________________________________________________________________________________*
* Modificaci�n Mayra S�enz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*

use "$ruta\person94.dta", clear
rename numren num_ren             //Modificado Mayra S�enz - Julio 2015
sort folio num_ren, stable

merge 1:1 folio num_ren using "$ruta\ingreso_deflactado94_per.dta"
rename _merge _merge_ing
sort folio num_ren, stable

merge m:1 folio using "$ruta\gtos_autoc94.dta"

*Modificaci�n Mayra S�enz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)


egen double ict=rsum(ing_monh nomon) if paren=="1" | paren=="2" //Mayra S�enz Agosto 2015 - Aumento esta condici�n porque esta base est� a nivel de personas
egen double gct=rsum(gasmon nomon)   if paren=="1" | paren=="2" //Mayra S�enz Agosto 2015 - Aumento esta condici�n porque esta base est� a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if paren=="1" | paren=="2" //Mayra S�enz Agosto 2015 - Aumento esta condici�n porque esta base est� a nivel de personas
egen double gnt=rsum(gasmon nomon redan reg_espn) if paren=="1" | paren=="2" //Mayra S�enz Agosto 2015 - Aumento esta condici�n porque esta base est� a nivel de personas

label var  ict  "Ingreso corriente total del hogar"
label var  gct  "Gasto corriente total del hogar"
label var  intt "Ingreso neto total del hogar"
label var  gnt  "Gasto neto total del hogar"

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
use "$ruta\vivi94.dta",clear
sort folio
saveold "$ruta\MEX_1994m8_m9.dta",replace
use "$ruta\person94.dta", clear
sort folio
merge folio using "$ruta\MEX_1994m8_m9.dta"
drop _merge
sort folio numren
gen id=_n
saveold "$ruta\MEX_1994m8_m9.dta",replace
use "$ruta\ingres94.dta", clear
tab  ocupacion,gen(ocup) /*se genera para saber la numero de ocupacion*/
gen clave1=substr(clave,1,1)
gen clave2=real(substr(clave,2,3))
sort folio numren
by folio numren:egen ring1a_ml=sum(ing_mp) if clave2>0 & clave2<=5 & ocupa=="1" 
by folio numren:egen ring1b_ml=sum(ing_mp) if clave2==14 & ocupa=="1" 
replace ring1a_ml=0 if ring1a_ml==.
replace ring1b_ml=0 if ring1b_ml==.
by folio numren:gen ring1_ml=ring1a_ml+ring1b_ml
by folio numren:egen ing1_ml=max(ring1_ml)
 
by folio numren:egen ring2a_ml=sum(ing_mp) if clave2>0 & clave2<=5 & ocupa~="1" 
by folio numren:egen ring2b_ml=sum(ing_mp) if clave2==14 & ocupa~="1" 
replace ring2a_ml=0 if ring2a_ml==.
replace ring2b_ml=0 if ring2b_ml==.
by folio numren:gen ring2_ml=ring2a_ml+ring2b_ml
by folio numren:egen ing2_ml=max(ring2_ml)

by folio numren:egen ring1a_npm=sum(ing_mp) if clave2>5 & clave2<=13 & ocupa=="1" 
by folio numren:egen ring1b_npm=sum(ing_mp) if clave2==15 & ocupa=="1" 
replace ring1a_npm=0 if ring1a_npm==.
replace ring1b_npm=0 if ring1b_npm==.
by folio numren:gen ring1_npm=ring1a_npm+ring1b_npm
by folio numren:egen ing1_npm=max(ring1_npm)
 
by folio numren:egen double ring2a_npm=sum(ing_mp) if clave2>5 & clave2<=13 & ocupa~="1" 
by folio numren:egen double ring2b_npm=sum(ing_mp) if clave2==15 & ocupa~="1" 
replace ring2a_npm=0 if ring2a_npm==.
replace ring2b_npm=0 if ring2b_npm==.
by folio numren:gen ring2_npm=ring2a_npm+ring2b_npm
by folio numren:egen ing2_npm=max(ring2_npm)

/*renta*/
by folio numren:egen ring_rpm=sum(ing_mp) if clave2>=16 & clave2<=22 
replace ring_rpm=0 if ring_rpm==.
by folio numren:egen ing_rpm=max(ring_rpm)
/*transferencias*/
by folio numren:egen ring_tm=sum(ing_mp) if  clave2>=23 & clave2<=27 
replace ring_tm=0 if ring_tm==.
by folio numren:egen ing_tm=max(ring_tm)

*transferencias por jublaciones y/o pensiones*
by folio numren:egen ring_jub=sum(ing_mp) if  clave2==23
replace ring_jub=0 if ring_jub==.
by folio numren:egen ing_jub=max(ring_jub)

/*regalos*/
by folio numren:egen ring_otm=sum(ing_mp) if clave2>=29 & clave2<=30 
replace ring_otm=0 if ring_otm==.
by folio numren:egen ing_otm=max(ring_otm)
/*remesas*/
by folio numren:egen ring_otrosp=sum(ing_mp) if clave2==28  
replace ring_otrosp=0 if ring_otrosp==.
by folio numren:egen ing_otrosp=max(ring_otrosp)
/*financieras*/
by folio numren:egen ring_otm1=sum(ing_mp) if clave2>=31 &  clave2<=42
replace ring_otm1=0 if ring_otm1==.
by folio numren:egen ing_otm1=max(ring_otm1)
by folio numren:gen cont=(_n) 
drop if cont>1

drop  ring* ocupacion clave ing_mp ing_tri mes2 mes1 mes3 mes4 mes5 cont
sort folio numren
merge folio numren using "$ruta\MEX_1994m8_m9.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge
sort folio numren
saveold "$ruta\MEX_1994m8_m9.dta",replace
use "$ruta\nomon94.dta", clear
sort folio numren
by folio numren:egen ing_nml=sum(estimado) 

by folio numren:gen cont=(_n) /*elimino repetidos*/
drop if cont>1
keep folio numren ing_nml
sort folio numren
merge folio numren using "$ruta\MEX_1994m8_m9.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge 


saveold "`base_out'", replace


log close


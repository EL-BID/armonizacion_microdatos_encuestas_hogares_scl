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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENIGH\2005\m8_m11\data_orig"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2005"
local ronda m8_m11

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (Nueva construcción)
Round: Agosto-Noviembre
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/




*Mayra Sáenz - Agosto 2015: Se realiza el merge con base en la sintaxis de CONEVAL, 
*pero con algunas modificaciones, y generando nuevas variables.

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

Base de ingresos: ingreso05.dta
Base de gasto monetario: gasto05.dta
Base de gasto no monetario: nomonetario05.dta
Base de concentrado: concentrado05.dta

En este programa se utilizan tres tipos de archivos, los cuales 
están ubicados en las siguientes carpetas:

1) Bases originales: "C:\pobreza ingresos\2005\ENIGH"
2) Bitácoras: "C:\pobreza ingresos\2005\Log"
3) Bases generadas: "C:\pobreza ingresos\2005\Resultados"


Para cambiar estas ubicaciones, se modifican los siguientes
globals 

gl data="C:\pobreza ingresos\2005\ENIGH"
gl log="C:\pobreza ingresos\2005\Log"
gl bases="C:\pobreza ingresos\2005\Resultados"

log using "$log\Pobreza por ingresos 2005.txt", text replace

*********************************************************
*
*	PROGRAMA PARA LA MEDICIÓN DE LA POBREZA 2005
*
*********************************************************

CONEVAL Última modificación: 24 de julio del 2013

*********************************************************

*Parte I

*Creación del ingreso monetario deflactado a pesos de 
agosto del 2005.

Los deflactores se crean previamente a partir del INPC 
general (ver Nota Técnica).

En esta parte se crea la base:
	"$ruta\ingreso_deflactado05.dta"
la cual se encuentra a nivel de folio.

********************************************************
*/
*Ingresos

use "$ruta\ingresos.dta"

/*La variable ‘meses’ define los meses a los que corresponden
cada uno de los ingresos de la persona en los seis meses 
anteriores al levantamiento de la información. Esta variable
toma cuatro valores:

070605040302
080706050403
090807060504
100908070605

Así, se sabe que la encuesta fue levantada entre los meses 
de agosto y noviembre, y por lo tanto, al preguntarse por 
los ingresos de los seis meses anteriores, se recolectó 
información correspondiente a febrero-julio, marzo-agosto,
abril-septiembre, y mayo-octubre*/

*Definición de los deflactores 2005

scalar ene05	=	0.9870820 	
scalar feb05	=	0.9903707 	
scalar mar05	=	0.9948346 	
scalar abr05	=	0.9983776 	
scalar may05	=	0.9958694 	
scalar jun05	=	0.9949135 	
scalar jul05	=	0.9988073 	
scalar ago05	=	1.0000000 	
scalar sep05	=	1.0040078 	
scalar oct05	=	1.0064722 	
scalar nov05	=	1.0137160 	
scalar dic05	=	1.0199426 	

*La estrategia para deflactar será dividir cada columna 
*de ingreso por el deflactor correspondiente a su mes

replace ing_6=ing_6/feb05 if meses=="070605040302"
replace ing_5=ing_5/mar05 if meses=="070605040302"
replace ing_4=ing_4/abr05 if meses=="070605040302"
replace ing_3=ing_3/may05 if meses=="070605040302"
replace ing_2=ing_2/jun05 if meses=="070605040302"
replace ing_1=ing_1/jul05 if meses=="070605040302"

replace ing_6=ing_6/mar05 if meses=="080706050403"
replace ing_5=ing_5/abr05 if meses=="080706050403"
replace ing_4=ing_4/may05 if meses=="080706050403"
replace ing_3=ing_3/jun05 if meses=="080706050403"
replace ing_2=ing_2/jul05 if meses=="080706050403"
replace ing_1=ing_1/ago05 if meses=="080706050403"

replace ing_6=ing_6/abr05 if meses=="090807060504"
replace ing_5=ing_5/may05 if meses=="090807060504"
replace ing_4=ing_4/jun05 if meses=="090807060504"
replace ing_3=ing_3/jul05 if meses=="090807060504"
replace ing_2=ing_2/ago05 if meses=="090807060504"
replace ing_1=ing_1/sep05 if meses=="090807060504"

replace ing_6=ing_6/may05 if meses=="100908070605"
replace ing_5=ing_5/jun05 if meses=="100908070605"
replace ing_4=ing_4/jul05 if meses=="100908070605"
replace ing_3=ing_3/ago05 if meses=="100908070605"
replace ing_2=ing_2/sep05 if meses=="100908070605"
replace ing_1=ing_1/oct05 if meses=="100908070605"

*Mayra Sáenz Julio 2015- Mes de referencia

g mes_ref = substr(meses, 1, 2)
destring mes_ref, replace


/*Una vez realizada la deflactación, se procede a obtener el 
ingreso mensual promedio en los últimos seis meses, para 
cada persona y clave de ingreso*/

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)

*Para obtener el ingreso corriente monetario, se seleccionan 
*las claves de ingreso correspondientes

gen double ing_mon=ing_mens if clave>="P001" & clave<="P060"


**Modificación Mayra Sáenz - Julio 2015 : En la sintaxis original de CONEVAL sólo generan ing_mon, los otros ingresos se generan con base en sintaxis anteriores.

gen double ing_lab=ing_mens if clave>="P001" & clave<="P038"

**Modificación Mayra Sáenz - Julio 2015 : Son ingresos laborales de P001-P014, se desagrega por actividad principal y secundaria
gen     ocuprisec = 1 if cod_trab ==1
replace ocuprisec = 2 if cod_trab !=1

forval j = 1/2 {
gen double ing_trab`j'=ing_mens if (clave>="P001" & clave<="P009") & ocuprisec == `j'
gen double ing_negp`j'=ing_mens if (clave>="P010" & clave<="P038") & ocuprisec == `j'
}

gen double ing_trab=ing_mens if clave>="P001" & clave<="P009"
gen double ing_negp=ing_mens if clave>="P010" & clave<="P038"
gen double ing_rent=ing_mens if clave>="P039" & clave<="P047"
gen double ing_tran=ing_mens if clave>="P048" & clave<="P060"

*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens  if  (clave=="P048" )                                     //Jubilaciones y pensiones originados dentro del país P048, No se incluyen las provenientes de otros países P049
g double trat_pr  = ing_mens  if  ((clave>="P050" & clave<="P052") | clave=="P058")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por accidentes de trabajo, Indemnizaciones por despido y retiro voluntario ,Ingresos provenientes de otros paises
g double trat_pu  = ing_mens  if  (clave>="P059" & clave<="P060")                      //Desde 1996 aparece PROCAMPO, en 2002 se incluye Beneficio del PROGRESA u Oportunidades.
g double dona_pu  = ing_mens  if  (clave=="P054" | clave=="P056")                     //Becas y donativos provenientes del gobierno                      
g double dona_pr  = ing_mens  if  (clave=="P053" | clave=="P055" | clave=="P057")      //Becas y donativos provenientes de organizaciones no gubernamentales; Regalos o donativos en dinero provenientes de otros hogares
g double otros    = ing_mens  if  ((clave>="P061" & clave<="P076"))                    //Otros ingresos corrientes no considerados en los anteriores (especifique); Retiro de inversiones, ahorros, tandas, cajas de ahorro, etc.;  Pagos recibidos de préstamos que usted hizo a personas no miembros del hogar;  Préstamos recibidos de personas no miembros del hogar o instituciones; Venta de monedas, metales preciosos, joyas y obras de arte; Venta de acciones; Venta de bonos; Venta de cédulas; Venta de marcas, patentes y derechos de autor; Herencias, dotes y legados; Loterías y juegos de azar;  Venta de casas, terrenos, condominios, etc. que están dentro del país propiedad de algún miembro del hogar;  Venta de casas, terrenos, condominios, etc.que están fuera del país propiedad de algún miembro del hogar; Venta de maquinaria, equipos, animales de producción, vehículos, etc. utilizados en el negocio propiedad del hogar; Venta de vehículos, aparátos eléctricos de segunda mano, etc.;  Préstamos hipotecarios por bienes inmuebles: casa, terrenos, edificios y locales; Seguro de vida; Otras percepciones financieras de capital no consideradas en las anteriores (especifique).
g double remesas  = ing_mens  if  (clave=="P058")                                      //Ingresos provenientes de otros paises


*Modificación Mayra Sáenz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}


*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.


/**A continuación se estima el total de ingresos de cada 
*hogar, y se guarda la base en un archivo aparte

collapse (sum) ing_mon, by(folio)

label var folio "Identificador del hogar"
label var ing_mon "Ingreso corriente monetario del hogar"
*/

collapse (sum) ing_mens ing_mon ing_lab ing_trab* ing_negp* ing_rent ing_tran ypension trat_pr trat_pu dona_pu dona_pr otros remesas P* (mean) mes_ref, by(folio num_ren)

label var folio     "Identificador del hogar"
label var ing_mon   "Ingreso corriente monetario del individuo sin otros ingresos"  //Antes era ingreso del hogar.
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

saveold "$ruta\ingreso_deflactado05_per.dta", replace

*********************************************************

/*Parte II

Creación del gasto monetario deflactado a pesos de agosto 
de 2005.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crea la base:
	"$ruta\gastomonetario_def05.dta"
la cual se encuentra a nivel de folio.
*/
*********************************************************

*Gasto Monetario

use "$ruta\gastos.dta", clear

/*En el caso de la información de gasto, para deflactar se 
utiliza la decena de levantamiento de la encuesta, la cual 
se encuentra en la séptima posición del folio del hogar. En 
primer lugar se obtiene una variable que identifique la 
decena de levantamiento*/

gen decena=real(substr(folio,7,1))
tab decena

/*Al comparar con la información del catálogo, se observa que la 
información se levantó en nueve decenas, correspondientes a:

Decena	Periodo de levantamiento

0	|	del 10 al 16 de agosto
1	|	del 20 al 26 de agosto
2	|	del 30 de agosto al 05 de septiembre
3	|	del 09 al 15 de septiembre
4	|	del 19 al 25 de septiembre
5	|	del 29 de septiembre al 05 de octubre
6	|	del 09 al 15 de octubre
7	|	del 19 al 25 de octubre
8	|	del 29 de octubre al 04 de noviembre
9	|	del 08 al 14 de noviembre

Así, para realizar el análisis, y conforme a lo establecido 
en la información de la ENIGH2005, se tomarán como periodos 
de referencia:

Decena	Periodo de referencia
		/Semanal	/Mensual	/Trimestral	/Semestral

0	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
1	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
2	|	/Agosto	/Agosto	/Junio a agosto	/Marzo a agosto
3	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
4	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
5	|	/Septiembre	/Septiembre	/Julio a septiembre	/Abril a septiembre
6	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
7	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
8	|	/Octubre	/Octubre	/Agosto a octubre	/Mayo a octubre
9	|	/Noviembre	/Octubre	/Agosto a octubre	/Mayo a octubre

*Los rubros a considerar según la metodología del CTMP, serán:

Rubro	/Periodicidad	/Nombre del deflactor
Alimentos y bebidas no alcohólicas	/Semanal	/d11wmes
Bebidas alcohólicas y tabaco	/Semanal	/d12wmes
Vestido y calzado	/Trimestral	/d2tmesini
Vivienda y servicios de conservación	/Mensual	/d3mmes
Estimación del alquiler	/No se deflactará	/No se deflactará
Artículos y servicios de limpieza	/Mensual	/d42mmes
Cristalería y utensilios domésticos	/Trimestral	/d42tmesini
Enseres domésticos y muebles	/Semestral	/d41smesini
Cuidados de la salud	/Trimestral	/d51tmesini
Transporte público	/Semanal	/d611wmes
Transporte foráneo	/Semestral	/d6smesini
Comunicaciones	/Mensual	/d6mmes
Educación básica	/Mensual	/d7mmes
Cuidado personal	/Mensual	/d23mmes
Accesorios personales	/Trimestral	/d23tmesini
Otros gastos y transferencias	/Semestral	/dINPCsmesini
Regalos otorgados	/Semestral	/dINPCsmesini
*/
*Definición de los deflactores

*Rubro 1.1 semanal

scalar d11w07	=	1.0014137	
scalar d11w08	=	1.0000000	
scalar d11w09	=	1.0007401	
scalar d11w10	=	0.9955677	
scalar d11w11	=	0.9899628	

*Rubro 1.2 semanal

scalar d12w07	=	0.9973669	
scalar d12w08	=	1.0000000	
scalar d12w09	=	1.0010331	
scalar d12w10	=	1.0008685	
scalar d12w11	=	1.0059884	

*Rubro 2 trimestral

scalar d2t05	=	0.9977200	
scalar d2t06	=	0.9982503	
scalar d2t07	=	0.9994276	
scalar d2t08	=	1.0018143	

*Rubro 3 mensual

scalar d3m07	=	0.9973427	
scalar d3m08	=	1.0000000	
scalar d3m09	=	1.0031940	
scalar d3m10	=	1.0129081	
scalar d3m11	=	1.0406426	

*Rubro 4.2 mensual

scalar d42m07	=	1.0009064	
scalar d42m08	=	1.0000000	
scalar d42m09	=	1.0034521	
scalar d42m10	=	1.0054578	
scalar d42m11	=	1.0076949	

*Rubro 4.2 trimestral

scalar d42t05	=	0.9994600	
scalar d42t06	=	1.0009321	
scalar d42t07	=	1.0014528	
scalar d42t08	=	1.0029700	

*Rubro 4.1 semestral

scalar d41s02	=	1.0007906	
scalar d41s03	=	1.0015464	
scalar d41s04	=	1.0013127	
scalar d41s05	=	1.0009812	

*Rubro 5.1 trimestral

scalar d51t05	=	0.9935201	
scalar d51t06	=	0.9966056	
scalar d51t07	=	0.9993313	
scalar d51t08	=	1.0020372	

*Rubro 6.1.1 semanal

scalar d611w07	=	0.9988355	
scalar d611w08	=	1.0000000	
scalar d611w09	=	1.0034676	
scalar d611w10	=	1.0035533	
scalar d611w11	=	1.0057280	

*Rubro 6 mensual

scalar d6m07	=	0.9995307	
scalar d6m08	=	1.0000000	
scalar d6m09	=	1.0013174	
scalar d6m10	=	1.0044216	
scalar d6m11	=	1.0066325	

*Rubro 6 semestral

scalar d6s02	=	0.9930276	
scalar d6s03	=	0.9955919	
scalar d6s04	=	0.9977606	
scalar d6s05	=	0.9995969	

*Rubro 7 mensual

scalar d7m07	=	0.9977200	
scalar d7m08	=	1.0000000	
scalar d7m09	=	1.0191116	
scalar d7m10	=	1.0195559	
scalar d7m11	=	1.0209222	

*Rubro 2.3 mensual

scalar d23m07	=	0.9989895	
scalar d23m08	=	1.0000000	
scalar d23m09	=	1.0032622	
scalar d23m10	=	1.0067725	
scalar d23m11	=	1.0149013	

*Rubro 2.3 trimestral

scalar d23t05	=	0.9975741	
scalar d23t06	=	0.9991372	
scalar d23t07	=	1.0007505	
scalar d23t08	=	1.0033449	

*INPC semestral

scalar dINPCs02	=	0.9955288	
scalar dINPCs03	=	0.9971337	
scalar dINPCs04	=	0.9986626	
scalar dINPCs05	=	1.0000117	

*Una vez definidos los deflactores, se seleccionan los rubros de gasto de la Metodología del CTMP

gen double gasm=gas_tri/3

*Gasto en Alimentos deflactado

gen ali_m=gasm if (clave>="A001" & clave<="A218") | (clave>="A235" & clave<="A239") | (clave=="A243")

replace ali_m=ali_m/d11w08 if decena==0
replace ali_m=ali_m/d11w08 if decena==1
replace ali_m=ali_m/d11w08 if decena==2
replace ali_m=ali_m/d11w09 if decena==3
replace ali_m=ali_m/d11w09 if decena==4
replace ali_m=ali_m/d11w09 if decena==5
replace ali_m=ali_m/d11w10 if decena==6
replace ali_m=ali_m/d11w10 if decena==7
replace ali_m=ali_m/d11w10 if decena==8
replace ali_m=ali_m/d11w11 if decena==9

*Gasto en Alcohol y tabaco deflactado

gen alta_m=gasm if (clave>="A219" & clave<="A234") | (clave>="A240" & clave<="A242")

replace alta_m=alta_m/d12w08 if decena==0
replace alta_m=alta_m/d12w08 if decena==1
replace alta_m=alta_m/d12w08 if decena==2
replace alta_m=alta_m/d12w09 if decena==3
replace alta_m=alta_m/d12w09 if decena==4
replace alta_m=alta_m/d12w09 if decena==5
replace alta_m=alta_m/d12w10 if decena==6
replace alta_m=alta_m/d12w10 if decena==7
replace alta_m=alta_m/d12w10 if decena==8
replace alta_m=alta_m/d12w11 if decena==9

*Gasto en Vestido y calzado deflactado

gen veca_m=gasm if (clave>="H001" & clave<="H072") | (clave>="H075" & clave<="H108")

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

*Gasto en Vivienda y servicios de conservación deflactado

gen viv_m=gasm if (clave>="G002" & clave<="G010") | (clave>="G019" & clave<="G029")

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

*Gasto en Artículos de limpieza deflactado

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

*Gasto en Cristalería y blancos deflactado

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

*Gasto en Enseres domésticos y muebles deflactado

gen ens_m=gasm if (clave>="K001" & clave<="K036")

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

gen sal_m=gasm if (clave>="J001" & clave<="J072")

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

*Gasto en Transporte público deflactado

gen tpub_m=gasm if (clave>="B001" & clave<="B007")

replace tpub_m=tpub_m/d611w08 if decena==0
replace tpub_m=tpub_m/d611w08 if decena==1
replace tpub_m=tpub_m/d611w08 if decena==2
replace tpub_m=tpub_m/d611w09 if decena==3
replace tpub_m=tpub_m/d611w09 if decena==4
replace tpub_m=tpub_m/d611w09 if decena==5
replace tpub_m=tpub_m/d611w10 if decena==6
replace tpub_m=tpub_m/d611w10 if decena==7
replace tpub_m=tpub_m/d611w10 if decena==8
replace tpub_m=tpub_m/d611w11 if decena==9

*Gasto en Transporte foráneo deflactado

gen tfor_m=gasm if (clave>="M001" & clave<="M018") | (clave>="F010" & clave<="F015")

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

gen com_m=gasm if (clave>="F001" & clave<="F009")

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

*Gasto en Educación y recreación deflactado

gen edre_m=gasm if (clave>="E001" & clave<="E033") | (clave>="H073" & clave<="H074") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005")

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

*Gasto en Educación básica deflactado

gen edba_m=gasm if (clave>="E002" & clave<="E003") | (clave>="H073" & clave<="H074")

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

gen edu_gtosm=gasm if (clave>="E001" & clave<="E012") | (clave=="E013")  | (clave>="E014" & clave<="E019")   | (clave=="H073")  | (clave=="H074")

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


*Gasto en Cuidado personal deflactado

gen cuip_m=gasm if (clave>="D001" & clave<="D024") | (clave=="H118")

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

*Gasto en Accesorios personales deflactado

gen accp_m=gasm if (clave>="H109" & clave<="H117") | (clave=="H119")

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

*Gasto en Otros gastos y transferencias deflactado

gen otr_m=gasm if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T101" & clave<="T104") | (clave>="T902" & clave<="T914")

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

gen reda_m=gasm if (clave>="T901" & clave<="T914") | (clave=="N013")

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

saveold "$ruta\gastomonetario_def05.dta", replace

*********************************************************

/*Parte III

Creación del Gasto no monetario deflactado a pesos de 
agosto del 2005.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crean las bases:
"$ruta\gastonomonetario_def05.dta"
a nivel de clave de gasto, y
"$ruta\auto_def05.dta"
"$ruta\esp_def05.dta"
"$ruta\reg_def05.dta"
a nivel de folio.*/

*********************************************************

*No Monetario

use "$ruta\nomon.dta", clear

/*En el caso de la información de gasto no monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la séptima posición del 
folio del hogar. Primero se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folio,7,1))
tab decena

/*Al comparar con la información del catálogo, se observa que la 
información se levantó en nueve decenas, correspondientes a:

Decena	Periodo de levantamiento

0	|	del 10 al 16 de agosto
1	|	del 20 al 26 de agosto
2	|	del 30 de agosto al 05 de septiembre
3	|	del 09 al 15 de septiembre
4	|	del 19 al 25 de septiembre
5	|	del 29 de septiembre al 05 de octubre
6	|	del 09 al 15 de octubre
7	|	del 19 al 25 de octubre
8	|	del 29 de octubre al 04 de noviembre
9	|	del 08 al 14 de noviembre

Así, para realizar el análisis, y conforme a lo establecido 
en la información de la ENIGH2005, se tomarán como periodos 
de referencia:

Decena	Periodo de referencia
		/Semanal	/Mensual	/Trimestral	/Semestral

0	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
1	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
2	|	/Agosto	/Agosto	/Junio a agosto	/Marzo a agosto
3	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
4	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
5	|	/Septiembre	/Septiembre	/Julio a septiembre	/Abril a septiembre
6	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
7	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
8	|	/Octubre	/Octubre	/Agosto a octubre	/Mayo a octubre
9	|	/Noviembre	/Octubre	/Agosto a octubre	/Mayo a octubre

*Los rubros a considerar según la metodología del CTMP, 
serán:

Rubro	/Periodicidad	/Nombre del deflactor
Alimentos y bebidas no alcohólicas	/Semanal	/d11wmes
Bebidas alcohólicas y tabaco	/Semanal	/d12wmes
Vestido y calzado	/Trimestral	/d2tmesini
Vivienda y servicios de conservación	/Mensual	/d3mmes
Estimación del alquiler	/No se deflactará	/No se deflactará
Artículos y servicios de limpieza	/Mensual	/d42mmes
Cristalería y utensilios domésticos	/Trimestral	/d42tmesini
Enseres domésticos y muebles	/Semestral	/d41smesini
Cuidados de la salud	/Trimestral	/d51tmesini
Transporte público	/Semanal	/d611wmes
Transporte foráneo	/Semestral	/d6smesini
Comunicaciones	/Mensual	/d6mmes
Educación básica	/Mensual	/d7mmes
Cuidado personal	/Mensual	/d23mmes
Accesorios personales	/Trimestral	/d23tmesini
Otros gastos y transferencias	/Semestral	/dINPCsmesini
Regalos otorgados	/Semestral	/dINPCsmesini
*/
*Definición de los deflactores

*Rubro 1.1 semanal

scalar d11w07	=	1.0014137	
scalar d11w08	=	1.0000000	
scalar d11w09	=	1.0007401	
scalar d11w10	=	0.9955677	
scalar d11w11	=	0.9899628	

*Rubro 1.2 semanal

scalar d12w07	=	0.9973669	
scalar d12w08	=	1.0000000	
scalar d12w09	=	1.0010331	
scalar d12w10	=	1.0008685	
scalar d12w11	=	1.0059884	

*Rubro 2 trimestral

scalar d2t05	=	0.9977200	
scalar d2t06	=	0.9982503	
scalar d2t07	=	0.9994276	
scalar d2t08	=	1.0018143	

*Rubro 3 mensual

scalar d3m07	=	0.9973427	
scalar d3m08	=	1.0000000	
scalar d3m09	=	1.0031940	
scalar d3m10	=	1.0129081	
scalar d3m11	=	1.0406426	

*Rubro 4.2 mensual

scalar d42m07	=	1.0009064	
scalar d42m08	=	1.0000000	
scalar d42m09	=	1.0034521	
scalar d42m10	=	1.0054578	
scalar d42m11	=	1.0076949	

*Rubro 4.2 trimestral

scalar d42t05	=	0.9994600	
scalar d42t06	=	1.0009321	
scalar d42t07	=	1.0014528	
scalar d42t08	=	1.0029700	

*Rubro 4.1 semestral

scalar d41s02	=	1.0007906	
scalar d41s03	=	1.0015464	
scalar d41s04	=	1.0013127	
scalar d41s05	=	1.0009812	

*Rubro 5.1 trimestral

scalar d51t05	=	0.9935201	
scalar d51t06	=	0.9966056	
scalar d51t07	=	0.9993313	
scalar d51t08	=	1.0020372	

*Rubro 6.1.1 semanal

scalar d611w07	=	0.9988355	
scalar d611w08	=	1.0000000	
scalar d611w09	=	1.0034676	
scalar d611w10	=	1.0035533	
scalar d611w11	=	1.0057280	

*Rubro 6 mensual

scalar d6m07	=	0.9995307	
scalar d6m08	=	1.0000000	
scalar d6m09	=	1.0013174	
scalar d6m10	=	1.0044216	
scalar d6m11	=	1.0066325	

*Rubro 6 semestral

scalar d6s02	=	0.9930276	
scalar d6s03	=	0.9955919	
scalar d6s04	=	0.9977606	
scalar d6s05	=	0.9995969	

*Rubro 7 mensual

scalar d7m07	=	0.9977200	
scalar d7m08	=	1.0000000	
scalar d7m09	=	1.0191116	
scalar d7m10	=	1.0195559	
scalar d7m11	=	1.0209222	

*Rubro 2.3 mensual

scalar d23m07	=	0.9989895	
scalar d23m08	=	1.0000000	
scalar d23m09	=	1.0032622	
scalar d23m10	=	1.0067725	
scalar d23m11	=	1.0149013	

*Rubro 2.3 trimestral

scalar d23t05	=	0.9975741	
scalar d23t06	=	0.9991372	
scalar d23t07	=	1.0007505	
scalar d23t08	=	1.0033449	

*INPC semestral

scalar dINPCs02	=	0.9955288	
scalar dINPCs03	=	0.9971337	
scalar dINPCs04	=	0.9986626	
scalar dINPCs05	=	1.0000117	

*Una vez definidos los deflactores, se seleccionan los rubros de gasto de la metodología del CTMP

gen double gasnomon=gas_tri/3
gen auto=1 if tipo_gas=="1"
gen esp=1 if tipo_gas=="2"
gen reg=1 if tipo_gas=="3"
gen alq=1 if tipo_gas=="0"

*Se eliminan las claves de estimación del valor del alquiler conceptualmente idénticas

drop if (clave=="G001" | clave=="G011" | clave=="G013" | clave=="G015" | clave=="G017" )

*Gasto en Alimentos deflactado

gen ali_nm=gasnomon if (clave>="A001" & clave<="A218") | (clave>="A235" & clave<="A239")  | (clave=="A243")

replace ali_nm=ali_nm/d11w08 if decena==0
replace ali_nm=ali_nm/d11w08 if decena==1
replace ali_nm=ali_nm/d11w08 if decena==2
replace ali_nm=ali_nm/d11w09 if decena==3
replace ali_nm=ali_nm/d11w09 if decena==4
replace ali_nm=ali_nm/d11w09 if decena==5
replace ali_nm=ali_nm/d11w10 if decena==6
replace ali_nm=ali_nm/d11w10 if decena==7
replace ali_nm=ali_nm/d11w10 if decena==8
replace ali_nm=ali_nm/d11w11 if decena==9

*Gasto en Alcohol y tabaco deflactado

gen alta_nm=gasnomon if (clave>="A219" & clave<="A234") | (clave>="A240" & clave<="A242")

replace alta_nm=alta_nm/d12w08 if decena==0
replace alta_nm=alta_nm/d12w08 if decena==1
replace alta_nm=alta_nm/d12w08 if decena==2
replace alta_nm=alta_nm/d12w09 if decena==3
replace alta_nm=alta_nm/d12w09 if decena==4
replace alta_nm=alta_nm/d12w09 if decena==5
replace alta_nm=alta_nm/d12w10 if decena==6
replace alta_nm=alta_nm/d12w10 if decena==7
replace alta_nm=alta_nm/d12w11 if decena==8
replace alta_nm=alta_nm/d12w11 if decena==9

*Gasto en Vestido y calzado deflactado

gen veca_nm=gasnomon if (clave>="H001" & clave<="H072") | (clave>="H075" & clave<="H108")

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

*Gasto en Vivienda y servicios de conservación deflactado

gen viv_nm=gasnomon if (clave>="G002" & clave<="G010") | (clave>="G019" & clave<="G029")

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

*Gasto en Artículos de limpieza deflactado

gen lim_nm=gasnomon if (clave>="C001" & clave<="C024")

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

*Gasto en Cristalería y blancos deflactado

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

*Gasto en Enseres domésticos y muebles deflactado

gen ens_nm=gasnomon if (clave>="K001" & clave<="K036")

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

gen sal_nm=gasnomon if (clave>="J001" & clave<="J072")

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

*Gasto en Transporte público deflactado

gen tpub_nm=gasnomon if (clave>="B001" & clave<="B901")

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

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M018") | (clave>="F010" & clave<="F015")

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

gen com_nm=gasnomon if (clave>="F001" & clave<="F009")

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

*Gasto en Educación y recreación deflactado

gen edre_nm=gasnomon if (clave>="E001" & clave<="E033") | (clave>="H073" & clave<="H074") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005")

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

*Gasto en Educación básica deflactado

gen edba_nm=gasnomon if (clave>="E002" & clave<="E003") | (clave>="H073" & clave<="H074")

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

*Gasto en Cuidado personal deflactado

gen cuip_nm=gasnomon if (clave>="D001" & clave<="D024") | (clave=="H118")

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

*Gasto en Accesorios personales deflactado

gen accp_nm=gasnomon if (clave>="H109" & clave<="H117") | (clave=="H119")

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

*Gasto en Otros gastos y transferencias deflactado

gen otr_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T101" & clave<="T104") | (clave>="T902" & clave<="T914")

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

gen reda_nm=gasnomon if (clave>="T101" & clave<="T104") | (clave>="T902" & clave<="T914") | (clave=="N013")

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

gen double est_alq=alq*gasnomon

saveold "$ruta\gastonomonetario_def05.dta", replace

*Construcción de la base de autoconsumo a partir de la base de gasto no monetario

keep if auto==1

collapse (sum) *_nm, by (folio)

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

saveold "$ruta\auto_def05.dta", replace

use "$ruta\gastonomonetario_def05.dta", clear

*Construcción de la base de pagos en especie a partir de la base de gasto no monetario

keep if esp==1

collapse (sum) *_nm, by (folio)

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

saveold "$ruta\esp_def05.dta", replace

use "$ruta\gastonomonetario_def05.dta", clear


*Construcción de base de regalos a partir de la base no monetaria 

keep if (reg==1 | alq==1)

collapse (sum) *_nm est_alq, by (folio)

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

saveold "$ruta\reg_def05.dta", replace

*********************************************************

/*Parte IV

Cálculo de la incidencia 2005

Con el propósito de obtener cifras representativas a nivel 
nacional, rural y urbano, se obtiene una base con las 
variables de tamaño del hogar, estrato y factor de 
expansión, y a ésta se le unen las bases anteriormente 
generadas. Se estiman los ingresos corriente total y 
corriente neto, se genera el factor de expansión para 
personas, y se calcula la incidencia de la pobreza de 
acuerdo con el cálculo oficial de la Sedesol.

Se genera la base 
	"$ruta\basefinal_05.dta" 
la cual se encuentra a nivel de folio.*/

*********************************************************

use "$ruta\concen.dta", clear
rename hog factor
keep folio estrato factor tam_hog educacion
sort folio

*Modificado Mayra Sáenz Julio 2015 - Incluyo la base de hogares

merge 1:1 folio using "$ruta\hogares.dta" 
tab _merge 
drop _merge 
sort folio

*Modificado Mayra Sáenz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*
merge folio using "$ruta\ingreso_deflactado05.dta"
tab _merge
drop _merge
sort folio*/

merge 1:1 folio using "$ruta\gastomonetario_def05.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\auto_def05.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\esp_def05.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\reg_def05.dta"
tab _merge
drop _merge
sort folio

/*Construcción del indicador del tamaño de la localidad 
(rural para hogares en localidades con menos de 15,000 
habitantes y urbano en caso contrario)*/

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

saveold "$ruta\gtos_autoc05.dta", replace //Mayra Sáenz Julio 2015

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*


use "$ruta\pobla.dta", clear

sort folio num_ren, stable

merge 1:1 folio num_ren using "$ruta\ingreso_deflactado05_per.dta"
rename _merge _merge_ing
sort folio num_ren, stable

merge m:1 folio using "$ruta\gtos_autoc05.dta"

*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict=rsum(ing_monh nomon)                 if parentes=="100" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gct=rsum(gasmon nomon)                   if parentes=="100" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gnt=rsum(gasmon nomon redan reg_espn)    if parentes=="100" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if parentes=="100" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas

label var  ict "Ingreso corriente total"
label var  gct "Gasto corriente total"
label var  intt "Ingreso neto total"
label var  gnt "Gasto neto total"

*Información per capita

gen double ictpc= ict/tam_hog
gen double gctpc= gct/tam_hog
gen double intpc= intt/tam_hog
gen double gntpc= gnt/tam_hog

label var  ictpc "Ingreso corriente total per capita"
label var  gctpc "Gasto corriente totalper capita"
label var  intpc "Ingreso neto total per capita"
label var  gntpc "Gasto neto total per capita"


saveold "`base_out'", replace


log close

/*Agosto 1, 2006 (Victoria)
La variable de anios de educacion fue modificada de modo de ser comparbale y coherente entre los distintos anios.
El codigo anterior se puede ver en la seccion de educacion con * adelante del comando
El criterio utilizado para asignar a cada nivel un determinado numero de anios de estudio se puede ver en el
siguiente documento: X:\Sociometro_2005\documentation\Years of education in Mexico. 
Tambien se cambiaron las varianles edupi, edupc, edusi, edusc y etc de modo de ser coherente con los siguientes
anios: Primaria 6, Secundaria 6 (12 acumulado) y Terciaria 5 (17 acumulado).  

Agosto 29 de 2006 (Mafe)
Deficion de empleo se cambio, no incluía a todos los empleados, 
codigo anterior:
gen trab=real(trabajo) /*ultimo mes*
gen byte emp3_ci=(trab==1)
replace emp3_ci=1 if (causa>=1 & causa<=4 | causa==6)

Sept 7, 2006 (Mafe) minor error in the program with huge consequences in te size of rural areas
content of the variable estrato changed and the program didn't include that change 
previous code:
gen byte zona_c=real(estrato)
replace zona_c=1 if zona>0 & zona<4
replace zona_c=0 if zona>3 & zona<6*/

/*
****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
************************* PARA MEXICO 2005 *********************************
****************************************************************************/

*separando viviendas y hogares del archivo hoga*

clear
capture log close
cd X:\ARM\MEX\ENIGH\2005\Arm_data
set mem 300m
set more off


use "$ruta\hogares.dta",clear
gen n_vivi=substr(folio,11,1)
gen vivi=substr(folio,1,10)
sort vivi folio

saveold "$ruta\hoga2005.dta",replace

*merge de archivos*
sort folio 
saveold "$ruta\MEX2005EA_BID.dta",replace

use "$ruta\pobla.dta", clear
sort folio
merge folio using "$ruta\MEX2005EA_BID.dta"
drop _merge
sort folio num_ren
gen id=_n
saveold "$ruta\MEX2005EA_BID.dta",replace


use "$ruta\ingresos.dta", clear
gen ocupa1=cod_trab
tab cod_trab,gen(ocup) 

gen clave1=substr(clave,1,1)
gen clave2=real(substr(clave,2,3))
sort folio num_ren

tostring ocupa1, gen(ocupa)

* empleo primario*
by folio num_ren:egen ring1a_ml=sum(ing_1) if clave2>0 & clave2<=9 & ocupa=="1" 
by folio num_ren:egen ring1b_ml=sum(ing_1) if clave2==17 & ocupa=="1" 
by folio num_ren:egen ring1c_ml=sum(ing_1) if clave2==19 & ocupa=="1" 
by folio num_ren:egen ring1d_ml=sum(ing_1) if clave2==29 & ocupa=="1" 
for var ring1a_ml ring1b_ml ring1c_ml ring1d_ml: replace X=0 if X==.
by folio num_ren:gen ring1_ml=ring1a_ml+ring1b_ml+ring1c_ml+ring1d_ml 
by folio num_ren:egen ing1_ml=max(ring1_ml)

*empleos secundarios*
by folio num_ren:egen ring2a_ml=sum(ing_1) if clave2>0 & clave2<=9 & ocupa~="1" 
by folio num_ren:egen ring2b_ml=sum(ing_1) if clave2==17 & ocupa~="1" 
by folio num_ren:egen ring2c_ml=sum(ing_1) if clave2==19 & ocupa~="1" 
by folio num_ren:egen ring2d_ml=sum(ing_1) if clave2==29 & ocupa~="1"
for var ring2a_ml ring2b_ml ring2c_ml ring2d_ml: replace X=0 if X==.
by folio num_ren:gen ring2_ml=ring2a_ml+ring2b_ml+ring2c_ml+ring2d_ml 
by folio num_ren:egen ing2_ml=max(ring2_ml)

*negocios propios, empleo primario*
by folio num_ren:egen ring1a_npm=sum(ing_1) if clave2>=10 & clave2<=16 & ocupa=="1" 
by folio num_ren:egen ring1b_npm=sum(ing_1) if clave2==18 & ocupa=="1" 
by folio num_ren:egen ring1c_npm=sum(ing_1) if clave2==28 & ocupa=="1"
by folio num_ren:egen ring1d_npm=sum(ing_1) if clave2==38 & ocupa=="1"
for var ring1a_npm ring1b_npm ring1c_npm ring1d_npm: replace X=0 if X==.
by folio num_ren:gen ring1_npm=ring1a_npm+ring1b_npm+ring1c_npm+ring1d_npm
by folio num_ren:egen ing1_npm=max(ring1_npm)

*negocios propios, empleo secundario*
by folio num_ren:egen double ring2a_npm=sum(ing_1) if clave2>=10 & clave2<=16 & ocupa=="1"  
by folio num_ren:egen double ring2b_npm=sum(ing_1) if clave2==18 & ocupa~="1" 
by folio num_ren:egen double ring2c_npm=sum(ing_1) if clave2==28 & ocupa~="1"
by folio num_ren:egen double ring2d_npm=sum(ing_1) if clave2==38 & ocupa~="1"
for var ring2a_npm ring2b_npm ring2c_npm ring2d_npm: replace X=0 if X==.
by folio num_ren:gen ring2_npm=ring2a_npm+ring2b_npm+ring2c_npm+ring2d_npm
by folio num_ren:egen ing2_npm=max(ring2_npm)

*renta*
by folio num_ren:egen ring_rpm=sum(ing_1) if clave2>=39 & clave2<=47 
replace ring_rpm=0 if ring_rpm==.
by folio num_ren:egen ing_rpm=max(ring_rpm)

*transferencias*
by folio num_ren:egen ring_tm=sum(ing_1) if  clave2>=48 & clave2<=56
replace ring_tm=0 if ring_tm==.
by folio num_ren:egen ing_tm=max(ring_tm)

*transferencias por jublaciones y/o pensiones originadas dentro del pais*
by folio num_ren:egen ring_jub=sum(ing_1) if  clave2==48
replace ring_jub=0 if ring_jub==.
by folio num_ren:egen ing_jub=max(ring_jub)

*categorizado como remesas en 2002 PERO NO DEBERIA SER!! son regalos o donativos
*provenientes de otros hogares*
by folio num_ren:egen ring_otrosp=sum(ing_1) if clave2==57  
replace ring_otrosp=0 if ring_otrosp==.
by folio num_ren:egen ing_otrosp=max(ring_otrosp)

*remesas: Ingresos provenientes de otros paises
*NO EXISTE EN 2002*
by folio num_ren:egen ring_remesas=sum(ing_1) if clave2==58  
replace ring_remesas=0 if ring_remesas==.
by folio num_ren:egen ing_remesas=max(ring_remesas)

*remesas: Ingresos provenientes de Oportunidades
by folio num_ren:egen ring_oport=sum(ing_1) if clave2==59  
replace ring_oport=0 if ring_oport==.
by folio num_ren:egen ing_oport=max(ring_oport)

*regalos???, son los beneficios de porgramas gob, PROGRESA, PROCAMPO, etc*
by folio num_ren:egen ring_otm=sum(ing_1) if clave2>=59 &  clave2<=60
replace ring_otm=0 if ring_otm==.
by folio num_ren:egen ing_otm=max(ring_otm)

*OTROS INGRESOS CORRIENTES no considerados anteriormente*
by folio num_ren:egen ring_otros=sum(ing_1) if clave2==61
replace ring_otros=0 if ring_otros==.
by folio num_ren:egen ing_otros=max(ring_otros)

*financieras*
by folio num_ren:egen ring_otm1=sum(ing_1) if clave2>=62 &  clave2<=76
replace ring_otm1=0 if ring_otm1==.
by folio num_ren:egen ing_otm1=max(ring_otm1)

by folio num_ren:gen cont=(_n) 
drop if cont>1
drop  rin* ocupa ocup* clave ing_1  ing_tri ing_2 ing_3 ing_4 ing_5 ing_6 cont
sort folio num_ren

merge folio num_ren using "$ruta\MEX2005EA_BID.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge
sort folio num_ren
saveold "$ruta\MEX2005EA_BID.dta",replace

use "X:\ARM\MEX\ENIGH\2005\Orig_data\nomon.dta", clear
sort folio num_ren
by folio num_ren:egen ing_nml=sum(gasto) 

by folio num_ren:gen cont=(_n) /*elimino repetidos*/
drop if cont>1
tostring  folio,replace
keep folio num_ren ing_nml
sort folio num_ren
merge folio num_ren using "$ruta\MEX2005EA_BID.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge 

saveold "`base_out'", replace


log close


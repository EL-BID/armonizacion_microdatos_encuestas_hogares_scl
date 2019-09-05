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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENIGH\2010\m8_m11\data_orig\STATA"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2010"
local ronda m8_m11

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
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

Todas las bases de  datos de la ENIGH 2010 pueden ser obtenidas 
en la página de internet del INEGI, www.inegi.gob.mx, y
deben estar convertidas a formato *.dta (Stata)

En este programa se utilizan las siguientes bases: 

Base de ingresos: Ingresos.dta
Bases de gasto monetario:
				 *G_diario.dta
				 *Gastos.dta
				 *G_educa.dta
				 *G_recibo.dta

Base de ingreso no monetario: Nomon.dta
Base de hogares:Hogares.dta
Base de concentrado: concen.dta

En este programa se utilizan tres tipos de archivos, los cuales 
están ubicados en las siguientes carpetas:

1) Bases originales: "C:\pobreza ingresos\2010\ENIGH"
2) Bitácoras: "C:\pobreza ingresos\2010\Log"
3) Bases generadas: "C:\pobreza ingresos\2010\Resultados"


Para cambiar estas ubicaciones, se modifican los siguientes
globals 

gl data="C:\pobreza ingresos\2010\ENIGH"
gl log="C:\pobreza ingresos\2010\Log"
gl bases="C:\pobreza ingresos\2010\Resultados"


log using "$log\Pobreza 2010.txt", text replace

*********************************************************
*
*	PROGRAMA PARA LA MEDICIÓN DE LA POBREZA 2010
*
*********************************************************

CONEVAL Última modificación: 24 de julio del 2013

*********************************************************

*Parte I

*Creación del ingreso monetario deflactado a pesos de 
agosto del 2010.

Los deflactores se crean previamente a partir del INPC 
general (ver Nota Técnica).

En esta parte se crea la base:
	"$ruta\ingreso_deflactado10.dta"
la cual se encuentra a nivel de hogar (folio).*/

********************************************************

*Ingresos
use "$ruta\Ingresos.dta"
************
gen str folio= folioviv + foliohog
destring mes_*, replace
/*Las variables mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 definen
los meses a los que corresponden cada uno de los ingresos de 
la persona en los seis meses anteriores al levantamiento de 
la información. Estas variables toman cuatro valores cada una:
mes_1
7
8
9
10
mes_2
6
7
8
9
mes_3
5
6
7
8
mes_4
4
5
6
7
mes_5
3
4
5
6
mes_6
2
3
4
5

Así, se sabe que la encuesta fue levantada entre los meses 
de agosto y noviembre, y, por lo tanto, al preguntarse por 
los ingresos de los seis meses anteriores se recolectó 
información correspondiente a los meses de febrero, marzo,
abril, mayo, junio y julio*/

*Definición de los deflactores 2010
scalar ene10	=	0.992073162 
scalar feb10	=	0.997811088 
scalar mar10	=	1.004894946 
scalar abr10	=	1.001693042 
scalar may10	=	0.995381324 
scalar jun10	=	0.995069634 
scalar jul10	=	0.997230211 
scalar ago10	=	1.000000000 
scalar sep10	=	1.005242055 
scalar oct10	=	1.011447516 
scalar nov10	=	1.019551450 
scalar dic10	=	1.024602241 

/*La estrategia para deflactar será dividir cada columna 
de ingreso por el deflactor correspondiente a su mes y la
decena en que fue levantado*/

replace ing_6=ing_6/feb10 if mes_6==2
replace ing_6=ing_6/mar10 if mes_6==3
replace ing_6=ing_6/abr10 if mes_6==4
replace ing_6=ing_6/may10 if mes_6==5

replace ing_5=ing_5/mar10 if mes_5==3
replace ing_5=ing_5/abr10 if mes_5==4
replace ing_5=ing_5/may10 if mes_5==5
replace ing_5=ing_5/jun10 if mes_5==6

replace ing_4=ing_4/abr10 if mes_4==4
replace ing_4=ing_4/may10 if mes_4==5
replace ing_4=ing_4/jun10 if mes_4==6
replace ing_4=ing_4/jul10 if mes_4==7

replace ing_3=ing_3/may10 if mes_3==5
replace ing_3=ing_3/jun10 if mes_3==6
replace ing_3=ing_3/jul10 if mes_3==7
replace ing_3=ing_3/ago10 if mes_3==8

replace ing_2=ing_2/jun10 if mes_2==6
replace ing_2=ing_2/jul10 if mes_2==7
replace ing_2=ing_2/ago10 if mes_2==8
replace ing_2=ing_2/sep10 if mes_2==9

replace ing_1=ing_1/jul10 if mes_1==7
replace ing_1=ing_1/ago10 if mes_1==8
replace ing_1=ing_1/sep10 if mes_1==9
replace ing_1=ing_1/oct10 if mes_1==10

/*Se deflacta la clave P008 y P015 (Reparto de utilidades) 
con el deflactor de mayo a agosto 2010
y se obtiene el promedio mensual.*/

replace ing_1=(ing_1/may10)/12 if clave=="P008" | clave=="P015"

/*Una vez realizada la deflactación, se procede a obtener el 
ingreso mensual promedio en los últimos seis meses, para 
cada persona y clave de ingreso*/

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)

/*Para obtener el ingreso corriente monetario, se seleccionan 
las claves de ingreso correspondientes*/

gen double ing_mon=ing_mens if (clave>="P001" & clave<="P008") | (clave>="P011" & clave<="P015") ///
                             | (clave>="P018" & clave<="P048") | (clave>="P067" & clave<="P081")
                             
*Modificación Mayra Sáenz - Agosto 2015 - en la sintaxis original no incluía la P008 ni P015  (Reparto de utilidades del ejercicio 2009) en el ingreso laboral, pero sí en el total monetario. Por lo tanto, se incluye.
gen double ing_lab=ing_mens if (clave>="P001" & clave<="P008") | (clave=="P011") | (clave>="P013" & clave<="P015") ///
                             | (clave=="P018") | (clave>="P020" & clave<="P022") ///
							 | (clave>="P067" & clave<="P081") 
                                                          
gen double ing_trab=ing_mens if (clave>="P001" & clave<="P008") | (clave=="P011") | (clave>="P013" & clave<="P015") ///
                             | (clave=="P018") | (clave=="P020") | (clave=="P067")

gen double ing_negp=ing_mens if (clave>="P068" & clave<="P081") | (clave>="P021" & clave<="P022") 

gen double ing_rent=ing_mens if (clave=="P012") | (clave=="P019") | (clave>="P023" & clave<="P031")

gen double ing_tran=ing_mens if (clave>="P032" & clave<="P048")


*Modificación Mayra Sáenz - Agosto 2015 - Se divide al ingreso laboral en principal y secundario

gen double ing_trab1=ing_mens if (clave>="P001" & clave<="P008") | (clave=="P011") | (clave=="P013") 
gen double ing_trab2=ing_mens if (clave>="P014" & clave<="P015") | (clave=="P018") | (clave=="P020") | (clave=="P067")

gen double ing_negp1=ing_mens if (clave>="P068" & clave<="P074") 
gen double ing_negp2=ing_mens if (clave>="P075" & clave<="P081") | (clave>="P021" & clave<="P022") 

*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens  if  (clave=="P032" )                                     //Jubilaciones y pensiones originados dentro del país P032, No se incluyen las provenientes de otros países P033
g double trat_pr  = ing_mens  if  ((clave>="P034" & clave<="P036") | clave=="P041")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por accidentes de trabajo, Indemnizaciones por despido y retiro voluntario ,Ingresos provenientes de otros paises
g double trat_pu  = ing_mens  if  (clave>="P042" & clave<="P048")                      //Desde 1996 aparece PROCAMPO, en 2002 se incluye Beneficio de Oportunidades; en 2008 se incluye programa para adultos mayores y beneficios de otros programas sociales; en 2010 Se desagregan los programas de adultos mayores y los otros programas sociales: Beneficio del programa 70 y más, Beneficio de otros programas para adultos mayores, Beneficio del programa Alimentario, Beneficio del programa Empleo Temporal, Beneficios de otros programas sociales.
g double dona_pu  = ing_mens  if  (clave=="P038")                                     //Becas provenientes del gobierno                      
g double dona_pr  = ing_mens  if  (clave=="P037" | clave=="P039" | clave=="P040")      //Becas y donativos provenientes de organizaciones no gubernamentales; Regalos o donativos en dinero provenientes de otros hogares
g double otros    = ing_mens  if  ((clave>="P049" & clave<="P066"))                    //Otros ingresos no considerados en los anteriores (especifique); ¿cuánto dinero recibió por rendimientos de acciones que posea de alguna empresa en la que no trabajó?; Retiro de inversiones, ahorros, tandas, cajas de ahorro, etc.;  Pagos recibidos de préstamos que usted hizo a personas no miembros del hogar;  Préstamos recibidos de personas no miembros del hogar o instituciones; Venta de monedas, metales preciosos, joyas y obras de arte; Venta de acciones; Venta de bonos; Venta de cédulas; Venta de marcas, patentes y derechos de autor; Herencias, dotes y legados; Loterías y juegos de azar;  Venta de casas, terrenos, condominios, etc. que están dentro del país propiedad de algún miembro del hogar;  Venta de casas, terrenos, condominios, etc.que están fuera del país propiedad de algún miembro del hogar; Venta de maquinaria, equipos, animales de producción, vehículos, etc. utilizados en el negocio propiedad del hogar; Venta de vehículos, aparátos eléctricos de segunda mano, etc.;  Préstamos hipotecarios por bienes inmuebles: casa, terrenos, edificios y locales; Seguro de vida; Otras percepciones financieras de capital no consideradas en las anteriores (especifique).
g double remesas  = ing_mens  if  (clave=="P041")                                      //Ingresos provenientes de otros paises


*Modificación Mayra Sáenz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}



*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.

/*A continuación se estima el total de ingresos de cada 
hogar y fuente de ingreso, y se guarda la base en un 
archivo aparte*/

*collapse (sum) ing_mon ing_lab ing_trab ing_negp ing_rent ing_tran, by(folio)


collapse (sum) ing_mens ing_mon ing_lab ing_trab* ing_negp* ing_rent ing_tran ypension trat_pr trat_pu dona_pu dona_pr otros remesas P*, by(folio numren)
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

sort folio numren, stable

saveold "$ruta\ingreso_deflactado10_per.dta", replace

*********************************************************
/*Parte II

Creación del gasto monetario deflactado a pesos de agosto 
de 2010.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crea la base:
	"$ruta\gastomonetario_def10.dta"
la cual se encuentra a nivel de hogares (folio).

*********************************************************

*Gasto Monetario
En la ENIGH 2010 hay 4 bases de gasto que se utilizan:
* G_diario: que contiene las claves de Alimentos y 
Transporte Público.
* G_educa: que contiene las claves de Educación (E001 a E007).
* Gastos: que contiene todas las demás claves que se utilizan. 
* G_recibo: que contiene los recibos pagados por el hogar

*Se unen las 4 bases de gasto contenidas en la ENIGH 2010*/

use "$ruta\G_recibo.dta", clear
rename recibo_tri gas_tri
g base = 1

append using "$ruta\G_diario.dta"
replace base = 2 if base ==.

append using "$ruta\Gastos.dta"
replace base = 3 if base ==.

append using "$ruta\G_educa.dta"
replace base = 4 if base ==.

label define base 1 "Gto. Recibo" 2 "Gto. Diario" 3 "Gastos" 4 "Gto. Educa", add modify
label value base base

gen str folio= folioviv + foliohog

/*En el caso de la información de gasto, para deflactar se 
utiliza la decena de levantamiento de la encuesta, la cual 
se encuentra en la tercera posición del folio del hogar. En 
primer lugar se obtiene una variable que identifique la 
decena de levantamiento*/

gen decena=real(substr(folio,3,1))
tab decena

/*Al comparar con la información del catálogo, se observa que la 
información se levantó en nueve decenas, correspondientes a:

Decena |	Periodo de levantamiento 	Periodo de referencia de mes pasado 
0	 |	11-17 de Agosto 			Julio 
1	 |	21-27 de Agosto 			Julio 
2	 |	31-06 de Septiembre 		Agosto 
3	 |	10-16 de Septiembre 		Agosto 
4	 |	20-26 Septiembre 			Agosto 
5	 |	30-06 de Octubre 			Septiembre 
6	 |	10-16 de Octubre 			Septiembre 
7	 |	20-26 de Octubre 			Septiembre 
8	 |	30-05 de Noviembre 		Octubre 
9	 |	09-15 de Noviembre 		Octubre 


Así, para realizar el análisis, y conforme a lo establecido 
en la información de la ENIGH2010, se tomarán como periodos 
de referencia:

Decena	|	Periodo de referencia
		/Semanal	/Mensual	/Trimestral	/Semestral

0     	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
1     	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
2     	|	/Agosto	/Agosto	/Junio a agosto	/Marzo a agosto
3     	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
4     	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
5     	|	/Septiembre	/Septiembre	/Julio a septiembre	/Abril a septiembre
6     	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
7     	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
8     	|	/Octubre	/Octubre	/Agosto a octubre	/Mayo a octubre
9     	|	/Noviembre	/Octubre	/Agosto a octubre	/Mayo a octubre

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

*Rubro 1.1 semanal, Alimentos

scalar d11w07=	0.994115718 
scalar d11w08=	1.000000000 
scalar d11w09=	1.008539462 
scalar d11w10=	1.017879893 
scalar d11w11=	1.027182482 

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco

scalar d12w07=	0.998897524 
scalar d12w08=	1.000000000 
scalar d12w09=	1.000831930 
scalar d12w10=	1.003916158 
scalar d12w11=	1.006574275 

*Rubro 2 trimestral, Vestido, calzado y accesorios

scalar d2t05	=	0.995272670 
scalar d2t06	=	0.996865859 
scalar d2t07	=	0.999512467 
scalar d2t08	=	1.004622858 

*Rubro 3 mensual, Vivienda

scalar d3m07	=	0.997305493 
scalar d3m08	=	1.000000000 
scalar d3m09	=	1.002184127 
scalar d3m10	=	1.013022202 
scalar d3m11	=	1.032274044 

*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar

scalar d42m07	=	0.996039267 
scalar d42m08	=	1.000000000 
scalar d42m09	=	1.005737511 
scalar d42m10	=	1.013903284 
scalar d42m11	=	1.013118541 

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar

scalar d42t05	=	0.996678413 
scalar d42t06	=	0.997051043 
scalar d42t07	=	1.000592259 
scalar d42t08	=	1.006546932 

*Rubro 4.1 semestral, Muebles y aparatos dómesticos

scalar d41s02	=	1.003788793 
scalar d41s03	=	1.003375892 
scalar d41s04	=	1.003130007 
scalar d41s05	=	1.002992373 

*Rubro 5.1 trimestral, Salud

scalar d51t05	=	0.993531742 
scalar d51t06	=	0.996780712 
scalar d51t07	=	0.999847027 
scalar d51t08	=	1.002728404 

*Rubro 6.1.1 semanal, Transporte público urbano

scalar d611w07	=	0.997849308 
scalar d611w08	=	1.000000000 
scalar d611w09	=	1.001300875 
scalar d611w10	=	1.001771543 
scalar d611w11	=	1.003163936 

*Rubro 6 mensual, Transporte

scalar d6m07	=	0.999363658 
scalar d6m08	=	1.000000000 
scalar d6m09	=	1.002852812 
scalar d6m10	=	1.004676038 
scalar d6m11	=	1.009516523 

*Rubro 6 semestral, Transporte

scalar d6s02	=	0.985248555 
scalar d6s03	=	0.990186756 
scalar d6s04	=	0.994255051 
scalar d6s05	=	0.997889586 

*Rubro 7 mensual, Educación y esparcimiento

scalar d7m07	=	1.001412258 
scalar d7m08	=	1.000000000 
scalar d7m09	=	1.013503880 
scalar d7m10	=	1.015561743 
scalar d7m11	=	1.015702968 

*Rubro 2.3 mensual, Accesorios y cuidados del vestido

scalar d23m07	=	0.995919730 
scalar d23m08	=	1.000000000 
scalar d23m09	=	1.010277472 
scalar d23m10	=	1.020695182 
scalar d23m11	=	1.033109620 

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido

scalar d23t05	=	0.996204659 
scalar d23t06	=	0.997974334 
scalar d23t07	=	1.002065734 
scalar d23t08	=	1.010324218 

*INPC semestral

scalar dINPCs02	=	0.998680041 
scalar dINPCs03	=	0.999044860 
scalar dINPCs04	=	0.999102711 
scalar dINPCs05	=	1.000728457 

*Una vez definidos los deflactores, se seleccionan los rubros de gasto de la Metodología del CTMP

gen double gasm=gas_tri/3

*Gasto en Alimentos deflactado (semanal)

gen ali_m=gasm if (clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247")

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

*Gasto en Alcohol y tabaco deflactado (semanal)

gen alta_m=gasm if (clave>="A223" & clave<="A241")

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

*Gasto en Vestido y calzado deflactado (trimestral)

gen veca_m=gasm if (clave>="H001" & clave<="H122") | (clave=="H136")

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

*Gasto en Vivienda y servicios de conservación deflactado (mensual)

gen viv_m=gasm if (clave>="G001" & clave<="G016") | (clave>="R001" & clave<="R004") | (clave=="R013")

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

*Gasto en Artículos de limpieza deflactado (mensual)

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

*Gasto en Cristalería y blancos deflactado (trimestral)

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

*Gasto en Enseres domésticos y muebles deflactado (semestral)

gen ens_m=gasm if (clave>="K001" & clave<="K037")

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

*Gasto en Salud deflactado (trimestral)

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

*Gasto en Transporte público deflactado (semanal)

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

*Gasto en Transporte foráneo deflactado (semestral)

gen tfor_m=gasm if (clave>="M001" & clave<="M018") | (clave>="F007" & clave<="F014")

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

*Gasto en Comunicaciones deflactado (mensual)

gen com_m=gasm if (clave>="F001" & clave<="F006") | (clave>="R005" & clave<="R008") | (clave>="R010" & clave<="R011")

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

*Gasto en Educación y recreación deflactado (mensual)

gen edre_m=gasm if (clave>="E001" & clave<="E034") | (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005") | (clave=="R009")


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


*Gasto en Educación básica deflactado (mensual)

gen edba_m=gasm if (clave>="E002" & clave<="E003") | (clave>="H134" & clave<="H135")


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


*================================================================================================================================================================
*Modificación Mayra Saenz - Abril 2017: Se desagrega el gasto unicamente en educación (se excluye recreación)

*En 2008, hay una parte de gastos que están a nivel de personas y otra a nivel de hogares, por lo que se calcula separado

*Gasto monetario sólo educación (gastos a nivel de personas)
preserve

gen edu_gtosm=gasm if ((clave>="E001" & clave<="E007") & base ==4) 

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


sort folioviv foliohog numren
collapse (sum) edu_gtosm  , by(folioviv foliohog numren)

rename  edu_gtosm edu_gtosmp
label var edu_gtosmp "Gto. Edu. a nivel de personas"

sort folioviv foliohog

save "$ruta\edu_gtosmp", replace

restore

preserve

*Gasto monetario sólo educación (gastos a nivel de hogares)

gen edu_gtosm=gasm if  ((clave>="E008" & clave<="E021") | (clave=="H134") | (clave=="H135")) & base ==3  

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

collapse (sum) edu_gtosm  , by(folio)

rename  edu_gtosm edu_gtosmh
label var edu_gtosmh "Gto. Edu. a nivel de hogar"

sort folio

save "$ruta\edu_gtosmh", replace

restore
*================================================================================================================================================================

*Gasto monetario sólo educación (gastos a nivel de hogares y personas)

gen edu_gtosm=gasm if ((clave>="E001" & clave<="E007") & base ==4)  | (((clave>="E008" & clave<="E021") | (clave=="H134") | (clave=="H135")) & base ==3)  

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


*Gasto en Cuidado personal deflactado (mensual)

gen cuip_m=gasm if (clave>="D001" & clave<="D026") | (clave=="H131")


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

*Gasto en Accesorios personales deflactado (trimestral)

gen accp_m=gasm if (clave>="H123" & clave<="H131") | (clave=="H133")

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

*Gasto en Otros gastos y transferencias deflactado (semestral)

gen otr_m=gasm if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T915") | (clave=="R012")

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

gen reda_m=gasm if (clave>="T901" & clave<="T915") | (clave=="N013")

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
sort folio

label var edu_gtosm "Gto. Edu. agregado de hogares y de personas"

saveold "$ruta\gastomonetario_def10.dta", replace

*********************************************************

/*Parte III 

Creación del ingreso no monetario deflactado a pesos de 
agosto del 2010.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crean las bases:
"$ruta\ingresonomonetario_def10.dta"
a nivel de clave de gasto, y
"$ruta\auto_def10.dta"
"$ruta\esp_def10.dta"
"$ruta\reg_def10.dta"
a nivel de hogar (folio).
*/
*********************************************************

*No Monetario

use "$ruta\Nomon.dta", clear
gen str folio= folioviv + foliohog

/*En el caso de la información de ingreso no monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la tercera posición del 
folio del hogar. En primer lugar se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folio,3,1))
tab decena,m


/*Al comparar con la información del catálogo, se observa que la 
información se levantó en nueve decenas, correspondientes a:

Decena |	Periodo de levantamiento 	Periodo de referencia de mes pasado 
0	 |	11-17 de Agosto 			Julio 
1	 |	21-27 de Agosto 			Julio 
2	 |	31-06 de Septiembre 		Agosto 
3	 |	10-16 de Septiembre 		Agosto 
4	 |	20-26 Septiembre 			Agosto 
5	 |	30-06 de Octubre 			Septiembre 
6	 |	10-16 de Octubre 			Septiembre 
7	 |	20-26 de Octubre 			Septiembre 
8	 |	30-05 de Noviembre 		Octubre 
9	 |	09-15 de Noviembre 		Octubre 


Así, para realizar el análisis, y conforme a lo establecido 
en la información de la ENIGH 2010, se tomarán como periodos 
de referencia:

Decena	|	Periodo de referencia
		/Semanal	/Mensual	/Trimestral	/Semestral

0     	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
1     	|	/Agosto	/Julio	/Mayo a julio	/Febrero a julio
2     	|	/Agosto	/Agosto	/Junio a agosto	/Marzo a agosto
3     	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
4     	|	/Septiembre	/Agosto	/Junio a agosto	/Marzo a agosto
5     	|	/Septiembre	/Septiembre	/Julio a septiembre	/Abril a septiembre
6     	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
7     	|	/Octubre	/Septiembre	/Julio a septiembre	/Abril a septiembre
8     	|	/Octubre	/Octubre	/Agosto a octubre	/Mayo a octubre
9     	|	/Noviembre	/Octubre	/Agosto a octubre	/Mayo a octubre

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

*Rubro 1.1 semanal, Alimentos

scalar d11w07=	0.994115718 
scalar d11w08=	1.000000000 
scalar d11w09=	1.008539462 
scalar d11w10=	1.017879893 
scalar d11w11=	1.027182482 

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco

scalar d12w07=	0.998897524 
scalar d12w08=	1.000000000 
scalar d12w09=	1.000831930 
scalar d12w10=	1.003916158 
scalar d12w11=	1.006574275 

*Rubro 2 trimestral, Vestido, calzado y accesorios

scalar d2t05	=	0.995272670 
scalar d2t06	=	0.996865859 
scalar d2t07	=	0.999512467 
scalar d2t08	=	1.004622858 

*Rubro 3 mensual, Vivienda

scalar d3m07	=	0.997305493 
scalar d3m08	=	1.000000000 
scalar d3m09	=	1.002184127 
scalar d3m10	=	1.013022202 
scalar d3m11	=	1.032274044 

*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar

scalar d42m07	=	0.996039267 
scalar d42m08	=	1.000000000 
scalar d42m09	=	1.005737511 
scalar d42m10	=	1.013903284 
scalar d42m11	=	1.013118541 

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar

scalar d42t05	=	0.996678413 
scalar d42t06	=	0.997051043 
scalar d42t07	=	1.000592259 
scalar d42t08	=	1.006546932 

*Rubro 4.1 semestral, Muebles y aparatos dómesticos

scalar d41s02	=	1.003788793 
scalar d41s03	=	1.003375892 
scalar d41s04	=	1.003130007 
scalar d41s05	=	1.002992373 

*Rubro 5.1 trimestral, Salud

scalar d51t05	=	0.993531742 
scalar d51t06	=	0.996780712 
scalar d51t07	=	0.999847027 
scalar d51t08	=	1.002728404 

*Rubro 6.1.1 semanal, Transporte público urbano

scalar d611w07	=	0.997849308 
scalar d611w08	=	1.000000000 
scalar d611w09	=	1.001300875 
scalar d611w10	=	1.001771543 
scalar d611w11	=	1.003163936 

*Rubro 6 mensual, Transporte

scalar d6m07	=	0.999363658 
scalar d6m08	=	1.000000000 
scalar d6m09	=	1.002852812 
scalar d6m10	=	1.004676038 
scalar d6m11	=	1.009516523 

*Rubro 6 semestral, Transporte

scalar d6s02	=	0.985248555 
scalar d6s03	=	0.990186756 
scalar d6s04	=	0.994255051 
scalar d6s05	=	0.997889586 

*Rubro 7 mensual, Educación y esparcimiento

scalar d7m07	=	1.001412258 
scalar d7m08	=	1.000000000 
scalar d7m09	=	1.013503880 
scalar d7m10	=	1.015561743 
scalar d7m11	=	1.015702968 

*Rubro 2.3 mensual, Accesorios y cuidados del vestido

scalar d23m07	=	0.995919730 
scalar d23m08	=	1.000000000 
scalar d23m09	=	1.010277472 
scalar d23m10	=	1.020695182 
scalar d23m11	=	1.033109620 

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido

scalar d23t05	=	0.996204659 
scalar d23t06	=	0.997974334 
scalar d23t07	=	1.002065734 
scalar d23t08	=	1.010324218 

*INPC semestral

scalar dINPCs02	=	0.998680041 
scalar dINPCs03	=	0.999044860 
scalar dINPCs04	=	0.999102711 
scalar dINPCs05	=	1.000728457 

/*Una vez definidos los deflactores, se seleccionan los 
rubros de gasto de la metodología del CTMP*/

gen double ingnomon=apo_tri/3
gen auto=1  if tipogasto=="1"
gen esp=1 if tipogasto=="2"
gen reg=1 if tipogasto=="3"
replace reg=1 if tipogasto=="4"

*Gasto en Alimentos deflactado (semanal) 

gen ali_nm=ingnomon if (clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247")

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

*Gasto en Alcohol y tabaco deflactado (semanal)

gen alta_nm=ingnomon if (clave>="A223" & clave<="A241")

replace alta_nm=alta_nm/d12w08 if decena==0
replace alta_nm=alta_nm/d12w08 if decena==1
replace alta_nm=alta_nm/d12w08 if decena==2
replace alta_nm=alta_nm/d12w09 if decena==3
replace alta_nm=alta_nm/d12w09 if decena==4
replace alta_nm=alta_nm/d12w09 if decena==5
replace alta_nm=alta_nm/d12w10 if decena==6
replace alta_nm=alta_nm/d12w10 if decena==7
replace alta_nm=alta_nm/d12w10 if decena==8
replace alta_nm=alta_nm/d12w11 if decena==9

*Gasto en Vestido y calzado deflactado (trimestral)

gen veca_nm=ingnomon if (clave>="H001" & clave<="H122") | (clave=="H136")

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

*Gasto en Vivienda y servicios de conservación deflactado (mensual)

gen viv_nm=ingnomon if (clave>="G001" & clave<="G016") | (clave>="R001" & clave<="R004") | (clave=="R013")

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

*Gasto en Artículos de limpieza deflactado (mensual)

gen lim_nm=ingnomon if (clave>="C001" & clave<="C024")

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

*Gasto en Cristalería y blancos deflactado (trimestral)

gen cris_nm=ingnomon if (clave>="I001" & clave<="I026")

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

*Gasto en Enseres domésticos y muebles deflactado (semestral)

gen ens_nm=ingnomon if (clave>="K001" & clave<="K037")

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

*Gasto en Salud deflactado (trimestral)

gen sal_nm=ingnomon if (clave>="J001" & clave<="J072")

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

*Gasto en Transporte público deflactado (semanal)

gen tpub_nm=ingnomon if (clave>="B001" & clave<="B007")

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

*Gasto en Transporte foráneo deflactado (semestral)

gen tfor_nm=ingnomon if (clave>="M001" & clave<="M018") | (clave>="F007" & clave<="F014")

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

*Gasto en Comunicaciones deflactado (mensual)

gen com_nm=ingnomon if (clave>="F001" & clave<="F006") | (clave>="R005" & clave<="R008") | (clave>="R010" & clave<="R011")

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

*Gasto en Educación y recreación deflactado (mensual)

gen edre_nm=ingnomon if (clave>="E001" & clave<="E034") | (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005") | (clave=="R009")


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


*Gasto en Educación básica deflactado (mensual)

gen edba_nm=ingnomon if (clave>="E002" & clave<="E003") | (clave>="H134" & clave<="H135")


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


*Modificación Mayra Saenz - Abril 2017: Se desagrega el gasto unicamente en educación (se excluye recreación)
*Los gastos no monetarios en educación ya sean de regalos o donaciones están a nivel de personas
preserve

gen edu_gtosnm=ingnomon if (clave>="E001" & clave<="E021") | (clave=="H134") | (clave=="H135")   

replace edu_gtosnm=edu_gtosnm/d7m07 if decena==0
replace edu_gtosnm=edu_gtosnm/d7m07 if decena==1
replace edu_gtosnm=edu_gtosnm/d7m08 if decena==2
replace edu_gtosnm=edu_gtosnm/d7m08 if decena==3
replace edu_gtosnm=edu_gtosnm/d7m08 if decena==4
replace edu_gtosnm=edu_gtosnm/d7m09 if decena==5
replace edu_gtosnm=edu_gtosnm/d7m09 if decena==6
replace edu_gtosnm=edu_gtosnm/d7m09 if decena==7
replace edu_gtosnm=edu_gtosnm/d7m10 if decena==8
replace edu_gtosnm=edu_gtosnm/d7m10 if decena==9


sort folioviv foliohog numren
collapse (sum) edu_gtosnm  , by(folioviv foliohog numren)

rename  edu_gtosnm edu_gtosnmp

sort folioviv foliohog

save "$ruta\edu_gtosnmp", replace

restore


*Gasto en Cuidado personal deflactado (mensual)

gen cuip_nm=ingnomon if (clave>="D001" & clave<="D026") | (clave=="H132")


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

*Gasto en Accesorios personales deflactado (trimestral)

gen accp_nm=ingnomon if (clave>="H123" & clave<="H131") | (clave=="H133")

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

*Gasto en Otros gastos y transferencias deflactado (semestral)

gen otr_nm=ingnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T915") | (clave=="R012")

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

gen reda_nm=ingnomon if (clave>="T901" & clave<="T915") | (clave=="N013")

replace reda_nm=reda_nm/dINPCs02 if decena==1
replace reda_nm=reda_nm/dINPCs03 if decena==2
replace reda_nm=reda_nm/dINPCs03 if decena==3
replace reda_nm=reda_nm/dINPCs03 if decena==4
replace reda_nm=reda_nm/dINPCs04 if decena==5
replace reda_nm=reda_nm/dINPCs04 if decena==6
replace reda_nm=reda_nm/dINPCs04 if decena==7
replace reda_nm=reda_nm/dINPCs05 if decena==8
replace reda_nm=reda_nm/dINPCs05 if decena==9

saveold "$ruta\ingresonomonetario_def10.dta", replace


*Costos (Temporalmente, sólo se guardan los costos de educación)

use "$ruta\Gastocosto.dta", clear
gen str folio= folioviv + foliohog

gen decena=real(substr(folio,3,1))
tab decena,m


*Rubro 7 mensual, Educación y esparcimiento

scalar d7m07	=	1.001412258 
scalar d7m08	=	1.000000000 
scalar d7m09	=	1.013503880 
scalar d7m10	=	1.015561743 
scalar d7m11	=	1.015702968 

gen double costom=costo_tri/3

gen edu_costo=costom if (clave>="E001" & clave<="E021") | (clave>="H134" & clave<="H135") 

replace edu_costo=edu_costo/d7m07 if decena==0
replace edu_costo=edu_costo/d7m07 if decena==1
replace edu_costo=edu_costo/d7m08 if decena==2
replace edu_costo=edu_costo/d7m08 if decena==3
replace edu_costo=edu_costo/d7m08 if decena==4
replace edu_costo=edu_costo/d7m09 if decena==5
replace edu_costo=edu_costo/d7m09 if decena==6
replace edu_costo=edu_costo/d7m09 if decena==7
replace edu_costo=edu_costo/d7m10 if decena==8
replace edu_costo=edu_costo/d7m10 if decena==9

sort folioviv foliohog 
collapse (sum)  edu_costo  , by(folio)

rename edu_costo edu_costoh
label var  edu_costoh "Costos en educación a nivel de hogar"


save "$ruta\edu_costosh", replace


*Construcción de la base de autoconsumo a partir de la base de ingreso no monetario

use "$ruta\ingresonomonetario_def10.dta", clear

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

saveold "$ruta\auto_def10.dta", replace

use "$ruta\ingresonomonetario_def10.dta", clear

*Construcción de la base de pagos en especie a partir de la base de ingreso no monetario

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

saveold "$ruta\esp_def10.dta", replace

use "$ruta\ingresonomonetario_def10.dta", clear

*Construcción de base de regalos a partir de la base de ingresos no monetarios 

keep if (reg==1)

collapse (sum) *_nm, by (folio)

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

saveold "$ruta\reg_def10.dta", replace


*Construcción de la base de Estimación del Alquiler
use "$ruta\Hogares.dta", clear

gen str folio= folioviv + foliohog

recode  estim_tri (.=0) (-1=0)
 
sort folio

collapse (sum) estim_tri, by(folio)
gen est_alq= estim_tri/3

sort folio

saveold "$ruta\alq_def10.dta", replace


*********************************************************

/*Parte IV

Cálculo de la incidencia 2010

Con el propósito de obtener cifras representativas a nivel 
nacional, rural y urbano, se obtiene una base con las 
variables de tamaño del hogar, estrato y factor de 
expansión, y a ésta se le unen las bases anteriormente 
generadas. Se estiman los ingresos corriente total y 
corriente neto, se genera el factor de expansión para 
personas, y se calcula la incidencia de la pobreza de 
acuerdo con la metodología del CTMP y 
el cálculo oficial de la Sedesol.

Se genera la base 
	"$ruta\basefinal10.dta" 
la cual se encuentra a nivel de hogar (folio).*/

*********************************************************


*Genero el identificador en la base de hogares.

use "$ruta\Hogares.dta" , clear
gen str folio= folioviv + foliohog
saveold "$ruta\Hogares_.dta", replace

use "$ruta\Concen.dta", clear

gen str folio= folioviv + foliohog

keep folio tam_hog tam_loc factor est_dis upm educacion

sort folio

*Modificado Mayra Sáenz Julio 2015 - Incluyo la base de hogares

merge 1:1 folio using "$ruta\Hogares_.dta" 
tab _merge 
drop _merge 
sort folio

*Modificado Mayra Sáenz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*
merge folio using "$ruta\ingreso_deflactado10.dta"
tab _merge
drop _merge
sort folio*/

merge 1:1 folio using "$ruta\gastomonetario_def10.dta"
tab _merge
drop _merge
sort folio

merge 1:1  folio using "$ruta\alq_def10.dta"
tab _merge
drop _merge
sort folio

merge 1:1  folio using "$ruta\esp_def10.dta"
tab _merge
drop _merge
sort folio

merge 1:1  folio using "$ruta\reg_def10.dta"
tab _merge
drop _merge
sort folio

merge 1:1  folio using "$ruta\auto_def10.dta"
tab _merge
drop _merge
sort folio

destring tam_loc, replace
gen rururb=1 if (tam_loc>2 & tam_loc!=.)
replace rururb=0 if tam_loc<=2
label define rururb 1 "Rural" 0 "Urbano"
label value rururb rururb

*Se calculan los gastos totales
capture drop autocons
egen double gasmon=rsum(ali_m alta_m veca_m viv_m lim_m ens_m cris_m sal_m tpub_m tfor_m com_m edre_m cuip_m accp_m otr_m)

egen double autocons=rsum(ali_nma alta_nma veca_nma viv_nma lim_nma ens_nma cris_nma sal_nma tpub_nma tfor_nma com_nma edre_nma cuip_nma accp_nma otr_nma)

egen double pago_esp=rsum(ali_nme alta_nme veca_nme viv_nme lim_nme ens_nme cris_nme sal_nme tpub_nme tfor_nme com_nme edre_nme cuip_nme accp_nme otr_nme)

egen double reg_esp=rsum(ali_nmr alta_nmr veca_nmr viv_nmr lim_nmr ens_nmr cris_nmr sal_nmr tpub_nmr tfor_nmr com_nmr edre_nmr cuip_nmr accp_nmr otr_nmr)

egen double nomon=rsum(autocons pago_esp reg_esp est_alq)

egen double reda=rsum(reda_m reda_nma reda_nme)

gen double redan= -1 * reda
gen double reg_espn = -1 * reg_esp


saveold "$ruta\gtos_autoc10.dta", replace //Mayra Sáenz Julio 2015

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*


use "$ruta\Pobla10.dta", clear //Base nueva
gen str folio= folioviv + foliohog
order folio, first
sort folio numren, stable

merge 1:1 folioviv foliohog numren using "$ruta\trabajos_reshape.dta"
drop _merge

merge 1:1 folio numren using "$ruta\ingreso_deflactado10_per.dta"
rename _merge _merge_ing
sort folio numren, stable

merge 1:1 folioviv foliohog numren using "$ruta\edu_gtosmp"
drop _merge

merge 1:1 folioviv foliohog numren using "$ruta\edu_gtosnmp"
drop _merge

merge m:1 folio using "$ruta\gtos_autoc10.dta"
drop _merge

merge m:1 folio using "$ruta\edu_gtosmh"
drop _merge

merge m:1 folio using "$ruta\edu_costosh"
drop _merge


*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict=rsum(ing_monh nomon)                 if parentesco=="101" | parentesco=="102" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gct=rsum(gasmon nomon)                   if parentesco=="101" | parentesco=="102" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gnt=rsum(gasmon nomon redan reg_espn)    if parentesco=="101" | parentesco=="102" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if parentesco=="101" | parentesco=="102" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas

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

/*


*************************
***    ENIGH 2010     ***		
*** SERIE TRADICIONAL ***
*************************

// MERGE

*nivel de personas, normal
use "$ruta\poblacion.dta",clear
	sort  folioviv foliohog numren
	save, replace
	
*nivel de hogar 
use "$ruta\hogares.dta",clear
	sort  folioviv foliohog 
	save, replace

	
*nivel de personas, (hacer reshape )
use "$ruta\trabajos.dta",clear
	keep folioviv foliohog numren numtrab trapais subor indep personal pago contrato tipocontr htrab cuo scian clas_emp tam_emp no_ing tiene_suel pres_*
	egen per = concat(folioviv foliohog numren)
	rename pres_1 pres_0
	reshape wide trapais subor indep personal pago contrato tipocontr htrab cuo scian clas_emp tam_emp no_ing tiene_suel pres_*, i(per) j(numtrab) string
	rename pres_01 pres_11
	rename pres_02 pres_12
	
	
	foreach var of varlist trapais1 subor1 indep1 personal1 pago1 contrato1 tipocontr1 htrab1 cuo1 scian1 clas_emp1 tam_emp1 no_ing1 tiene_suel1 ///
	pres_11 pres_21 pres_31 pres_41 pres_51 pres_61 pres_71 pres_81 pres_91 pres_101 pres_111 pres_121 pres_131 pres_141 pres_151 pres_161 pres_171 pres_181 pres_191 pres_201 {
	    label var `var' "`var' del primer trabajo"
	}
	foreach var of varlist trapais2 subor2 indep2 personal2 pago2 contrato2 tipocontr2 htrab2 cuo2 scian2 clas_emp2 tam_emp2 no_ing2 tiene_suel2 ///
	pres_12 pres_22 pres_32 pres_42 pres_52 pres_62 pres_72 pres_82 pres_92 pres_102 pres_112 pres_122 pres_132 pres_142 pres_152 pres_162 pres_172 pres_182 pres_192 pres_202 {
	    label var `var' "`var' del segundo trabajo"
	}
		
	drop per
	sort  folioviv foliohog numren
	saveold "trabajos_reshape.dta", replace

*nivel de personas, la clave indica el tipo de ingresos que tiene una persona)
*la persona uno tiene tres tipos de ingresos, hacer un reshape

use "$ruta\ingresos.dta",clear
	sort  folioviv foliohog numren
	egen per = concat(folioviv foliohog numren)
	egen ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)
	drop  mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 ing_tri ing_1 ing_2 ing_3 ing_4 ing_5 ing_6
	rename ing_mens ing_1
	reshape wide ing_1, i(per) j(clave)string
	drop per
	sort  folioviv foliohog  numren
	saveold "ingresos_reshape.dta", replace
	
//MERGE
clear
use "$ruta\poblacion.dta"
merge folioviv foliohog using "$ruta\hogares.dta"
	tab _merge
		drop _merge
	sort  folioviv foliohog numren

merge  folioviv foliohog numren using "$ruta\trabajos_reshape.dta"
	tab _merge
	*Faltan observaciones en using data porque la base trabajos solo contiene a las personas que tienen trabajo no a todas (no todos los miembros del hogar)
	drop _merge
	sort  folioviv foliohog numren
	
merge  folioviv foliohog numren using "$ruta\ingresos_reshape.dta"
	tab _merge
	*Faltan observaciones en using data porque la base ingresos solo contiene a las personas que tienen un ingreso no a todas
	drop _merge
	sort  folioviv foliohog numren	
	
	


saveold "`base_out'", replace


log close



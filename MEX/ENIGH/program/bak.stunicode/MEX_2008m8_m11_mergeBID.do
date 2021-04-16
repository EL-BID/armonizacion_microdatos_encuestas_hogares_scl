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
 
*global ruta = "${surveysFolder}\\survey\MEX\ENIGH\2008\m8_m11\data_orig\STATA"
global ruta = "${surveysFolder}\\survey\MEX\ENIGH\2008\m8_m11\data_orig\"
                           
local PAIS MEX
local ENCUESTA ENIGH
local ANO "2008"
local ronda m8_m11

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (Nueva construcción)
Round: Agosto-Noviembre
Autores: No consta el nombre del autor/a de las versiones anteriores.
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013
*Modificación Mayra Sáenz: 5 de Septiembre de 2014

							SCL/LMK - IADB
****************************************************************************/


*Mayra Sáenz - Agosto 2015: Se realiza el merge con base en la sintaxis de CONEVAL, 
*pero con algunas modificaciones, y generando nuevas variables.

*********************************************************

/*Este programa debe ser utilizado con el Software Stata 
versión 8 o superior. 

Todas las bases de  datos de la ENIGH 2008 pueden ser obtenidas 
en la página de internet del INEGI, www.inegi.gob.mx, y
deben estar convertidas a formato *.dta (Stata)

En este programa se utilizan las siguientes bases, 
renombrándolas de la siguiente forma:

Base de ingresos: Ingresos.dta
Bases de gasto monetario:
				 *G_diario.dta
				 *Gastos.dta
				 *G_educa.dta
Base de gasto no monetario: Nomon.dta
Base de concentrado: concen.dta

En este programa se utilizan tres tipos de archivos, los cuales 
están ubicados en las siguientes carpetas:

1) Bases originales: "${surveysFolder}\pobreza ingresos\2008\ENIGH"
2) Bitácoras: "${surveysFolder}\pobreza ingresos\2008\Log"
3) Bases generadas: "${surveysFolder}\pobreza ingresos\2008\Resultados"


Para cambiar estas ubicaciones, se modifican los siguientes
globals 

gl data="${surveysFolder}\pobreza ingresos\2008\ENIGH"
gl log="${surveysFolder}\pobreza ingresos\2008\Log"
gl bases="${surveysFolder}\pobreza ingresos\2008\Resultados"


log using "$log\Pobreza 2008.txt", text replace

*********************************************************
*
*	PROGRAMA PARA LA MEDICIÓN DE LA POBREZA 2008
*
*********************************************************

CONEVAL Última modificación: 24 de julio del 2013

*********************************************************

*Parte I

*Creación del ingreso monetario deflactado a pesos de 
agosto del 2008.

Los deflactores se crean previamente a partir del INPC 
general (ver Nota Técnica).

En esta parte se crea la base:
	"$ruta\ingreso_deflactado08.dta"
la cual se encuentra a nivel de hogar (folio).*/

********************************************************

*Ingresos
use "$ruta\ingresos.dta", clear
************
gen str folio= folioviv + foliohog
order folio, first

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

*Definición de los deflactores 2008
scalar ene08	=	0.973529049	
scalar feb08	=	0.976423103	
scalar mar08	=	0.983500031	
scalar abr08	=	0.9857381	
scalar may08	=	0.984673088	
scalar jun08	=	0.988747916	
scalar jul08	=	0.994258196	
scalar ago08	=	1	        
scalar sep08	=	1.006814534	
scalar oct08	=	1.013675372	
scalar nov08	=	1.025197567	
scalar dic08	=	1.032297648	



/*La estrategia para deflactar será dividir cada columna 
de ingreso por el deflactor correspondiente a su mes y la
decena en que fue levantado*/

replace ing_6=ing_6/feb08 if mes_6==2
replace ing_6=ing_6/mar08 if mes_6==3
replace ing_6=ing_6/abr08 if mes_6==4
replace ing_6=ing_6/may08 if mes_6==5

replace ing_5=ing_5/mar08 if mes_5==3
replace ing_5=ing_5/abr08 if mes_5==4
replace ing_5=ing_5/may08 if mes_5==5
replace ing_5=ing_5/jun08 if mes_5==6

replace ing_4=ing_4/abr08 if mes_4==4
replace ing_4=ing_4/may08 if mes_4==5
replace ing_4=ing_4/jun08 if mes_4==6
replace ing_4=ing_4/jul08 if mes_4==7

replace ing_3=ing_3/may08 if mes_3==5
replace ing_3=ing_3/jun08 if mes_3==6
replace ing_3=ing_3/jul08 if mes_3==7
replace ing_3=ing_3/ago08 if mes_3==8

replace ing_2=ing_2/jun08 if mes_2==6
replace ing_2=ing_2/jul08 if mes_2==7
replace ing_2=ing_2/ago08 if mes_2==8
replace ing_2=ing_2/sep08 if mes_2==9

replace ing_1=ing_1/jul08 if mes_1==7
replace ing_1=ing_1/ago08 if mes_1==8
replace ing_1=ing_1/sep08 if mes_1==9
replace ing_1=ing_1/oct08 if mes_1==10



/*Se deflacta la clave P008 (Reparto de utilidades) 
con el deflactor de mayo a agosto 2008
y se obtiene el promedio mensual.*/

replace ing_1=(ing_1/may08)/12 if clave=="P008"

/*Una vez realizada la deflactación, se procede a obtener el 
ingreso mensual promedio en los últimos seis meses, para 
cada persona y clave de ingreso*/

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)

*Para obtener el ingreso corriente monetario, se seleccionan las claves de ingreso correspondientes

gen double ing_mon=ing_mens if (clave>="P001" & clave<="P008") | (clave>="P011" & clave<="P013") ///
                             | (clave>="P015" & clave<="P018") | (clave>="P020" & clave<="P045") ///
                             | (clave=="P063") | (clave>="P067" & clave<="P080")

*Modificación Mayra Sáenz - Agosto 2015 - en la sintaxis original no incluía la P008 (Reparto de utilidades del ejercicio 2007)
gen double ing_lab=ing_mens if (clave>="P001" & clave<="P008") | (clave=="P011") | (clave=="P013") ///
                             | (clave=="P015") | (clave>="P017" & clave<="P018") | (clave>="P020" & clave<="P022") ///
                             | (clave=="P063") | (clave>="P067" & clave<="P080")

gen double ing_trab=ing_mens if (clave>="P001" & clave<="P008") | (clave=="P011") | (clave=="P013") ///
                             | (clave=="P015") | (clave>="P017" & clave<="P018") |  (clave=="P063")

gen double ing_negp=ing_mens if (clave>="P067" & clave<="P080") | (clave>="P020" & clave<="P022")

gen double ing_rent=ing_mens if (clave=="P012") | (clave=="P016") | (clave>="P023" & clave<="P031")
gen double ing_tran=ing_mens if (clave>="P032" & clave<="P045")
							 
*Modificación Mayra Sáenz - Agosto 2015 - Se divide al ingreso laboral en principal y secundario

gen double ing_trab1=ing_mens if (clave>="P001" & clave<="P008") | (clave=="P011") | (clave=="P013") 
gen double ing_trab2=ing_mens if (clave=="P015") | (clave>="P017" & clave<="P018") |  (clave=="P063")
gen double ing_negp1=ing_mens if (clave>="P067" & clave<="P073") 
gen double ing_negp2=ing_mens if (clave>="P074" & clave<="P080") | (clave>="P020" & clave<="P022")

*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens  if  (clave=="P032" )                                     //Jubilaciones y pensiones originados dentro del país P032, No se incluyen las provenientes de otros países P033
g double trat_pr  = ing_mens  if  ((clave>="P034" & clave<="P036") | clave=="P041")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por accidentes de trabajo, Indemnizaciones por despido y retiro voluntario ,Ingresos provenientes de otros paises
g double trat_pu  = ing_mens  if  (clave>="P042" & clave<="P045")                      //Desde 1996 aparece PROCAMPO, en 2002 se incluye Beneficio de Oportunidades; en 2008 se incluye programa para adultos mayores y beneficios de otros programas sociales.
g double dona_pu  = ing_mens  if  (clave=="P038")                                     //Becas provenientes del gobierno                      
g double dona_pr  = ing_mens  if  (clave=="P037" | clave=="P039" | clave=="P040")      //Becas y donativos provenientes de organizaciones no gubernamentales; Regalos o donativos en dinero provenientes de otros hogares
g double otros    = ing_mens  if  ((clave>="P046" & clave<="P062"))                    //Otros ingresos no considerados en los anteriores (especifique); ¿cuánto dinero recibió por rendimientos de acciones que posea de alguna empresa en la que no trabajó?; Retiro de inversiones, ahorros, tandas, cajas de ahorro, etc.;  Pagos recibidos de préstamos que usted hizo a personas no miembros del hogar;  Préstamos recibidos de personas no miembros del hogar o instituciones; Venta de monedas, metales preciosos, joyas y obras de arte; Venta de acciones; Venta de bonos; Venta de cédulas; Venta de marcas, patentes y derechos de autor; Herencias, dotes y legados; Loterías y juegos de azar;  Venta de casas, terrenos, condominios, etc. que están dentro del país propiedad de algún miembro del hogar;  Venta de casas, terrenos, condominios, etc.que están fuera del país propiedad de algún miembro del hogar; Venta de maquinaria, equipos, animales de producción, vehículos, etc. utilizados en el negocio propiedad del hogar; Venta de vehículos, aparátos eléctricos de segunda mano, etc.;  Préstamos hipotecarios por bienes inmuebles: casa, terrenos, edificios y locales; Seguro de vida; Otras percepciones financieras de capital no consideradas en las anteriores (especifique).
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

saveold "$ruta\ingreso_deflactado08_per.dta", replace

*********************************************************
/*Parte II

Creación del gasto monetario deflactado a pesos de agosto 
de 2008.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crea la base:
	"$ruta\gastomonetario_def08.dta"
la cual se encuentra a nivel de hogares (folio).*/

*********************************************************

*Gasto Monetario
*En la ENIGH 2008 hay 3 bases de gasto que se utilizan:
* G_diario: que contiene las claves de Alimentos y Transporte Público.
* G_educa: que contiene las claves de Educación (E001 a E007).
* gastos: que contiene todas las demás claves que se utilizan. 

*Se unen las 3 bases de gasto contenidas en la ENIGH 2008

use "$ruta\G_diario.dta", clear
g base = 1

append using "$ruta\gastos.dta"

replace base = 2 if base ==.

append using "$ruta\G_educa.dta"

replace base = 3 if base ==.

label define base 1 "Gto. Diario" 2 " Gtos" 3 "Gto Educa", add modify
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
en la información de la ENIGH2008, se tomarán como periodos 
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

scalar d11w07=	0.992849335	
scalar d11w08=	1.000000000	
scalar d11w09=	1.007547538	
scalar d11w10=	1.013695578	
scalar d11w11=	1.031951707	

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco

scalar d12w07=	0.9932201	
scalar d12w08=	1.000000000	
scalar d12w09=	1.004577412	
scalar d12w10=	1.006356646	
scalar d12w11=	1.009037254	

*Rubro 2 trimestral, Vestido, calzado y accesorios

scalar d2t05	=	0.995665531	
scalar d2t06	=	0.996958756	
scalar d2t07	=	0.999047098	
scalar d2t08	=	1.002728766	

*Rubro 3 mensual, Vivienda

scalar d3m07	=	0.9958319	
scalar d3m08	=	1	
scalar d3m09	=	1.0030142	
scalar d3m10	=	1.0168688	
scalar d3m11	=	1.0365948	


*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar

scalar d42m07	=	0.991289243	
scalar d42m08	=	1	
scalar d42m09	=	1.013674952	
scalar d42m10	=	1.02126174	
scalar d42m11	=	1.027860798	

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar

scalar d42t05	=	0.984148069	
scalar d42t06	=	0.992234399	
scalar d42t07	=	1.001654732	
scalar d42t08	=	1.011645564	

*Rubro 4.1 semestral, Muebles y aparatos dómesticos

scalar d41s02	=	0.992557573	
scalar d41s03	=	0.995164752	
scalar d41s04	=	0.996819338	
scalar d41s05	=	0.998708459	

*Rubro 5.1 trimestral, Salud

scalar d51t05	=	0.996679643	
scalar d51t06	=	0.998782369	
scalar d51t07	=	1.000350038	
scalar d51t08	=	1.00260528	

*Rubro 6.1.1 semanal, Transporte público urbano

scalar d611w07	=	0.980054808	
scalar d611w08	=	1	
scalar d611w09	=	1.016238497	
scalar d611w10	=	1.020556684	
scalar d611w11	=	1.027320837	

*Rubro 6 mensual, Transporte

scalar d6m07	=	0.990822285	
scalar d6m08	=	1	
scalar d6m09	=	1.007629278	
scalar d6m10	=	1.014032711	
scalar d6m11	=	1.019274815	

*Rubro 6 semestral, Transporte

scalar d6s02	=	0.979756067	
scalar d6s03	=	0.985035808	
scalar d6s04	=	0.990415013	
scalar d6s05	=	0.996299612	

*Rubro 7 mensual, Educación y esparcimiento

scalar d7m07	=	0.996539589	
scalar d7m08	=	1	
scalar d7m09	=	1.018093842	
scalar d7m10	=	1.020146628	
scalar d7m11	=	1.023013196	

*Rubro 2.3 mensual, Accesorios y cuidados del vestido

scalar d23m07	=	1.008233945	
scalar d23m08	=	1	
scalar d23m09	=	0.998860856	
scalar d23m10	=	1.007889908	
scalar d23m11	=	1.012568807	

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido

scalar d23t05	=	1.004801223	
scalar d23t06	=	1.00401631	
scalar d23t07	=	1.002364934	
scalar d23t08	=	1.002250255	

*INPC semestral

scalar dINPCs02	=	0.985556739	
scalar dINPCs03	=	0.989486222	
scalar dINPCs04	=	0.993371972	
scalar dINPCs05	=	0.998028184	

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

gen viv_m=gasm if (clave>="G002" & clave<="G022")

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

gen tfor_m=gasm if (clave>="M001" & clave<="M018") | (clave>="F010" & clave<="F017")

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


*Gasto en Educación y recreación deflactado (mensual)

gen edre_m=gasm if (clave>="E001" & clave<="E033") | (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005")


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

gen edu_gtosm=gasm if ((clave>="E001" & clave<="E007") & base ==3)  

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

gen edu_gtosm=gasm if  (((clave>="E008" & clave<="E019") | (clave=="H134") | (clave=="H135")) & base ==2)  

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

gen edu_gtosm=gasm if ((clave>="E001" & clave<="E007") & base ==3)  | (((clave>="E008" & clave<="E019") | (clave=="H134") | (clave=="H135")) & base ==2)  

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

gen cuip_m=gasm if (clave>="D001" & clave<="D026") | (clave=="H132")


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

gen otr_m=gasm if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T914")

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

saveold "$ruta\gastomonetario_def08.dta", replace

*********************************************************

/*Parte III 

Creación del ingreso no monetario deflactado a pesos de 
agosto del 2008.

Los deflactores se crean previamente a partir del INPC según
aparece en la Nota Técnica.

Se crean las bases:
"$ruta\gastonomonetario_def08.dta"
a nivel de clave de gasto, y
"$ruta\auto_def08.dta"
"$ruta\esp_def08.dta"
"$ruta\reg_def08.dta"
a nivel de hogar (folio).*/

*********************************************************

*No Monetario

use "$ruta\Nomon.dta", clear
gen str folio= folioviv + foliohog

/*En el caso de la información de gasto no monetario, para 
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
en la información de la ENIGH2008, se tomarán como periodos 
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

scalar d11w07=	0.992849335	
scalar d11w08=	1.000000000	
scalar d11w09=	1.007547538	
scalar d11w10=	1.013695578	
scalar d11w11=	1.031951707	

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco

scalar d12w07=	0.9932201	
scalar d12w08=	1.000000000	
scalar d12w09=	1.004577412	
scalar d12w10=	1.006356646	
scalar d12w11=	1.009037254	

*Rubro 2 trimestral, Ropa, calzado y accesorios

scalar d2t05	=	0.995665531	
scalar d2t06	=	0.996958756	
scalar d2t07	=	0.999047098	
scalar d2t08	=	1.002728766	

*Rubro 3 mensual, Vivienda

scalar d3m07	=	0.9958319	
scalar d3m08	=	1	
scalar d3m09	=	1.0030142	
scalar d3m10	=	1.0168688	
scalar d3m11	=	1.0365948	

*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar

scalar d42m07	=	0.991289243	
scalar d42m08	=	1	
scalar d42m09	=	1.013674952	
scalar d42m10	=	1.02126174	
scalar d42m11	=	1.027860798	

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar

scalar d42t05	=	0.984148069	
scalar d42t06	=	0.992234399	
scalar d42t07	=	1.001654732	
scalar d42t08	=	1.011645564	

*Rubro 4.1 semestral, Muebles y aparatos dómesticos

scalar d41s02	=	0.992557573	
scalar d41s03	=	0.995164752	
scalar d41s04	=	0.996819338	
scalar d41s05	=	0.998708459	

*Rubro 5.1 trimestral, Salud

scalar d51t05	=	0.996679643	
scalar d51t06	=	0.998782369	
scalar d51t07	=	1.000350038	
scalar d51t08	=	1.00260528	

*Rubro 6.1.1 semanal, Transporte público urbano

scalar d611w07	=	0.980054808	
scalar d611w08	=	1	
scalar d611w09	=	1.016238497	
scalar d611w10	=	1.020556684	
scalar d611w11	=	1.027320837	

*Rubro 6 mensual, Transporte

scalar d6m07	=	0.990822285	
scalar d6m08	=	1	
scalar d6m09	=	1.007629278	
scalar d6m10	=	1.014032711	
scalar d6m11	=	1.019274815	

*Rubro 6 semestral, Transporte

scalar d6s02	=	0.979756067	
scalar d6s03	=	0.985035808	
scalar d6s04	=	0.990415013	
scalar d6s05	=	0.996299612	

*Rubro 7 mensual, Educación y esparcimiento

scalar d7m07	=	0.996539589	
scalar d7m08	=	1	
scalar d7m09	=	1.018093842	
scalar d7m10	=	1.020146628	
scalar d7m11	=	1.023013196	

*Rubro 2.3 mensual, Accesorios y cuidados del vestido

scalar d23m07	=	1.008233945	
scalar d23m08	=	1	
scalar d23m09	=	0.998860856	
scalar d23m10	=	1.007889908	
scalar d23m11	=	1.012568807	

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido

scalar d23t05	=	1.004801223	
scalar d23t06	=	1.00401631	
scalar d23t07	=	1.002364934	
scalar d23t08	=	1.002250255	

*INPC semestral

scalar dINPCs02	=	0.985556739	
scalar dINPCs03	=	0.989486222	
scalar dINPCs04	=	0.993371972	
scalar dINPCs05	=	0.998028184	

*Una vez definidos los deflactores, se seleccionan los rubros de gasto de la metodología del CTMP

gen double gasnomon=apo_tri/3
gen auto=1  if tipogasto==1
gen esp=1 if tipogasto==2
gen reg=1 if tipogasto==3
replace reg=1 if tipogasto==4

*Gasto en Alimentos deflactado (semanal) 

gen ali_nm=gasnomon if (clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247")

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

gen alta_nm=gasnomon if (clave>="A223" & clave<="A241")

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

gen veca_nm=gasnomon if (clave>="H001" & clave<="H122") | (clave=="H136")

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

gen viv_nm=gasnomon if (clave>="G002" & clave<="G022")

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

*Gasto en Cristalería y blancos deflactado (trimestral)

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

*Gasto en Enseres domésticos y muebles deflactado (semestral)

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

*Gasto en Salud deflactado (trimestral)

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

*Gasto en Transporte público deflactado (semanal)

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

*Gasto en Transporte foráneo deflactado (semestral)

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M018") | (clave>="F010" & clave<="F017")

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


*Gasto en Educación y recreación deflactado (mensual)

gen edre_nm=gasnomon if (clave>="E001" & clave<="E033") | (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005")

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

gen edba_nm=gasnomon if (clave>="E002" & clave<="E003") | (clave>="H134" & clave<="H135")

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

gen edu_gtosnm=gasnomon if (clave>="E001" & clave<="E019") | (clave=="H134") | (clave=="H135")   

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

gen cuip_nm=gasnomon if (clave>="D001" & clave<="D026") | (clave=="H132")

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

gen accp_nm=gasnomon if (clave>="H123" & clave<="H131") | (clave=="H133")

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

gen otr_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T914")

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

gen reda_nm=gasnomon if (clave>="T901" & clave<="T914") | (clave=="N013")

replace reda_nm=reda_nm/dINPCs02 if decena==1
replace reda_nm=reda_nm/dINPCs03 if decena==2
replace reda_nm=reda_nm/dINPCs03 if decena==3
replace reda_nm=reda_nm/dINPCs03 if decena==4
replace reda_nm=reda_nm/dINPCs04 if decena==5
replace reda_nm=reda_nm/dINPCs04 if decena==6
replace reda_nm=reda_nm/dINPCs04 if decena==7
replace reda_nm=reda_nm/dINPCs05 if decena==8
replace reda_nm=reda_nm/dINPCs05 if decena==9

saveold "$ruta\gastonomonetario_def08.dta", replace

*Construcción de la base de autoconsumo a partir de la base de gasto no monetario

use "$ruta\gastonomonetario_def08.dta", clear

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

saveold "$ruta\auto_def08.dta", replace

use "$ruta\gastonomonetario_def08.dta", clear

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

saveold "$ruta\esp_def08.dta", replace

use "$ruta\gastonomonetario_def08.dta", clear

*Construcción de base de regalos a partir de la base no monetaria 

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

saveold "$ruta\reg_def08.dta", replace


*Construcción de la base de Estimación del Alquiler
use "$ruta\Hogares.dta", clear

gen str folio= folioviv + foliohog

recode  estim32tri (.=0) (-1=0)
 
sort folio

collapse (sum) estim32tri, by(folio)
gen est_alq= estim32tri/3

sort folio

saveold "$ruta\alq_def08.dta", replace

*********************************************************

/*Parte IV

Cálculo de la incidencia 2008

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
	"$ruta\basefinal08.dta" 
la cual se encuentra a nivel de hogar (folio).*/

*********************************************************

*Genero el identificador en la base de hogares.

use "$ruta\Hogares.dta" , clear
gen str folio= folioviv + foliohog
saveold "$ruta\Hogares_.dta", replace



use "$ruta\Concen.dta", clear

gen str folio= folioviv + foliohog

keep folio estrato factor tam_hog  est_dis upm educacion

sort folio


*Modificado Mayra Sáenz Julio 2015 - Incluyo la base de hogares

merge 1:1 folio using "$ruta\Hogares_.dta" 
tab _merge 
drop _merge 
sort folio

*Modificado Mayra Sáenz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*

merge folio using "$ruta\ingreso_deflactado08.dta"
tab _merge
drop _merge
sort folio*/

merge 1:1 folio using "$ruta\gastomonetario_def08.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\alq_def08.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\esp_def08.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\reg_def08.dta"
tab _merge
drop _merge
sort folio

merge 1:1 folio using "$ruta\auto_def08.dta"
tab _merge
drop _merge
sort folio

destring estrato, replace
gen rururb=1 if (estrato>2 & estrato!=.)
replace rururb=0 if estrato<=2
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


saveold "$ruta\gtos_autoc08.dta", replace //Mayra Sáenz Julio 2015

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*

*Genero la base con variables del trabajo
use "$ruta\trabajos.dta", clear 
forval j = 1/2 {
foreach k of varlist trapais-com_fis {
g `k'_`j' = `k' if numtrab == `j'
}
}

drop trapais-com_fis
gen str folio= folioviv + foliohog

preserve
drop if numtrab == 1
keep folio* numren *_2
sort folio numren, stable

saveold "$ruta\trabajos_secundario.dta", replace
restore

drop if numtrab == 2
drop *_2

sort folio numren, stable


merge 1:1 folio numren using "$ruta\trabajos_secundario.dta"
drop _merge

saveold "$ruta\trabajos_.dta", replace 

*====================================================================================*
* Unión de las bases de personas con las de ingresos deflactados, vivienda y gastos.
*====================================================================================*

use "$ruta\pobla08.dta", clear //Base nueva
gen str folio= folioviv + foliohog
sort folio numren, stable

merge 1:1 folioviv foliohog numren using "$ruta\trabajos_.dta"
drop _merge

merge 1:1 folio numren using "$ruta\ingreso_deflactado08_per.dta"
rename _merge _merge_ing
sort folio numren, stable

merge 1:1 folioviv foliohog numren using "$ruta\edu_gtosmp"
drop _merge

merge 1:1 folioviv foliohog numren using "$ruta\edu_gtosnmp"
drop _merge


merge m:1 folio using "$ruta\gtos_autoc08.dta"
drop _merge

merge m:1 folio using "$ruta\edu_gtosmh.dta"
drop _merge

*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict=rsum(ing_monh nomon)                 if parentesco==101 //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gct=rsum(gasmon nomon)                   if parentesco==101 //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gnt=rsum(gasmon nomon redan reg_espn)    if parentesco==101 //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if parentesco==101 //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas

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

*Generación del factor de expansión para personas

gen factorp=factor*tam_hog



saveold "`base_out'", replace


log close


/*


***ESTABA EN OTRO DO FILE

use "$ruta\ingresos.dta", clear

keep FOLIO* NUMREN CLAVE ING_1


reshape wide ING_1, i(FOLIO* NUMREN) j(CLAVE) string

sort FOLIOVIV FOLIOHOG

saveold "$ruta\ingresosb.dta", replace

use "$ruta\trabajos.dta", clear
ren PERSONAL PERSONAL_CARGO
global LABORAL "TRAB_1 TRAB_2 TRAB_3 TRAB_4 PRES_8 PRES_9 TIPOCONTR CLAS_EMP TRAPAIS SUBOR INDEP PERSONAL_CARGO CONTRATO CMO SCIAN TIPOACT TAM_EMP HTRAB OTRO_TRAB PAGO"
keep $LABORAL FOLIOVIV FOLIOHOG NUMREN NUMTRAB 
                                
reshape wide $LABORAL, i(FOLIOVIV FOLIOHOG NUMREN) j(NUMTRAB)
saveold "$ruta\trabajosb.dta", replace



* MERGE

*************************
***    ENIGH 2008     ***		
*** SERIE ACTUALIZADA ***
*************************
clear
// MERGE

use "$ruta\poblacion.dta"
	sort FOLIOVIV FOLIOHOG
	
merge FOLIOVIV FOLIOHOG using "$ruta\hogares.dta"
	tab _merge
	drop _merge
	sort FOLIOVIV FOLIOHOG 

merge FOLIOVIV FOLIOHOG using "$ruta\concen.dta"
	tab _merge
	drop _merge
	sort FOLIOVIV FOLIOHOG 
	
merge FOLIOVIV FOLIOHOG using "$ruta\gastososb.dta"
	tab _merge
	drop _merge
	sort FOLIOVIV FOLIOHOG 
	
merge 1:1 FOLIOVIV FOLIOHOG NUMREN using "$ruta\trabajosb.dta"
	tab _merge
	drop _merge
	sort FOLIOVIV FOLIOHOG NUMREN
	
merge 1:1 FOLIOVIV FOLIOHOG NUMREN using "$ruta\ingresosb.dta"
	tab _merge
	drop _merge
	sort FOLIOVIV FOLIOHOG NUMREN

/*
merge folio using ubicacion.dta		// BASE DE DATOS NO DISPONIBLE EN LA PAG. DEL INEGI
	tab _merge			// CONTIENE LAS VARIABLES AGEB, UPM, Y OTRAS
	drop if _merge==2  		// RELACIONADAS A LA UBICACION DE LA VIVIENDA
	sort folio num_ren
*/

destring, replace

*saveold mex08B.dta, replace

/* Bases de datos no incluidas
	1. Ingresos.dta
	2. Nomon.dta
	3. Gastos.dta
	4. Eroga.dta */
	
* Nota sobre la actualización de la serie 2000 - 2005 *	

/*

El Instituto Nacional de Estadística, Geografía e Informática (INEGI), en 
un esfuerzo por fortalecer el servicio público de información estadística 
presenta las Bases de Datos de la Encuesta Nacional de Ingresos y Gastos 
de los Hogares para 2000, 2002, 2004, y 2005, Armonizadas de acuerdo con 
la Conciliación Demográfica, con la intención de dar respuesta a los 
requerimientos de aquellos usuarios especializados, con un interés particular 
en el análisis de los microdatos, que permiten un conocimiento más detallado 
del monto, la estructura y la distribución de los ingresos de los hogares y 
del destino de los gastos del hogar en bienes de consumo duradero y no 
duradero. También se obtiene información sobre la infraestructura de las 
viviendas, la composición familiar de los hogares, así como de la actividad 
económica de cada uno de sus miembros. 

Esta encuesta proporciona información a nivel nacional tanto para el conjunto 
de localidades de 2 500 y más habitantes, como para el de aquellas con menos 
de 2 500 habitantes. 

Las cifras que se incluyen para el periodo 2000-2005 han sido sometidas a un 
proceso de armonización acorde con las cifras de la Conciliación Demográfica 
realizada conjuntamente por el Consejo Nacional de Población, El Colegio de 
México y el INEGI. 

A la par del ejercicio de armonización, se llevó a cabo una revisión de la 
información captada por las cuatro últimas ENIGH, con el propósito de uniformarlas. 
Este ejercicio permitió homologar las bases de datos en conjunto, y no una a una 
de manera aislada. En particular, en la ENIGH 2004 se corrigieron pequeños errores 
en la construcción de algunas variables. 

Es importante mencionar que con las nuevas bases de datos 2000-2005 de las ENIGH sí 
es posible analizar la evolución de los ingresos y de los gastos de los hogares 
mexicanos en el periodo. 

Las bases de datos están conformadas por 10 archivos: siete de la base de datos (DBF), 
tres de catálogos (pdf), incluyendo uno con la estructura de la base de datos (pdf). 
La información detallada puede consultarse en la descripción de la base de datos (pdf). 
Para bajar las bases de datos de la ENIGH, sólo tiene que descargarlas en un directorio 
de su equipo y desempacarlo. Para el manejo de las bases de datos, y considerando el 
volumen de información, se recomienda utilizar software como Fox o Access. 

Por medio de esta encuesta y de la difusión de sus resultados, tanto en productos impresos 
y electrónicos, como en bases de datos, el INEGI consolida su compromiso con la población 
de brindar información que contribuya al conocimiento de la realidad sociodemográfica y 
económica del país. 

Conviene señalar que estas bases de datos, en formato de disco compacto, estarán a su 
disposición en los próximos días en los Centros de Información del INEGI 
 

Fuente:
http://www.inegi.gob.mx/est/contenidos/espanol/sistemas/enigh/bd/default.asp
Consultado: Abril, 2007

*/




saveold  "`base_out'", replace


log close

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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENIGH\1992\m8_m11\data_orig"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "1992"
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


clear all
use "$ruta\\ingres92.dta", clear

*Definicion de los deflactores 1992

scalar	ene92= 	0.94505228	  
scalar	feb92= 	0.95624903	  
scalar	mar92= 	0.96598094	  
scalar	abr92= 	0.97459196	  
scalar	may92= 	0.98101788	  
scalar	jun92= 	0.98765799	  
scalar	jul92= 	0.99389409	  
scalar	ago92= 	1.00000000	  
scalar	sep92= 	1.00869879	  
scalar	oct92= 	1.01596171	  
scalar	nov92= 	1.02440306	  
scalar	dic92= 	1.03898979	 

 *La estrategia para deflactar sera dividir cada columna de ingreso por el deflactor correspondiente a su mes

rename ing_mp ing_1
rename mes1 ing_2
rename mes2 ing_3
rename mes3 ing_4
rename mes4 ing_5
rename mes5 ing_6
rename mes_ref meses

replace ing_6=ing_6/ene92 if meses=="060504030201"
replace ing_5=ing_5/feb92 if meses=="060504030201"
replace ing_4=ing_4/mar92 if meses=="060504030201"
replace ing_3=ing_3/abr92 if meses=="060504030201"
replace ing_2=ing_2/may92 if meses=="060504030201"
replace ing_1=ing_1/jun92 if meses=="060504030201"

replace ing_6=ing_6/feb92 if meses=="070605040302"
replace ing_5=ing_5/mar92 if meses=="070605040302"
replace ing_4=ing_4/abr92 if meses=="070605040302"
replace ing_3=ing_3/may92 if meses=="070605040302"
replace ing_2=ing_2/jun92 if meses=="070605040302"
replace ing_1=ing_1/jul92 if meses=="070605040302"

replace ing_6=ing_6/mar92 if meses=="080706050403"
replace ing_5=ing_5/abr92 if meses=="080706050403"
replace ing_4=ing_4/may92 if meses=="080706050403"
replace ing_3=ing_3/jun92 if meses=="080706050403"
replace ing_2=ing_2/jul92 if meses=="080706050403"
replace ing_1=ing_1/ago92 if meses=="080706050403"

replace ing_6=ing_6/abr92 if meses=="090807060504"
replace ing_5=ing_5/may92 if meses=="090807060504"
replace ing_4=ing_4/jun92 if meses=="090807060504"
replace ing_3=ing_3/jul92 if meses=="090807060504"
replace ing_2=ing_2/ago92 if meses=="090807060504"
replace ing_1=ing_1/sep92 if meses=="090807060504"

replace ing_6=ing_6/may92 if meses=="100908070605"
replace ing_5=ing_5/jun92 if meses=="100908070605"
replace ing_4=ing_4/jul92 if meses=="100908070605"
replace ing_3=ing_3/ago92 if meses=="100908070605"
replace ing_2=ing_2/sep92 if meses=="100908070605"
replace ing_1=ing_1/oct92 if meses=="100908070605"

replace ing_6=ing_6/jun92 if meses=="111009080706"
replace ing_5=ing_5/jul92 if meses=="111009080706"
replace ing_4=ing_4/ago92 if meses=="111009080706"
replace ing_3=ing_3/sep92 if meses=="111009080706"
replace ing_2=ing_2/oct92 if meses=="111009080706"
replace ing_1=ing_1/nov92 if meses=="111009080706"


*Mayra Sáenz Julio 2015- Mes de referencia

g mes_ref = substr(meses, 1, 2)
destring mes_ref, replace

*Una vez realizada la deflactacion, se procede a obtener el promedio de ingreso mensual en los ultimos 6 meses para cada persona y clave de ingreso

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)

*Para obtener el ingreso corriente monetario, se seleccionan las claves de ingreso correspondientes

gen double ing_mon =ing_mens if (clave>="P001" & clave<="P027") 
gen double ing_lab =ing_mens if (clave>="P001" & clave<="P014") 

**Modificación Mayra Sáenz - Julio 2015 : Son ingresos laborales de P001-P014, se desagrega por actividad principal y secundaria
gen     ocuprisec = 1 if ocupacion =="1" 
replace ocuprisec = 2 if ocupacion =="0" | ocupacion =="2"

forval j = 1/2 {
gen double ing_trab`j'=ing_mens if (clave>="P001" & clave<="P006") & ocuprisec == `j'
gen double ing_negp`j'=ing_mens if (clave>="P007" & clave<="P014") & ocuprisec == `j'
}
gen double ing_trab=ing_mens if clave>="P001" & clave<="P006" 
gen double ing_negp=ing_mens if clave>="P007" & clave<="P014" 
gen double ing_rent=ing_mens if (clave>="P015" & clave<="P021") 
gen double ing_tran=ing_mens if (clave>="P022" & clave<="P027") 

*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens if  (clave=="P022")                                      //Jubilaciones y pensiones
g double trat_pr = ing_mens  if  ((clave>="P023" & clave<="P024") | clave=="P027")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por despido y accidentes de trabajo, Ingresos provenientes de otros paises
g double trat_pu  = 0
g double dona_pu  = 0                                                                 
g double dona_pr  = ing_mens if  (clave=="P025" | clave=="P026")                      //Becas y donaciones provenientes de Instituciones, Regalos y donativos originados dentro del país
g double otros   = ing_mens  if ((clave>="P028" & clave<="P029"))                     //Venta de automóviles, aparatos electricos de segunda mano, etc.  Otros ingresos corrientes no considerados en los anteriores, (viÃ¡ticos, etc.).
g double remesas = ing_mens  if  (clave=="P027")                                      //Ingresos provenientes de otros paises

*Modificación Mayra Sáenz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}


*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.
*A continuacionn se estima el total de ingresos de cada individuo, y se guarda la base en un archivo aparte

collapse (sum) ing_mens ing_mon ing_lab ing_trab* ing_negp* ing_rent ing_tran ypension trat_pr trat_pu dona_pu dona_pr otros remesas P* Q* T* (mean) mes_ref, by(folio numren)

label var folio     "Identificador del hogar"
label var ing_mon   "Ingreso corriente monetario del hogar"
label var ing_lab   "Ingreso laboral act. princ. y sec"
label var ing_trab1 "Ingreso por remuneraciones al trabajo act. princ."
label var ing_negp1 "Ingresos por negocios propios act. princ."
label var ing_trab2 "Ingreso por remuneraciones al trabajo act. sec."
label var ing_negp2 "Ingresos por negocios propios act. sec."
label var ing_rent  "Ingresos por renta de la propiedad"
label var ing_tran  "Ingresos por transferencias"
label var ypension   "Ingresos por Jubilacion"
label var trat_pr  "Transferencias monetarias y no monetarias privadas" 
label var trat_pu "Transferencias monetarias y no monetarias publicas"
label var dona_pu  "Donaciones Publicas"
label var dona_pr  "Donaciones Privadas"
label var remesas   "Remesas"
label var otros    "Otros"

sort folio

g per = numren
replace per = "99999" if numren ==""

sort folio per, stable
bys folio: gen nper = _n  if per != "99999"

*Asigno el ingreso a nivel de hogar a todos los miembros
foreach x of varlist Q* T* {
bys folio: egen m`x' = max(`x')
replace m`x' = . if per == "99999"
replace `x' = m`x'
drop m`x'
label var `x' "`x' a nivel de hogar se debe dividir para #miembros" 
}

drop if per== "99999"  

*Base a nivel de personas

saveold "$ruta\\ingreso_deflactado92_per.dta", replace


*Parte II
************************************************************************************************************
*Creacion del Gasto Monetario deflactado a pesos de Agosto del 1992.
*Los deflactores se crean previamente a partir del INPC segÃºn aparece en la Nota TÃ©cnica.
*En esta parte se crea la base: "$ruta\gastomonetario_def92.dta" la cual se encuentra a nivel de folio.
************************************************************************************************************

*Gasto monetario

use "$ruta\gasto92.dta", clear

*En el caso de la informacion de gasto, para deflactar se utiliza la decena de levantamiento de la encuesta, la cual esta dada dentro
*del folio de identificacion del hogar en las posiciones septima y octava. En primer lugar se obtiene una variable que identifique 
*la decena de levantamiento

gen decena=real(substr(folio,7,1))
tab decena

*Al comparar con la informaciÃ³n del catÃ¡logo, se observa que la informaciÃ³n se levantÃ³ en nueve decenas, correspondientes a:

/*Decena                           Periodo de Levantamiento

1	 |	Del 21 al 30 de agosto
2	 |	Del 31 de agosto al 9 de septiembre
3	 |	Del 10 al 19 de septiembre
4	 |	Del 20 al 29 de septiembre
5	 |	Del 30 de septiembre al 9 de octubre
6	 |	Del 10 al 19 de octubre
7	 |	Del 20 al 29 de octubre
8	 |	Del 30 de octubre al 8 de noviembre
9	 |	Del 9 al 18 de noviembre

Asi, para realizar el analisis, y conforme a lo establecido en la
informacion de la ENIGH 1992, se tomara como periodos de referencia

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


*Los rubros a considerar, segun la metodologia del CTMP, seran:

Rubro                                  /Periodicidad     /Nombre del deflactor
ali_ms y Bebidas No Alcoholicas        /Semanal          /d11wmes
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
Comunicaciones                         /Mensual          /d6mmes
Educacion basica                       /Mensual          /d7mmes
Articulos y servicios para el cuidado  /Mensual          /d23mmes
personal
Accesorios personales                  /Trimestral       /d23tmesini
Otros gastos diversos y transf.        /Semestral        /dINPCsmesini
Regalos otorgados                      /Semestral        /dINPCsmesini*/


*Rubro 1.1 semanal		

scalar d11w08 =	1.00000000	
scalar d11w09 =	1.00171323	
scalar d11w10 =	1.00635641	
scalar d11w11 =	1.01319565	
		
*Rubro 1.2 semanal		
		
scalar d12w08 =	1.00000000	
scalar d12w09 =	1.02421777	
scalar d12w10 =	1.04625335	
scalar d12w11 =	1.05128173	
		
*Rubro 2 trimestral		
		
scalar d2t05 =	0.98308872	
scalar d2t06 =	0.99144161	
scalar d2t07 =	0.99914746	
scalar d2t08 =	1.00754335	
		
*Rubro 3 mensual		
		
scalar d3m07 = 	0.99116484	
scalar d3m08 = 	1.00000000	
scalar d3m09 = 	1.00754683	
scalar d3m10 = 	1.01698980	
		
*Rubro 4.2 mensual		
		
scalar d42m07 = 	0.99052930	
scalar d42m08 = 	1.00000000	
scalar d42m09 = 	1.00412142	
scalar d42m10 = 	1.00714637	
		
*Rubro 4.2 trimestral		
		
scalar d42t05 = 	0.98593996	
scalar d42t06 = 	0.99255741	
scalar d42t07 = 	0.99821691	
scalar d42t08 = 	1.00375593	
		
*Rubro 4.1 semestral		
		
scalar d41s02 = 	0.96723131	
scalar d41s03 = 	0.97726805	
scalar d41s04 = 	0.98536727	
scalar d41s05 = 	0.99115218	
 		
*Rubro 5.1 trimestral		
		
scalar d51t05 = 	0.97226914	
scalar d51t06 = 	0.98695360	
scalar d51t07 = 	1.00052199	
scalar d51t08 = 	1.01523494	
		
*Rubro 6.1.1 semanal		
		
scalar d611w08 = 	1.00000000	
scalar d611w09 = 	0.99991266	
scalar d611w10 = 	1.00199265	
scalar d611w11 = 	1.00541193	
		
*Rubro 6 mensual		
		
scalar d6m07 =	0.99358703	
scalar d6m08 =	1.00000000	
scalar d6m09 =	1.00305152	
scalar d6m10 =	1.00937980	
		
*Rubro 6 semestral		
		
scalar d6s02 =	0.97446907	
scalar d6s03 =	0.98240052	
scalar d6s04 =	0.98879344	
scalar d6s05 =	0.99497187	
		
*Rubro 7 mensual		
		
scalar d7m07 = 	0.98589700	
scalar d7m08 = 	1.00000000	
scalar d7m09 = 	1.06222821	
scalar d7m10 = 	1.07696475	
		
*Rubro 2.3 mensual		
		
scalar d23m07 = 	0.99235959	
scalar d23m08 = 	1.00000000	
scalar d23m09 = 	1.01527723	
scalar d23m10 = 	1.02373379	
		
*Rubro 2.3 trimestral		
		
scalar d23t05 = 	0.98419079	
scalar d23t06 = 	0.99154548	
scalar d23t07 = 	1.00254561	
scalar d23t08 = 	1.01300367	
		
*INPC semestral		
		
scalar dINPCs02 = 	0.97656532	
scalar dINPCs03 = 	0.98385714	
scalar dINPCs04 = 	0.99097679	
scalar dINPCs05 = 	0.99787174	

*Una vez definidos los deflactores, se seleccionan los rubros de gasto de la metodologÃ­a del CTMP

gen double gasm=gas_tri/3

*Gasto en alimentos deflactado

gen ali_m=gasm if (clave>="A001" & clave<="A189") | (clave>="A199" & clave<="A202")

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

gen alta_m=gasm if (clave>="A190" & clave<="A198") | (clave>="A203" & clave<="A205")

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

gen veca_m=gasm if (clave>="H001" & clave<="H028") | (clave>="H030" & clave<="H055")  

replace veca_m=veca_m/d2t05 if decena==1
replace veca_m=veca_m/d2t06 if decena==2
replace veca_m=veca_m/d2t06 if decena==3
replace veca_m=veca_m/d2t06 if decena==4
replace veca_m=veca_m/d2t07 if decena==5
replace veca_m=veca_m/d2t07 if decena==6
replace veca_m=veca_m/d2t07 if decena==7
replace veca_m=veca_m/d2t08 if decena==8
replace veca_m=veca_m/d2t08 if decena==9

*Gasto en Vivienda y servicios de conservaciÃ³n deflactado

gen viv_m=gasm if (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G029") 

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

*Gasto en Cristaleria y blancos deflactado

gen cris_m=gasm if (clave>="I001" & clave<="I024")

replace cris_m=cris_m/d42t05 if decena==1
replace cris_m=cris_m/d42t06 if decena==2
replace cris_m=cris_m/d42t06 if decena==3
replace cris_m=cris_m/d42t06 if decena==4
replace cris_m=cris_m/d42t07 if decena==5
replace cris_m=cris_m/d42t07 if decena==6
replace cris_m=cris_m/d42t07 if decena==7
replace cris_m=cris_m/d42t08 if decena==8
replace cris_m=cris_m/d42t08 if decena==9

*Gasto en Enseres domÃ©sticos y muebles deflactado

gen ens_m=gasm if (clave>="K001" & clave<="K029")

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

gen sal_m=gasm if (clave>="J001" & clave<="J043")

replace sal_m=sal_m/d51t05 if decena==1
replace sal_m=sal_m/d51t06 if decena==2
replace sal_m=sal_m/d51t06 if decena==3
replace sal_m=sal_m/d51t06 if decena==4
replace sal_m=sal_m/d51t07 if decena==5
replace sal_m=sal_m/d51t07 if decena==6
replace sal_m=sal_m/d51t07 if decena==7
replace sal_m=sal_m/d51t08 if decena==8
replace sal_m=sal_m/d51t08 if decena==9

*Gasto en Transporte pÃºblico deflactado

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

*Gasto en Transporte forÃ¡neo deflactado

gen tfor_m=gasm if (clave>="M001" & clave<="M016") | (clave>="F006" & clave<="F010")

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

replace com_m=com_m/d6m07 if decena==1
replace com_m=com_m/d6m08 if decena==2
replace com_m=com_m/d6m08 if decena==3
replace com_m=com_m/d6m08 if decena==4
replace com_m=com_m/d6m09 if decena==5
replace com_m=com_m/d6m09 if decena==6
replace com_m=com_m/d6m09 if decena==7
replace com_m=com_m/d6m10 if decena==8
replace com_m=com_m/d6m10 if decena==9

*Gasto en Educacion y recreacion deflactado

gen edre_m=gasm if (clave>="E001" & clave<="E025") | (clave=="H029") | (clave>="L001" & clave<="L024") | (clave>="N003" & clave<="N005")

replace edre_m=edre_m/d7m07 if decena==1
replace edre_m=edre_m/d7m08 if decena==2
replace edre_m=edre_m/d7m08 if decena==3
replace edre_m=edre_m/d7m08 if decena==4
replace edre_m=edre_m/d7m09 if decena==5
replace edre_m=edre_m/d7m09 if decena==6
replace edre_m=edre_m/d7m09 if decena==7
replace edre_m=edre_m/d7m10 if decena==8
replace edre_m=edre_m/d7m10 if decena==9

*Gasto en Educacion bÃ¡sica deflactado

gen edba_m=gasm if (clave>="E003" & clave<="E004") | (clave=="E011") | (clave=="E029")

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
gen edu_gtosm=gasm if (clave>="E001" & clave<="E008") | (clave>="E010" & clave<="E013") 

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

gen cuip_m=gasm if (clave>="D001" & clave<="D022") 

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

gen accp_m=gasm if (clave>="H056" & clave<="H064")

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

gen otr_m=gasm if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N015") | (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905")

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

collapse (sum) *_m   edu_gtosm, by(folio)
egen gmontot=rsum(*_m)
rename edu_gtosm edu_gtosmh

sort folio

*Base a nivel de hogar
saveold "$ruta\gastomonetario_def92.dta", replace


*********************************************************

/*Parte III

Creacion del Gasto No Monetario deflactado a pesos de agosto del 1992.

Los deflactores se crean previamente a partir del INPC segÃºn aparece en la Nota TÃ©cnica.

Se crean las bases:
	"$ruta\gastonomonetario_def92.dta"
a nivel de clave de gasto, y
	"$ruta\auto_def92.dta"
	"$ruta\esp_def92.dta"
	"$ruta\reg_def92.dta"
a nivel de folio.

*/
*********************************************************

*No Monetario


use "$ruta\nomon92.dta", clear

/*En el caso de la informacion no meonetario, para deflactar se utiliza 
la decena de levantamiento de la encuesta, la cual esta dada dentro
del folio de identificacion del hogar en las posiciones septima y octava.
 En primer lugar se obtiene una variable que identifique la decena de
levantamiento*/

gen decena=real(substr(folio,7,1))
tab decena

/*Al comparar con la informaciÃ³n del catÃ¡logo, se observa que la 
informaciÃ³n se levantÃ³ en nueve decenas, correspondientes a:

Decena                           Periodo de levantamiento

1	 |	Del 21 al 30 de agosto
2	 |	Del 31 de agosto al 9 de septiembre
3	 |	Del 10 al 19 de septiembre
4	 |	Del 20 al 29 de septiembre
5	 |	Del 30 de septiembre al 9 de octubre
6	 |	Del 10 al 19 de octubre
7	 |	Del 20 al 29 de octubre
8	 |	Del 30 de octubre al 8 de noviembre
9	 |	Del 9 al 18 de noviembre

Asi, para realizar el analisis, y conforme a lo establecido en la
informacion de la ENIGH1992, se tomara como periodos de referencia

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

*DefiniciÃ³n de los deflactores


*Rubro 1.1 semanal		

scalar d11w08 =	1.00000000	
scalar d11w09 =	1.00171323	
scalar d11w10 =	1.00635641	
scalar d11w11 =	1.01319565	
		
*Rubro 1.2 semanal		
		
scalar d12w08 =	1.00000000	
scalar d12w09 =	1.02421777	
scalar d12w10 =	1.04625335	
scalar d12w11 =	1.05128173	
		
*Rubro 2 trimestral		
		
scalar d2t05 =	0.98308872	
scalar d2t06 =	0.99144161	
scalar d2t07 =	0.99914746	
scalar d2t08 =	1.00754335	
		
*Rubro 3 mensual		
		
scalar d3m07 = 	0.99116484	
scalar d3m08 = 	1.00000000	
scalar d3m09 = 	1.00754683	
scalar d3m10 = 	1.01698980	
		
*Rubro 4.2 mensual		
		
scalar d42m07 = 	0.99052930	
scalar d42m08 = 	1.00000000	
scalar d42m09 = 	1.00412142	
scalar d42m10 = 	1.00714637	
		
*Rubro 4.2 trimestral		
		
scalar d42t05 = 	0.98593996	
scalar d42t06 = 	0.99255741	
scalar d42t07 = 	0.99821691	
scalar d42t08 = 	1.00375593	
		
*Rubro 4.1 semestral		
		
scalar d41s02 = 	0.96723131	
scalar d41s03 = 	0.97726805	
scalar d41s04 = 	0.98536727	
scalar d41s05 = 	0.99115218	
 		
*Rubro 5.1 trimestral		
		
scalar d51t05 = 	0.97226914	
scalar d51t06 = 	0.98695360	
scalar d51t07 = 	1.00052199	
scalar d51t08 = 	1.01523494	
		
*Rubro 6.1.1 semanal		
		
scalar d611w08 = 	1.00000000	
scalar d611w09 = 	0.99991266	
scalar d611w10 = 	1.00199265	
scalar d611w11 = 	1.00541193	
		
*Rubro 6 mensual		
		
scalar d6m07 =	0.99358703	
scalar d6m08 =	1.00000000	
scalar d6m09 =	1.00305152	
scalar d6m10 =	1.00937980	
		
*Rubro 6 semestral		
		
scalar d6s02 =	0.97446907	
scalar d6s03 =	0.98240052	
scalar d6s04 =	0.98879344	
scalar d6s05 =	0.99497187	
		
*Rubro 7 mensual		
		
scalar d7m07 = 	0.98589700	
scalar d7m08 = 	1.00000000	
scalar d7m09 = 	1.06222821	
scalar d7m10 = 	1.07696475	
		
*Rubro 2.3 mensual		
		
scalar d23m07 = 	0.99235959	
scalar d23m08 = 	1.00000000	
scalar d23m09 = 	1.01527723	
scalar d23m10 = 	1.02373379	
		
*Rubro 2.3 trimestral		
		
scalar d23t05 = 	0.98419079	
scalar d23t06 = 	0.99154548	
scalar d23t07 = 	1.00254561	
scalar d23t08 = 	1.01300367	
		
*INPC semestral		
		
scalar dINPCs02 = 	0.97656532	
scalar dINPCs03 = 	0.98385714	
scalar dINPCs04 = 	0.99097679	
scalar dINPCs05 = 	0.99787174	

*Una vez definidos los deflactores, se seleccionan los 
*rubros de gasto de la metodologÃ­a del CTMP

drop if (clave=="G002")
drop if (clave>="K030" & clave<"K031")
drop if (clave>="Q001" & clave<="Q015")
drop if (clave>="T001" & clave<="T101")
drop if (clave=="T906")

gen double gasnomon=gas_tri/3
destring tipo_gas, replace
drop if (tipo_gas==4)
gen auto=1 if tipo_gas==1
gen esp=1 if tipo_gas==2
gen reg=1 if tipo_gas==3
gen alq=1 if tipo_gas==0

*Gasto en alimentos deflactado

gen ali_nm=gasnomon if (clave>="A001" & clave<="A189") | (clave>="A199" & clave<="A202")

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

gen alta_nm=gasnomon if (clave>="A190" & clave<="A198") | (clave>="A203" & clave<="A205")

replace alta_nm=alta_nm/d12w08 if decena==1
replace alta_nm=alta_nm/d12w08 if decena==2
replace alta_nm=alta_nm/d12w09 if decena==3
replace alta_nm=alta_nm/d12w09 if decena==4
replace alta_nm=alta_nm/d12w09 if decena==5
replace alta_nm=alta_nm/d12w10 if decena==6
replace alta_nm=alta_nm/d12w10 if decena==7
replace alta_nm=alta_nm/d12w10 if decena==8
replace alta_nm=alta_nm/d12w11 if decena==9

*Gasto en Vestido y calzado deflactado

gen veca_nm=gasnomon if (clave>="H001" & clave<="H028") | (clave>="H030" & clave<="H055") 

replace veca_nm=veca_nm/d2t05 if decena==1
replace veca_nm=veca_nm/d2t06 if decena==2
replace veca_nm=veca_nm/d2t06 if decena==3
replace veca_nm=veca_nm/d2t06 if decena==4
replace veca_nm=veca_nm/d2t07 if decena==5
replace veca_nm=veca_nm/d2t07 if decena==6
replace veca_nm=veca_nm/d2t07 if decena==7
replace veca_nm=veca_nm/d2t08 if decena==8
replace veca_nm=veca_nm/d2t08 if decena==9

*Gasto en Vivienda y servicios de conservaciÃ³n deflactado

gen viv_nm=gasnomon if (clave>="G003" & clave<="G006") | (clave>="G008" & clave<="G009") | (clave>="G011" & clave<="G029") 

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

gen cris_nm=gasnomon if (clave>="I001" & clave<="I024")

replace cris_nm=cris_nm/d42t05 if decena==1
replace cris_nm=cris_nm/d42t06 if decena==2
replace cris_nm=cris_nm/d42t06 if decena==3
replace cris_nm=cris_nm/d42t06 if decena==4
replace cris_nm=cris_nm/d42t07 if decena==5
replace cris_nm=cris_nm/d42t07 if decena==6
replace cris_nm=cris_nm/d42t07 if decena==7
replace cris_nm=cris_nm/d42t08 if decena==8
replace cris_nm=cris_nm/d42t08 if decena==9

*Gasto en Enseres domÃ©sticos y muebles deflactado

gen ens_nm=gasnomon if (clave>="K001" & clave<="K029")

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

gen sal_nm=gasnomon if (clave>="J001" & clave<="J043")

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

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M016") | (clave>="F006" & clave<="F010")

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

gen edre_nm=gasnomon if (clave>="E001" & clave<="E025") | (clave =="H029" ) | (clave>="L001" & clave<="L024") | (clave>="N003" & clave<="N005")

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

gen edba_nm=gasnomon if (clave>="E003" & clave<="E004") | (clave=="E011") | (clave=="E029")

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

gen cuip_nm=gasnomon if (clave>="D001" & clave<="D022")

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

gen accp_nm=gasnomon if (clave>="H056" & clave<="H064")

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

gen otr_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N015") | (clave>="T101" & clave<="T103") | (clave>="T902" & clave<="T905")

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

saveold "$ruta\gastonomonetario_def92.dta", replace

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
*Base a nivel de hogar
saveold "$ruta\auto_def92.dta", replace

use "$ruta\gastonomonetario_def92.dta", clear


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

saveold "$ruta\esp_def92.dta", replace

use "$ruta\gastonomonetario_def92.dta", clear


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
*Base a nivel de hogares
saveold "$ruta\reg_def92.dta", replace

****************

/*Parte IV

Calculo de la Incidencia 1992.

Con el propÃ³sito de obtener cifras representativas a nivel 
nacional, rural y urbano, se obtiene una base con las 
variables de tamaÃ±o del hogar, estrato y factor de 
expansiÃ³n, y a Ã©sta se le unen las bases anteriormente 
generadas. Se estiman los ingresos corriente total y 
corriente neto, se genera el factor de expansiÃ³n para 
personas, y se calcula la incidencia de la pobreza de 
acuerdo con el cÃ¡lculo oficial de la Sedesol.

En esta parte se genera la base "$ruta\basefinal_92.dta" a nivel de folio.*/

****************

use "$ruta\vivi92.dta", clear
*keep folio tam_hog
sort folio
saveold "$ruta\hogares92.dta", replace

use "$ruta\concen.dta", clear
rename folio folio1              //Modificado Mayra Sáenz - Julio 2015
gen folio = substr(folio1,4,100) //Modificado Mayra Sáenz - Julio 2015

rename hog factor
keep folio factor estrato ubica_ge educacion
sort folio

merge folio using "$ruta\hogares92.dta"
tab _merge
drop _merge
sort folio

*Modificado Mayra Sáenz Julio 2015 - Este ingreso es a nivel de hogar, se reemplaza por el ingreso a nivel de persona.
/*merge folio using "$ruta\ingreso_deflactado92.dta"
tab _merge
drop _merge
sort folio*/

merge folio using "$ruta\gastomonetario_def92.dta"
tab _merge
drop _merge
sort folio

merge folio using "$ruta\auto_def92.dta"
tab _merge
drop _merge
sort folio

merge folio using "$ruta\esp_def92.dta"
tab _merge
drop _merge
sort folio

merge folio using "$ruta\reg_def92.dta"
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

egen double gasmon   =rsum(ali_m alta_m veca_m viv_m lim_m ens_m cris_m sal_m tpub_m tfor_m com_m edre_m cuip_m accp_m otr_m)

egen double autocons =rsum( ali_nma alta_nma veca_nma viv_nma lim_nma ens_nma cris_nma sal_nma tpub_nma tfor_nma com_nma edre_nma cuip_nma accp_nma otr_nma)

egen double pago_esp =rsum(ali_nme alta_nme veca_nme viv_nme lim_nme ens_nme cris_nme sal_nme tpub_nme tfor_nme com_nme edre_nme cuip_nme accp_nme otr_nme)

egen double reg_esp =rsum(ali_nmr alta_nmr veca_nmr viv_nmr lim_nmr ens_nmr cris_nmr sal_nmr tpub_nmr tfor_nmr com_nmr edre_nmr cuip_nmr accp_nmr otr_nmr)

egen double nomon   =rsum(autocons pago_esp reg_esp est_alq)

egen double reda    =rsum(reda_m reda_nma reda_nme)

gen double redan= -1 * reda
gen double reg_espn = -1 * reg_esp

saveold "$ruta\gtos_autoc.dta", replace //Mayra Sáenz Julio 2015

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*

use "$ruta\person92.dta", clear

sort folio numren, stable
merge 1:1 folio numren using "$ruta\ingreso_deflactado92_per.dta"
rename _merge _merge_ing
sort folio numren, stable

merge m:1 folio using "$ruta\gtos_autoc.dta"

*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict =rsum(ing_monh nomon) if paren=="1" | paren=="2" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gct =rsum(gasmon nomon)   if paren=="1" | paren=="2" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double intt=rsum(ing_monh nomon redan reg_espn) if paren=="1" | paren=="2" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas
egen double gnt =rsum(gasmon nomon redan reg_espn)   if paren=="1" | paren=="2" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas

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

use "$ruta\vivi92.dta",clear
sort folio
saveold  "$ruta\MEX_1992m8_m11.dta",replace
use "$ruta\person92.dta", clear
sort folio
merge folio using "$ruta\MEX_1992m8_m11.dta"
drop _merge
sort folio numren
gen id=_n
saveold  "$ruta\MEX_1992m8_m11.dta",replace
use "$ruta\ingres92.dta", clear
tab  ocupacion,gen(ocup) /*se genera para saber la numero de ocupacion*/
gen clave1=substr(clave,1,1)
gen clave2=real(substr(clave,2,3))
sort folio numren
by folio numren:egen ing1_ml=sum(ing_mp) if clave1=="P" & clave2>0 & clave2<=6 & ocupa=="1" 
by folio numren:egen ing2_ml=sum(ing_mp) if clave1=="P" & clave2>0 & clave2<=6 & ocupa~="1" 
by folio numren:egen ing1_npm=sum(ing_mp) if clave1=="P" & clave2>6 & clave2<=14 & ocupa=="1" 
by folio numren:egen double ing2_npm=sum(ing_mp) if  clave1=="P" & clave2>6 & clave2<=14 & ocupa~="1" 
/*renta*/
by folio numren:egen ing_rpm=sum(ing_mp) if clave1=="P" & clave2>=15 & clave2<=21 
/*transferencias*/
by folio numren:egen ing_tm=sum(ing_mp) if  clave1=="P" & clave2>=22 & clave2<=26 

*transferencias por jublaciones y/o pensiones*
by folio numren:egen ing_jub=sum(ing_mp) if  clave2==22

/*otros ingresos-no considerados en los anteriores*/
by folio numren:egen ing_otm=sum(ing_mp) if clave1=="P" & clave2>=28 & clave2<=29 
/*remesas*/
by folio numren:egen ing_otrosp=sum(ing_mp) if clave1=="P" & clave2>=27
/*financieras*/
by folio numren:egen ring_otm1=sum(ing_mp) if clave1=="Q" & clave2>=135 &  clave2<=36
replace ring_otm1=0 if ring_otm1==.
by folio numren:egen ing_otm1=max(ring_otm1)
by folio numren:gen cont=(_n) 
drop if cont>1
drop  ocupacion clave ing_mp ing_tri mes2 mes1 mes3 mes4 mes5 cont
sort folio numren
merge folio numren using "$ruta\MEX_1992m8_m11.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge
sort folio numren
saveold  "$ruta\MEX_1992m8_m11.dta",replace
use "$ruta\nomon92.dta", clear
sort folio numren
by folio numren:egen ing_nml=sum(gas_est) 

by folio numren:gen cont=(_n) /*elimino repetidos*/
drop if cont>1
keep folio numren ing_nml
sort folio numren
merge folio numren using "$ruta\MEX_1992m8_m11.dta"
sort id
drop if id==id[_n-1]
drop if _merge==1
drop _merge
	
saveold "`base_out'", replace


log close
/*

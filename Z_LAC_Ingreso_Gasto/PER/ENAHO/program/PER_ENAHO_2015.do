clear
clear all
clear matrix
set more off
set matsize 2000


/*************************************************************************
 *************************************************************************			       	
	            Inter-American Development Bank

1) Elaborado por: Daniela Zuluaga Gordillo - SCL
			       danielazu@iadb.org

2) Fecha: Marzo 2017

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los choques climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático

5) País: Perú

6) Encuesta: Encuesta Nacional de Hogares-ENAHO

7) Ano: 2015

8) Inputs: 	$RAW\enaho01-2015-100.dta
            $RAW\enaho01a-2015-300.dta
			$RAW\enaho01a-2015-500.dta
            $RAW\enaho01-2015-601.dta 
            $RAW\enaho01-2015-604.dta 
			$RAW\enaho01-2015-605.dta 
			$RAW\enaho01-2015-611.dta
			$RAW\sumaria-2015.dta
			
			 
 
6) Outputs: $data_arm\PER_ENAHO_2015.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: Se trabaja con los valores netos y las variables imputadas-deflactadas
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
global RAW    "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\PER\ENAHO\RAW"
global data_arm  "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\PER\ENAHO\data_arm"

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\sumaria-2015.dta" , clear 
	

          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

/*Todas las variables empleadas se encuentran anualizadas, por lo cual al generar 
nuevas variables estas se mensualizan*/


/*A pesar de que el el ingreso monetario neto incluye la variable insedlhd (neta) al calcular el 
ingreso neto total no se utiliza la variable neta insedlhd sino la bruta insedthd que corresponde al
ingreso neto y bruto de la actividad secundaria dependiente respectivamente
Es por esta razón que para el ingreso laboral total se incluye cada una de las variables del ingreso laboral
monetario cambiando insedlhd  por insedthd y se le agrega tambien el ingreso laboral no monetario. */


*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

egen ing_lab_mon = rowtotal(ingnethd ingindhd insedthd ingseihd ingexthd), missing


*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

gen  ing_ren_mon= ingrenhd

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

preserve

**se usa una nueva base de datos para obtener este rubro**

use "$RAW\enaho01a-2015-500.dta" , clear 

drop if p203==8 | p203==9 | p208a<14

forvalues i=1/3 {
gen x`i'= d556`i'c if p556`i'a==1 & p204==1 
}

forvalues i=9/9 {
gen x`i'= d556`i'c if p556`i'a==1 & p204==1 
}

egen  ing_trspri_mon= rowtotal( x1 x2 x3 x9), missing

forvalues i=4/7 {
gen x`i'= d556`i'c if p556`i'a==1 & p204==1 
}

forvalues i=8/8 {
gen x`i'= d556`i'c if p556`i'a==1 & p204==1 
}

egen ing_trsgob_mon= rowtotal( x4 x5 x6 x7 x8), missing

**Para conservar los missing values** Para distinguir missing values de los ceros. *
global missing "ing_trspri_mon ing_trsgob_mon"
foreach var in $missing {
egen `var'_missing= count(`var') if `var'!=.
}
collapse (sum) ing_trspri_mon ing_trsgob_mon ing_trspri_mon_missing ing_trsgob_mon_missing, by(conglome vivienda hogar)
replace ing_trspri_mon=. if ing_trspri_mon_missing==0
replace ing_trsgob_mon=. if ing_trsgob_mon_missing==0

drop ing_trspri_mon_missing ing_trsgob_mon_missing

tempfile transf
save `transf' , replace
restore 

mmerge conglome vivienda hogar using `transf', t(1:1)

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

egen  ing_ct_mon= rowtotal(ingtpu01 ingtpu03 ingtpu04 ingtpu05), missing

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

**se le resta a este rubro las transferencias de los programas sociales**


replace ing_trsgob_mon=ing_trsgob_mon-ing_ct_mon if ing_trsgob_mon!=0


*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= ingtexhd


*------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS *  
*------------------------------------------------*

gen ing_otro_mon=  ingoexhd

*--------------------*
* INGRESO MONETARIO  *  
*--------------------*

**se mensualizan todas las variables creadas, para poder generar el ingreso monetario mensual**

global ingreso_m  "ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon"

foreach var in $ingreso_m {
replace `var'=`var'/12
}

egen  ing_mon= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing


         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*


**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

egen  ing_lab_nomon = rowtotal(pagesphd ingauthd paesechd isecauhd), missing

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

gen ing_ren_nomon= ia01hd 

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

egen  ing_trspri_nomon= rowtotal(ig06hd ig08hd gru13hd2 gru23hd2 gru24hd gru33hd2 gru34hd  gru43hd2 gru44hd gru53hd2 gru54hd gru63hd2 gru64hd gru73hd2 gru74hd gru83hd2 gru84hd gru14hd4 sg42d sg42d1 sg42d2 sg42d3 gru14hd5 gru83hd3 gru73hd3 gru63hd3 gru53hd3 gru43hd3 gru33hd3 gru23hd3 gru13hd3 sig24 sig26), missing 


*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

gen ing_ct_nomon=. 

*-----------------------------------------------------------------------*
* INGRESO NO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*-----------------------------------------------------------------------*

egen ing_trsgob_nomon= rowtotal(gru13hd1 gru23hd1 gru33hd1 gru43hd1 gru53hd1 gru63hd1 gru73hd1 gru83hd1 gru14hd3), missing

*--------------------------------------------------*
* INGRESO NO MONETARIO POR REMESAS INTERNACIONALES * 
*--------------------------------------------------*

gen ing_rem_nomon= .

*------------------------------------------------------------------*
* INGRESO NO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------------------*

gen ing_otro_nomon=.
*----------------------------*
* INGRESO NO MONETARIO TOTAL * 
*----------------------------*

**se mensualizan todas las variables creadas, para poder generar el ingreso no monetario mensual**

global ingreso_nm  "ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_trsgob_nomon"

foreach var in $ingreso_nm {
replace `var'=`var'/12
}

*Para las transferencias privadas**

/*Nota se resta el ingreso imputado ga04hd por estar incluido dentro del grupo de autoconsumo GRU34HD.*/
replace ga04hd=ga04hd/12
gen aux =ing_trspri_nomon-ga04hd 
replace ing_trspri_nomon=aux 
drop aux


egen  ing_nomon= rowtotal( ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon), missing

          *=======================================================*
          * 1.3. INGRESO CORRIENTE TOTAL (MONETARIO+NO MONETARIO) *
          *=======================================================*


**Composicion del ingreso total**

*-----------------------*
* INGRESO LABORAL       * 
*-----------------------*

egen ing_lab= rowtotal( ing_lab_mon ing_lab_nomon), missing

*-----------------------------------*
* INGRESO POR RENTA DE LA PROPIEDAD * 
*-----------------------------------*

egen ing_ren= rowtotal( ing_ren_mon ing_ren_nomon), missing

*---------------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS LOCALES * 
*---------------------------------------------*

egen  ing_trspri= rowtotal( ing_trspri_mon ing_trspri_nomon), missing

*-------------------------------------*
* INGRESO POR REMESAS INTERNACIONALES * 
*-------------------------------------*

egen ing_rem= rowtotal(ing_rem_mon ing_rem_nomon), missing

*-------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS * 
*-------------------------------------*

egen ing_tpriv= rowtotal(ing_trspri ing_rem), missing

*--------------------------------------------------------------*
* INGRESO  POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------*

egen ing_ct= rowtotal(ing_ct_mon ing_ct_nomon), missing

*----------------------------------------------------------*
* INGRESO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*----------------------------------------------------------*

egen ing_trsgob= rowtotal (ing_trsgob_mon ing_trsgob_nomon), missing


*-------------------------------------------*
* INGRESO POR TRANSFERENCIAS DEL GOBIERNO   * 
*-------------------------------------------*

egen ing_tpub= rowtotal(ing_trsgob ing_ct), missing

*------------------------------------------------------*
* INGRESO  POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------*

egen ing_otro= rowtotal(ing_otro_mon ing_otro_nomon), missing

*--------------------------*
* INGRESO CORRIENTE TOTAL  *  
*--------------------------*

egen  ict= rowtotal( ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro), missing


** Etiquetas Variables Monetarias**

label var ing_mon "Ingreso monetario del hogar"
label var ing_lab_mon "Ingreso monetario laboral del hogar"
label var ing_ren_mon "Ingreso monetario por renta de la propiedad del hogar"
label var ing_trspri_mon "Ingreso monetario por transferencias privadas del hogar"
label var ing_ct_mon "Ingreso monetario del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob_mon "Ingreso monetario por transferencias publicas del hogar"
label var ing_rem_mon "Ingreso monetario del hogar por remesas internacionales"
label var ing_otro_mon "Otros ingresos monetarios/ ingresos extraordinarios del hogar" 

** Etiquetas Variables No Monetarias**

label var ing_nomon "Ingreso no monetario del hogar"
label var ing_lab_nomon "Ingreso no monetario laboral del hogar"
label var ing_ren_nomon "Ingreso no monetario por renta de la propiedad del hogar"
label var ing_trspri_nomon "Ingreso no monetario por transferencias privadas del hogar"
label var ing_ct_nomon "Ingreso no monetario del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob_nomon "Ingreso no monetario del hogar por otras transferencias del gobierno"
label var ing_rem_nomon "Ingreso no monetario del hogar por remesas internacionales"
label var ing_otro_nomon "Otros ingresos no monetarios/ ingresos extraordinarios del hogar" 

** Etiquetas Variables Agregadas**

label var ict "Ingreso corriente total"
label var ing_lab "Ingreso laboral del hogar"
label var ing_ren "Ingreso por renta de la propiedad del hogar"
label var ing_trspri "Ingreso por transferencias privadas locales del hogar"
label var ing_rem "Ingreso del hogar por remesas internacionales"
label var ing_tpriv "Ingreso del hogar por transferencias privadas"
label var ing_ct "Ingreso del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob "Ingreso del hogar por otras transferencias del gobierno"
label var ing_tpub "Ingreso del hogar por transferencias del gobierno"
label var ing_otro "Otros ingresos/ ingresos extraordinarios del hogar"  

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	
	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
		  

/*Para los 8 grupos de gasto establecidos por la encuesta, se crea una variable 
g`i' que representa la suma total gastada para dicho grupo*/


forvalues i=1/8{
egen  g`i'= rowtotal(gru`i'1hd gru`i'2hd1 gru`i'2hd2 gru`i'3hd1 gru`i'3hd2 gru`i'3hd3 gru`i'4hd), missing
 }	
 
/*Se renombra cada una de las variables segun el grupo de gasto y se le agregan otros rubros a estas variables cuando
 es necesario*/
 
*-------------------------------------------*
* GASTO EN ALIMENTOS CONSUMIDOS EN EL HOGAR *
*-------------------------------------------*

**Se le agregan a la variable g1 otras variables asociadas al gasto en alimentos consumidos en el hogar**

egen gasto_alihogar=rowtotal(g1 sg23 sig24 gru14hd1 gru14hd2 gru14hd3 gru14hd4 gru14hd5), missing


*-----------------------------------------------*
* GASTO EN ALIMENTOS CONSUMIDOS FUERA DEL HOGAR *
*-----------------------------------------------*

egen gasto_alifuera= rowtotal(g05hd g05hd1 g05hd2 g05hd3 g05hd4 g05hd5 g05hd6 ig06hd sg25 sig26), missing


*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*
preserve

**Se usa la base de datos de gasto en otros bienes y servicios para obtener el gasto en tabaco**

use "$RAW\enaho01-2015-611.dta" , clear 

egen gasto_alta= rowtotal( i611b i611c)if p611n==11

replace gasto_alta=. if i611b==. & i611c==.

keep conglome vivienda hogar gasto_alta 

collapse (sum) gasto_alta , by(conglome vivienda hogar)

tempfile tabaco
save `tabaco' , replace
restore 


**se hace el merge de la base de tabaco con la base consolidada de peru-Sumaria**

mmerge conglome vivienda hogar using `tabaco' , t(1:1) 
	
drop _merge

*------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS *
*------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing


*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

rename g2 gasto_veca

*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*		   

rename g3 gasto_viv


preserve

/*se utiliza la base de datos de gasto en vivienda y servicios de conservacion para obtener
el gasto en Agua, Electricidad, Gas Natural, y combustibles usados en el hogar. 
Estos rubros deben extrarse del grupo 3 (Gasto en vivienda, combustibles
y servicios de conservacion)-variable gasto_viv*/


use "$RAW\enaho01-2015-100.dta" , clear 

forvalues i=1/9{
gen v`i'_gasto= i1172_0`i' if p1175_0`i'==0
gen v`i'_donacion= i1173_0`i'
gen v`i'_autoconsumo= i1174_0`i'
gen v`i'_imputado= i1172_0`i' if p1175_0`i'==3
egen v`i'= rowtotal(v`i'_gasto v`i'_donacion v`i'_autoconsumo v`i'_imputado), missing
 }	
 
 
forvalues i=10 (1) 10 {
gen v`i'_gasto= i1172_`i' if p1175_`i'==0
gen v`i'_donacion= i1173_`i'
gen v`i'_autoconsumo= i1174_`i'
gen v`i'_imputado= i1172_`i' if p1175_`i'==3
egen v`i'= rowtotal(v`i'_gasto v`i'_donacion v`i'_autoconsumo v`i'_imputado), missing
 }	
 
 

 keep conglome vivienda hogar v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 

tempfile combustibles
save `combustibles' , replace
restore 

**se hace el merge de la base de combustibles con la base consolidada de peru-Sumaria**

mmerge conglome vivienda hogar using `combustibles' , t(1:1) 
	
drop if _merge==2
drop _merge


 /*Para obtener el gasto en vivienda y servicios de conservacion (excluyendo
el gasto en agua, electricidad, gas y combustibles) se suman estos rubros y se le
restan a la variable gasto_viv */

egen aux = rowtotal(v1 v2 v3 v4 v5 v6 v7 v8 v9 v10), missing

gen gasto_vcon= gasto_viv-aux

drop aux

/*De esta manera en gasto en vivienda y servicios de conservacion (excluyendo
el gasto en agua, electricidad, gas y combustibles) se representa por la variable viv */

**Se renombran las variables v1....v10 **

*---------------*
* GASTO EN AGUA *
*---------------*	 

rename v1 gasto_vag

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*		  

rename v2 gasto_vele
		  
*-------------------*
* GASTO EN KEROSENE *
*-------------------*	

rename v3 gasto_vk

*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*	

rename v4 gasto_vlp

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*	

rename v5 gasto_vgn

*-----------------*
* GASTO EN CARBON *
*-----------------*	

rename v7 gasto_vca


*---------------*
* GASTO EN LENA *
*---------------*		  

rename v8 gasto_vle

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing


*-------------------*
* GASTO EN PETROLEO *
*-------------------*	

rename v9 gasto_vp

*-------------------*
* GASTO EN GASOLINA *
*-------------------*		  
		  
rename v10 gasto_vgas

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi=.

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing		  
		  
*--------------------------------------------*
* OTROS GASTOS EN COMBUSTIBLES PARA CALENTAR *
*--------------------------------------------*		   
		  
gen gasto_vot= v6


*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

**Se le agrega a la variable g4 otras variables asociadas al gasto en enseres**

egen gasto_ens= rowtotal(g4 sg42 sg421 sg42d sg42d1), missing


*----------------*
* GASTO EN SALUD *
*----------------*		  

rename g5 gasto_sal

*---------------------------------------*
* GASTO EN TRANSPORTE Y COMUNICACIONES  *
*---------------------------------------*

**Es necesario separar el gasto en comunicaciones y transporte**

preserve

**Se usa la base de transporte y comunicaciones**

use "$RAW\enaho01-2015-604.dta" , clear 

**Se obtienen los gastos desagregados en combustibles y comunicaciones**

forvalues i=1/13{
gen  t`i'_gasto= i604b if p604a1==1 & p604n==`i'
gen  t`i'_autoconsumo= i604c if (p604a2==1| p604a3==1) & p604n==`i'
gen  t`i'_pagoespecie= i604c if p604a4==1& p604n==`i'
gen  t`i'_donacionpri= i604c if p604a5==1& p604n==`i'
gen  t`i'_donacionpu= i604c if p604a6==1& p604n==`i'
gen  t`i'_otro= i604c if p604a7==1& p604n==`i'
gen  t`i'_nosabe= i604b if p604a8==1& p604n==`i'
egen t`i'= rowtotal(t`i'_gasto t`i'_autoconsumo t`i'_pagoespecie t`i'_donacionpri t`i'_donacionpu t`i'_otro t`i'_nosabe), missing
 }	

 /*forvalues i=1/13{
egen  t`i'= rowtotal(i604b i604c) if p604n==`i', missing
 }*/

**Para conservar los missing values**
global missi "t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13"
foreach var in $missi {
egen `var'_missi= count(`var') if `var'!=.
}

collapse (sum) t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 (count) t1_missi t2_missi t3_missi t4_missi t5_missi t6_missi t7_missi t8_missi t9_missi t10_missi t11_missi t12_missi t13_missi, by(conglome vivienda hogar)

forvalues i=1/13{
replace t`i'=. if t`i'_missi==0
drop t`i'_missi
}



tempfile transporte
save `transporte' , replace
restore 

**se hace el merge de la base de transporte con la base consolidada de peru-sumaria**

mmerge conglome vivienda hogar using `transporte' , t(1:1) 
	
drop _merge


**Se renombran las variables t1 t2 t3**

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*
rename t1 gasto_tga
 
*----------------------------*
* GASTO EN DIESEL-TRANSPORTE * 
*----------------------------*	

**Se asume que en Peru el petroleo es el mismo diesel**

rename t2 gasto_tdie
		  
*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*			  
  
rename t3 gasto_tman

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

egen gasto_tadq= rowtotal( sg42d2 sg422 ), missing    		  
  

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

preserve

**se usa una nueva base de datos para obtener la variable de gasto en servicios de transporte**

use "$RAW\enaho01a-2015-500.dta" , clear 

drop if p203==8 | p203==9 | p208a<14

forvalues i=1/9 {
gen  tc`i' = i560d`i' if (p204==1) & (p203!=8) & (p203!=9) & (p560c_0`i'==1)
 }
  

  
  forvalues i=10 (1) 10 {
gen  tc`i' = i560d`i' if (p204==1) & (p203!=8) & (p203!=9) & (p560c_`i'==1)
 }


**Para conservar los missing values**
global miss "tc1 tc2 tc3 tc4 tc5 tc6 tc7 tc8 tc9 tc10"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}

collapse (sum) tc1 tc2 tc3 tc4 tc5 tc6 tc7 tc8 tc9 tc10 (count) tc1_miss tc2_miss tc3_miss tc4_miss tc5_miss tc6_miss tc7_miss tc8_miss tc9_miss tc10_miss, by(conglome vivienda hogar)

forvalues i=1/10{
replace tc`i'=. if tc`i'_miss==0
drop tc`i'_miss
}


tempfile serv
save `serv' , replace
restore 

**se hace el merge de la base de transporte con la base consolidada de peru-sumaria**

mmerge conglome vivienda hogar using `serv' , t(1:1) 
	
drop _merge

egen gasto_tserv=rowtotal(t4 t5 t6 t7 t8 t9  tc1 tc2 tc3 tc4 tc5 tc6 tc7)


*-----------------------------*
* OTROS GASTOS EN  TRANSPORTE *
*-----------------------------*

preserve

**se usa una nueva base de datos para obtener la variable de otros gastos en transporte**

use "$RAW\enaho01-2015-605.dta" , clear 

forvalues i=1/1{
gen co`i'_gasto= i605b if p605a1==1 & p605n==`i'
gen co`i'_donacionpri= i605c if p605a2==1& p605n==`i'
gen co`i'_otro= i605c if p605a5==1& p605n==`i'
gen co`i'_nosabe1= i605c if p605a3==1  & p605n==`i'
gen co`i'_nosabe2= i605b if p605a6==1  & p605n==`i'
egen cochera= rowtotal(co`i'_gasto co`i'_donacionpri co`i'_otro co`i'_nosabe1 co`i'_nosabe1), missing
 }	

collapse (sum) cochera (count) nonmiss=cochera, by(conglome vivienda hogar)
replace cochera=. if nonmiss==0
drop nonmiss
tempfile cocher
save `cocher' , replace
restore 

**se hace el merge de la base de transporte con la base consolidada de peru-sumaria**

mmerge conglome vivienda hogar using `cocher' , t(1:1) 
	
drop _merge

gen gasto_totros=cochera

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tdie gasto_tman gasto_totros gasto_tadq), missing



**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*---------------------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO-TRANSPORTE *
*---------------------------------------------*

gen gasto_tlp=.


*---------------------------------*
* GASTO EN GAS ALCOHOL-TRANSPORTE *
*---------------------------------*

gen gasto_talc=.

*------------------------------------------------*
* GASTO EN GAS NATURAL COMPRIMIDO GNC-TRANSPORTE * 
*------------------------------------------------*

gen gasto_tgnc=. 


*----------------------------------------*
* GASTO EN OTROS COMBUSTIBLES-TRANSPORTE * 
*----------------------------------------*

gen gasto_totcomb= . 


*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*

egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb), missing



*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*
preserve

/*se utiliza la base de datos de gasto en vivienda y servicios de conservacion para obtener
el gasto en telefono, television etc*/


use "$RAW\enaho01-2015-100.dta" , clear 

forvalues i=11/14{
gen c`i'_gasto= i1172_`i' if i1172_`i'>0
gen c`i'_autoconsumo= i1174_`i' if i1174_`i'>0
gen c`i'_donacionpri= i1173_`i' if i1173_`i'>0
gen c`i'_nogasto= i1172_`i' if i1172_`i'>0 & p1175_`i'==3
egen c`i'= rowtotal(c`i'_gasto  c`i'_autoconsumo c`i'_donacionpri c`i'_nogasto), missing
 }
 
 
keep c11 c12 c13 c14 conglome vivienda hogar 

tempfile comunicaciones
save `comunicaciones' , replace
restore 

**se hace el merge con la base consolidada de peru-Sumaria**

mmerge conglome vivienda hogar using `comunicaciones' , t(1:1) 
	
drop if _merge==2
drop _merge

preserve

/*se utiliza una nueva base de datos para obtener gastos relacionados con el uso del internet*/


use "$RAW\enaho01a-2015-300.dta" , clear 

forvalues i=1/1{
gen in`i'_gasto= i315a if i315a>0
gen in`i'_autoc= i315b if p3152==1 & i315b>0
gen in`i'_especie= i315b if p3153==1 & i315b>0
gen in`i'_donacionpu= i315b if p3155==1 & i315b>0
gen in`i'_donacionpri= i315b if p3154==1& i315b>0
gen in`i'_otro= i315b if p3156==1& i315b>0
egen internet= rowtotal(in`i'_gasto in`i'_autoc in`i'_especie in`i'_donacionpu in`i'_donacionpri in`i'_otro), missing
 }	
 
 


collapse (sum) internet (count) nonmiss=internet , by (conglome vivienda hogar)
replace internet=. if nonmiss==0
drop nonmiss
tempfile int
save `int' , replace
restore 

**se hace el merge con la base consolidada de peru-Sumaria**

mmerge conglome vivienda hogar using `int' , t(1:1) 
	
drop if _merge==2
drop _merge


egen gasto_com= rowtotal(t10 t11 t12 t13 tc8 tc9 tc10 c11 c12 c13 c14 internet)


*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*		  

rename g7 gasto_edre
	  
		  
*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*

**Se le agrega a la variable g8 otras variables asociadas a otros gastos**

egen otr= rowtotal(g8 ig08hd sg423 sg42d3), missing

**se le resta a esta variable el gasto en tabaco**

gen gasto_otros= otr-gasto_alta
		  
*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

**se mensualizan las variables para poder crear el gasto corriente total mensual**

global gasto  "gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros"

foreach var in $gasto {
replace `var'=`var'/12
}

egen gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros)

**Etiquetas variables de gasto**

label var gct "Gasto corriente total"
label var gasto_ali "Gasto en alimentos, bebidas y tabaco"
label var gasto_alihogar "Gasto en alimentos consumidos en el hogar"
label var gasto_alifuera "Gasto en alimentos consumidos fuera del hogar"
label var gasto_alta "Gasto en alcohol y tabaco"
label var gasto_veca "Gasto en vestido y calzado"
label var gasto_viv "Gasto en vivienda, servicios de conservacion y combustibles"
label var gasto_vcon "Gasto en vivienda y servicios de conservacion"
label var gasto_vk "Gasto en kerosene para uso domestico"
label var gasto_vleca "Gasto en lena y carbon para uso domestico"
label var gasto_vle "Gasto en lena para uso domestico"
label var gasto_vca "Gasto en carbon para uso domestico"
label var gasto_vlp "Gasto en gas licuado de petroleo para uso domestico"
label var gasto_vdi "Gasto en diesel para uso domestico"
label var gasto_vp "Gasto en petroleo para uso domestico"
label var gasto_vgas "Gasto en gasolina para uso domestico"
label var gasto_vpgk "Gasto en petroleo, gasolina y kerosene para uso domestico"
label var gasto_vot "Gasto en otros combustibles para uso domestico"
label var gasto_vele "Gasto en electricidad"
label var gasto_vgn "Gasto en gas natural"
label var gasto_vag "Gasto en agua"
label var gasto_ens "Gasto en muebles, enseres y mantenimiento de la vivienda"
label var gasto_sal "Gasto en salud"
label var gasto_trans "Gasto en transporte"
label var gasto_tserv "Gasto en servicios de transporte"
label var gasto_tga "Gasto en gasolina para transporte"
label var gasto_tdie "Gasto en diesel y gas para transporte"
label var gasto_tgnc "Gasto en gas natural comprimido para transporte"
label var gasto_tcomb "Gasto en combustible para transporte"
label var gasto_tlp "Gasto en gas licuado de petroleo para transporte"
label var gasto_talc "Gasto en alcohol para transporte"
label var gasto_totcomb "Gasto en otros combustibles para transporte"
label var gasto_tman "Gasto en reparacion y conservacion de vehiculos"
label var gasto_tadq "Gasto en adquisicion de vehiculos"
label var gasto_totros "Otros gastos en transporte"
label var gasto_com "Gasto en comunicaciones"
label var gasto_edre "Gasto en educacion y recreacion"
label var gasto_otros "Gasto en otros bienes y servicios"

 
keep conglome vivienda hogar mieperho factor07 gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**

gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "PER"
label var pais "Pais" 
gen anio=2015
label var anio "Anio de la encuesta" 
gen encuesta="ENAHO"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename mieperho miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename factor07 factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros conglome vivienda hogar

		  
saveold "$data_arm\PER_ENAHO_2015.dta" , replace v(12)



		  


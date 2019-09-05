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

2) Fecha: Mayo 2017

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los choques climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático

5) País: Bolivia

6) Encuesta: Encuesta de hogares- EH

7) Ano: 2015

8) Inputs: 	$RAW\eh2015_persona.dta 
            $RAW\eh2015_gastos_alimentarios.dta
			$RAW\eh2015_gastos_noalimentarios.dta
			$RAW\eh2015_gastos_equipamiento.dta
			$RAW\eh2015_vivienda.dta
 
 
6) Outputs: $data_arm\BOL_EH_2015.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\BOL\EH\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\BOL\EH\data_arm"


	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\eh2015_persona.dta" , clear 
	

         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

		 
**Se crea la variable de miembros del hogar a partir de la relación con el jefe de hogar**

gen relacion=.
replace relacion=1 if  s2a_05==1
replace relacion=2 if  s2a_05==2
replace relacion=3 if  s2a_05==3
replace relacion=4 if  s2a_05>=4 &  s2a_05<=9
replace relacion=5 if  s2a_05==10 |  s2a_05==12 
replace relacion=6 if  s2a_05==11

label variable relacion "Relacion con el jefe del hogar"
label define relacion 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion 6 "Empleado/a domestico/a", add
label value relacion relacion_ci
**Los miembros del hogar es la suma de individuos que componen el hogar, excluyenndo empleados domesticos y otros no parientes**

gen miembros= .

replace miembros=1 if relacion<5

/*Para los ingresos del hogar, unicamente se considera la suma de los ingresos de aquellos individuos que conforman el hogar,
de esta manera, no se considera en la suma aquellos ingresos de empleados domesticos u otros no parientes. Se eliminan estas personas de la muestra para practicidad*/

drop if miembros==.

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

**Se mensualizan los rubros correspondientes**


replace s6c_25a=s6c_25a*30 if s6c_25b==1
replace s6c_25a=s6c_25a*4.33 if s6c_25b==2
replace s6c_25a=s6c_25a*2 if s6c_25b==3
replace s6c_25a=s6c_25a/3 if s6c_25b==6
replace s6c_25a=s6c_25a/6 if s6c_25b==7
replace s6c_25a=s6c_25a/12 if s6c_25b==8


replace s6c_27aa=s6c_27aa*30 if s6c_27ab==1
replace s6c_27aa=s6c_27aa*4.33 if s6c_27ab==2
replace s6c_27aa=s6c_27aa*2 if s6c_27ab==3
replace s6c_27aa=s6c_27aa/2 if s6c_27ab==5
replace s6c_27aa=s6c_27aa/3 if s6c_27ab==6
replace s6c_27aa=s6c_27aa/6 if s6c_27ab==7
replace s6c_27aa=s6c_27aa/12 if s6c_27ab==8


replace s6c_27ba=s6c_27ba*30 if s6c_27bb==1
replace s6c_27ba=s6c_27ba*4.33 if s6c_27bb==2
replace s6c_27ba=s6c_27ba*2 if s6c_27bb==3
replace s6c_27ba=s6c_27ba/2 if s6c_27bb==5
replace s6c_27ba=s6c_27ba/3 if s6c_27bb==6
replace s6c_27ba=s6c_27ba/6 if s6c_27bb==7
replace s6c_27ba=s6c_27ba/12 if s6c_27bb==8


replace s6f_41a=s6f_41a*30 if s6f_41b==1
replace s6f_41a=s6f_41a*4.33 if s6f_41b==2
replace s6f_41a=s6f_41a*2 if s6f_41b==3
replace s6f_41a=s6f_41a/3 if s6f_41b==6


replace s6d_33a= s6d_33a*30 if s6d_33b==1
replace s6d_33a= s6d_33a*4.3 if s6d_33b==2
replace s6d_33a= s6d_33a*2 if s6d_33b==3
replace s6d_33a= s6d_33a if s6d_33b==4
replace s6d_33a= s6d_33a/2 if s6d_33b==5
replace s6d_33a= s6d_33a/3 if s6d_33b==6
replace s6d_33a= s6d_33a/6 if s6d_33b==7
replace s6d_33a= s6d_33a/12	if s6d_33b==8

replace s6c_26a=s6c_26a/12
replace s6c_26b=s6c_26b/12

replace s6f_42a1=s6f_42a1/12


egen ing_lab_mon= rowtotal(s6c_25a s6c_26a s6c_26b s6c_27aa s6c_27ba s6d_33a s6f_41a s6f_42a1), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

**se mensualizan los valores correspondientes para este rubro**

replace s7a_3a=s7a_3a/12
replace s7a_3b=s7a_3b/12
replace s7a_3c=s7a_3c/12


egen ing_ren_mon=rowtotal(s7a_2a s7a_2b s7a_2c s7a_3a s7a_3b s7a_3c), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

**se mensualizan los valores correspondientes para este rubro**

replace s7a_4a =s7a_4a/12
replace s7a_4b = s7a_4b/12
replace s7a_4c= s7a_4c/12

replace s7b_5ba=s7b_5ba*4.33 if s7b_5bb==2
replace s7b_5ba=s7b_5ba*2 if s7b_5bb==3
replace s7b_5ba=s7b_5ba/2 if s7b_5bb==5
replace s7b_5ba=s7b_5ba/3 if s7b_5bb==6
replace s7b_5ba=s7b_5ba/6 if s7b_5bb==7
replace s7b_5ba=s7b_5ba/12 if s7b_5bb==8

replace s7b_5aa=s7b_5aa*4.33 if s7b_5ab==2
replace s7b_5aa=s7b_5aa*2 if s7b_5ab==3
replace s7b_5aa=s7b_5aa/2 if s7b_5ab==5
replace s7b_5aa=s7b_5aa/3 if s7b_5ab==6
replace s7b_5aa=s7b_5aa/6 if s7b_5ab==7
replace s7b_5aa=s7b_5aa/12 if s7b_5ab==8

egen ing_trspri_mon= rowtotal(s7a_4a s7a_4b s7a_4c s7b_5ba s7b_5aa), missing

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen ing_ct_mon= s7a_1eb

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

egen ing_trsgob_mon= rowtotal(s7a_1a s7a_1b s7a_1c s7a_1d), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

**se mensualizan los valores correspondientes para este rubro**


replace s7c_08a=s7c_08a*4.33 if s7c_07==2
replace s7c_08a=s7c_08a*2 if s7c_07==3
replace s7c_08a=s7c_08a/2 if s7c_07==5
replace s7c_08a=s7c_08a/3 if s7c_07==6
replace s7c_08a=s7c_08a/6 if s7c_07==7
replace s7c_08a=s7c_08a/12 if s7c_07==8


/*Teniendo en cuenta que en las remesas internacionales se incluyen montos con moneda distinta a los
bolivianos (dolar, euro etc) se utiliza una tasa de cambio para convertir dichos valores a bolivianos*/

/*
    MONEDA

A. BOLIVIANOS 
B. EUROS
C. DԌARES
D. PESOS ARGENTINOS
E. REALES
F. PESOS CHILENOS
G. OTRO

https://www.bcb.gob.bo/?q=cotizaciones_tc
Al  2 DE ENERO DE 2015 
*/


gen ing_rem_mon=.
replace ing_rem_mon =  s7c_08a 			if s7c_08b== "A" /*bolivianos*/
replace ing_rem_mon =  s7c_08a*8.30056   if s7c_08b== "B" /*euro*/
replace ing_rem_mon =  s7c_08a*6.86		if s7c_08b== "C" /*dolar*/
replace ing_rem_mon =  s7c_08a*0.81040   if s7c_08b== "D" /*peso argentino*/
replace ing_rem_mon =  s7c_08a*2.58128   if s7c_08b== "E" /*real*/
replace ing_rem_mon =  s7c_08a*0.01131	if s7c_08b== "F" /*peso chileno*/
replace ing_rem_mon =  s7c_08a*2.30240   if s7c_08b== "G" /*soles*/

*----------------------------------------------------------------*
* INGRESO  MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*----------------------------------------------------------------*

gen ing_otro_mon= .

*--------------------*
* INGRESO MONETARIO  *  
*--------------------*

egen ing_mon= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing


         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*


**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

**se mensualizan los valores correspondientes para este rubro**

replace s6c_30a1a=s6c_30a1a*30 if s6c_30a1b==1
replace s6c_30a1a=s6c_30a1a*4.33 if s6c_30a1b==2
replace s6c_30a1a=s6c_30a1a*2 if s6c_30a1b==3
replace s6c_30a1a=s6c_30a1a/2 if s6c_30a1b==5
replace s6c_30a1a=s6c_30a1a/3 if s6c_30a1b==6
replace s6c_30a1a=s6c_30a1a/6 if s6c_30a1b==7
replace s6c_30a1a=s6c_30a1a/12 if s6c_30a1b==8

replace s6c_30b1a=s6c_30b1a*30 if s6c_30b1b==1
replace s6c_30b1a=s6c_30b1a*4.33 if s6c_30b1b==2
replace s6c_30b1a=s6c_30b1a*2 if s6c_30b1b==3
replace s6c_30b1a=s6c_30b1a/2 if s6c_30b1b==5
replace s6c_30b1a=s6c_30b1a/12 if s6c_30b1b==8

replace s6c_30c1a=s6c_30c1a/2 if s6c_30c1b==5
replace s6c_30c1a=s6c_30c1a/3 if s6c_30c1b==6
replace s6c_30c1a=s6c_30c1a/6 if s6c_30c1b==7
replace s6c_30c1a=s6c_30c1a/12 if s6c_30c1b==8


/*Dado que no hay registros de frecuencia para la variable s6c_30d1a, como la pregunta 
hace referencia a los doce meses anteriores, esta variable se divide en 12 para obtener su
valor mensual*/

replace s6c_30d1a=s6c_30d1a/12

replace s6c_30e1a=s6c_30e1a*30 if s6c_30e1b==1
replace s6c_30e1a=s6c_30e1a*4.33 if s6c_30e1b==2
replace s6c_30e1a=s6c_30e1a/12 if s6c_30e1b==8


replace s6f_42b1=s6f_42b1/12
replace s6f_42c1=s6f_42c1/12

egen ing_lab_nomon = rowtotal( s6c_30a1a s6c_30b1a s6c_30c1a s6c_30d1a s6c_30e1a s6f_42b1 s6f_42c1), missing

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

gen ing_ren_nomon = .

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

gen ing_trspri_nomon = .

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

gen ing_ct_nomon=.


*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*--------------------------------------------------*

gen ing_trsgob_nomon= .

*------------------------------------------------------------------*
* INGRESO NO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------------------*

gen ing_otro_nomon=.


*--------------------------------------------------*
* INGRESO NO MONETARIO POR REMESAS INTERNACIONALES * 
*--------------------------------------------------*

**se mensualizan los valores correspondientes para este rubro**

replace s7c_10=s7c_10*4.33 if s7c_07==2
replace s7c_10=s7c_10*2 if s7c_07==3
replace s7c_10=s7c_10/2 if s7c_07==5
replace s7c_10=s7c_10/3 if s7c_07==6
replace s7c_10=s7c_10/6 if s7c_07==7
replace s7c_10=s7c_10/12 if s7c_07==8

gen ing_rem_nomon=s7c_10



*-----------------------*
* INGRESO NO MONETARIO  * 
*-----------------------*

egen ing_nomon= rowtotal(ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon), missing

          *=======================================================*
          * 1.3. INGRESO CORRIENTE TOTAL (MONETARIO+NO MONETARIO) *
          *=======================================================*


**Composicion del ingreso total**

*-----------------------*
* INGRESO LABORAL       * 
*-----------------------*

egen ing_lab = rowtotal(ing_lab_mon ing_lab_nomon), missing

*-----------------------------------*
* INGRESO POR RENTA DE LA PROPIEDAD * 
*-----------------------------------*

egen ing_ren= rowtotal (ing_ren_mon ing_ren_nomon), missing 

*---------------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS LOCALES * 
*---------------------------------------------*

egen ing_trspri= rowtotal(ing_trspri_mon ing_trspri_nomon), missing

*-------------------------------------*
* INGRESO POR REMESAS INTERNACIONALES * 
*-------------------------------------*

egen ing_rem= rowtotal(ing_rem_mon ing_rem_nomon), missing

*-------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS * 
*-------------------------------------*

egen ing_tpriv= rowtotal(ing_trspri ing_rem), missing

*-------------------------------------------------------------*
* INGRESO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-------------------------------------------------------------*

egen ing_ct= rowtotal(ing_ct_mon ing_ct_nomon), missing

*-------------------------------------------*
* INGRESO POR OTRAS TRANSFERENCIAS PUBLICAS * 
*-------------------------------------------*

egen ing_trsgob= rowtotal(ing_trsgob_mon ing_trsgob_nomon), missing

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

egen ict= rowtotal(ing_mon ing_nomon), missing


**Se guarda la base de datos de ingresos**

**Para conservar los missing values**
global miss "ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_rem_nomon ing_trspri_nomon ing_trsgob_nomon ing_ct_nomon ing_otro_nomon  ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}


collapse (sum) miembros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_rem_nomon ing_trspri_nomon ing_trsgob_nomon ing_ct_nomon ing_otro_nomon  ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count)  ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_rem_nomon_miss ing_trspri_nomon_miss ing_trsgob_nomon_miss ing_ct_nomon_miss ing_otro_nomon_miss   ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(folio)

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}

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


saveold "$RAW\ingresos.dta" , replace v(12)


/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	
	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*


**Se usan las bases ubicadas en la carpeta RAW**


 use "$RAW\eh2015_gastos_noalimentarios.dta", clear
 
 **Se hace el merge con la base de datos de ingresos**
 
 mmerge folio using "$RAW\ingresos.dta", t(1:1)
 
 drop _merge 
 
 **Se mensualizan todas las variables que se utilizaran posteriormente para la composicion del gasto**

forvalues i=1/9 {
replace s8b_11_0`i'= s8b_11_0`i'/3
}

forvalues i=1/9 {
replace s8b_12_0`i'= s8b_12_0`i'/12
}

forvalues i=10/21 {
replace s8b_12_`i'= s8b_12_`i'/12
}


/*Se usa la base de datos de gastos alimentarios para obtener el gasto en alimentos
consumidos dentro del hogar*/ 

 preserve

 use "$RAW\eh2015_gastos_alimentarios.dta", clear
 
*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

**Se mensualizan las variables correspondientes para este rubro**

replace s8a_04=s8a_04*30 if s8a_02==1
replace s8a_04=s8a_04*15 if s8a_02==2
replace s8a_04=s8a_04*8.66 if s8a_02==3
replace s8a_04=s8a_04*4.33 if s8a_02==4
replace s8a_04=s8a_04*2 if s8a_02==5
replace s8a_04=s8a_04/3 if s8a_02==7
replace s8a_04=s8a_04/6 if s8a_02==8
replace s8a_04=s8a_04/12 if s8a_02==9

replace s8a_07=s8a_07*30 if s8a_05==1
replace s8a_07=s8a_07*15 if s8a_05==2
replace s8a_07=s8a_07*8.66 if s8a_05==3
replace s8a_07=s8a_07*4.33 if s8a_05==4
replace s8a_07=s8a_07*2 if s8a_05==5
replace s8a_07=s8a_07/3 if s8a_05==7
replace s8a_07=s8a_07/6 if s8a_05==8
replace s8a_07=s8a_07/12 if s8a_05==9


egen gasto_alihogar= rowtotal(s8a_04 s8a_07 s8a_09), missing

**se consideran las bebidas alcoholicas por separado-codigo de producto 66**

gen bebidas_alcoholicas_hogar=gasto_alihogar if nproducto==66

replace gasto_alihogar=. if nproducto==66

collapse (sum) gasto_alihogar bebidas_alcoholicas_hogar, by(folio)

/*Dado que ninguna observación para la variable bebidas alcoholicas era 0, se 
reemplazan todos los 0 obtenidos despues del collapse (sum) por missing values*/

replace bebidas_alcoholicas_hogar=. if bebidas_alcoholicas_hogar==0

**Se agregan las variables construidas a a la base de vivienda**

	tempfile alimentos
	save `alimentos' , replace
	restore 

mmerge folio using `alimentos' , t(1:1)
drop _merge

/*Se usa la base de datos de gastos no alimentarios para obtener el gasto en alimentos
consumidos fuera  del hogar y el gasto en los otros rubros*/ 


*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

egen gasto_alifuera= rowtotal(s8b_10_15 s8b_10_16 s8b_10_17 s8b_10_18 s8b_10_19 s8b_10_20 s8b_10_22), missing


*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

**se incluye en este rubro las bebidas alcoholicas consumidas dentro y fuera del hogar**

egen gasto_alta=rowtotal (bebidas_alcoholicas_hogar s8b_10_21 s8b_10_11), missing 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

egen gasto_veca= rowtotal(s8b_11_02 s8b_11_03 s8b_11_04 s8b_11_05 s8b_11_06 s8b_11_07 s8b_11_09), missing

*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

preserve

**Se usa la base de vivienda para encontrar el gasto en este rubro**

 use "$RAW\eh2015_vivienda.dta", clear
 
 egen gasto_viv=rowtotal(s1a_03 s1a_13 s1a_19 s1a_25), missing

*---------------*
* GASTO EN AGUA *
*---------------*

gen gasto_vag= s1a_13

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

gen gasto_vele= s1a_19

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*

gen gasto_vgn= s1a_25 if s1a_24 ==4

*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

gen gasto_vlp= s1a_25 if s1a_24 ==3

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle= s1a_25 if s1a_24 ==1


*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

gen gasto_vot= s1a_25 if s1a_24 ==2

**Se crean variables de vivienda, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*-------------------*
* GASTO EN PETROLEO *
*-------------------*	

gen gasto_vp=.

*-------------------*
* GASTO EN GASOLINA *
*-------------------*		  
		  
gen gasto_vgas=.

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi=.

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

gen gasto_vk=.

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing

*----------------*
* GASTO EN CARBON*
*----------------*

gen gasto_vca=.

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 

*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

gen gasto_vcon= s1a_03
 

 keep folio gasto_viv gasto_vag gasto_vele gasto_vgn gasto_vcon gasto_vle gasto_vpgk gasto_vca gasto_vgas gasto_vdi gasto_vk gasto_vleca gasto_vot gasto_vlp s1a_29 gasto_vp
 

	tempfile vivienda
	save `vivienda' , replace
	
**Se agregan las variables construidas **

	restore 

mmerge folio using `vivienda' , t(1:1)
drop _merge


*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

**Se usa la base de equipamiento del hogar para agregar el gasto al rubro de muebles y enseres**

preserve 

 use "$RAW\eh2015_gastos_equipamiento.dta", clear
 
/*Para este rubro se consideran unicamente aquellos equipos (televisor, autos, nevera etc)
que hayan sido adquiridos durante el ultimo ano*/

keep if s8_15==1

**Se mensualizan los rubros correspondientes**

replace s8_16 =s8_16 /12

gen gasto_enseres=.

replace gasto_enseres=s8_16  

**Se crea el gasto en adquisicion de vehiculos y se elimina este rubro del gasto en enseres**

gen gasto_vehiculos= gasto_enseres if item==10 | item==9

replace gasto_enseres=.  if item==10 | item==9
 
**Para conservar los missing values**
global mis "gasto_enseres gasto_vehiculos"
foreach var in $mis {
egen `var'_mis= count(`var') if `var'!=.
}
collapse (sum) gasto_enseres gasto_vehiculos (count) gasto_enseres_mis gasto_vehiculos_mis , by(folio)

foreach var in $mis {
replace `var'=. if `var'_mis==0
drop `var'_mis
}

	tempfile enseres
	save `enseres' , replace
	
**Se agregan las variables construidas **

restore 

mmerge folio using `enseres' , t(1:1)
drop _merge
 
 
 egen gasto_ens= rowtotal(s8b_12_06 s8b_12_07 s8b_10_01 s8b_10_10 gasto_enseres), missing

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans= rowtotal( s8b_10_26 s8b_10_02 s8b_10_03 s8b_12_19 s8b_12_18 gasto_vehiculos), missing


*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*

gen gasto_tcomb=  s8b_10_26

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

egen gasto_tserv= rowtotal(s8b_10_02 s8b_10_03), missing


*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

egen gasto_totros= rowtotal(s8b_12_19), missing

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga=.

*---------------------------------*
* GASTO EN GAS LICUADO-TRANSPORTE * 
*---------------------------------*

gen gasto_tlp=.

*-----------------*
* GASTO EN DIESEL * 
*-----------------*

gen gasto_tdie=.

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

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

gen gasto_tman=s8b_12_18


*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

rename gasto_vehiculos gasto_tadq


*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

egen gasto_com= rowtotal(s8b_11_01  s8b_10_27 s8b_10_28 s8b_10_29 s1a_29), missing 


*----------------*
* GASTO EN SALUD *
*----------------*

gen gasto_sal= s8b_11_08

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

egen gasto_edre= rowtotal(s8b_12_13 s8b_12_14 s8b_12_15 s8b_12_16 s8b_12_17 s8b_10_12 s8b_10_13 s8b_10_14 s8b_10_04 s8b_10_05 s8b_10_06 s8b_12_09 s8b_10_24), missing

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	

egen gasto_otros= rowtotal(s8b_12_21 s8b_10_07 s8b_10_08 s8b_10_09 s8b_12_08 s8b_10_25 s8b_12_20), missing


*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

egen gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros), missing



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

 
keep folio miembros factor gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**
**Dado que esta encuesta se realizó entre el 16 de marzo de 2012 y el 19 de marzo de 2013, por lo cual se escoge el 2012 como el anio de la encuesta, pues tuvo la mayoria de meses.*
gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "BOL"
label var pais "Pais" 
gen anio=2015
label var anio "Anio de la encuesta" 
gen encuesta="EH"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename miembros miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename factor factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros folio

			  
saveold "$data_arm\BOL_EH_2015.dta" , replace v(12)

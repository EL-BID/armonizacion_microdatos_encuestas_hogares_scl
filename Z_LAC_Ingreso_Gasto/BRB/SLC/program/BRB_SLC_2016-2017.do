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

5) País: Barbados

6) Encuesta: Survey of Living Conditions-SLC

7) Ano: 2016-2017

8) Inputs: 	$RAW\RT002_Anonymized.dta 
            $RAW\RT001_Anonymized.dta
            $RAW\RT140_Anonymized.dta
			$RAW\RT011_Anonymized.dta
			$RAW\RT141_Anonymized.dta
 
 
6) Outputs: $data_arm\BRB_SLC_2016-2017.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)

8) Notas: Se trabaja con los valores netos de los ingresos
				  

				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\BRB\SLC\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\BRB\SLC\data_arm"

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\RT002_Anonymized.dta" , clear 
	
**Se crea la variable de miembros del hogar a partir de la relación con el jefe de hogar**

gen relacion=.
replace relacion=1 if  q1_02 ==1
replace relacion=2 if  q1_02 ==2
replace relacion=3 if  q1_02 ==3
replace relacion=4 if  q1_02 >=4 &  q1_02 <=8
replace relacion=5 if  q1_02 ==11 &  q1_02 ==9
replace relacion=6 if  q1_02 ==10


label variable relacion "Relacion con el jefe del hogar"
label define relacion 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 
label define relacion 6 "Empleado/a domestico/a", add
label value relacion relacion_ci

**Los miembros del hogar es la suma de individuos que componen el hogar, excluyenndo empleados domesticos y otros no parientes**

gen miembros= .

replace miembros=1 if relacion<5

/*Para los ingresos del hogar, unicamente se considera la suma de los ingresos de aquellos individuos que conforman el hogar,
de esta manera, no se considera en la suma aquellos ingresos de empleados domesticos u otros no parientes. De esta manera se eliminan de la muestra aquellos "no miembros".*/

drop if miembros==.
         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

egen ing_lab_mon = rowtotal(q10_02b q10_04b ), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

**Se mensualizan las variables correspondientes a este rubro**

replace q10_17=q10_17/12
replace q10_18=q10_18/12
replace q10_19=q10_19/12
replace q10_20=q10_20/12

egen ing_ren_mon= rowtotal(q10_06 q10_17 q10_18 q10_19 q10_20), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

**Se mensualizan las variables correspondientes a este rubro**

replace q10_16 =q10_16 /12

egen ing_trspri_mon= rowtotal(q10_08 q10_09 q10_13 q10_14 q10_16 ), missing

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*
**Se mensualizan las variables correspondientes a este rubro**

replace q4_02_1c=q4_02_1c/3
replace q4_02_2c=q4_02_2c/3
replace q4_02_3c=q4_02_3c/3

 egen ing_ct_mon =rowtotal(q10_11 q4_02_1c q4_02_2c q4_02_3c), missing

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

egen ing_trsgob_mon= rowtotal(q10_07 q10_12 q10_10 ), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= q10_15 

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

**Se mensualizan las variables correspondientes a este rubro**

replace q10_21 =q10_21 /12

gen ing_otro_mon= q10_21 

*--------------------*
* INGRESO MONETARIO  *  
*--------------------*

egen ing_mon= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing


         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*

**Todas las variables incluidas en la encuesta se incluyeron dentro del ingreso monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

gen ing_lab_nomon = .

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

*--------------------------------------------------*
* INGRESO NO MONETARIO POR REMESAS INTERNACIONALES * 
*--------------------------------------------------*

gen ing_rem_nomon= .

*------------------------------------------------------------------*
* INGRESO NO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------------------*

gen ing_otro_nomon=.

*----------------------*
* INGRESO NO MONETARIO * 
*----------------------*

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


**Para conservar los missing values**
global miss "ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}

**Se agregan las variables a nivel de hogar y se guarda la base de datos con las variables relevantes**

collapse (sum) miembros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(questid)


foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}

**Etiquetas de las variables**

label var miembros "Miembros del hogar"


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
	
	use "$RAW\RT001_Anonymized.dta" , clear
	
	**Se hace el merge con la base de ingresos**

mmerge questid using "$RAW\ingresos.dta", t(1:1)
drop _merge
	
	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
		  
preserve

**Se usa la base de alimentos consumidos en el hogar**

 use "$RAW\RT140_Anonymized.dta" , clear 
		  
*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

gen frecuencia=30/q14_02
gen gasto=q14_03d*frecuencia 


bys questid: egen aux=sum(gasto) if foodcode>=1001& foodcode<=1009
bys questid: egen bebidas_alcoholicas_h=max(aux)
drop aux

bys questid: egen aux=sum(gasto) if (foodcode!=1001 & foodcode!=1002 & foodcode!=1003 & foodcode!=1004 & foodcode!=1005 & foodcode!=1006 & foodcode!=1007 & foodcode!=1008 & foodcode!=1009)
bys questid: egen gasto_alihogar=max(aux)
drop aux

**Se deja una sola observacion por hogar y las variables relevantes**
bys questid: gen id=_n
keep if id==1

keep questid bebidas_alcoholicas_h gasto_alihogar

/*Se hace el merge con la base anterior a nivel del hogar*/

tempfile alimentos
	save `alimentos' , replace
	restore 

mmerge questid using `alimentos' , t(1:1)
drop _merge

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*
preserve

**Se utiliza la base de datos de gastos personales**

 use "$RAW\RT011_Anonymized.dta" , clear 
 
 **Se mensualizan las variables correspondientes a este rubro**
 
 replace q11_02=q11_02*4.3

bys questid: egen aux= sum(q11_02) if (perexpco>=3902 & perexpco<=3911) | perexpco==3913 | perexpco==3914 | perexpco==3915
bys questid: egen gasto_alifuera=max(aux)
drop aux

**Se crean igualmente las variables de bebidas alcoholicas y tabaco**

bys questid: egen aux= sum(q11_02) if perexpco==3901 & perexpco==3912
bys questid: egen bebidas_alcoholicas_i=max(aux)
drop aux

bys questid: egen aux= sum(q11_02) if perexpco>=4001 & perexpco<=4005
bys questid: egen tabaco=max(aux)
drop aux

**Se deja una sola observacion por hogar y las variables relevantes**
bys questid: gen id_1=_n
keep if id_1==1

keep questid bebidas_alcoholicas_i gasto_alifuera tabaco

/*Se hace el merge con la base anterior a nivel del hogar*/

tempfile alimentosfuera
	save `alimentosfuera' , replace
	restore 

mmerge questid using `alimentosfuera' , t(1:1)
drop _merge


*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

**se incluye en este rubro las bebidas alcoholicas consumidas dentro y fuera del hogar**

egen gasto_alta=rowtotal (bebidas_alcoholicas_h bebidas_alcoholicas_i tabaco), missing 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing


*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

preserve

**Se utiliza la base de datos de gastos del hogar**

 use "$RAW\RT141_Anonymized.dta" , clear 
 
 **Se mensualizan los valores trimestrales para la variable de gasto**
 
 replace q14_2_=q14_2_/3 if nonfoodc>=2101 & nonfoodc<=2512
 
 **Se mensualizan los valores anuales para la variable de gasto**
 
 replace q14_2_=q14_2_/12 if nonfoodc>=2601 & nonfoodc<=3811
 
 
 **Gasto en vestido y calzado**
 
bys questid: egen aux= sum(q14_2_) if nonfoodc>=2101 & nonfoodc<=2502
bys questid: egen gasto_veca=max(aux)
drop aux



*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc==1101 | nonfoodc==1102 | nonfoodc==1105| nonfoodc==1106 | (nonfoodc>=1201 & nonfoodc<=1210) | nonfoodc==1103
bys questid: egen gasto_viv=max(aux)
drop aux


*---------------*
* GASTO EN AGUA *
*---------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc==1205
bys questid: egen gasto_vag=max(aux)
drop aux

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc==1201
bys questid: egen gasto_vele=max(aux)
drop aux

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc==1202
bys questid: egen gasto_vk=max(aux)
drop aux

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc==1203
bys questid: egen gasto_vgn=max(aux)
drop aux 

*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc==1204
bys questid: egen gasto_vlp=max(aux)
drop aux 


*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc>=1206& nonfoodc<=1210
bys questid: egen gasto_vot=max(aux)
drop aux


*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

bys questid: egen aux= sum(q14_2) if nonfoodc==1101 | nonfoodc==1102 | nonfoodc==1105| nonfoodc==1106 | nonfoodc==1103
bys questid: egen gasto_vcon=max(aux)
drop aux


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

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing

*----------------*
* GASTO EN CARBON*
*----------------*

gen gasto_vca=.

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle=.

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=1301 & nonfoodc<=1508) | nonfoodc==2001 | (nonfoodc>=2701 & nonfoodc<=2707) | (nonfoodc>=2901 & nonfoodc<=3203) | (nonfoodc>=2601 & nonfoodc<=2617) 
bys questid: egen gasto_ens=max(aux)
drop aux

*----------------*
* GASTO EN SALUD *
*----------------*

bys questid: egen aux= sum(q14_2_) if nonfoodc>=1701 & nonfoodc<=1703  
bys questid: egen gasto_sal=max(aux)
drop aux




/*	____________________________________________________________________________
EN ESTE PUNTO, SE CREAN VARIABLES(A NIVEL DE HOGAR _h) QUE SE USARAN MAS ADELANTE 
PARA LA COMPOSICION DE LOS RUBROS DESAGREGADOS DEL GASTO, Y DEBEN SUMARSE CON LAs
VARIABLES QUE SE ENCUENTRAN EN LA BASE DE DATOS DE GASTOS PERSONALES
	____________________________________________________________________________	
*/
	

**OTROS GASTOS**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=1601 & nonfoodc<=1621)| (nonfoodc>=2002 & nonfoodc<=2008) | (nonfoodc>=2503 & nonfoodc<=2512) | (nonfoodc>=3801 & nonfoodc<=3811) | nonfoodc==1104
bys questid: egen gasto_otros_h=max(aux)
drop aux

**GASTOS EDUCACION Y RECREACION**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=1901 & nonfoodc<=1903) | (nonfoodc>=3701 & nonfoodc<=3707) | (nonfoodc>=2801 & nonfoodc<=2809)
bys questid: egen gasto_edre_h=max(aux)
drop aux

**GASTO EN COMUNICACIONES**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=1801 & nonfoodc<=1804) 
bys questid: egen gasto_com_h=max(aux)
drop aux

**GASTO EN MANTENIMIENTO VEHICULOS**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=3301 & nonfoodc<=3307) 
bys questid: egen gasto_tman_h=max(aux)
drop aux


**GASTO EN COMPRA VEHICULOS**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=3401 & nonfoodc<=3405) 
bys questid: egen gasto_tadq_h=max(aux)
drop aux

**OTROS GASTOS EN TRANSPORTE**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=3501 & nonfoodc<=3504) 
bys questid: egen gasto_totros_h=max(aux)
drop aux

**GASTO EN SERVICIOS TRANSPORTE**

bys questid: egen aux= sum(q14_2_) if (nonfoodc>=3601 & nonfoodc<=3602) 
bys questid: egen gasto_tserv_h=max(aux)
drop aux

**Se deja una sola observacion por hogar y las variables relevantes**
bys questid: gen id=_n
keep if id==1

keep questid gasto_veca gasto_vp gasto_vleca gasto_vle gasto_vpgk gasto_vdi gasto_vca gasto_vgas gasto_viv gasto_vag gasto_vk gasto_vgn gasto_vele gasto_vot gasto_vcon gasto_vlp gasto_ens gasto_sal gasto_otros_h gasto_edre_h gasto_com_h gasto_tman_h gasto_tadq_h gasto_totros_h gasto_tserv_h

/*Se hace el merge con la base anterior a nivel del hogar*/

tempfile gastos_hogar
	save `gastos_hogar' , replace
	restore 

mmerge questid using `gastos_hogar' , t(1:1)
drop _merge

/*	____________________________________________________________________________
EN ESTE PUNTO, SE CREAN VARIABLES(A NIVEL DE INDIVIDUOS PERTENECIENTES AL MISMO HOGAR _i) 
QUE SE USARAN MAS ADELANTE PARA AGREGARLAS CON LAS VARIABLES PREVIAMENTE CONSTRUIDAS.
	____________________________________________________________________________	
*/
	
preserve

**Se utiliza la base de datos de gastos personales**

 use "$RAW\RT011_Anonymized.dta" , clear 

 **Se mensualizan las variables correspondientes a este rubro**
 
 replace q11_02=q11_02*4.3
 
 
 **OTROS GASTOS EN TRANSPORTE**

bys questid: egen aux= sum(q11_02) if (perexpco>=4104 & perexpco<=4105) 
bys questid: egen gasto_totros_i=max(aux)
drop aux

**GASTO EN SERVICIOS TRANSPORTE**

bys questid: egen aux= sum(q11_02) if (perexpco>=4201 & perexpco<=4204) 
bys questid: egen gasto_tserv_i=max(aux)
drop aux
 
**GASTO EN MANTENIMIENTO VEHICULOS**

bys questid: egen aux= sum(q11_02) if perexpco==4103 
bys questid: egen gasto_tman_i=max(aux)
drop aux

**GASTO EN DIESEL-TRANSPORTE**

bys questid: egen aux= sum(q11_02) if perexpco==4101
bys questid: egen gasto_tdie_i=max(aux)
drop aux

**GASTO EN GASOLINA-TRANSPORTE**

bys questid: egen aux= sum(q11_02) if perexpco==4102
bys questid: egen gasto_tga_i=max(aux)
drop aux
 
**GASTOS EDUCACION Y RECREACION**

bys questid: egen aux= sum(q11_02) if (perexpco>=4301 & perexpco<=4309) 
bys questid: egen gasto_edre_i=max(aux)
drop aux

**GASTO EN COMUNICACIONES**

bys questid: egen aux= sum(q11_02) if (perexpco>=4401 & perexpco<=4402) 
bys questid: egen gasto_com_i=max(aux)
drop aux
 
 
**Se deja una sola observacion por hogar y las variables relevantes**
bys questid: gen id_1=_n
keep if id_1==1

keep questid gasto_edre_i gasto_com_i gasto_tman_i  gasto_totros_i gasto_tserv_i gasto_tga_i gasto_tdie_i

/*Se hace el merge con la base anterior a nivel del hogar*/

tempfile gastos_individuos
	save `gastos_individuos' , replace
	restore 

mmerge questid using `gastos_individuos' , t(1:1)
drop _merge
 
 
 
*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans=rowtotal(gasto_tga_i gasto_tdie_i gasto_tserv_h gasto_tserv_i gasto_totros_h gasto_totros_i gasto_tman_h gasto_tman_i gasto_tadq_h), missing


*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga= gasto_tga_i

*-----------------*
* GASTO EN DIESEL * 
*-----------------*

gen gasto_tdie= gasto_tdie_i

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

egen gasto_tserv=rowtotal(gasto_tserv_h gasto_tserv_i), missing

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

egen gasto_totros= rowtotal(gasto_totros_h gasto_totros_i), missing


*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

egen gasto_tman=rowtotal(gasto_tman_h gasto_tman_i), missing

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

gen gasto_tadq= gasto_tadq_h

**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**



*---------------------------------*
* GASTO EN GAS LICUADO-TRANSPORTE * 
*---------------------------------*

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

 egen gasto_com= rowtotal(gasto_com_h gasto_com_i), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

egen gasto_edre= rowtotal(gasto_edre_h gasto_edre_i), missing

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	

gen gasto_otros=gasto_otros_h


*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

egen gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros)

*Etiquetas variables de gasto**

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

**Se genera una variable que permita eliminar las encuestas/ observaciones de hogares que no se completaron en ninguna de las visitas**

gen encuesta_completa=0

replace encuesta_completa=1 if vis1_res==1 | vis2_res==1 | vis3_res==1| vis4_res==1 | vis5_res==1 | vis6_res==1| vis7_res==1

drop if encuesta_completa==0
 
keep questid weight miembros gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**
**La encuesta fue realizada en su mayoría durante el 2016.*
gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "BRB"
label var pais "Pais" 
gen anio=2016
label var anio "Anio de la encuesta" 
gen encuesta="SLC"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename miembros miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename weight factor_expansion
label var factor_expansion "Factor de Expansion" 



order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros questid

		  
saveold "$data_arm\BRB_SLC_2016-2017.dta" , replace v(12)





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

2) Fecha: Abril 2017

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los choques climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático

5) País: Argentina

6) Encuesta: Encuesta Nacional de Gastos de los Hogares -ENGHO

7) Ano: 2012-2013

8) Inputs: 	$RAW\ingresos.dta 
            $RAW\hogares.dta 
			$RAW\gastos.dta 
            
 
 
6) Outputs: $data_arm\ARG_ENGHO_2012-2013.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\ARG\ENGHO\RAW"
	global data_arm  "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\ARG\ENGHO\data_arm"

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\ingresos.dta" , clear 
	

         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

egen ing_lab_mon = rowtotal(ingocpal ingocsec ingocant), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

gen ing_ren_mon= irentas

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

gen ing_trspri_mon= itransfermon

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen ing_ct_mon=.

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

gen ing_trsgob_mon= ijubilacion

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= .

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

gen ing_otro_mon=.

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

gen ing_lab_nomon = iautoconsumo

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

**se agregan las variables a nivel de hogar**

**Para conservar los missing values**
global miss "ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}


collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(clave) 

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


saveold "$RAW\ingresos_hogar.dta" , replace v(12)

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	use "$RAW\hogares.dta" , clear 
	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*

**Se hace el merge con la base de ingresos**

mmerge clave using "$RAW\ingresos_hogar.dta", t(1:1)

 replace ict=0 if ict==.		  
		  
preserve

** Se utiliza la base de gastos para desagregar la informacion del gasto en alimentos**

use "$RAW\gastos.dta" , clear 		  


*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

bys clave: egen aux= sum(monto) if division==100000 & grupo==110000
bys clave: egen alimentos_hogar= max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if  grupo==120000 & clase==121000
bys clave: egen bebidas_hogar= max(aux) 
drop aux 

egen gasto_alihogar=rowtotal( alimentos_hogar bebidas_hogar), missing

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

bys clave: egen aux= sum(monto) if division==100000 & grupo==130000
bys clave: egen gasto_alifuera = max(aux) 
drop aux 

**gasto en bebidas alcoholicas consumidas fuera del hogar**

bys clave: egen aux= sum(monto) if division==100000 & (articulo==131114 | articulo==131115 | articulo==131116)
bys clave: egen alcohol = max(aux) 
drop aux  

/*Se le resta a este rubro el gasto en bebidas alcoholicas para incluirlo en la siguiente
categoría*/

bys clave:replace gasto_alifuera= gasto_alifuera-alcohol if alcohol!=.



*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

bys clave: egen aux= sum(monto) if grupo==120000 & clase==122000
bys clave: egen bebidas_alcoholicas= max(aux) 
drop aux 

bys clave: egen aux=sum( monto) if  division==900000 & grupo==910000
bys clave: egen tabaco = max(aux) 
drop aux  

egen gasto_alta=rowtotal(alcohol tabaco bebidas_alcoholicas), missing

**se deja una observacion por cada hogar**

bys clave:gen id=_n
keep if id==1

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

keep gasto_ali gasto_alifuera gasto_alihogar clave gasto_alta tabaco
	tempfile alimentos
	save `alimentos' , replace
	restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge clave using `alimentos' , t(1:1)
drop _merge


*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

rename gc_2 gasto_veca

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*

rename gc_3 gasto_viv

/*se desagrega la variable gasto_viv para obtener los gastos específicos en agua,
electricidad y combustibles*/

preserve
use "$RAW\gastos.dta" , clear 		

*---------------*
* GASTO EN AGUA *
*---------------*

bys clave: egen aux= sum(monto) if clase==331000 & subclase==331100
bys clave: egen gasto_vag= max(aux) 
drop aux 


*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

bys clave: egen aux= sum(monto) if clase==341000 & subclase==341100
bys clave: egen gasto_vele= max(aux) 
drop aux 

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*

**Se incluye en esta categoría el gas envasado en tubo, garrafa y granel**

bys clave: egen aux= sum(monto) if subclase==342100 & (articulo==342103 | articulo==342101 | articulo==342104 | articulo==342102)
bys clave: egen gasto_vgn= max(aux) 
drop aux 

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

bys clave: egen aux= sum(monto) if subclase==343100 & articulo==343101
bys clave: egen gasto_vk= max(aux) 
drop aux 
 

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

bys clave: egen aux= sum(monto) if subclase==343100 & articulo==343102
bys clave: egen gasto_vleca= max(aux) 
drop aux 


*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

bys clave: egen aux= sum(monto) if subclase==343100 & articulo==343103
bys clave: egen gasto_vot= max(aux) 
drop aux 


**se deja una observacion por cada hogar**

bys clave:gen id=_n
keep if id==1

keep clave gasto_vag gasto_vele gasto_vgn  gasto_vleca gasto_vk gasto_vot
	
tempfile servicios
save `servicios' , replace
restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge clave using `servicios' , t(1:1)
drop _merge

*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

**se crea la variable gasto_vcon**

egen aux= rowtotal(gasto_vag gasto_vele gasto_vgn  gasto_vleca gasto_vk gasto_vot), missing
gen gasto_vcon= gasto_viv-aux 
replace gasto_vcon=gasto_viv if aux==.
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

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing

*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

gen gasto_vlp=.

*-----------------*
* GASTO EN CARBON *
*-----------------*

gen gasto_vca=.

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle=.

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi=.
 
*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

rename gc_4 gasto_ens

*----------------*
* GASTO EN SALUD *
*----------------*

rename  gc_5 gasto_sal

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

**Se separa el gasto en transporte y comunicaciones haciendo uso de la base de gastos**

preserve
use "$RAW\gastos.dta" , clear 	
	
bys clave: egen aux= sum(monto) if division==600000 & grupo==610000
bys clave: egen gasto_trans= max(aux) 
drop aux 
replace gasto_trans=0 if gasto_trans==.

/*se desagrega el gasto en transporte para obtener el gasto en rubros específicos*/


*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611405
bys clave: egen gasolina1= max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611406
bys clave: egen gasolina2= max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611407
bys clave: egen gasolina3= max(aux) 
drop aux 

egen gasto_tga= rowtotal(gasolina1 gasolina2 gasolina3), missing

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611403
bys clave: egen diesel1= max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611404
bys clave: egen diesel2= max(aux) 
drop aux 

egen gasto_tdie= rowtotal(diesel1 diesel2), missing

*------------------------------------------------*
* GASTO EN GAS NATURAL COMPRIMIDO GNC-TRANSPORTE * 
*------------------------------------------------*

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611402
bys clave: egen gasto_tgnc= max(aux) 
drop aux 

*-----------------------------*
* GASTO EN OTROS COMBUSTIBLES * 
*-----------------------------*

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611408
bys clave: egen gasto_totcomb= max(aux) 
drop aux 


*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

bys clave: egen aux= sum(monto) if grupo==610000 & clase==612000
bys clave: egen gasto_tserv= max(aux) 
drop aux 

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

bys clave: egen aux= sum(monto) if clase==611000 & subclase==611600
bys clave: egen otros1=max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if clase==611000 & subclase==611700
bys clave: egen otros2=max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if articulo>=611111 & articulo<=611117
bys clave: egen otros3= max(aux) 
drop aux 


egen gasto_totros=rowtotal(otros1 otros2 otros3), missing


**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**


*---------------------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO-TRANSPORTE *
*---------------------------------------------*

gen gasto_tlp=.


*---------------------------------*
* GASTO EN GAS ALCOHOL-TRANSPORTE *
*---------------------------------*

gen gasto_talc=.

*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*

egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb), missing


*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

bys clave: egen aux= sum(monto) if clase==611000 & subclase==611500
bys clave: egen mantenimiento1= max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if clase==611000 & subclase==611300
bys clave: egen mantenimiento2= max(aux) 
drop aux 

bys clave: egen aux= sum(monto) if subclase==611400 & articulo==611401
bys clave: egen mantenimiento3= max(aux) 
drop aux 

egen gasto_tman=rowtotal(mantenimiento1 mantenimiento2 mantenimiento3 ), missing
 
*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

bys clave: egen aux= sum(monto) if articulo>=611101 & articulo<=611107
bys clave: egen gasto_tadq= max(aux) 
drop aux 

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

bys clave: egen aux= sum(monto) if division==600000 & grupo==620000
bys clave: egen gasto_com=max(aux) 
drop aux 
replace gasto_com=0 if gasto_com==.


**se deja una observacion por cada hogar**

bys clave:gen id=_n
keep if id==1

keep clave gasto_trans gasto_com gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_totcomb gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp	gasto_talc	gasto_totcomb
	
tempfile transporte
save `transporte' , replace
restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge clave using `transporte' , t(1:1)
drop _merge


*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

egen gasto_edre= rowtotal(gc_8 gc_7), missing

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	



**Se le resta a este grupo el rubro de tabaco que fue incluido previamente en gasto en bebidas alcoholicas y tabaco**

gen gasto_otros=gc_9-tabaco
replace gasto_otros=gc_9 if tabaco==.


*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

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

 
keep clave cantmiem expan gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub

**Se generan variables de identificacion del hogar, pais, anio y encuesta**
**Dado que esta encuesta se realizó entre el 16 de marzo de 2012 y el 19 de marzo de 2013, por lo cual se escoge el 2012 como el anio de la encuesta, pues tuvo la mayoria de meses.*
gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "ARG"
label var pais "Pais" 
gen anio=2012
label var anio "Anio de la encuesta" 
gen encuesta="ENGHO"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename cantmiem miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename expan factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros clave

		  
saveold "$data_arm\ARG_ENGHO_2012-2013.dta" , replace v(12)


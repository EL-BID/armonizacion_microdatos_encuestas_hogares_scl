clear
clear all
clear matrix
set more off
set matsize 2000


/*************************************************************************
 *************************************************************************			       	
	            Inter-American Development Bank

1) Elaborado por: Laura Di Capua
			      lauradicapua@gmail.com

2) Fecha: Agosto 2018

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los shocks climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático			 
5) País: Uruguay

6) Encuesta: Encuesta Nacional de Gastos e Ingresos de los Hogares - ENGIH

7) Ano: 2005-2006					

8) Inputs: 	$RAW\mhogar.dta 
            $RAW\persona.dta
			$RAW\pers14omaso.dta 
			$RAW\consumos.dta 
             
6) Outputs: $data_arm\URY_ENGIH_2005-2006.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\URY\ENGIH\RAW"
global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\URY\ENGIH\data_arm"


*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\mhogar.dta" , clear 	
	preserve
	
****Importacion de datos faltantes*****
	
	use "$RAW\pers14omaso.dta" , clear
	destring per, replace
	local ingextraord "f5912 f5922 f5932 f5942 f5952 f5962 f5972 f5982 f5992 f59102 f59112"
	foreach x of local ingextraord {
               gen `x'_men=`x'/12
        }
    foreach x of local ingextraord {
	bys viv hog: egen `x'_mensual=max(`x'_men)
	drop `x'_men
	}
	bys viv hog: egen miembros_hogar=max(per)
		
	egen ing_extraord= rowtotal(f5912_mensual f5922_mensual f5932_mensual f5942_mensual f5952_mensual f5962_mensual f5972_mensual f5982_mensual f5992_mensual f59102_mensual f59112_mensual), missing

	collapse (firstnm) miembros_hogar ing_extraord, by(viv hog)
	label var miembros_hogar "Cantidad de miembros del hogar"
	tempfile cant_personas
	save `cant_personas' , replace
	restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge viv hog using `cant_personas' , t(1:1)
drop _merge

	
	/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

egen ing_lab_mon = rowtotal(k5111 k5124 k61 k62), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

egen ing_ren_mon= rowtotal(k521 k522 k523 k524), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

gen ing_trspri_mon= k543 - k561

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen ing_ct_mon= k544

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

egen ing_trsgob_mon= rowtotal(k541 k542), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

egen ing_rem_mon= rowtotal(k545 k64 k63), missing

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

gen ing_otro_mon= ing_extraord

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

egen ing_lab_nomon = rowtotal(k5112 k5125), missing

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

egen ing_ren_nomon = rowtotal(k531 k532), missing

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

gen ing_trspri_nomon = .

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

gen ing_ct_nomon= k571


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
* INGRESO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
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


collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(viv hog) 

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


saveold "$INTERIM\ingresos_hogar.dta" , replace 

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
use "$RAW\consumos.dta" , clear 
		
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
 

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=111101 & ccifdef<211101)
bys viv hog: egen gasto_alihogar= max(aux) 
drop aux 

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=1111101 & ccifdef<=1112104)
bys viv hog: egen gasto_alifuera = max(aux) 
drop aux 

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=211101 & ccifdef<311101)
bys viv hog: egen gasto_alta= max(aux) 
drop aux 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*
egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=311101 & ccifdef<=325102)
bys viv hog: egen gasto_veca= max(aux) 
drop aux 

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=411101 & ccifdef<=432108) | (ccifdef>=443101 & ccifdef<=443203) | ccifdef==1252101 | (ccifdef>=1411101 & ccifdef<=1411207) | (ccifdef>=1431101 & ccifdef<=1441104)
bys viv hog: egen gasto_vcon= max(aux) 
drop aux 

*---------------*
* GASTO EN AGUA *
*---------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=441101 & ccifdef<=442101) | ccifdef==461101
bys viv hog: egen gasto_vag= max(aux) 
drop aux 

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=451101 & ccifdef<=451102)
bys viv hog: egen gasto_vele= max(aux) 
drop aux  

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef==452101 | ccifdef==452302)
bys viv hog: egen gasto_vgn= max(aux) 
drop aux 

*-----------------------------------------------------------*
* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
*-----------------------------------------------------------*
gen gasto_vp=.
gen gasto_vgas=.

bys viv hog: egen aux= sum(valorcontm) if ccifdef==453103
bys viv hog: egen gasto_vk= max(aux)
drop aux

egen gasto_vpgk= rowtotal(gasto_vp gasto_vgas gasto_vk), missing 

*-----------------------------------------------------*
* GASTO EN GAS LICUADO DE PETRÓLEO PARA USO DOMÉSTICO *
*-----------------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=452201 & ccifdef<=452301) 
bys viv hog: egen gasto_vlp= max(aux) 
drop aux 

*------------------------*
* GASTO EN LENIA Y CARBON *
*------------------------*
bys viv hog: egen aux= sum(valorcontm) if ccifdef==454101 
bys viv hog: egen gasto_vca= max(aux) 
drop aux

bys viv hog: egen aux= sum(valorcontm) if ccifdef==454102 
bys viv hog: egen gasto_vle= max(aux) 
drop aux

egen gasto_vleca= rowtotal(gasto_vca gasto_vle), missing

*------------------------------------*
* GASTO EN DIESEL PARA USO DOMÉSTICO *
*------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if ccifdef==453102 
bys viv hog: egen gasto_vdi=max(aux) 
drop aux

*---------------------------------------------*
*Otros gastos en combustibles de uso domestico*
*---------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if ccifdef==453101 
bys viv hog: egen gasto_vot= max(aux) 
drop aux

*----------------------------------------------------------------------------*
*Gasto en vivienda, servicios de conservación y combustibles de uso doméstico*
*----------------------------------------------------------------------------*
egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot), missing

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=511101 & ccifdef<=562207)  
bys viv hog: egen gasto_ens= max(aux) 
drop aux

*----------------*
* GASTO EN SALUD *
*----------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=611101 & ccifdef<711101)  | ccifdef==1241101 | ccifdef>=1253101 
bys viv hog: egen gasto_sal= max(aux) 
drop aux

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=811101 & ccifdef<=834106)  
bys viv hog: egen gasto_com= max(aux) 
drop aux

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=731101 & ccifdef<=736104)
bys viv hog: egen gasto_tserv= max(aux) 
drop aux 
	
*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*
bys viv hog: egen aux= sum(valorcontm) if ccifdef==722101
bys viv hog: egen gasto_tga= max(aux) 
drop aux

*----------------------------------------------*
* Gasto en gas licuado de petróleo-transporte  *
*----------------------------------------------*
gen gasto_tlp=.

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*
bys viv hog: egen aux= sum(valorcontm) if ccifdef==722201
bys viv hog: egen gasto_tdie= max(aux) 
drop aux

*------------------------------------------------*
* GASTO EN GAS NATURAL COMPRIMIDO GNC-TRANSPORTE * 
*------------------------------------------------*
gen gasto_tgnc= .

*-----------------------------------*
* Gasto en alcohol para transporte  *
*-----------------------------------*
gen gasto_talc=.

*---------------------------------------------*
* Otros gastos en combustible para transporte * 
*---------------------------------------------*
gen gasto_totcomb= .

*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*
egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc gasto_totcomb), missing

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=721101 & ccifdef<=721103)  | (ccifdef>=723101 & ccifdef<=724404)
bys viv hog: egen gasto_tman= max(aux) 
drop aux 

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=711101 & ccifdef<=714103) 
bys viv hog: egen gasto_tadq= max(aux) 
drop aux 

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=724101 & ccifdef<=724404) | ccifdef==722301 | ccifdef==1254101
bys viv hog: egen gasto_totros= max(aux) 
drop aux 

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*
egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman gasto_tadq gasto_totros), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=911101 & ccifdef<=954111) | (ccifdef>=1011101 & ccifdef<=1063102) | (ccifdef>=961101 & ccifdef<=961105) | (ccifdef>=1121101 & ccifdef<=1122105) 
bys viv hog: egen gasto_edre= max(aux) 
drop aux

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	
bys viv hog: egen aux= sum(valorcontm) if (ccifdef>=1211101 & ccifdef<1241101) | (ccifdef>=1254102 & ccifdef<=1322103) | ccifdef==1421101 | ccifdef==1421102 | (ccifdef>=1451101 & ccifdef<=1751106)
bys viv hog: egen gasto_otros= max(aux) 
drop aux

*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

egen gct= rowtotal (gasto_ali gasto_veca gasto_viv gasto_ens gasto_sal gasto_com gasto_trans gasto_edre gasto_otros), m

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

**se deja una observacion por cada hogar**
bys viv hog: gen id=_n
keep if id==1

saveold "$INTERIM\gastos_hogar.dta", replace 

**Se hace el merge con la base de ingresos**
mmerge viv hog using "$INTERIM\ingresos_hogar.dta", t(1:1)
drop _merge
replace ict=0 if ict==.	
**Se hace el merge para obtener cant de miembros del hogar**
merge 1:1 viv hog using "$INTERIM\cant_personas.dta", keepusing(persona)
drop _merge
rename persona miembros_hogar 
label var miembros_hogar "Cantidad de miembros del hogar"
 
keep viv hog facexp miembros_hogar gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "URY"	
label var pais "Pais de la Encuesta"

gen anio="2005-2006"
label var anio "Anio de la Encuesta"

gen encuesta = "ENGIH"
label var encuesta "Encuesta"

egen cod_hogar=concat(viv hog)
label var cod_hogar "Codigo de hogar"

rename facexp factor_expansion
label var factor_expansion "Factor de expansion" 

order  pais anio encuesta cod_hogar factor_expansion miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\URY_ENGIH_2005-2006.dta" , replace


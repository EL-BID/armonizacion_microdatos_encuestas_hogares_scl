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

2) Fecha: Abril 2018

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los shocks climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático			 
5) País: Chile

6) Encuesta: Encuesta de Presupuestos Familiares-EPF

7) Ano: 2013					

8) Inputs: 	$RAW\BASE_GASTOS_VIIEPF.dta 
            $RAW\BASE_PERSONAS_VIIEPF_31032015.dta 
			$RAW\CCIF_VIIEPF.dta 
             
6) Outputs: $data_arm\CHL_EPF_2013.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\CHL\EPF\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\CHL\EPF\data_arm"

	
**Cantidad de miembros del hogar**
	use "$RAW\BASE_PERSONAS_VIIEPF_31032015.dta" , clear 
	collapse (firstnm) NPERSONA, by(FOLIO)
	saveold "$RAW\datos_hogar.dta" , replace 

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\BASE_PERSONAS_VIIEPF_31032015.dta" , clear 
	
         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

egen ing_lab_mon = rowtotal(INGTD INGTI INGOTA INGOTI), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

egen ing_ren_mon= rowtotal(INGP INGF), missing

*-----------------------------------------------------------*
* INGRESO NETO MONETARIO POR TRANSFERENCIAS PRIVADAS Y/O PÚBLICAS* 
*-----------------------------------------------------------*

gen ing_tnet_mon= INGT_NETA

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

gen ing_trspri_mon= .

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen ing_ct_mon=.

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

gen ing_trsgob_mon= INGJ

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

*En este caso particular de Chile no hay info desagregada de transferencias publicas y privadas, sino un monto neto (ing_tnet_mon) ///
	// Sin embargo este ing_tnet_mon, NO se computa en el total el ing_mon 


         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*


**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

gen ing_lab_nomon = .

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

egen ing_ren_nomon = rowtotal(AI_VP AI_VS2), missing

*-------------------------------------------------------------------*
* INGRESO NETO NO MONETARIO POR TRANSFERENCIAS PRIVADAS Y/O PÚBLICAS* 
*-------------------------------------------------------------------*

gen ing_tnet_nomon= .

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

*-------------------------------------------------------*
* INGRESO NETO POR TRANSFERENCIAS PRIVADAS Y/O PÚBLICAS * 
*-------------------------------------------------------*

egen ing_tnet= rowtotal(ing_tnet_mon ing_tnet_nomon), missing 

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
global miss "ing_mon ing_lab_mon ing_ren_mon ing_tnet_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_tnet_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_tnet ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}


collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_tnet_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_tnet_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_tnet ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_tnet_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_tnet_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_tnet_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(FOLIO) 

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}


** Etiquetas Variables Monetarias**

label var ing_mon "Ingreso monetario del hogar"
label var ing_lab_mon "Ingreso monetario laboral del hogar"
label var ing_ren_mon "Ingreso monetario por renta de la propiedad del hogar"
label var ing_tnet_mon "Ingreso neto monetario por transferencias privadas y/o públicas del hogar"
label var ing_trspri_mon "Ingreso monetario por transferencias privadas del hogar"
label var ing_ct_mon "Ingreso monetario del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob_mon "Ingreso monetario por transferencias publicas del hogar"
label var ing_rem_mon "Ingreso monetario del hogar por remesas internacionales"
label var ing_otro_mon "Otros ingresos monetarios/ ingresos extraordinarios del hogar" 

** Etiquetas Variables No Monetarias**

label var ing_nomon "Ingreso no monetario del hogar"
label var ing_lab_nomon "Ingreso no monetario laboral del hogar"
label var ing_ren_nomon "Ingreso no monetario por renta de la propiedad del hogar"
label var ing_tnet_nomon "Ingreso neto no monetario por transferencias privadas y/o públicas del hogar"
label var ing_trspri_nomon "Ingreso no monetario por transferencias privadas del hogar"
label var ing_ct_nomon "Ingreso no monetario del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob_nomon "Ingreso no monetario del hogar por otras transferencias del gobierno"
label var ing_rem_nomon "Ingreso no monetario del hogar por remesas internacionales"
label var ing_otro_nomon "Otros ingresos no monetarios/ ingresos extraordinarios del hogar" 

** Etiquetas Variables Agregadas**

label var ict "Ingreso corriente total"
label var ing_lab "Ingreso laboral del hogar"
label var ing_ren "Ingreso por renta de la propiedad del hogar"
label var ing_tnet "Ingreso neto por transferencias privadas y/o públicas del hogar"
label var ing_trspri "Ingreso por transferencias privadas locales del hogar"
label var ing_rem "Ingreso del hogar por remesas internacionales"
label var ing_tpriv "Ingreso del hogar por transferencias privadas"
label var ing_ct "Ingreso del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob "Ingreso del hogar por otras transferencias del gobierno"
label var ing_tpub "Ingreso del hogar por transferencias del gobierno"
label var ing_otro "Otros ingresos/ ingresos extraordinarios del hogar" 


saveold "$RAW\ingresos_hogar.dta" , replace 

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
use "$RAW\BASE_GASTOS_VIIEPF.dta" , clear 
		
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
 
egen CCIF_num=concat(D G C SC P)
destring CCIF_num, replace

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num>=1100000 & CCIF_num<2000000
bys FOLIO: egen gasto_alihogar= max(aux) 
drop aux 

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=11110000 & CCIF_num<=11110104) | (CCIF_num>=11110109 & CCIF_num<=11110404) | (CCIF_num>=11110409 & CCIF_num<=11120201)
bys FOLIO: egen gasto_alifuera = max(aux) 
drop aux 

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=2000000 & CCIF_num<3000000) | (CCIF_num>=11110105 & CCIF_num<=11110108) | (CCIF_num>=11110405 & CCIF_num<=11110408)
bys FOLIO: egen gasto_alta= max(aux) 
drop aux 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*
egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=3000000 & CCIF_num<4000000)
bys FOLIO: egen gasto_veca= max(aux) 
drop aux 

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=4100000 & CCIF_num<4400000) | (CCIF_num>=4420000 & CCIF_num<4450000) | (CCIF_num>=12510000 & CCIF_num<12520000)
bys FOLIO: egen gasto_vcon= max(aux) 
drop aux 

*---------------*
* GASTO EN AGUA *
*---------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=4410000 & CCIF_num<4420000)
bys FOLIO: egen gasto_vag= max(aux) 
drop aux 

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=4510000 & CCIF_num<4520000)
bys FOLIO: egen gasto_vele= max(aux) 
drop aux  

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=4520100 & CCIF_num<=4520102)
bys FOLIO: egen gasto_vgn= max(aux) 
drop aux 

*-----------------------------------------------------------*
* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
*-----------------------------------------------------------*
gen gasto_vp=.
gen gasto_vgas=.
gen gasto_vk=.

bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=4530000 & CCIF_num<4540000)
bys FOLIO: egen gasto_vpgk= max(aux) 
drop aux 

*-----------------------------------------------------*
* GASTO EN GAS LICUADO DE PETRÓLEO PARA USO DOMÉSTICO *
*-----------------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==4520103 
bys FOLIO: egen gasto_vlp= max(aux) 
drop aux 

*------------------------*
* GASTO EN LENIA Y CARBON *
*------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==4540101 
bys FOLIO: egen gasto_vca= max(aux) 
drop aux

bys FOLIO: egen aux= sum(GASTO) if CCIF_num==4540102 
bys FOLIO: egen gasto_vle= max(aux) 
drop aux

egen gasto_vleca= rowtotal(gasto_vca gasto_vle), missing

*------------------------------------*
* GASTO EN DIESEL PARA USO DOMÉSTICO *
*------------------------------------*
gen gasto_vdi=.

*---------------------------------------------*
*Otros gastos en combustibles de uso domestico*
*---------------------------------------------*
gen gasto_vot=.

*-----------------------------*
* OTROS GASTOS no desglozados *
*-----------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=4600000 & CCIF_num<5000000)  
bys FOLIO: egen gasto_votot= max(aux) 
drop aux

*----------------------------------------------------------------------------*
*Gasto en vivienda, servicios de conservación y combustibles de uso doméstico*
*----------------------------------------------------------------------------*
egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot gasto_votot), missing

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=5000000 & CCIF_num<6000000)  
bys FOLIO: egen gasto_ens= max(aux) 
drop aux

*----------------*
* GASTO EN SALUD *
*----------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=6000000 & CCIF_num<7000000)  | (CCIF_num>=12400000 & CCIF_num<12500000) | (CCIF_num>=12520000 & CCIF_num<12530000) 
bys FOLIO: egen gasto_sal= max(aux) 
drop aux

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=8000000 & CCIF_num<9000000)  
bys FOLIO: egen gasto_com= max(aux) 
drop aux

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num>=7300000 & CCIF_num<7400000
bys FOLIO: egen gasto_tserv= max(aux) 
drop aux 
	
*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==7220101
bys FOLIO: egen gasto_tga= max(aux) 
drop aux

*----------------------------------------------*
* Gasto en gas licuado de petróleo-transporte  *
*----------------------------------------------*
gen gasto_tlp=.

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==7220102
bys FOLIO: egen gasto_tdie= max(aux) 
drop aux

*------------------------------------------------*
* GASTO EN GAS NATURAL COMPRIMIDO GNC-TRANSPORTE * 
*------------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==7220103
bys FOLIO: egen gasto_tgnc= max(aux) 
drop aux

*-----------------------------------*
* Gasto en alcohol para transporte  *
*-----------------------------------*
gen gasto_talc=.

*---------------------------------------------*
* Otros gastos en combustible para transporte * 
*---------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==7220104
bys FOLIO: egen gasto_totcomb= max(aux) 
drop aux 

*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*
egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc gasto_totcomb), missing

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=7210000 & CCIF_num<7220000)  | (CCIF_num>=7230000 & CCIF_num<7240000)
bys FOLIO: egen gasto_tman= max(aux) 
drop aux 

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=7100000 & CCIF_num<7200000) 
bys FOLIO: egen gasto_tadq= max(aux) 
drop aux 

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*
bys FOLIO: egen aux= sum(GASTO) if CCIF_num==7220105 | (CCIF_num>=7240000 & CCIF_num<7300000)  | (CCIF_num>=7400000 & CCIF_num<8000000) | (CCIF_num>=12530000 & CCIF_num<12540000)
bys FOLIO: egen gasto_totros= max(aux) 
drop aux 

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*
egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman gasto_tadq gasto_totros), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=9000000 & CCIF_num<11000000)
bys FOLIO: egen gasto_edre= max(aux) 
drop aux

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	
bys FOLIO: egen aux= sum(GASTO) if (CCIF_num>=11200000 & CCIF_num<12500000) | (CCIF_num>=12540000 & CCIF_num<12900000)
bys FOLIO: egen gasto_otros= max(aux) 
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
label var gasto_votot "Otros gastos no desglosados en alojamiento, agua, electricidad, gas y otros combustibles"
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
bys FOLIO:gen id=_n
keep if id==1

saveold "$RAW\gastos_hogar.dta", replace 

**Se hace el merge con la base de ingresos**
drop if FOLIO==""
mmerge FOLIO using "$RAW\ingresos_hogar.dta", t(1:1)
drop _merge
replace ict=0 if ict==.	
**Se hace el merge para obtener cant de miembros del hogar**
merge 1:1 FOLIO using "$RAW\datos_hogar.dta", keepusing(NPERSONA)
drop _merge
rename NPERSONA miembros_hogar
label var miembros_hogar "Número de miembros del hogar"
 
keep FOLIO FE miembros_hogar gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_votot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_tnet_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_tnet_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_tnet ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "CHI"	
label var pais "Pais de la Encuesta"

gen anio= 2013
label var anio "Anio de la Encuesta"

gen encuesta = "EPF"
label var encuesta "Encuesta"

encode FOLIO, gen(cod_hogar)
label var cod_hogar "Codigo de hogar"

rename FE factor_expansion
label var factor_expansion "Factor de expansion" 

order  pais anio encuesta cod_hogar factor_expansion miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_tnet_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_tnet_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_tnet ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_votot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\CHL_EPF_2013.dta" , replace


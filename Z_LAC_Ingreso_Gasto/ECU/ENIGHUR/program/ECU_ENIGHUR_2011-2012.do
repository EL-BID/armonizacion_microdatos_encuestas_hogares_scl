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
5) País: Ecuador

6) Encuesta: Encuesta nacional de ingresos y gastos de hogares urbanos y rurales - ENIGHUR

7) Ano: 2011-2012					

8) Inputs: 	$RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\02_enighur11_personas_ingresos.dta 
            $RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\10_enighur11_hogares_agregados.dta 
			$RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\07_enighur11_gastos_htot.dta 
             
6) Outputs: $data_arm\ECU_ENIGHUR_2011-2012.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
	global RAW    "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\ECU\ENIGHUR\RAW"
	global data_arm  "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\ECU\ENIGHUR\data_arm"
	
	
*Cantidad de miembros del hogar*

	use "$RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\02_enighur11_personas_ingresos.dta" , clear 
	
	bys identif_hog: gen miembros=_n if p04<=8 //solo se toman los miembros del hogar
	bys identif_hog: egen miembros_hogar=max(miembros)
	label var miembros_hogar "Número de miembros del hogar"

	collapse (firstnm) miembros_hogar, by(identif_hog)
	saveold "$RAW\datos_hogar.dta" , replace 
	
	
	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\02_enighur11_personas_ingresos.dta" , clear 
	
	
	merge m:1 identif_hog using "$RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\10_enighur11_hogares_agregados.dta", keepusing (alq_imp)
	drop _merge
         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*
*Trabajadores en relacion de dependencia
egen ing_lab_mon_bruto_asal = rowtotal(i1401097 i1404001 i1404002 i1404003 i1404004 i1404005 i1404006), m
gen tot_deduc=i170109
recode ing_lab_mon_bruto_asal .=0 if tot_deduc!=.
recode tot_deduc .=0 if ing_lab_mon_bruto_asal!=.
gen ing_lab_mon_neto_asal = ing_lab_mon_bruto_asal - tot_deduc
*Trabajadores por cuenta propia
egen ing_lab_mon_neto_cp= rowtotal(i1407099 i1443001 a1443001 b1443001), m
*Trabajadores por cuenta propia agricolas 
egen ing_lab_mon_bruto_agri = rowtotal(i1408097 i1409097 i1416097), m
gen gtos_agri=g1703097
recode ing_lab_mon_bruto_agri .=0 if gtos_agri!=.
recode gtos_agri .=0 if ing_lab_mon_bruto_agri!=.
gen ing_lab_mon_neto_agri = ing_lab_mon_bruto_agri - gtos_agri 
*Trabajadores por cuenta propia actividad forestal 
gen ing_lab_mon_bruto_for = i1421097
gen gtos_for=g1704097
recode ing_lab_mon_bruto_for .=0 if gtos_for!=.
recode gtos_for .=0 if ing_lab_mon_bruto_for!=.
gen ing_lab_mon_neto_for = ing_lab_mon_bruto_for  - gtos_for 
*Trabajadores por cuenta propia actividad pecuaria 
egen ing_lab_mon_bruto_pec = rowtotal(i1424097 i1428097 i1431097), m
gen gtos_pec=g1705097
recode ing_lab_mon_bruto_pec .=0 if gtos_pec!=.
recode gtos_pec .=0 if ing_lab_mon_bruto_pec!=.
gen ing_lab_mon_neto_pec = ing_lab_mon_bruto_pec  - gtos_pec
*Trabajadores por cuenta propia actividad de recoleccion 
gen ing_lab_mon_bruto_rec = i1436097
gen gtos_rec=g1706097
recode ing_lab_mon_bruto_rec .=0 if gtos_rec!=.
recode gtos_rec .=0 if ing_lab_mon_bruto_rec!=.
gen ing_lab_mon_neto_rec = ing_lab_mon_bruto_rec  - gtos_rec 

egen ing_lab_mon = rowtotal(ing_lab_mon_neto_asal ing_lab_mon_neto_cp ing_lab_mon_neto_agri ing_lab_mon_neto_for ing_lab_mon_neto_pec ing_lab_mon_neto_rec), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

egen ing_ren_mon= rowtotal(i1445001 i1445002 i1445003 i1445004 i1445005 i1445006 i1445007), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

egen aux=rowtotal(i1444007 i1444004), m
egen aux2=rowtotal(i1709006 i1709007), m
recode aux .=0 if aux2!=.
recode aux2 .=0 if aux!=.
gen ing_trspri_mon= aux - aux2
drop aux aux2

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

egen ing_ct_mon=rowtotal(i1444002 i1444003 i1444006 i1444008), m

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

gen ing_trsgob_mon= i1444001

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= i1444005

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

egen aux=rowtotal(i1446001 i1446002 i1446003 i1501001 i1501002 i1501003 i1501004 i1501005 i1501006), m
egen aux2=rowtotal(i1601001 i1601002 i1601003 i1601004 i1601006), m
recode aux .=0 if aux2!=.
recode aux2 .=0 if aux!=.
gen ing_otro_mon= aux - aux2
drop aux aux2

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
*Trabajadores en relacion de dependencia
gen ing_lab_nomon_neto_asal = i1402098
*Trabajadores por cuenta propia
gen ing_lab_nomon_neto_cp=i1405098
*Trabajadores por cuenta propia agricolas 
egen ing_lab_nomon_neto_agri = rowtotal(i1410098 i1411098 i1412098 i1413098 i1417098 i1418098), m
*Trabajadores por cuenta propia actividad forestal
gen ing_lab_nomon_neto_for = i1422098
*Trabajadores por cuenta propia actividad pecuaria 
egen ing_lab_nomon_neto_pec = rowtotal (i1425098 i1426098 i1427098 i1432098 i1433098), m
*Trabajadores por cuenta propia actividad de recoleccion 
egen ing_lab_nomon_neto_rec = rowtotal(i1437098 i1438098), m

egen ing_lab_nomon = rowtotal(ing_lab_nomon_neto_asal ing_lab_nomon_neto_cp ing_lab_nomon_neto_agri ing_lab_nomon_neto_for ing_lab_nomon_neto_pec ing_lab_nomon_neto_rec), m

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

gen ing_ren_nomon = alq_imp

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


collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(identif_hog) 

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


saveold "$RAW\ingresos_hogar.dta" , replace 

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	use "$RAW\02 BASE DE DATOS\02 TABLAS DE TRABAJO\07_enighur11_gastos_htot.dta" , clear 
		
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
 
*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*
egen gasto_alihogar= rowtotal(c111000 - c122199), m


*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*
egen gasto_alifuera= rowtotal(c1111000 - c1112099), m

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*
egen gasto_alta= rowtotal(c211000 - c231099), m 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*
egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), m

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*
egen gasto_veca= rowtotal(c311000 - c322003), m

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*
egen aux=rowtotal(c411000 - c432099), m
egen aux2=rowtotal(c442000 - c444099), m 
egen aux3=rowtotal(c1252000 - c1252099), m 
egen aux4=rowtotal(c1801001 - c1801004), m 
 
egen gasto_vcon=rowtotal(aux aux2 aux3 aux4), m
drop aux aux2 aux3 aux4

*---------------*
* GASTO EN AGUA *
*---------------*
egen gasto_vag= rowtotal(c441000 - c441099), m

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*
egen gasto_vele= rowtotal(c451000 - c451002), m

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*
egen gasto_vgn= rowtotal(c452000 - c452099), m

*-----------------------------------------------------------*
* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
*-----------------------------------------------------------*
gen gasto_vp=.
gen gasto_vgas=c453002
gen gasto_vk= c453003

egen gasto_vpgk= rowtotal(gasto_vp gasto_vgas gasto_vk), m

*-----------------------------------------------------*
* GASTO EN GAS LICUADO DE PETRÓLEO PARA USO DOMÉSTICO *
*-----------------------------------------------------*
gen gasto_vlp=.

*------------------------*
* GASTO EN LENIA Y CARBON *
*------------------------*
gen gasto_vca= c454002
gen gasto_vle= c454004 

egen gasto_vleca= rowtotal(gasto_vca gasto_vle), missing

*------------------------------------*
* GASTO EN DIESEL PARA USO DOMÉSTICO *
*------------------------------------*
gen gasto_vdi=c453001

*---------------------------------------------*
*Otros gastos en combustibles de uso domestico*
*---------------------------------------------*
egen gasto_vot= rowtotal(c453099 c454001 c454003 c454005 c454099 c455000 c455001 c455002 c455003 c455004), m

*-----------------------------*
* OTROS GASTOS no desglozados *
*-----------------------------*
gen gasto_votot=. 

*----------------------------------------------------------------------------*
*Gasto en vivienda, servicios de conservación y combustibles de uso doméstico*
*----------------------------------------------------------------------------*
egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot gasto_votot), missing

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*
egen gasto_ens= rowtotal(c511000 - c562299), m  

*----------------*
* GASTO EN SALUD *
*----------------*
egen aux= rowtotal (c611000 - c631001), m 
egen aux2= rowtotal (c1241000 - c1251099), m 
egen aux3= rowtotal (c1253000 - c1253099), m 

egen gasto_sal= rowtotal(aux aux2 aux3), m
drop aux aux2 aux3

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*
egen gasto_com= rowtotal(c811000 - c831099), m 

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*
egen gasto_tserv= rowtotal(c731000 - c736099), m 
	
*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*
egen gasto_tga= rowtotal(c722003 c722004 c722005), m

*----------------------------------------------*
* Gasto en gas licuado de petróleo-transporte  *
*----------------------------------------------*
gen gasto_tlp=c722002

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*
gen gasto_tdie= c722001 

*------------------------------------------------*
* GASTO EN GAS NATURAL COMPRIMIDO GNC-TRANSPORTE * 
*------------------------------------------------*
gen gasto_tgnc=.

*-----------------------------------*
* Gasto en alcohol para transporte  *
*-----------------------------------*
gen gasto_talc=.

*---------------------------------------------*
* Otros gastos en combustible para transporte * 
*---------------------------------------------*
gen gasto_totcomb=. 

*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*
egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc gasto_totcomb), missing

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*
egen aux= rowtotal(c722100 - c722199), m
egen aux2= rowtotal(c723000 - c723099), m

egen gasto_tman= rowtotal(aux aux2), m 
drop aux aux2

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*
egen gasto_tadq= rowtotal(c711000 - c714099), m 

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*
egen aux= rowtotal(c722100 - c725002), m
egen aux2= rowtotal(c1254000 - c1254099), m

egen gasto_totros= rowtotal(aux aux2), m 
drop aux aux2

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*
egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman gasto_tadq gasto_totros), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*
egen gasto_edre= rowtotal(c911000 - c1061011), m

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	
egen aux= rowtotal(c1121000 - c1232099), m
egen aux2= rowtotal(c1255000 - c1271099), m

egen gasto_otros= rowtotal(aux aux2), m 
drop aux aux2

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

saveold "$RAW\gastos_hogar.dta", replace 

**Se hace el merge con la base de ingresos**
mmerge identif_hog using "$RAW\ingresos_hogar.dta", t(1:1)
drop _merge
replace ict=0 if ict==.	

**Se hace el merge para obtener cant de miembros del hogar**
merge 1:1 identif_hog using "$RAW\datos_hogar.dta", keepusing(miembros_hogar)
drop _merge
 
keep identif_hog fexp_cen2010 miembros_hogar gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_votot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "ECU"	
label var pais "Pais de la Encuesta"

gen anio= "2011-2012"
label var anio "Anio de la Encuesta"

gen encuesta = "ENIGHUR"
label var encuesta "Encuesta"

encode identif_hog, gen(cod_hogar)
label var cod_hogar "Codigo de hogar"

rename fexp_cen2010 factor_expansion
label var factor_expansion "Factor de expansion" 

order  pais anio encuesta cod_hogar factor_expansion miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_votot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\ECU_ENIGHUR_2011-2012.dta" , replace


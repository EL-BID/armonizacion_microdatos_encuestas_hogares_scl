clear all
clear matrix
set more off
set matsize 2000


/*************************************************************************
 *************************************************************************			       	
	            Inter-American Development Bank

1) Elaborado por: Laura Di Capua
			      lauradicapua@gmail.com

2) Fecha: Mayo 2018

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los shocks climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático			 
5) País: Paraguay

6) Encuesta: Encuesta de Ingresos y Gastos y de Condiciones de Vida - EIGyCV

7) Ano: 2011-2012					

8) Inputs: 	$RAW\Datos de la poblacion reg02t.dta 
            $RAW\Gastos a nivel de hogar sumaria_hogar.dta 
			$RAW\Agregado a nivel de transaccion agregado.dta
			             
6) Outputs: $data_arm\PRY_EIGyCV_2011-2012.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
global RAW    "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\PRY\EIGyCV\RAW"
global data_arm  "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\PRY\EIGyCV\data_arm"
	
	

	use "$RAW\Datos de la poblacion reg02t.dta" , clear 	
	bys upm nvivi nhoga: egen miembros=max(l02)
	collapse (firstnm) fex miembros, by(upm nvivi nhoga)	
	saveold "$RAW\datos_hogar.dta" , replace 
	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*
	
	use "$RAW\Datos de la poblacion reg02t.dta" , clear 	
		
	merge m:1 upm nvivi nhoga using "$RAW\Gastos a nivel de hogar sumaria_hogar.dta", keepusing (div4ai)
	drop _merge
	
         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**
global ingresos "dd0101 dd0102 dd0103 dd0104 dd0105 dd0106 dd0107 dd0108 dd0109 dd0110 dd0111 dd0201 dd0202 dd0203 dd0204 dd0205 dd0206"
foreach var in $ingresos { 
recode `var' 99999999999=.
}

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*
egen ing_lab_mon=rowtotal(dd0101 dd0102 dd0103), m

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*
egen ing_ren_mon= rowtotal(dd0104 dd0105), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

egen ing_trspri_mon= rowtotal(dd0106 dd0110), missing

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen ing_ct_mon=.

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

egen ing_trsgob_mon= rowtotal(dd0108 dd0109), missing //el label de la variable "dd0109" esta mal nombrado en la base

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= dd0107

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

egen ing_otro_mon= rowtotal(dd0111 dd0201 dd0202 dd0203 dd0204 dd0205 dd0206), m

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

gen ing_lab_nomon = .

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

gen ing_ren_nomon = div4ai

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


collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(upm nvivi nhoga) 

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
	use "$RAW\Agregado a nivel de transaccion agregado.dta" , clear 
	
	egen gto=rowtotal(dga04 idga05g), m
	
	*-----------------------------------------------------*
	* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
	*-----------------------------------------------------*
	bys upm nvivi nhoga: egen gasto_alihogar= total(gto) if (ga01c>=100000000 & ga01c<102300000) | (ga01c>=103000000 & ga01c<104000000) 

   *---------------------------------------------------------*
	* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
	*---------------------------------------------------------*
	bys upm nvivi nhoga: egen gasto_alifuera =total(gto) if (ga01c>=104000000 & ga01c<=104303022) | (ga01c>=104500000 & ga01c<=104502594)
	
	*---------------------------------------*
	* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
	*---------------------------------------*
	bys upm nvivi nhoga: egen gasto_alta= total(gto) if (ga01c>=102300000 & ga01c<103000000) | (ga01c>=104400000 & ga01c<=104401062) | (ga01c>=105000000 & ga01c<=105102003)

	*----------------------------*
	* GASTO EN VESTIDO Y CALZADO *
	*----------------------------*
	bys upm nvivi nhoga: egen gasto_veca= total(gto) if (ga01c>=200000000 & ga01c<300000000)
	
	*----------------------------------------------*
	* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
	*----------------------------------------------*
	bys upm nvivi nhoga: egen gasto_vcon=total(gto) if (ga01c>=301000000 & ga01c<305000000) | (ga01c>=305200000 & ga01c<305402000) | (ga01c>=710102000 & ga01c<711000000)
		
	*---------------*
	* GASTO EN AGUA *
	*---------------*
	bys upm nvivi nhoga: egen gasto_vag=total(gto) if ga01c>=305100000 & ga01c<305200000
	
	*-----------------------*
	* GASTO EN ELECTRICIDAD *
	*-----------------------*
	bys upm nvivi nhoga: egen gasto_vele=total(gto) if ga01c>=306100000 & ga01c<=306101000
	
	*----------------------*
	* GASTO EN GAS NATURAL *
	*----------------------*
	gen gasto_vgn= . 
	
	*-----------------------------------------------------------*
	* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
	*-----------------------------------------------------------*
	gen gasto_vp=.
	gen gasto_vgas=.
	bys upm nvivi nhoga: egen gasto_vk=total(gto) if ga01c==306301002
		
	*-----------------------------------------------------*
	* GASTO EN GAS LICUADO DE PETRÓLEO PARA USO DOMÉSTICO *
	*-----------------------------------------------------*
	bys upm nvivi nhoga: egen gasto_vlp=total(gto) if ga01c>=306201000 & ga01c<=306201003
		
	*------------------------*
	* GASTO EN LENIA Y CARBON *
	*------------------------*
	bys upm nvivi nhoga: egen gasto_vca=total(gto) if ga01c==306401001
	bys upm nvivi nhoga: egen gasto_vle=total(gto) if ga01c==306401002
		
	*------------------------------------*
	* GASTO EN DIESEL PARA USO DOMÉSTICO *
	*------------------------------------*
	gen gasto_vdi=.
	
	*---------------------------------------------*
	*Otros gastos en combustibles de uso domestico*
	*---------------------------------------------*
	bys upm nvivi nhoga: egen gasto_vot=total(gto) if ga01c==306401003 | ga01c==306401004 | ga01c==306301001
	
	*----------------------------------------------------------*
	* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
	*----------------------------------------------------------*
	bys upm nvivi nhoga: egen gasto_ens= total(gto) if ga01c>=307000000 & ga01c<400000000

	*----------------*
	* GASTO EN SALUD *
	*----------------*
	bys upm nvivi nhoga: egen gasto_sal=total(gto) if (ga01c>=400000000 & ga01c<500000000) | (ga01c>=705000000 & ga01c<706000000)

	*-------------------------*
	* GASTO EN COMUNICACIONES *
	*-------------------------*
	bys upm nvivi nhoga: egen gasto_com=total(gto) if ga01c>=504000000 & ga01c<600000000

	*----------------------------------*
	* GASTO EN SERVICIOS DE TRANSPORTE *
	*----------------------------------*
	bys upm nvivi nhoga: egen gasto_tserv= total(gto) if ga01c>=503000000 & ga01c<504000000
	
	*------------------------------*
	* GASTO EN GASOLINA-TRANSPORTE *
	*------------------------------*
	bys upm nvivi nhoga: egen gasto_tga= total(gto) if ga01c>=502201000 & ga01c<502202000

	
	*----------------------------------------------*
	* Gasto en gas licuado de petróleo-transporte  *
	*----------------------------------------------*
	gen gasto_tlp=.

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*
gen gasto_tdie= .

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
	

	*-------------------------------------------------*
	* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
	*-------------------------------------------------*
	bys upm nvivi nhoga: egen gasto_tman= total(gto) if (ga01c>=502100000 & ga01c<502200000) | (ga01c>=502202000 & ga01c<502400000)

	*--------------------------------------------*
	* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
	*--------------------------------------------*
	bys upm nvivi nhoga: egen gasto_tadq= total(gto) if ga01c>=501100000 & ga01c<501200000

	*----------------------------*
	* OTROS GASTOS EN TRANSPORTE *
	*----------------------------*
	bys upm nvivi nhoga: egen gasto_totros= total(gto) if (ga01c>=501300000 & ga01c<502000000) | (ga01c>=502400000 & ga01c<503000000)

	*---------------------------------*
	* GASTO EN EDUCACION Y RECREACION *
	*---------------------------------*
	bys upm nvivi nhoga: egen gasto_edre=total(gto) if (ga01c>=600000000 & ga01c<700000000) | (ga01c>=709000000 & ga01c<710000000)
 
	*-----------------------------------*
	* GASTO EN OTROS BIENES Y SERVICIOS *
	*-----------------------------------*	
	bys upm nvivi nhoga: egen gasto_otros= total(gto) if (ga01c>=700000000 & ga01c<705000000) | (ga01c>=706000000 & ga01c<709000000) | (ga01c>=710000000 & ga01c<710102000) | (ga01c>=711000000 & ga01c<=711101006)
		
	
	global miss "gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vcon gasto_vk gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vot gasto_vele gasto_vgn gasto_vag gasto_ens gasto_sal gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq  gasto_totros gasto_com gasto_edre gasto_otros"
	foreach var in $miss {
	egen `var'_miss= count(`var') if `var'!=.
	}
	collapse (firstnm) gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vcon gasto_vk gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vot gasto_vele gasto_vgn gasto_vag gasto_ens gasto_sal gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq  gasto_totros gasto_com gasto_edre gasto_otros (count) gasto_alihogar_miss gasto_alifuera_miss gasto_alta_miss gasto_veca_miss gasto_vcon_miss gasto_vk_miss gasto_vle_miss gasto_vca_miss gasto_vlp_miss gasto_vdi_miss gasto_vp_miss gasto_vgas_miss gasto_vot_miss gasto_vele_miss gasto_vgn_miss gasto_vag_miss gasto_ens_miss gasto_sal_miss gasto_tserv_miss gasto_tga_miss gasto_tdie_miss gasto_tgnc_miss gasto_tlp_miss gasto_talc_miss gasto_totcomb_miss gasto_tman_miss gasto_tadq_miss gasto_totros_miss gasto_com_miss gasto_edre_miss gasto_otros_miss, by(upm nvivi nhoga) 
	foreach var in $miss {
	replace `var'=. if `var'_miss==0
	drop `var'_miss
	}
	
	egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing
	egen gasto_vpgk= rowtotal(gasto_vp gasto_vgas gasto_vk), m 
	egen gasto_vleca= rowtotal(gasto_vca gasto_vle), missing
	egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot), missing
	egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc gasto_totcomb), missing
	egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman gasto_tadq gasto_totros), missing
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


saveold "$RAW\gastos_hogar_new.dta", replace 

**Se hace el merge con la base de ingresos**
mmerge upm nvivi nhoga using "$RAW\ingresos_hogar.dta", t(1:1)
drop _merge
replace ict=0 if ict==.	

mmerge upm nvivi nhoga using "$RAW\datos_hogar.dta", t(1:1)
drop _merge
 
keep upm nvivi nhoga fex miembros gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "PRY"	
label var pais "Pais de la Encuesta"

gen anio= "2011-2012"
label var anio "Anio de la Encuesta"

gen encuesta = "EIGyCV"
label var encuesta "Encuesta"

gen aux=00
egen aux2=concat(upm aux nvivi aux nhoga)
encode aux2, gen(cod_hogar)
label var cod_hogar "Codigo de hogar"
drop aux aux2

rename fex factor_expansion
label var factor_expansion "Factor de expansion" 

rename miembros miembros_hogar
label var miembros_hogar "Número de miembros del hogar"

order  pais anio encuesta cod_hogar factor_expansion miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\PRY_EIGyCV_2011-2012.dta" , replace


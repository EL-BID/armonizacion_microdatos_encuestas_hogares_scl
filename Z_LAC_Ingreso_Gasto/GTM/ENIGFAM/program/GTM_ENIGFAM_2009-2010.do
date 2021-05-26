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

2) Fecha: Mayo 2018

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los shocks climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático			 
5) País: Guatemala

6) Encuesta: Encuesta Nacional De Ingresos y Gastos Familiares -ENIGFAM-

7) Ano: 2009-2010					

8) Inputs: 	$RAW\data_orig\base_de_datos_personas.dta 
            $RAW\data_orig\b01_programas_asist_social.dta 
			$RAW\data_orig\base_de_datos_hogares.dta
			$RAW\data_orig\b02_base_alimentos_seccion_a.dta
			$RAW\data_orig\b03_gastos_anuales.dta
			$RAW\data_orig\b03_gastos_semestrales.dta
			$RAW\data_orig\b03_gastos_trimestrales.dta
			$RAW\data_orig\b03_gastos_mensuales.dta
             
6) Outputs: $data_arm\GTM_ENIGFAM_2009-2010.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
	global RAW    "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\GTM\ENIGFAM\RAW"
	global data_arm  "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\GTM\ENIGFAM\data_arm"

*Cantidad de miembros del hogar
	use "$RAW\data_orig\base_de_datos_personas.dta" , clear
	sort formulario id
	gen uno=1
	bys formulario: egen miembros_hogar=total(uno) 
	label var miembros_hogar "Número de miembros del hogar"
	collapse (firstnm) miembros_hogar, by(formulario)
	saveold "$RAW\datos_hogar.dta" , replace 
	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se prepara base de asistencia social para exportar datos de ingresos a base de personas

	use "$RAW\data_orig\b01_programas_asist_social.dta", clear 

gen aux_ing_ct_mon=.
replace aux_ing_ct_mon= b1p01e04 if b1p01e03==1 & (b1p01e01a==7 | b1p01e01a==6)

gen aux_ing_ct_nomon=.
replace aux_ing_ct_nomon=b1p01e04 if b1p01e03==1 & (b1p01e01a!=7 & b1p01e01a!=6)

gen aux_ing_rem_mon=.
replace aux_ing_rem_mon=b1p01e04 if b1p01e03==5 & (b1p01e01a==7 | b1p01e01a==6)

gen aux_ing_rem_nomon=.
replace aux_ing_rem_nomon=b1p01e04 if b1p01e03==5 & (b1p01e01a!=7 | b1p01e01a!=6)	

gen aux_ing_trspri_mon=.
replace aux_ing_trspri_mon=b1p01e04 if (b1p01e03>=2 & b1p01e03<=4) & (b1p01e01a==7 | b1p01e01a==6)

gen aux_ing_trspri_nomon=.
replace aux_ing_trspri_nomon=b1p01e04 if (b1p01e03>=2 & b1p01e03<=4) & (b1p01e01a!=7 | b1p01e01a!=6)

**se agregan las variables a nivel de hogar**

**Para conservar los missing values**
global miss "aux_ing_ct_mon aux_ing_ct_nomon aux_ing_rem_mon aux_ing_rem_nomon aux_ing_trspri_mon aux_ing_trspri_nomon"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}

collapse (sum) aux_ing_ct_mon aux_ing_ct_nomon aux_ing_rem_mon aux_ing_rem_nomon aux_ing_trspri_mon aux_ing_trspri_nomon (count) aux_ing_ct_mon_miss aux_ing_ct_nomon_miss aux_ing_rem_mon_miss aux_ing_rem_nomon_miss aux_ing_trspri_mon_miss aux_ing_trspri_nomon_miss, by(formulario) 

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}	

saveold "$RAW\ingresos_asistencia_social_hogar.dta" , replace 

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\data_orig\base_de_datos_personas.dta", clear 
	
         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*
*Trabajadores en relacion de dependencia
global varanuales "b1p02b13b b1p02b14b b1p02b15b b1p02b16b b1p02b17b b1p02b18b b1p02b20 b1p02c12 b1p02c08b b1p02c09b b1p02c10b b1p03c01b b1p03c02b b1p03c03b b1p03c04b b1p03c05b b1p03c06b b1p03c07b b1p03c08b b1p03c09b b1p03d01b b1p03d02b"
foreach var in $varanuales {
gen `var'_men= `var'/12
}
egen ing_lab_mon_asal = rowtotal(b1p02b06 b1p02b07b b1p02b08b b1p02b13b_men b1p02b14b_men b1p02b15b_men b1p02b16b_men b1p02b17b_men b1p02b18b_men b1p02c05 b1p02c06b b1p02c08b_men b1p02c09b_men b1p02c10b_men b1p03d01b_men), m
*Trabajadores por cuenta propia
egen ing_lab_mon_cp=rowtotal (b1p02b19 b1p02c11), m
*Trabajadores por cuenta propia agropecuarios 
egen ing_lab_mon_agri = rowtotal(b1p02b20_men b1p02c12_men b1p03d02b_men), m

egen ing_lab_mon = rowtotal(ing_lab_mon_asal ing_lab_mon_cp ing_lab_mon_agri), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*
global vartrimestrales "b1p03a01b b1p03a02b b1p03a03b b1p03a04b b1p03a05b b1p03a06b b1p03a07b b1p03a08b"
foreach var in $vartrimestrales {
gen `var'_men= `var'/3
}
egen ing_ren_mon= rowtotal(b1p03a01b_men b1p03a02b_men b1p03a03b_men), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*
egen aux2_ing_trspri_mon= rowtotal(b1p03a04b_men b1p03a05b_men), missing

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*
gen aux2_ing_ct_mon=b1p03a07b_men

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*
egen ing_trsgob_mon= rowtotal(b1p03a06b_men b1p03a08b_men), m

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*
global varsemestrales "b1p03b02b b1p03b03b b1p03b04b b1p03b05b b1p03b06b b1p03b07b"
foreach var in $varsemestrales {
gen `var'_men= `var'/6
}
egen aux2_ing_rem_mon= rowtotal(b1p03b02b_men b1p03b03b_men b1p03b04b_men b1p03b05b_men b1p03b06b_men b1p03b07b_men), m

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*
egen ing_otro_mon= rowtotal(b1p03c01b_men b1p03c02b_men b1p03c03b_men b1p03c04b_men b1p03c05b_men b1p03c06b_men b1p03c07b_men b1p03c08b_men b1p03c09b_men), m


         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*

**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*
*Trabajadores en relacion de dependencia
egen ing_lab_nomon = rowtotal(b1p02b09b b1p02b10b b1p02b11b b1p02b12b b1p02c07b), m

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*
*Se importará de otra base

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*
*Se importará de otra base

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*
*Se importará de otra base

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*--------------------------------------------------*

gen ing_trsgob_nomon= .

*--------------------------------------------------*
* INGRESO NO MONETARIO POR REMESAS INTERNACIONALES * 
*--------------------------------------------------*
*Se importará de otra base

*------------------------------------------------------------------*
* INGRESO NO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------------------*

gen ing_otro_nomon=. 


**se agregan las variables a nivel de hogar**

**Para conservar los missing values**
global miss "ing_lab_mon ing_ren_mon aux2_ing_trspri_mon aux2_ing_ct_mon ing_trsgob_mon aux2_ing_rem_mon ing_otro_mon ing_lab_nomon ing_trsgob_nomon ing_otro_nomon"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}

collapse (sum) ing_lab_mon ing_ren_mon aux2_ing_trspri_mon aux2_ing_ct_mon ing_trsgob_mon aux2_ing_rem_mon ing_otro_mon ing_lab_nomon ing_trsgob_nomon ing_otro_nomon (count) ing_lab_mon_miss ing_ren_mon_miss aux2_ing_trspri_mon_miss aux2_ing_ct_mon_miss ing_trsgob_mon_miss aux2_ing_rem_mon_miss ing_otro_mon_miss ing_lab_nomon_miss ing_trsgob_nomon_miss ing_otro_nomon_miss, by(formulario) 

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}

	
	merge 1:1 formulario using "$RAW\data_orig\base_de_datos_hogares.dta", keepusing(b1p01b02 factor_expansion_2)
	drop _merge
	gen ing_ren_nomon=b1p01b02
	drop b1p01b02
	
	merge 1:1 formulario using "$RAW\ingresos_asistencia_social_hogar.dta"
	drop _merge

egen ing_trspri_mon=rowtotal(aux_ing_trspri_mon aux2_ing_trspri_mon), missing
drop aux_ing_trspri_mon aux2_ing_trspri_mon 
egen ing_ct_mon=rowtotal(aux_ing_ct_mon aux2_ing_ct_mon), m
drop aux_ing_ct_mon aux2_ing_ct_mon	
egen ing_rem_mon= rowtotal(aux_ing_rem_mon aux2_ing_rem_mon), m
drop aux_ing_rem_mon aux2_ing_rem_mon
gen ing_trspri_nomon = aux_ing_trspri_nomon
drop aux_ing_trspri_nomon
gen ing_ct_nomon= aux_ing_ct_nomon
drop aux_ing_ct_nomon
gen ing_rem_nomon=aux_ing_rem_nomon
drop aux_ing_rem_nomon

*--------------------*
* INGRESO MONETARIO  *  
*--------------------*
egen ing_mon= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing

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
	use "$RAW\data_orig\b02_base_alimentos_seccion_a.dta", clear 
	
	*----------------------------------*
	* GASTO EN SERVICIOS DE TRANSPORTE *
	*----------------------------------*
	gen transporte=0
	replace transporte=1 if b2p04a01==732104 | b2p04a01==732105
	recode alimentos_y_beb_no_alco 1=0 if transporte==1
	gen gasto_tserv_sem=b2p01a06 if transporte==1
	gen gasto_tserv_aux=gasto_tserv_sem*4
		
	*-----------------------------------------------------*
	* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
	*-----------------------------------------------------*
	gen gasto_alihogar_sem=b2p01a06 if alimentos_y_beb_no_alco==1
	gen gasto_alihogar=gasto_alihogar_sem*4
	
	*---------------------------------------------------------*
	* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
	*---------------------------------------------------------*	
	gen gasto_alifuera_sem=b2p01a06 if res_y_hotel==1
	gen gasto_alifuera=gasto_alifuera_sem*4
	
	*---------------------------------------*
	* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
	*---------------------------------------*
	gen gasto_alta_sem=b2p01a06 if beb_alco_tab_y_estu==1
	gen gasto_alta=gasto_alta_sem*4	
	
				
	**se agregan las variables a nivel de hogar**

	**Para conservar los missing values**
	global miss "gasto_alihogar gasto_alifuera gasto_alta gasto_tserv_aux"
	foreach var in $miss {
	egen `var'_miss= count(`var') if `var'!=.
	}

	collapse (sum) gasto_alihogar gasto_alifuera gasto_alta gasto_tserv_aux (count) gasto_alihogar_miss gasto_alifuera_miss gasto_alta_miss gasto_tserv_aux_miss, by(formulario) 

	foreach var in $miss {
	replace `var'=. if `var'_miss==0
	drop `var'_miss
	}

	*-------------------------------*
	* GASTO EN ALIMENTOS Y BEBIDAS  *
	*-------------------------------*
	egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), m
	
	saveold "$RAW\gastos_alimentos_mensualizados_hogar.dta", replace
	
	use "$RAW\data_orig\b03_gastos_anuales.dta", clear 	
	gen gtomensual=b3p04a06/12
	gen ccif=b3p04a01
	saveold "$RAW\gastos_anuales_mensualizados.dta", replace 
	
	use "$RAW\data_orig\b03_gastos_semestrales.dta", clear	
	gen gtomensual=b3p03a06/6
	gen ccif=b3p03a01
	saveold "$RAW\gastos_semestrales_mensualizados.dta", replace 
	
	use "$RAW\data_orig\b03_gastos_trimestrales.dta", clear	
	gen gtomensual=b3p02a06/3
	gen ccif=b3p02a01
	saveold "$RAW\gastos_trimestrales_mensualizados.dta", replace
	
	use "$RAW\data_orig\b03_gastos_mensuales.dta", clear
	gen aux=b3p01b07/12
	gen aux2=b3p01b08/12	
	egen gtomensual=rowtotal(b3p01a06 b3p01b05 b3p01b06 aux aux2), m
	drop aux aux2
	gen ccif=b3p01a01
		
	append using "$RAW\gastos_anuales_mensualizados.dta"
	append using "$RAW\gastos_semestrales_mensualizados.dta"
	append using "$RAW\gastos_trimestrales_mensualizados.dta"
	
	sort formulario
	
	merge m:1 formulario using "$RAW\gastos_alimentos_mensualizados_hogar.dta", keepusing (gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_tserv_aux)
	drop _merge
	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
 
*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*
bys formulario: egen gasto_veca= total(gtomensual) if ccif>=300000 & ccif<400000, m

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*
bys formulario: egen gasto_vcon_aux= total(gtomensual) if ccif>=400000 & ccif<440000, m
bys formulario: egen gasto_vcon_aux2= total(gtomensual) if ccif>=442000 & ccif<450000, m
bys formulario: egen gasto_vcon_aux3= total(gtomensual) if ccif>=461109 & ccif<=462106, m

*---------------*
* GASTO EN AGUA *
*---------------*
bys formulario: egen gasto_vag= total(gtomensual) if ccif>=441000 & ccif<442000

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*
bys formulario: egen gasto_vele= total(gtomensual) if ccif>=451000 & ccif<452000

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*
gen gasto_vgn=.

*-----------------------------------------------------------*
* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
*-----------------------------------------------------------*
gen gasto_vp=.
gen gasto_vgas=.
gen gasto_vk=.

egen gasto_vpgk= rowtotal(gasto_vp gasto_vgas gasto_vk), m

*-----------------------------------------------------*
* GASTO EN GAS LICUADO DE PETRÓLEO PARA USO DOMÉSTICO *
*-----------------------------------------------------*
bys formulario: egen gasto_vlp=total(gtomensual) if ccif>=452100 & ccif<452103

*------------------------*
* GASTO EN LENIA Y CARBON *
*------------------------*
bys formulario: egen gasto_vca= total(gtomensual) if ccif==452104
bys formulario: egen gasto_vle= total(gtomensual) if ccif==452103

*------------------------------------*
* GASTO EN DIESEL PARA USO DOMÉSTICO *
*------------------------------------*
gen gasto_vdi=.

*---------------------------------------------*
*Otros gastos en combustibles de uso domestico*
*---------------------------------------------*
bys formulario: egen gasto_vot=total(gtomensual) if ccif==452105

*-----------------------------*
* OTROS GASTOS no desglozados *
*-----------------------------*
gen gasto_votot=. 

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*
bys formulario: egen gasto_ens= total(gtomensual) if ccif>=511000 & ccif<611101

*----------------*
* GASTO EN SALUD *
*----------------*
bys formulario: egen gasto_sal_aux=total(gtomensual) if ccif>=611101 & ccif<=641106
bys formulario: egen gasto_sal_aux2= total(gtomensual) if ccif>=1251102 & ccif<=1251105 
bys formulario: egen gasto_sal_aux3= total(gtomensual) if ccif>=1252101 & ccif<=1252106

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*
bys formulario: egen gasto_com= total(gtomensual) if ccif>=811101  & ccif<=841102  

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*
bys formulario: egen gasto_tserv_aux2=total(gtomensual) if ccif>=731133 & ccif<=736102
bys formulario: egen gasto_tserv_aux3=total(gtomensual) if ccif==1271143
	
*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*
bys formulario: egen gasto_tga= total(gtomensual) if ccif>=722101 & ccif<=722102

*----------------------------------------------*
* Gasto en gas licuado de petróleo-transporte  *
*----------------------------------------------*
bys formulario: egen gasto_tlp=total(gtomensual) if ccif==722104

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*
bys formulario: egen gasto_tdie= total(gtomensual) if ccif==722103

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

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*
bys formulario: egen gasto_tman_aux= total(gtomensual) if ccif>=721101 & ccif<=721199
bys formulario: egen gasto_tman_aux2= total(gtomensual) if ccif>=722105 & ccif<=723199

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*
bys formulario: egen gasto_tadq= total(gtomensual) if ccif>=711101 & ccif<=711116 

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*
bys formulario: egen gasto_totros_aux= total(gtomensual) if ccif>=724101 & ccif<=724105
bys formulario: egen  gasto_totros_aux2= total(gtomensual) if ccif>1241137 & ccif<=1251101
bys formulario: egen  gasto_totros_aux3= total(gtomensual) if ccif>=1271199 & ccif<=1281209

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*
bys formulario: egen gasto_edre= total(gtomensual) if ccif>=911101 & ccif<=1171137

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	
bys formulario: egen gasto_otros_aux= total(gtomensual) if ccif>=1211101 & ccif<=1241137
bys formulario: egen gasto_otros_aux2= total(gtomensual) if ccif>=1271102 & ccif<=1271162
bys formulario: egen gasto_otros_aux3= total(gtomensual) if ccif>=1281216  & ccif<=1285101

				
	**se agregan las variables a nivel de hogar**

	**Para conservar los missing values**
	global miss "gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_tserv_aux gasto_veca gasto_vcon_aux gasto_vcon_aux2 gasto_vcon_aux3 gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vp gasto_vgas gasto_vk gasto_vlp gasto_vca gasto_vle gasto_vdi gasto_vot gasto_votot gasto_ens gasto_sal_aux gasto_sal_aux2 gasto_sal_aux3 gasto_com gasto_tserv_aux2 gasto_tserv_aux3 gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman_aux gasto_tman_aux2 gasto_tadq gasto_totros_aux gasto_totros_aux2 gasto_totros_aux3 gasto_edre gasto_otros_aux gasto_otros_aux2 gasto_otros_aux3"
	foreach var in $miss {
	egen `var'_miss= count(`var') if `var'!=.
	}

	collapse (firstnm) gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_tserv_aux gasto_veca gasto_vcon_aux gasto_vcon_aux2 gasto_vcon_aux3 gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vp gasto_vgas gasto_vk gasto_vlp gasto_vca gasto_vle gasto_vdi gasto_vot gasto_votot gasto_ens gasto_sal_aux gasto_sal_aux2 gasto_sal_aux3 gasto_com gasto_tserv_aux2 gasto_tserv_aux3 gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman_aux gasto_tman_aux2 gasto_tadq gasto_totros_aux gasto_totros_aux2 gasto_totros_aux3 gasto_edre gasto_otros_aux gasto_otros_aux2 gasto_otros_aux3 (count) gasto_ali_miss gasto_alihogar_miss gasto_alifuera_miss gasto_alta_miss gasto_tserv_aux_miss gasto_veca_miss gasto_vcon_aux_miss gasto_vcon_aux2_miss gasto_vcon_aux3_miss gasto_vag_miss gasto_vele_miss gasto_vgn_miss gasto_vpgk_miss gasto_vp_miss gasto_vgas_miss gasto_vk_miss gasto_vlp_miss gasto_vca_miss gasto_vle_miss gasto_vdi_miss gasto_vot_miss gasto_votot_miss gasto_ens_miss gasto_sal_aux_miss gasto_sal_aux2_miss gasto_sal_aux3_miss gasto_com_miss gasto_tserv_aux2_miss gasto_tserv_aux3_miss gasto_tga_miss gasto_tlp_miss gasto_tdie_miss gasto_tgnc_miss gasto_talc_miss gasto_totcomb_miss gasto_tman_aux_miss gasto_tman_aux2_miss gasto_tadq_miss gasto_totros_aux_miss gasto_totros_aux2_miss gasto_totros_aux3_miss gasto_edre_miss gasto_otros_aux_miss gasto_otros_aux2_miss gasto_otros_aux3_miss, by(formulario) 

	foreach var in $miss {
	replace `var'=. if `var'_miss==0
	drop `var'_miss
	}

egen gasto_vcon=rowtotal(gasto_vcon_aux gasto_vcon_aux2 gasto_vcon_aux3), m 
drop gasto_vcon_aux gasto_vcon_aux2 gasto_vcon_aux3
egen gasto_vleca= rowtotal(gasto_vca gasto_vle), missing 
*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*
egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc gasto_totcomb), missing 
*----------------------------------------------------------------------------*
*Gasto en vivienda, servicios de conservación y combustibles de uso doméstico*
*----------------------------------------------------------------------------*
egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot gasto_votot), missing 
egen gasto_sal= rowtotal(gasto_sal_aux gasto_sal_aux2 gasto_sal_aux3), m 
drop gasto_sal_aux gasto_sal_aux2 gasto_sal_aux3
egen gasto_tserv= rowtotal(gasto_tserv_aux gasto_tserv_aux2 gasto_tserv_aux3), m 
drop gasto_tserv_aux gasto_tserv_aux2 gasto_tserv_aux3
egen gasto_tman= rowtotal(gasto_tman_aux gasto_tman_aux2), m 
drop gasto_tman_aux gasto_tman_aux2
egen gasto_totros= rowtotal(gasto_totros_aux gasto_totros_aux2 gasto_totros_aux3), m 
drop gasto_totros_aux gasto_totros_aux2 gasto_totros_aux3
egen gasto_otros= rowtotal(gasto_otros_aux gasto_otros_aux2 gasto_otros_aux3), m 
drop gasto_otros_aux gasto_otros_aux2 gasto_otros_aux3
*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*
egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman gasto_tadq gasto_totros), missing 
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
mmerge formulario using "$RAW\ingresos_hogar.dta", t(1:1)
drop _merge
replace ict=0 if ict==.	
**Se hace el merge para obtener cant de miembros del hogar**
merge 1:1 formulario using "$RAW\datos_hogar.dta", keepusing(miembros_hogar)
drop _merge

 
keep formulario factor_expansion_2 miembros_hogar gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_votot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "GTM"	
label var pais "Pais de la Encuesta"

gen anio= "2009-2010"
label var anio "Anio de la Encuesta"

gen encuesta = "ENIGFAM"
label var encuesta "Encuesta"

rename formulario cod_hogar
label var cod_hogar "Codigo de hogar"

rename factor_expansion_2 factor_expansion
label var factor_expansion "Factor de expansion" 

order  pais anio encuesta cod_hogar factor_expansion miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_votot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\GTM_ENIGFAM_2009-2010.dta", replace


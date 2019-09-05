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

2) Fecha: Noviembre 2018

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los shocks climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático			 
5) País: Honduras

6) Encuesta: Encuesta de Condiciones de vida (ENCOVI)

7) Ano: 2004					

8) Inputs: 	$RAW\hnd04-encovi.dta 
			$RAW\gasto en alimentos,bebidas y tabaco.dta
			$RAW\rubros.dta
			
             
6) Outputs: $data_arm\HND_ENCOVI_2004.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\HND\ENCOVI\RAW"
global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\HND\ENCOVI\data_arm"


/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*
use "$RAW\hnd04-encovi.dta", clear	
		  
**Composicion del ingreso monetario**
drop if nper!=1 /*Todas las variables de la pregunta 13 a la pregunta  53 estarán validas solo a nivel de la persona 1,  que indica que es hogar. 
				Por lo tanto el resto de registros que se refiere a otras personas u  otros hogares de la vivienda están en blanco*/


*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*
egen ing_lab_mon = rowtotal(ysmophg ycmophg ysmoshg ycmoshg ysmothg ycmothg yoaophg yoaoshg yoaothg), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*
egen ing_ren_mon = rowtotal(yalquhg yintbhg ybonohg), m

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*
egen ing_trspri_mon=rowtotal(ypordhg yayufahg yayupahg yherehg), m

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*
egen ing_ct_mon=rowtotal(ysubshg yprelhg), m

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

gen ing_trsgob_mon= ypenjuhg

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= yremexhg

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*
egen ing_otro_mon=rowtotal(ylotjhg ycomphg yotrohg), m

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

egen ing_lab_nomon = rowtotal(yseophg yceophg yseoshg yceoshg yseothg yceothg yauto), m

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*
local tdc = 18.41 //Tipo de cambio promedio Lempiras por dólar año 2004
gen alqimp=.
replace alqimp=s1p16b if s1p16a==1 | s1p16a==9
replace alqimp=s1p16b * `tdc'  if s1p16a==2
gen ing_ren_nomon = alqimp

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

gen ing_trspri_nomon = .

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

gen ing_ct_nomon= .


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

keep ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro totper factor hogar

saveold "$RAW\ingresos_hogar.dta" , replace 

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\hnd04-encovi.dta", clear	
	drop if nper!=1 	
	local tdc = 18.41 //Tipo de cambio promedio Lempiras por dólar año 2004
	gen aux1=s1p15a2
	replace aux1=s1p15a2*`tdc' if s1p15a1==2
	gen aux2=s1p16b
	replace aux2=s1p16b*`tdc' if s1p16a==2
	egen gasto_vconaux1=rowtotal(aux1 aux2), m
	drop aux1 aux2
	gen gasto_vconaux2=s1p37b
	replace gasto_vconaux2=s1p37b/12 if s1p37a==2
	egen gasto_vcon1= rowtotal(gasto_vconaux1 gasto_vconaux2), m 
	drop gasto_vconaux1 gasto_vconaux2	
	gen gasto_vag= s1p26	
	gen gasto_vele=s1p40	
	gen gasto_vot1=s1p41
	gen gasto_vot2=s1p52 if s1p44==3 | s1p44==6
	egen gasto_vot= rowtotal(gasto_vot1 gasto_vot2), m
	drop gasto_vot1 gasto_vot2	
	gen gasto_vlp=s1p52 if s1p44==4
	gen gasto_vk=s1p52 if s1p44==5
	gen gasto_vle=s1p52 if s1p44==1	
	gen aux1=s1p53adl*`tdc'
	gen aux2=s1p53bdl*`tdc'
	egen gasto_com1=rowtotal(s1p53alp aux1 s1p53blp aux2), m
	recode gasto_com1 -9=.
	drop aux1 aux2		
	foreach x in s1p53cdl s1p53c2d s1p53ddl s1p53edl {
		gen aux`x'=`x'*`tdc'
	}
	egen gasto_edre1=rowtotal(s1p53cdl s1p53c2d s1p53ddl s1p53edl auxs1p53cdl auxs1p53c2d auxs1p53ddl auxs1p53edl), m 
	drop auxs1p53cdl auxs1p53c2d auxs1p53ddl auxs1p53edl	
	keep gasto_edre1 gasto_com1 gasto_vle gasto_vk gasto_vlp gasto_vot gasto_vele gasto_vag gasto_vcon1 totper factor hogar 
	save "$RAW\gastos_hogar1.dta" , replace
	
	use "$RAW\hnd04-encovi.dta", clear
	foreach x in s4p12a s4p12b s4p12c s4p15a s4p15b s4p15c s4p13a s4p13b s4p13c s4p13d s4p13e s4p13f s4p13g s4p17a s4p17b s4p17c s4p17d s4p17e ///
				 s4p56a s4p56b s4p56c s4p56d s4p56e s4p57a s4p57b s4p57c s4p57d s4p57e s4p57f s4p57g s4p66a s4p66b s4p66c s4p66d s4p66e{
		recode `x' 9999=.
	}
	foreach x in s4p13a s4p13b s4p13c s4p13d s4p13e s4p13f s4p13g s4p17a s4p17b s4p17c s4p17d s4p17e s4p57a s4p57b s4p57c s4p57d s4p57e s4p57f s4p57g s4p59 s4p66a s4p66b s4p66c s4p66d s4p66e {
	gen `x'_men=`x'/12
	}
	gen s4p51_men=s4p51*4	
	egen gasto_edre2aux=rowtotal(s4p12a s4p12b s4p12c s4p15a s4p15b s4p15c s4p13a_men s4p13b_men s4p13c_men s4p13d_men s4p13e_men s4p13f_men s4p13g_men s4p17a_men s4p17b_men s4p17c_men s4p17d_men s4p17e_men s4p56a s4p56b s4p56c s4p56d s4p56e s4p57a_men s4p57b_men s4p57c_men s4p57d_men s4p57e_men s4p57f_men s4p57g_men s4p59_men s4p61 s4p64a s4p64b s4p64c s4p64d s4p66a_men s4p66b_men s4p66c_men s4p66d_men s4p66e_men), m
	bys hogar: egen gasto_edre2= total(gasto_edre2aux), m
	replace gasto_edre2=. if gasto_edre2<0
	drop gasto_edre2aux	
	egen gasto_alifueraaux=rowtotal(s4p08 s4p10), m
	bys hogar: egen gasto_alifuera1= total(gasto_alifueraaux), m
	drop gasto_alifueraaux	
	collapse (firstnm) gasto_edre2 gasto_alifuera1 factor totper, by(hogar)	
	save "$RAW\gastos_hogar2.dta" , replace
	
	use "$RAW\gasto en alimentos,bebidas y tabaco.dta", clear	
	gen aux1=lpsmen03 if (rubro>=1 & rubro<=93) | (rubro>=95 & rubro<=128) | rubro==135
	bys hogar: egen gasto_alihogar=total(aux1), m   
	drop aux1
	gen aux2=lpsmen03 if (rubro>=129 & rubro<=134) 
	bys hogar: egen gasto_alta=total(aux2), m   
	drop aux2
	gen aux3=lpsmen03 if rubro==94 
	bys hogar: egen gasto_alifuera2=total(aux3), m   
	drop aux3	
	collapse (firstnm) gasto_alihogar gasto_alta gasto_alifuera2 factor totper, by(hogar)
	save "$RAW\gastos_hogar3.dta" , replace
	
	use "$RAW\rubros.dta", clear	
	gen aux1=commes if (rubro>=410 & rubro<=411) | rubro==101
	gen aux2=donmes if (rubro>=410 & rubro<=411) | rubro==101
	egen gasto_tservaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_tserv=total(gasto_tservaux), m
	drop aux1 aux2 gasto_tservaux
	gen aux1=commes if rubro==105
	gen aux2=donmes if rubro==105
	egen gasto_tcombaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_tcomb=total(gasto_tcombaux), m
	drop aux1 aux2 gasto_tcombaux
	gen aux1=commes if (rubro>=227 & rubro<=231) | rubro==106 | rubro==318 | rubro==419 | rubro==426
	gen aux2=donmes if (rubro>=227 & rubro<=231) | rubro==106 | rubro==318 | rubro==419 | rubro==426
	egen gasto_totrosaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_totros=total(gasto_totrosaux), m
	drop aux1 aux2 gasto_totrosaux
	gen aux1=commes if rubro==428 | rubro==102 | (rubro>=223 & rubro<=226) | (rubro>=417 & rubro<=418)
	gen aux2=donmes if rubro==428 | rubro==102 | (rubro>=223 & rubro<=226) | (rubro>=417 & rubro<=418)
	egen gasto_edreaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_edre3=total(gasto_edreaux), m
	drop aux1 aux2 gasto_edreaux
	gen aux1=commes if rubro==103 | rubro==104 
	gen aux2=donmes if rubro==103 | rubro==104
	egen gasto_comaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_com2=total(gasto_comaux), m
	drop aux1 aux2 gasto_comaux
	gen aux1=commes if (rubro>=201 & rubro<=222) | (rubro>=412 & rubro<=414) | rubro==416 | (rubro>=420 & rubro<=422) | (rubro>=424 & rubro<=425) | rubro==427
	gen aux2=donmes if (rubro>=201 & rubro<=222) | (rubro>=412 & rubro<=414) | rubro==416 | (rubro>=420 & rubro<=422) | (rubro>=424 & rubro<=425) | rubro==427
	egen gasto_otrosaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_otros=total(gasto_otrosaux), m
	drop aux1 aux2 gasto_otrosaux
	gen aux1=commes if (rubro>=301 & rubro<=309) 
	gen aux2=donmes if (rubro>=301 & rubro<=309) 
	egen gasto_vecaaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_veca=total(gasto_vecaaux), m
	drop aux1 aux2 gasto_vecaaux
	gen aux1=commes if (rubro>=401 & rubro<=406) | (rubro>=310 & rubro<=315) | rubro==317
	gen aux2=donmes if (rubro>=401 & rubro<=406) | (rubro>=310 & rubro<=315) | rubro==317 
	egen gasto_ensaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_ens=total(gasto_ensaux), m
	drop aux1 aux2 gasto_ensaux
	gen aux1=commes if rubro==316
	gen aux2=donmes if rubro==316 
	egen gasto_tmanaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_tman=total(gasto_tmanaux), m
	drop aux1 aux2 gasto_tmanaux
	gen aux1=commes if (rubro>=407 & rubro<=409)
	gen aux2=donmes if (rubro>=407 & rubro<=409) 
	egen gasto_tadqaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_tadq=total(gasto_tadqaux), m
	drop aux1 aux2 gasto_tadqaux
	gen aux1=commes if rubro==415
	gen aux2=donmes if rubro==415 
	egen gasto_salaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_sal=total(gasto_salaux), m
	drop aux1 aux2 gasto_salaux
	gen aux1=commes if rubro==423
	gen aux2=donmes if rubro==423 
	egen gasto_vconaux=rowtotal(aux1 aux2), m
	bys hogar: egen gasto_vcon2=total(gasto_vconaux), m
	drop aux1 aux2 gasto_vconaux	
	collapse (firstnm) gasto_tserv gasto_tcomb gasto_totros gasto_edre3 gasto_com2 gasto_otros gasto_veca gasto_ens gasto_tman gasto_tadq gasto_sal gasto_vcon2 factor, by(hogar)
	save "$RAW\gastos_hogar4.dta" , replace
	
	foreach file in "$RAW\gastos_hogar1.dta" "$RAW\gastos_hogar2.dta" "$RAW\gastos_hogar3.dta" {
                merge 1:1 hogar using "`file'"
				drop _merge
        }
 
 egen gasto_vcon=rowtotal(gasto_vcon1 gasto_vcon2), m
 drop gasto_vcon1 gasto_vcon2
 egen gasto_edre=rowtotal(gasto_edre1 gasto_edre2 gasto_edre3), m
 drop gasto_edre1 gasto_edre2 gasto_edre3
 egen gasto_alifuera=rowtotal(gasto_alifuera1 gasto_alifuera2)
 drop gasto_alifuera1 gasto_alifuera2
 egen gasto_com=rowtotal(gasto_com1 gasto_com2), m
 drop gasto_com1 gasto_com2

			
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*
egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*
gen gasto_vgn=. 

*-----------------------------------------------------------*
* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
*-----------------------------------------------------------*
gen gasto_vp=.
gen gasto_vgas=.

gen gasto_vpgk=gasto_vk  

*------------------------*
* GASTO EN LENIA Y CARBON *
*------------------------*
gen gasto_vca= . 

gen gasto_vleca= gasto_vle

*------------------------------------*
* GASTO EN DIESEL PARA USO DOMÉSTICO *
*------------------------------------*
gen gasto_vdi=.


*----------------------------------------------------------------------------*
*Gasto en vivienda, servicios de conservación y combustibles de uso doméstico*
*----------------------------------------------------------------------------*
egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot), missing


*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*
gen gasto_tga=. 

*----------------------------------------------*
* Gasto en gas licuado de petróleo-transporte  *
*----------------------------------------------*
gen gasto_tlp=.

*-----------------------------*
* GASTO EN DIESEL-TRANSPORTE  * 
*-----------------------------*
gen gasto_tdie=.

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
gen gasto_totcomb= .

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
merge 1:1 hogar using "$RAW\ingresos_hogar.dta"
drop _merge

keep hogar factor totper gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "HND"	
label var pais "Pais de la Encuesta"

gen anio=2004
label var anio "Anio de la Encuesta"

gen encuesta = "ENCOVI"
label var encuesta "Encuesta"

rename hogar cod_hogar
label var cod_hogar "Codigo de hogar"

rename totper miembros_hogar 
label var miembros_hogar "Cantidad de miembros en el hogar"

rename factor factor_expansion
label var factor_expansion "Factor de expansion con consumo" 

order  pais anio encuesta cod_hogar factor_expansion miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\HND_ENCOVI_2004.dta" , replace


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

2) Fecha: Septiembre 2018

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los shocks climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático			 
5) País: Nicaragua

6) Encuesta: Encuesta de Medición de Nivel de Vida (EMNV)

7) Ano: 2014					

8) Inputs: 	$RAW\emnv14_02_datos_de_la_vivienda_y_el_hogar.dta 
			$RAW\emnv14_03_seccion_1_programas_sociales.dta
			$RAW\emnv14_04_poblacion.dta
			$RAW\emnv14_05_negocios.dta 
			$RAW\emnv14_08_parte_a_de_la_seccion_7.dta
			$RAW\emnv14_09_parte_b1_de_la_seccion_7.dta
			$RAW\emnv14_10_parte_b2_de_la_seccion_7.dta
			$RAW\emnv14_11_parte_b3_de_la_seccion_7.dta
			$RAW\emnv14_12_parte_b4_de_la_seccion_7.dta
			$RAW\emnv14_13_parte_c1_de_la_seccion_7.dta 
			$RAW\emnv14_14_parte_c2_de_la_seccion_7.dta
			$RAW\emnv14_15_parte_c3_de_la_seccion_7.dta
			$RAW\emnv14_16_parte_c4_de_la_seccion_7.dta
			$RAW\emnv14_17_parte_c5_de_la_seccion_7.dta
             
6) Outputs: $data_arm\NIC_EMNV_2014.dta 							

7) Version: Stata 13.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
						
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\NIC\EMNV\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\NIC\EMNV\data_arm"

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\emnv14_02_datos_de_la_vivienda_y_el_hogar.dta", clear	
	recode s1p14a 99998 99999=.
	recode s1p14b 99998 99999=.
	gen s1p14b_cor=s1p14b * 26 //Cotizacion Cordoba nicaraguense por usd promedio 2014= 26 nio/usd
	egen ing_ren_nomon_1= rowtotal (s1p14a s1p14b_cor), m	
	keep i00 dominio4 i06 ing_ren_nomon_1 peso2 peso3	
	save "$RAW\alquileres.dta" , replace

	use "$RAW\emnv14_03_seccion_1_programas_sociales.dta", clear 
	recode s1p35 999998 999999=.
	recode s1p34c 998 999=.
	gen ing_ct_0=(s1p35*s1p34c)/12
	bys i00: egen ing_ct_1=total(ing_ct_0), missing
	collapse (firstnm) ing_ct_1, by(i00)	
	save "$RAW\ing_ct.dta", replace
	
	use "$RAW\emnv14_05_negocios.dta", clear
	recode s5p64 998 999=.
	recode s5p66f 9999998 9999999=.
	gen ing_lab_0=(s5p64/100*s5p66f)
	bys i00: egen ing_lab_1=total(ing_lab_0), m
	collapse (firstnm) ing_lab_1, by(i00)	
	save "$RAW\ing_negocios.dta", replace
	
	use "$RAW\emnv14_04_poblacion.dta", clear
	bys i00: egen miembros_hogar=count(i00)
	label var miembros_hogar "Número de miembros del hogar"
	
	foreach var of varlist s5p19a s5p35a s5p50a s5p52b s5p21b s5p37b s5p25b s5p41b s5p53b s5p26a s5p42a s5p54a s5p57b {
	recode `var'  9999998 9999999=.
	}
	gen sal_1=.	
	replace sal_1=s5p19a * 22 if s5p19b==1
	replace sal_1=s5p19a * 4 if s5p19b==2
	replace sal_1=s5p19a * 2 if (s5p19b>=3 & s5p19b<=4)
	replace sal_1=s5p19a if s5p19b==5
	replace sal_1=s5p19a/3 if s5p19b==6
	replace sal_1=s5p19a/6 if s5p19b==7
	replace sal_1=s5p19a/12 if s5p19b==8
	gen sal_2=.	
	replace sal_2=s5p35a * 22 if s5p35b==1
	replace sal_2=s5p35a * 4 if s5p35b==2
	replace sal_2=s5p35a * 2 if (s5p35b>=3 & s5p35b<=4)
	replace sal_2=s5p35a if s5p35b==5
	replace sal_2=s5p35a/3 if s5p35b==6
	replace sal_2=s5p35a/6 if s5p35b==7
	replace sal_2=s5p35a/12 if s5p35b==8	
	gen sal_3=.	
	replace sal_3=s5p50a * 4 if s5p50b==1
	replace sal_3=s5p50a * 2 if (s5p50b>=2 & s5p50b<=3)
	replace sal_3=s5p50a if s5p50b==4
	replace sal_3=s5p50a/3 if s5p50b==5
	replace sal_3=s5p50a/6 if s5p50b==6
	replace sal_3=s5p50a/12 if s5p50b==7
	egen sal=rowtotal(sal_1 sal_2 sal_3), missing
	
	gen extras_3=s5p52b/12
	egen extras=rowtotal(s5p20b s5p36b s5p51b extras_3), m
	
	gen aguinaldo_1=.
	replace aguinaldo_1=s5p21b/s5p21c if s5p21c<98
	gen aguinaldo_2=.
	replace aguinaldo_2=s5p37b/s5p37c if s5p37c<98
	egen aguinaldo=rowtotal(aguinaldo_1 aguinaldo_2), missing
	
	recode s5p25c 99 98=.
	recode s5p41c 99 98=.
	gen extras_uniforme=(s5p25b*s5p25c)/12
	gen extras_uniforme_2=(s5p41b*s5p41c)/12	
	gen extras_especies_3=s5p53b/12
	egen extras_especies=rowtotal(s5p22b s5p23b s5p24b s5p38b s5p39b s5p40b extras_especies_3 extras_uniforme extras_uniforme_2), missing
	
	gen ing_ctapropia_1=.
	replace ing_ctapropia_1=s5p26a * 22 if s5p26b==1
	replace ing_ctapropia_1=s5p26a * 4 if s5p26b==2
	replace ing_ctapropia_1=s5p26a * 2 if (s5p26b>=3 & s5p26b<=4)
	replace ing_ctapropia_1=s5p26a if s5p26b==5
	replace ing_ctapropia_1=s5p26a/3 if s5p26b==6
	replace ing_ctapropia_1=s5p26a/6 if s5p26b==7
	replace ing_ctapropia_1=s5p26a/12 if s5p26b==8	
	gen ing_ctapropia_2=.
	replace ing_ctapropia_2=s5p42a * 22 if s5p42b==1
	replace ing_ctapropia_2=s5p42a * 4 if s5p42b==2
	replace ing_ctapropia_2=s5p42a * 2 if (s5p42b>=3 & s5p42b<=4)
	replace ing_ctapropia_2=s5p42a if s5p42b==5
	replace ing_ctapropia_2=s5p42a/3 if s5p42b==6
	replace ing_ctapropia_2=s5p42a/6 if s5p42b==7
	replace ing_ctapropia_2=s5p42a/12 if s5p42b==8
	gen ing_ctapropia_3=.
	replace ing_ctapropia_3=s5p54a * 22 if s5p54b==1
	replace ing_ctapropia_3=s5p54a * 4 if s5p54b==2
	replace ing_ctapropia_3=s5p54a * 2 if (s5p54b>=3 & s5p54b<=4)
	replace ing_ctapropia_3=s5p54a if s5p54b==5
	replace ing_ctapropia_3=s5p54a/3 if s5p54b==6
	replace ing_ctapropia_3=s5p54a/6 if s5p54b==7
	replace ing_ctapropia_3=s5p54a/12 if s5p54b==8
	egen ing_ctapropia= rowtotal(ing_ctapropia_1 ing_ctapropia_2 ing_ctapropia_3), m
	
	recode s5p57c 99 98=.
	gen otros_ing=.
	replace otros_ing=(s5p57b*s5p57c)/12 if s5p57c<98
	
	foreach x of varlist sal extras aguinaldo extras_especies ing_ctapropia otros_ing {
	bys i00: egen `x'_hogar=total(`x'), m
	}
	collapse (firstnm) sal_hogar extras_hogar aguinaldo_hogar extras_especies_hogar ing_ctapropia_hogar otros_ing_hogar miembros_hogar, by(i00)
	save "$RAW\ing_laborales.dta", replace
	
	use "$RAW\emnv14_15_parte_c3_de_la_seccion_7.dta", clear
	recode s7p35a 9999998 9999999=.
	recode s7p35b 9999998 9999999=.
	gen s7p35b_cor=s7p35b*26
	egen ing=rowtotal(s7p35a s7p35b_cor), m
	bys i00: egen renta=total(ing) if s7c3cod>=1 & s7c3cod<=2, m
	bys i00: egen beca=total(ing) if s7c3cod==3, m
	bys i00: egen pen_ali=total(ing) if s7c3cod==4, m
	bys i00: egen jub_pen=total(ing) if s7c3cod>=5 & s7c3cod<=6, m
	collapse (firstnm) renta beca pen_ali jub_pen, by(i00)
	save "$RAW\ing_rentasyjub.dta", replace
	
	use "$RAW\emnv14_16_parte_c4_de_la_seccion_7.dta", clear
	recode s7p37a 9999998 9999999=.
	recode s7p37b 9999998 9999999=.
	gen s7p37b_cor=s7p37b*26
	egen ing=rowtotal(s7p37a s7p37b_cor), m
	bys i00: egen int_ganados=total(ing) if s7c4cod>=1 & s7c4cod<=2, m
	bys i00: egen indemniz=total(ing) if (s7c4cod>=3 & s7c4cod<=4) | s7c4cod==7, m
	bys i00: egen dividendos=total(ing) if s7c4cod==5, m
	bys i00: egen otros_ing=total(ing) if (s7c4cod>=9 & s7c4cod<=10) | s7c4cod==6, m
	bys i00: egen donaciones=total(ing) if s7c4cod==8, m
	collapse (firstnm) int_ganados indemniz dividendos otros_ing donaciones, by(i00)
	save "$RAW\ing_otros.dta", replace
	
	use "$RAW\emnv14_17_parte_c5_de_la_seccion_7.dta", clear
	recode s7p40a 9999998 9999999=.
	recode s7p40b 9999998 9999999=.
	gen s7p40b_cor=s7p40b*26
	egen ing=rowtotal(s7p40a s7p40b_cor), m
	bys i00: egen transpriv_mon=total(ing) if s7c5cod==1, m
	bys i00: egen transpriv_nomon=total(ing) if s7c5cod==3, m
	bys i00: egen remesas_mon=total(ing) if s7c5cod==2, m
	bys i00: egen remesas_nomon=total(ing) if s7c5cod==4, m
	collapse (firstnm) transpriv_mon transpriv_nomon remesas_mon remesas_nomon, by(i00)
	save "$RAW\transfpriv.dta", replace
	
	foreach file in "$RAW\ing_otros.dta" "$RAW\ing_rentasyjub.dta" "$RAW\ing_laborales.dta" "$RAW\ing_negocios.dta" "$RAW\ing_ct.dta" "$RAW\alquileres.dta" {
                mmerge i00 using "`file'", t(1:1)
				drop _merge
        }
 
	save "$RAW\ingresos.dta", replace	
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

egen ing_lab_mon = rowtotal(sal_hogar extras_hogar aguinaldo_hogar ing_ctapropia_hogar otros_ing_hogar ing_lab_1), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

egen ing_ren_mon= rowtotal(renta int_ganados dividendos), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

egen ing_trspri_mon= rowtotal(transpriv_mon donaciones), m

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

egen ing_ct_mon= rowtotal(beca ing_ct_1), m

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*-----------------------------------------------*

egen ing_trsgob_mon= rowtotal(pen_ali jub_pen), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= remesas_mon

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

egen ing_otro_mon= rowtotal(indemniz otros_ing), m

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

gen ing_lab_nomon = extras_especies_hogar

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

gen ing_ren_nomon = ing_ren_nomon_1

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

gen ing_trspri_nomon = transpriv_nomon

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

gen ing_rem_nomon= remesas_nomon

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


collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss ing_ren_nomon_miss ing_trspri_nomon_miss ing_ct_nomon_miss ing_trsgob_nomon_miss ing_rem_nomon_miss ing_otro_nomon_miss ict_miss ing_lab_miss ing_ren_miss ing_trspri_miss ing_ct_miss ing_trsgob_miss ing_rem_miss ing_otro_miss ing_tpriv_miss ing_tpub_miss, by(i00) 

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
	
*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\emnv14_02_datos_de_la_vivienda_y_el_hogar.dta", clear	
	foreach x of varlist s1p12a s1p12b s1p20 s1p23 s1p24 s1p27 s1p30a s1p30b s1p30c s1p30d s1p30e s1p30f {
	recode `x'  99998 99999 9998 9999=.
	}
	gen s1p12b_cor=s1p12b * 26 //Cotizacion Cordoba nicaraguense por usd promedio 2014= 26 nio/usd
	egen gasto_vcon_1= rowtotal (s1p12a s1p12b_cor s1p20), m
	gen gasto_vele_1=s1p23
	gen gasto_vpgk_1=s1p24
	gen gasto_vleca_1=s1p27
	egen gasto_edre_1=rowtotal(s1p30a s1p30b s1p30c), m
	egen gasto_com_1=rowtotal(s1p30d s1p30e s1p30f), m		
	keep i00 dominio4 i06 gasto_vcon_1 gasto_vele_1 gasto_vpgk_1 gasto_vleca_1 gasto_edre_1 gasto_com_1 peso2 peso3	
	save "$RAW\gto_1.dta" , replace
	
	use "$RAW\emnv14_04_poblacion.dta", clear	
	foreach x of varlist s3p4b s3p5b s3p6b s3p7b s3p9b s3p10b s3p13b s3p15b s3p16b s4p6b s4p7b s4p8b ///
						 s4p8c s4p9b s4p9c s4p10b-s4p10e s4p22b s4p23b s4p24b s4p24c s4p25b s4p25c ///
						 s4p26b-s4p26d s4p28 s4p30 {
	recode `x'  99998 99999 9998 9999=.
	}
	gen aux=s3p13b/12
	egen gasto_sal_aux=rowtotal(s3p4b s3p5b s3p6b s3p7b s3p10b aux s3p15b s3p16b), m
	egen gasto_tserv_aux=rowtotal(s3p9b s4p7b s4p23b), m
	egen aux2=rowtotal(s4p9b s4p9c s4p10b s4p10c s4p10d s4p10e s4p25b s4p25c s4p26b s4p26c s4p26d s4p28 s4p30), m
	gen aux3=aux2/12
	egen gasto_edre_aux=rowtotal(s4p6b s4p8b s4p8c aux3 s4p22b s4p24b s4p24c), m
	
	bys i00: egen gasto_sal_1=total(gasto_sal_aux), m
	bys i00: egen gasto_tserv_1=total(gasto_tserv_aux), m
	bys i00: egen gasto_edre_2=total(gasto_edre_aux), m
	collapse (firstnm) gasto_sal_1 gasto_tserv_1 gasto_edre_2, by(i00)
	save "$RAW\gto_2.dta" , replace
	
	use "$RAW\emnv14_08_parte_a_de_la_seccion_7.dta", clear	
	recode s7p6 99998 99999=.
	recode s7p10 99998 99999=.
	
	gen s7p6_men=.
	replace s7p6_men=s7p6*30 if s7p4==1
	replace s7p6_men=s7p6*4 if s7p4==2
	replace s7p6_men=s7p6*2 if s7p4==3
	replace s7p6_men=s7p6*1 if s7p4==4
	replace s7p6_men=s7p6/3 if s7p4==5
	replace s7p6_men=s7p6/6 if s7p4==6
	replace s7p6_men=s7p6/12 if s7p4==7
	gen s7p10_men=.
	replace s7p10_men=s7p10*30 if s7p8==1
	replace s7p10_men=s7p10*4 if s7p8==2
	replace s7p10_men=s7p10*2 if s7p8==3
	replace s7p10_men=s7p10*1 if s7p8==4
	replace s7p10_men=s7p10/3 if s7p8==5
	replace s7p10_men=s7p10/6 if s7p8==6
	replace s7p10_men=s7p10/12 if s7p8==7
	egen gato_mens=rowtotal(s7p6_me s7p10_me), m
	
	bys i00: egen gasto_alihogar_1=total(gato_mens) if (s7prod>=1 & s7prod<=53) | s7prod==56 | (s7prod>=59 & s7prod<=62), m
	bys i00: egen gasto_alta_1=total(gato_mens) if (s7prod>=54 & s7prod<=55) | s7prod==57, m
	bys i00: egen gasto_alifuera_1=total(gato_mens) if (s7prod>=58 & s7prod<59), m
	collapse (firstnm) gasto_alihogar_1 gasto_alta_1 gasto_alifuera_1, by(i00)
	save "$RAW\gto_3.dta" , replace

	use "$RAW\emnv14_09_parte_b1_de_la_seccion_7.dta", clear	
	recode s7p18 999998 999999=.
	gen s7p18_men=s7p18*4
	bys i00: egen gasto_tserv_2=total(s7p18_men) if s7b1cod==1, m
	bys i00: egen gasto_edre_3=total(s7p18_men) if s7b1cod==2, m
	bys i00: egen gasto_com_1=total(s7p18_men) if s7b1cod==3, m
	bys i00: egen gasto_tcomb_1=total(s7p18_men) if s7b1cod==4, m
	collapse (firstnm) gasto_tserv_2 gasto_edre_3 gasto_com_1 gasto_tcomb_1, by(i00)
	save "$RAW\gto_4.dta" , replace
	
	use "$RAW\emnv14_10_parte_b2_de_la_seccion_7.dta", clear	
	recode s7p20 999998 999999=.
	bys i00: egen gasto_otros_1=total(s7p20) if (s7b2cod>=1 & s7b2cod<=17) | (s7b2cod>=20 & s7b2cod<=24), m
	bys i00: egen gasto_edre_4=total(s7p20) if (s7b2cod>=18 & s7b2cod<=19), m	
	collapse (firstnm) gasto_otros_1 gasto_edre_4, by(i00)
	save "$RAW\gto_5.dta" , replace
	
	use "$RAW\emnv14_11_parte_b3_de_la_seccion_7.dta", clear	
	recode s7p22 9999998 9999999=.
	gen s7p22_men=s7p22/6
	bys i00: egen gasto_veca_1=total(s7p22_men) if (s7b3cod>=1 & s7b3cod<=4), m
	bys i00: egen gasto_tman_1=total(s7p22_men) if s7b3cod==5, m
	bys i00: egen gasto_ens_1=total(s7p22_men) if (s7b3cod>=6 & s7b3cod<=11), m
	bys i00: egen gasto_otros_2=total(s7p22_men) if (s7b3cod>=12 & s7b3cod<=14), m
	collapse (firstnm) gasto_veca_1 gasto_tman_1 gasto_ens_1 gasto_otros_2, by(i00)
	save "$RAW\gto_6.dta" , replace
	
	use "$RAW\emnv14_12_parte_b4_de_la_seccion_7.dta", clear	
	recode s7p25 9999998 9999999=.
	gen s7p25_men=s7p25/12
	bys i00: egen gasto_vcon_2=total(s7p25_men) if s7b4cod==1, m
	bys i00: egen gasto_ens_2=total(s7p25_men) if (s7b4cod>=2 & s7b4cod<=4), m
	bys i00: egen gasto_tserv_3=total(s7p25_men) if s7b4cod==5, m
	bys i00: egen gasto_otros_3=total(s7p25_men) if (s7b4cod>=6 & s7b4cod<=7) | (s7b4cod>=11 & s7b4cod<=19), m
	bys i00: egen gasto_tadq_1=total(s7p25_men) if (s7b4cod>=8 & s7b4cod<=9), m
	bys i00: egen gasto_totros_1=total(s7p25_men) if s7b4cod==10, m
	collapse (firstnm) gasto_vcon_2 gasto_ens_2 gasto_tserv_3 gasto_otros_3 gasto_tadq_1 gasto_totros_1, by(i00)
	save "$RAW\gto_7.dta" , replace
	
	foreach file in "$RAW\gto_1.dta" "$RAW\gto_2.dta" "$RAW\gto_3.dta" "$RAW\gto_4.dta" "$RAW\gto_5.dta" "$RAW\gto_6.dta" {
                mmerge i00 using "`file'", t(1:1)
				drop _merge
        }
 
	save "$RAW\gastos.dta", replace
		
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
 

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*
gen gasto_alihogar= gasto_alihogar_1

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*
gen gasto_alifuera = gasto_alifuera_1

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*
gen gasto_alta= gasto_alta_1 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*
egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*
gen gasto_veca= gasto_veca_1 

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*
egen gasto_vcon= rowtotal(gasto_vcon_1 gasto_vcon_2), m

*---------------*
* GASTO EN AGUA *
*---------------*
gen gasto_vag=. 

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*
gen gasto_vele= gasto_vele_1 

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*
gen gasto_vgn=. 

*-----------------------------------------------------------*
* GASTO EN petroleo, gasolina y kerosene para uso doméstico *
*-----------------------------------------------------------*
gen gasto_vp=.
gen gasto_vgas=.
gen gasto_vk= .

gen gasto_vpgk=gasto_vpgk_1  

*-----------------------------------------------------*
* GASTO EN GAS LICUADO DE PETRÓLEO PARA USO DOMÉSTICO *
*-----------------------------------------------------*
gen gasto_vlp=. 

*------------------------*
* GASTO EN LENIA Y CARBON *
*------------------------*
gen gasto_vca= . 
gen gasto_vle= .

gen gasto_vleca= gasto_vleca_1

*------------------------------------*
* GASTO EN DIESEL PARA USO DOMÉSTICO *
*------------------------------------*
gen gasto_vdi=.

*---------------------------------------------*
*Otros gastos en combustibles de uso domestico*
*---------------------------------------------*
gen gasto_vot=. 

*----------------------------------------------------------------------------*
*Gasto en vivienda, servicios de conservación y combustibles de uso doméstico*
*----------------------------------------------------------------------------*
egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vgn gasto_vpgk gasto_vlp gasto_vleca gasto_vdi gasto_vot), missing

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*
egen gasto_ens= rowtotal(gasto_ens_1 gasto_ens_2) 

*----------------*
* GASTO EN SALUD *
*----------------*
gen gasto_sal= gasto_sal_1 

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*
gen gasto_com= gasto_com_1 

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*
egen gasto_tserv= rowtotal(gasto_tserv_1 gasto_tserv_2 gasto_tserv_3), m 
	
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

*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*
gen gasto_tcomb= gasto_tcomb_1

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*
gen gasto_tman= gasto_tman_1

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*
gen gasto_tadq= gasto_tadq_1 

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*
gen gasto_totros= gasto_totros_1 

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*
egen gasto_trans=rowtotal(gasto_tserv gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc gasto_totcomb gasto_tman gasto_tadq gasto_totros), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*
egen gasto_edre= rowtotal(gasto_edre_1 gasto_edre_2 gasto_edre_3 gasto_edre_4) 

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	
egen gasto_otros= rowtotal(gasto_otros_1 gasto_otros_2 gasto_otros_3) 

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
mmerge i00 using "$RAW\ingresos_hogar.dta", t(1:1)
drop _merge
merge 1:1 i00 using "$RAW\ing_laborales.dta", keepusing(miembros_hogar)
drop _merge
merge 1:1 i00 using "$RAW\alquileres.dta", keepusing(peso2 peso3)
 
keep i00 peso2 peso3 miembros_hogar gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vk gasto_vleca ///
	gasto_vle gasto_vca gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vot gasto_vele gasto_vgn gasto_vag gasto_ens ///
	gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tgnc gasto_tcomb gasto_tlp gasto_talc gasto_totcomb gasto_tman gasto_tadq ///
	gasto_totros gasto_com gasto_edre gasto_otros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub


*-----------------------------------------------------------*
	* Variables identificatorias de la Encuesta      * 
*-----------------------------------------------------------*

gen pais = "NIC"	
label var pais "Pais de la Encuesta"

gen anio=2014
label var anio "Anio de la Encuesta"

gen encuesta = "EMNV"
label var encuesta "Encuesta"

gen cod_hogar=i00
label var cod_hogar "Codigo de hogar"

gen factor_expansion_1=peso2
label var factor_expansion_1 "Factor de expansion con consumo" 
gen factor_expansion_2=peso3
label var factor_expansion_2 "Factor de expansion con consumo* miembros/hogar(totmiembc)" 

order  pais anio encuesta cod_hogar factor_expansion_1 factor_expansion_2 miembros_hogar ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ///
	   ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ///
	   ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
	   gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca ///
	   gasto_vlp gasto_vdi gasto_vp gasto_vgas gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv ///
	   gasto_tcomb gasto_tga gasto_tlp gasto_tdie gasto_tgnc gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros
		  
saveold "$data_arm\NIC_EMNV_2014.dta" , replace


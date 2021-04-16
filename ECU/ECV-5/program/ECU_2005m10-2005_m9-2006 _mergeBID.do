***********************
** ECUADOR ECV 05/06 **
***********************

* 13,881 Households
* 55,666 Individuals

clear 
set mem 300m
set more off

 ** MERGE

 * Individuals

 * E5R_PER - DATOS DEL MIEMBRO DE HOGAR

 use e5r_per.dta, clear 
 sort ciudad zona sector vivienda hogar persona 

 ** Households

 * E5R_UBI - IDENTIFICACIÓN Y UBICACIÓN DE LA VIVIENDA

 merge ciudad zona sector vivienda hogar using "e5r_ubi.dta"
 	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar
	
 * E5R_VIV - DATOS DE LA VIVIENDA

 merge ciudad zona sector vivienda hogar using "e5r_viv.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_GAL1 - SECC.8.II. GASTOS EN ALIMENTOS - PREGUNTAS FILTRO

 merge ciudad zona sector vivienda hogar using "e5r_gal1.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_GOTR - SECC.8.III. GASTOS NO ALIMENTICIOS

 merge ciudad zona sector vivienda hogar using "e5r_gotr.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_IOTR - SECC.8 OTROS INGRESOS DEL HOGAR

 merge ciudad zona sector vivienda hogar using "e5r_iotr.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_PRE1 - SECC.8 PRESTAMOS REALIZADOS POR EL HOGAR - PREGUNTAS FILTRO

 merge ciudad zona sector vivienda hogar using "e5r_pre1.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_CRE1 - SECC.8.COMPRAS A CREDITO DEL HOGAR -  PREGUNTAS FILTRO

 merge ciudad zona sector vivienda hogar using "e5r_cre1.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_EMIG - SECC9. CAPITAL SOCIAL - EMIGRACIÓN LABORAL

 merge ciudad zona sector vivienda hogar using "e5r_emig.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_EML1 - SECC9. EX MIEMBROS DEL HOGAR QUE MIGRARON - PREG. FILTRO

 merge ciudad zona sector vivienda hogar using "e5r_eml1.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_NEG1 - SECC10. NEGOCIOS DEL HOGAR - PREG. FILTRO
 
 merge ciudad zona sector vivienda hogar using "e5r_neg1.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 * E5R_AGR1 - SECC11. ACTIV. AGROPECUARIAS -PREG FILTRO

 merge ciudad zona sector vivienda hogar using "e5r_agr1.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 destring ciudad zona sector vivienda hogar, replace 

 sort ciudad zona sector vivienda hogar persona
 
 save ecv0506_prel.dta, replace

 * Agregado de consumo /* Final - Abril 2007 */

 use ecv0506_prel.dta, clear

 merge ciudad zona sector vivienda hogar using "agregado_consumo.dta"
	
	tab _merge
	drop _merge
	sort ciudad zona sector vivienda hogar

 save ecv0506.dta, replace


********************************************************************************

/* NOT INCLUDED 
  E5R_GALI	SECC.8.II. GASTO EN ALIMENTOS
  E5R_PRES	SECC.8.PRESTAMOS REALIZADOS POR EL HOGAR
  E5R_CRED 	SECC.8.COMPRAS A CREDITO DEL HOGAR 
  E5R_EQUI	SECC.8.EQUIPAMIENTO Y PROPIEDADES DEL HOGAR
  E5R_EMLA	SECC9. EX MIEMBROS DEL HOGAR QUE MIGRARON 
  E5R_NEGO	SECC10. NEGOCIOS DEL HOGAR 
  E5R_FA_I	SECC11. ACTIV. AGROPECUARIAS (PARTE A - I)
  E5R_FA_II	SECC11. ACTIV. AGROPECUARIAS (PARTE A - II)
  E5R_FB	SECC11. ACTIV. AGROPECUARIAS (PARTE B)
  E5R_FC	SECC11. ACTIV. AGROPECUARIAS (PARTE C)
  E5R_FDE	SECC11. ACTIV. AGROPECUARIAS (PARTE D Y E)
  E5R_FF	SECC11. ACTIV. AGROPECUARIAS (PARTE F- I,II,III)
  E5R_FFIV	SECC11. ACTIV. AGROPECUARIAS (PARTE F - VI)
  E5R_FGHI	SECC11. ACTIV. AGROPECUARIAS (PARTE F - V, PARTE G, H, I)
  E5R_FJ	SECC11. ACTIV. AGROPECUARIAS (PARTE J)
*/

********************************************************************************







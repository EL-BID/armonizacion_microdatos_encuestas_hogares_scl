
*Mayra Sáenz-Enero 2014: Este do file no corre. Falta armonizar la base de datos.
/*

* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


global ruta = "\\Sdssrv03\surveys"

local PAIS CHL
local ENCUESTA CASEN
local ANO "1987"
local ronda m11_m12 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Chile
Encuesta: CASEN
Round: Noviembre- Diciembre
Autores: 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/



use `base_in', clear

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=4

*MLO:falta armonizar base (se recomienda bajar nuevamente base original)
	
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*******************
****** tc_c *******
*******************

gen tc_c = 219.4067

label var tc_c "Tasa de cambio LCU/USD"

*******************
****** ipc_c ******
*******************

gen ipc_c = .


label var ipc_c "Índice de precios al consumidor"

*******************
****** ppp_c ******
*******************

gen ppp_c = .

label var ppp_c "Factor de conversión PPP LCU/USD"

****************
*lp25_2005_ci***
****************

gen lp25_2005_ci = .

label var lp25_2005_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2005"

***************
*lp4_2005_ci***
***************

gen lp4_2005_ci = .
  
label var lp4_2005_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2005"

********* 
*lpl25_ci
*********

gen lp25_ci = .

 
label var lp25_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2011"

*********
*lpl4_ci*
*********

gen lp4_ci = .

label var lp4_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2011"

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 10158        if zona_c==1  /*urbana*/
replace lp_ci= 6850        if zona_c==0	/*rural*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 5079       if zona_c==1  /*urbana*/
replace lpe_ci= 3914       if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (o15 >= 1 & o15 <= 6)
recode cotizando_ci .=0 if (pea1_ci==1 & desemp1_ci==0)
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if (o27 >= 1 & o27 <= 6)
recode afiliado_ci .=0 if o27 == 7 | (pea1_ci==1 & desemp1_ci==0)
label var afiliado_ci "Afiliado a la Seguridad Social"


****************
*tipopen_ci*****
****************

gen tipopen_ci=.
* no esta la variable
label define  t 1 "Jubilacion" 2 "Pension invalidez" 3 "Pension viudez" 12 " Jub y inv" 13 "Jub y viud" 23 "Viud e inv"  123 "Todas"
label value tipopen_ci t

label var tipopen_ci "Tipo de pension - variable original de cada pais" 


****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=o15 if o15<=6
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o9==1 & categopri_ci==3
replace tipocontrato_ci=2 if o9==2 & categopri_ci==3
replace tipocontrato_ci=3 if o9==4 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
	


****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if o1==1 | o1==2 & o2==1
replace condocup_ci=2 if o1==2 & o2==2 & o3==1
replace condocup_ci=3 if o1==2 & o2==2 & o3==2 
replace condocup_ci=4 if edad<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
	
************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)

*************
*cesante_ci* 
*************
gen cesante_ci=1 if o4==1
replace cesante_ci=0 if o4==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

/*o14: Cuántas personas trabajan en total en esa empresa (en Chile)?
	A. 1 persona
	B. 2 a 5 pers
	C. 6 a 9 pers
	D. 10 a 49 pers
	E. 50 a 199 pers
	F. 200 o más pers
	X. No sabe*/
gen tamemp_ci= o14

label var tamemp_ci "# empleados en la empresa"



*************
**pension_ci*
*************
egen auxpen=rsum(yjubaj)
*no hay otra variable, supongo que estan todos los tipos de pension incluidos en esta
gen pension_ci=1 if auxpen>0
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=auxpen
replace ypen_ci=. if auxpen<0
drop auxpen
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=1 if ypasaj>0
recode pensionsub_ci .=0 
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring auxpens, replace
gen ypensub_ci=auxpens
replace ypensub_ci=. if auxpens<0
drop auxpens
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=.

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* CHL 1987
gen salmm_ci= 	.

label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************

gen tecnica_ci=.

label var tecnica_ci "1=formacion terciaria tecnica"

**************
***tamemp_ci**
**************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

****************
**categoinac_ci*
****************

gen categoinac_ci=.
label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
	
***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"



/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* También se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch	notropari_ch notronopari_ch	nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci lp25_ci lp4_ci	lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch tc_c ipc_c ppp_c lp25_2005_ci lp4_2005_ci, first

*firmapeq_ci


	
qui destring $var, replace


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
set more off
compress


do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"


saveold "`base_out'", replace


log close

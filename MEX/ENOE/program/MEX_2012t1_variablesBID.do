* (versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOvI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS MEX
local ENCUESTA ENOE
local ANO "2012"
local ronda t1 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: México
Encuesta: ENOE
Round: t1
Autores: Melany Gualavisi (melanyg@iadb.org)
Fecha última modificación: Diciembre, 2015

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

/* solo se generaron las siguientes variables - 
region_BID_c
idh_ch
idp_ci
edad_ci
sexo_ci
aedu_ci
condocup_ci
formal_ci
categopri_ci
spublico_ci 
horaspri_ci
tamemp_ci
ylmpri_ci

el resto está pendiente de armonizar*/

************************
*** region según BID ***
************************
gen region_BID_c=1 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
****idh_ch*****
***************
egen idh_ch=group(cd_a  ent  con  v_sel  n_hog  h_mud) 
label var idh_ch "ID del hogar"


***************
****idp_ci*****
***************
egen idp_ci=group(idh_ch n_ren)
label var idp_ci "ID de la persona en el hogar"

******************************
*	pais_c
******************************
gen str3 pais_c="MEX"

******************************
*	anio_c
******************************
gen anio_c=2012
label var anio_c "Año de la encuesta"

******************************
*	trimestre_c
******************************
gen trimestre_c=1
label var trimestre_c "Trimestre de la encuesta" 
label define trimestre_c 1 "Ene-Feb-Mar" 2 "Abr-May-Jun" 3 "Jul-Ago-Sep" 4 "Oct-Nov-Dic", add modify 
label value trimestre_c trimestre_c

******************************
*	factor_ci
******************************
gen factor_ci=fac
label var factor_ci "Factor de expansión"

******************************
*	factor_ci
******************************
destring cd_a, replace
gen zona_c=1 if cd_a>=1 & cd_a<=43
replace zona_c=2 if cd_a>=44 & cd_a<=86
label define zona_c 1 "Urbano" 2 "Rural"
label value zona_c zona_c
label var zona_c "Ámbito Geográfico"

***************
****edad_ci****
***************
gen edad_ci=eda
label var edad_ci "Edad del individuo en años"

***************
****sexo_ci****
***************
destring sex, replace
gen sexo_ci=sex
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer", add modify
label value sexo_ci sexo_ci

***************
****aedu_ci****
***************
/*

00 Ninguno
01 Preescolar
02 Primaria
03 Secundaria
04 Preparatoria o bachillerato
05 Normal
06 Carrera técnica
07 Profesional
08 Maestría
09 Doctorado
99 No sabe
*/

destring cs_p13_1 cs_p13_2, replace

gen nivel_ed=cs_p13_1
gen grado_ed=cs_p13_2

gen aedu_ci=.
replace aedu_ci=0 if nivel_ed==0 |nivel_ed==1 
replace aedu_ci=grado_ed if nivel_ed==2
replace aedu_ci= grado_ed+6 if nivel_ed==3
replace aedu_ci= grado_ed+12 if nivel_ed==4 | nivel_ed==5 |nivel_ed==6
replace aedu_ci= grado_ed+12 if nivel_ed==7
replace aedu_ci= grado_ed+17 if nivel_ed==8 | nivel_ed==9

label var aedu_ci "Anios de educacion aprobados"


****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if clase1==1 & clase2==1
replace condocup_ci=2 if clase1==1 & clase2==2
replace condocup_ci=3 if clase1==2
* replace condocup_ci=4 if edad<12 * no hay en la encuesta menores de 12 años

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "No_responde_por_menor_edad", add modify
label value condocup_ci condocup_ci

	
***************
****formal_ci**
***************
gen formal_ci=.
replace formal_ci=1 if condocup_ci==1 & o_seg_soc==1
replace formal_ci=0 if condocup_ci==1 & (o_seg_soc==2 | o_seg_soc==3)
label var formal_ci "Formalidad Laboral"

*****************
****categopri_ci*
*****************
g categopri_ci=.
replace categopri_ci=1 if o_pos_ocu==2 & condocup_ci==1
replace categopri_ci=2 if o_pos_ocu==3 & condocup_ci==1
replace categopri_ci=3 if o_pos_ocu==1 & condocup_ci==1
replace categopri_ci=4 if o_pos_ocu==4 & condocup_ci==1

label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4" No_remunerado" 0 "Otro" , add modify
label value categopri_ci categopri_ci
label var categopri_ci "Categoria ocupacional en la actividad principal"

****************
****spublico_ci*
****************
gen spublico_ci=.
replace spublico_ci=1 if condocup_ci==1 & tue2==4
replace spublico_ci=0 if condocup_ci==1 & tue2!=4 
label var spublico_ci "=1: Personas que trabajan en el sector público"


****************
****horaspri_ci*
****************
gen horaspri_ci=hrsocup
label var horaspri_ci "Horas trabajadas en la actividad principal"

****************
****tamemp_ci*
****************
destring p3q, replace
gen tamemp_ci=.
replace tamemp_ci=1 if (p3q==1 | p3q==2) & condocup_ci==1
replace tamemp_ci=2 if (p3q>=3 & p3q<=7) & condocup_ci==1
replace tamemp_ci=3 if (p3q>=8 & p3q<=11) & condocup_ci==1


label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande", add modify
label value tamemp_ci tamemp_ci
	
****************
****ylmpri_ci*
****************
destring p6b1 p6b2, replace
recode p6b2 (999999=.) 

gen ylmpri_ci=ingocup if condocup_ci==1
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 




/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* También se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/
/*
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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

*/


	
*qui destring $var, replace


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
set more off
compress


*do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"


saveold "`base_out'", replace


log close




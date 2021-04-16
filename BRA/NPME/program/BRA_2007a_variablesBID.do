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

local PAIS BRA
local ENCUESTA NPME
local ANO "2007"
local ronda a 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Brasil
Encuesta: NPME
Round: a
Autores: María Laura Oliveri (mloliveri@iadb.org / lauraoliveri@iadb.org)
Fecha última modificación: Diciembre, 2015

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

/* solo se generaron las siguientes variables - 
region_BID_c
factor_ci
anio_c
mes_c
trimestre_c
zona_c
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

***************
*entrevista_ci*
***************
gen entrevista_ci=v072

***************
*****region_c*
***************
gen region_c = 1 if v035 == 26
	replace region_c = 2 if v035 == 29
	replace region_c = 3 if v035 == 31
	replace region_c = 4 if v035 == 33
	replace region_c = 5 if v035 == 35
	replace region_c = 6 if v035 == 43
	replace region_c = 7 if v035 == 41
label define region_c 1 "REC" 2 "SAL" 3 "BHO" 4 "RJA" 5 "SPA" 6 "POA" 7 "CUR"
	label values region_c region_c
	label var region_c "Region Metropolitana"
	
***************
****anio_c*****
***************
*gen anio_c=. /*se generó en do file merge*/
label var anio_c "Anio de la encuesta" 
***************
****pais_c*****
***************
gen pais_c="BRA"
label var pais_c "Nombre del País"

***************
****factor_ci**
***************
gen factor_ci=v215
label var factor_ci "Factor de expansion del individuo"

******************
****trimestre_c***
******************
*se creo en el merge de la base
*gen trimestre_c=.
label var trimestre_c "Trimestre de la encuesta" 
	label define trimestre_c 1 "Ene-Feb-Mar" 2 "Abr-May-Jun" 3 "Jul-Ago-Sep" 4 "Oct-Nov-Dic", add modify 
	label value trimestre_c trimestre_c

***************
****mes_c******
***************
*se creo en el merge de la base
*gen mes_c=.
label var mes_c "Mes de la encuesta" 
	label define mes_c 1 "Ene" 2 "Feb" 3 "Mar" 4 "Abr" 5 "May" 6 "Jun" 7 "Jul" 8 "Ago" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dic", add modify 
	label value mes_c mes_c

***************
****zona_c*****
***************
gen zona_c=1 /* Only the 6 main metropolitan regions*/
label var zona_c "Zona del pais"
	label define zona_c 1 "urbana" 0 "rural", add modify
	label value zona_c zona_c

************************
*** region según BID ***
************************
gen region_BID_c=4 
label var region_BID_c "Regiones BID"
	label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
	label value region_BID_c region_BID_c

***************
****relacion_ci
***************
gen relacion_ci = 1 if v206==1
	replace relacion_ci = 2 if v206==2
	replace relacion_ci = 3 if v206==3
	replace relacion_ci = 4 if v206==4
	replace relacion_ci = 5 if v206==5 | v206==6 | v206==8
	replace relacion_ci = 6 if v206==7
label var relacion_ci "Relacion o parentesco con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Conyuge/esposo/compañero" 3 "Hijo/a" 4 "Otros_parientes" 5 "Otros_no_Parientes" 6 "Empleado/a_domestico/a", add modify 
	label values relacion_ci relacion_ci

***************
****idh_ch*****
***************
egen idh_ch=group(v040 v050 v060 v063 v207)
label var idh_ch "ID del hogar"


***************
****idp_ci*****
***************
egen idp_ci=group(v040 v050 v060 v063 v201 v203) 
label var idp_ci "ID de la persona en el hogar"

***************
****edad_ci****
***************
gen edad_ci=v234
label var edad_ci "Edad del individuo en años"

***************
****sexo_ci****
***************
gen sexo_ci=v203
label var sexo_ci "Sexo del individuo" 
	label define sexo_ci 1 "Hombre" 2 "Mujer", add modify
	label value sexo_ci sexo_ci

***************
****aedu_ci****
***************
gen aedu_ci =  0 if (v302==2 & v306==2) | v303==6 | v303==7 | v307==7 | v307==8 |((v303==1 | v303==3) & ((v304==2 & v301==2) | v305==1)) | ((v307==1 | v307==4) & v309==2) | (v307==4 & v308==2 & v311==2)
	replace aedu_ci =  1 if ((v303==1 | v303==3) & ((v304==2 & v301==1) | v305==2)) | ((v307==1 | v307==4) & v310==1)
	replace aedu_ci =  2 if ((v303==1 | v303==3) & v305==3) | ((v307==1 | v307==4) & v310==2)
	replace aedu_ci =  3 if ((v303==1 | v303==3) & v305==4) | ((v307==1 | v307==4) & v310==3)
	replace aedu_ci =  4 if ((v303==1 | v303==3) & v305==5) | ((v307==1 | v307==4) & v310==4) | (v307==2 & v308==2 & v311==2) | (v307==2 & v309==2)
	replace aedu_ci =  5 if ((v303==1 | v303==3) & v305==6) | ((v307==1 | v307==4) & v310==5) | (v307==2 & v310==1)
	replace aedu_ci =  6 if ((v303==1 | v303==3) & v305==7) | ((v307==1 | v307==4) & v310==6) | (v307==2 & v310==2)
	replace aedu_ci =  7 if ((v303==1 | v303==3) & v305==8) | (v307==4 & v310==7) | (v307==2 & v310==3)
	replace aedu_ci =  8 if (v303==2 & v305==1) | v303==4 | (v307==2 & ((v308==2 & v311==1) | v310==4)) | (v307==4 & v311==1) | ((v307==3 | v307==5) & (v309==2 | (v308==2 & v311==2)))
	replace aedu_ci =  9 if (v303==2 & v305==2) | (v307==2 & v310==5) | ((v307==3 | v307==5) & v310==1)
	replace aedu_ci = 10 if (v303==2 & v305==3) | ((v307==3 | v307==5) & v310==2)
	replace aedu_ci = 11 if (v303==2 & v305==4) | v303==8 | (v303==5 & v305==1) | ((v307==3 | v307==5) & ((v308==2 & v311==1) | v310==3 | v310==4)) | (v307==6 & v309==2)
	replace aedu_ci = 12 if (v303==5 & v305==2) | (v307==6 & v310==1)
	replace aedu_ci = 13 if (v303==5 & v305==3) | (v307==6 & v310==2 & v311==2)
	replace aedu_ci = 14 if (v303==5 & v305>=4 & v305<=6) | (v307==6 & v310>=3 & v310<=6 & v311==2)
	replace aedu_ci = 15 if  v303==9 | (v307==6 & v311==1) | v307==9

recode aedu_ci (0/8=1 "[0,8]") (9/13=2 "[9,13]") (14/max=3 "14+ años"), gen(edu_group)
label var edu_group "Años de educación"

label var aedu_ci "Anios de educacion aprobados"

****************
****condocup_ci*
****************
gen condocup_ci = 1 if vd1 == 1
	replace condocup_ci = 2 if vd1 == 2 
	replace condocup_ci = 3 if vd1 == 3
	replace condocup_ci = 4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
	label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "No_responde_por_menor_edad", add modify
	label value condocup_ci condocup_ci
	
***************
****formal_ci**
***************
gen formal_ci= 1 if v414==1  /* empleados militares o servidores publicos*/
	replace formal_ci= 1 if  v415==1 | v416==1  | v425==1  /* cartera de trabajo asignada o contribuye al instituto de previdencia - actividad principal */
	replace formal_ci=1 if v432==1  /* si cotiza en la actividad secundaria */
	recode formal_ci .=0 
label var formal_ci "Formalidad Laboral"

*****************
****categopri_ci*
*****************
gen categopri_ci =1 if  v409==4
	replace categopri_ci = 2 if v409==3
	replace categopri_ci = 3 if v409==2 | v409==1
	replace categopri_ci = 4 if v409==5 | v409==6
label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4" No_remunerado" 0 "Otro" , add modify
	label value categopri_ci categopri_ci
	label var categopri_ci "Categoria ocupacional en la actividad principal"

****************
****spublico_ci*
****************
gen spublico_ci=v411==2 
label var spublico_ci "=1: Personas que trabajan en el sector público"

****************
****horaspri_ci*
****************
gen horaspri_ci=v429
label var horaspri_ci "Horas trabajadas en la actividad principal"

****************
****tamemp_ci*
****************
gen tamemp_ci=. /*PME only gives categorical variables for firm size*/
	replace tamemp_ci=1 if v412==1   /*Asalariados: de 2 a 5 empleados */
	replace tamemp_ci=2 if v412==2   /*Asalariados: de 6 a 10 empleados */
	replace tamemp_ci=3 if v412==3	 /*Asalariados: 11+ empleados */
	replace tamemp_ci=1 if (v409==3 | v409==4) & v421==2     /* independientes */
	replace tamemp_ci=v422 if (v409==3 | v409==4) & v421==1  /* independientes con más de un empleado */
replace tamemp_ci=v426 if (v409==3 | v409==4) & v421==2  /* independientes con más de un empleado */
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande", add modify
	label value tamemp_ci tamemp_ci
	
****************
****ylmpri_ci*
****************
gen ylmpri_ci=vd24 /*VD24=V4191 ou V4241*/
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

******************
****antiguedad_ci*
******************
gen antiguedad_ci = v4271/30         if v427==1 & vd1==1 
    replace antiguedad_ci = v4272            if v427==2 & vd1==1 
    replace antiguedad_ci = 12 + v4275       if v427==3 & vd1==1 
    replace antiguedad_ci = v4274*12         if v427==4 & vd1==1 
label var antiguedad_ci "Antiguedad en la actividad actual"

******************
***durades_ci*
******************
gen durades_ci= .
    replace durades_ci = 0.5              if v453==1 & (vd1==2 | vd1==3)
    replace durades_ci = v4541            if v454==1 & (vd1==2 | vd1==3)
    replace durades_ci = 12 + v4544       if v454==2 & (vd1==2 | vd1==3)
    replace durades_ci = v4543*12         if v454==3 & (vd1==2 | vd1==3)
label var durades_ci "Duracion del desempleo en meses"

*****************
**asiste_ci******
*****************
gen asiste_ci = 1 if v302==1
replace asiste_ci = 0 if v302==2
label var asiste_ci "=1 si asiste actualmente a la escuela"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* También se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/
/*
order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
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

* por ahora dejo solo las variables armonizadas para reducir el tamaño de la base
keep entrevista_ci region_c anio_c pais_c factor_ci trimestre_c mes_c zona_c region_BID_c relacion_ci ///
idh_ch idp_ci edad_ci sexo_ci aedu_ci edu_group condocup_ci formal_ci categopri_ci spublico_ci ///
horaspri_ci tamemp_ci ylmpri_ci durades_ci asiste_ci
	
qui destring $var, replace


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
set more off
compress


*do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"


saveold "`base_out'", replace

forvalues i=1/4 {
preserve
keep if trimestre_c==`i'
saveold "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO't`i'_BID.dta", replace
restore
}


log close


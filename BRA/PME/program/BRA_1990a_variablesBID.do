* (versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOvI.
 *________________________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS BRA
local ENCUESTA PME
local ANO "1990"
local ronda a 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
  
 
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Brasil
Encuesta: PME
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
gen entrevista_ci=.

***************
*****region_c*
***************
destring unidade, replace
gen region_c = 1 if unidade == 26
	replace region_c = 2 if unidade == 29
	replace region_c = 3 if unidade == 31
	replace region_c = 4 if unidade == 33
	replace region_c = 5 if unidade == 35
	replace region_c = 6 if unidade == 43
	replace region_c = 7 if unidade == 41
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
*gen factor_ci=peso
			sort anio_c mes_c region_c
			merge anio_c mes_c region_c using "Y:\survey\BRA\PME\1982\a\data_orig\factor.dta"
			ta _merge
			drop if _merge==2
			drop _merge
			
/********** en 1982 es necesario incorporar pesos a la base -  se utilizan los de 1983 por area metropolitana****/
label var factor_ci "Factor de expansion del individuo"

***************
****factor_ch**
***************
gen factor_ch=.
label var factor_ch "Factor de expansion del individuo"

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
gen relacion_ci = .
label var relacion_ci "Relacion o parentesco con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Conyuge/esposo/compañero" 3 "Hijo/a" 4 "Otros_parientes" 5 "Otros_no_Parientes" 6 "Empleado/a_domestico/a", add modify 
	label values relacion_ci relacion_ci

***************
****idh_ch*****
***************
order v001 serie unidade ordemd ordem condom nfam
egen idh_ch=group(v001 serie unidade ordemd ordem condom nfam)
label var idh_ch "ID del hogar"

***************
****idp_ci*****
***************
order v001 serie unidade ordemd ordem condom sexo 
egen idp_ci=group(v001 serie unidade ordemd ordem condom sexo) 
label var idp_ci "ID de la persona en el hogar"

***************
****edad_ci****
***************
gen edad_ci=idade
/*
** gen new year of born  + 1000 .
gen yob = nasano+1000

** gen continuos variable for the ANO and MES of the survey. 
g calendar_s = ym(anio_c,mes_c)

** gen continuos variable for Year and Month of born
g calendar_b = ym(yob,nasmes)

g age_a = calendar_s - calendar_b
drop  calendar_s calendar_b 
g age_f = age_a/12

g edad_ci = int(age_f)

drop age_a age_f */
label var edad_ci "Edad del individuo en años"

***************
****sexo_ci****
***************
gen sexo_ci=sexo==1
replace sexo_ci=2 if sexo==3
label var sexo_ci "Sexo del individuo" 
	label define sexo_ci 1 "Hombre" 2 "Mujer", add modify
	label value sexo_ci sexo_ci

***************
****aedu_ci****
***************
ren grau    grau_ed
ren 	serie 	serie_ed
	label var serie_ed	 "what was the last grade ... concluded?"
	label var grau_ed		"highest educational level achieved"
	
gen aedu_ci=.	
** generates education variable
			
			gen yeduc = .
			replace yeduc = 1 if grau_ed == 0
			replace yeduc = 2 if ((grau_ed == 1 | grau_ed == 3) & ( serie_ed >=1 & serie_ed <=3))
			replace yeduc = 3 if ((grau_ed == 1 | grau_ed == 3) & ( serie_ed >=4 & serie_ed <=6)) | (grau_ed == 2 & (serie_ed>= 1 & serie_ed <= 3)) | (grau_ed == 3 & serie_ed == 7)
			replace yeduc = 4 if (grau_ed == 2 & (serie_ed >=4 & serie_ed <= 5)) 
			replace yeduc = 4 if (grau_ed == 3 & serie_ed == 8)
			replace yeduc = 4 if (grau_ed == 4 & (serie_ed>= 1 & serie_ed<=2))
			replace yeduc = 4 if (grau_ed == 5 & (serie_ed>= 1 & serie_ed<=2))
			replace yeduc = 5 if ((grau_ed == 4 | grau_ed ==5) & ( serie_ed>=3 & serie_ed<=4)) | grau_ed == 6 | grau_ed == 7
			label define yeduc_l 1"< 1" 2"1 to 3" 3"4 to 7" 4"8 to 10" 5"11 or more" 
			label values yeduc yeduc_l
			label var yeduc "categories of years of education, adapted from IGBE"					
			
recode yeduc (0/3=1 "[0,7]") (4=2 "[8,10]") (5=3 "11+ años"), gen(edu_group)
label var edu_group "Años de educación"

label var aedu_ci "Anios de educacion aprobados"

****************
****condocup_ci*
****************
/* según do file de Goñi:
label define work_statuslf 1"worker" 2"worker but no work" 3"job search" 4"retired" 5"student" 6"domestic" 7"other" 
			label values cativ work_statuslf*/

gen condocup_ci = 1 if cativ == 1 | cativ==2 | cativ==6
	replace condocup_ci = 2 if cativ == 3
	replace condocup_ci = 3 if cativ == 4  | cativ == 5 | cativ == 7
	replace condocup_ci = 4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
	label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "No_responde_por_menor_edad", add modify
	label value condocup_ci condocup_ci
	
***************
****formal_ci**
***************
gen formal_ci= 1 if  cart==2 /* cartera de trabajo asignada  -solo asalariados- es lo unico que se pregunta*/
		recode formal_ci .=0 
label var formal_ci "Formalidad Laboral"

*****************
****categopri_ci*
*****************
*label define posocu 2"salaried worker" 4"self employed" 6"employer" 8"worker w.o wage"
		
gen categopri_ci =1 if  posocu==6
	replace categopri_ci = 2 if posocu==4
	replace categopri_ci = 3 if posocu==2
	replace categopri_ci = 4 if posocu==8
label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4" No_remunerado" 0 "Otro" , add modify
	label value categopri_ci categopri_ci
	label var categopri_ci "Categoria ocupacional en la actividad principal"

****************
****spublico_ci*
****************
gen spublico_ci=.
label var spublico_ci "=1: Personas que trabajan en el sector público"

****************
****horaspri_ci*
****************
gen horaspri_ci=horas
label var horaspri_ci "Horas trabajadas en la actividad principal"

****************
****tamemp_ci*
****************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande", add modify
	label value tamemp_ci tamemp_ci
	
****************
****ylmpri_ci*
****************

gen ylmpri_ci=rend if rend!=9999999  & condocup_ci==1
replace ylmpri_ci=. if ylmpri_ci==1.000e+09 | ylmpri_ci==9999998
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*****************
**asiste_ci******
*****************
gen asiste_ci = 1 if escola==1
replace asiste_ci = 0 if escola==3
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
horaspri_ci tamemp_ci ylmpri_ci asiste_ci
	
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


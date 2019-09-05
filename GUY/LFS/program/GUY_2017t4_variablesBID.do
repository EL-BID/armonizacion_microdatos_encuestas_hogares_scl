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

local PAIS GUY
local ENCUESTA LFS
local ANO "2017"
local ronda t4

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
      
capture log close


log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guyana
Encuesta: GLFS
Round: Octubre a Diciembre
Autores: Stephanie González - Email: stephanigo@iadb.org

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/

use `base_in', clear

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=.
replace region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroam곩ca_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

	***************
	***region_c ***
	***************
	destring region, replace
	gen region_c=region
	label define region_c 				///
	1"Barima Waini"                     ///
	2"Pomeroon-Supenaam"				///
	3"Essequibo Islands-West Demerara"  ///
	4"Demerara- Mahaica"                ///
	5"Mahaica-Berbice"                  ///
	6"East Berbice - Corentyne"  		///
	7"Cuyuni-Mazaruni"                  ///
	8"Potaro-Siparuni"                  ///
	9"Upper Takutu- Upper Essequibo"    ///
	10"Upper Demerara-Upper Berbice"   
   label value region_c region_c
   label var region_c "division politico-administrativa, region"
   
   
*************
* factor_ch *
*************

gen factor_ch=weight
label var factor_ch "Factor de expansión el hogar"

*************
* idh_ch    *
*************
sort hhid
egen idh_ch=group(hhid) 
label var idh_ch "ID del hogar"

*************
* idp_ci    *
*************
gen idp_ci=member
label variable idp_ci "ID de la persona en el hogar"

*************
* zona_c    *
*************
gen zona_c=zone
label variable zona_c "Zona urbana vs. rural"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

*************
* pais_c    *
*************
gen pais_c="GUY"
label variable pais_c "Pais"


*************
* anio_c    *
*************
gen anio_c=2017
label variable anio_c "Anio de la encuesta"

*************
* mes_c    *
*************
g mes_c=11

***************
* relacion_ci *
***************
gen relacion_ci=1     if q1_02==1
replace relacion_ci=2 if q1_02==2 
replace relacion_ci=3 if q1_02==3 | q1_02==4 | q1_02==6
replace relacion_ci=4 if (q1_02>=7 & q1_02<=9) | q1_02==5
replace relacion_ci=5 if q1_02==10
label var relacion_ci "Relación de parentesco con el jefe"
label def relacion_ci 1"Jefe" 2"Conyuge" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico"
label val relacion_ci relacion_ci	

		**************************
		* VARIABLES DEMOGRAFICAS *
		**************************

***************
* factor_ci   * 
***************
gen factor_ci=weight
label variable factor_ci "Factor de expansion del individuo"

***************
* sexo_ci     * 
***************
gen sexo_ci=q1_03
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

*************************
*** VARIABLES DE RAZA ***
*************************
gen raza_ci=.
replace raza_ci= 1 if (q1_06==4)
replace raza_ci= 2 if (q1_06==1) & raza_ci==.
replace raza_ci= 3 if (q1_06==2 | q1_06==3 |q1_06==5 |q1_06==6 |q1_06==7 |q1_06==8) & raza_ci==.

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo"

gen raza_idioma_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

***************
* edad_ci     * 
***************
gen edad_ci=q1_04
replace edad_ci=. if edad_ci<0
label var edad_ci "Edad del individuo"

***************
* civil_ci    * 
***************
gen civil_ci=1     if q1_05==1
replace civil_ci=2 if q1_05==2 | q1_05==3
replace civil_ci=3 if q1_05==4 | q1_05==5
replace civil_ci=4 if q1_05==6
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

***************
* jefe_ci     * 
***************
gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"

****************
* nconyuges_ch * 
****************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

****************
* nhijos_ch    * 
****************
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

****************
* notropari_ch * 
****************
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

******************
* notronopari_ch * 
******************
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"

****************
* nempdom_ch   * 
****************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

****************
* clasehog_ch  * 
****************
gen clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

****************
* nmiembros_ch * 
****************
bysort idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=4) 
label variable nmiembros_ch "Numero de familiares en el hogar"

****************
* nmayor21_ch  * 
****************
bysort idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci>=21 & edad_ci<=98))
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

****************
* nmenor21_ch  * 
****************
bysort idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci<21))
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

****************
* nmayor65_ch  * 
****************
bysort idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci>=65))
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
* nmenor6_ch   * 
****************
bysort idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci<6))
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
* nmenor1_ch   * 
****************
bysort idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci<1))
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
* miembros_ci   * 
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

		*********************************
		* VARIABLES DEL MERCADO LABORAL *
		*********************************

*******************
****condocup_ci****
*******************

gen condocup_ci=.
replace condocup_ci=1 if q2_04==1 | q2_05==1 | (q2_06==1 | q2_06==2) | (q2_07==1 & (q2_09>=1&q2_09<=3)) | (q2_10==1)
replace condocup_ci=2 if (q2_19==1 | q2_19==2) & q2_14==1 & (q2_15>=1&q2_15<=10)
replace condocup_ci=2 if (q2_19==1 | q2_19==2) & q2_14==2 & (q2_17==1|q2_17==2)
recode condocup_ci (.=3) if edad_ci>=15
replace condocup_ci=4 if edad<15
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

****************
* horaspri_ci  * 
****************
gen horaspri_ci=q3_03
label var horaspri_ci "Horas totales trabajadas en la actividad principal"

****************
* horastot_ci  * 
****************
gen horastot_ci=q3_05
label var horastot_ci "Horas totales trabajadas en todas las actividades"

****************
* desalent_ci  * 
**************** 
gen desalent_ci=(q2_17==9 | q2_17==10)
label var desalent_ci "Trabajadores desalentados"

****************
* subemp_ci    * 
**************** 
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci<=30 & q3_11==1)
label var subemp_ci "Personas en subempleo por horas"
 
****************
*tiempoparc_ci * 
**************** 
gen tiempoparc_ci=(q3_03<=30 & q3_11==1) 
replace tiempoparc_ci=. if emp_ci!=1
label var tiempoparc_c "Personas que trabajan medio tiempo" 

****************
*categopri_ci  * 
**************** 
gen categopri_ci=.
*replace categopri_ci=1 if q3_16==1
replace categopri_ci=2 if q3_16==3 | q3_16==4
replace categopri_ci=3 if q3_16==1
replace categopri_ci=4 if q3_16==2
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional en la actividad principal"

****************
*categosec_ci  * 
****************
gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categosec_ci 3"Empleado" 4" No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

****************
* contrato_ci  * 
**************** 
gen contrato_ci=(q3_18==1)
replace contrato_ci=. if emp_ci!=1
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

****************
* nempleos_ci  * 
**************** 
gen nempleos_ci=.
label var nempleos_ci "Número de empleos" 
label def nempleos_ci 1"Un empleo" 2"Más de un empleo"
label val nempleos_ci nempleos_ci

****************
* spublico_ci  * 
**************** 
gen spublico_ci=(q3_34==3 | q3_34==4)
replace spublico_ci=. if emp_ci!=1
label var spublico_ci "Personas que trabajan en el sector público"

		********************************
		* VARIABLES DE DEMANDA LABORAL *
		********************************

****************
* ocupa_ci     * 
****************
* Utiliza CIUO-08 (SGR 6/18/18)

destring isco_code, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (isco_code>=2111 & isco_code<=3522) & emp_ci==1
replace ocupa_ci=2 if (isco_code>=1111 & isco_code<=1439) & emp_ci==1
replace ocupa_ci=3 if (isco_code>=4110 & isco_code<=4419 | isco_code>=410 & isco_code<=430) & emp_ci==1
replace ocupa_ci=4 if ((isco_code>=5211 & isco_code<=5249) | (isco_code>=9510 & isco_code<=9520)) & emp_ci==1
replace ocupa_ci=5 if ((isco_code>=5111 & isco_code<=5169) | (isco_code>=5311 & isco_code<=5419) | (isco_code>=9111 & isco_code<=9129) | (isco_code>=9611 & isco_code<=9624) ) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((isco_code>=6111 & isco_code<=6340) | (isco_code>=9211 & isco_code<=9216)) & emp_ci==1
replace ocupa_ci=7 if ((isco_code>=7111 & isco_code<=8350) | (isco_code>=9311 & isco_code<=9412)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (isco_code>=110 & isco_code<=310) & emp_ci==1
replace ocupa_ci=9 if (isco_code==9629) & emp_ci==1

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"Profesional y tecnico" 2"Director o funcionario sup" 3"Administrativo y nivel intermedio"
label define ocupa_ci  4 "Comerciantes y vendedores" 5 "En servicios" 6 "Trabajadores agricolas", add
label define ocupa_ci  7 "Obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

*ISIC Rev.4
destring isic_code, replace

gen rama_ci=.
replace rama_ci=1 if (isic_code>0 & isic_code<=400)      & emp_ci==1
replace rama_ci=2 if (isic_code>=500 & isic_code<=1000)  & emp_ci==1
replace rama_ci=3 if (isic_code>=1010 & isic_code<=3400) & emp_ci==1
replace rama_ci=4 if (isic_code>=3500 & isic_code<=4000) & emp_ci==1
replace rama_ci=5 if (isic_code>=4100 & isic_code<=4400) & emp_ci==1
replace rama_ci=6 if ((isic_code>=4500 & isic_code<=4800) | (isic_code>=5500 & isic_code<=5700)) & emp_ci==1
replace rama_ci=7 if ((isic_code>=4900 & isic_code<=5400) | (isic_code>=6100 & isic_code<=6199)) & emp_ci==1
replace rama_ci=8 if (isic_code>=6400 & isic_code<=8300) & emp_ci==1
replace rama_ci=9 if ((isic_code>=5800 & isic_code<=6090) | (isic_code>=6200 & isic_code<=6399) | (isic_code>=8400 & isic_code<=9900))& emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

		**************
		***INGRESOS***
		**************
****************
* ylmpri_ci    * 
****************
foreach var of varlist q6_01 q6_06 q6_04a q6_04b q6_04c q6_04d q6_04e q6_04f{
replace `var'=. if `var'<0
}
*
egen ylmpri_ci=rsum(q6_01 q6_06 q6_04a q6_04b q6_04c q6_04d q6_04e), missing
replace ylmpri_ci=. if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

****************
* ylnmpri_ci   * 
**************** 

foreach var of varlist q6_05a-q6_05i{
replace `var'=. if `var'<0
}
*
replace q6_09=. if q6_09<0 | q6_08==2

egen ylnmpri_ci=rsum(q6_05a q6_05b q6_05c q6_05d q6_05e q6_05f q6_05g q6_05h q6_05i q6_09), missing
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal" 

****************
* ylmsec_ci    * 
**************** 
replace q6_10=. if q6_10<0
gen ylmsec_ci=q6_10
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 
 
****************
* ylnmsec_ci   * 
**************** 
gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

****************
* ylmotros_ci  * 
**************** 
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

****************
* ylnmotros_ci * 
**************** 
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

****************
* nrylmpri_ci  * 
**************** 
gen nrylmpri_ci=(emp_ci==1 & ylmpri_ci==.)
replace nrylmpri_ci=. if emp_ci!=1
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

****************
* ylm_ci       * 
**************** 
gen ylm_ci=ylmpri_ci+ylmsec_ci
replace ylm_ci=. if emp_ci!=1
label var ylm_ci "Ingreso laboral monetario total" 

****************
* ylnm_ci      * 
**************** 
gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral NO monetario total"  

****************
* ynlm_ci      * 
****************
foreach var of varlist q6_11 q6_12 q6_13 q6_14 q6_15 q6_16 q6_17 q6_18 q6_19 q6_20a q6_20b q6_21 q6_22 q6_23 q6_24a q6_24b{
replace `var'=. if `var'<0
}
*
*http://www.bankofguyana.org.gy/bog/images/research/Reports/Dec2017.pdf#page=72
replace q6_20b=q6_20b*206.50

egen ynlm_ci =rsum(q6_11 q6_12 q6_13 q6_14 q6_15 q6_16 q6_17 q6_18 q6_19 q6_20a q6_20b q6_21 q6_22 q6_23 q6_24a q6_24b)
label var ynlm_ci "Ingreso no laboral monetario"  

****************
* ynlnm_ci     * 
**************** 
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


****************
* nrylmpri_ch  * 
**************** 
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

****************
* ylm_ch       * 
**************** 
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

****************
* ylnm_ch      * 
**************** 
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

************
* ylmnr_ch *
************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***********
* ynlm_ch *
***********
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"

*************
* ynlnm_ch  *
*************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*****************
* ymlhopri_ci   *
*****************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 

*************
* ylmho_ci  *
*************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 

****************
* rentaimp_ch  * 
**************** 
g rentaimp_ch=.

****************
* autocons_ci  * 
**************** 
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

****************
* autocons_ci  * 
**************** 
gen autocons_ch=.
label var autocons_ch "Autoconsumo del hogar"

****************
* remesas_ci   * 
**************** 
gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
* remesas_ch   * 
**************** 
gen remesas_ch=.
label var remesas_ch "Remesas mensuales del hogar"

****************
* durades_ci   * 
**************** 
/*
           1 Menos que 3 meses
           2 Tres a cinco meses
           3 Seis a doce meses
           4 Un a dos años
           5 Tres a cuatro años
           6 Cinco o más años
           7 No sabe
Obs. Unidad de medida: promedio de meses (por eso se divide entre 2)
*/
gen durades_ci=.
*Menos que 3 meses: 1 semana a 4.3*3=12.9 semanas
replace durades_ci=(1+12.9)/2/4.3 if q2_18==1
*Tres a cinco meses
replace durades_ci=(3+5)/2 if q2_18==2
*Seis a doce meses
replace durades_ci=(6+12)/2 if q2_18==3
*Un a dos años
replace durades_ci=(12+24)/2 if q2_18==4
*Tres a cuatro años
replace durades_ci=(36+48)/2 if q2_18==5
*Cinco o más años
replace durades_ci=(60+72)/2 if q2_18==6
label variable durades_ci "Duracion del desempleo en meses"

****************
* antiguedad_ci* 
**************** 
/*
           1 Menos que 6 meses
           2 De 6 meses a menos de un año
           3 Un año o más, pero menos que cinco años
           4 Cinco años o más, pero menos que diez años
           5 Diez años o más
*/
gen antiguedad_ci=.
*Menos que 6 meses
replace antiguedad_ci=(1+6)/12 if q3_40==1
*De 6 meses a menos de un año
replace antiguedad_ci=(6+11)/2 if q3_40==2
*Un año o más, pero menos que cinco años
replace antiguedad_ci=(12+59)/2 if q3_40==3
*Cinco años o más, pero menos que diez años
replace antiguedad_ci=(60+119)/2 if q3_40==4
*Diez años o más
replace antiguedad_ci=(120+132)/2 if q3_40==5
label var antiguedad_ci "Antiguedad en la actividad actual en años"
replace antiguedad_ci=. if  emp_ci!=1 | antiguedad_ci>edad_ci

		************************
		* VARIABLES EDUCATIVAS *
		************************

****************
* asiste_ci    * 
**************** 
gen asiste_ci=(q1_12==1)
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

****************
* aedu_ci      * 
**************** 
/*
*La base solo tiene dos categorías: Primary and secondary
gen aedu_ci=.
replace aedu_ci=0   if q1_13==1             /*Pre-primary*/
replace aedu_ci=1   if q1_13==2 & q1_14==1  /*Primary*/
replace aedu_ci=2   if q1_13==2 & q1_14==2 
replace aedu_ci=3   if q1_13==2 & q1_14==3     
replace aedu_ci=4   if q1_13==2 & q1_14==4     
replace aedu_ci=5   if q1_13==2 & q1_14==5     
replace aedu_ci=6   if q1_13==2 & q1_14==6 

replace aedu_ci=6   if q1_13==3 & q1_14==1  /*Secondary*/
replace aedu_ci=6   if q1_13==3 & q1_14==6  /*Preguntar*/
replace aedu_ci=7   if q1_13==3 & q1_14==7
replace aedu_ci=8   if q1_13==3 & q1_14==8     
replace aedu_ci=9   if q1_13==3 & q1_14==9  
replace aedu_ci=10  if q1_13==3 & q1_14==10
replace aedu_ci=11  if q1_13==3 & q1_14==11
replace aedu_ci=12  if q1_13==3 & q1_14==12

replace aedu_ci=2 if q1_14==2 & q1_13==1	   /*Captura el grado 1 y 2*/
replace aedu_ci=3 if q1_14==3 & q1_13==1
replace aedu_ci=4 if q1_14==4 & q1_13==1
replace aedu_ci=5 if q1_14==5 & q1_13==1
replace aedu_ci=6 if q1_14==6 & q1_13==1

*Preguntar q1_15 */

*Modificado por Angela Lopez 05142019
gen aedu_ci=.
replace aedu_ci=0   if q1_11==2              /*never attended */
replace aedu_ci=0   if q1_13==1 			 /*Pre-primary*/
replace aedu_ci=0   if q1_13==2 & q1_14==1   

replace aedu_ci=q1_14 if q1_13==2 & q1_14!=1 & q1_14!=. /*primary*/
replace aedu_ci=6 	  if q1_13==3 & q1_14==1 | q1_14== 6 

replace aedu_ci=q1_14 if q1_13==3 & q1_14>=7 & q1_14!=. /*secondary*/
replace aedu_ci=11    if q1_13==4 &  q1_15 == 1

replace aedu_ci=11+2 if q1_13==4 & q1_15== 2  /*postsecondary */
replace aedu_ci=11+2 if q1_13==5 & q1_15== 2  // tech

replace aedu_ci=11+3 if q1_13==5 & q1_15== 3 // Univ
replace aedu_ci=11+4 if q1_13==5 & q1_15== 4 // Bachel

replace aedu_ci=11+5 if q1_13==5 & q1_15== 5 //posgr
replace aedu_ci=11+6 if q1_13==5 & q1_15== 6 //masters
replace aedu_ci=11+8 if q1_13==5 & q1_15== 7 //doc

label var aedu_ci "Anios de educacion aprobados" 

**************
***eduno_ci***
**************
gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<16
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=16
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
gen edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=. 
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=. 
label variable edus2c_ci "2do ciclo de la secundaria completo"

***************
***edupre_ci***
***************
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
g asispre_ci=1 if asiste_ci == 1 & q1_13 == 1
la var asispre_ci "Asiste a educacion prescolar"

**************
***eduac_ci***
**************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

****************
**pqnoasis_ci***
****************
gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"

**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = .
label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


**************
**repite_ci***
**************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado"

**************
*repiteult_ci*
**************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el último grado"

**************
*edupub_ci   *
**************
gen edupub_ci=.
label var edupub_ci "Personas que asisten a centros de enseñanza públicos"

		******************************************
		* VARIABLES DE INFRAESTRUCTURA DEL HOGAR *
		******************************************

***************
* aguared_ch  *
***************
gen aguared_ch=.
label var aguared_ch "Acceso a fuente de agua por red"

***************
* aguadist_ch *
***************
gen aguadist_ch=.
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la vivienda" 2"Afuera de la vivienda" 3"La acarrean"
label val aguadist_ch aguadist_ch

***************
* aguamala_ch *
***************
gen aguamala_ch=.
label var aguamala_ch "La principal fuente de agua es unimproved según MDG"

***************
* aguamide_ch *
***************
gen aguamide_ch=.
label var aguamide_ch "El hogar usa un medidor para pagar por su consumo de agua"

***************
* luz_ch      *
***************
gen luz_ch=.
label var luz_ch "La principal fuente de iluminación es electricidad"

***************
* luzmide_ch  *
***************
gen luzmide_ch=.
label var luzmide_ch "El hogar usa un medidor para pagar el consumo de electricidad"

***************
* combust_ch  *
***************
gen combust_ch=.
label var combust_ch "El combustible pricipal usado en el hogar es gas o electricidad"

***************
* bano_ch     *
***************
gen bano_ch=.
label var bano_ch "El hogar tiene algún tipo de servicio higiénico"

***************
* banoex_ch   *
***************
gen banoex_ch=.
label var banoex_ch "El servicio higiénico es de uso exclusivo del hogar"

***************
* des1_ch     *
***************
gen des1_ch=.
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

***************
* des2_ch     *
***************
gen des2_ch=.
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

***************
* piso_ch     *
***************
gen piso_ch=.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

***************
* pared_ch    *
***************
gen pared_ch=.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch

***************
* techo_ch    *
***************
gen techo_ch=.
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes"
label val techo_ch techo_ch

***************
* resid_ch    *
***************
gen resid_ch=.
label var resid_ch "Método de eliminación de residuos"
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch =.

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch =.

***************
* dorm_ch     *
***************
gen dorm_ch=.
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

***************
* cuartos_ch  *
***************
gen int cuartos_ch =.
label var cuartos_ch "Cantidad de habitaciones en el hogar"

***************
* cocina_ch  *
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar" 

***************
* telef_ch    *
***************
bysort idh_ch: gen telef_ch=.
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
* refrig_ch   *
***************
g refrig_ch=.
label var refrig_ch "El hogar posee heladera o refrigerador"

***************
* freez_ch    *
***************
gen freez_ch=.
label var freez_ch "El hogar posee congelador"

***************
* auto_ch     * 
***************
bysort idh_ch: gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"

***************
* compu_ch    * 
***************
bysort idh_ch: gen compu_ch=.
label var compu_ch "El hogar posee computadora"

***************
* internet_ch * 
***************
bysort idh_ch: gen internet_ch=.
label var internet_ch "El hogar tiene conexión a Internet"

*************
* cel_ch    * 
*************
bysort idh_ch: gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefónico celular"

***************
* vivi1_ch    * 
*************** 
gen vivi1_ch=.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

***************
* vivi2_ch    * 
*************** 
gen vivi2_ch=.
label var vivi2_ch "La vivienda es una casa o un departamento"

***************
* viviprop_ch * 
*************** 
gen viviprop_ch=.
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

***************
* vivitit_ch  * 
***************
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad" 

***************
* vivialq_ch  * 
***************
gen vivialq_ch=.
label var vivialq_ch "Alquier mensual"

***************
*vivialqimp_ch* 
***************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen aux=real(q3_29)
gen cotizando_ci=.
replace cotizando_ci=1 if (q3_26==1 | aux==1 | aux==2)
recode cotizando_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizando_ci1=.
replace cotizando_ci1=1 if (q3_26==1 | aux==1 | aux==2)
recode cotizando_ci1 .=0 if (condocup_ci==1 | condocup_ci==2 | condocup_ci==3)
label var cotizando_ci1 "Cotizante a la Seguridad Social"
drop aux
****************
*afiliado_ci****
****************
gen afiliado_ci=.	
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
* Esta encuesta sólo incluye 3 tipos de pensiones: NIS pension, old age pension (Non-contributiva), and service pension.
egen vejez=rsum(q6_11 q6_12 q6_13), m
gen tipopen_ci=.
replace tipopen_ci = 1 if (vejez > 0 & vejez!= .)
/*replace tipopen_ci = 2 if (invalidez> 0 & invalidez!= .)
replace tipopen_ci = 3 if (montepio> 0 & montepio!= .)
replace tipopen_ci = 4 if (orfandad> 0 & orfandad != .)
replace tipopen_ci = 12 if (vejez > 0 & vejez!= .) | (invalidez> 0 & invalidez!= .)
replace tipopen_ci = 13 if (vejez > 0 & vejez!= .) | (montepio> 0 & montepio!= .)
replace tipopen_ci = 14 if (vejez > 0 & vejez!= .) | (orfandad> 0 & orfandad != .)
replace tipopen_ci = 23 if (invalidez> 0 & invalidez!= .)| (montepio> 0 & montepio!= .)
replace tipopen_ci = 24 if (invalidez> 0 & invalidez!= .) | (orfandad> 0 & orfandad != .)

replace tipopen_ci = 123 if (vejez > 0 & vejez!= .) | (invalidez> 0 & invalidez!= .) | (montepio> 0 & montepio!= .)

replace tipopen_ci = 1234 if (vejez > 0 & vejez!= .) | (invalidez> 0 & invalidez!= .) | (montepio> 0 & montepio!= .) | (orfandad> 0 & orfandad != .)
*/

label define  t 1 "Jubilacion" /*2 "Pension por servicio" 3 "Pension viudez" 4 "Orfandad" 12 " Jub y inv" 13 "Jub y viud" 14 "Jub y orfandad" 23 "Viud e inv" 24 "orfandad y inv"  123 "Jub inv viud" 1234 "Todas"*/
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
label define instcot_ci 1 "AFP" 2 "IPS ex-INP" 3 "CAPRE-DENA" 4 "DIPRECA" 5  "Otros"
label value instcot_ci instcot_ci
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (q3_19==2) & categopri_ci==3
replace tipocontrato_ci=2 if (q3_19==1) & categopri_ci==3
replace tipocontrato_ci=3 if (q3_18==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
gen cesante_ci=1 if q4_02==1 & (q2_14==1 | q2_17==1 | q2_17==1 )
replace cesante_ci=0 if q4_02==2 & (q2_14==1 | q2_17==1 | q2_17==1 )
label var cesante_ci "Desocupado - definicion oficial del pais"	

**************
***tamemp_ci**
**************
gen tamemp_ci=1 if q3_38==1 | q3_38==2 
replace tamemp_ci=2 if q3_38==3 | q3_38==4 | q3_38==5 
replace tamemp_ci=3 if q3_38==6

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

gen tamemp_o=1 if q3_38==1 | q3_38==2 | q3_38==3 
replace tamemp_o=2 if q3_38==4 | q3_38==5 
replace tamemp_o=3 if q3_38==6 

label var tamemp_ci "# empleados en la empresa segun rangos-OECD"
label define tamemp_o 1 "[1-9]" 2 "[10-49]" 3 "[50 y mas]"
label value tamemp_o tamemp_o

*************
**pension_ci*
*************
*En este caso solo tendrán valores positivos quienes tienen pension_ci==1
gen pension_ci=(q6_11>0&q6_11<.) | (q6_13>0&q6_13<.) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************
egen ypen_ci=rsum(q6_11 q6_13), missing
replace ypen_ci=. if ypen_ci<0
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=(q6_12>0&q6_12<.)
*egen auxpens=rsum(y26_1am  y26_1dm ), missing
*gen pensionsub_ci=1 if auxpens>0 & auxpens!=.
*recode pensionsub_ci .=0 
*label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=q6_12
*gen ypensub_ci=auxpens
*replace ypensub_ci=. if auxpens<0
*drop auxpens
*label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**salmm_ci***
*************
*https://guyanachronicle.com/2016/11/24/new-minimum-wage-order
gen salmm_ci= 44200
label var salmm_ci "Salario minimo legal"

*************
**tecnica_ci*
*************
gen tecnica_ci=.
replace tecnica_ci=1 if q1_15==2
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"

**************
**categoinac_ci*
**************

gen categoinac_ci=1 if q2_20==4
replace categoinac_ci=2 if q2_20==1
replace categoinac_ci=3 if q2_20==2
replace categoinac_ci=4 if (q2_20==3 | q2_20==5 | q2_20==6 | q2_20==7 | q2_20==8)

label var categoinac_ci "Condición de inactividad"
label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
label value categoinac_ci categoinac_ci
	
***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

g formal_1=cotizando_ci1

*******************
***  benefdes_ci  ***
*******************

g benefdes_ci=.
replace benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci  ***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"

* variables que faltan crear
gen tcylmpri_ci =.
gen tcylmpri_ch =.

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch idh_ch	idp_ci	factor_ci sexo_ci edad_ci isic_code isco_code  ///
raza_idioma_ci  id_ind_ci id_afro_ci raza_ci  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch, first


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) */
rename isic_code codindustria
rename isco_code codocupa
compress

saveold "`base_out'", replace


log close


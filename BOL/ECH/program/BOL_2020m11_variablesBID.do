	* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*



global ruta = "${surveysFolder}"

local PAIS BOL
local ENCUESTA ECH
local ANO "2020"
local ronda m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11
Autores: Mayra Sáenz
	Stephanie González Rubio (Jul 18, 2019)
	Cesar Lins (2021/03/09)
	Carolina Rivas (June 2021)
Última versión: 

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use "`base_in'", clear


	****************
	* region_BID_c *
	****************
	
gen region_BID_c=3

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
	************
	* region_c *
	************
*destring depto, gen(region_c)
rename depto region_c

/*label define region_c ///
1"Chuquisaca"         ///     
2"La Paz"             ///
3"Cochabamba"         ///
4"Oruro"              ///
5"Potosí"             ///
6"Tarija"             ///
7"Santa Cruz"         ///
8"Beni"               ///
9"Pando"              
label value region_c region_c
*/
label var region_c "division politica, estados"

***************
***factor_ch***
***************
gen factor_ch= factor
label variable factor_ch "Factor de expansion del hogar"


	***************
	***upm_ci***
	***************
gen upm_ci=upm
	***************
	***estrato_ci***
	***************
gen estrato_ci=estrato


***************
****idh_ch*****
***************
sort folio
egen idh_ch = group(folio)
destring idh_ch, replace
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************
gen idp_ci= nro
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 	if area==2
replace zona_c=1 	if area==1
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c
label variable zona_c "Zona del pais"

************
****pais****
************
gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2020
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
*19 de octubre al 20 de diciembre
gen mes_c=11
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if  s01a_05==1
replace relacion_ci=2 if  s01a_05==2
replace relacion_ci=3 if  s01a_05==3 
replace relacion_ci=4 if  s01a_05>=4 &  s01a_05<=9
replace relacion_ci=5 if  s01a_05==10 |  s01a_05==12 
replace relacion_ci=6 if  s01a_05==11

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************
gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci = s01a_02
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci= s01a_03
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************
destring s01a_09, i("NA") replace
gen civil_ci=.
replace civil_ci=1 		if s01a_09==1
replace civil_ci=2 		if s01a_09==2 | s01a_09==3
replace civil_ci=3 		if s01a_09==4 | s01a_09==5
replace civil_ci=4 		if s01a_09==6
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

*************
***jefe_ci***
*************
gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"

******************
***nconyuges_ch***
******************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"

****************
***nempdom_ch***
****************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

*****************
***clasehog_ch***
*****************
gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************
by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************
by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************
by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************
by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************
by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"



*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

	***************
	***afroind_ci***
	***************
**Pregunta: como boliviano o boliviana, pertenece a una nación o pueblo indígena? (s03a_04) (PERTENCE 1, NO PERTENECE 2, NO SOY BOLIVIANO/BOLIVIANA 3)
**Pregunta: a qué nación o pueblo pertenece? (s03a_04n)(ALL CATEGORIES ARE INDIGENOUS INCLUDING AFROBOLIVIANS)
** La base original difiere de la base armonizada (s03a_04npioc vs s03a_04n). 


gen afroind_ci=. 
/*
replace afroind_ci=1 if s03a_04==1
replace afroind_ci=2 if s03a_04==0 
replace afroind_ci=3 if s03a_04==2 
replace afroind_ci=9 if s03a_04==3 
*/

	***************
	***afroind_ch***
	***************
gen afroind_jefe=.
gen afroind_ch  = .
/*
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
*/
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2012


	*************
	***dis_ci***
	**************
gen dis_ci = .
gen dis_ch =.

/*
foreach i in a b c d e f  {
forvalues j=2/4 {
recode dis_ci 0=1 if s04a_06`i'==`j'
}
}
recode dis_ci nonmiss=. if s04a_06a==9 & s04a_06b==9 & s04a_06c==9 & s04a_06d==9 & s04a_06e==9 & s04a_06f==9 
recode dis_ci nonmiss=. if s04a_06a>=. & s04a_06b>=. & s04a_06c>=. & s04a_06d>=. & s04a_06e>=. & s04a_06f>=.

	*************
	***dis_ch***
	**************	
egen dis_ch =.
*sum(dis_ci), by(idh_ch) 
*/

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/************************************************************************************************************
* Líneas de pobreza oficiales
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =z
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =zext
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
*https://www.ine.gob.bo/subtemas_cuadros/salarioMinimo_html/SalarioMinimo_41201.htm
gen salmm_ci= 2122	/*2018: 2060*/
label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
destring s04f_39, replace i("NA")

gen afiliado_ci= s04f_39==1	
	*Solo eisten missing para los p_aspirantes condact==3 en la variable de afiliacón. Los condact>=3 son inactivos.
	*tab condact s06h_59, missing
recode afiliado_ci .=0  if condact>=1 & condact<=3
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
destring s05a_01*, i("NA") replace
gen tipopen_ci=.
replace tipopen_ci=1 if s05a_01a>0 & s05a_01a!=.
replace tipopen_ci=2 if s05a_01d>0 & s05a_01d!=.
replace tipopen_ci=3 if s05a_01b>0 & s05a_01b!=.
replace tipopen_ci=4 if s05a_01c>0 & s05a_01c!=. 
replace tipopen_ci=12 if (s05a_01a>0 & s05a_01d>0) & (s05a_01a!=. & s05a_01d!=.)
replace tipopen_ci=13 if (s05a_01a>0 & s05a_01b>0) & (s05a_01a!=. & s05a_01b!=.)
replace tipopen_ci=23 if (s05a_01d>0 & s05a_01b>0) & (s05a_01d!=. & s05a_01b!=.)
replace tipopen_ci=123 if (s05a_01a>0 & s05a_01b>0 & s05a_01c>0) & (s05a_01a!=. & s05a_01b!=. & s05a_01c!=.)
label define tipopen_ci 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci tipopen_ci
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
gen instcot_ci=. 


****************
****condocup_ci*
****************
destring ocupado desocupado pei, i("NA") replace

gen condocup_ci=.
replace condocup_ci=1 if ocupado==1 
replace condocup_ci=2 if desocupado==1 
replace condocup_ci=3 if pei==1

recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci


*************
*cesante_ci* 
*************
gen cesante_ci =.
label var cesante_ci "Desocupado - definicion oficial del pais"	

/*
gen cesante_ci = 1 if condocup_ci==2 & s06a_07==1
replace cesante_ci = 0 if condocup_ci==2 & s06a_07==2
label var cesante_ci "Desocupado - definicion oficial del pais"	
*/
*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa segun rangos"

/*
replace tamemp_ci=1 if s06b_21>=1 & s06b_21<=5
replace tamemp_ci=2 if s06b_21>=6 & s06b_21<=49
replace tamemp_ci=3 if s06b_21>49 & s06b_21!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
*/

*Bolivia micro 1 a 4 pequeña 5 a 14 Mediana 15-40 Grande mas 41
gen tamemp=.
/*
replace tamemp=1 if s06b_21>=1 & s06b_21<=4
replace tamemp=2 if s06b_21>=5 & s06b_21<=14
replace tamemp=3 if s06b_21>=15 & s06b_21<=40
replace tamemp=4 if s06b_21>=41 & s06b_21!=.
label var tamemp "# empleados en la empresa segun rangos"
label define tamemp 1 "Micro" 2 "Pequeña" 3 "Mediana" 4 "Grande"
label value tamemp tamemp
*/
*************
**pension_ci*
*************
egen aux_p=rsum(s05a_01a s05a_01b s05a_01c s05a_01d ), missing

gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************
*11/4/2015 MGD: no considerar ceros. En el SIMS se reemplazan los 0 en missings. 
gen ypen_ci=aux_p 
*recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci = (s05a_01e_1==1)  
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
*Modificación Cesar Lins - Feb 2021, s07a_010 changed to s07a_01e0 (same as 2017)
gen  ypensub_ci=s05a_01e_2 if s05a_01e_2>0 & s05a_01e_2!=. 
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
	
/* Esta sección es para los residentes habituales del hogar mayores a 7 años. Sin embargo, las variables construidas 
por el centro de estadística tienen en cuenta a la población con 10 años o más. Esto no es un problema dado que el 
programa para generar los indicadores de sociómetro restrige  todo a 15 o más años para que haya comparabilidad entre
países
*/

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"

*****************
***desalent_ci***
*****************
destring s04a_07, i("NA") replace
gen desalent_ci=(emp_ci==0 & (s04a_07==3 | s04a_07==4))
replace desalent_ci=. if emp_ci==.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
  *s04e_33ab: 23b. cuantas horas en promedio trabaja al dia .. ? (minutos)
  *s04e_33aa: 23a. cuantas horas en promedio trabaja al dia .. ? (horas)
  *s04e_32: 22. cuantos dias a la semana trabaja
destring s04e_33ab s04e_33aa s04e_32, i("NA") replace
gen aux_min=  s04e_33ab/60
egen horas_min= rsum(s04e_33aa aux_min), m

gen horaspri_ci = horas_min*s04e_32
replace horaspri_ci=. if s04e_32==. | s04e_33aa==. |  s04e_33ab==.
replace horaspri_ci=. if emp_ci!=1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
gen horastot_ci = .
/*
s6_39a:  39a. cuantos dias trabajó la semana anterior ?
s6_39ba: 39b. cuantas horas promedio al dia trabajÓ la semana anterior ?
s6_39m: 39b. cuantos minutos promedio al dia trabajÓ la semana anterior ?


gen aux_min2=s06f_45b/60
egen horas_min2=rsum(s06f_45a aux_min2)
gen horassec_ci= horas_min2*s06f_44
replace horassec_ci=. if s06f_44==. | s06f_45a==. | s06f_45b==.
replace horassec_ci=. if emp_ci!=1

egen horastot_ci= rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = . if horaspri_ci == . & horassec_ci == .
replace horassec_ci=. if emp_ci~=1
*/
***************
***subemp_ci***
***************

* Se considera subempleo visible: quiere trabajar mas horas y esta disponible. 
gen subemp_ci=.
/*
gen subemp_ci=0
replace subemp_ci=1 if (s06h_52==1 & s06h_53==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"
*/

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
*Mod. MLO 2015, 10
/*
replace tiempoparc_ci=(s06h_52==2 & horaspri_ci<30 & emp_ci == 1)
replace tiempoparc_ci=. if emp_ci==0
*replace tiempoparc_ci=1 if s6_46==2 & horastot_ci<=30 & emp_ci == 1
*replace tiempoparc_ci=0 if s6_46==2 & emp_ci == 1 & horastot_ci>30
label var tiempoparc_c "Personas que trabajan medio tiempo" 
*/

******************
***categopri_ci***
******************
destring s04b_12 s04e_29, i("NA") replace

gen categopri_ci=.
replace categopri_ci=1 if s04b_12>=4 & s04b_12<=6
replace categopri_ci=2 if s04b_12==3
replace categopri_ci=3 if s04b_12==1 | s04b_12==2 
replace categopri_ci=4 if s04b_12==7
replace categopri_ci=0 if s04b_12==8

replace categopri_ci=. if emp_ci~=1
label define categopri_ci 0 "Otro" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************
gen categosec_ci=.
replace categosec_ci=1 if s04e_29>=4 & s04e_29<=6
replace categosec_ci=2 if s04e_29==3
replace categosec_ci=3 if s04e_29==1 | s04e_29==2 
replace categosec_ci=4 if s04e_29==7
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"


/*
replace tipocontrato_ci=1 if s06b_17==4 & categopri_ci==3
replace tipocontrato_ci=2 if s06b_17==1 & categopri_ci==3
*replace tipocontrato_ci=3 if ((s6_21==2 | s6_21==4) | tipocontrato_ci==.) & categopri_ci==3
*Mod. MLO 2015,10
replace tipocontrato_ci=3 if ((s06b_17==2 | s06b_17==3 | s06b_17==5) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
label var nempleos_ci "Número de empleos" 

/*
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s06e_39==1
label var nempleos_ci "Número de empleos" 
label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
label value nempleos_ci nempleos_ci
*/
				

*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
label var firmapeq_ci "Trabajadores informales"

/*
replace firmapeq_ci=1 if  s6_20>=1 & s6_20<=5 
replace firmapeq_ci=0 if  s6_20>=6 & s6_20!=.
label var firmapeq_ci "Trabajadores informales"
 */
 
*****************
***spublico_ci***
*****************
gen spublico_ci=.
label var spublico_ci "Personas que trabajan en el sector público"

/*
gen spublico_ci=.
replace spublico_ci=1 if s06b_18==1 | s06b_18==2
replace spublico_ci=0 if (s06b_18>=3 & s06b_18<=6)
replace spublico_ci=. if emp_ci~=1
label var spublico_ci "Personas que trabajan en el sector público"
*/
**************
***ocupa_ci***
**************
*cob_op:
*NA: No se puede estandarizar ya que no se distingue entre dos categorias:
*comerciantes y vendedores y trabajadores en servicios 

* MGD 5/24/2016: no es posible dividir entre trabajadores de los servicios y comerciantes.
* Usa CIUO-08
/*
gen ocupa_ci=.

replace ocupa_ci=1 if (cob_op==2 | cob_op==3)& emp_ci==1
replace ocupa_ci=2 if (cob_op==1) & emp_ci==1
replace ocupa_ci=3 if (cob_op==4) & emp_ci==1
replace ocupa_ci=5 if (cob_op==5) & emp_ci==1
replace ocupa_ci=6 if (cob_op==6) & emp_ci==1
replace ocupa_ci=7 if (cob_op==7 | cob_op==8) & emp_ci==1
replace ocupa_ci=8 if (cob_op==0) & emp_ci==1
replace ocupa_ci=9 if (cob_op==9) & emp_ci==1
label define ocupa_ci 1 "profesional y tecnico" 2"director o funcionario sup" 3 "administrativo y nivel intermedio"
label define ocupa_ci 4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci 7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci 8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"
*/

* MGD 6/15/2017: usa variable a mas digitos para hacer la clasificación, se cambia a CIUO-08

*Modificación Cesar Lins - Feb 2021, s06b_110 changed to s06b_11a_cod
tostring s04b_09a_cod, replace
gen longi=length(s04b_09a_cod)
g new_cod=substr(s04b_09a_cod,1,3) if longi>3
replace new_cod=s04b_09a_cod if longi<=3
destring new_cod, replace

gen ocupa_ci=.
replace ocupa_ci=1 if ((new_cod>=210 & new_cod<=352) | (new_cod>=21 & new_cod<=34)) & emp_ci==1
replace ocupa_ci=2 if ((new_cod>=110 & new_cod<=143) |  new_cod==11) & emp_ci==1
replace ocupa_ci=3 if ((new_cod>=410 & new_cod<=441) |  new_cod==41 |  new_cod==42 |  new_cod==43) & emp_ci==1
replace ocupa_ci=4 if ((new_cod>=520 & new_cod<=529) | (new_cod>=910 & new_cod<=911) | new_cod==52 | new_cod==91) & emp_ci==1
replace ocupa_ci=5 if ((new_cod>=510 & new_cod<=519) | (new_cod>=530 & new_cod<=541) | (new_cod>=910 & new_cod<=912) | new_cod==51) & emp_ci==1
replace ocupa_ci=6 if ((new_cod>=610 & new_cod<=634) | (new_cod>=920 & new_cod<=921) | new_cod==61) & emp_ci==1
replace ocupa_ci=7 if ((new_cod>=710 & new_cod<=835) | (new_cod>=930 & new_cod<=970) | new_cod==71 | new_cod==72 | new_cod==73 | new_cod==75 | new_cod==81 | new_cod==83)& emp_ci==1
replace ocupa_ci=8 if ((new_cod>=0 & new_cod<=8) | new_cod==10 | new_cod==20) & emp_ci==1

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"

*************
***rama_ci***
*************
/*
caeb_op:
0 Agricultura,Ganadería,Caza,Pesca y Silv
1 Explotación de Minas y Canteras
2 Industria Manufacturera
3 Suministro de electricidad,gas,vapor y
4 Suministro de agua, evac. de aguas res,
5 Construcción
6 Venta por mayor y menor,reparación de a
7 Transporte y Almacenamiento
8 Actividades de alojamiento y servicio d
9 Informaciones y Comunicaciones
10 Intermediación Financiera y Seguros
11 Actividades inmobiliarias
12 Servicios Profesionales y Técnicos
13 Actividades de Servicios Administrativo
14 Adm. Pública, Defensa y Seguridad Socia
15 Servicios de Educación
16 Servicios de Salud y Asistencia Social
17 Actividades artisticas,entretenimiento
18 Otras actividades de servicios
19 Actividades de Hogares Privados
20 Servicio de Organismos Extraterritorial
99 NS/NR
*/ 

destring caeb_op, i("NA") replace
gen rama_ci=.
replace rama_ci=1 if caeb_op==0 & emp_ci==1
replace rama_ci=2 if caeb_op==1 & emp_ci==1
replace rama_ci=3 if caeb_op==2 & emp_ci==1
replace rama_ci=4 if (caeb_op==3 | caeb_op==4) & emp_ci==1
replace rama_ci=5 if caeb_op==5 & emp_ci==1
replace rama_ci=6 if (caeb_op>=6 & caeb_op<=8) & emp_ci==1 
replace rama_ci=7 if caeb_op==7 & emp_ci==1
replace rama_ci=8 if (caeb_op>=10 & caeb_op<=11) & emp_ci==1
replace rama_ci=9 if (caeb_op==9 | (caeb_op>=12 & caeb_op<=20)) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
gen durades_ci=.
label variable durades_ci "Duracion del desempleo en meses"

/*
replace durades_ci=s06a_08a/4.3  if s06a_08b==2
replace durades_ci=s06a_08a       if s06a_08b==4
replace durades_ci=s06a_08a*12   if s06a_08b==8
label variable durades_ci "Duracion del desempleo en meses"
*/
*******************
***antiguedad_ci***
*******************
destring s04b_11ba s04b_11bb s04a_06, i("NA") replace
gen antiguedad_ci=.	
replace antiguedad_ci=s04b_11ba/52.14  	if s04b_11bb==2 & emp_ci==1
replace antiguedad_ci=s04b_11ba/12   	if s04b_11bb==4 & emp_ci==1
replace antiguedad_ci=s04b_11ba	   		if s04b_11bb==8 & emp_ci==1
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***categoinac_ci***
*******************
*Modificacion MLO, 2015 m4 (se cambió s5_14 por s6_09)
gen categoinac_ci =1 	  if (s04a_06==3 & condocup_ci==3)
replace categoinac_ci = 2 if  (s04a_06==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s04a_06==2 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringe a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

g formal_1=afiliado_ci

**************
***INGRESOS***
**************

*************************
*********LABORAL*********
*************************
/*
s2_38f:
1  diario
2  semanal
3  quincenal
4  mensual
8  anual
*/

destring s04a_* s04c_* s04d_* s04f_* s03a_*  s03b_* s07a_* s03c_13, i("NA") replace


*******************
* salario líquido *
*******************
gen yliquido = .
replace yliquido= s04c_18a*30	if s04c_18b==1
replace yliquido= s04c_18a*4.3	if s04c_18b==2
replace yliquido= s04c_18a*2	if s04c_18b==3
replace yliquido= s04c_18a		if s04c_18b==4
replace yliquido= s04c_18a/2	if s04c_18b==5
replace yliquido= s04c_18a/3	if s04c_18b==6
replace yliquido= s04c_18a/6	if s04c_18b==7
replace yliquido= s04c_18a/12	if s04c_18b==8

**************
* comisiones *
**************
gen ycomisio = .
replace ycomisio= s04c_20aa*30	if s04c_20ab==1
replace ycomisio= s04c_20aa*4.3	if s04c_20ab==2
replace ycomisio= s04c_20aa*2	if s04c_20ab==3
replace ycomisio= s04c_20aa		if s04c_20ab==4
replace ycomisio= s04c_20aa/2	if s04c_20ab==5
replace ycomisio= s04c_20aa/3	if s04c_20ab==6
replace ycomisio= s04c_20aa/6 	if s04c_20ab==7
replace ycomisio= s04c_20aa/12 	if s04c_20ab==8

****************
* horas extras *
****************
gen yhrsextr= .
replace yhrsextr= s04c_20ba *30	    if s04c_20bb==1
replace yhrsextr= s04c_20ba *4.3  	if s04c_20bb==2
replace yhrsextr= s04c_20ba *2		if s04c_20bb==3
replace yhrsextr= s04c_20ba 		if s04c_20bb==4
replace yhrsextr= s04c_20ba /2		if s04c_20bb==5
replace yhrsextr= s04c_20ba /3		if s04c_20bb==6
replace yhrsextr= s04c_20ba /6	    if s04c_20bb==7
replace yhrsextr= s04c_20ba /12	    if s04c_20bb==8

*********
* prima *
*********

gen yprima = .
replace yprima = s04c_19a/12

*************
* aguinaldo *
*************

gen yaguina = .
replace yaguina = s04c_19b/12

*********************************************************************

*************
* alimentos *
*************
gen yalimen = .
replace yalimen= s04c_22a2 *30		if s04c_22a1 ==1 & s04c_22a==1
replace yalimen= s04c_22a2 *4.3 	if s04c_22a1 ==2 & s04c_22a==1
replace yalimen= s04c_22a2 *2		if s04c_22a1 ==3 & s04c_22a==1
replace yalimen= s04c_22a2 		    if s04c_22a1 ==4 & s04c_22a==1
replace yalimen= s04c_22a2 /2		if s04c_22a1 ==5 & s04c_22a==1
replace yalimen= s04c_22a2 /3		if s04c_22a1 ==6 & s04c_22a==1
replace yalimen= s04c_22a2 /6		if s04c_22a1 ==7 & s04c_22a==1
replace yalimen= s04c_22a2 /12		if s04c_22a1 ==8 & s04c_22a==1

**************
* transporte *
**************
gen ytranspo = .
replace ytranspo= s04c_22b2*30	    if s04c_22b1==1 & s04c_22b==1
replace ytranspo= s04c_22b2*4.3	    if s04c_22b1==2 & s04c_22b==1
replace ytranspo= s04c_22b2*2		if s04c_22b1==3 & s04c_22b==1
replace ytranspo= s04c_22b2 	    if s04c_22b1==4 & s04c_22b==1
replace ytranspo= s04c_22b2/2		if s04c_22b1==5 & s04c_22b==1
replace ytranspo= s04c_22b2/3		if s04c_22b1==6 & s04c_22b==1
replace ytranspo= s04c_22b2/6		if s04c_22b1==7 & s04c_22b==1
replace ytranspo= s04c_22b2/12	    if s04c_22b1==8 & s04c_22b==1

**************
* vestimenta *
**************

gen yvesti = .
replace yvesti= s04c_22c*30			if s04c_22c1==1 & s04c_22c==1
replace yvesti= s04c_22c*4.3		if s04c_22c1==2 & s04c_22c==1
replace yvesti= s04c_22c*2		    if s04c_22c1==3 & s04c_22c==1
replace yvesti= s04c_22c			if s04c_22c1==4 & s04c_22c==1
replace yvesti= s04c_22c/2		    if s04c_22c1==5 & s04c_22c==1
replace yvesti= s04c_22c/3		    if s04c_22c1==6 & s04c_22c==1
replace yvesti= s04c_22c/6		    if s04c_22c1==7 & s04c_22c==1
replace yvesti= s04c_22c/12		if s04c_22c1==8 & s04c_22c==1

************
* vivienda *
************

gen yvivien = .
replace yvivien= s04c_22d2*30		if s04c_22d1==1 & s04c_22d==1
replace yvivien= s04c_22d2*4.3	    if s04c_22d1==2 & s04c_22d==1
replace yvivien= s04c_22d2*2		if s04c_22d1==3 & s04c_22d==1
replace yvivien= s04c_22d2		    if s04c_22d1==4 & s04c_22d==1
replace yvivien= s04c_22d2/2		if s04c_22d1==5 & s04c_22d==1
replace yvivien= s04c_22d2/3		if s04c_22d1==6 & s04c_22d==1
replace yvivien= s04c_22d2/6		if s04c_22d1==7 & s04c_22d==1
replace yvivien= s04c_22d2/12		if s04c_22d1==8 & s04c_22d==1


*************
* otros *
*************

gen yotros = .
replace yotros= s04c_22e2*30	if s04c_22e1==1 & s04c_22e==1
replace yotros= s04c_22e2*4.3	if s04c_22e1==2 & s04c_22e==1
replace yotros= s04c_22e2*2	    if s04c_22e1==3 & s04c_22e==1
replace yotros= s04c_22e2		if s04c_22e1==4 & s04c_22e==1
replace yotros= s04c_22e2/2	    if s04c_22e1==5 & s04c_22e==1
replace yotros= s04c_22e2/3		if s04c_22e1==6 & s04c_22e==1
replace yotros= s04c_22e2/6		if s04c_22e1==7 & s04c_22e==1
replace yotros= s04c_22e2/12	if s04c_22e1==8 & s04c_22e==1
**********************************************************************


**********************************
* ingreso act. pr independientes *
**********************************
*Aquí se tiene en cuenta el monto de dinero que les queda a los independientes para el uso del hogar
gen yactpri = .
replace yactpri= s04d_25a*30	if s04d_25b==1
replace yactpri= s04d_25a*4.3	if s04d_25b==2
replace yactpri= s04d_25a*2		if s04d_25b==3
replace yactpri= s04d_25a		if s04d_25b==4
replace yactpri= s04d_25a/2		if s04d_25b==5
replace yactpri= s04d_25a/3		if s04d_25b==6
replace yactpri= s04d_25a/6		if s04d_25b==7
replace yactpri= s04d_25a/12	if s04d_25b==8

*********************
* salario liquido 2 *
*********************
/*

           1 diario
           2 semanal
           3 quicenal
           4 mensual
           5 bimestral
           6 trimestral
           7 semestral
           8 anual

*/

gen yliquido2 = .
replace yliquido2= s04f_35a*30		if s04f_35b==1
replace yliquido2= s04f_35a*4.3		if s04f_35b==2
replace yliquido2= s04f_35a*2		if s04f_35b==3
replace yliquido2= s04f_35a			if s04f_35b==4
replace yliquido2= s04f_35a/2		if s04f_35b==5
replace yliquido2= s04f_35a/3		if s04f_35b==6
replace yliquido2= s04f_35a/6		if s04f_35b==7
replace yliquido2= s04f_35a/12		if s04f_35b==8

*****************
* Horas extra 2 *
*****************
gen yhrsextr2 = .
replace yhrsextr2=s04f_36a1/12 if s04f_36a==1

***************************************
* alimentos, transporte y vestimenta2 *
***************************************
gen yalimen2 = .
replace yalimen2=s04f_36b1/12	if s04f_36b==1

**************
* vivienda 2 *
**************
*Modificación Cesar Lins - Feb 2021, replaced by 2017 variable names
gen yvivien2= .
replace yvivien2=s04f_36c1/12	if s04f_36c==1



*************************
******NO-LABORAL*********
*************************
destring s05a_* s05b_*, i("NA") replace

*************
* intereses *
*************
gen yinteres = .
replace yinteres = s05a_02a	

**************
* alquileres *
**************

gen yalqui = .
replace yalqui = s05a_02b	


****************
* otras rentas *
****************

gen yotren = .
replace yotren = s05a_02c

**************
* jubilacion *
**************

gen yjubi = .
replace yjubi = s05a_01a 

**************
* benemerito *
**************

gen ybene = .
replace ybene = s05a_01b 

*************
* invalidez *
*************

gen yinvali = .
replace yinvali = s05a_01c

**********
* viudez *
**********

gen yviudez = .
replace yviudez = s05a_01d  


************************
* alquileres agricolas *
************************

gen yalqagri = .
replace yalqagri =  s05a_03a/12		


**************
* dividendos *
**************

gen ydivi = .
replace ydivi =  s05a_03b/12


*************************
* alquileres maquinaria *
*************************

gen yalqmaqui = .
replace yalqmaqui = s05a_03c/12

 
******************
* indem. trabajo *
******************

gen yindtr = .
replace yindtr =  s05a_04a/12


******************
* indem. seguros *
******************

gen yindseg = .
replace yindseg = s05a_04b/12


******************
* renta dignidad *
******************
* Modificación Cesar Lins - Feb 2021, s07a_010 changed to s07a_01e0
gen ybono = .
replace ybono = s05a_01e_2

******************
* otros ingresos *
******************

gen yotring = .
replace yotring = s05a_04d/12

*******************
* asist. familiar *
*******************
/*
  2  semanal
  3  quincenal
  4  mensual
  5  bimestral
  6  trimestral
  7  semestral
  8  anual
*/
* No hay la categoria de diario en s7b_5ab
gen yasistfam = .
replace yasistfam= s05b_05aa*4.3	if s05b_05ab==2
replace yasistfam= s05b_05aa*2		if s05b_05ab==3
replace yasistfam= s05b_05aa		if s05b_05ab==4
replace yasistfam= s05b_05aa/2		if s05b_05ab==5
replace yasistfam= s05b_05aa/3		if s05b_05ab==6
replace yasistfam= s05b_05aa/6		if s05b_05ab==7
replace yasistfam= s05b_05aa/12		if s05b_05ab==8

*********************
* Trans. monetarias *
*********************
* No hay la categoria de diario en s7b_5bb
* Modificación SGR Julio 19: Desde 2018 desagregan más las preguntas: Dinero+Alimentos+Otros bonos sociales

*Modificación Cesar Lins - Feb 2021, all variables in ydinero, yalimento, yotro_bono, yotro_bono2
* renamed according to the pattern: s07b_0xb --> s07b_0xba, s07b_0x* --> s07b_0xbb
gen ydinero = .
replace ydinero = s05b_05ba*4.3		if s05b_05bb==2
replace ydinero= s05b_05ba*2		if s05b_05bb==3
replace ydinero= s05b_05ba	    	if s05b_05bb==4
replace ydinero= s05b_05ba/2		if s05b_05bb==5
replace ydinero= s05b_05ba/3		if s05b_05bb==6
replace ydinero= s05b_05ba/6		if s05b_05bb==7
replace ydinero= s05b_05ba/12		if s05b_05bb==8

gen yalimento= .
replace yalimento = s05b_05ca*4.3	    if s05b_05cb==2
replace yalimento= s05b_05ca*2			if s05b_05cb==3
replace yalimento= s05b_05ca	        if s05b_05cb==4
replace yalimento= s05b_05ca/2			if s05b_05cb==5
replace yalimento= s05b_05ca/3			if s05b_05cb==6
replace yalimento= s05b_05ca/6			if s05b_05cb==7
replace yalimento= s05b_05ca/12			if s05b_05cb==8


gen yotro_bono= .
replace yotro_bono= s05b_06ea	        if s05b_06eb==4
replace yotro_bono= s05b_06ea/12		if s05b_06eb==8

gen yotro_bono2= .
replace yotro_bono2= s05b_06fa	    	if s05b_06fb==4
replace yotro_bono2= s05b_06fa/12		if s05b_06fb==8

egen ytransmon=rsum(ydinero yotro_bono), missing

/*
replace ytransmon= s07b_05ba*4.3	if s07b_05bb==2
replace ytransmon= s07b_05ba*2		if s07b_05bb==3
replace ytransmon= s07b_05ba	    if s07b_05bb==4
replace ytransmon= s07b_05ba/2		if s07b_05bb==5
replace ytransmon= s07b_05ba/3		if s07b_05bb==6
replace ytransmon= s07b_05ba/6		if s07b_05bb==7
replace ytransmon= s07b_05ba/12		if s07b_05bb==8
*/
***********
* remesas *
***********
/*
    MONEDA

A. BOLIVIANOS 
B. EUROS
C. DÓLARES
D. PESOS ARGENTINOS
E. REALES
F. PESOS CHILENOS
G. OTRO

https://www.bcb.gob.bo/?q=cotizaciones_tc
Al 2 DE ENERO DE 2018
*/
destring s05c_*, replace i("NA")
gen s6_112= .
replace s6_112 =  s05c_09a 			 if s05c_09b==1 /*bolivianos*/
replace s6_112 =  s05c_09a*8.23678   if s05c_09b==2 /*euro*/
replace s6_112 =  s05c_09a*6.96		 if s05c_09b==3 /*dolar*/
replace s6_112 =  s05c_09a*0.36836   if s05c_09b==4 /*peso argentino*/
replace s6_112 =  s05c_09a*2.07094   if s05c_09b==5 /*real*/
replace s6_112 =  s05c_09a*0.01115	 if s05c_09b==6 /*peso chileno*/
*replace s6_112 =  s07c_08a*2.00961   if s07c_08b==7 /*soles*/ En la 201 es Otro

* se suman remesas monetarias y en especie
egen rem = rsum(s05c_10 s6_112), m

gen yremesas = .
replace yremesas= rem*4.3		if s05c_08==2
replace yremesas= rem*2		    if s05c_08==3
replace yremesas= rem			if s05c_08==4
replace yremesas= rem/2			if s05c_08==5
replace yremesas= rem/3			if s05c_08==6
replace yremesas= rem/6			if s05c_08==7
replace yremesas= rem/12		if s05c_08==8

/* 
ylm:
yliquido 
ycomisio 
ypropinas 
yhrsextr 
yprima 
yaguina
yactpri 
yliquido2

ylnm:
yrefrige 
yalimen 
ytranspo 
yvesti 
yvivien 
yguarde */


***************
***ylmpri_ci***
***************

egen ylmpri_ci=rsum(yliquido ycomisio yhrsextr yprima yaguina yactpri), missing
replace ylmpri_ci=. if yliquido ==. & ycomisio ==. &  yhrsextr ==. & yprima ==. &  yaguina ==. &  yactpri==.  
replace ylmpri_ci=. if emp_ci~=1
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


******************
*** ylnmpri_ci ***
******************

egen ylnmprid=rsum(yalimen ytranspo yvesti yvivien yotros), missing
replace ylnmprid=. if yalimen==. & ytranspo==. & yvesti==. & yvivien==. & yotros==.   
replace ylnmprid=0 if categopri_ci==4

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

egen ylmsec_ci= rsum(yliquido2 yhrsextr2), missing
replace ylmsec_ci=. if emp_ci~=1 & yhrsextr2==. & yliquido2 ==.
replace ylmsec_ci=0 if categosec_ci==4
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


******************
****ylnmsec_ci****
******************

egen ylnmsec_ci=rsum(yalimen2  yvivien2), missing
replace ylnmsec_ci=. if yalimen2==.  & yvivien2==.  
replace ylnmsec_ci=0 if categosec_ci==4
replace ylnmsec_ci=. if emp_ci==0
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

*****************
***ylmotros_ci***
*****************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


 
/* 

ynlm:

yinteres 
yalqui 
yjubi 
ybene 
yinvali 
yviudez 
yotren  
yalqagri 
ydivi 
yalqmaqui  
yindtr  
yindseg 
yheren 
ypasu 
ybono  
yotring  
yasistfam 
ytransmon 
yremesas 
yinvers 
yhipotec 
ybonos 
ypresta 
yprestata 
yinmueb 
yinmrur 
yvehi 
yelec 
ymuebles 
yjoyas */



*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(yinteres yalqui yjubi ybene yinvali yviudez yotren yalqagri ydivi yalqmaqui yindtr yindseg ybono yotring yasistfam ytransmon yremesas ), missing
replace ynlm_ci=. if 	yinteres==. & yalqui==. & yjubi==. & ybene==. & yinvali==. & yviudez==. & yotren==. & yalqagri==. & ydivi==. & yalqmaqui==. & yindtr==. & yindseg==. & ///
			ybono==. & yotring==. & yasistfam==. & ytransmon==. & yremesas==. 
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************
*Modificación SGR Julio 2019: En esta encuesta se pregunta por transferencia en alimentos u otras especies.
egen ynlnm_ci=rsum(yalimento yotro_bono2), missing
replace ynlnm_ci=. if yalimento==. & yotro_bono2==.
label var ynlnm_ci "Ingreso no laboral no monetario" 


*****************
***remesas_ci***
*****************

gen remesas_ci=yremesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 


************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

**************
*** ylm_ch ***
**************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

****************
*** ylmnr_ch ***
****************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

*******************
*** remesas_ch ***
*******************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 

***************
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"

****************
*** ynlnm_ch ***
****************
*Modificación SGR Julio 2019: En esta encuesta se pregunta por transferencia en alimentos u otras especies.
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*******************
*** autocons_ci ***
*******************
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

*******************
*** autocons_ch ***
*******************
gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"

*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch= .
label var rentaimp_ch "Rentas imputadas del hogar"

*****************
***ylhopri_ci ***
*****************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 

***************
***ylmho_ci ***
****************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 



****************************
***VARIABLES DE EDUCACION***
****************************

/*En esta sección es sólo para personas de 4 años o más de edad*/

/*
 s5_02a
11. NINGUNO
12. CURSO DE ALFABETIZACIÓN
13. EDUCACIÓN INICIAL O PRE-ESCOLAR (PRE KINDER/KINDER)
81.OTROS CURSOS (Duración menor a 1 año)

PRIMARIA
21.BÁSICO (1 A 5 AÑOS)
22.INTERMEDIO (1 A 3 AÑOS)
23.MEDIO (1 A 4 AÑOS)
31. PRIMARIA (1 A 8 AÑOS)
41. PRIMARIA (1 A 6 AÑOS)


SECUNDARIA
32. SECUNDARIA (1 A 4 AÑOS)
42. SECUNDARIA (1 A 6 AÑOS)

EDUCACIÓN SUPERIOR
71. NORMAL (ESCUELA SUP. DE FORMACIÒN DE MAESTROS)
72. UNIVERSIDAD PÚBLICA (Licenciatura)
73. UNIVERSIDAD PRIVADA (Licenciatura)
74.POSTGRADO DIPLOMADO
75. POSTGRADO MAESTRÍA,
76. POSTGRADO DOCTORADO
77. TÉCNICO DE UNIVERSIDAD
78 TÉCNICO DE INSTITUTO (Duración mayor o igual a 1 año)
79. INSTITUTOS DE FORMACIÓN MILITAR Y POLICIAL

		  
*No se consideran por no ser educación formal sino "Alternativa" o "No formal"
EDUCACIÓN DE ADULTOS(Sistema Antiguo)
51. EDUCACIÓN BÁSICA DE ADULTOS (EBA)
52. CENTRO DE EDUCACIÓN MEDIA DE ADULTOS (CEMA)
61.EDUCACIÓN JUVENIL ALTERNATIVA (EJA)
62.EDUCACIÓN PRIMARIA DE ADULTOS (EPA)
63.EDUCACIÓN SECUNDARIA DE ADULTOS (ESA)
64.PROGRAMA NACIONAL DE POST ALFABETIZACIÓN
65.EDUCACIÓN ESPECIAL
80. EDUCACIÓN TÉCNICA DE ADULTOS (ETA)	  
*/


/* Opcion 2
gen aedu_ci = .
* Ninguno o preescolar
replace aedu_ci = 0 if s05a_02a==11 | s05a_02a==12 | s05a_02a==13 | s05a_02a==81

* Primaria & Secundaria
* Sistema escolar antiguo
replace aedu_ci = s05a_02c if s05a_02a==21
replace aedu_ci = s05a_02c+5 if s05a_02a==22
replace aedu_ci = s05a_02c+8 if s05a_02a==23
* Sistema escolar anterior
replace aedu_ci = s05a_02c if s05a_02a==31
replace aedu_ci = s05a_02c+8 if s05a_02a==32
* Sistema escolar actual
replace aedu_ci = s05a_02c if s05a_02a==41
replace aedu_ci = s05a_02c+6 if s05a_02a==42

* Superior
replace aedu_ci = s05a_02c+12 if (s05a_02a>=71 & s05a_02a<=73) | (s05a_02a>=77 & s05a_02a<=79)
replace aedu_ci = s05a_02c+17 if (s05a_02a>=74 & s05a_02a<=76)
recode aedu_ci 25=22
*/

gen aedu_ci = .

* Ninguno o preescolar
replace aedu_ci = 0 if s03a_02a==11 | s03a_02a==12 | s03a_02a==13 

*Primaria & Secundaria
replace aedu_ci = s03a_02c if s03a_02a==21 | s03a_02a==31 | s03a_02a==41 | s03a_02a==51

* Secundaria  sistema escolar antiguo 
replace aedu_ci = s03a_02c+5   if  s03a_02a==22 // Intermedio
replace aedu_ci = s03a_02c+5+3 if    s03a_02a==23 // Medio

* Secundaria sistema escolar anterior
replace aedu_ci = s03a_02c+8     if (s03a_02a==32) 

* Secundaria sistema escolar actual 
replace aedu_ci = s03a_02c+6     if (s03a_02a==42)

* Educacion para adultos 
replace aedu_ci = s03a_02c+3     if (s03a_02a==52)

** La educación Alternativa para jóvenes y adultos no hace parte de la educación formal (es preparatiorio para ello)

replace aedu_ci = 0 if (s03a_02a>=61 & s03a_02a<=65)

* Superior

replace aedu_ci = s03a_02c + 12     	if s03a_02c <= 5 & (s03a_02a ==71 | s03a_02a ==72) // normal, universidad y técnico-tecnológico
replace aedu_ci = s03a_02c + 12 		if s03a_02a >=76 & s03a_02a <=81
replace aedu_ci = s03a_02c + 12 + 5 	if s03a_02c <= 5 & (s03a_02a ==73 | s03a_02a ==74) // postgrado, maestria
replace aedu_ci = s03a_02c + 12 + 5 + 2 if s03a_02c <= 5 & (s03a_02a ==75) // doctorado

* Terminación nivel

replace aedu_ci = 12+4   if s03a_02a ==71 & s03a_02c == 8 //Terminó escuela normal
replace aedu_ci = 12+5   if s03a_02a ==72 & s03a_02c == 8 //Terminó universidad
replace aedu_ci = 12+5+1 if s03a_02a ==73 & s03a_02c == 8 //Terminó posgrado
replace aedu_ci = 12+5+2 if s03a_02a ==74 & s03a_02c == 8 //Terminó maestria
replace aedu_ci = 12+5+5 if s03a_02a ==75 & s03a_02c == 8 //Terminó doctorado


**************
***eduno_ci***
**************

gen byte eduno_ci= 1 if aedu_ci == 0
replace eduno_ci= 0 if aedu_ci > 0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<=5)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=7 & aedu_ci<=11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(aedu_ci>=6 & aedu_ci<=7)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==8)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci>=9 & aedu_ci<=11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************
* Se incorpora la restricción s5_02b<8 para que sea comparable con los otros años LCM dic 2013

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & s03a_02c<8)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & s03a_02c==8) | (aedu_ci>=17 & aedu_ci<.)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa"

***************
***edupre_ci***
***************

gen byte edupre_ci=(s03a_02a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
	g asispre_ci=.
	/*
	replace asispre_ci=1 if s05b_10==1 & s05a_06a==13
	recode asispre_ci (.=0)
	replace asispre_ci = 0 if s05b_10 == 2
	la var asispre_ci "Asiste a educacion prescolar"
	*/
**************
***eduac_ci***
**************

* Se cambia para universidad completa o más 
gen byte eduac_ci=.
replace eduac_ci=1 if (s03a_02a>=72 & s03a_02a<=73)
replace eduac_ci=0 if (s03a_02a>=77 & s03a_02a<=79)
label variable eduac_ci "Superior universitario vs superior no universitario"
/*cambio de eduuc_ci de LCM introcucido por YL solo para este año.
YL: No estoy segura de aceptar esta definicion pero la copio para hacerla comparable con
los otros años*/

***************
***asiste_ci***
***************

*(introducido por YL): Se cambia la forma de cálculo porque se deben considerar los rangos de edad 
*gen asiste_ci= 1 if s5b_10==1
*replace asiste_ci = 0 if s5b_10==2
*Modificación Mayra Sáenz Enero-2017: Se genera la dummy de acuerdo al documento metodológico.
*Modificación Victor Saldarriaga Enero-2017: Muchos de estos niños se encuentran en periodo de vacaciones o ya han culminado el año escolar y por ello no asisten a la escuela
*											 en ese caso se considera que asisten si es que se encuentran matriculados (s5a_4 == 1) y si es que se encuentran en vacaciones/culminó estudios. 
*gen asiste_ci= (s5b_10==1) | (s5a_4==1 & s5b_10==2 & s5b_11==1) | (s5a_4==1 & s5b_10==2 & s5b_11==2)
*Modificación Mayra Sáenz Enero 25-2017: Se incluye la asistencia de los menores de 5
*gen asiste_ci=1 if (s05b_10==1) | (s05a_05==1 & s05b_10==2 &s05b_11==1) | (s05a_05==1 & s05b_10==2 &s05b_11==2) |s04c_22 ==1 | (s05a_06a>=13 & s05a_06a<=80)
*Modificación Daniela Zuluaga Nov-2018: Se pone la condición de escolaridad con & y se elimina la opción de "culminó estudios"
*replace asiste_ci=0 if (s05b_10==2 | s05a_05==2  |s04c_22 ==2) & asiste_ci==.

gen asiste_ci =((s03a_04==1) & (s03a_05a>=13 & s03a_05a<=80))
label variable asiste_ci "Asiste actualmente a la escuela"

/*
replace asiste_ci=0 if (s05b_10==2 | s05a_05==2  |s04c_22 ==2)
replace asiste_ci=1 if (((s05b_10==1) | (s05a_05==1 & s05b_10==2 &s05b_11==1) |s04c_22 ==1) & (s05a_06a>=13 & s05a_06a<=80))
replace asiste_ci=0 if asiste_ci ==.
label variable asiste_ci "Asiste actualmente a la escuela"
*/
**************
***pqnoasis***
**************

gen pqnoasis_ci=.
/*
clonevar pqnoasis_ci=s05b_11


**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	*/
**************
*pqnoasis1_ci*
**************

gen pqnoasis1_ci=.
/*
gen pqnoasis1_ci = 1 if s05b_11==5
replace pqnoasis1_ci = 2 if s05b_11==11 | s05b_11==12
replace pqnoasis1_ci = 3 if s05b_11==3
replace pqnoasis1_ci = 4 if s05b_11==9
replace pqnoasis1_ci = 5 if s05b_11==4 | s05b_11==10
replace pqnoasis1_ci = 6 if s05b_11==2
*replace pqnoasis1_ci =7 														/*No está esta categoría*/
replace pqnoasis1_ci = 8 if s05b_11==6
replace pqnoasis1_ci = 9 if s05b_11==1  | s05b_11==7 | s05b_11==8 | s05b_11==14

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5 "Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci
*/
***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"

******************
***repiteult_ci***
******************

*En este año no se hace pregunta sobre si individuo repitió año
gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************

/*
s5_09:	   
 1 fiscal - pÚblico
 2 pÚblico de convenio
 3 particular - privado
*/

gen edupub_ci=.
replace edupub_ci= 1 if s03b_07==1
replace edupub_ci= 0 if s03b_07==2
label var edupub_ci "Asiste a un centro de ensenanza público"

**************
***tecnica_ci*
**************

gen tecnica_ci = (s03a_02a==77 | s03a_02a==79 | s03a_02a==76 | s03a_02a==78)
label var tecnica_ci "1=formacion terciaria tecnica"

***************
* Universidad *
***************


gen universidad_ci = (s03a_02a==72 )
label var universidad_ci "1=formacion universitaria"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

****************
***aguared_ch***
****************

/*s1a_11 :	
1 cañería de red dentro de la vivienda?
2 cañería de red fuera de la vivienda, pero dentro del lote
3 pileta pública
4  pozo perforado o entubado, con bomba?
5 pozo escavado cubierto, con bomba? 
6 pozo excavado cubierto, sin bomba?
7 pozo excavado no cubierto? 
8 manantial o vertiente protegida?
9 río/acequia/vertiente no protegida?
10  agua embotellada?
11 carro repartidor (aguatero)?
12 otro? (especifique)
*/

gen aguared_ch=(s07a_01==1 | s07a_01==2)
replace aguared_ch=. if s07a_01==.
label var aguared_ch "Acceso a fuente de agua por red"

****************
***aguared_ch***
****************

gen aguadist_ch=1 if s07a_01==1
replace aguadist_ch=2 if s07a_01==2
replace aguadist_ch=3 if (s07a_01==3 | s07a_01==12)
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_chs1

*****************
***aguamala_ch***
*****************

gen aguamala_ch=(s07a_01==8 | s07a_01==9 | s07a_01==10)
replace aguamala_ch=. if s07a_01==.
label var aguamala_ch "Agua unimproved según MDG" 

*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch=(s07a_04  ==1)
replace luz_ch =. if  s07a_04  == .
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch=. 
*(s01a_25==4 |  s01a_25==6)
*replace combust_ch = . if s01a_25==.
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

gen bano_ch= (s07a_02>=1 & s07a_02<=4)
label var bano_ch "El hogar tiene servicio sanitario"

***************
***banoex_ch***
***************

gen banoex_ch=.
*(s01a_17==1)
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************
/*
s01a_15
1 a la red de alcantarillado?
2 a una cámara séptica?
3 a un pozo de absorción?
4 a la superficie (calle/quebrada/rio)
5 otro
6 no sabe
*/

gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if s07a_03==1 | s07a_03==2
replace des1_ch=2 if s07a_03==3
replace des1_ch=3 if s07a_03==4
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if s07a_03==1 | s07a_03==2 | s07a_03==3 
replace des2_ch=2 if s07a_03==4
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************
gen piso_ch=.
label var piso_ch "Materiales de construcción del piso"  

/*
gen piso_ch=0 if s01a_09==1 
replace piso_ch=1 if  s01a_09>=2 &  s01a_09<=7 
replace piso_ch=2 if  s01a_09==8
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales"
label val piso_ch piso_ch
*/

**************
***pared_ch***
**************
gen pared_ch=.
label var pared_ch "Materiales de construcción de las paredes"

/*
gen pared_ch=0 if s01a_06==6
replace pared_ch=1 if s01a_06>=1 & s01a_06<=5
replace pared_ch=2 if s01a_06==7
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val pared_ch pared_ch
*/

**************
***techo_ch***
**************

gen techo_ch=.
label var techo_ch "Materiales de construcción del techo"

/*
gen techo_ch=0 if s01a_08 ==4
replace techo_ch=1 if s01a_08 >=1 & s01a_08 <=3
replace techo_ch=2 if s01a_08 ==5
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val techo_ch techo_ch
*/

**************
***resid_ch***
**************
gen resid_ch =.
label var resid_ch "Método de eliminación de residuos"

/*
gen resid_ch =0 if s01a_21==5 | s01a_21==6
replace resid_ch=1 if s01a_21==2 | s01a_21==4
replace resid_ch=2 if s01a_21==1 | s01a_21==3
replace resid_ch=3 if s01a_21==7
replace resid_ch=. if s01a_21==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch
*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
*Modificado SGR Julio 2018. En la encuesta se agregó una alternativa más : Cosecha de agua de lluvia
gen aguamejorada_ch = 1 if (s07a_01 >= 1 &  s07a_01 <=3) | s07a_01==5 | s07a_01==6 | s07a_01==9
replace aguamejorada_ch = 0 if (s07a_01 >=7 &  s07a_01 <=8) | (s07a_01 >= 10 &  s07a_01 <=13)
				
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if ((s07a_02>= 1 & s07a_02<=2) & (s07a_03 >= 1 & s07a_03 <=3))
replace banomejorado_ch = 0 if ((s07a_02>= 1 & s07a_02<=2) & (s07a_03 >= 1 & s07a_03 <=3)) | (s07a_02>= 3 & s07a_02<= 6)  | ((s07a_02>= 1 & s07a_02<=2)  & (s07a_03 >= 4 & s07a_03 <=5))
		
*************
***dorm_ch***
*************

gen dorm_ch= .
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************

*Esta pregunta no incluye baño, cocina, lavandería, garage, depósito o negocio 
gen cuartos_ch=.
label var cuartos_ch "Habitaciones en el hogar"


***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=.
label var telef_ch "El hogar tiene servicio telefónico fijo"


********
***NA***
********
gen refrig_ch=.
label var refrig_ch "El hogar posee refrigerador o heladera"

gen freez_ch=.
label var freez_ch "El hogar posee congelador"

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"

gen compu_ch=(s07a_07_1==1)
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************
gen internet_ch=(s07a_05==1)
replace internet_ch = .   if  s07a_05== .
label var internet_ch "El hogar posee conexión a Internet"



************
***cel_ch***
************

gen cel_ch= (s03c_13==1)
label var cel_ch "El hogar tiene servicio telefonico celular"


**************
***vivi1_ch***
**************
gen vivi1_ch=.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"

/*
gen vivi1_ch=1 if s01a_01==1
replace vivi1_ch=2 if s01a_01==3
replace vivi1_ch=3 if s01a_01==2 | (s01a_01>=4 & s01a_01<=6)
replace vivi1_ch=. if s01a_01==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch
*/
*************
***vivi2_ch**
*************
gen vivi2_ch=.
label var vivi2_ch "La vivienda es casa o departamento"

/*
gen vivi2_ch= (vivi1_ch==1 | vivi1_ch==2)
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"
*/

*****************
***viviprop_ch***
*****************

*Se crea una variable parecida, pero con otro nombre
gen viviprop_ch=.
label var viviprop_ch "Propiedad de la vivienda"


/*
gen viviprop_ch=0 if s01a_02==3
replace viviprop_ch=1 if s01a_02==1
replace viviprop_ch=2 if s01a_02==2 
replace viviprop_ch=3 if s01a_02==5 | s01a_02==6 | s01a_02==7 
replace viviprop_ch=4 if s01a_02==4 | s01a_02==8

label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2 "Propia y en proceso de pago" 
label def viviprop_ch 3"Ocupada (propia de facto)" 4"Otra", add
label val viviprop_ch viviprop_ch
*/
****************
***vivitit_ch***
****************

gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch= .
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"

*******************
*** benefdes_ci ***
*******************
g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"

		
*******************
*** SALUD  ***
*******************

*******************
*** cobsalud_ci ***
*******************

gen cobsalud_ci=1  if cobersal<4
replace cobsalud_ci=0 if cobersal==4

label var cobsalud_ci "Tiene cobertura de salud"
label define cobsalud_ci 0 "No" 1 "Si" 
label value cobsalud_ci cobsalud_ci

************************
*** tipocobsalud_ci  ***
************************

gen tipocobsalud_ci=cobersal
replace tipocobsalud_ci=0 if cobsalud_ci==0

label var tipocobsalud_ci "Tipo cobertura de salud"
lab def tipocobsalud_ci 0"Sin cobertura" 1 "Publico" 2"Privado" 3"otros" 
lab val tipocobsalud_ci tipocobsalud_ci


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(s01a_08==3) if s01a_08!=. 	
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	
	/*
	gen migantiguo5_ci=(migrante_ci==1 & inlist(s03a_01a,1,2)) if !mi(migrante_ci) & s03a_01a!=4
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	*/
	
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	
	/*
	gen migantiguo5_ci=(migrante_ci==1 & inlist(s03a_01a,1,2)) if !mi(migrante_ci) & s03a_01a!=4
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	*/
	
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"

******************************
********* PTMC y PNC *********
******************************

* PTMC: s03b_06 ¿Recibió el Bono Juancito Pinto el año pasado (2019)?
* PNC: 	s05a_01e_1 Recibe usted ingresos (rentas) mensuales por: E. ¿Renta Dignidad?
*		s05a_01e_2 Monto

* Ingreso del hogar
egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)
drop ingreso_total

* Personas que perciben transferencias
gen percibe_ptmc_ci = (s03b_06==1)
bys idh_ch: egen ptmc_ch = max(percibe_ptmc)

* Se imputa el ingreso del bono juancito pinto el cual es de Bs. 200 anuales 
* por estudiente que asiste a la escuela y tiene entre 6-20 años
* para este año no se puede ver si asiste a la escuela 

gen ing_bjp = (200/12) if ptmc ==1 & (edad_ci>5 & edad_ci<21)
bys idh_ch: egen ing_ptmc = sum(ing_bjp)
drop ing_bjp

replace ing_ptmc=. if y_hog==.
replace ptmc_ch  = ((percibe_ptmc==1)| (ing_ptmc>0 & ing_ptmc!=.))

* Personas que reciben pensión no contributiva
gen mayor64_ci=(edad>64 & edad!=.)
gen pnc_ci= (s05a_01e_1==1)

* Monto pension no contributiva
bys idh_ch: egen ing_pension = sum(s05a_01e_2)
replace ing_pension=. if y_hog==.

* Ingreso neto del hogar
gen y_pc_net = (y_hog - ing_ptmc - ing_pension) / nmiembros_ch
drop y_hog 

lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

lab def pnc_ci 1 "Beneficiario PNC" 0 "No beneficiario PNC"
lab val pnc_ci pnc_ci

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\Labels&ExternalVars_Harmonized_DataBank.do"

*_____________________________________________________________________________________________________*

*  Pobres extremos, pobres moderados, vulnerables y no pobres 
* con base en ingreso neto (Sin transferencias)
* y líneas de pobreza internacionales
gen     grupo_int = 1 if (y_pc_net<lp31_ci)
replace grupo_int = 2 if (y_pc_net>=lp31_ci & y_pc_net<(lp31_ci*1.6))
replace grupo_int = 3 if (y_pc_net>=(lp31_ci*1.6) & y_pc_net<(lp31_ci*4))
replace grupo_int = 4 if (y_pc_net>=(lp31_ci*4) & y_pc_net<.)

tab grupo_int, gen(gpo_ingneto)

* Crear interacción entre recibirla la PTMC y el gpo de ingreso
gen ptmc_ingneto1 = 0
replace ptmc_ingneto1 = 1 if ptmc_ch == 1 & gpo_ingneto1 == 1

gen ptmc_ingneto2 = 0
replace ptmc_ingneto2 = 1 if ptmc_ch == 1 & gpo_ingneto2 == 1

gen ptmc_ingneto3 = 0
replace ptmc_ingneto3 = 1 if ptmc_ch == 1 & gpo_ingneto3 == 1

gen ptmc_ingneto4 = 0
replace ptmc_ingneto4 = 1 if ptmc_ch == 1 & gpo_ingneto4 == 1

lab def grupo_int 1 "Pobre extremo" 2 "Pobre moderado" 3 "Vulnerable" 4 "No pobre"
lab val grupo_int grupo_int


/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) 
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 */

*Modificación Cesar Lins - Feb 2021, s06b_110 -> s06b_11a_cod
*rename s06b_11a_cod codocupa
rename caeb_op codindustria

compress


* Úlrima modificación Carolina Rivas - June 2021 / saveold didn't work because labels are too long
save "`base_out'", replace 


log close



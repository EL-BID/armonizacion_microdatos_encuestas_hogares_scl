
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

local PAIS BHS
local ENCUESTA LFS
local ANO "2014"
local ronda a


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bahamas
Encuesta: LFS
Round: a
Autores: Melany Gualavisi
Última versión: Melany Gualavisi E-mail: melanyg@iadb.org
Última modificación: Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Enero 2018

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

****************
* region_BID_c *
****************
gen region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
*	region_c  * 
***************

gen region_c=.
label var region_c "division politico-administrativa, dominio estudio encuesta"



******************************
*	idh_ch
******************************
egen idh_ch= concat(island hhno)
la var idh_ch "Household ID"


******************************
*	idp_cI
******************************
egen idp_ci= concat(idh_ch ind_no)
la var idp_ci "Individual ID"

***************
***factor_ch***
***************

gen factor_ch= weights
bys idh_ch: egen aux = max(factor_ch)
replace factor_ch=aux if factor_ch==.
drop aux
label variable factor_ch "Factor de expansion del hogar"

**********
***zona***
**********

g zona_c=1
label variable zona_c "Zona del pais"


************
****pais****
************

gen str3 pais_c="BHS"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2014
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=.
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if  rel_head==1
replace relacion_ci=2 if  rel_head==2
replace relacion_ci=3 if  rel_head==3
replace relacion_ci=5 if  rel_head==5
replace relacion_ci=6 if  rel_head==4

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
* MGD 03/2016: Creación de un factor de expansión alternagtivo que incluya a toda la población ya que el factor solo esta generado para activos.
* Población: 383054
* Observaciones: 6705
*g factor_ci= 383054/6705

/*Daniela Zuluaga- Enero 2018: Para aquellos individuos con factor de expansión como valor perdido, se le asigna el máximo valor del factor de expansión
de los miembros del hogar*/

gen factor_ci=weight 
bys idh_ch: egen aux = max(factor_ch)
replace factor_ci=aux if factor_ci==.
drop aux
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci = sex
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci= age
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************
* No hay categorias para viudo o divorciado
gen civil_ci=.
replace civil_ci=1 if marital_statu ==1
replace civil_ci=2 if marital_statu ==2 | marital_statu ==3
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
/*Daniela Zuluaga-Enero 2018: La encuesta solo identifica personas mayores de 15 años, sin embargo hay dos preguntas que permiten identificar el numero total de mujeres (nfemale)
y el numero total de hombres (nmale) en el hogar. Restandole a estas variables el numero de mujeres mayores de 15 años (nfemales_15) y hombres mayores de 15 años (nmales_15) 
respectivamente podríamos identificar el numero total de niños en el hogar e incluirlos en la suma de miembros del hogar. Se estaría asumiendo entonces que todos los niños
hacen parte del hogar. */

gen ninas=female_total -females_15_years
gen ninos=males_total -male_15_years

by idh_ch, sort: egen aux=sum(relacion_ci>=1 & relacion_ci<=4)
egen nmiembros_ch=rowtotal(aux ninos ninas), m
label variable nmiembros_ch "Numero de familiares en el hogar"

drop ninos ninas aux

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
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
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

*************
**salmm_ci***
*************

gen salmm_ci= .
label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=(i42a_insurance==1)	
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.

label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
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
* Encuesta no incluye a menores y se usa la clasificación del país con variable creada.
gen condocup_ci=.
replace condocup_ci=1 if employ==1
replace condocup_ci=2 if employ==2
replace condocup_ci=3 if employ==3
recode condocup_ci .=4 if edad_ci<15
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if workbefore==1 & condocup_ci==2
replace cesante_ci=0 if workbefore==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
gen tamemp_ci=.
replace tamemp_ci = 1 if i76_business_size==1 | i76_business_size==2
replace tamemp_ci = 2 if i76_business_size==3 | i76_business_size==4
replace tamemp_ci = 3 if i76_business_size==5
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=. 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci =.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen  ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
	

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
gen desalent_ci=(emp_ci==0 & ( i46_not_looking==4))
replace desalent_ci=. if emp_ci==.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=i26a_main_job
replace horaspri_ci=. if emp_ci==0
replace horaspri_ci=. if i26a_main_job==.
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"


*****************
***horastot_ci***
*****************

egen horastot_ci= rsum(horaspri_ci i26b_other_job ) if emp_ci==1  
replace horastot_ci=. if emp_ci==0

***************
***subemp_ci***
***************

gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & emp_ci==1 & i31_available >=1 & i31_available <=6 

label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
gen categopri_ci=.
replace categopri_ci=1 if i33_category==5
replace categopri_ci=2 if i33_category==6
replace categopri_ci=3 if i33_category>=1 & i33_category<=4
replace categopri_ci=4 if i33_category==7
replace categopri_ci=. if emp_ci~=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if i34_contract==1 & i35_duration==2 & categopri_ci==3
replace tipocontrato_ci=2 if i34_contract==1 & i35_duration==1 & categopri_ci==3
replace tipocontrato_ci=3 if i34_contract==2 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************
* La categoría dos indica 2 o mas empleos
gen nempleos_ci=.
replace nempleos_ci=1 if i24_paid_jobs==2
replace nempleos_ci=2 if i24_paid_jobs==3
label var nempleos_ci "Número de empleos" 


*****************
***spublico_ci***
*****************
gen spublico_ci=1 if ((i33_category==1 | i33_category==2) & condocup_ci==1)
replace spublico_ci=0 if spublico_ci==. & condocup_ci==1
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
gen ocupa_ci=.
replace ocupa_ci=1 if ((i56_occupation>=2111 & i56_occupation<=3522))& emp_ci==1
replace ocupa_ci=2 if ((i56_occupation>=1111 & i56_occupation<=1439)) & emp_ci==1
replace ocupa_ci=3 if ((i56_occupation>=4110 & i56_occupation<=4419))& emp_ci==1
replace ocupa_ci=4 if ((i56_occupation>=5200 & i56_occupation<=5249) | (i56_occupation>=9510 & i56_occupation<=9520)) & emp_ci==1
replace ocupa_ci=5 if ((i56_occupation>=5110 & i56_occupation<=5169) | (i56_occupation>=5310 & i56_occupation<=5419) | (i56_occupation>=9110 & i56_occupation<=9129) | (i56_occupation>=9610 & i56_occupation<=9711)) & emp_ci==1
replace ocupa_ci=6 if ((i56_occupation>=6110 & i56_occupation<=6349) | (i56_occupation>=9211 & i56_occupation<=9216)) & emp_ci==1
replace ocupa_ci=7 if ((i56_occupation>=7111 & i56_occupation<=8350) | (i56_occupation>=9310 & i56_occupation<=9412))& emp_ci==1

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"

*************
***rama_ci***
*************
* CIIU rev 4.

gen rama_ci=.
replace rama_ci=1 if (i62_business_act>=113 & i62_business_act<=399) & emp_ci==1
replace rama_ci=2 if i62_business_act>=510 & i62_business_act<=999 & emp_ci==1
replace rama_ci=3 if i62_business_act>=1000 & i62_business_act<=3399 & emp_ci==1
replace rama_ci=4 if i62_business_act>=3500 & i62_business_act<=3999 & emp_ci==1
replace rama_ci=5 if i62_business_act>=4100 & i62_business_act<=4399 & emp_ci==1
replace rama_ci=6 if ((i62_business_act>=4500 & i62_business_act<=4799) | (i62_business_act>=5500 & i62_business_act<=5699)) & emp_ci==1
replace rama_ci=7 if i62_business_act>=4900 & i62_business_act<=5399 & emp_ci==1
replace rama_ci=8 if (i62_business_act>=6400 & i62_business_act<=6899) & emp_ci==1
replace rama_ci=9 if ((i62_business_act>=5800 & i62_business_act<=6399) | (i62_business_act>=6900 & i62_business_act<=9999)) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
* Variable categorica
gen durades_ci=.
replace durades= 0.5  if i51_how_long_lon ==1
replace durades= 2    if i51_how_long_lon ==2
replace durades= 4.5  if i51_how_long_lon ==3
replace durades= 9    if i51_how_long_lon ==4
replace durades= 12   if i51_how_long_lon ==5
label variable durades_ci "Duracion del desempleo en meses"

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.	
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (i43_activity ==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (i43_activity ==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (i43_activity ==4 & condocup_ci==3)
replace categoinac_ci = 4 if  (i43_activity ==7 | i43_activity ==3) & condocup_ci==3
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************
gen byte formal_ci=afiliado_ci
label var formal_ci "1=afiliado o cotizante / PEA"

g formal_1=.
**************
***INGRESOS***
**************

***************
***ylmpri_ci***
***************
*Asalariados
recode i77a1_main_job (99999999999=.)
recode  i78a_first_business (99999999999=.) (1000000000=.)
/*g main_job= i77a1_main_job
replace main_job= i77a1_main_job*365 if i79_often_paid==1
replace main_job= i77a1_main_job*24.3 if i79_often_paid==3
replace main_job= i77a1_main_job*52.1 if i79_often_paid==2
replace main_job= i77a1_main_job if i79_often_paid==4*/

egen ylmpri_aux=rsum( i77a1_main_job i78a_first_business), m 
g ylmpri_ci=ylmpri_aux/12
replace ylmpri_ci=. if emp_ci~=1
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


******************
*** ylnmpri_ci ***
******************

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

recode  i79a2_second_job (99999999999=.)
recode i78b_second_businest (99999999999=.)
/*g second_job= i79a2_second_job
replace second_job= i79a2_second_job*365 if i79_often_paid==1
replace second_job= i79a2_second_job*24.3 if i79_often_paid==3
replace second_job= i79a2_second_job*52.1 if i79_often_paid==2
replace second_job= i79a2_second_job if i79_often_paid==4
*/
egen ylmsec_aux= rsum(i79a2_second_job i78b_second_businest), m
g ylmsec_ci=ylmsec_aux/12
replace ylmsec_ci=. if emp_ci~=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.
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
recode   i79b2_source (99999999999=.)
/*g oth_job= i79b2_source
replace oth_job= i79b2_source*365 if i79_often_paid==1
replace oth_job= i79b2_source*24.3 if i79_often_paid==3
replace oth_job= i79b2_source*52.1 if i79_often_paid==2
replace oth_job= i79b2_source if i79_often_paid==4
*/
gen ylmotros_ci=  i79b2_source/12
replace ylmotros_ci=. if emp_ci~=1
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

******************
***ylnmotros_ci***
******************

gen ylnmotros_ci= .
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

gen ylnm_ci=.
label var ylnm_ci "Ingreso laboral NO monetario total"  


 
*************
***ynlm_ci***
*************

gen ynlm_ci=. 
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


*****************
***remesas_ci***
*****************

gen remesas_ci=.
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
gen ynlnm_ch=.
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
	g ylmhopri_ci = ylmpri_ci / (horaspri_ci * 4.3)
	la var ylmhopri_ci "Salario monetario de la actividad principal" 

***************
***ylmho_ci ***
***************
	g ylmho_ci = ylm_ci / (horastot_ci * 4.3)
	la var ylmho_ci "Salario monetario de todas las actividades" 



****************************
***VARIABLES DE EDUCACION***
****************************

/*NO es posible saber cuantos años de educación tienen los individuos pero si definir cual es 
el nivel mas alto alcanzado*/


gen aedu_ci = .

**************
***eduno_ci***
**************

gen byte eduno_ci= (education==1)
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci= (education==2)
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(education==3)
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(education==4)
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(education==5)
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************
gen byte eduui_ci=((education==6 | education==7) & (highest_certi==5))
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************
gen byte eduuc_ci=((education==6 | education==7) & (highest_certi==6 | highest_certi==7))
label variable eduuc_ci "Universitaria completa"

***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

**************
***eduac_ci***
**************

gen byte eduac_ci=1 if (education==6 | education==7)
replace eduac_ci=0 if education==8
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"vacación/receso" 2"falta de dinero" 3"por trabajo" 4"por enfermedad/accidente/discapacidad"
label def pqnoasis_ci 5"los establecimientos son distantes" 6"culminó sus estudios" 7"edad temprana/ edad avanzada", add
label def pqnoasis_ci 8"falta de interés" 9"labores de casa/ embarazo/cuidado de niños/as" 10"otra", add
label val pqnoasis_ci pqnoasis 

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci**

******************
***pqnoasis1_ci***
******************

gen pqnoasis1_ci=.

***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************

gen edupub_ci=.
label var edupub_ci "Asiste a un centro de ensenanza público"

**************
***tecnica_ci*
**************

gen tecnica_ci=(education==8)
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

* Daniela Zuluaga-Enero 2018: Se generan variables de vivienda que no habían sido creadas pero cuya información está disponible en la base.

****************
***aguared_ch***
****************

gen aguared_ch=.
replace aguared_ch=1 if water_supply==1
replace aguared_ch=0 if (water_supply>=2 & water_supply<=4)
label var aguared_ch "Acceso a fuente de agua por red"


****************
***aguadist_ch***
****************


gen aguadist_ch=.
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************

gen aguamala_ch=.
replace aguamala_ch=1 if water_supply==4
replace aguamala_ch=0 if (water_supply>=1 & water_supply<=3)
label var aguamala_ch "Agua unimproved según MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch= .
replace luz_ch=1 if lighting==1
replace luz_ch=0 if (lighting==2 | lighting==3 | lighting==4)
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch= .
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

gen bano_ch= .
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=.
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

gen piso_ch=.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales"
label val piso_ch piso_ch


**************
***pared_ch***
**************

gen pared_ch=.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val pared_ch pared_ch


**************
***techo_ch***
**************

gen techo_ch=.
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val techo_ch techo_ch


**************
***resid_ch***
**************


gen resid_ch =.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


*************
***dorm_ch***
*************

gen dorm_ch=.
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

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
replace telef_ch=1 if fixed_telephone==1
replace telef_ch=0 if fixed_telephone==2
label var telef_ch "El hogar tiene servicio telefónico fijo"


**************
***compu_ch***
**************

gen compu_ch=.
replace compu_ch=1 if computer==1
replace compu_ch=0 if computer==2

********
***NA***
********
gen refrig_ch=.
label var refrig_ch "El hogar posee refrigerador o heladera"

gen freez_ch=.
label var freez_ch "El hogar posee congelador"

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"



*****************
***internet_ch***
*****************
gen internet_ch=.
replace internet_ch=1 if internet==1
replace internet_ch=0 if internet==2
label var internet_ch "El hogar posee conexión a Internet"



************
***cel_ch***
************

gen cel_ch= .
replace cel_ch=1 if cellular>=2 & cellular<=4
replace cel_ch=0 if cellular==1
label var cel_ch "El hogar tiene servicio telefonico celular"


**************
***vivi1_ch***
**************

gen vivi1_ch=.
replace vivi1_ch=1 if (dewelling==1 | dewelling==2)
replace vivi1_ch=2 if dewelling==3
replace vivi1_ch=3 if (dewelling==4 | dewelling==5)
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

gen vivi2_ch= (vivi1_ch==1 | vivi1_ch==2)
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************
gen viviprop_ch=.
replace viviprop_ch=0 if tenure==2
replace viviprop_ch=1 if tenure==1
replace viviprop_ch=3 if tenure==3

label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia" 
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

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

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************

gen aguamejorada_ch=.
replace aguamejorada_ch=1 if water_supply>=1 & water_supply<=3
replace aguamejorada_ch=0 if water_supply==4
	
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch=.

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

rename education education_1


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(inlist(where_born,2)) if where_born!=. & where_born!=9		/* Categoria Not Stated no se incluye en la variable*/
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
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
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	
	

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first



compress


saveold "`base_out'", replace


log close

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

local PAIS SLV
local ENCUESTA EHPM
local ANO "1995"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: El Salvador
Encuesta: EHPM
Round: a
Autores: 2006- Analia
Generacion de nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
Última versión: Maria Laura Oliveri - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Fecha última modificación: 23 de Octubre de 2013

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

clear all
set more off
use "`base_in'", clear

/*foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }*/

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

***************
***factor_ch***
***************

gen factor_ch=factorex
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

gen idh_ch= id_hogar
label variable idh_ch "ID del hogar"

************
***idp_ci***
************

gen idp_ci=nrorden
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if area==2
replace zona_c=1 if area==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="SLV"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1995
label variable anio_c "Anio de la encuesta"


*********
***mes***
*********
gen byte mes_c=.
/*No disponible en la encuesta*/

label variable mes_c "Mes de la encuesta"


*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=10
replace relacion_ci=5 if parentco==12 | parentco==13
replace relacion_ci=6 if parentco==11

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

gen factor_ci=factorex
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=sexo

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estcivil==1
replace civil_ci=2 if estcivil==2
replace civil_ci=3 if estcivil==4 
replace civil_ci=4 if estcivil==3
replace civil_ci=. if edad_ci<12 

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Casado"
label define civil_ci 3 "Divorciado" 4 "Viudo" , add
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
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

*2014, 01 modificado segun documento metodologico by MLO
g miembros_ci=(relacion_ci<5)
*g miembros_ci=(relacion_ci<6)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .

gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"
notes raza_ci: En el cuestionario no consta una pregunta relacionada con raza.

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact>=2 & condact<=3
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=10
recode condocup_ci .=4 if edad_ci<10 
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

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
/*
************
***emp_ci***
************

gen byte emp_ci=0
replace emp_ci=1 if trabajo==1
replace emp_ci=1 if trabajo==2 & tienetra==1 

****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 & busco==1)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 & busco==1 | (busco==2 & (pqnobus==1 |pqnobus==2 | pqnobus==4)))

****************
***desemp3_ci***
****************
gen desemp3_ci=.

*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1


*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1

*************
***pea3_ci***
*************
gen pea3_ci=.*/


*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & busco==2 & (pqnobus==5))

***************
***subemp_ci***
***************

gen subemp_ci=.
**no se pregunta si se quiere trabajar más horas

*****************
***horaspri_ci***
*****************

gen horaspri_ci=horastra if trabajo==1
replace horaspri_ci=hrshab if trabajo==2 & tienetra==1

replace horaspri_ci=. if horastra==99
replace horaspri_ci=. if hrshab==99
replace horaspri_ci=. if emp==0

*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci hrssec)
replace horastot_ci=. if horaspri_ci==. & hrssec==.
replace horastot_ci=horaspri_ci if hrssec==99
replace horastot_ci=. if emp_ci==0

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categ==1
replace categopri_ci=2 if categ==2 | categ==4 
replace categopri_ci=3 if  categ==5 |categ==6 | categ==7 |categ==8  
replace categopri_ci=4 if categ==3
replace categopri_ci=0 if categ==9

label define categopri_ci 0"Otro" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

/*
*****************
***contrato_ci***
*****************

gen contrato_ci=.


***************
***segsoc_ci***
***************

gen segsoc_ci=.
replace segsoc_ci=1 if segsoci==1 
replace segsoc_ci=0 if segsoci==2*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & otrotrab==1
replace nempleos_ci=. if pea_ci==0
/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if tamest==1 | tamest==2
replace firmapeq_ci=0 if tamest>=3 & tamest<=8
replace firmapeq_ci=. if emp==0

/*En este caso no se le pregunta a los que trabajan como servicio domestico.
Y estos son missings */
*/

*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if sectorp==2
replace spublico_ci=0 if sectorp==1
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | categ==8 | categ==4
/*Sólo se le hace esta pregunta a los asalariados, aprendices y otros*/

**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (ocup>=211 & ocup<=369) & emp_ci==1
replace ocupa_ci=2 if (ocup>=111 & ocup<=131) & emp_ci==1
replace ocupa_ci=3 if (ocup>=411 & ocup<=422) & emp_ci==1
replace ocupa_ci=4 if ((ocup>=520 & ocup<=526) | ocup==911) & emp_ci==1
replace ocupa_ci=5 if ((ocup>=511 & ocup<=516) | (ocup>=912 & ocup<=916)) & emp_ci==1
replace ocupa_ci=6 if ((ocup>=611 & ocup<=621) | (ocup>=921 & ocup<=922)) & emp_ci==1
replace ocupa_ci=7 if ((ocup>=711 & ocup<=834) | (ocup>=931 & ocup<=933)) & emp_ci==1
replace ocupa_ci=8 if ocup==11 & emp_ci==1
replace ocupa_ci=. if emp_ci==0 | ocup==999 | ocup==0


*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (rama>=11 & rama<=50) & emp_ci==1
replace rama_ci=2 if (rama>=101 & rama<=142) & emp_ci==1
replace rama_ci=3 if (rama>=151 & rama<=372) & emp_ci==1
replace rama_ci=4 if (rama>=401 & rama<=410) & emp_ci==1
replace rama_ci=5 if (rama>=451 & rama<=455) & emp_ci==1
replace rama_ci=6 if (rama>=501 & rama<=552) & emp_ci==1 
replace rama_ci=7 if (rama>=601 & rama<=642) & emp_ci==1
replace rama_ci=8 if (rama>=651 & rama<=749) & emp_ci==1
replace rama_ci=9 if (rama>=751 & rama<=990) & emp_ci==1


*****************
***ocupault_ci***
*****************

gen ocupault_ci=.
replace ocupault_ci=1 if (ocupant>=211 & ocupant<=348) & busco==1 & trabante==1
replace ocupault_ci=2 if (ocupant>=111 & ocupant<=131) & busco==1 & trabante==1
replace ocupault_ci=3 if (ocupant>=411 & ocupant<=422) & busco==1 & trabante==1
replace ocupault_ci=4 if (ocupant>=521 & ocupant<=523) & busco==1 & trabante==1
replace ocupault_ci=5 if (ocupant>=511 & ocupant<=516) & busco==1 & trabante==1
replace ocupault_ci=6 if (ocupant>=611 & ocupant<=621) & busco==1 & trabante==1
replace ocupault_ci=7 if (ocupant>=711 & ocupant<=834) & busco==1 & trabante==1
replace ocupault_ci=8 if (ocupant==11) & busco==1 & trabante==1
replace ocupault_ci=9 if (ocupant>=911 & ocupant<=933) & busco==1 & trabante==1

****************
***ramault_ci***
****************

gen ramault_ci=.
replace ramault_ci=1 if (ramaant>=11 & ramaant<=50) & busco==1 & trabante==1
replace ramault_ci=2 if (ramaant>=101 & ramaant<=142) & busco==1 & trabante==1
replace ramault_ci=3 if (ramaant>=151 & ramaant<=372) & busco==1 & trabante==1
replace ramault_ci=4 if (ramaant>=401 & ramaant<=410) & busco==1 & trabante==1
replace ramault_ci=5 if (ramaant>=451 & ramaant<=455) & busco==1 & trabante==1
replace ramault_ci=6 if (ramaant>=501 & ramaant<=552) & busco==1 & trabante==1 
replace ramault_ci=7 if (ramaant>=601 & ramaant<=642) & busco==1 & trabante==1
replace ramault_ci=8 if (ramaant>=651 & ramaant<=749) & busco==1 & trabante==1
replace ramault_ci=9 if (ramaant>=751 & ramaant<=990) & busco==1 & trabante==1

******************
***ocupault2_ci***
******************

gen ocupault2_ci=.
replace ocupault2_ci=1 if (ocupant>=211 & ocupant<=348) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=2 if (ocupant>=111 & ocupant<=131) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=3 if (ocupant>=411 & ocupant<=422) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=4 if (ocupant>=521 & ocupant<=523) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=5 if (ocupant>=511 & ocupant<=516) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=6 if (ocupant>=611 & ocupant<=621) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=7 if (ocupant>=711 & ocupant<=834) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=8 if (ocupant==11) & busco==1 & trabante==1 & desemp_ci==1
replace ocupault2_ci=9 if (ocupant>=911 & ocupant<=933) & busco==1 & trabante==1 & desemp_ci==1

*****************
***ramault2_ci***
*****************

gen ramault2_ci=.
replace ramault2_ci=1 if (ramaant>=11 & ramaant<=50) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=2 if (ramaant>=101 & ramaant<=142) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=3 if (ramaant>=151 & ramaant<=372) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=4 if (ramaant>=401 & ramaant<=410) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=5 if (ramaant>=451 & ramaant<=455) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=6 if (ramaant>=501 & ramaant<=552) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=7 if (ramaant>=601 & ramaant<=642) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=8 if (ramaant>=651 & ramaant<=749) & busco==1 & trabante==1 & desemp_ci==1
replace ramault2_ci=9 if (ramaant>=751 & ramaant<=990) & busco==1 & trabante==1 & desemp_ci==1

****************
***durades_ci***
****************

gen durades_ci=.
/*No se puede crear una variable en meses*/

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.



*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión Colones a dólares

/*
Fuente: http://www.iadb.org/en/research-and-data/latin-american-and-caribbean-macro-watch,8633.html

1995-Jan	8.76
1995-Feb	8.76
1995-Mar	8.75
1995-Apr	8.75
1995-May	8.76
1995-Jun	8.76
1995-Jul	8.76
1995-Aug	8.76
1995-Sep	8.76
1995-Oct	8.76
1995-Nov	8.76
1995-Dec	8.76

*/

***************
***ylmpri_ci***
***************

gen ylmpri_ci=.
replace ylmpri_ci=ingresos
replace ylmpri_ci=. if ingresos==999999
replace ylmpri_ci=. if emp==0

*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace ylmpri_ci= ylmpri_ci/8.76     
gen ylmpri1_ci=. /*NA, esta variable aparece recién en 1998*/

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=. /*NA, esta variable aparece recién en 1998*/

*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

gen ylnmpri_ci=. /*NA, esta variable aparece recién en 1998*/

gen ylnmpri1_ci=. /*NA, esta variable aparece recién en 1998*/

***************
***ylmsec_ci***
***************

gen ylmsec_ci=ysec 
replace ylmsec_ci=. if emp==0 | otrotrab==2
replace ylmsec_ci=. if ysec==999999

*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace ylmsec_ci= ylmsec_ci/8.76    
gen ylmsec1_ci=.

****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=. /*NA, esta variable aparece recién en 1998*/

**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

gen ylm1_ci=. /*NA, esta variable aparece recién en 1998*/


************************
***ylnm_ci & ylnm1_ci***
************************

gen ylnm_ci=. /*NA, esta variable aparece recién en 1998*/

gen ylnm1_ci=. /*NA, esta variable aparece recién en 1998*/


**************************
***ynlm0_ci & ynlnm0_ci***
**************************
/*Si bien en el cuestionario existe la pregunta para crear esta variable, no se suministra 
la infomación en la base de datos del organismo de estadística de El Salvador*/

gen ynlm0_ci=.

gen ynlnm0_ci=.

*Modificación Mayra Sáenz - Septiembre 2014
*gen ynlm_ci= . /*NA, esta variable aparece recién en 1998*/

egen ingrem = rsum(consumo vestuar cuota repara comer ahorros medicos educac insumos otros), missing
replace ingrem = ingrem/8.76
gen ynlm_ci=ingrem


gen ynlnm_ci=.

****************
***remesas_ci***
****************
*gen remesas_ci=. /*NA, esta variable aparece recién en 1998*/
*Modificación Mayra Sáenz - Septiembre 2014
*No se puede identificar si las remesas son mensuales
g remesas_ci= ingrem

************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

**********************************
*** nrylmpri_ch & nrylmpri1_ch ***
**********************************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

gen nrylmpri1_ch=. /*NA, esta variable aparece recién en 1998*/
 

************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

gen ylm1_ch=. /*NA, esta variable aparece recién en 1998*/

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

gen ylmnr1_ch=. /*NA, esta variable aparece recién en 1998*/


**************************
*** ylnm_ch & ylnm1_ch ***
**************************

gen ylnm_ch=. /*NA, esta variable aparece recién en 1998*/
gen ylnm1_ch=. /*NA, esta variable aparece recién en 1998*/

*******************
*** remesas_ch  ***
*******************

*gen remesas_ch=. /*NA, esta variable aparece recién en 1997*/
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1

***************
*** ynlm_ch ***
***************

gen ynlm0_ch=. /*NA, esta variable aparece en 1997*/
*gen ynlm_ch=.
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

****************
*** ynlnm_ch ***
****************

gen ynlnm0_ch=. /*NA, esta variable aparece en 1997*/
gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen autocons_ci=. /*NA, esta variable aparece recién en 1999*/

*******************
*** autocons_ch ***
*******************

gen autocons_ch=. /*NA, esta variable aparece recién en 1999*/

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=alquiler
replace rentaimp_ch=. if alquiler==99999
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace rentaimp_ch= rentaimp_ch/8.76

******************************
***ylhopri_ci & ylhopri1_ci***
******************************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

gen ylmhopri1_ci=. /*NA, esta variable aparece recién en 1998*/

**************************
***ylmho_ci & ylm1ho_ci***
**************************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen ylmho1_ci=. /*NA, esta variable aparece recién en 1998*/




**************************
***EDUCATION INDICATORS***
**************************

/* Las variables NIVEDU y NIVELCURSA nos permiten identificar los años de educación
para aquellos individuos que actualmente estan estudiando. 
Las variables ULTGRADO y ULTNIVEL indican el último nivel alcanzado y el año 
alcanzado en dicho nivel, permiten calcular los años de educación para aquellos que
actualmente no asisten a un establecimiento escolar.
En El Salvador, la educación básica dura nueve años y la educación media tres años*/

gen byte aedu_ci=.

/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/

replace aedu_ci=0 if nivedu==0 & estudia==1
replace aedu_ci=0 if nivedu==11 |  nivedu==12 | nivedu==13

replace aedu_ci=1 if  nivedu==21
replace aedu_ci=2 if  nivedu==22
replace aedu_ci=3 if  nivedu==23
replace aedu_ci=4 if  nivedu==24
replace aedu_ci=5 if  nivedu==25
replace aedu_ci=6 if  nivedu==26
replace aedu_ci=7 if  nivedu==27
replace aedu_ci=8 if  nivedu==28
replace aedu_ci=9 if  nivedu==29
replace aedu_ci=10 if  nivedu==41
replace aedu_ci=11 if  nivedu==42
replace aedu_ci=12 if  nivedu==43
replace aedu_ci=12 if  nivedu==44
replace aedu_ci=13 if  nivedu==51 | nivedu==61 
replace aedu_ci=14 if  nivedu==52 | nivedu==62
replace aedu_ci=15 if  nivedu==53 | nivedu==63
replace aedu_ci=16 if  nivedu==54 | nivedu==64
replace aedu_ci=17 if  nivedu==65
replace aedu_ci=18 if  nivedu==66
replace aedu_ci=19 if  nivedu==67
replace aedu_ci=20 if  nivedu==68
replace aedu_ci=21 if  nivedu==69
replace aedu_ci=22 if  nivedu==70

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0

/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if ultgrado==0 & (hasist==1 | hasist==2)
replace aedu_ci=0 if ultgrado==12 | ultgrado==13

replace aedu_ci=1 if  ultgrado==21
replace aedu_ci=2 if  ultgrado==22
replace aedu_ci=3 if  ultgrado==23
replace aedu_ci=4 if  ultgrado==24
replace aedu_ci=5 if  ultgrado==25
replace aedu_ci=6 if  ultgrado==26
replace aedu_ci=7 if  ultgrado==27
replace aedu_ci=8 if  ultgrado==28
replace aedu_ci=9 if  ultgrado==29
replace aedu_ci=10 if  ultgrado==41
replace aedu_ci=11 if  ultgrado==42
replace aedu_ci=12 if  ultgrado==43
replace aedu_ci=12 if  ultgrado==44
replace aedu_ci=13 if  ultgrado==51 | ultgrado==61 
replace aedu_ci=14 if  ultgrado==52 | ultgrado==62
replace aedu_ci=15 if  ultgrado==53 | ultgrado==63
replace aedu_ci=16 if  ultgrado==54 | ultgrado==64
replace aedu_ci=17 if  ultgrado==65
replace aedu_ci=18 if  ultgrado==66
replace aedu_ci=19 if  ultgrado==67
replace aedu_ci=20 if  ultgrado==68
replace aedu_ci=21 if  ultgrado==69
replace aedu_ci=22 if  ultgrado==70

replace aedu_ci=0 if estudia==2 & hasist==2
replace aedu_ci=. if aedu_ci>edad & aedu_ci~=. 

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
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la eecundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"
***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=0
replace edupre_ci=1 if ultgrado==11 | ultgrado==12 | ultgrado==13 | nivedu==11 | nivedu==12 | nivedu==13 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (nivedu>=61 & nivedu<=69) | (ultgrado>=61 & ultgrado<=69) & aedu_ci~=.
replace eduac_ci=0 if (nivedu>=51 & nivedu<=54) | (ultgrado>=51 & ultgrado<=54) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if estudia==1
label variable asiste_ci "Asiste actualmente a la escuela"

****************
***asispre_ci***
****************
*Agregada por Iván Bornacelly - 01/23/2017
	g asispre_ci=.
	replace asispre_ci=1 if asiste_ci==1 & (nivedu==11 | nivedu==12 | nivedu==13)
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"	

*****************
***pqnoasis_ci***
*****************

rename pqnoasis pqnoasis_orig

gen pqnoasis_ci=pqnoestu
replace pqnoasis_ci=. if estudia~=2

label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 
label define pqnoasis_ci 2 "Muy caro" 3 "Muy lejos" 4 "No hay profesor", add 
label define pqnoasis_ci 5 "Cerro el centro" 6 "Repitio mucho" , add
label define pqnoasis_ci 7 "No vale la pena" 8 "Por la edad" 9 " No hay escuela nocturna", add
label define pqnoasis_ci 10 " Finalizo sus estudios"  11 " Causas del hogar" 12 " No existe otro grado" 13 "Otros" , add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if pqnoestu ==2
replace pqnoasis1_ci = 2 if pqnoestu ==1
replace pqnoasis1_ci = 4 if pqnoestu ==7
replace pqnoasis1_ci = 5 if pqnoestu ==11
replace pqnoasis1_ci = 6 if pqnoestu ==10
replace pqnoasis1_ci = 7 if pqnoestu ==8
replace pqnoasis1_ci = 8 if pqnoestu ==3  | pqnoestu ==4  | pqnoestu ==5  | pqnoestu ==9 | pqnoestu ==12 
replace pqnoasis1_ci = 9 if pqnoestu ==6  | pqnoestu ==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


***************
***repite_ci***
***************
gen repite_ci=.

/*NA*/

******************
***repiteult_ci***
******************

gen repiteult_ci=(repite==1)
replace repiteult_ci=. if asiste_ci==0
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if centroen==1 
replace edupub_ci=2 if centroen==2 | centroen==3

/* Variable centroen:
1: Centro de enseñanza oficial: 
Es aquel cuya administración y funcionamiento depende del gobierno.
2: Centro de Enseñanza Laico: 
Son todos los centros educativos privados no religiosos. 
3: Centro de Enseñanza religioso: 
Son todos los centros educativos que pertenecen a una Congregación Religiosa. 
*/

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(servagua==1 | servagua==2)

gen aguadist_ch=1 if servagua==1
replace aguadist_ch=2 if servagua==2
replace aguadist_ch=3 if abasagua>=1 & abasagua<=8

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(servalum==1 | servalum==2)

gen luzmide_ch=.
/*NA*/

gen combust_ch=(servalum>=1 & servalum<=4)

gen bano_ch=((servsani>=1 & servsani<=4) | (notiene>=1 & notiene<=3))

gen banoex_ch=(notiene>=1 & notiene<=3)

* MGR Jul, 2015: variable había sido generada como missing, construyo variable de la misma manera que los otros años
gen des1_ch=.
replace des1_ch = 0 if servsani==5
replace des1_ch = 1 if servsani>=1 & servsani<=2
replace des1_ch = 2 if servsani>=3 & servsani<=4

* MGR Jul, 2015: variable había sido generada como missing, construyo variable de la misma manera que los otros años
gen des2_ch=.
replace des2_ch = 0 if servsani==5
replace des2_ch = 1 if des1_ch==1 | des1_ch==2

gen piso_ch=0 if piso==5
replace piso_ch=1 if piso>=1 & piso<=4

gen pared_ch=0 if paredes==2 | paredes==3 | paredes==6 | paredes==7
replace pared_ch=1 if paredes==1 | paredes==4 | paredes==5

gen techo_ch=0 if techo==5 | techo==6
replace techo_ch=1 if techo>=1 & techo<=4

gen resid_ch=0 if basura==1 | basura==2
replace resid_ch=1 if basura==4 | basura==5
replace resid_ch=2 if basura==6
replace resid_ch=3 if basura==3 | basura==7


**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (servagua >=1 & servagua <=2) | abasagua == 1 | abasagua == 4 | abasagua == 8
replace aguamejorada_ch = 0 if (servagua >=3 & servagua <=4) | abasagua == 2 | abasagua == 3 | (abasagua >=5 & abasagua <=7) | abasagua == 9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (servsani >=1 & servsani <=4)
replace banomejorado_ch = 0 if  servbano ==3 | servsani ==5
		
gen dorm_ch=nrodorm

gen cuartos_ch=nrohabit

gen cocina_ch=.
/*NA*/

gen telef_ch=(telefono==1)
replace telef_ch=. if telefono==9

gen refrig_ch=(refri==1)
replace refrig_ch=. if refri==9

gen freez_ch=.
/*NA*/

gen auto_ch=(vehiculo==1)
replace auto_ch=. if vehiculo==9


gen compu_ch=(computad==1)
replace compu_ch=. if computad==9

gen internet_ch=.
/*NA*/

gen cel_ch=.
/*NA*/

gen vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv>=2 & tipoviv<=4
replace vivi1_ch=3 if tipoviv>4 & tipoviv<9

gen vivi2_ch=(tipoviv>=1 & tipoviv<=4)
replace vivi2_ch=. if tipoviv==9

gen viviprop_ch=0 if tenencia==3
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=2 if tenencia==2
replace viviprop_ch=3 if tenencia>=4 & tenencia<=6


gen viviitit_ch=.
/*NA*/

gen vivialq_ch=cuomes if tenencia==3
replace vivialq_ch=. if cuomes==99999
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace vivialq_ch= vivialq_ch/8.76

gen vivialqimp_ch=alquiler
replace vivialqimp_ch=. if alquiler==99999
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76

************************
*** El SALVADOR 1995 ***
************************

/* 
Relación de parentesco
 1. Jefe
 2. Cónyuge
 3. Hijo
 4. Padre o madre
 5. Hermano(a)
 6. Yerno o nuera
 7. Nieto(a)
 8. Suegro(a)
 9. Cuñado(a)
 10. Otros familiares
 11. Empleado doméstico
 12. Pupilos
 13. Otros
*/

 count
 tab parentco
 
 gen	 incl=1 if (parentco>=1 & parentco<=13)
 replace incl=0 if (parentco==11)

 rename factorex factor

** AREA

 tab area [w=factor]

** Gender classification of the population refering to the head of the household.

 sort id_hogar nrorden

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 
 tab sexo   [w=factor]
 tab sexo_d [w=factor]

 tab sexo sexo_d if parentco==1

 sort id_hogar nrorden

** Years of Education
/*
8. ¿Sabe leer y escribir?

9. ¿Estudia actualmente?
1. Si
2. No ==> 18 

10. ¿Qué grado o curso estudia actualmente?

11: Kinder					41: Bachillerato, primer año
12: Preparatoria				42: Bachillerato, segundo año
13: Preparatoria 2				43: Bachillerato, tercer año
21: Educación básica, primer grado		44: Bachillerato, cuarto año
22: Educación básica, segundo grado		51: Educación superior no universitaria, primer año
23: Educación básica, tercer grado		52: Educación superior no universitaria, segundo año
24: Educación básica, cuarto grado		53: Educación superior no universitaria, tercer año
25: Educación básica, quinto grado		61: Educación universitaria, primer año
26: Educación básica, sexto grado		62: Educación universitaria, segundo año
27: Educación básica, séptimo grado		63: Educación universitaria, tercer año
28: Educación básica, octavo grado		64: Educación universitaria, cuarto año
29: Educación básica, noveno grado		65: Educación universitaria, quinto año
31: Educación de adultos, primer grado		66: Educación universitaria, sexto año
32: Educación de adultos, segundo grado		67: Educación universitaria, séptimo año
33: Educación de adultos, tercer grado		69: Graduados
34: Educación de adultos, cuarto grado		70: Post grado, maestría
37: Educación de adultos, séptimo grado		80: Educación especial
39: Educación de adultos noveno grado


18. ¿Cuál fue su último grado aprobado y título obtenido?

0: Ningún grado aprobado			41: Bachillerato, primer año
11: Kinder					42: Bachillerato, segundo año
13: Preparatoria 2				43: Bachillerato, tercer año
21: Educación básica, primer grado		44: Bachillerato, cuarto año
22: Educación básica, segundo grado		51: Educación superior no universitaria, primer año
23: Educación básica, tercer grado		52: Educación superior no universitaria, segundo año
24: Educación básica, cuarto grado		53: Educación superior no universitaria, tercer año
25: Educación básica, quinto grado		54: Educación superior no universitaria, cuarto año
26: Educación básica, sexto grado		61: Educación universitaria, primer año
27: Educación básica, séptimo grado		62: Educación universitaria, segundo año
28: Educación básica, octavo grado		63: Educación universitaria, tercer año
29: Educación básica, noveno grado		64: Educación universitaria, cuarto año
33: Educación de adultos, tercer grado		65: Educación universitaria, quinto año
						66: Educación universitaria, sexto año
						67: Educación universitaria, séptimo año
						68: Educación universitaria, octavo año
						69: Graduados
						70: Post grado, maestría

21. ¿Ha asistido alguna vez a la escuela?
1. Si
2. No

*/

 rename anoest anoest_orig

 gen anoest=.
 replace anoest=0  if ultgrad==11 | ultgrad==13 | ultgrad==0 | (nivedu==0) | nivedu==11 | nivedu==12 | nivedu==13 | nivedu==21 | nivedu==31 | hasist==2
 replace anoest=1  if (ultgrad==21) | (ultgrad==31) | (nivedu==22) | (nivedu==32)
 replace anoest=2  if (ultgrad==22) | (ultgrad==32) | (nivedu==23) | (nivedu==33)
 replace anoest=3  if (ultgrad==23) | (ultgrad==33) | (nivedu==24) | (nivedu==34)
 replace anoest=4  if (ultgrad==24) | (ultgrad==34) | (nivedu==25) | (nivedu==35)
 replace anoest=5  if (ultgrad==25) | (ultgrad==35) | (nivedu==26) | (nivedu==36)
 replace anoest=6  if (ultgrad==26) | (ultgrad==36) | (nivedu==27) | (nivedu==37)
 replace anoest=7  if (ultgrad==27) | (ultgrad==37) | (nivedu==28) | (nivedu==38)
 replace anoest=8  if (ultgrad==28) | (ultgrad==38) | (nivedu==29) | (nivedu==39)
 replace anoest=9  if (ultgrad==29) | (ultgrad==39) | (nivedu==41)
 replace anoest=10 if (ultgrad==41) | (nivedu==42)
 replace anoest=11 if (ultgrad==42) | (nivedu==43)
 replace anoest=12 if (ultgrad==43) | (nivedu==44)  | (nivedu==51) | (nivedu==61)
 replace anoest=13 if (ultgrad==44) | (ultgrad==51) | (ultgrad==61)| (nivedu==52) | (nivedu==62)
 replace anoest=14 if (ultgrad==52) | (ultgrad==62) | (nivedu==53) | (nivedu==63)
 replace anoest=15 if (ultgrad==53) | (ultgrad==63) | (nivedu==64)
 replace anoest=16 if (ultgrad==54) | (ultgrad==64) | (nivedu==65)
 replace anoest=17 if (ultgrad==65) | (nivedu==66)
 replace anoest=18 if (ultgrad==66) | (nivedu==67)
 replace anoest=19 if (ultgrad==67) | (ultgrad==68) | (ultgrad==69) | (nivedu==69)
 replace anoest=20 if (ultgrad==70) | (nivedu==70)
 replace anoest=99 if (ultgrad==80) | (nivedu==80)

** Economic Active Population 
/*
actpr
Actividad principal
0: Ocupados
1: Desocupados
2: Inactivos
*/

 gen	 peaa=0
 replace peaa=1 if actpr==0
 replace peaa=2 if actpr==1
 replace peaa=3 if actpr==2

 gen	 tasadeso=0 if peaa==1
 replace tasadeso=1 if peaa==2

 tab peaa [w=factor]
 tab peaa [w=factor] if incl==1

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*
8. ¿Sabe leer y escribir?

9. ¿Estudia actualmente?
1. Si
2. No ==> 18 

10. ¿Qué grado o curso estudia actualmente?

11: Kinder					41: Bachillerato, primer año
12: Preparatoria				42: Bachillerato, segundo año
13: Preparatoria 2				43: Bachillerato, tercer año
21: Educación básica, primer grado		44: Bachillerato, cuarto año
22: Educación básica, segundo grado		51: Educación superior no universitaria, primer año
23: Educación básica, tercer grado		52: Educación superior no universitaria, segundo año
24: Educación básica, cuarto grado		53: Educación superior no universitaria, tercer año
25: Educación básica, quinto grado		61: Educación universitaria, primer año
26: Educación básica, sexto grado		62: Educación universitaria, segundo año
27: Educación básica, séptimo grado		63: Educación universitaria, tercer año
28: Educación básica, octavo grado		64: Educación universitaria, cuarto año
29: Educación básica, noveno grado		65: Educación universitaria, quinto año
31: Educación de adultos, primer grado		66: Educación universitaria, sexto año
32: Educación de adultos, segundo grado		67: Educación universitaria, séptimo año
33: Educación de adultos, tercer grado		69: Graduados
34: Educación de adultos, cuarto grado		70: Post grado, maestría
37: Educación de adultos, séptimo grado		80: Educación especial
39: Educación de adultos noveno grado
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION
* ISCED 1

 gen     NERP=0 if (edad>=7 & edad<=12) & (estudia==1 | estudia==2)
 replace NERP=1 if (edad>=7 & edad<=12) & (estudia==1 & (nivedu>=21 & nivedu<=26))

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (estudia==1 | estudia==2)
 replace NERS=1 if (edad>=13 & edad<=18) & (estudia==1) & ((nivedu>=27 & nivedu<=29) | (nivedu>=41 & nivedu<=43))

** Upper secondary
* Educación Media

 gen     NERS2=0 if (edad>=16 & edad<=18) & (estudia==1 | estudia==2)
 replace NERS2=1 if (edad>=16 & edad<=18) & (estudia==1 & (nivedu>=41 & nivedu<=43))

** Target 3, Indicator: Literacy Rate of 15-24 Years Old

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old*
* Read & write

 gen     LIT2=0 if (edad>=15 & edad<=24) & (alfabet>=1 & alfabet<=2)
 replace LIT2=1 if (edad>=15 & edad<=24) & (alfabet==1)
	
*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if (estudia==1 & (nivedu>=21 & nivedu<=26))
 gen sec=1 if  (estudia==1 & ((nivedu>=27 & nivedu<=29) | (nivedu>=41 & nivedu<=43)))
 gen ter=1 if  (estudia==1 & ((nivedu>=61 & nivedu<=65) | (nivedu>=51 & nivedu<=53)))

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.
 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.
 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.
 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  


** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.
 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.
 gen     RATIOLIT2=0 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.
 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

* Without Domestic Service
/*
categ						ramar
16. ¿TRABAJA USTED EN CALIDAD DE?		RAMA DE ACTIVIDAD ECONÓMICA DE 
 0: No aplicable				LOS OCUPADOS A UN DÍGITO
 1: Patrono
 2: Cuenta propia
 3: Familiar no remunerado
 4: Cooperativista
 5: Asalariado permanente
 6: Asalariado temporal
 7: Aprendiz
 8: Servicio doméstico
 9: Otros
*/

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ==5 | categ==6) & (ramar>=2 & ramar<=16)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==5 | categ==6) & (ramar>=2 & ramar<=16) & sexo==2
	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categ==5 | categ==6 | categ==8) & (ramar>=2 & ramar<=16)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==5 | categ==6 | categ==8) & (ramar>=2 & ramar<=16) & sexo==2

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator
/*
5a 
 1: Electricidad
 2: Planta eléctrica
 3: Kerosene (gas)
 4: Gas propano
 5: Candela
 6: Otra clase
*/

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (servalum>=1 & servalum<=6) /* Total population excluding missing information */
 replace ELEC=1 if (servalum>=1 & servalum<=2)
 	
** Target 10, Indicator: Proportion of the population using solidfuels (%)
/*
5h. ¿COCINAN EN ESTE HOGAR? 
1: Si
	COMBUSTI 
	¿QUÉ COMBUSTIBLE UTILIZAN PARA COCINAR? 
	0: No aplicable
	1: Electricidad
	2: Kerosene (gas)
	3: Gas propano
	4: Leña
	5: Carbón
2. No
*/

* Gender classification of the population refers to the head of the household.

 gen	 SFUELS=0 if (combusti>=0 & combusti<=5)  /* Total population excluding missing information */
 replace SFUELS=1 if (combusti==4 | combusti==5)

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
SERVAGUA 
5b. ¿TIENE ESTA VIVIENDA INSTALACIONES DE SERVICIO DE AGUA POTABLE? 
1: Si, dentro de la vivienda	==>  ¿DE CUÁNTO FUE EL 
2: Si, fuera de la vivienda 	==>   GASTO EN SERVICIO DE 
   pero dentro de la propiedad	==>   AGUA EN EL ÚLTIMO 	
3: Tiene pero no funciona	==>   MES?
4: No tiene			==> 5c			
   
ABASAGUA 
5c. SI NO CUENTA CON INSTALACIONES DE SERVICIO DE AGUA POTABLE, ¿CÓMO SE ABASTECEN DE ELLA?
0: No aplicable
1: Por pila pública
2: Chorro común
3: Por camión, carreta o pipa
4: Por pozo (privado o común)
5: Por río o quebrada
6: Por ojo de agua
7: Se la regalan
8: Por lluvia
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (servagua>=1 & servagua<=4)  /* Total population excluding missing information */
 replace WATER=1 if (servagua>=1 & servagua<=3) | (abasagua==1 | abasagua==2 | abasagua==4 | abasagua==8)
 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*

SERVSANI 
5f. ¿TIENE ESTA VIVIENDA INSTALACIONES DE SERVICIO SANITARIO? 
 1: Si, inodoro a alcantarillado	==> 5h
 2: Si, inodoro a fosa séptica		==> 5h
 3: Si, letrina				==> 5h
 4: Si, letrina abonera			==> 5h
 5: No posee				==> 5g
 
NOTIENE 
5g SI NO TIENE LA VIVIENDA INSTALACIONES DE SERVICIO SANITARIO, ¿CÓMO SATISFACE ESTA NECESIDAD? 
 0: No aplicable
 1: Inodoro común a alcantarillado
 2: Inodoro común a fosa séptica
 3: Letrina común
 4: Otros
*/

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (servsani>=1 & servsani<=5) /* Total population excluding missing information */
 replace SANITATION=1 if (servsani>=1 & servsani<=2) | (notiene>=1 & notiene<=2)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
/*
tipoviv
1. TIPO DE VIVIENDA 
 1: Casa privada
 2: Apartamento
 3: Apartamento en edificio de apartamentos
 4: Apartamento en una casa
 5: Pieza en una casa
 6: Pieza en un mesón
 7: Vivienda improvisada
 8: Rancho

tenencia
4a. TENENCIA DE LA VIVIENDA 
 1: Propietario
 2: Propietario de la vivienda pero la está pagando a plazos
 3: Inquilino
 4: Colono
 5: Guardián de la vivienda
 6: Ocupante gratuito

paredes
2b. PAREDES 
 1: Concreto o mixto
 2: Bahareque
 3: Adobe
 4: Madera
 5: Lámina
 6: Paja o palma
 7: Materiales de desechos

piso 
2c. PISO 
 1: Ladrillo de cemento
 2: Ladrillo de barro
 3: Cemento
 4: Madera
 5: Tierra

nrohabit 
3.1. ¿CUÁNTAS HABITACIONES SON DE USO EXCLUSIVO DEL HOGAR SIN CONTAR BAÑOS, COCINA NI
PASILLO? 

nroneg 
3.3 ¿CUÁNTOS UTILIZAN PARA TRABAJAR O PARA NEGOCIO? 
0: No lo utilizan o lo comparten

*/

 gen cuartneg=1 if (nroneg>=1 & nroneg<=5)
 recode cuartneg (.=0)

 gen nrocuart= nrohabit-cuartneg if nrohabit>nroneg /* Not used */

 gen persroom = (pers/nrohabit)
 
* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenencia>=1 & tenencia<=6) & (tipoviv>=1 & tipoviv<=8)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=4 & tenencia<=6) | (tipoviv>=7 & tipoviv<=8))

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if ((paredes>=1 & paredes<=7) & (piso>=1 & piso<=5))         /* Total population excluding missing information */
 replace secten_2=1 if ((paredes>=5 & paredes<=7) | (piso>=4 & piso<=5))

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator
* 2c. PISO 

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=5)
 replace DIRT=1 if (piso==5)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (tasadeso==0 | tasadeso==1) & (edad>=15 & edad<=24)
 replace UNMPLYMENT15=1 if (tasadeso==1)	       & (edad>=15 & edad<=24)

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
/*
5j. ¿TIENE USTED TELÉFONO EN USO? 
*/

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (telefono>=1 & telefono<=2) /* Total population excluding missing information */
 replace TELCEL=1 if  (telefono==1 		)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono>=1 & telefono<=2) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1 		    )

** Target 18, Indicator: "Personal computers in use per 100 population"
/*
6. Equipamento del hogar
9. Computadora
*/

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if (computad>=1 & computad<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computad==1)

* Target 18, Indicator: "Internet users per 100 population"

** Not available for this year

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* INCLUDES POPULATION 12 TO 15 YEARS-OLD

 gen     CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & peaa==1

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

** Disconnected Youths
/*
pqnobus
9. INDIQUE LA RAZÓN POR NO HABER BUSCADO TRABAJO NI HECHO NADA PARA ESTABLECER NEGOCIO O
EMPRESA PROPIA 
0: No aplicable
1: Espera ser reintegrado a su trabajo
2: Espera respuesta de empleadores
3: Espera período de actividad agrícola
4: Encontró trabajo y espera fecha de inicio
5: Creía que no había trabajo disponible en la región
6: No sabía como buscar empleo
7: Asiste a un centro de enseñanza o está de vacaciones
8: Por obligaciones familiares o personales
9: Enfermedad o accidente
10: Quehaceres domésticos
11: Jubilado o pensionado
12: No puede trabajar (inválido, anciano, etc.)
13: Asiste a un centro de formación o academia
*/

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (pqnobus==5 | pqnobus==6 |pqnobus==11)
	
******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

*** Proportion of population below corresponding grade for age

 gen     rezago=0       if (anoest>=0 & anoest<99)  & edad==7 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==8
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==8
 
 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==9
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==10
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==11
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==11

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==12
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==12

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==13
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==14
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==15
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==16
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==17
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==17

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==18
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==18

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if  (edad>=8 & edad<=18) & (rezago==1 | rezago==0)
 replace REZ=1 if  (edad>=8 & edad<=18) & (rezago==1)
	
* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
	
* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))
 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))
 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=18) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=18) & (anoest>=0 & anoest<99)
rename ocup ocuppr

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.
   
****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (segsoci==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=1 if tamest>=1 & tamest<=2
replace tamemp_ci=2 if tamest==3 & tamest!=.
replace tamemp_ci=3 if tamest==4 & tamest!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci

*************
**pension_ci*
*************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if trabante==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*li_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=78 if rama_ci==1
replace salmm_ci=132 if rama_ci==3
replace salmm_ci=132 if rama_ci==6
replace salmm_ci=114 if salmm_ci==.
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
gen tecnica_ci=((nivedu>=51 & nivedu<=53) | (ultgrado>=51 & ultgrado<=54))
label var tecnica_ci "=1 formacion terciaria tecnica"	


*****************
**categoinac_ci**
*****************	
ren pqnobus r407
gen categoinac_ci=.	
replace categoinac_ci=1 if r407==11
replace categoinac_ci=2 if r407==7 | r407==13
replace categoinac_ci=3 if r407==10
recode categoinac_ci .=4 if condocup_ci==3
label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
*****************
***formal_ci*****
*****************
gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"


*variables que faltan generar
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen tipopen_ci=.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen vivitit_ch=.
gen categosec_ci=.

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_idioma_ci  id_ind_ci id_afro_ci raza_ci  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close



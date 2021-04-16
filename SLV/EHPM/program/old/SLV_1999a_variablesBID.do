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
local ANO "1999"
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
2013 - incoporacion de Variables LMK por Yessenia Loaysa
Última versión: Maria Laura Oliveri - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Fecha última modificación: 23 de Octubre de 2013

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
/*April 10, 2006 (Analia)
The following line was dropped:
replace ynlm_ci=. if emp_ci==0
*July 3, 2010 (Yanira)
Excluding all no answer codes*/
****************************************************************************/

clear all
set more off
use "`base_in'", clear

foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

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

sort folio tipo
egen idh_ch= group(folio tipo)
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

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

gen anio_c=1999
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=r015

label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril"
label define mes_c 5 "Mayo" 6 " Junio" 7 "Julio" 8 "Agosto", add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add

label value mes_c mes_c

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=8
replace relacion_ci=5 if parentco==10 | parentco==11
replace relacion_ci=6 if parentco==9

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

* Modificaciones Marcela Rubio - Septiembre 2014

/*
gen sexo_ci=r106
*/

gen sexo_ci=sexo
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci


**********
***edad***
**********

* Modificaciones Marcela Rubio - Septiembre 2014

/*
gen edad_ci=r108
label variable edad_ci "Edad del individuo"
*/

gen edad_ci=edad
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estcivil==6
replace civil_ci=2 if estcivil==1 | estcivil==2
replace civil_ci=3 if estcivil==4 | estcivil==5
replace civil_ci=4 if estcivil==3

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

*************
***raza_ci***
*************

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
/*
gen condocup_ci=.
* modifica MLO: 2014,01
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact==2 | condact==3
replace condocup_ci=3 if condact>=4 & condact<=11
replace condocup_ci=4 if condact==0 /*menor a 10*/
/*
old version
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if busco==1 & (r408>=1 & r408<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10*/
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
* Considerando a quienes hicieron algo para buscar trabajo. MGD 06/10/2014
/*gen condocup_ci=.
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact==2 | condact==3
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=10
recode condocup_ci .=4 if edad_ci<10 

label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"*/

* NOTA MGD 10/07/2014: condocup se crea como missing para esto años dado que la variable CONDACT
* esta con errores ( dentro de menores CONDACT==0 de edad estan individuos de todas las edades)
* Al tratar de generar con las variable originales tampoco coincide con la tendencia.

g condocup_ci=.

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


capture drop trabajo
capture drop tienetra
capture drop busco
capture drop pqnobus
rename r403 trabajo
rename r404 tienetra
rename r406 busco
rename r407 pqnobus
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
gen desemp1_ci=(emp_ci==0 & busco==1) | (pqnobus==14)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 & busco==1 | (busco==2 & (pqnobus>=12  & pqnobus<=14)))

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
gen pea3_ci=.

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & busco==2 & (pqnobus==1 | pqnobus==3))*/


*****************
***horaspri_ci***
*****************
capture drop horastra
capture drop hrshabi
rename r411a horastra
rename r412a hrshabi

gen horaspri_ci=horastra if trabajo==1
replace horaspri_ci=hrshabi if trabajo==2 & tienetra==1

replace horaspri_ci=. if horastra==99
replace horaspri_ci=. if hrshabi==99
replace horaspri_ci=. if emp_ci==0

*****************
***horastot_ci***
*****************
capture drop hrssec
rename r432 hrssec

egen horastot_ci=rsum(horaspri_ci hrssec)
replace horastot_ci=. if horaspri_ci==. & hrssec==.
replace horastot_ci=horaspri_ci if hrssec==99
replace horastot_ci=. if emp_ci==0


******************************
*	subemp_ci
******************************
*NA
/*no se pregunta en la encuesta si se quiere trabajar mas.La encuesta pregunta porque motivo trabaja menos de 40 horas*/
*Alternativa MGD 06/21/2014: aunque no se pregunta si quiere trabajr ams horas se puede inferir con la pregunta de que trabaja
*menor de 40 horas porque solo consiguio empleo parcial o falta de trabajo.
g subemp_ci=0
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (men40hrs== 3 | men40hrs==2)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.


******************
***categopri_ci***
******************
capture drop categ
rename r417 categ

gen categopri_ci=.
replace categopri_ci=1 if categ==1
replace categopri_ci=2 if categ==2 | categ==3 |categ==4 
replace categopri_ci=3 if categ==6 | categ==7 |categ==8 | categ==9 
replace categopri_ci=0 if categ==10
replace categopri_ci=4 if categ==5
replace categopri_ci=. if emp_ci==0 /*Hay un desempleado que contesto en la categoría de empleo*/

label define categopri_ci 0"Otros" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


/*Dentro de la categoría empleado se incluyen a los cooperativistas, esta definición es más amplia de 
lo usual, principalmemte porque se incluye a los cooperativistas,pero la encuesta considera 
a todos estos como trabajadores dependientes. 
En el manual del encuestador: "En el caso de una persona que está asociada 
a la cooperativa y además trabaja como patrono o cuenta propia en tierras que son propiedad 
de la empresa cooperativa, se le anotará código 01 a 03, según el caso. Cuando la persona 
cumple siempre el primer requisito o sea que es asociada y trabaja como jornalero o empleado 
en el trabajo colectivo de la cooperativa se le anotará código igual 04."
Por otra parte el grupo "otros" se incorpora dentro de la categoría empleados ya 
que la encuesta los considera empleados dependientes y declaran ingresos*/
/*
*****************
***contrato_ci***
*****************

capture drop contrato
rename r418 contrato

gen contrato_ci=.
replace contrato_ci=1 if contrato==1
replace contrato_ci=0 if contrato==2

/*Esta pregunta se le hace solamente a los asalariados, aprendices u otros
de la pregunta 417, de manera que no incluye a todos los empleados como se define
categopri_ci==3*/

***************
***segsoc_ci***
***************

capture drop segsoc
rename r421 segsoc

gen segsoc_ci=.
replace segsoc_ci=1 if segsoc==1 
replace segsoc_ci=0 if segsoc==3 | segsoc==2

/*Consideramos con seguridad social a los que son afiliados*/
/*En este caso no se le pregunta a los que trabajan como servicio domestico*/
*/
*****************
***nempleos_ci***
*****************

capture drop otrotrab
rename r431 otrotrab

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & otrotrab==1
replace nempleos_ci=. if pea_ci==0
/*
*****************
***firmapeq_ci***
*****************

capture drop tamest
rename r420 tamest

gen firmapeq_ci=.
replace firmapeq_ci=1 if tamest<5
replace firmapeq_ci=0 if tamest>=5 & tamest<99
replace firmapeq_ci=. if categ==9
replace firmapeq_ci=. if emp_ci==0

/* No se le pregunta a los empleados domesticos*/
*/
*****************
***spublico_ci***
*****************

capture drop sectorp
rename r419 sectorp

gen spublico_ci=.
replace spublico_ci=1 if sectorp==2
replace spublico_ci=0 if sectorp==1
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | categ==9 | categ==4
/*Sólo se le hace esta pregunta a los asalariados, aprendices y otros*/

**************
***ocupa_ci***
**************


gen ocupa_ci=.
replace ocupa_ci=1 if (r414>=211 & r414<=369) & emp_ci==1
replace ocupa_ci=2 if (r414>=111 & r414<=131) & emp_ci==1
replace ocupa_ci=3 if (r414>=411 & r414<=422) & emp_ci==1
replace ocupa_ci=4 if ((r414>=520 & r414<=526) | r414==911) & emp_ci==1
replace ocupa_ci=5 if ((r414>=511 & r414<=516) | (r414>=912 & r414<=916)) & emp_ci==1
replace ocupa_ci=6 if ((r414>=611 & r414<=621) | (r414>=921 & r414<=922)) & emp_ci==1
replace ocupa_ci=7 if ((r414>=711 & r414<=834) | (r414>=931 & r414<=933)) & emp_ci==1
replace ocupa_ci=8 if r414==11 & emp_ci==1
replace ocupa_ci=. if emp_ci==0 | r414==999 | r414==0
*************
***rama_ci***
*************

capture drop rama
rename r416 rama

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

capture drop ocupant
capture drop trabante
rename r435 ocupant
rename r410 trabante

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

capture drop ramaant
rename r437 ramaant

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

capture drop tpobusm
rename r409 tpobusm

gen durades_ci=tpobusm/4.3 if tpobusm>0 & tpobusm<999 /*La variable debe llevarse a meses*/
replace durades_ci=. if tpobusm==0 /*No aplica*/
replace durades_ci=. if tpobusm==999 /*No responde*/ 
replace durades_ci=. if emp_ci==1

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
1999-Jan	8.76
1999-Feb	8.76
1999-Mar	8.76
1999-Apr	8.76
1999-May	8.76
1999-Jun	8.76
1999-Jul	8.76
1999-Aug	8.76
1999-Sep	8.76
1999-Oct	8.76
1999-Nov	8.76
1999-Dec	8.76
*/
****************************
***ylmpri_ci & ylmpri1_ci***
****************************

/*Para los trabajadores dependientes*/

capture drop ingresos
capture drop tipopago
rename r423 ingresos
rename r422 tipopago

gen yprid=.
replace yprid=ingresos*30 if tipopago==1
replace yprid=ingresos*4.3 if tipopago==2
replace yprid=ingresos*2 if tipopago==3
replace yprid=ingresos if tipopago==4 | tipopago==5
replace yprid=0 if tipopago==6
replace yprid=999999 if ingresos==999999

gen hrsextrasd=.
replace hrsextrasd=r42401a*r42401b/12
replace hrsextrasd=999999 if r42401a==999999 | r42401b==999

gen vacacionesd=.
replace vacacionesd=r42402a*r42402b/12
replace vacacionesd=999999 if r42402a==999999 | r42402b==999

gen aguinaldod=.
replace aguinaldod=r42403a*r42403b/12
replace aguinaldod=999999 if r42403a==999999 | r42403b==999

gen bonificacionesd=.
replace bonificacionesd=r42404a*r42404b/12
replace bonificacionesd=999999 if r42404a==999999 | r42404b==999

egen yprijbd=rsum( yprid hrsextrasd vacacionesd aguinaldod bonificacionesd)
replace yprijbd=999999 if yprid==999999 | hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999 
replace yprijbd=yprid if yprid>0 & yprid~=999999 & ( hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. & hrsextrasd==. & vacacionesd==. & aguinaldod==. & bonificacionesd==. 

/*Para los trabajadores independientes*/
capture drop ingrbru
capture drop costos
capture drop frecingr
rename r427 ingrbru
rename r428 costos
rename r426 frecingr

gen ingrneto=ingrbru-costos
replace ingrneto=0 if ingrneto<0 
replace ingrneto=999999 if ingrbru==9999999 | costos==9999999

gen yprijbi=.
replace yprijbi=ingrneto*30 if frecingr==1
replace yprijbi=ingrneto*4.3 if frecingr==2
replace yprijbi=ingrneto*2 if frecingr==3
replace yprijbi=ingrneto if frecingr==4 | frecingr==6
replace yprijbi=ingrneto/12 if frecingr==5
replace yprijbi=999999 if ingrneto==999999 | frecingr==9
replace yprijbi=. if categopri_ci>2

*replace yprijbi=0 if catego<=2 & yprijbi==. & emp_ci==1 

/*Ojo con esto último. Originalmente la encuesta computa una serie de 
missings que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/

egen ylmpri_ci=rsum(yprijbi yprid)
replace ylmpri_ci=. if yprijbi==999999 | yprid==999999
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp_ci==0
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylmpri_ci= ylmpri_ci/8.76


egen ylmpri1_ci=rsum(yprijbi yprijbd)
replace ylmpri1_ci=. if yprijbi==999999 | yprijbd==999999
replace ylmpri1_ci=. if yprijbd==. & yprijbi==.
replace ylmpri1_ci=. if emp_ci==0

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=(ylmpri1_ci==. & emp_ci==1)

*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

gen p42405=.
replace p42405=r42405a*r42405b/12 
replace p42405=0 if emp_ci==1 & r42405a==.
replace p42405=. if r42405a==999999 | r42405b==999

gen p42406=.
replace p42406=r42406a*r42406b/12 
replace p42406=0 if emp_ci==1 & r42406a==.
replace p42406=. if r42406a==999999 | r42406b==999


gen p42407=.
replace p42407=r42407a*r42407b/12 
replace p42407=0 if emp_ci==1 & r42407a==.
replace p42407=. if r42407a==999999 | r42407b==999


gen p42408=.
replace p42408=r42408a*r42408b/12 
replace p42408=0 if emp_ci==1 & r42408a==.
replace p42408=. if r42408a==999999 | r42408b==999


gen p42409=.
replace p42409=r42409a*r42409b/12 
replace p42409=0 if emp_ci==1 & r42409a==.
replace p42409=. if r42409a==999999 | r42409b==999


gen p42410=.
replace p42410=r42410a*r42410b/12 
replace p42410=0 if emp_ci==1 & r42410a==.
replace p42410=. if r42410a==999999 | r42410b==999


gen p42411=.
replace p42411=r42411a*r42411b/12 
replace p42411=0 if emp_ci==1 & r42411a==.
replace p42411=. if r42411a==999999 | r42411b==999


egen ylnmpri_ci=rsum(p42405 p42406 p42407 p42408 p42409 p42410 p42411)
replace ylnmpri_ci=. if p42405==. & p42406==. & p42407==. & p42408==. & p42409==. & p42410==. & p42411==. 
replace ylnmpri_ci=. if emp_ci==0

*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylnmpri_ci= ylnmpri_ci/8.76

* Ingreso laboral no monetario de la actividad principal 1 (SUMAMOS AUTOCONSUMO PARA LOS INDEPENDIENTES)

capture drop valaut
rename r430 valaut

egen ylnmpri1_ci=rsum(ylnmpri_ci valaut)
replace ylnmpri1_ci=. if ylnmpri_ci==. & valaut==.
replace ylnmpri1_ci=ylnmpri_ci if valaut==999999
replace ylnmpri1_ci=. if emp_ci==0

***************
***ylmsec_ci***
***************

capture drop ysec
rename r433 ysec

gen ysec1=ysec
replace ysec1=. if ysec==999999

gen hrsextras=.
replace hrsextras=r43401a*r43401b/12
replace hrsextras=. if r43401a==999999 | r43401b==999

gen vacaciones=.
replace vacaciones=r43402a*r43402b/12
replace vacaciones=. if r43402a==999999 | r43402b==999

gen aguinaldo=.
replace aguinaldo=r43403a*r43403b/12
replace aguinaldo=. if r43403a==999999 | r43403b==999

gen bonificaciones=.
replace bonificaciones=r43404a*r43404b/12
replace bonificaciones=. if r43404a==999999 | r43404b==999

gen ylmsec_ci=ysec1
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylmsec_ci= ylmsec_ci/8.76

egen ylmsec1_ci=rsum(ysec1 hrsextras vacaciones aguinaldo bonificaciones)
replace ylmsec1_ci=. if ysec1==. & hrsextras==. & vacaciones==. & aguinaldo==. & bonificaciones==. 
replace ylmsec1_ci=. if emp_ci==0 | otrotrab==2

******************
****ylnmsec_ci****
******************

gen p43405=.
replace p43405=r43405a*r43405b/12 
replace p43405=0 if emp_ci==1 & r43405a==.
replace p43405=. if r43405a==999999 | r43405b==999

gen p43406=.
replace p43406=r43406a*r43406b/12 
replace p43406=0 if emp_ci==1 & r43406a==.
replace p43406=. if r43406a==999999 | r43406b==999

gen p43407=.
replace p43407=r43407a*r43407b/12 
replace p43407=0 if emp_ci==1 & r43407a==.
replace p43407=. if r43407a==999999 | r43407b==999

gen p43408=.
replace p43408=r43408a*r43408b/12 
replace p43408=0 if emp_ci==1 & r43408a==.
replace p43408=. if r43408a==999999 | r43408b==999

gen p43409=.
replace p43409=r43409a*r43409b/12 
replace p43409=0 if emp_ci==1 & r43409a==.
replace p43409=. if r43409a==999999 | r43409b==999

gen p43410=.
replace p43410=r43410a*r43410b/12 
replace p43410=0 if emp_ci==1 & r43410a==.
replace p43410=. if r43410a==999999 | r43410b==999

gen p43411=.
replace p43411=r43411a*r43411b/12 
replace p43411=0 if emp_ci==1 & r43411a==.
replace p43411=. if r43411a==999999 | r43411b==999

egen ylnmsec_ci=rsum(p43405 p43406 p43407 p43408 p43409 p43410 p43411)
replace ylnmsec_ci=. if p43405==. & p43406==. & p43407==. & p43408==. & p43409==. & p43410==. & p43411==. 
replace ylnmsec_ci=. if emp_ci==0
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylnmsec_ci= ylnmsec_ci/8.76

**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylm1_ci=rsum(ylmpri1_ci ylmsec1_ci)
replace ylm1_ci=. if ylmpri1_ci==. & ylmsec1_ci==.


************************
***ylnm_ci & ylnm1_ci***
************************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

egen ylnm1_ci=rsum(ylnmpri1_ci ylnmsec_ci)
replace ylnm1_ci=. if ylnmpri1_ci==. & ylnmsec_ci==.

*************
***ynlm_ci***
*************

gen remesasext=.
replace remesasext=r44201a*r44201b/12
replace remesasext=. if r44201a==999999 | r44201b==999

gen ayuda=.
replace ayuda=r44202a*r44202b/12
replace ayuda=. if r44202a==999999 | r44202b==999

gen cuotalim=.
replace cuotalim=r44203a*r44203b/12
replace cuotalim=. if r44203a==999999 | r44203b==999

gen alqui=.
replace alqui=r44204a*r44204b/12
replace alqui=. if r44204a==999999 | r44204b==999

rename alqneg alqneg1
gen alqneg=.
replace alqneg=r44205a*r44205b/12
replace alqneg=. if r44205a==999999 | r44205b==999

rename alqterr alqterr1
gen alqterr=.
replace alqterr=r44206a*r44206b/12
replace alqterr=. if r44206a==999999 | r44206b==999

rename jubil jubil1
gen jubil=.
replace jubil=r44207a*r44207b/12
replace jubil=. if r44207a==999999 | r44207b==999

gen deveh=.
replace deveh=r44208a*r44208b/12
replace deveh=. if r44208a==999999 | r44208b==999

rename otros otros2
gen otros=.
replace otros=r44209a*r44209b/12
replace otros=. if r44209a==999999 | r44209b==999

gen utilidades=.
replace utilidades=r44301/12
replace utilidades=. if r44301==999999

gen dividendos=.
replace dividendos=r44302/12
replace dividendos=. if r44302==999999

gen intereses=.
replace intereses=r44303/12
replace intereses=. if r44303==999999

gen herencias=.
replace herencias=r44304/12
replace herencias=. if r44304==999999

gen indemnizacion=.
replace indemnizacion=r44305/12
replace indemnizacion=. if r44305==999999

gen ayudagob=.
replace ayudagob=r44306/12
replace ayudagob=. if r44306==999999

gen otross=.
replace otross=r44307/12
replace otross=. if r44307==999999

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg alqterr jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob otross)
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & alqterr==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & otross==. 

*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ynlm_ci= ynlm_ci/8.76

gen ynlnm_ci=.
****************
***remesas_ci***
****************

gen remesas_ci=remesasext
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace remesas_ci= remesas_ci/8.76

************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

*********************************
*** nrylmpri_ch & nrylmpri_ch ***
*********************************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

by idh_ch, sort: egen nrylmpri1_ch=sum(nrylmpri1_ci) if miembros_ci==1
replace nrylmpri1_ch=1 if nrylmpri1_ch>0 & nrylmpri1_ch<.
replace nrylmpri1_ch=. if nrylmpri1_ch==.


************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1
replace ylmnr1_ch=. if nrylmpri1_ch==1

**************************
*** ylnm_ch & ylnm1_ch ***
**************************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
by idh_ch, sort: egen ylnm1_ch=sum(ylnm1_ci) if miembros_ci==1

*******************
*** remesasm_ch ***
*******************

gen remesash=.
replace remesash=ayudaef/12 if frecayud==1
replace remesash=ayudaef/6 if frecayud==2
replace remesash=ayudaef/3 if frecayud==3
replace remesash=ayudaef/2 if frecayud==4
replace remesash=ayudaef if frecayud==5
replace remesash=ayudaef*2 if frecayud==6
replace remesash=. if ayudaef==99999 /*Cero significa No aplicable*/
replace remesash=. if frecayud==0 /*Cero significa No aplicable*/
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace remesash= remesash/8.76

gen remesasnm=.
replace remesasnm=ayudaes/12
replace remesasnm=. if ayudaes==999999
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace remesasnm= remesasnm/8.76

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash remesasnm)
replace remesas_ch=. if remesasi==. & remesash==. & remesasnm==.



***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm_ch=rsum(ynlm remesash)
replace ynlm_ch=. if ynlm==. & remesash==.
drop ynlm


****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=remesasnm


*******************
*** autocons_ci ***
*******************

gen autocons_ci=valaut
replace autocons_ci=. if valaut==999999
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace autocons_ci= autocons_ci/8.76

*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=r311
replace rentaimp_ch=. if r311==99999
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace rentaimp_ch= rentaimp_ch/8.76



******************************
***ylhopri_ci & ylhopri1_ci***
******************************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

**************************
***ylmho_ci & ylm1ho_ci***
**************************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)


****************************
***VARIABLES DE EDUCACION***
****************************


gen byte aedu_ci=.

/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/

replace aedu_ci=0 if r204==0 & r203==1
replace aedu_ci=0 if r204==1 & r203==1

replace aedu_ci=1 if r204==2 & r205==1 & r203==1
replace aedu_ci=2 if r204==2 & r205==2 & r203==1
replace aedu_ci=3 if r204==2 & r205==3 & r203==1
replace aedu_ci=4 if r204==2 & r205==4 & r203==1
replace aedu_ci=5 if r204==2 & r205==5 & r203==1
replace aedu_ci=6 if r204==2 & r205==6 & r203==1
replace aedu_ci=7 if r204==2 & r205==7 & r203==1
replace aedu_ci=8 if r204==2 & r205==8 & r203==1
replace aedu_ci=9 if r204==2 & r205==9 & r203==1

replace aedu_ci=10 if r204==3 & r205==10 & r203==1
replace aedu_ci=11 if r204==3 & r205==11 & r203==1
replace aedu_ci=12 if r204==3 & r205==12 & r203==1

replace aedu_ci=13 if r204==4 & r205==1 & r203==1
replace aedu_ci=14 if r204==4 & r205==2 & r203==1
replace aedu_ci=15 if r204==4 & r205==3 & r203==1
replace aedu_ci=16 if r204==4 & r205==4 & r203==1
replace aedu_ci=17 if r204==4 & r205==5 & r203==1
replace aedu_ci=18 if r204==4 & r205==6 & r203==1
replace aedu_ci=19 if r204==4 & r205==7 & r203==1
replace aedu_ci=20 if r204==4 & r205==8 & r203==1 
replace aedu_ci=22 if r204==4 & r205==10 & r203==1
replace aedu_ci=23 if r204==4 & r205==11 & r203==1
replace aedu_ci=24 if r204==4 & r205==12 & r203==1
replace aedu_ci=25 if r204==4 & r205==13 & r203==1
replace aedu_ci=27 if r204==4 & r205==15 & r203==1

replace aedu_ci=13 if r204==5 & r205==1 & r203==1
replace aedu_ci=14 if r204==5 & r205==2 & r203==1
replace aedu_ci=15 if r204==5 & r205==3 & r203==1


/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if r219a==0 & r217==1
replace aedu_ci=0 if r219a==1 & r217==1

replace aedu_ci=1 if r219a==2 & r219b==1 & r217==1
replace aedu_ci=2 if r219a==2 & r219b==2 & r217==1
replace aedu_ci=3 if r219a==2 & r219b==3 & r217==1
replace aedu_ci=4 if r219a==2 & r219b==4 & r217==1
replace aedu_ci=5 if r219a==2 & r219b==5 & r217==1
replace aedu_ci=6 if r219a==2 & r219b==6 & r217==1
replace aedu_ci=7 if r219a==2 & r219b==7 & r217==1
replace aedu_ci=8 if r219a==2 & r219b==8 & r217==1
replace aedu_ci=9 if r219a==2 & r219b==9 & r217==1
replace aedu_ci=9 if r219a==2 & r219b==10 & r217==1

replace aedu_ci=10 if r219a==3 & r219b==10 & r217==1 
replace aedu_ci=11 if r219a==3 & r219b==11 & r217==1
replace aedu_ci=12 if r219a==3 & r219b==12 & r217==1

replace aedu_ci=13 if r219a==4 & r219b==1 & r217==1
replace aedu_ci=14 if r219a==4 & r219b==2 & r217==1
replace aedu_ci=15 if r219a==4 & r219b==3 & r217==1
replace aedu_ci=16 if r219a==4 & r219b==4 & r217==1
replace aedu_ci=17 if r219a==4 & r219b==5 & r217==1
replace aedu_ci=18 if r219a==4 & r219b==6 & r217==1
replace aedu_ci=19 if r219a==4 & r219b==7 & r217==1
replace aedu_ci=20 if r219a==4 & r219b==8 & r217==1
replace aedu_ci=21 if r219a==4 & r219b==9 & r217==1
replace aedu_ci=22 if r219a==4 & r219b==10 & r217==1
replace aedu_ci=23 if r219a==4 & r219b==11 & r217==1
replace aedu_ci=24 if r219a==4 & r219b==12 & r217==1
replace aedu_ci=25 if r219a==4 & r219b==13 & r217==1
replace aedu_ci=26 if r219a==4 & r219b==14 & r217==1
replace aedu_ci=27 if r219a==4 & r219b==15 & r217==1


replace aedu_ci=13 if r219a==5 & r219b==1 & r217==1
replace aedu_ci=14 if r219a==5 & r219b==2 & r217==1
replace aedu_ci=15 if r219a==5 & r219b==3 & r217==1

replace aedu_ci=0 if r203==2 & r217==2
replace aedu_ci=. if edad<=3

replace aedu_ci=. if aedu_ci>edad & aedu_ci~=. 
/*Hay 3 casos en donde los años de educación son mayores a la edad por eso lo ajustamos de 
esta forma*/

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
replace edupre_ci=1 if r219a==1 | r204==1 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (r219a==4 | r204==4) & aedu_ci~=.
replace eduac_ci=0 if (r219a==5 | r204==5) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if r203==1
label variable asiste_ci "Asiste actualmente a la escuela"


*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=r221
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 " Enfermedad", add 
label define pqnoasis_ci 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis_ci 7 "Otros", add
label value pqnoasis_ci pqnoasis_ci

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
replace repiteult_ci=. if repite==0
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


gen aguared_ch=(r313==1 | r313==2)

gen aguadist_ch=1 if r313==1
replace aguadist_ch=2 if r313==2
replace aguadist_ch=3 if r313>=2 & r313<=9

gen aguamala_ch=(r314==2)
replace aguamala_ch=. if r314==9

gen aguamide_ch=.
/*NA*/

gen luz_ch=(r312==1)
replace luz_ch=. if r312==9

gen luzmide_ch=.
/*NA*/

gen combust_ch=(r312>=1 & r312<=2)
replace combust_ch=. if r312==9

gen bano_ch=(r317>=1 & r317<=6)

gen banoex_ch=(r317>=4 & r317<=6)

gen des1_ch=.
/*NA*/

gen des2_ch=.
/*NA*/

gen piso_ch=0 if r304==4
replace piso_ch=1 if r304>=1 & r304<=3
replace piso_ch=2 if r304==5

gen pared_ch=0 if r303==2 | r303==3 | r303==6 | r303==7
replace pared_ch=1 if r303==1 | r303==4 | r303==5
replace pared_ch=2 if r303==8

gen techo_ch=0 if r302==5 | r302==6
replace techo_ch=1 if r302>=1 & r302<=4
replace techo_ch=2 if r302==7

gen resid_ch=0 if r322==1 | r322==2
replace resid_ch=1 if r322==4 | r322==5
replace resid_ch=2 if r322==6
replace resid_ch=3 if r322==3 | r322==7

gen dorm_ch=r306

gen cuartos_ch=r305

gen cocina_ch=.
/*NA*/

gen telef_ch=(r321>=1 & r321<=4)
replace telef_ch=. if r321==9

gen refrig_ch=(r32305==1)
replace refrig_ch=. if r32305==9

gen freez_ch=.
/*NA*/

gen auto_ch=(r32311==1)
replace auto_ch=. if r32311==9


gen compu_ch=(r32309==1)
replace compu_ch=. if r32309==9

gen internet_ch=.
/*NA*/

gen cel_ch=(r321>=2 & r321<=4)
replace cel_ch=. if r321==9


gen vivi1_ch=1 if r301==1
replace vivi1_ch=2 if r301==2
replace vivi1_ch=3 if r301>2 & r301<9

gen vivi2_ch=(r301>=1 & r301<=2)
replace vivi2_ch=. if r301==9

gen viviprop_ch=0 if r308a==1
replace viviprop_ch=1 if r308a==3
replace viviprop_ch=2 if r308a==2
replace viviprop_ch=3 if r308a>=4 & r308a<=6


gen viviitit_ch=.
/*NA*/

gen vivialq_ch=r308c if r308a==1
replace vivialq_ch=. if r308c==99999
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace vivialq_ch= vivialq_ch/8.76

gen vivialqimp_ch=r311
replace vivialqimp_ch=. if r311==99999
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76

rename ocup ocuppr



****************
*afiliado_ci****
****************
gen afiliado_ci= (segsalud==1 | segsalud==2)
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.
   
****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (segsoc==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
* Incorporando variable de temporalidad en asalariados y firma de contrato. MGD 06/16/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (r418==1 & categ==6) & categopri_ci==3
replace tipocontrato_ci=2 if (r418==1 & categ==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (r418==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*tamemp_ci***
*************
gen tamemp_ci=1 if tamest>=1 & tamest<=5
replace tamemp_ci=2 if tamest>=6 & tamest<=50
replace tamemp_ci=3 if tamest>50 & tamest!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (r44207a>0 & r44207a<999999 ) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=r44207a*r44207b/12 if pension_ci==1
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ypen_ci= ypen_ci/8.76
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

*********
*lp25_ci***
*********
gen lp25_ci =34.64886
label var lp25_ci "Linea de pobreza 2.5 dólares, año base 2005"

*********
*lp4_ci***
*********
gen lp4_ci =55.43818
label var lp4_ci "Linea de pobreza 4 dólares, año base 2005"

*************
**salmm_ci***
*************
gen salmm_ci= .
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
gen tecnica_ci=(ultnivel==5 | nivel==5)
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
gen desalent_ci=.
gen categosec_ci=.

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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

*firmapeq_ci


	
qui destring $var, replace


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
set more off
compress


do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"


saveold "`base_out'", replace


log close


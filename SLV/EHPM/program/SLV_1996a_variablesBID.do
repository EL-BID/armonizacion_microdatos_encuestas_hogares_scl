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

local PAIS SLV
local ENCUESTA EHPM
local ANO "1996"
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
2013 - incoporacion de Variables LMK por Yessenia Loayza (desloay@hotmail.com)
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

gen factor_ch=FACTOREX
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

gen idh_ch=ID_HOGAR
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci=NRORDEN
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if AREA==2
replace zona_c=1 if AREA==1

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

gen anio_c=1996
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
replace relacion_ci=1 if PARENTCO==1
replace relacion_ci=2 if PARENTCO==2
replace relacion_ci=3 if PARENTCO==3
replace relacion_ci=4 if PARENTCO>=4 & PARENTCO<=10
replace relacion_ci=5 if PARENTCO==12 | PARENTCO==13
replace relacion_ci=6 if PARENTCO==11

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

gen factor_ci=FACTOREX
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=SEXO

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***EDAD***
**********

gen edad_ci=EDAD
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if ESTCIVIL==1
replace civil_ci=2 if ESTCIVIL==2
replace civil_ci=3 if ESTCIVIL==4 
replace civil_ci=4 if ESTCIVIL==3
replace civil_ci=. if edad_ci<12 /*Hay una persona menor a 12 que contesta esta pregunta*/

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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & EDAD>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & EDAD<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & EDAD>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & EDAD<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & EDAD<1)
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
/*
gen condocup_ci=.
replace condocup_ci=1 if CONDACT==1
replace condocup_ci=2 if BUSCO==1 & (ULT30DIA>=1 & ULT30DIA<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion armonizada"
*/

*Modif. MGD 06/10/2014
gen condocup_ci=.
replace condocup_ci=1 if CONDACT==1
replace condocup_ci=2 if CONDACT==2 | CONDACT==3
replace condocup_ci=4 if CONDACT==0
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2 & condocup_ci!=4) & edad_ci>=10
*recode condocup_ci .=4 if edad_ci<10 
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion armonizada"

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
replace emp_ci=1 if TRABAJO==1
replace emp_ci=1 if TRABAJO==2 & TIENETRA==1 

****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 & BUSCO==1)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 & BUSCO==1 | (BUSCO==2 & (PQNOBUS==1 |PQNOBUS==2 | PQNOBUS==4)))

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

gen desalent_ci=(emp_ci==0 & BUSCO==2 & (PQNOBUS==5))


*****************
***horaspri_ci***
*****************

gen horaspri_ci=HORASTRA if TRABAJO==1
replace horaspri_ci=HRSHAB if TRABAJO==2 & TIENETRA==1

replace horaspri_ci=. if HORASTRA==99
replace horaspri_ci=. if HRSHAB==99
replace horaspri_ci=. if emp_ci==0

*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci HRSSEC)
replace horastot_ci=. if horaspri_ci==. & HRSSEC==.
replace horastot_ci=horaspri_ci if HRSSEC==99
replace horastot_ci=. if emp_ci==0


******************************
*	subemp_ci
******************************
*NA
/*no se pregunta en la encuesta si se quiere trabajar mas.La encuesta pregunta porque motivo trabaja menos de 40 horas*/
*Alternativa MGD 06/21/2014: aunque no se pregunta si quiere trabajr ams horas se puede inferir con la pregunta de que trabaja
*menor de 40 horas porque solo consiguio empleo parcial o falta de trabajo.
g subemp_ci=0
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (MEN40HRS== 3 | MEN40HRS==2)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if CATEGO==1
replace categopri_ci=2 if CATEGO==2 | CATEGO==4
replace categopri_ci=3 if  CATEGO==5 |CATEGO==6 | CATEGO==7 |CATEGO==8 
replace categopri_ci=4 if CATEGO==3
replace categopri_ci=0 if CATEGO==9

label define categopri_ci 0"Otro" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional TRABAJO principal"

/*
*****************
***contrato_ci***
*****************

gen contrato_ci=.


***************
***segsoc_ci***
***************

gen segsoc_ci=.
replace segsoc_ci=1 if SEGSOCI==1 
replace segsoc_ci=0 if SEGSOCI==2*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & OTROTRAB==1
replace nempleos_ci=. if pea_ci==0
/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if TAMEST==1 | TAMEST==2
replace firmapeq_ci=0 if TAMEST==3 | TAMEST==4
replace firmapeq_ci=. if emp==0

/*En este caso no se le pregunta a los que trabajan como servicio domestico.
Y estos son missings */
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if SECTOR==2
replace spublico_ci=0 if SECTOR==1
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | CATEGO==8 | CATEGO==4
/*Sólo se le hace esta pregunta a los asalariados, aprendices y otros*/


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (OCUPACIO>=211 & OCUPACIO<=369) & emp_ci==1
replace ocupa_ci=2 if (OCUPACIO>=111 & OCUPACIO<=131) & emp_ci==1
replace ocupa_ci=3 if (OCUPACIO>=411 & OCUPACIO<=422) & emp_ci==1
replace ocupa_ci=4 if ((OCUPACIO>=520 & OCUPACIO<=526) | OCUPACIO==911) & emp_ci==1
replace ocupa_ci=5 if ((OCUPACIO>=511 & OCUPACIO<=516) | (OCUPACIO>=912 & OCUPACIO<=916)) & emp_ci==1
replace ocupa_ci=6 if ((OCUPACIO>=611 & OCUPACIO<=621) | (OCUPACIO>=921 & OCUPACIO<=922)) & emp_ci==1
replace ocupa_ci=7 if ((OCUPACIO>=711 & OCUPACIO<=834) | (OCUPACIO>=931 & OCUPACIO<=933)) & emp_ci==1
replace ocupa_ci=8 if OCUPACIO==11 & emp_ci==1
replace ocupa_ci=. if emp_ci==0 | OCUPACIO==999 | OCUPACIO==0

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (RAMACTV>=11 & RAMACTV<=50) & emp_ci==1
replace rama_ci=2 if (RAMACTV>=101 & RAMACTV<=142) & emp_ci==1
replace rama_ci=3 if (RAMACTV>=151 & RAMACTV<=372) & emp_ci==1
replace rama_ci=4 if (RAMACTV>=401 & RAMACTV<=410) & emp_ci==1
replace rama_ci=5 if (RAMACTV>=451 & RAMACTV<=455) & emp_ci==1
replace rama_ci=6 if (RAMACTV>=501 & RAMACTV<=552) & emp_ci==1 
replace rama_ci=7 if (RAMACTV>=601 & RAMACTV<=642) & emp_ci==1
replace rama_ci=8 if (RAMACTV>=651 & RAMACTV<=749) & emp_ci==1
replace rama_ci=9 if (RAMACTV>=751 & RAMACTV<=990) & emp_ci==1

*****************
***ocupault_ci***
*****************

gen ocupault_ci=.
replace ocupault_ci=1 if (OCUPANT>=211 & OCUPANT<=348) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=2 if (OCUPANT>=111 & OCUPANT<=131) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=3 if (OCUPANT>=411 & OCUPANT<=422) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=4 if (OCUPANT>=521 & OCUPANT<=523) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=5 if (OCUPANT>=511 & OCUPANT<=516) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=6 if (OCUPANT>=611 & OCUPANT<=621) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=7 if (OCUPANT>=711 & OCUPANT<=834) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=8 if (OCUPANT==11) & BUSCO==1 & TRABANTE==1
replace ocupault_ci=9 if (OCUPANT>=911 & OCUPANT<=933) & BUSCO==1 & TRABANTE==1

****************
***ramault_ci***
****************

gen ramault_ci=.
replace ramault_ci=1 if (RAMAANT>=11 & RAMAANT<=50) & BUSCO==1 & TRABANTE==1
replace ramault_ci=2 if (RAMAANT>=101 & RAMAANT<=142) & BUSCO==1 & TRABANTE==1
replace ramault_ci=3 if (RAMAANT>=151 & RAMAANT<=372) & BUSCO==1 & TRABANTE==1
replace ramault_ci=4 if (RAMAANT>=401 & RAMAANT<=410) & BUSCO==1 & TRABANTE==1
replace ramault_ci=5 if (RAMAANT>=451 & RAMAANT<=455) & BUSCO==1 & TRABANTE==1
replace ramault_ci=6 if (RAMAANT>=501 & RAMAANT<=552) & BUSCO==1 & TRABANTE==1 
replace ramault_ci=7 if (RAMAANT>=601 & RAMAANT<=642) & BUSCO==1 & TRABANTE==1
replace ramault_ci=8 if (RAMAANT>=651 & RAMAANT<=749) & BUSCO==1 & TRABANTE==1
replace ramault_ci=9 if (RAMAANT>=751 & RAMAANT<=990) & BUSCO==1 & TRABANTE==1

******************
***ocupault2_ci***
******************

gen ocupault2_ci=.
replace ocupault2_ci=1 if (OCUPANT>=211 & OCUPANT<=348) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=2 if (OCUPANT>=111 & OCUPANT<=131) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=3 if (OCUPANT>=411 & OCUPANT<=422) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=4 if (OCUPANT>=521 & OCUPANT<=523) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=5 if (OCUPANT>=511 & OCUPANT<=516) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=6 if (OCUPANT>=611 & OCUPANT<=621) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=7 if (OCUPANT>=711 & OCUPANT<=834) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=8 if (OCUPANT==11) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ocupault2_ci=9 if (OCUPANT>=911 & OCUPANT<=933) & BUSCO==1 & TRABANTE==1 & desemp_ci==1

*****************
***ramault2_ci***
*****************

gen ramault2_ci=.
replace ramault2_ci=1 if (RAMAANT>=11 & RAMAANT<=50) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=2 if (RAMAANT>=101 & RAMAANT<=142) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=3 if (RAMAANT>=151 & RAMAANT<=372) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=4 if (RAMAANT>=401 & RAMAANT<=410) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=5 if (RAMAANT>=451 & RAMAANT<=455) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=6 if (RAMAANT>=501 & RAMAANT<=552) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=7 if (RAMAANT>=601 & RAMAANT<=642) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=8 if (RAMAANT>=651 & RAMAANT<=749) & BUSCO==1 & TRABANTE==1 & desemp_ci==1
replace ramault2_ci=9 if (RAMAANT>=751 & RAMAANT<=990) & BUSCO==1 & TRABANTE==1 & desemp_ci==1

****************
***durades_ci***
****************
/*Si bien la pregunta está en el cuestionario, no se encuentra en la base de datos*/

gen durades_ci=.

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

1996-Jan	8.76
1996-Feb	8.76
1996-Mar	8.76
1996-Apr	8.76
1996-May	8.76
1996-Jun	8.76
1996-Jul	8.76
1996-Aug	8.76
1996-Sep	8.76
1996-Oct	8.76
1996-Nov	8.76
1996-Dec	8.76
*/

***************
***ylmpri_ci***
***************

/*Para todos los trabajadores empleados*/

gen ylmpri_ci=.
replace ylmpri_ci=INGRESOS*30 if TIPOPAGO==1
replace ylmpri_ci=INGRESOS*4.3 if TIPOPAGO==2
replace ylmpri_ci=INGRESOS*2 if TIPOPAGO==3
replace ylmpri_ci=INGRESOS if TIPOPAGO==4 | TIPOPAGO==8
replace ylmpri_ci=INGRESOS/3 if TIPOPAGO==5
replace ylmpri_ci=INGRESOS/6 if TIPOPAGO==6
replace ylmpri_ci=INGRESOS/12 if TIPOPAGO==7
replace ylmpri_ci=0 if TIPOPAGO==9
replace ylmpri_ci=. if INGRESOS==99999
replace ylmpri_ci=0 if categopri_ci==4 /*Los familiares no remunerados estan con missings*/
replace ylmpri_ci=. if emp_ci==0

*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace ylmpri_ci= ylmpri_ci/8.76 

*replace ylmpri_ci=0 if catego<=2 & ylmpri_ci==. & emp==1 
/*Ojo con esto último. Originalmente la encuesta conputa una serie de 
missings que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/

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

gen ylmsec_ci=YSEC 
replace ylmsec_ci=. if emp==0 | OTROTRAB==2
replace ylmsec_ci=. if YSEC==99999

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

/*Si bien estan estas preguntas en el cuestionario, 
no se encuentran disponibles en la base de datos*/

gen ynlm0_ci=.

gen ynlnm0_ci=.

*Modificación Mayra Sáenz - Septiembre 2014
*gen ynlm_ci= . /*NA, esta variable aparece recién en 1998*/

egen ingrem = rsum(CONSUMO VESTUAR CUOTA REPARA COMER AHORROS MEDICOS EDUCAC INSUMOS OTROS), missing
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

******************
*** remesas_ch *** 
******************

/*No se encuentra disponible en la base de datos*/

*gen remesas_ch=. /*NA, esta variable aparece recién en 1997*/
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1

***************
*** ynlm_ch ***
***************
/*
by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm0_ch=rsum(ynlm remesas_ch)
replace ynlm0_ch=. if ynlm==. & remesas_ch==.
drop ynlm
gen ynlm_ch=.
*/
*gen ynlm_ch=.
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing


****************
*** ynlnm_ch ***
****************

by idh_ch, sort: egen ynlnm0_ch=sum(ynlnm0_ci) if miembros_ci==1
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

gen rentaimp_ch=ALQUILER
replace rentaimp_ch=. if ALQUILER==99999

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

replace aedu_ci=0 if NIVEDU==0 & ESTUDIA==1
replace aedu_ci=0 if NIVEDU==11 |  NIVEDU==12 | NIVEDU==13

replace aedu_ci=1 if  NIVEDU==21
replace aedu_ci=2 if  NIVEDU==22
replace aedu_ci=3 if  NIVEDU==23
replace aedu_ci=4 if  NIVEDU==24
replace aedu_ci=5 if  NIVEDU==25
replace aedu_ci=6 if  NIVEDU==26
replace aedu_ci=7 if  NIVEDU==27
replace aedu_ci=8 if  NIVEDU==28
replace aedu_ci=9 if  NIVEDU==29
replace aedu_ci=10 if  NIVEDU==41
replace aedu_ci=11 if  NIVEDU==42
replace aedu_ci=12 if  NIVEDU==43
replace aedu_ci=12 if  NIVEDU==44
replace aedu_ci=13 if  NIVEDU==51 | NIVEDU==61 
replace aedu_ci=14 if  NIVEDU==52 | NIVEDU==62
replace aedu_ci=15 if  NIVEDU==53 | NIVEDU==63
replace aedu_ci=16 if  NIVEDU==64
replace aedu_ci=17 if  NIVEDU==65
replace aedu_ci=18 if  NIVEDU==66
replace aedu_ci=19 if  NIVEDU==67
replace aedu_ci=20 if  NIVEDU==68
replace aedu_ci=21 if  NIVEDU==69
replace aedu_ci=22 if  NIVEDU==70

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0

/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if ULTGRADO==0 & (HASIST==1 | HASIST==2)
replace aedu_ci=0 if ULTGRADO==12 | ULTGRADO==13

replace aedu_ci=1 if  ULTGRADO==21
replace aedu_ci=2 if  ULTGRADO==22
replace aedu_ci=3 if  ULTGRADO==23
replace aedu_ci=4 if  ULTGRADO==24
replace aedu_ci=5 if  ULTGRADO==25
replace aedu_ci=6 if  ULTGRADO==26
replace aedu_ci=7 if  ULTGRADO==27
replace aedu_ci=8 if  ULTGRADO==28
replace aedu_ci=9 if  ULTGRADO==29
replace aedu_ci=10 if  ULTGRADO==41
replace aedu_ci=11 if  ULTGRADO==42
replace aedu_ci=12 if  ULTGRADO==43
replace aedu_ci=12 if  ULTGRADO==44
replace aedu_ci=13 if  ULTGRADO==51 | ULTGRADO==61 
replace aedu_ci=14 if  ULTGRADO==52 | ULTGRADO==62
replace aedu_ci=15 if  ULTGRADO==53 | ULTGRADO==63
replace aedu_ci=16 if  ULTGRADO==64
replace aedu_ci=17 if  ULTGRADO==65
replace aedu_ci=18 if  ULTGRADO==66
replace aedu_ci=19 if  ULTGRADO==67
replace aedu_ci=20 if  ULTGRADO==68
replace aedu_ci=21 if  ULTGRADO==69
replace aedu_ci=22 if  ULTGRADO==70

replace aedu_ci=0 if ESTUDIA==2 & HASIST==2
replace aedu_ci=. if aedu_ci>edad_ci & aedu_ci~=. 


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
replace edupre_ci=1 if ULTGRADO==11 | ULTGRADO==12 | ULTGRADO==13 | NIVEDU==11 | NIVEDU==12 | NIVEDU==13 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (NIVEDU>=61 & NIVEDU<=69) | (ULTGRADO>=61 & ULTGRADO<=69) & aedu_ci~=.
replace eduac_ci=0 if (NIVEDU>=51 & NIVEDU<=53) | (ULTGRADO>=51 & ULTGRADO<=53) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if ESTUDIA==1
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=PQNOESTU
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 
label define pqnoasis_ci 2 "Muy caro" 3 "Muy lejos" 4 "No hay profesor", add 
label define pqnoasis_ci 5 "Cerro el centro" 6 "Repitio mucho" , add
label define pqnoasis_ci 7 "No vale la pena" 8 "Por la EDAD" 9 " No hay escuela nocturna", add
label define pqnoasis_ci 10 " Finalizo sus estudios"  11 " Causas del hogar" 12 " No existe otro grado" 13 "Otros" , add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if PQNOESTU ==2
replace pqnoasis1_ci = 2 if PQNOESTU ==1
replace pqnoasis1_ci = 4 if PQNOESTU ==7
replace pqnoasis1_ci = 5 if PQNOESTU ==11
replace pqnoasis1_ci = 6 if PQNOESTU ==10
replace pqnoasis1_ci = 7 if PQNOESTU ==8
replace pqnoasis1_ci = 8 if PQNOESTU ==3  | PQNOESTU ==4  | PQNOESTU ==5  | PQNOESTU ==9 | PQNOESTU ==12 
replace pqnoasis1_ci = 9 if PQNOESTU ==6  | PQNOESTU ==13

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

gen repiteult_ci=(REPITE==1)
replace repiteult_ci=. if asiste_ci==0
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if CENTROEN==1 
replace edupub_ci=2 if CENTROEN==2 | CENTROEN==3

/* Variable CENTROEN:
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

gen aguared_ch=(SERVAGUA==1 | SERVAGUA==2)

gen aguadist_ch=1 if SERVAGUA==1
replace aguadist_ch=2 if SERVAGUA==2
replace aguadist_ch=3 if ABASAGUA>=1 & ABASAGUA<=8

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(SERVALUM==1 | SERVALUM==2)

gen luzmide_ch=.
/*NA*/

gen combust_ch=(SERVALUM>=1 & SERVALUM<=4)

gen bano_ch=((SERVSANI>=1 & SERVSANI<=4) | (NOTIENE>=1 & NOTIENE<=3))

gen banoex_ch=(NOTIENE>=1 & NOTIENE<=3)

* MGR Jul, 2015: variable había sido generada como missing, construyo variable de la misma manera que los otros años
gen des1_ch=.
replace des1_ch = 0 if SERVSANI==5
replace des1_ch = 1 if SERVSANI>=1 & SERVSANI<=2
replace des1_ch = 2 if SERVSANI>=3 & SERVSANI<=4

* MGR Jul, 2015: variable había sido generada como missing, construyo variable de la misma manera que los otros años
gen des2_ch=.
replace des2_ch = 0 if SERVSANI==5
replace des2_ch = 1 if des1_ch==1 | des1_ch==2

gen piso_ch=0 if PISO==5
replace piso_ch=1 if PISO>=1 & PISO<=4

gen pared_ch=0 if PAREDES==2 | PAREDES==3 | PAREDES==6 | PAREDES==7
replace pared_ch=1 if PAREDES==1 | PAREDES==4 | PAREDES==5

gen techo_ch=0 if TECHO==5 | TECHO==6
replace techo_ch=1 if TECHO>=1 & TECHO<=4

gen resid_ch=0 if BASURA==1 | BASURA==2
replace resid_ch=1 if BASURA==4 | BASURA==5
replace resid_ch=2 if BASURA==6
replace resid_ch=3 if BASURA==3 | BASURA==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (SERVAGUA >=1 & SERVAGUA <=2) | ABASAGUA == 1 | ABASAGUA == 4 | ABASAGUA == 8
replace aguamejorada_ch = 0 if (SERVAGUA >=3 & SERVAGUA <=4) | ABASAGUA == 2 | ABASAGUA == 3 | (ABASAGUA >=5 & ABASAGUA <=7) | ABASAGUA == 9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (SERVSANI >=1 & SERVSANI <=4)
replace banomejorado_ch = 0 if  SERVBANO ==3 | SERVSANI ==5

gen dorm_ch=NRODORM
replace dorm_ch=. if NRODORM==9

gen cuartos_ch=NROHABIT

gen cocina_ch=.
/*NA*/

gen telef_ch=(TELEFONO==1)
replace telef_ch=. if TELEFONO==9

gen refrig_ch=(REFRI==1)
replace refrig_ch=. if REFRI==9

gen freez_ch=.
/*NA*/

gen auto_ch=(VEHICULO==1)
replace auto_ch=. if VEHICULO==9


gen compu_ch=(COMPUTAD==1)
replace compu_ch=. if COMPUTAD==9

gen internet_ch=.
/*NA*/

gen cel_ch=.
/*NA*/

gen vivi1_ch=1 if TIPOVIV==1
replace vivi1_ch=2 if TIPOVIV>=2 & TIPOVIV<=4
replace vivi1_ch=3 if TIPOVIV>4 & TIPOVIV<9

gen vivi2_ch=(TIPOVIV>=1 & TIPOVIV<=4)
replace vivi2_ch=. if TIPOVIV==9

gen viviprop_ch=0 if TENENCIA==3
replace viviprop_ch=1 if TENENCIA==1
replace viviprop_ch=2 if TENENCIA==2
replace viviprop_ch=3 if TENENCIA>=4 & TENENCIA<=6


gen viviitit_ch=.
/*NA*/

gen vivialq_ch=CUOMES if TENENCIA==3
replace vivialq_ch=. if CUOMES==99999
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace vivialq_ch= vivialq_ch/8.76 



gen vivialqimp_ch=ALQUILER
replace vivialqimp_ch=. if ALQUILER==99999
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión de colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76 


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
replace cotizando_ci=1 if (SEGSOCI==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
/*En este caso no se le pregunta a los que trabajan como servicio domestico.
Y estos son missings */

gen tamemp_ci=1 if TAMEST==1 | TAMEST==2
replace tamemp_ci=2 if TAMEST==3 & TAMEST!=.
replace tamemp_ci=3 if TAMEST==4 & TAMEST!=.
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
replace cesante_ci=1 if TRABANTE==1 & condocup_ci==2
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
gen tecnica_ci=((NIVEDU>=51 & NIVEDU<=53) | (ULTGRADO>=51 & ULTGRADO<=53))
label var tecnica_ci "=1 formacion terciaria tecnica"	


*****************
**categoinac_ci**
*****************	
ren PQNOBUS r407
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



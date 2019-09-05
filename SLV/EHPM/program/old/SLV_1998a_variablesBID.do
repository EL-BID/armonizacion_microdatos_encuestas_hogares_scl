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
local ANO "1998"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
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

gen factor_ch=factori
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

gen anio_c=1998
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

gen factor_ci=factord
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
replace condocup_ci=2 if (ult30dia>=1 & ult30dia<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10*/
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
* Corregido inactivos y menores. MGD 06/10/2014
/*gen condocup_ci=.
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact==2 | condact==3
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/

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
/*
************
***emp_ci***
************

gen byte emp_ci=0
replace emp_ci=1 if trabajo==1
replace emp_ci=1 if trabajo==2 & tienetra==1 

/*Es la variable empfijo, pero se importó con otro nombre*/
/*Se podría considerar empleado a aquellos que contestan la pregunta 405 con un 1
sin embargo a este individuo no se le sigue preguntando como si fuera empleado
sino como si fuera desempleado, así que lo consideramos desempleado*/

****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 & busco==1)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 & busco==1 | (busco==2 & pqnobus==1))

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
*/

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & busco==2 & (pqnobus==3 | pqnobus==5))


*****************
***horaspri_ci***
*****************

destring hrshab, generate (hrshabi)

gen horaspri_ci=horastra if trabajo==1
replace horaspri_ci=hrshabi if trabajo==2 & tienetra==1

replace horaspri_ci=. if horastra==99
replace horaspri_ci=. if hrshabi==99
replace horaspri_ci=. if emp_ci==0

*****************
***horastot_ci***
*****************

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

gen categopri_ci=.
replace categopri_ci=1 if categ==1
replace categopri_ci=2 if categ==2 | categ==3  |categ==4 
replace categopri_ci=3 if categ==6 | categ==7 |categ==8 | categ==9
replace categopri_ci=4 if categ==5
replace categopri_ci=0 if categ==10

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

gen contrato_ci=.
replace contrato_ci=1 if contrato==1
replace contrato_ci=0 if contrato==2

/*Esta pregunta se le hace solamente a los asalariados, aprendices u otros
de la pregunta 417, de manera que no incluye a todos los empleados como se define
categopri_ci==3*/


***************
***segsoc_ci***
***************

gen segsoc_ci=.
replace segsoc_ci=1 if segsoc==1 
replace segsoc_ci=0 if segsoc==3 | segsoc==2

/*Consideramos con seguridad social a los que son afiliados*/
/*En este caso no se le pregunta a los que trabajan como servicio domestico*/
*/
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
*/
/*Debido a que en este año en la categoria de empleados hay muchos missings que hacen que la media
para formal sea muy baja comparando con el resto de los años y para no crear confusiones
se opta por no crear esta variable para 1998*/
/*En este caso no se le pregunta a los que trabajan como servicio domestico.
Y estos deben ser missings */

*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if sectorp==2
replace spublico_ci=0 if sectorp==1
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | categ==9 | categ==4
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
gen sem=.
replace sem=semanasbus/4.3 if semanasbus>0 & semanasbus<9 /*La variable debe llevarse a meses*/
drop meses
gen meses=.
replace meses=mesesbus if mesesbus>0 & mesesbus<99
gen anosb=.
replace anosb=anosbus/12 if anosbus>0 & anosbus<99 /*La variable debe llevarse a meses*/

egen durades_ci=rsum(sem meses anosb)
replace durades_ci=. if sem==. & meses==. & anosb==.
replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************
* Modificacion MGD 07/10/2014: para este anio hay la variables de meses y anios trabajados sin interrupciones.
recode anostra (99=.)
recode mesestra (-1=.)
gen antiguedad_ci=anostra if anostra>=1 & anostra!=.
replace antiguedad_ci=mesestra/12 if anostra<1 & mesestra!=.


*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************
*Modificación Mayra Sáenz - Septiembre 2014
*Conversión Colones a dólares
/*
Fuente: http://www.iadb.org/en/research-and-data/latin-american-and-caribbean-macro-watch,8633.html
1998-Jan	8.76
1998-Feb	8.76
1998-Mar	8.76
1998-Apr	8.76
1998-May	8.76
1998-Jun	8.76
1998-Jul	8.76
1998-Aug	8.76
1998-Sep	8.76
1998-Oct	8.76
1998-Nov	8.76
1998-Dec	8.76
*/


****************************
***ylmpri_ci & ylmpri1_ci***
****************************

/*Para los trabajadores dependientes*/

gen yprid=.
replace yprid=ingresos*30 if tipopago==1
replace yprid=ingresos*4.3 if tipopago==2
replace yprid=ingresos*2 if tipopago==3
replace yprid=ingresos if tipopago==4 | tipopago==5
replace yprid=0 if tipopago==6
replace yprid=999999 if ingresos==999999

gen hrsextrasd=.
replace hrsextrasd=hrextcol*frhrextcol/12
replace hrsextrasd=999999 if hrextcol==999999 | frhrextcol==999

gen vacacionesd=.
replace vacacionesd=savaccol*frsavaccol/12
replace vacacionesd=999999 if savaccol==999999 | frsavaccol==999

gen aguinaldod=.
replace aguinaldod=aguincol*fraguincol/12
replace aguinaldod=999999 if aguincol==999999 | fraguincol==999

gen bonificacionesd=.
replace bonificacionesd=bonoscol*frbonoscol/12
replace bonificacionesd=999999 if bonoscol==999999 | frbonoscol==999

egen yprijbd=rsum( yprid hrsextrasd vacacionesd aguinaldod bonificacionesd)
replace yprijbd=999999 if yprid==999999 | hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999 
replace yprijbd=yprid if yprid>0 & yprid~=999999 & ( hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. & hrsextrasd==. & vacacionesd==. & aguinaldod==. & bonificacionesd==. 

/*Para los trabajadores independientes*/

gen ingrneto=ingrbru-costos
replace ingrneto=0 if ingrneto<0
replace ingrneto=999999 if ingrbru==999999 | costos==999999

gen yprijbi=.
replace yprijbi=ingrneto*30 if frecingr==1
replace yprijbi=ingrneto*4.3 if frecingr==2
replace yprijbi=ingrneto*2 if frecingr==3
replace yprijbi=ingrneto if frecingr==4 | frecingr==6
replace yprijbi=ingrneto/12 if frecingr==5
replace yprijbi=999999 if ingrneto>=969999 | frecingr==9
replace yprijbi=. if categopri_ci>2


*replace yprijbi=0 if catego<=2 & yprijbi==. & emp==1 
/*Ojo con esto último. Originalmente la encuesta conputa una serie de 
missings que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/


egen ylmpri_ci=rsum(yprijbi yprid)
replace ylmpri_ci=. if yprijbi==999999 | yprid==999999
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp==0
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylmpri_ci= ylmpri_ci/8.76


egen ylmpri1_ci=rsum(yprijbi yprijbd)
replace ylmpri1_ci=. if yprijbi==999999 | yprijbd==999999
replace ylmpri1_ci=. if yprijbd==. & yprijbi==.
replace ylmpri1_ci=. if emp==0

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=(ylmpri1_ci==. & emp_ci==1)

*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

gen food1=.
replace food1=foodcol*frfoodcol/12 
replace food1=0 if emp==1 & foodcol==.
replace food1=. if foodcol==999999 | frfoodcol==999

gen ropa1=.
replace ropa1=ropacol*frbropacol/12 
replace ropa1=0 if emp==1 & ropacol==.
replace ropa1=. if ropacol==999999 | frbropacol==999

gen merca1=.
replace merca1=mercacol*frmercacol/12 
replace merca1=0 if emp==1 & mercacol==.
replace merca1=. if mercacol==999999 | frmercacol==999

gen vivi1=.
replace vivi1=vivicol*frvivicol/12 
replace vivi1=0 if emp==1 & vivicol==.
replace vivi1=. if vivicol==999999 | frvivicol==999

gen trans1=.
replace trans1=transcol*frtranscol/12 
replace trans1=0 if emp==1 & transcol==.
replace trans1=. if transcol==999999 | frtranscol==999

gen segur1=.
replace segur1=segurcol*frsegurcol/12 
replace segur1=0 if emp==1 & segurcol==.
replace segur1=. if segurcol==999999 | frsegurcol==999

gen otross1=.
replace otross1=otrosscol*frmotroscol/12 
replace otross1=0 if emp==1 & otrosscol==.
replace otross1=. if otrosscol==999999 | frmotroscol==999

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1)
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylnmpri_ci= ylnmpri_ci/8.76


gen ylnmpri1_ci=.
/*No hay autoconsumo en esta encuesta*/

***************
***ylmsec_ci***
***************

gen ysec1=ysec
replace ysec1=. if ysec==999999

gen hrsextras=.
replace hrsextras=hrxtraco*frhrxtraco/12
replace hrsextras=. if hrxtraco==999999 | frhrxtraco==999

gen vacaciones=.
replace vacaciones=salvaco*frsalvaco/12
replace vacaciones=. if salvaco==999999 | frsalvaco==999

gen aguinaldo=.
replace aguinaldo=aguinco*fraguinco/12
replace aguinaldo=. if aguinco==999999 | fraguinco==999

gen bonificaciones=.
replace bonificaciones=bonosco*frbonosco/12
replace bonificaciones=. if bonosco==999999 | frbonosco==999

gen ylmsec_ci=ysec1

egen ylmsec1_ci=rsum(ysec1 hrsextras vacaciones aguinaldo bonificaciones)
replace ylmsec1_ci=. if ysec1==. & hrsextras==. & vacaciones==. & aguinaldo==. & bonificaciones==. 
replace ylmsec1_ci=. if emp_ci==0 | otrotrab==2

*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ylmsec_ci= ylmsec_ci/8.76

******************
****ylnmsec_ci****
******************

gen food2=.
replace food2=foodco*frfoodco/12 
replace food2=0 if emp==1 & foodco==.
replace food2=. if foodco==999999 | frfoodco==999

gen ropa2=.
replace ropa2=ropaco*frbropaco/12 
replace ropa2=0 if emp==1 & ropaco==.
replace ropa2=. if ropaco==999999 | frbropaco==999

gen merca2=.
replace merca2=mercaco*frmercaco/12 
replace merca2=0 if emp==1 & mercaco==.
replace merca2=. if mercaco==999999 | frmercaco==999

gen vivi2=.
replace vivi2=vivico*frvivico/12 
replace vivi2=0 if emp==1 & vivico==.
replace vivi2=. if vivico==999999 | frvivico==999

gen trans2=.
replace trans2=transco*frtransco/12 
replace trans2=0 if emp==1 & transco==.
replace trans2=. if transco==999999 | frtransco==999

gen segur2=.
replace segur2=segurco*frsegurco/12 
replace segur2=0 if emp==1 & segurco==.
replace segur2=. if segurco==999999 | frsegurco==999

gen otross2=.
replace otross2=otrossco*frmotrosco/12 
replace otross2=0 if emp==1 & otrossco==.
replace otross2=. if otrossco==999999 | frmotrosco==999

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2)
replace ylnmsec_ci=. if food2==. &  ropa2==. & merca2==. & vivi2==. & trans2==. & segur2==. & otross2==. 
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

gen ylnm1_ci=.
/*Como no ahy autoconsumo no tenemos este bloque*/


*************
***ynlm_ci***
*************

gen remesasext=.
replace remesasext=remescol*fqremescol/12 
replace remesasext=0 if emp==1 & remescol==.
replace remesasext=. if remescol==999999 | fqremescol==999

gen ayuda=.
replace ayuda=ayudacol*fqayudacol/12
replace ayuda=0 if emp==1 & ayudacol==.
replace ayuda=. if ayudacol==999999 | fqayudacol==999

gen cuotalim=.
replace cuotalim=qotalcol*fqqotalcol/12
replace cuotalim=0 if emp==1 & qotalcol==.
replace cuotalim=. if qotalcol==999999 | fqqotalcol==999

gen alqui=.
replace alqui=alquicol*fqalquicol/12
replace alqui=0 if emp==1 & alquicol==.
replace alqui=. if alquicol==999999 | fqalquicol==999

cap drop alqneg
gen alqneg=.
replace alqneg=alqnegco*fqalqnegco/12
replace alqneg=0 if emp==1 & alqnegco==.
replace alqneg=. if alqnegco==999999 | fqalqnegco==999

gen alqter=.
replace alqter=alqterco*fqalqterco/12
replace alqter=0 if emp==1 & alqterco==.
replace alqter=. if alqterco==999999 | fqalqterco==999

cap drop jubil
gen jubil=.
replace jubil=jubilcol*fqjubilcol/12
replace jubil=0 if emp==1 & jubilcol==.
replace jubil=. if jubilcol==999999 | fqjubilcol==999

gen deveh=.
replace deveh=devehcol*fqdevehcol/12
replace deveh=0 if emp==1 & devehcol==.
replace deveh=. if devehcol==999999 | fqdevehcol==999

rename otros otros1
gen otros=.
replace otros=otroscol*fqotroscol/12
replace otros=0 if emp==1 & otroscol==.
replace otros=. if otroscol==999999 | fqotroscol==999

gen utilidades=.
replace utilidades=utilcol/12
replace utilidades=0 if emp==1 & utilcol==.
replace utilidades=. if utilcol==999999

gen dividendos=.
replace dividendos=divaccol/12
replace dividendos=0 if emp==1 & divaccol==.
replace dividendos=. if divaccol==999999

gen intereses=.
replace intereses=intercol/12
replace intereses=0 if emp==1 & intercol==.
replace intereses=. if intercol==999999

gen herencias=.
replace herencias=herencol/12
replace herencias=0 if emp==1 & herencol==.
replace herencias=. if herencol==999999

gen vtain=.
replace vtain=vtainmue/12
replace vtain=0 if emp==1 & vtainmue==.
replace vtain=. if vtainmue==999999

gen indemnizacion=.
replace indemnizacion=indemcol/12
replace indemnizacion=0 if emp==1 & indemcol==.
replace indemnizacion=. if indemcol==999999

gen ayudagob=.
replace ayudagob=aygobcol/12
replace ayudagob=0 if emp==1 & aygobcol==.
replace ayudagob=. if aygobcol==999999

gen otross=.
replace otross=otroscol443/12
replace otross=0 if emp==1 & otroscol==.
replace otross=. if otroscol443==999999

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg alqter jubil deveh otros utilidades dividendos intereses herencias vtain indemnizacion ayudagob otross)
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & alqter==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & vtain==. & indemnizacion==. & ayudagob==.  & otross==. 
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace ynlm_ci= ynlm_ci/8.76

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=remesasext
replace remesas_ci=. if remesasext==0

*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace remesas_ci= remesas_ci/8.76
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
gen ylnm1_ch=.

******************
*** remesas_ch ***
******************

gen remesash=.
replace remesash=cantida/12 if frecuna==1
replace remesash=cantida/6 if frecuna==2
replace remesash=cantida/3 if frecuna==3
replace remesash=. if frecuna==4
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

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=.

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=alquiler
replace rentaimp_ch=. if alquiler==99999
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

replace aedu_ci=0 if nivelcursa==0 & estudia==1
replace aedu_ci=0 if nivelcursa==1 | nivelcursa==3

replace aedu_ci=1 if nivelcursa==2 & nivedu==1
replace aedu_ci=2 if nivelcursa==2 & nivedu==2
replace aedu_ci=3 if nivelcursa==2 & nivedu==3
replace aedu_ci=4 if nivelcursa==2 & nivedu==4
replace aedu_ci=5 if nivelcursa==2 & nivedu==5
replace aedu_ci=6 if nivelcursa==2 & nivedu==6
replace aedu_ci=7 if nivelcursa==2 & nivedu==7
replace aedu_ci=8 if nivelcursa==2 & nivedu==8
replace aedu_ci=9 if nivelcursa==2 & nivedu==9

replace aedu_ci=1 if nivelcursa==4 & nivedu==1
replace aedu_ci=2 if nivelcursa==4 & nivedu==2
replace aedu_ci=3 if nivelcursa==4 & nivedu==3
replace aedu_ci=4 if nivelcursa==4 & nivedu==4
replace aedu_ci=5 if nivelcursa==4 & nivedu==5
replace aedu_ci=6 if nivelcursa==4 & nivedu==6
replace aedu_ci=7 if nivelcursa==4 & nivedu==7
replace aedu_ci=8 if nivelcursa==4 & nivedu==8
replace aedu_ci=9 if nivelcursa==4 & nivedu==9

replace aedu_ci=10 if nivelcursa==5 & nivedu==10
replace aedu_ci=11 if nivelcursa==5 & nivedu==11
replace aedu_ci=12 if nivelcursa==5 & nivedu==12

replace aedu_ci=13 if nivelcursa==6 & nivedu==1
replace aedu_ci=14 if nivelcursa==6 & nivedu==2
replace aedu_ci=15 if nivelcursa==6 & nivedu==3
replace aedu_ci=16 if nivelcursa==6 & nivedu==4
replace aedu_ci=17 if nivelcursa==6 & nivedu==5
replace aedu_ci=18 if nivelcursa==6 & nivedu==6
replace aedu_ci=19 if nivelcursa==6 & nivedu==7
replace aedu_ci=23 if nivelcursa==6 & nivedu==11
replace aedu_ci=24 if nivelcursa==6 & nivedu==12

replace aedu_ci=13 if nivelcursa==7 & nivedu==1
replace aedu_ci=14 if nivelcursa==7 & nivedu==2
replace aedu_ci=15 if nivelcursa==7 & nivedu==3
replace aedu_ci=16 if nivelcursa==7 & nivedu==4


/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if ultnivel==0 & (hasist==1 | hasist==2)
replace aedu_ci=0 if ultnivel==1 | ultnivel==3

replace aedu_ci=1 if ultnivel==2 & ultgrado==1
replace aedu_ci=2 if ultnivel==2 & ultgrado==2
replace aedu_ci=3 if ultnivel==2 & ultgrado==3
replace aedu_ci=4 if ultnivel==2 & ultgrado==4
replace aedu_ci=5 if ultnivel==2 & ultgrado==5
replace aedu_ci=6 if ultnivel==2 & ultgrado==6
replace aedu_ci=7 if ultnivel==2 & ultgrado==7
replace aedu_ci=8 if ultnivel==2 & ultgrado==8
replace aedu_ci=9 if ultnivel==2 & ultgrado==9

replace aedu_ci=1 if ultnivel==4 & ultgrado==1
replace aedu_ci=2 if ultnivel==4 & ultgrado==2
replace aedu_ci=3 if ultnivel==4 & ultgrado==3
replace aedu_ci=4 if ultnivel==4 & ultgrado==4
replace aedu_ci=5 if ultnivel==4 & ultgrado==5
replace aedu_ci=6 if ultnivel==4 & ultgrado==6
replace aedu_ci=7 if ultnivel==4 & ultgrado==7
replace aedu_ci=8 if ultnivel==4 & ultgrado==8
replace aedu_ci=9 if ultnivel==4 & ultgrado==9

replace aedu_ci=10 if ultnivel==5 & ultgrado==10
replace aedu_ci=11 if ultnivel==5 & ultgrado==11
replace aedu_ci=12 if ultnivel==5 & ultgrado==12
replace aedu_ci=12 if ultnivel==5 & ultgrado==13

replace aedu_ci=13 if ultnivel==6 & ultgrado==1
replace aedu_ci=14 if ultnivel==6 & ultgrado==2
replace aedu_ci=15 if ultnivel==6 & ultgrado==3
replace aedu_ci=16 if ultnivel==6 & ultgrado==4
replace aedu_ci=17 if ultnivel==6 & ultgrado==5
replace aedu_ci=18 if ultnivel==6 & ultgrado==6
replace aedu_ci=19 if ultnivel==6 & ultgrado==7
replace aedu_ci=23 if ultnivel==6 & ultgrado==10
replace aedu_ci=24 if ultnivel==6 & ultgrado==12

replace aedu_ci=13 if ultnivel==7 & ultgrado==1
replace aedu_ci=14 if ultnivel==7 & ultgrado==2
replace aedu_ci=15 if ultnivel==7 & ultgrado==3
replace aedu_ci=16 if ultnivel==7 & ultgrado==4
replace aedu_ci=17 if ultnivel==7 & ultgrado==5

replace aedu_ci=0 if estudia==2 & hasist==2
replace aedu_ci=. if edad<=3
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
replace edupre_ci=1 if ultnivel==1 | nivelcursa==1 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (ultnivel==6 | nivelcursa==6) & aedu_ci~=.
replace eduac_ci=0 if (ultnivel==7 | nivelcursa==7) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if estudia==1
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=pqnoestu
replace pqnoasis_ci=. if asiste_ci==1
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 "Muy lejos" 5 " No existe otro grado" 6 "No vale la pena" 7 " Enfermedad", add 
label define pqnoasis_ci 8 "Embarazo" 9 "Finalizo sus estudios" 10 "No hay escuela nocturna" 11 "Por invalidez" 12 "Requiere educacion especial", add
label define pqnoasis_ci 13 "Los padres no quieren" 14 "Otros" 99 "No responde", add
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
replace aguadist_ch=3 if abasagua>=1 & abasagua<=10

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(servalum==1 | servalum==2)
replace luz_ch=. if servalum==9

gen luzmide_ch=.
/*NA*/

gen combust_ch=(servalum>=1 & servalum<=4)
replace combust_ch=. if servalum==9

gen bano_ch=((servsani>=1 & servsani<=5) | (notiene>=1 & notiene<=3))

gen banoex_ch=(notiene>=1 & notiene<=3)

gen des1_ch=.
/*NA*/

gen des2_ch=.
/*NA*/

gen piso_ch=0 if piso==5
replace piso_ch=1 if piso>=1 & piso<=4

gen pared_ch=0 if paredes==2 | paredes==3 | paredes==6 | paredes==7
replace pared_ch=1 if paredes==1 | paredes==4 | paredes==5
replace pared_ch=2 if paredes==8

gen techo_ch=0 if techo==5 | techo==6
replace techo_ch=1 if techo>=1 & techo<=4

gen resid_ch=0 if basura==1 | basura==2
replace resid_ch=1 if basura==4 | basura==5
replace resid_ch=2 if basura==6
replace resid_ch=3 if basura==3 | basura==7

gen dorm_ch=nrodorm
replace dorm_ch=. if nrodorm==9

gen cuartos_ch=nrohabit
replace cuartos_ch=. if nrohabit==99

gen cocina_ch=.
/*NA*/

gen telef_ch=(telefono==1 | telefono==2 | telefono==3)
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

gen cel_ch=(telefono==2 | telefono==3)
replace cel_ch=. if telefono==9


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
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace vivialq_ch= vivialq_ch/8.76


gen vivialqimp_ch=alquiler
replace vivialqimp_ch=. if alquiler==99999
*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76

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
/*gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (segsoc==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"*/

* Nota MGD 09/23/2014: se utiliza la variable segsoci en lugar de segsos que esta incompleta.

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
* Incorporando variable de temporalidad en asalariados y firma de contrato. MGD 06/16/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (contrato==1 & categ==6) & categopri_ci==3
replace tipocontrato_ci=2 if (contrato==1 & categ==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (contrato==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=1 if tamest>=0 & tamest<=5
replace tamemp_ci=2 if tamest>=6 & tamest<=50
replace tamemp_ci=3 if tamest>50 & tamest!=.

label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci



*************
**pension_ci*
*************
gen pension_ci=(jubilcol>=1 & jubilcol<999999)
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

gen ypen_ci=jubilcol*fqjubilcol/12 if pension_ci==1
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
gen lp25_ci = 34.4714
label var lp25_ci "Linea de pobreza 2.5 dólares, año base 2005"

*********
*lp4_ci***
*********
gen lp4_ci =55.15424
label var lp4_ci "Linea de pobreza 4 dólares, año base 2005"

*************
**salmm_ci***
*************
gen salmm_ci= .
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
gen tecnica_ci=(nivel==7 | ultnivel==7)
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

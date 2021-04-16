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
local ANO "1997"
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
/*
foreach v of varlist _all {
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

gen anio_c=1997
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

gen factor_ci=FACTOREX 
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
*Modif. MGD 06/10/2014
/*gen condocup_ci=.
* modifica MLO: 2014,01
replace condocup_ci=1 if CONDACT==1
replace condocup_ci=2 if CONDACT==2 | CONDACT==3
*replace condocup_ci=3 if CONDACT>=4 & CONDACT<=11
*replace condocup_ci=4 if CONDACT==0
replace condocup_ci=4 if CONDACT==0
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2 & condocup_ci!=4) & edad_ci>=10

/*old version
gen condocup_ci=.
replace condocup_ci=1 if CONDACT==1
replace condocup_ci=2 if busco==1 & (ult30dia>=1 & ult30dia<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10*/
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"*/

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
gen pea3_ci=.

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & busco==2 & (pqnobus==5))*/


*****************
***horaspri_ci***
*****************

gen horaspri_ci=horastra if trabajo==1
replace horaspri_ci=hrshab if trabajo==2 & tienetra==1

replace horaspri_ci=. if horastra==99
replace horaspri_ci=. if hrshab==99
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
replace categopri_ci=2 if categ==2 | categ==4 
replace categopri_ci=3 if categ==5 |categ==6 | categ==7 |categ==8 
replace categopri_ci=4 if categ==3
replace categopri_ci=0 if categ==9

label define categopri_ci 0 "Otro" 1"Patron" 2"Cuenta propia" 
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
replace firmapeq_ci=1 if 
<5
replace firmapeq_ci=0 if tamest>=5 & tamest<99999
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
gen sem=.
replace sem=tpobuss/4.3 if tpobuss>0 & tpobuss<9 /*La variable debe llevarse a meses*/
gen mesess=.
replace mesess=tpobusm if tpobusm>0 & tpobusm<99
gen anosb=.
replace anosb=tpobusa/12 if tpobusa>0 & tpobusa<99 /*La variable debe llevarse a meses*/

egen durades_ci=rsum(sem mesess anosb)
replace durades_ci=. if sem==. & mesess==. & anosb==.
replace durades_ci=. if emp==1

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

1997-Jan	8.76
1997-Feb	8.76
1997-Mar	8.76
1997-Apr	8.76
1997-May	8.76
1997-Jun	8.76
1997-Jul	8.76
1997-Aug	8.76
1997-Sep	8.76
1997-Oct	8.76
1997-Nov	8.76
1997-Dec	8.76

*/



gen bonif=.
replace bonif=bonifici*bon/12
replace bonif=0 if emp==1 & bonifici==.
replace bonif=. if bonifici==999999 | bon==999

gen comisiones=.
replace comisiones=comision*comi/12
replace comisiones=0 if emp==1 & comision==.
replace comisiones=. if comision==999999 | comi==999

gen dietass=.
replace dietass=dietas*die/12
replace dietass=0 if emp==1 & dietas==.
replace dietass=. if dietas==999999 | die==999

gen viaticoss=.
replace viaticoss=viaticos*viat/12
replace viaticoss=0 if emp==1 & viaticos==.
replace viaticoss=. if viaticos==999999 | viat==999

gen especiess=.
replace especiess=especies*espe/12
replace especiess=0 if emp==1 & especies==.
replace especiess=. if especies==999999 | espe==999

gen deveh=.
replace deveh=depreci*depre/12
replace deveh=0 if emp==1 & depreci==.
replace deveh=. if depreci==999999 | depre==999

gen combuss=.
replace combuss=combus*comb/12
replace combuss=0 if emp==1 & combus==.
replace combuss=. if combus==999999 | comb==999

gen jubilacion=.
replace jubilacion=jubila*jubil/12
replace jubilacion=0 if emp==1 & jubila==.
replace jubilacion=. if jubila==999999 | jubil==999

gen acteventual=.
replace acteventual=eventua*event/12
replace acteventual=0 if emp==1 & eventua==.
replace acteventual=. if eventua==999999 | event==999

gen cuotalim=.
replace cuotalim=alimen*ali/12
replace cuotalim=0 if emp==1 & alimen==.
replace cuotalim=. if alimen==999999 | ali==999

gen alquileres=.
replace alquileres=alquil*alqui/12
replace alquileres=0 if emp==1 & alquil==.
replace alquileres=. if alquil==999999 | alqui==999

gen alqneg=.
replace alqneg=negocio*neg/12
replace alqneg=0 if emp==1 & negocio==.
replace alqneg=. if negocio==999999 | neg==999

gen arrendamiento=.
replace arrendamiento=terreno*terr/12
replace arrendamiento=0 if emp==1 & terreno==.
replace arrendamiento=. if terreno==999999 | terr==999

rename ayuda fqayudacol
gen ayuda=.
replace ayuda=ayudapa*fqayudacol/12
replace ayuda=0 if emp==1 & ayudapa==.
replace ayuda=. if ayudapa==999999 | fqayudacol==999

gen otross=.
replace otross=otros*otr/12
replace otross=0 if emp==1 & otros==.
replace otross=. if otros==999999 | otr==999

gen aguinals=.
replace aguinals=aguinal/12
replace aguinals=0 if emp==1 & aguinal==.
replace aguinals=. if aguinal==999999 

gen dividendos=.
replace dividendos=dividen/12
replace dividendos=0 if emp==1 & dividen==.
replace dividendos=. if dividen==999999

gen bonificaciones=.
replace bonificaciones=bonifica/12
replace bonificaciones=0 if emp==1 & bonifica==.
replace bonificaciones=. if bonifica==999999

gen depauto=.
replace depauto=deprecia/12
replace depauto=0 if emp==1 & deprecia==.
replace depauto=. if deprecia==999999

gen vtainm=.
replace vtainm=vtabien/12
replace vtainm=0 if emp==1 & vtabien==.
replace vtainm=. if vtabien==999999

***************
***ylmpri_ci***
***************

/*Para todos los trabajadores empleados*/

gen ylmpri_ci=.
replace ylmpri_ci=ingresos*30 if tipopago==1
replace ylmpri_ci=ingresos*4.3 if tipopago==2
replace ylmpri_ci=ingresos*2 if tipopago==3
replace ylmpri_ci=ingresos if tipopago==4 | tipopago==8
replace ylmpri_ci=ingresos/3 if tipopago==5
replace ylmpri_ci=ingresos/6 if tipopago==6
replace ylmpri_ci=ingresos/12 if tipopago==7
replace ylmpri_ci=0 if tipopago==9
replace ylmpri_ci=. if ingresos==999999
replace ylmpri_ci=. if emp==0


*Modificación Mayra Sáenz Septiembre 2014 y se realiza la conversión de colones a dólares
egen ylmpriaux = rsum(comisiones dietass viaticoss bonif aguinals bonificaciones ylmpri_ci), missing
replace ylmpri_ci= ylmpriaux
replace ylmpri_ci=. if ingresos==999999
replace ylmpri_ci=. if emp==0
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

*gen ylnmpri_ci=. /*NA, esta variable aparece recién en 1998*/
*Modificación Mayra Sáenz - Septiembre 2014
egen ylnmpri_ci=rsum(especiess combuss), missing
replace ylnmpri_ci=. if especiess==. & combuss==. 
replace ylnmpri_ci=. if emp_ci==0
*Conversión colones a dólares
replace ylnmpri_ci= ylnmpri_ci/8.76 


gen ylnmpri1_ci=. /*NA, esta variable aparece recién en 1998*/


***************
***ylmsec_ci***
***************

gen ylmsec_ci=ysec 
replace ylmsec_ci=. if emp==0 | otrotrab==2
replace ylmsec_ci=. if ysec==999999
*Conversión colones a dólares
replace ylmsec_ci= ylmsec_ci/8.76 


gen ylmsec1_ci=.

****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=. /*NA, esta variable aparece recién en 1998*/



****************
***remesas_ci***
****************


forvalues i=1(1)5 {

gen remesas`i'=.
replace remesas`i'=.  if cantid`i'a==9999
replace remesas`i'=cantid`i'a*2 if frecun`i'a==0
replace remesas`i'=cantid`i'a if frecun`i'a==1
replace remesas`i'=cantid`i'a/2 if frecun`i'a==3
replace remesas`i'=cantid`i'a/3 if frecun`i'a==4
replace remesas`i'=cantid`i'a/4 if frecun`i'a==4
replace remesas`i'=cantid`i'a/5 if frecun`i'a==5
replace remesas`i'=cantid`i'a/6 if frecun`i'a==6
replace remesas`i'=cantid`i'a/8 if frecun`i'a==8
replace remesas`i'=cantid`i'a/12 if frecun`i'a==12


}

/*egen remesas_ch=rsum(remesas1 remesas2 remesas3 remesas4 remesas4 remesas5) 
replace remesas_ch=. if remesas1==. & remesas2==. & remesas3==. & remesas4==. & remesas5==. 
gen remesas_ci=. /*NA, esta variable aparece recién en 1998*/
*/
*Modificación Mayra Sáenz - Septiembre 2014

egen remesaux=rsum(remesas1 remesas2 remesas3 remesas4 remesas4 remesas5), missing
replace remesaux=. if remesas1==. & remesas2==. & remesas3==. & remesas4==. & remesas5==.

gen remesas_ci=. 
replace remesas_ci = remesaux
*Conversión colones a dólares
replace remesas_ci= remesas_ci/8.76 


**************************
***ynlm0_ci & ynlnm0_ci***
**************************
/*Modificación Mayra Sáenz - Septiembre 2014
*Aparentemente en el cuestionario estas variables son para desempleados, pero también responden los empleados
*Incluyo los ingresos en actividad principal con filtro de emp_ci ==1 y en otros ingresos si está]
*desempleado, como está en el manual metodológico.

/*Estas dos variables de ingreso no laboral, aparecen sólo en este año.
A partir de 1998, el cuestionario cambia y la variable de ingreso no laboral 
disponible va a ser ynlm_ci*/

gen bonif=.
replace bonif=bonifici*bon/12
replace bonif=0 if emp==1 & bonifici==.
replace bonif=. if bonifici==999999 | bon==999

gen comisiones=.
replace comisiones=comision*comi/12
replace comisiones=0 if emp==1 & comision==.
replace comisiones=. if comision==999999 | comi==999

gen dietass=.
replace dietass=dietas*die/12
replace dietass=0 if emp==1 & dietas==.
replace dietass=. if dietas==999999 | die==999

gen viaticoss=.
replace viaticoss=viaticos*viat/12
replace viaticoss=0 if emp==1 & viaticos==.
replace viaticoss=. if viaticos==999999 | viat==999

gen especiess=.
replace especiess=especies*espe/12
replace especiess=0 if emp==1 & especies==.
replace especiess=. if especies==999999 | espe==999

gen deveh=.
replace deveh=depreci*depre/12
replace deveh=0 if emp==1 & depreci==.
replace deveh=. if depreci==999999 | depre==999

gen combuss=.
replace combuss=combus*comb/12
replace combuss=0 if emp==1 & combus==.
replace combuss=. if combus==999999 | comb==999

gen jubilacion=.
replace jubilacion=jubila*jubil/12
replace jubilacion=0 if emp==1 & jubila==.
replace jubilacion=. if jubila==999999 | jubil==999

gen acteventual=.
replace acteventual=eventua*event/12
replace acteventual=0 if emp==1 & eventua==.
replace acteventual=. if eventua==999999 | event==999

gen cuotalim=.
replace cuotalim=alimen*ali/12
replace cuotalim=0 if emp==1 & alimen==.
replace cuotalim=. if alimen==999999 | ali==999

gen alquileres=.
replace alquileres=alquil*alqui/12
replace alquileres=0 if emp==1 & alquil==.
replace alquileres=. if alquil==999999 | alqui==999

gen alqneg=.
replace alqneg=negocio*neg/12
replace alqneg=0 if emp==1 & negocio==.
replace alqneg=. if negocio==999999 | neg==999

gen arrendamiento=.
replace arrendamiento=terreno*terr/12
replace arrendamiento=0 if emp==1 & terreno==.
replace arrendamiento=. if terreno==999999 | terr==999

rename ayuda fqayudacol
gen ayuda=.
replace ayuda=ayudapa*fqayudacol/12
replace ayuda=0 if emp==1 & ayudapa==.
replace ayuda=. if ayudapa==999999 | fqayudacol==999

gen otross=.
replace otross=otros*otr/12
replace otross=0 if emp==1 & otros==.
replace otross=. if otros==999999 | otr==999

gen aguinals=.
replace aguinals=aguinal/12
replace aguinals=0 if emp==1 & aguinal==.
replace aguinals=. if aguinal==999999 

gen dividendos=.
replace dividendos=dividen/12
replace dividendos=0 if emp==1 & dividen==.
replace dividendos=. if dividen==999999

gen bonificaciones=.
replace bonificaciones=bonifica/12
replace bonificaciones=0 if emp==1 & bonifica==.
replace bonificaciones=. if bonifica==999999

gen depauto=.
replace depauto=deprecia/12
replace depauto=0 if emp==1 & deprecia==.
replace depauto=. if deprecia==999999

gen vtainm=.
replace vtainm=vtabien/12
replace vtainm=0 if emp==1 & vtabien==.
replace vtainm=. if vtabien==999999


egen ynlm0_ci=rsum(comisiones dietass viaticoss bonif ayuda cuotalim alquileres alqneg jubilacion deveh otross  dividendos  acteventual arrendamiento  aguinals bonificaciones depauto vtainm)
replace ynlm0_ci=. if comisiones==. & dietass==. & viaticoss==. &  ayuda==. & cuotalim==. & alquileres==. & alqneg==. & jubilacion==. & deveh==.  & dividendos==.  & acteventual==. & arrendamiento==. & otross==. & bonif==. & aguinals==. & bonificaciones==. & depauto==. & vtainm==.
replace ynlm0_ci=. if emp_ci==0

egen ynlnm0_ci=rsum(especiess combuss)
replace ynlnm0_ci=. if especiess==. & combuss==. 
replace ynlnm0_ci=. if emp_ci==0

gen ynlm_ci=. /*NA, esta variable aparece recién en 1998*/
*/

*Modificación Mayra Sáenz - Septiembre 2014
egen ynlm_ci=rsum(ayuda cuotalim alquileres alqneg jubilacion deveh otross dividendos acteventual arrendamiento depauto vtainm remesaux), missing
replace ynlm_ci=. if emp_ci==0
*Conversión colones a dólares
replace ynlm_ci= ynlm_ci/8.76 

gen ynlnm_ci=.

* Para capturar el ingreso de los desocupados 
egen ylmotros_ci = rsum(comisiones dietass viaticoss bonif aguinals bonificaciones), missing
replace ylmotros_ci =. if emp_ci ==1
*Conversión colones a dólares
replace ylmotros_ci= ylmotros_ci/8.76 

egen ylnmotros_ci=rsum(especiess combuss), missing
replace ylnmotros_ci=. if emp_ci==1
*Conversión colones a dólares
replace ylnmotros_ci= ylnmotros_ci/8.76 


**********************
***ylm_ci & ylm1_ci***
**********************
*Modificación Mayra Sáenz - Septiembre 2014
*Incluyo los ingresos de otros trabajos.
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing

*gen ylm1_ci=. /*NA, esta variable aparece recién en 1998*/


************************
***ylnm_ci & ylnm1_ci***
************************
*gen ylnm_ci=. /*NA, esta variable aparece recién en 1998*/

egen ylnm_ci= rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci), missing

*gen ylnm1_ci=. /*NA, esta variable aparece recién en 1998*/



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

*by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar"

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

*gen ylnm_ch=. /*NA, esta variable aparece recién en 1998*/
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch  "Ingreso laboral no monetario del Hogar"


*gen ylnm1_ch=. /*NA, esta variable aparece recién en 1998*/

******************
*** remesas_ch ***
******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 



***************
*** ynlm_ch ***
***************

/*by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm0_ch=rsum(ynlm remesas_ch)
replace ynlm0_ch=. if ynlm==. & remesas_ch==.
drop ynlm
*/
*gen ynlm_ch=.
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del Hogar"

****************
*** ynlnm_ch ***
****************

*by idh_ch, sort: egen ynlnm0_ch=sum(ynlnm0_ci) if miembros_ci==1

*gen ynlnm_ch=.
*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1

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
*Conversión colones a dólares
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
replace aedu_ci=16 if  nivedu==64
replace aedu_ci=17 if  nivedu==65
replace aedu_ci=18 if  nivedu==66
replace aedu_ci=19 if  nivedu==67
replace aedu_ci=20 if  nivedu==68
replace aedu_ci=21 if  nivedu==69
replace aedu_ci=22 if  nivedu==70

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
replace aedu_ci=16 if  ultgrado==64
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
replace eduac_ci=0 if (nivedu>=51 & nivedu<=53) | (ultgrado>=51 & ultgrado<=53) & aedu_ci~=.
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
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 
label define pqnoasis_ci 2 "Muy caro" 3 "Muy lejos" 4 "No hay profesor", add 
label define pqnoasis_ci 5 "Cerro el centro" 6 "Repitio mucho" , add
label define pqnoasis_ci 7 "No vale la pena" 8 "Por la edad" 9 " No hay escuela nocturna", add
label define pqnoasis_ci 10 " Finalizo sus estudios"  11 " Causas del hogar" 12 " No existe otro grado" 13 "Otros" , add
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

gen des1_ch=.
/*NA*/

gen des2_ch=.
/*NA*/

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

*Conversión colones a dólares
replace vivialq_ch= vivialq_ch/8.76 


gen vivialqimp_ch=alquiler
replace vivialqimp_ch=. if alquiler==99999
*Conversión colones a dólares
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
replace cotizando_ci=1 if (segsoci==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"*/

* Nota MGD 09/23/2014: se utiliza la variable SEGSOCI en lugar de segsoci que esta incompleta.
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
gen tamemp_ci=1 if tamest>=1 & tamest<=5
replace tamemp_ci=2 if tamest>=6 & tamest<=50
replace tamemp_ci=3 if tamest>50 & tamest!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci

*************
**pension_ci*
*************
gen pension_ci=(jubila>=1 & jubila<999999)
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

gen ypen_ci=jubila*jubil/12 if pension_ci==1
*Conversión colones a dólares
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
gen lp25_ci = 33.6152
label var lp25_ci "Linea de pobreza 2.5 dólares, año base 2005"

*********
*lp4_ci***
*********
gen lp4_ci =53.78432
label var lp4_ci "Linea de pobreza 4 dólares, año base 2005"

*************
**salmm_ci***
*************
gen salmm_ci= .
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


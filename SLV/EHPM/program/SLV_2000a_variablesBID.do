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
local ANO "2000"
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
*Inclusión Mayra Sáenz - Abril 2014
gen region_c= depto

label define region_c  ///
          1 "Ahuachapán" ///
           2 "Santa Ana" ///
           3 "Sonsonate" ///
           4 "Chalatenango" ///
           5 "La Libertad" ///
           6 "San Salvador" ///
           7 "Cuscatlán" ///
           8 "La Paz" ///
           9 "Cabañas" ///
          10 "San Vicente" ///
          11 "Usulután" ///
          12 "San Miguel" ///
          13 "Morazán" ///
          14 "La Unión" 
label value region_c region_c
label var region_c "División política, departamento"


***************
***factor_ch***
***************

gen factor_ch=factori
label variable factor_ch "Factor de expansion del hogar"

************
***idh_ch***
************

sort folio tipo
egen idh_ch= group(folio tipo)
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

gen anio_c=2000
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=mes

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
replace relacion_ci=5 if parentco==10
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


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Julio 2021	

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
 
****************
****condocup_ci*
****************
/*
generat r405=1
replace r405=2 if labterre==2 & negpropio==2 & vendprodu==2  & cocilavo==2  & hizotorti==2 & homeprod==2 & ayudfami==2  & otroingr==2   
replace r405=. if labterre==. & negpropio==. & vendprodu==.  & cocilavo==.  & hizotorti==. & homeprod==. & ayudfami==.  & otroingr==.
replace r405=. if labterre==0 & negpropio==0 & vendprodu==0  & cocilavo==0  & hizotorti==0 & homeprod==0 & ayudfami==0  & otroingr==0

gen condocup_ci=.
replace condocup_ci=1 if trabajo==1 | tienetra==1 | r405==1
replace condocup_ci=2 if busco==1 & (comobusco>=1 & comobusco<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
drop r405
*/
* Se considera el limite inferior de la encuesta que es de 10 anios y mas. MGD 06/10/2014
gen condocup_ci=.
replace condocup_ci=1 if trabajo==1 | tienetra==1 | (labterre==1 | negpropio==1 | vendprodu==1 | cocilavo==1  | hizotorti==1 | homeprod==1 | ayudfami==1  | otroingr==1) 
replace condocup_ci=2 if condocup_ci!=1 & (busco==1 | (pqnobus==14 | pqnobus==15) | (comobusco>=1 & comobusco<=8) | (tpobusm>=1 & tpobusm<=4))
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=5
replace condocup_ci=4 if edad_ci<5
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais

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

/*En este único año la encuesta pregunta a los mayores de 5 años en vez de 10 años
dentro de la sección de actividad económica, a fin de mantener la consistencia de la
muestra entre años hay que controlar de que se este acotando a este grupo*/

****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 & busco==1)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 & busco==1 | (busco==2 & (pqnobus==14 |pqnobus==15)))

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

gen desalent_ci=(emp_ci==0 & busco==2 & (pqnobus==1 | pqnobus==3))

*****************
***horaspri_ci***
*****************
destring hrshab, generate (hrshabi)
replace hrshabi=. if idh_ch==14615 & idp_ci==1 
/*Ajuste porque esta persona tiene un 99 en hrshabi y sino se hace missing cuando no corresponde*/

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
replace categopri_ci=2 if categ==2 | categ==3 |categ==4 
replace categopri_ci=3 if categ==6 | categ==7 |categ==8 | categ==9 
replace categopri_ci=4 if categ==5
replace categopri_ci=0 if categ==10

label define categopri_ci 0 "Otros" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


/*Dentro de la categoría empleado se incluyen 65 cooperativistas, esta definición es más amplia de 
lo usual, principalmemte porque se incluye a los cooperativistas,pero la encuesta considera 
a todos estos como trabajadores dependientes. 
En el manual del encuestador: "En el caso de una persona que está asociada 
a la cooperativa y además trabaja como patrono o cuenta propia en tierras que son propiedad 
de la empresa cooperativa, se le anotará código 01 a 03, según el caso. Cuando la persona 
cumple siempre el primer requisito o sea que es asociada y trabaja como jornalero o empleado 
en el trabajo colectivo de la cooperativa se le anotará código igual 04."
Por otra parte el grupo "otros" que son 50 casos se incorpora dentro de la categoría empleados ya 
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
replace firmapeq_ci=1 if tamest<5
replace firmapeq_ci=0 if tamest>=5 & tamest<99
replace firmapeq_ci=. if categ==9
replace firmapeq_ci=. if emp==0

/* No se le pregunta a los empleados domesticos, en la codificación se les asignó cero
lo reeplazamos por missing*/
*/
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

ren ocup ocup_aux
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

gen durades_ci=tpobusm/4.3 if tpobusm>0 & tpobusm<999 /*La variable debe llevarse a meses*/
*replace durades_ci=. if tpobusm==0 /*No aplica*/
*replace durades_ci=. if tpobusm==999 /*No responde*/ 
*replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.



*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

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

egen yprijbd=rsum( yprid hrsextrasd vacacionesd aguinaldod bonificacionesd), missing
replace yprijbd=999999 if yprid==999999 | hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999 
replace yprijbd=yprid if yprid>0 & yprid~=999999 & ( hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. & hrsextrasd==. & vacacionesd==. & aguinaldod==. & bonificacionesd==. 

/*Para los trabajadores independientes*/


gen ingrneto=ingrbru-costos
replace ingrneto=0 if ingrneto<0 /*Son 7 observaciones*/
replace ingrneto=999999 if ingrbru==9999999 | costos==9999999

gen yprijbi=.
replace yprijbi=ingrneto*30 if frecingr==1
replace yprijbi=ingrneto*4.3 if frecingr==2
replace yprijbi=ingrneto*2 if frecingr==3
replace yprijbi=ingrneto if frecingr==4 | frecingr==9
replace yprijbi=ingrneto/2 if frecingr==5
replace yprijbi=ingrneto/3 if frecingr==6
replace yprijbi=ingrneto/6 if frecingr==7
replace yprijbi=ingrneto/12 if frecingr==8
replace yprijbi=999999 if ingrneto==999999 | frecingr==99
replace yprijbi=. if categopri_ci>2


*replace yprijbi=0 if categopri_ci<=2 & yprijbi==. & emp_ci==1 

/*Ojo con esto último. Originalmente la encuesta conputa una serie de 
missings (88 casos)que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/
/*Para los trabajadores independientes*/

egen ylmpri_ci=rsum(yprijbi yprid), missing
replace ylmpri_ci=. if yprijbi==999999 | yprid==999999
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp==0

egen ylmpri1_ci=rsum(yprijbi yprijbd), missing
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

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0

egen ylnmpri1_ci=rsum(ylnmpri_ci valaut), missing
replace ylnmpri1_ci=. if ylnmpri_ci==. & valaut==.
replace ylnmpri1_ci=ylnmpri_ci if valaut==999999
replace ylnmpri1_ci=. if emp_ci==0


***************
***ylmsec_ci***
***************
*Yanira: se eliminan las observaciones para aquellos individuos que no tienen otro empleo o para 
*aquellos que no aplica la pregunta de cuánto gana en su segundo trabajo.

gen ysec1=ysec
replace ysec1=. if ysec==999999 
replace ysec1=. if otrotrab!=1

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

egen ylmsec1_ci=rsum(ysec1 hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if ysec1==. & hrsextras==. & vacaciones==. & aguinaldo==. & bonificaciones==. 
replace ylmsec1_ci=. if emp_ci==0 | otrotrab==2

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

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
replace ylnmsec_ci=. if food2==. &  ropa2==. & merca2==. & vivi2==. & trans2==. & segur2==. & otross2==. 
replace ylnmsec_ci=. if emp_ci==0


**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylm1_ci=rsum(ylmpri1_ci ylmsec1_ci), missing
replace ylm1_ci=. if ylmpri1_ci==. & ylmsec1_ci==.


************************
***ylnm_ci & ylnm1_ci***
************************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

egen ylnm1_ci=rsum(ylnmpri1_ci ylnmsec_ci), missing
replace ylnm1_ci=. if ylnmpri1_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

gen remesasext=.
replace remesasext=remescol*fqremescol/12
replace remesasext=. if remescol==999999 | fqremescol==999

gen ayuda=.
replace ayuda=ayudacol*fqayudacol/12
replace ayuda=. if ayudacol==999999 | fqayudacol==999

gen cuotalim=.
replace cuotalim=qotalcol*fqqotalcol/12
replace cuotalim=. if qotalcol==999999 | fqqotalcol==999

gen alqui=.
replace alqui=alquicol*fqalquicol/12
replace alqui=. if alquicol==999999 | fqalquicol==999

gen alqneg=.
replace alqneg=alqnegco*fqalqnegco/12
replace alqneg=. if alqnegco==999999 | fqalqnegco==999

gen jubil=.
replace jubil=jubilcol*fqjubilcol/12
replace jubil=. if jubilcol==999999 | fqjubilcol==999

gen deveh=.
replace deveh=devehcol*fqdevehcol/12
replace deveh=. if devehcol==999999 | fqdevehcol==999

gen otros=.
replace otros=otroscol*fqotroscol/12
replace otros=. if otroscol==999999 | fqotroscol==999

gen utilidades=.
replace utilidades=utilcol/12
replace utilidades=. if utilcol==999999

gen dividendos=.
replace dividendos=divaccol/12
replace dividendos=. if divaccol==999999

gen intereses=.
replace intereses=intercol/12
replace intereses=. if intercol==999999

gen herencias=.
replace herencias=herencol/12
replace herencias=. if herencol==999999

gen indemnizacion=.
replace indemnizacion=indemcol/12
replace indemnizacion=. if indemcol==999999

gen ayudagob=.
replace ayudagob=aygobcol/12
replace ayudagob=. if aygobcol==999999

gen acteventual=.
replace acteventual=acevecol/12
replace acteventual=. if acevecol==999999

gen arrendamiento=.
replace arrendamiento=alqtiecol/12
replace arrendamiento=. if alqtiecol==999999

gen otross=.
replace otross=otroscol443/12
replace otross=. if otroscol443==999999

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross), missing
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & acteventual==. & arrendamiento==. & otross==. 

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=remesasext
replace remesas_ci=. if remesasext==0

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

******************
*** remesas_ch ***
******************

rename r704 frecuna
rename r705 cantida
rename r707 ayudaes

gen remesash=.
replace remesash=cantida/12 if frecuna==1
replace remesash=cantida/6 if frecuna==2
replace remesash=cantida/3 if frecuna==3
replace remesash=cantida/2 if frecuna==4
replace remesash=cantida if frecuna==5
replace remesash=cantida*2 if frecuna==6
replace remesash=. if cantida==99999 | cantida==0 /*No aplicable, no responde*/

gen remesasnm=.
replace remesasnm=ayudaes/12
replace remesasnm=. if ayudaes==999999

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

*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=r311
replace rentaimp=. if r311==99999

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
replace aedu_ci=0 if nivelcursa==1 

replace aedu_ci=1 if nivelcursa==2 & nivedu==1
replace aedu_ci=2 if nivelcursa==2 & nivedu==2
replace aedu_ci=3 if nivelcursa==2 & nivedu==3
replace aedu_ci=4 if nivelcursa==2 & nivedu==4
replace aedu_ci=5 if nivelcursa==2 & nivedu==5
replace aedu_ci=6 if nivelcursa==2 & nivedu==6
replace aedu_ci=7 if nivelcursa==2 & nivedu==7
replace aedu_ci=8 if nivelcursa==2 & nivedu==8
replace aedu_ci=9 if nivelcursa==2 & nivedu==9

replace aedu_ci=10 if nivelcursa==3 & nivedu==10
replace aedu_ci=11 if nivelcursa==3 & nivedu==11
replace aedu_ci=12 if nivelcursa==3 & nivedu==12

replace aedu_ci=13 if nivelcursa==4 & nivedu==1
replace aedu_ci=14 if nivelcursa==4 & nivedu==2
replace aedu_ci=15 if nivelcursa==4 & nivedu==3
replace aedu_ci=16 if nivelcursa==4 & nivedu==4
replace aedu_ci=17 if nivelcursa==4 & nivedu==5
replace aedu_ci=18 if nivelcursa==4 & nivedu==6
replace aedu_ci=19 if nivelcursa==4 & nivedu==7
replace aedu_ci=20 if nivelcursa==4 & nivedu==8
replace aedu_ci=22 if nivelcursa==4 & nivedu==10
replace aedu_ci=23 if nivelcursa==4 & nivedu==11
replace aedu_ci=27 if nivelcursa==4 & nivedu==15

replace aedu_ci=13 if nivelcursa==5 & nivedu==1
replace aedu_ci=14 if nivelcursa==5 & nivedu==2
replace aedu_ci=15 if nivelcursa==5 & nivedu==3

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0

/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if ultnivel==0 & (hasist==1 | hasist==2)
replace aedu_ci=0 if ultnivel==1

replace aedu_ci=1 if ultnivel==2 & ultgrado==1
replace aedu_ci=2 if ultnivel==2 & ultgrado==2
replace aedu_ci=3 if ultnivel==2 & ultgrado==3
replace aedu_ci=4 if ultnivel==2 & ultgrado==4
replace aedu_ci=5 if ultnivel==2 & ultgrado==5
replace aedu_ci=6 if ultnivel==2 & ultgrado==6
replace aedu_ci=7 if ultnivel==2 & ultgrado==7
replace aedu_ci=8 if ultnivel==2 & ultgrado==8
replace aedu_ci=9 if ultnivel==2 & ultgrado==9
replace aedu_ci=9 if ultnivel==2 & ultgrado==11
replace aedu_ci=9 if ultnivel==2 & ultgrado==12

replace aedu_ci=10 if ultnivel==3 & ultgrado==10
replace aedu_ci=11 if ultnivel==3 & ultgrado==11
replace aedu_ci=12 if ultnivel==3 & ultgrado==12

replace aedu_ci=13 if ultnivel==4 & ultgrado==1
replace aedu_ci=14 if ultnivel==4 & ultgrado==2
replace aedu_ci=15 if ultnivel==4 & ultgrado==3
replace aedu_ci=16 if ultnivel==4 & ultgrado==4
replace aedu_ci=17 if ultnivel==4 & ultgrado==5
replace aedu_ci=18 if ultnivel==4 & ultgrado==6
replace aedu_ci=19 if ultnivel==4 & ultgrado==7
replace aedu_ci=20 if ultnivel==4 & ultgrado==8
replace aedu_ci=21 if ultnivel==4 & ultgrado==9
replace aedu_ci=22 if ultnivel==4 & ultgrado==10
replace aedu_ci=23 if ultnivel==4 & ultgrado==11
replace aedu_ci=24 if ultnivel==4 & ultgrado==12
replace aedu_ci=27 if ultnivel==4 & ultgrado==15

replace aedu_ci=13 if ultnivel==5 & ultgrado==1
replace aedu_ci=14 if ultnivel==5 & ultgrado==2
replace aedu_ci=15 if ultnivel==5 & ultgrado==3

replace aedu_ci=. if aedu_ci>edad & aedu_ci~=. 
/*Hay un caso en donde los años de educación son mayores a la edad*/

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
replace eduac_ci=1 if (ultnivel==4 | nivelcursa==4) & aedu_ci~=.
replace eduac_ci=0 if (ultnivel==5 | nivelcursa==5) & aedu_ci~=.
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
replace pqnoasis_ci=. if pqnoestu==0
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 " Enfermedad", add 
label define pqnoasis_ci 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis_ci 7 "Finalizo sus estudios" 8 "No existe escuela cercana o cupo", add
label define pqnoasis_ci 9 "Otros" 99 "No responde", add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if pqnoestu ==3
replace pqnoasis1_ci = 2 if pqnoestu ==1
replace pqnoasis1_ci = 3 if pqnoestu ==4 | pqnoestu ==5
replace pqnoasis1_ci = 5 if pqnoestu ==2
replace pqnoasis1_ci = 6 if pqnoestu ==7
replace pqnoasis1_ci = 7 if pqnoestu ==6
replace pqnoasis1_ci = 8 if pqnoestu ==8 
replace pqnoasis1_ci = 9 if pqnoestu ==9

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


gen aguared_ch=(r313==1 | r313==2)

gen aguadist_ch=1 if r313==1
replace aguadist_ch=2 if r313==2
replace aguadist_ch=3 if r313>=2 & r313<=9

gen aguamala_ch=(r314==2)
replace aguamala_ch=. if r314==9

gen aguamide_ch=.
/*NA*/

gen luz_ch=(r312==1 | r312==2)
replace luz_ch=. if r312==9

gen luzmide_ch=.
/*NA*/

gen combust_ch=(r312>=1 & r312<=3)
replace combust_ch=. if r312==9

gen bano_ch=(r317>=1 & r317<=6)

gen banoex_ch=(r317>=4 & r317<=6)

* MGR Jul, 2015: variable generada como missing ya que no tenemos opción 3, pero genero de la misma manera que los años anteriores
gen des1_ch=.
replace des1_ch = 0 if r317==7
replace des1_ch = 1 if r317>=1 & r317<=2
replace des1_ch = 2 if r317>=3 & r317<=6

* MGR Jul, 2015: variable generada como missing ya que no tenemos opción 2, pero genero de la misma manera que los años anteriores
gen des2_ch=.
replace des2_ch = 0 if r317==7
replace des2_ch = 1 if des1_ch==1 | des1_ch==2


gen piso_ch=0 if r304==4
replace piso_ch=1 if r304>=1 & r304<=3
replace piso_ch=2 if r304==5

gen pared_ch=0 if r303==2 | r303==3 | r303==6 | r303==7
replace pared_ch=1 if r303==1 | r303==4 | r303==5
replace pared_ch=2 if r303==8

gen techo_ch=0 if r302==5 | r302==6
replace techo_ch=1 if r302>=1 & r302<=4
replace techo_ch=2 if r302==7

gen resid_ch=0 if r324==1 | r324==2
replace resid_ch=1 if r324==4 | r324==5
replace resid_ch=2 if r324==6
replace resid_ch=3 if r324==3 | r324==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (r313 >=1 & r313 <=3) | r313 == 6 
replace aguamejorada_ch = 0 if (r313 >=4 & r313 <=5) | (r313 >=7 & r313 <=9)

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (r317>=1 & r317 <=3)
replace banomejorado_ch = 0 if (r317>=4 & r317 <=7)

gen dorm_ch=r306

gen cuartos_ch=r305

gen cocina_ch=.
/*NA*/

gen telef_ch=(r323>=1 & r323<=7)
replace telef_ch=. if r323==9

gen refrig_ch=(r32505==1)
replace refrig_ch=. if r32505==9

gen freez_ch=.
/*NA*/

gen auto_ch=(r32512==1)
replace auto_ch=. if r32512==9


gen compu_ch=(r32509==1)
replace compu_ch=. if r32509==9

gen internet_ch=(r32510==1)
replace internet_ch=. if r32510==9


gen cel_ch=(r323>=2 & r323<=7)
replace cel_ch=. if r323==9


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

gen vivialqimp_ch=r311
replace vivialqimp_ch=. if r311==99999

************************
*** El SALVADOR 2000 ***
************************

/* 
103. Relación de parentesco
 1. Jefe(a)
 2. Esposo(a)
 3. Hijo(a)
 4. Padre/madre 
 5. Yerno/nuera
 6. Nieto(a)
 7. Suegro(a)
 8. Otros familiares
 9. Empleada doméstica 
 10.Otros
*/

 count
 tab parentco
 
 gen	 incl=1 if (parentco>=1 & parentco<=10)
 replace incl=0 if (parentco==9)
 
 
 
** AREA


** Gender classification of the population refering to the head of the household.

 sort idh_ch nrorden

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(idh_ch)
 
 tab sexo sexo_d if parentco==1

 sort idh_ch nrorden
 
** Years of Education
/*
r202a (alfabet)
202. ¿SABE...(NOMBRE)... LEER Y ESCRIBIR? 
1. Si 
2. No Pase a 203
 r202b
 SI CONTESTÓ 1, ¿DÓNDE APRENDIÓ?
 1. Centro educativo o escuela 4. Escuela nocturna 
 2. Familiares, amigo o vecino 5. Otros
 3. Círculos de alfabetización

r203 (ESTUDIA)
203. ¿ESTUDIA ACTUALMENTE O ASISTE A UN CENTRO MATERNAL? 
 1. Si 
 2. No ==> Si es menor de 4 años termine, si no pase a 216

r204 (NIVEL)						r205  (GRADO)
204. ¿QUÉ NIVEL CURSA O ASISTE ACTUALMENTE?		205. ¿QUÉ GRADO O CURSO ESTUDIA ACTUALMENTE EN ESE NIVEL?
 0. Maternal 				==>214			     0
 1. Parvularia 							(1º  a 3º )
 2. Básica   							(1º  a 9º )
 3. Media      							(10º a 12º)
 4. Superior universitario 					(1º  a 15º)
 5. Superior no universitario 					(1º  a 3º ) 
 6. Educación de adultos 					(1º  a 12º)
 7. Educación especial 						(1º  a 12º)
 8. Otros							    ---

r216
216. ¿SE MATRICULÓ...(NOMBRE)... DURANTE ESTE AÑO LECTIVO?

r217 (HASIST)
217. ¿HA ASISTIDO ALGUNA VEZ...(NOMBRE)... A UN CENTRO DE ENSEÑANZA?
 1. Si
 2. No ==> 221

r218
218. EL CENTRO DE ENSEÑANZA AL QUE ASISTIÓ ERA?
 1. ¿Oficial? 2. ¿Laico? 3. ¿Religioso?

219 ¿CUÁL FUE EL ÚLTIMO NIVEL Y GRADO QUE APROBÓ?

r219a	(ULTNIVEL)					r219b (ULTGRADO) 
 0. Ninguno 							     0
 1. Parvularia 							(1º  a 3º ) 
 2. Básica 							(1°  a 9° ) 
 3. Media 							(10° a 12°) 
 4. Superior universitario 					(1º  a 15º)
 5. Superior no universitario 					(1º  a 3º )
 6. Educación de adultos 					(1º  a 12º)
 7. Educación especial 						(1º  a 12º)
 8. Otros
*/

 gen nivel = nivelcursa 
 gen grado = nivedu

 
 gen anoest=.
 replace anoest=0  if  ultnivel==0 | ultnivel==1  | (ultnivel==2 & ultgrado==0)  | (ultnivel==6 & ultgrado==0)  | (ultnivel==7 & ultgrado==0) | (nivel==0) | nivel==1 | (nivel==2 & grado==1) | (nivel==6 & grado==1) | (nivel==7 & grado==1)| hasist==2
 replace anoest=1  if (ultnivel==2 & ultgrado==1) | (ultnivel==6 & ultgrado==1)  | (ultnivel==7 & ultgrado==1)  | (nivel==2 & grado==2) | (nivel==6 & grado==2) | (nivel==7 & grado==2)
 replace anoest=2  if (ultnivel==2 & ultgrado==2) | (ultnivel==6 & ultgrado==2)  | (ultnivel==7 & ultgrado==2)  | (nivel==2 & grado==3) | (nivel==6 & grado==3) | (nivel==7 & grado==3) | ultnivel==8 & ultgrado==2
 replace anoest=3  if (ultnivel==2 & ultgrado==3) | (ultnivel==6 & ultgrado==3)  | (ultnivel==7 & ultgrado==3)  | (nivel==2 & grado==4) | (nivel==6 & grado==4) | (nivel==7 & grado==4)
 replace anoest=4  if (ultnivel==2 & ultgrado==4) | (ultnivel==6 & ultgrado==4)  | (ultnivel==7 & ultgrado==4)  | (nivel==2 & grado==5) | (nivel==6 & grado==5) | (nivel==7 & grado==5) 
 replace anoest=5  if (ultnivel==2 & ultgrado==5) | (ultnivel==6 & ultgrado==5)  | (ultnivel==7 & ultgrado==5)  | (nivel==2 & grado==6) | (nivel==6 & grado==6) | (nivel==7 & grado==6)
 replace anoest=6  if (ultnivel==2 & ultgrado==6) | (ultnivel==6 & ultgrado==6)  | (ultnivel==7 & ultgrado==6)  | (nivel==2 & grado==7) | (nivel==6 & grado==7) | (nivel==7 & grado==7)
 replace anoest=7  if (ultnivel==2 & ultgrado==7) | (ultnivel==6 & ultgrado==7)  | (ultnivel==7 & ultgrado==7)  | (nivel==2 & grado==8) | (nivel==6 & grado==8) | (nivel==7 & grado==8)
 replace anoest=8  if (ultnivel==2 & ultgrado==8) | (ultnivel==6 & ultgrado==8)  | (ultnivel==7 & ultgrado==8)  | (nivel==2 & grado==9) | (nivel==6 & grado==9) | (nivel==7 & grado==9)
 replace anoest=9  if (ultnivel==2 & ultgrado==9) | (ultnivel==3 & ultgrado==0)  | (ultnivel==6 & ultgrado==9)  | (ultnivel==7 & ultgrado==9)  | (nivel==3 & grado==10) | (nivel==6 & grado==10) | (nivel==7 & grado==10)
 replace anoest=10 if (ultnivel==3 & ultgrado==10)| (ultnivel==6 & ultgrado==10)| (ultnivel==7 & ultgrado==10) | (nivel==3 & grado==11) | (nivel==6 & grado==11) | (nivel==7 & grado==11) 
 replace anoest=11 if (ultnivel==3 & ultgrado==11)| (ultnivel==6 & ultgrado==11)| (ultnivel==7 & ultgrado==11) | (nivel==3 & grado==12) | (nivel==6 & grado==12) | (nivel==7 & grado==12) | (ultnivel==2 & ultgrado==11)
 replace anoest=12 if (ultnivel==3 & ultgrado==12)| (ultnivel==4 & ultgrado==0)  | (ultnivel==5 & ultgrado==0)  | (ultnivel==6 & ultgrado==12)  | (ultnivel==7 & ultgrado==12)  | (nivel==4 & grado==1) | (nivel==5 & grado==1)| (ultnivel==2 & ultgrado==12)
 replace anoest=13 if (ultnivel==4 & ultgrado==1) | (ultnivel==5 & ultgrado==1)  | (nivel==4 & grado==2) | (nivel==5 & grado==2)
 replace anoest=14 if (ultnivel==4 & ultgrado==2) | (ultnivel==5 & ultgrado==2)  | (nivel==4 & grado==3) | (nivel==5 & grado==3)
 replace anoest=15 if (ultnivel==4 & ultgrado==3) | (ultnivel==5 & ultgrado==3)  | (nivel==4 & grado==4)
 replace anoest=16 if (ultnivel==4 & ultgrado==4) | (nivel==4 & grado==5)
 replace anoest=17 if (ultnivel==4 & ultgrado==5) | (nivel==4 & grado==6)
 replace anoest=18 if (ultnivel==4 & ultgrado==6) | (nivel==4 & (grado==7 | grado==8 | grado==9))
 replace anoest=19 if (ultnivel==4 & (ultgrado==7 | ultgrado==8 | ultgrado==9))| (nivel==4 & grado==10)
 replace anoest=20 if (ultnivel==4 & ultgrado==10)| (nivel==4 & grado==11)
 replace anoest=21 if (ultnivel==4 & ultgrado==11)| (nivel==4 & grado==12) 
 replace anoest=22 if (ultnivel==4 & ultgrado==12)| (nivel==4 & grado==13)
 replace anoest=23 if (ultnivel==4 & ultgrado==13)| (nivel==4 & grado==14)
 replace anoest=24 if (ultnivel==4 & ultgrado==14)| (nivel==4 & grado==15)
 replace anoest=25 if (ultnivel==4 & ultgrado==15)
 replace anoest=99 if anoest==.


** Economic Active Population 
/*
ACTPR
Actividad principal
0: Ocupados
1: Desocupados
2: Inactivos
*/

gen actpr=actippal 

 gen peaa=0
 replace peaa=1 if actpr==1
 replace peaa=2 if actpr==2
 replace peaa=3 if actpr==3

 gen tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 

 tab peaa [w=factor_ci]
 tab peaa [w=factor_ci] if incl==1
 

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*
r202a (alfabet)
202. ¿SABE...(NOMBRE)... LEER Y ESCRIBIR? 
1. Si 
2. No Pase a 203
 r202b
 SI CONTESTÓ 1, ¿DÓNDE APRENDIÓ?
 1. Centro educativo o escuela 4. Escuela nocturna 
 2. Familiares, amigo o vecino 5. Otros
 3. Círculos de alfabetización

r203 (ESTUDIA)
203. ¿ESTUDIA ACTUALMENTE O ASISTE A UN CENTRO MATERNAL? 
1. Si 
2. No ==> Si es menor de 4 años termine, si no pase a 216

r204 (NIVEL)						r205  (GRADO)
204. ¿QUÉ NIVEL CURSA O ASISTE ACTUALMENTE?		205. ¿QUÉ GRADO O CURSO ESTUDIA ACTUALMENTE EN ESE NIVEL?
0. Maternal 				==>214			     0
1. Parvularia 							(1º  a 3º )
2. Básica   							(1º  a 9º )
3. Media      							(10º a 12º)
4. Superior universitario 					(1º  a 15º)
5. Superior no universitario 					(1º  a 3º ) 
6. Educación de adultos 					(1º  a 12º)
7. Educación especial 						(1º  a 12º)
8. Otros							    ---

*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen     NERP=0 if (edad>=7 & edad<=12) & (estudia==1 | estudia==2)
 replace NERP=1 if (edad>=7 & edad<=12) & (estudia==1) & ((nivel==2) & (grado>=1 & grado<=6))
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (estudia==1 | estudia==2)
 replace NERS=1 if (edad>=13 & edad<=18) & (estudia==1) & ( (nivel==2 & (grado>=7 & grado<=9)) | (nivel==3 & (grado>=10 & grado<=12)) )

** Upper secondary
* Educación Media

 gen     NERS2=0 if (edad>=16 & edad<=18) & (estudia==1 | estudia==2)
 replace NERS2=1 if (edad>=16 & edad<=18) & (estudia==1) & (nivel==3 & (grado>=10 & grado<=12))
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old*
* Read & write


 gen     LIT2=0 if (edad>=15 & edad<=24) & (alfabet>=1 & alfabet<=2)
 replace LIT2=1 if (edad>=15 & edad<=24) & (alfabet==1)

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  (estudia==1 & nivel==2 & (grado>=1 & grado<=6))
 gen sec=1  if  (estudia==1 & nivel==2 & (grado>=7 & grado<=9)) | (estudia==1 & nivel==3 & (grado>=10 & grado<=12))
 gen ter=1  if  (estudia==1 & ((nivel==4 & (grado>=1 & grado<=5)) | nivel==5))

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
/*
r417 (CATEG)						RAMAR
EN SU OCUPACION PRINCIPAL TRABAJA EN CALIDAD DE:	RAMA DE ACTIVIDAD ECONÓMICA DE 
 01. ¿Empleador o patrono?				LOS OCUPADOS A UN DÍGITO
 02. ¿Cuenta propia con local? 
 03. ¿Cuenta propia sin local? 
 04. ¿Cooperativista?
 05. ¿Familiar no remunerado? 
 06. ¿Asalariado permanente? 
 07. ¿Asalariado temporal? 
 08. ¿Aprendiz? 
 09. ¿Servicio doméstico? 
 10. Otros
*/

gen ramar= rama

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ==6 | categ==7) & (ramar>=20 & ramar<=160)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==6 | categ==7) & (ramar>=20 & ramar<=160) & sexo==2

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categ==6 | categ==7 | categ==9) & (ramar>=20 & ramar<=160)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==6 | categ==7 | categ==9) & (ramar>=20 & ramar<=160) & sexo==2

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator
/*
312. ¿QUÉ TIPO DE ALUMBRADO POSEE ESTA VIVIENDA?
 1. Electricidad
 2. Conexión eléctrica vecino
 3. Kerosene (gas)
 4. Candela
 5. Otra clase ________________________
*/

 rename r312 servalum

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (servalum>=1 & servalum<=5) /* Total population excluding missing information */
 replace ELEC=1 if (servalum>=1 & servalum<=2)
 	
** Target 9, Indicator: Proportion of the population using solidfuels (%)

/*
r319 (COMBUSTI)
319. ¿QUÉ COMBUSTIBLE UTILIZAN PREDOMINANTEMENTE PARA COCINAR? 

1. Electricidad
2. Kerosene (gas) 
3. Gas propano
4. Leña 
5. Carbón
6. Ninguno 		==> 321 
7. Otro

320. ¿DÓNDE SE ENCUENTRA UBICADA LA COCINA?
*/

* Gender classification of the population refers to the head of the household.

rename r319 combusti

 gen	 SFUELS=0 if (combusti>=1 & combusti<=7)  /* Total population excluding missing information */
 replace SFUELS=1 if (combusti==4 | combusti==5)
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
r313 (SERAGUA)
313. EL ABASTECIMIENTO DE AGUA DE ESTA VIVIENDA PROCEDE DE:
 1. ¿Cañería dentro de la vivienda?
 2. ¿Cañería fuera de la vivienda pero
 3. ¿Cañería del vecino?
 4. ¿Pila o chorro público?
 5. ¿Chorro común?
 6. ¿Camión, carreta o pipa?
 7. ¿Pozo (privado o común)?
 8. ¿Ojo de agua, río o quebrada?
 9. ¿Otros medios?

314. ¿CREE UD QUE EL AGUA DE SU CASA ES ADECUADA PARA BEBER?
 1. Si ==> 316
 2. No

315. ¿QUÉ HACE ENTONCES CON EL AGUA QUE BEBE? 
 1. La hierve antes de usarla
 2. La trata con lejía u otro producto 
 3. La filtra
 4. No le hace nada 
 5. Compra agua purificada
 6. Otros 

316. EL SERVICIO DE BAÑO LO SATISFACE POR:
*/

* Gender classification of the population refers to the head of the household.

 rename r313 seragua

 gen	 WATER=0 if (seragua>=1 & seragua<=9)  /* Total population excluding missing information */
 replace WATER=1 if ((seragua>=1 & seragua<=5) | seragua==7)
 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
317. EL SERVICIO SANITARIO QUE TIENE ESTA VIVIENDA ES DE: 
 1. ¿Inodoro a alcantarillado?
 2. ¿Inodoro a fosa séptica? 
 3. ¿Letrina privada? 
 4. ¿Inodoro común a alcantarillado?
 5. ¿Inodoro común a fosa séptica? 
 6. ¿Letrina común?
 7. No tiene

318. ¿DÓNDE SE ENCUENTRA EL SERVICIO SANITARIO?
 1. Dentro de la vivienda
 2. Fuera de la vivienda
*/

* Gender classification of the population refers to the head of the household.

 gen servsani = r317

 gen	 SANITATION=0 if (servsani>=1 & servsani<=7) /* Total population excluding missing information */
 replace SANITATION=1 if (servsani>=1 & servsani<=2) | (servsani>=4 & servsani<=5)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
/*
r301 (TIPOVIV)
301.TIPO DE VIVIENDA:
 1. Casa privada o independiente
 2. Apartamento 
 3. Pieza en una Casa 
 4. Pieza en un Mesón 
 5. Casa improvisada 
 6. Rancho 
 7. Otro

r308a (TENENCIA)
308. ¿CUÁL ES LA FORMA DE TENENCIA DE ESTA VIVIENDA?
 1. Inquilino				==> Cuota mensual	
 2. Propietario de la vivienda 
    pero la está pagando a plazos	==> Cuota mensual	
 3. Propietario 
 4. Colono
 5. Guardián de la Vivienda
 6. Ocupante gratuito
 7. Otros

r303 (PAREDES)
303. EL MATERIAL PREDOMINANTE EN LAS  PAREDES ES:
 1. ¿Concreto o mixto?
 2. ¿Bahareque? 
 3. ¿Adobe?   
 4. ¿Madera?
 5. ¿Lámina? 
 6. ¿Paja o palma? 
 7. ¿Materiales de desecho? 
 8. ¿Otros materiales?

r304 (PISO)
304. EL MATERIAL PREDOMINANTE EN EL PISO ES
 1. ¿Ladrillo de cemento?
 2. ¿Ladrillo de barro? 
 3. ¿Cemento?
 4. ¿Tierra? 
 5. ¿Otros materiales?

r305 (NROHABIT)
305. SIN CONTAR BAÑOS, COCINA, PASILLO NI  GARAGE ¿CUANTAS HABITACIONES 
SON DE USO EXCLUSIVO DEL HOGAR?

306. ¿CUÁNTAS HABITACIONES SON UTILIZADAS EXCLUSIVAMENTE
COMO DORMITORIOS? 

r307 (NRONEG)
307. ¿CUÁNTAS HABITACIONES SON UTILIZADAS EXCLUSIVAMENTE PARA TRABAJAR O PARA NEGOCIO? 
(No lo utilizan o lo comparten anotar 00)
*/

gen nroneg = r307
gen nrohabit = r305
gen piso = r304
gen paredes = r303
gen tenencia = r308a
gen tipoviv = r301



 gen cuartneg=1 if (nroneg>=1 & nroneg<=4)
 recode cuartneg (.=0)

 gen nrocuart= nrohabit-cuartneg if nrohabit>nroneg /* Not used */
 
 gen pers = miemh
 
 gen persroom = (pers/nrohabit) 

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenencia>=1 & tenencia<=7) & (tipoviv>=1 & tipoviv<=7)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=4 & tenencia<=7) | (tipoviv>=5 & tipoviv<=7))

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if ((paredes>=1 & paredes<=8) & (piso>=1 & piso<=5))         /* Total population excluding missing information */
 replace secten_2=1 if ((paredes>=5 & paredes<=8) | (piso>=4 & piso<=5))

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator
* 304. EL MATERIAL PREDOMINANTE EN EL PISO ES

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=5)
 replace DIRT=1 if (piso==4)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (tasadeso==0 | tasadeso==1) & (edad>=15 & edad<=24)
 replace UNMPLYMENT15=1 if (tasadeso==1)	       & (edad>=15 & edad<=24)

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
/*
r323 (TELEFONO)
323. ¿TIENE UD. TELÉFONO O BEEPER EN USO?
1. Si, fijo 
2. Si, celular 
3. Si, beeper 
4. Si, fijo y celular 
5. Si, fijo, celular y beeper
6. Fijo y beeper
7. Celular y beeper
8. No tiene
*/

gen telefono = r323

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (telefono>=1 & telefono<=8) /* Total population excluding missing information */
 replace TELCEL=1 if  (telefono==1 | telefono==2 | (telefono>=4 & telefono<=7))

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono>=1 & telefono<=8) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1 | telefono==6 | telefono==4 | telefono==5)

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (telefono>=1 & telefono<=8) /* Total population excluding missing information */
 replace CEL=1 if (telefono==2 | telefono==7 | telefono==4 | telefono==5)

** Target 18, Indicator: "Personal computers in use per 100 population"
/*
325. EQUIPAMIENTO DEL HOGAR 	
						Variable
9. Computadora			1. SI , 2. NO	r32509
   Red Email			1. SI , 2. NO	r32510

*/
** Target 18, Indicator: "Personal computers in use per 100 population"
/*
325. EQUIPAMIENTO DEL HOGAR 	
						Variable
9. Computadora			1. SI , 2. NO	r32509
   Red Email			1. SI , 2. NO	r32510
*/

* Gender classification of the population refers to the head of the household.

 gen computad = r32509
 gen redemail = r32510

 gen     COMPUTER=0 if (computad>=1 & computad<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computad==1)
		
* Target 18, Indicator: "Internet users per 100 population"

* Gender classification of the population refers to the head of the household.

 gen	 INTUSERS=0 if (redemail>=1 & redemail<=2) /* Total population excluding missing information */
 replace INTUSERS=1 if (redemail==1)

*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

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
r407 (PQNOBUS)
407. ¿POR QUÉ NO BUSCÓ TRABAJO? 
 01. Creía que no había trabajo disponible en la zona
 02. No sabía como buscar trabajo 
 03. Se cansó de buscar trabajo 
 04. Espera ciclo agrícola
 05. Asiste a un centro de enseñanza o está de vacaciones escolares 
 06. Por obligaciones familiares o personales
 07. Enfermedad o accidente
 08. Quehaceres domésticos 
 09. Jubilado o pensionado
 10. No puede trabajar (discapacitado, anciano, etc.) 
 11. Asiste a un centro de formación académica
 12. Capacitación personal
 13. Otro
 14. Ya encontró trabajo
 15. Espera respuesta del empleador
 16. Si buscó trabajo
*/

 *gen pqnobus = r407

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (pqnobus==1 | pqnobus==2 | pqnobus==3 | pqnobus==9 | pqnobus==13)
	
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



****************
*afiliado_ci****
****************
gen afiliado_ci= .
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
replace tipocontrato_ci=1 if (contrato==1 & categ==6) & categopri_ci==3
replace tipocontrato_ci=2 if (contrato==1 & categ==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (contrato==2 | tipocontrato_ci==.) & categopri_ci==3
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
replace pension_ci=1 if (jubilcol>0 & jubilcol<999999 ) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
recode fqjubilcol (999=.)
gen ypen_ci=jubilcol*fqjubilcol/12 if pension_ci==1
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
*gen salmm_ci= 124.79 /*fuente: ILO*/

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=78 if rama_ci==1
replace salmm_ci=144 if rama_ci==3
replace salmm_ci=144 if rama_ci==6
replace salmm_ci=122 if salmm_ci==.
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
gen categosec_ci=.

**Cambio de moneda a partir de 2000 - Modificación Mayra Sáenz Abril 2014

qui sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch

local varing "ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci  lp_ci lpe_ci vivialq_ch vivialqimp_ch"
foreach e of local varing {
replace `e' = `e'/8.755 
}

qui sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close





*** MGR Jul, 2015: Por alguna razón la armonización se ha copiado dos veces y al hacer alguna modificación en la primera, cualquier cambio
* no se refleja ya que se reemplaza por la armonización de abajo. Desactivo la segunda. 

/*

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
local ANO "2000"
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
*Inclusión Mayra Sáenz - Abril 2014
gen region_c= depto

label define region_c  ///
          1 "Ahuachapán" ///
           2 "Santa Ana" ///
           3 "Sonsonate" ///
           4 "Chalatenango" ///
           5 "La Libertad" ///
           6 "San Salvador" ///
           7 "Cuscatlán" ///
           8 "La Paz" ///
           9 "Cabañas" ///
          10 "San Vicente" ///
          11 "Usulután" ///
          12 "San Miguel" ///
          13 "Morazán" ///
          14 "La Unión" 
label value region_c region_c
label var region_c "División política, departamento"


***************
***factor_ch***
***************

gen factor_ch=factori
label variable factor_ch "Factor de expansion del hogar"

************
***idh_ch***
************

sort folio tipo
egen idh_ch= group(folio tipo)
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

gen anio_c=2000
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=mes

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
replace relacion_ci=5 if parentco==10
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
generat r405=1
replace r405=2 if labterre==2 & negpropio==2 & vendprodu==2  & cocilavo==2  & hizotorti==2 & homeprod==2 & ayudfami==2  & otroingr==2   
replace r405=. if labterre==. & negpropio==. & vendprodu==.  & cocilavo==.  & hizotorti==. & homeprod==. & ayudfami==.  & otroingr==.
replace r405=. if labterre==0 & negpropio==0 & vendprodu==0  & cocilavo==0  & hizotorti==0 & homeprod==0 & ayudfami==0  & otroingr==0

gen condocup_ci=.
replace condocup_ci=1 if trabajo==1 | tienetra==1 | r405==1
replace condocup_ci=2 if busco==1 & (comobusco>=1 & comobusco<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
drop r405
*/
* Se considera el limite inferior de la encuesta que es de 10 anios y mas. MGD 06/10/2014
gen condocup_ci=.
replace condocup_ci=1 if trabajo==1 | tienetra==1 | (labterre==1 | negpropio==1 | vendprodu==1 | cocilavo==1  | hizotorti==1 | homeprod==1 | ayudfami==1  | otroingr==1) 
replace condocup_ci=2 if condocup_ci!=1 & (busco==1 | (pqnobus==14 | pqnobus==15) | (comobusco>=1 & comobusco<=8) | (tpobusm>=1 & tpobusm<=4))
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=5
replace condocup_ci=4 if edad_ci<5
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais

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

/*En este único año la encuesta pregunta a los mayores de 5 años en vez de 10 años
dentro de la sección de actividad económica, a fin de mantener la consistencia de la
muestra entre años hay que controlar de que se este acotando a este grupo*/

****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 & busco==1)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 & busco==1 | (busco==2 & (pqnobus==14 |pqnobus==15)))

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

gen desalent_ci=(emp_ci==0 & busco==2 & (pqnobus==1 | pqnobus==3))

*****************
***horaspri_ci***
*****************
destring hrshab, generate (hrshabi)
replace hrshabi=. if idh_ch==14615 & idp_ci==1 
/*Ajuste porque esta persona tiene un 99 en hrshabi y sino se hace missing cuando no corresponde*/

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
replace categopri_ci=2 if categ==2 | categ==3 |categ==4 
replace categopri_ci=3 if categ==6 | categ==7 |categ==8 | categ==9 
replace categopri_ci=4 if categ==5
replace categopri_ci=0 if categ==10

label define categopri_ci 0 "Otros" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


/*Dentro de la categoría empleado se incluyen 65 cooperativistas, esta definición es más amplia de 
lo usual, principalmemte porque se incluye a los cooperativistas,pero la encuesta considera 
a todos estos como trabajadores dependientes. 
En el manual del encuestador: "En el caso de una persona que está asociada 
a la cooperativa y además trabaja como patrono o cuenta propia en tierras que son propiedad 
de la empresa cooperativa, se le anotará código 01 a 03, según el caso. Cuando la persona 
cumple siempre el primer requisito o sea que es asociada y trabaja como jornalero o empleado 
en el trabajo colectivo de la cooperativa se le anotará código igual 04."
Por otra parte el grupo "otros" que son 50 casos se incorpora dentro de la categoría empleados ya 
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
replace firmapeq_ci=1 if tamest<5
replace firmapeq_ci=0 if tamest>=5 & tamest<99
replace firmapeq_ci=. if categ==9
replace firmapeq_ci=. if emp==0

/* No se le pregunta a los empleados domesticos, en la codificación se les asignó cero
lo reeplazamos por missing*/
*/
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

ren ocup ocup_aux
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

gen durades_ci=tpobusm/4.3 if tpobusm>0 & tpobusm<999 /*La variable debe llevarse a meses*/
*replace durades_ci=. if tpobusm==0 /*No aplica*/
*replace durades_ci=. if tpobusm==999 /*No responde*/ 
*replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.



*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

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
replace ingrneto=0 if ingrneto<0 /*Son 7 observaciones*/
replace ingrneto=999999 if ingrbru==9999999 | costos==9999999

gen yprijbi=.
replace yprijbi=ingrneto*30 if frecingr==1
replace yprijbi=ingrneto*4.3 if frecingr==2
replace yprijbi=ingrneto*2 if frecingr==3
replace yprijbi=ingrneto if frecingr==4 | frecingr==9
replace yprijbi=ingrneto/2 if frecingr==5
replace yprijbi=ingrneto/3 if frecingr==6
replace yprijbi=ingrneto/6 if frecingr==7
replace yprijbi=ingrneto/12 if frecingr==8
replace yprijbi=999999 if ingrneto==999999 | frecingr==99
replace yprijbi=. if categopri_ci>2


*replace yprijbi=0 if categopri_ci<=2 & yprijbi==. & emp_ci==1 

/*Ojo con esto último. Originalmente la encuesta conputa una serie de 
missings (88 casos)que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/
/*Para los trabajadores independientes*/

egen ylmpri_ci=rsum(yprijbi yprid)
replace ylmpri_ci=. if yprijbi==999999 | yprid==999999
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp==0

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

egen ylnmpri1_ci=rsum(ylnmpri_ci valaut)
replace ylnmpri1_ci=. if ylnmpri_ci==. & valaut==.
replace ylnmpri1_ci=ylnmpri_ci if valaut==999999
replace ylnmpri1_ci=. if emp_ci==0


***************
***ylmsec_ci***
***************
*Yanira: se eliminan las observaciones para aquellos individuos que no tienen otro empleo o para 
*aquellos que no aplica la pregunta de cuánto gana en su segundo trabajo.

gen ysec1=ysec
replace ysec1=. if ysec==999999 
replace ysec1=. if otrotrab!=1

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
replace remesasext=remescol*fqremescol/12
replace remesasext=. if remescol==999999 | fqremescol==999

gen ayuda=.
replace ayuda=ayudacol*fqayudacol/12
replace ayuda=. if ayudacol==999999 | fqayudacol==999

gen cuotalim=.
replace cuotalim=qotalcol*fqqotalcol/12
replace cuotalim=. if qotalcol==999999 | fqqotalcol==999

gen alqui=.
replace alqui=alquicol*fqalquicol/12
replace alqui=. if alquicol==999999 | fqalquicol==999

gen alqneg=.
replace alqneg=alqnegco*fqalqnegco/12
replace alqneg=. if alqnegco==999999 | fqalqnegco==999

gen jubil=.
replace jubil=jubilcol*fqjubilcol/12
replace jubil=. if jubilcol==999999 | fqjubilcol==999

gen deveh=.
replace deveh=devehcol*fqdevehcol/12
replace deveh=. if devehcol==999999 | fqdevehcol==999

gen otros=.
replace otros=otroscol*fqotroscol/12
replace otros=. if otroscol==999999 | fqotroscol==999

gen utilidades=.
replace utilidades=utilcol/12
replace utilidades=. if utilcol==999999

gen dividendos=.
replace dividendos=divaccol/12
replace dividendos=. if divaccol==999999

gen intereses=.
replace intereses=intercol/12
replace intereses=. if intercol==999999

gen herencias=.
replace herencias=herencol/12
replace herencias=. if herencol==999999

gen indemnizacion=.
replace indemnizacion=indemcol/12
replace indemnizacion=. if indemcol==999999

gen ayudagob=.
replace ayudagob=aygobcol/12
replace ayudagob=. if aygobcol==999999

gen acteventual=.
replace acteventual=acevecol/12
replace acteventual=. if acevecol==999999

gen arrendamiento=.
replace arrendamiento=alqtiecol/12
replace arrendamiento=. if alqtiecol==999999

gen otross=.
replace otross=otroscol443/12
replace otross=. if otroscol443==999999

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross)
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & acteventual==. & arrendamiento==. & otross==. 

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=remesasext
replace remesas_ci=. if remesasext==0

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

******************
*** remesas_ch ***
******************

rename r704 frecuna
rename r705 cantida
rename r707 ayudaes

gen remesash=.
replace remesash=cantida/12 if frecuna==1
replace remesash=cantida/6 if frecuna==2
replace remesash=cantida/3 if frecuna==3
replace remesash=cantida/2 if frecuna==4
replace remesash=cantida if frecuna==5
replace remesash=cantida*2 if frecuna==6
replace remesash=. if cantida==99999 | cantida==0 /*No aplicable, no responde*/

gen remesasnm=.
replace remesasnm=ayudaes/12
replace remesasnm=. if ayudaes==999999

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

*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=r311
replace rentaimp=. if r311==99999

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
replace aedu_ci=0 if nivelcursa==1 

replace aedu_ci=1 if nivelcursa==2 & nivedu==1
replace aedu_ci=2 if nivelcursa==2 & nivedu==2
replace aedu_ci=3 if nivelcursa==2 & nivedu==3
replace aedu_ci=4 if nivelcursa==2 & nivedu==4
replace aedu_ci=5 if nivelcursa==2 & nivedu==5
replace aedu_ci=6 if nivelcursa==2 & nivedu==6
replace aedu_ci=7 if nivelcursa==2 & nivedu==7
replace aedu_ci=8 if nivelcursa==2 & nivedu==8
replace aedu_ci=9 if nivelcursa==2 & nivedu==9

replace aedu_ci=10 if nivelcursa==3 & nivedu==10
replace aedu_ci=11 if nivelcursa==3 & nivedu==11
replace aedu_ci=12 if nivelcursa==3 & nivedu==12

replace aedu_ci=13 if nivelcursa==4 & nivedu==1
replace aedu_ci=14 if nivelcursa==4 & nivedu==2
replace aedu_ci=15 if nivelcursa==4 & nivedu==3
replace aedu_ci=16 if nivelcursa==4 & nivedu==4
replace aedu_ci=17 if nivelcursa==4 & nivedu==5
replace aedu_ci=18 if nivelcursa==4 & nivedu==6
replace aedu_ci=19 if nivelcursa==4 & nivedu==7
replace aedu_ci=20 if nivelcursa==4 & nivedu==8
replace aedu_ci=22 if nivelcursa==4 & nivedu==10
replace aedu_ci=23 if nivelcursa==4 & nivedu==11
replace aedu_ci=27 if nivelcursa==4 & nivedu==15

replace aedu_ci=13 if nivelcursa==5 & nivedu==1
replace aedu_ci=14 if nivelcursa==5 & nivedu==2
replace aedu_ci=15 if nivelcursa==5 & nivedu==3


/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if ultnivel==0 & (hasist==1 | hasist==2)
replace aedu_ci=0 if ultnivel==1

replace aedu_ci=1 if ultnivel==2 & ultgrado==1
replace aedu_ci=2 if ultnivel==2 & ultgrado==2
replace aedu_ci=3 if ultnivel==2 & ultgrado==3
replace aedu_ci=4 if ultnivel==2 & ultgrado==4
replace aedu_ci=5 if ultnivel==2 & ultgrado==5
replace aedu_ci=6 if ultnivel==2 & ultgrado==6
replace aedu_ci=7 if ultnivel==2 & ultgrado==7
replace aedu_ci=8 if ultnivel==2 & ultgrado==8
replace aedu_ci=9 if ultnivel==2 & ultgrado==9
replace aedu_ci=9 if ultnivel==2 & ultgrado==11
replace aedu_ci=9 if ultnivel==2 & ultgrado==12

replace aedu_ci=10 if ultnivel==3 & ultgrado==10
replace aedu_ci=11 if ultnivel==3 & ultgrado==11
replace aedu_ci=12 if ultnivel==3 & ultgrado==12

replace aedu_ci=13 if ultnivel==4 & ultgrado==1
replace aedu_ci=14 if ultnivel==4 & ultgrado==2
replace aedu_ci=15 if ultnivel==4 & ultgrado==3
replace aedu_ci=16 if ultnivel==4 & ultgrado==4
replace aedu_ci=17 if ultnivel==4 & ultgrado==5
replace aedu_ci=18 if ultnivel==4 & ultgrado==6
replace aedu_ci=19 if ultnivel==4 & ultgrado==7
replace aedu_ci=20 if ultnivel==4 & ultgrado==8
replace aedu_ci=21 if ultnivel==4 & ultgrado==9
replace aedu_ci=22 if ultnivel==4 & ultgrado==10
replace aedu_ci=23 if ultnivel==4 & ultgrado==11
replace aedu_ci=24 if ultnivel==4 & ultgrado==12
replace aedu_ci=27 if ultnivel==4 & ultgrado==15

replace aedu_ci=13 if ultnivel==5 & ultgrado==1
replace aedu_ci=14 if ultnivel==5 & ultgrado==2
replace aedu_ci=15 if ultnivel==5 & ultgrado==3

replace aedu_ci=. if aedu_ci>edad & aedu_ci~=. 
/*Hay un caso en donde los años de educación son mayores a la edad*/

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
replace eduac_ci=1 if (ultnivel==4 | nivelcursa==4) & aedu_ci~=.
replace eduac_ci=0 if (ultnivel==5 | nivelcursa==5) & aedu_ci~=.
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
replace pqnoasis_ci=. if pqnoestu==0
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 " Enfermedad", add 
label define pqnoasis_ci 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis_ci 7 "Finalizo sus estudios" 8 "No existe escuela cercana o cupo", add
label define pqnoasis_ci 9 "Otros" 99 "No responde", add
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


gen aguared_ch=(r313==1 | r313==2)

gen aguadist_ch=1 if r313==1
replace aguadist_ch=2 if r313==2
replace aguadist_ch=3 if r313>=2 & r313<=9

gen aguamala_ch=(r314==2)
replace aguamala_ch=. if r314==9

gen aguamide_ch=.
/*NA*/

gen luz_ch=(r312==1 | r312==2)
replace luz_ch=. if r312==9

gen luzmide_ch=.
/*NA*/

gen combust_ch=(r312>=1 & r312<=3)
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

gen resid_ch=0 if r324==1 | r324==2
replace resid_ch=1 if r324==4 | r324==5
replace resid_ch=2 if r324==6
replace resid_ch=3 if r324==3 | r324==7

gen dorm_ch=r306

gen cuartos_ch=r305

gen cocina_ch=.
/*NA*/

gen telef_ch=(r323>=1 & r323<=7)
replace telef_ch=. if r323==9

gen refrig_ch=(r32505==1)
replace refrig_ch=. if r32505==9

gen freez_ch=.
/*NA*/

gen auto_ch=(r32512==1)
replace auto_ch=. if r32512==9


gen compu_ch=(r32509==1)
replace compu_ch=. if r32509==9

gen internet_ch=(r32510==1)
replace internet_ch=. if r32510==9


gen cel_ch=(r323>=2 & r323<=7)
replace cel_ch=. if r323==9


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

gen vivialqimp_ch=r311
replace vivialqimp_ch=. if r311==99999

************************
*** El SALVADOR 2000 ***
************************

/* 
103. Relación de parentesco
 1. Jefe(a)
 2. Esposo(a)
 3. Hijo(a)
 4. Padre/madre 
 5. Yerno/nuera
 6. Nieto(a)
 7. Suegro(a)
 8. Otros familiares
 9. Empleada doméstica 
 10.Otros
*/

 count
 tab parentco
 
 gen	 incl=1 if (parentco>=1 & parentco<=10)
 replace incl=0 if (parentco==9)
 
 
 
** AREA


** Gender classification of the population refering to the head of the household.

 sort idh_ch nrorden

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(idh_ch)
 
 tab sexo sexo_d if parentco==1

 sort idh_ch nrorden
 
** Years of Education
/*
r202a (alfabet)
202. ¿SABE...(NOMBRE)... LEER Y ESCRIBIR? 
1. Si 
2. No Pase a 203
 r202b
 SI CONTESTÓ 1, ¿DÓNDE APRENDIÓ?
 1. Centro educativo o escuela 4. Escuela nocturna 
 2. Familiares, amigo o vecino 5. Otros
 3. Círculos de alfabetización

r203 (ESTUDIA)
203. ¿ESTUDIA ACTUALMENTE O ASISTE A UN CENTRO MATERNAL? 
 1. Si 
 2. No ==> Si es menor de 4 años termine, si no pase a 216

r204 (NIVEL)						r205  (GRADO)
204. ¿QUÉ NIVEL CURSA O ASISTE ACTUALMENTE?		205. ¿QUÉ GRADO O CURSO ESTUDIA ACTUALMENTE EN ESE NIVEL?
 0. Maternal 				==>214			     0
 1. Parvularia 							(1º  a 3º )
 2. Básica   							(1º  a 9º )
 3. Media      							(10º a 12º)
 4. Superior universitario 					(1º  a 15º)
 5. Superior no universitario 					(1º  a 3º ) 
 6. Educación de adultos 					(1º  a 12º)
 7. Educación especial 						(1º  a 12º)
 8. Otros							    ---

r216
216. ¿SE MATRICULÓ...(NOMBRE)... DURANTE ESTE AÑO LECTIVO?

r217 (HASIST)
217. ¿HA ASISTIDO ALGUNA VEZ...(NOMBRE)... A UN CENTRO DE ENSEÑANZA?
 1. Si
 2. No ==> 221

r218
218. EL CENTRO DE ENSEÑANZA AL QUE ASISTIÓ ERA?
 1. ¿Oficial? 2. ¿Laico? 3. ¿Religioso?

219 ¿CUÁL FUE EL ÚLTIMO NIVEL Y GRADO QUE APROBÓ?

r219a	(ULTNIVEL)					r219b (ULTGRADO) 
 0. Ninguno 							     0
 1. Parvularia 							(1º  a 3º ) 
 2. Básica 							(1°  a 9° ) 
 3. Media 							(10° a 12°) 
 4. Superior universitario 					(1º  a 15º)
 5. Superior no universitario 					(1º  a 3º )
 6. Educación de adultos 					(1º  a 12º)
 7. Educación especial 						(1º  a 12º)
 8. Otros
*/

 gen nivel = nivelcursa 
 gen grado = nivedu

 
 gen anoest=.
 replace anoest=0  if  ultnivel==0 | ultnivel==1  | (ultnivel==2 & ultgrado==0)  | (ultnivel==6 & ultgrado==0)  | (ultnivel==7 & ultgrado==0) | (nivel==0) | nivel==1 | (nivel==2 & grado==1) | (nivel==6 & grado==1) | (nivel==7 & grado==1)| hasist==2
 replace anoest=1  if (ultnivel==2 & ultgrado==1) | (ultnivel==6 & ultgrado==1)  | (ultnivel==7 & ultgrado==1)  | (nivel==2 & grado==2) | (nivel==6 & grado==2) | (nivel==7 & grado==2)
 replace anoest=2  if (ultnivel==2 & ultgrado==2) | (ultnivel==6 & ultgrado==2)  | (ultnivel==7 & ultgrado==2)  | (nivel==2 & grado==3) | (nivel==6 & grado==3) | (nivel==7 & grado==3) | ultnivel==8 & ultgrado==2
 replace anoest=3  if (ultnivel==2 & ultgrado==3) | (ultnivel==6 & ultgrado==3)  | (ultnivel==7 & ultgrado==3)  | (nivel==2 & grado==4) | (nivel==6 & grado==4) | (nivel==7 & grado==4)
 replace anoest=4  if (ultnivel==2 & ultgrado==4) | (ultnivel==6 & ultgrado==4)  | (ultnivel==7 & ultgrado==4)  | (nivel==2 & grado==5) | (nivel==6 & grado==5) | (nivel==7 & grado==5) 
 replace anoest=5  if (ultnivel==2 & ultgrado==5) | (ultnivel==6 & ultgrado==5)  | (ultnivel==7 & ultgrado==5)  | (nivel==2 & grado==6) | (nivel==6 & grado==6) | (nivel==7 & grado==6)
 replace anoest=6  if (ultnivel==2 & ultgrado==6) | (ultnivel==6 & ultgrado==6)  | (ultnivel==7 & ultgrado==6)  | (nivel==2 & grado==7) | (nivel==6 & grado==7) | (nivel==7 & grado==7)
 replace anoest=7  if (ultnivel==2 & ultgrado==7) | (ultnivel==6 & ultgrado==7)  | (ultnivel==7 & ultgrado==7)  | (nivel==2 & grado==8) | (nivel==6 & grado==8) | (nivel==7 & grado==8)
 replace anoest=8  if (ultnivel==2 & ultgrado==8) | (ultnivel==6 & ultgrado==8)  | (ultnivel==7 & ultgrado==8)  | (nivel==2 & grado==9) | (nivel==6 & grado==9) | (nivel==7 & grado==9)
 replace anoest=9  if (ultnivel==2 & ultgrado==9) | (ultnivel==3 & ultgrado==0)  | (ultnivel==6 & ultgrado==9)  | (ultnivel==7 & ultgrado==9)  | (nivel==3 & grado==10) | (nivel==6 & grado==10) | (nivel==7 & grado==10)
 replace anoest=10 if (ultnivel==3 & ultgrado==10)| (ultnivel==6 & ultgrado==10)| (ultnivel==7 & ultgrado==10) | (nivel==3 & grado==11) | (nivel==6 & grado==11) | (nivel==7 & grado==11) 
 replace anoest=11 if (ultnivel==3 & ultgrado==11)| (ultnivel==6 & ultgrado==11)| (ultnivel==7 & ultgrado==11) | (nivel==3 & grado==12) | (nivel==6 & grado==12) | (nivel==7 & grado==12) | (ultnivel==2 & ultgrado==11)
 replace anoest=12 if (ultnivel==3 & ultgrado==12)| (ultnivel==4 & ultgrado==0)  | (ultnivel==5 & ultgrado==0)  | (ultnivel==6 & ultgrado==12)  | (ultnivel==7 & ultgrado==12)  | (nivel==4 & grado==1) | (nivel==5 & grado==1)| (ultnivel==2 & ultgrado==12)
 replace anoest=13 if (ultnivel==4 & ultgrado==1) | (ultnivel==5 & ultgrado==1)  | (nivel==4 & grado==2) | (nivel==5 & grado==2)
 replace anoest=14 if (ultnivel==4 & ultgrado==2) | (ultnivel==5 & ultgrado==2)  | (nivel==4 & grado==3) | (nivel==5 & grado==3)
 replace anoest=15 if (ultnivel==4 & ultgrado==3) | (ultnivel==5 & ultgrado==3)  | (nivel==4 & grado==4)
 replace anoest=16 if (ultnivel==4 & ultgrado==4) | (nivel==4 & grado==5)
 replace anoest=17 if (ultnivel==4 & ultgrado==5) | (nivel==4 & grado==6)
 replace anoest=18 if (ultnivel==4 & ultgrado==6) | (nivel==4 & (grado==7 | grado==8 | grado==9))
 replace anoest=19 if (ultnivel==4 & (ultgrado==7 | ultgrado==8 | ultgrado==9))| (nivel==4 & grado==10)
 replace anoest=20 if (ultnivel==4 & ultgrado==10)| (nivel==4 & grado==11)
 replace anoest=21 if (ultnivel==4 & ultgrado==11)| (nivel==4 & grado==12) 
 replace anoest=22 if (ultnivel==4 & ultgrado==12)| (nivel==4 & grado==13)
 replace anoest=23 if (ultnivel==4 & ultgrado==13)| (nivel==4 & grado==14)
 replace anoest=24 if (ultnivel==4 & ultgrado==14)| (nivel==4 & grado==15)
 replace anoest=25 if (ultnivel==4 & ultgrado==15)
 replace anoest=99 if anoest==.


** Economic Active Population 
/*
ACTPR
Actividad principal
0: Ocupados
1: Desocupados
2: Inactivos
*/

gen actpr=actippal 

 gen peaa=0
 replace peaa=1 if actpr==1
 replace peaa=2 if actpr==2
 replace peaa=3 if actpr==3

 gen tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 

 tab peaa [w=factor_ci]
 tab peaa [w=factor_ci] if incl==1
 

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*
r202a (alfabet)
202. ¿SABE...(NOMBRE)... LEER Y ESCRIBIR? 
1. Si 
2. No Pase a 203
 r202b
 SI CONTESTÓ 1, ¿DÓNDE APRENDIÓ?
 1. Centro educativo o escuela 4. Escuela nocturna 
 2. Familiares, amigo o vecino 5. Otros
 3. Círculos de alfabetización

r203 (ESTUDIA)
203. ¿ESTUDIA ACTUALMENTE O ASISTE A UN CENTRO MATERNAL? 
1. Si 
2. No ==> Si es menor de 4 años termine, si no pase a 216

r204 (NIVEL)						r205  (GRADO)
204. ¿QUÉ NIVEL CURSA O ASISTE ACTUALMENTE?		205. ¿QUÉ GRADO O CURSO ESTUDIA ACTUALMENTE EN ESE NIVEL?
0. Maternal 				==>214			     0
1. Parvularia 							(1º  a 3º )
2. Básica   							(1º  a 9º )
3. Media      							(10º a 12º)
4. Superior universitario 					(1º  a 15º)
5. Superior no universitario 					(1º  a 3º ) 
6. Educación de adultos 					(1º  a 12º)
7. Educación especial 						(1º  a 12º)
8. Otros							    ---

*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen     NERP=0 if (edad>=7 & edad<=12) & (estudia==1 | estudia==2)
 replace NERP=1 if (edad>=7 & edad<=12) & (estudia==1) & ((nivel==2) & (grado>=1 & grado<=6))
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (estudia==1 | estudia==2)
 replace NERS=1 if (edad>=13 & edad<=18) & (estudia==1) & ( (nivel==2 & (grado>=7 & grado<=9)) | (nivel==3 & (grado>=10 & grado<=12)) )

** Upper secondary
* Educación Media

 gen     NERS2=0 if (edad>=16 & edad<=18) & (estudia==1 | estudia==2)
 replace NERS2=1 if (edad>=16 & edad<=18) & (estudia==1) & (nivel==3 & (grado>=10 & grado<=12))
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old*
* Read & write


 gen     LIT2=0 if (edad>=15 & edad<=24) & (alfabet>=1 & alfabet<=2)
 replace LIT2=1 if (edad>=15 & edad<=24) & (alfabet==1)

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  (estudia==1 & nivel==2 & (grado>=1 & grado<=6))
 gen sec=1  if  (estudia==1 & nivel==2 & (grado>=7 & grado<=9)) | (estudia==1 & nivel==3 & (grado>=10 & grado<=12))
 gen ter=1  if  (estudia==1 & ((nivel==4 & (grado>=1 & grado<=5)) | nivel==5))

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
/*
r417 (CATEG)						RAMAR
EN SU OCUPACION PRINCIPAL TRABAJA EN CALIDAD DE:	RAMA DE ACTIVIDAD ECONÓMICA DE 
 01. ¿Empleador o patrono?				LOS OCUPADOS A UN DÍGITO
 02. ¿Cuenta propia con local? 
 03. ¿Cuenta propia sin local? 
 04. ¿Cooperativista?
 05. ¿Familiar no remunerado? 
 06. ¿Asalariado permanente? 
 07. ¿Asalariado temporal? 
 08. ¿Aprendiz? 
 09. ¿Servicio doméstico? 
 10. Otros
*/

gen ramar= rama

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ==6 | categ==7) & (ramar>=20 & ramar<=160)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==6 | categ==7) & (ramar>=20 & ramar<=160) & sexo==2

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categ==6 | categ==7 | categ==9) & (ramar>=20 & ramar<=160)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==6 | categ==7 | categ==9) & (ramar>=20 & ramar<=160) & sexo==2

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator
/*
312. ¿QUÉ TIPO DE ALUMBRADO POSEE ESTA VIVIENDA?
 1. Electricidad
 2. Conexión eléctrica vecino
 3. Kerosene (gas)
 4. Candela
 5. Otra clase ________________________
*/

 rename r312 servalum

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (servalum>=1 & servalum<=5) /* Total population excluding missing information */
 replace ELEC=1 if (servalum>=1 & servalum<=2)
 	
** Target 9, Indicator: Proportion of the population using solidfuels (%)

/*
r319 (COMBUSTI)
319. ¿QUÉ COMBUSTIBLE UTILIZAN PREDOMINANTEMENTE PARA COCINAR? 

1. Electricidad
2. Kerosene (gas) 
3. Gas propano
4. Leña 
5. Carbón
6. Ninguno 		==> 321 
7. Otro

320. ¿DÓNDE SE ENCUENTRA UBICADA LA COCINA?
*/

* Gender classification of the population refers to the head of the household.

rename r319 combusti

 gen	 SFUELS=0 if (combusti>=1 & combusti<=7)  /* Total population excluding missing information */
 replace SFUELS=1 if (combusti==4 | combusti==5)
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
r313 (SERAGUA)
313. EL ABASTECIMIENTO DE AGUA DE ESTA VIVIENDA PROCEDE DE:
 1. ¿Cañería dentro de la vivienda?
 2. ¿Cañería fuera de la vivienda pero
 3. ¿Cañería del vecino?
 4. ¿Pila o chorro público?
 5. ¿Chorro común?
 6. ¿Camión, carreta o pipa?
 7. ¿Pozo (privado o común)?
 8. ¿Ojo de agua, río o quebrada?
 9. ¿Otros medios?

314. ¿CREE UD QUE EL AGUA DE SU CASA ES ADECUADA PARA BEBER?
 1. Si ==> 316
 2. No

315. ¿QUÉ HACE ENTONCES CON EL AGUA QUE BEBE? 
 1. La hierve antes de usarla
 2. La trata con lejía u otro producto 
 3. La filtra
 4. No le hace nada 
 5. Compra agua purificada
 6. Otros 

316. EL SERVICIO DE BAÑO LO SATISFACE POR:
*/

* Gender classification of the population refers to the head of the household.

 rename r313 seragua

 gen	 WATER=0 if (seragua>=1 & seragua<=9)  /* Total population excluding missing information */
 replace WATER=1 if ((seragua>=1 & seragua<=5) | seragua==7)
 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
317. EL SERVICIO SANITARIO QUE TIENE ESTA VIVIENDA ES DE: 
 1. ¿Inodoro a alcantarillado?
 2. ¿Inodoro a fosa séptica? 
 3. ¿Letrina privada? 
 4. ¿Inodoro común a alcantarillado?
 5. ¿Inodoro común a fosa séptica? 
 6. ¿Letrina común?
 7. No tiene

318. ¿DÓNDE SE ENCUENTRA EL SERVICIO SANITARIO?
 1. Dentro de la vivienda
 2. Fuera de la vivienda
*/

* Gender classification of the population refers to the head of the household.

 gen servsani = r317

 gen	 SANITATION=0 if (servsani>=1 & servsani<=7) /* Total population excluding missing information */
 replace SANITATION=1 if (servsani>=1 & servsani<=2) | (servsani>=4 & servsani<=5)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
/*
r301 (TIPOVIV)
301.TIPO DE VIVIENDA:
 1. Casa privada o independiente
 2. Apartamento 
 3. Pieza en una Casa 
 4. Pieza en un Mesón 
 5. Casa improvisada 
 6. Rancho 
 7. Otro

r308a (TENENCIA)
308. ¿CUÁL ES LA FORMA DE TENENCIA DE ESTA VIVIENDA?
 1. Inquilino				==> Cuota mensual	
 2. Propietario de la vivienda 
    pero la está pagando a plazos	==> Cuota mensual	
 3. Propietario 
 4. Colono
 5. Guardián de la Vivienda
 6. Ocupante gratuito
 7. Otros

r303 (PAREDES)
303. EL MATERIAL PREDOMINANTE EN LAS  PAREDES ES:
 1. ¿Concreto o mixto?
 2. ¿Bahareque? 
 3. ¿Adobe?   
 4. ¿Madera?
 5. ¿Lámina? 
 6. ¿Paja o palma? 
 7. ¿Materiales de desecho? 
 8. ¿Otros materiales?

r304 (PISO)
304. EL MATERIAL PREDOMINANTE EN EL PISO ES
 1. ¿Ladrillo de cemento?
 2. ¿Ladrillo de barro? 
 3. ¿Cemento?
 4. ¿Tierra? 
 5. ¿Otros materiales?

r305 (NROHABIT)
305. SIN CONTAR BAÑOS, COCINA, PASILLO NI  GARAGE ¿CUANTAS HABITACIONES 
SON DE USO EXCLUSIVO DEL HOGAR?

306. ¿CUÁNTAS HABITACIONES SON UTILIZADAS EXCLUSIVAMENTE
COMO DORMITORIOS? 

r307 (NRONEG)
307. ¿CUÁNTAS HABITACIONES SON UTILIZADAS EXCLUSIVAMENTE PARA TRABAJAR O PARA NEGOCIO? 
(No lo utilizan o lo comparten anotar 00)
*/

gen nroneg = r307
gen nrohabit = r305
gen piso = r304
gen paredes = r303
gen tenencia = r308a
gen tipoviv = r301



 gen cuartneg=1 if (nroneg>=1 & nroneg<=4)
 recode cuartneg (.=0)

 gen nrocuart= nrohabit-cuartneg if nrohabit>nroneg /* Not used */
 
 gen pers = miemh
 
 gen persroom = (pers/nrohabit) 

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenencia>=1 & tenencia<=7) & (tipoviv>=1 & tipoviv<=7)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=4 & tenencia<=7) | (tipoviv>=5 & tipoviv<=7))

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if ((paredes>=1 & paredes<=8) & (piso>=1 & piso<=5))         /* Total population excluding missing information */
 replace secten_2=1 if ((paredes>=5 & paredes<=8) | (piso>=4 & piso<=5))

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator
* 304. EL MATERIAL PREDOMINANTE EN EL PISO ES

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=5)
 replace DIRT=1 if (piso==4)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (tasadeso==0 | tasadeso==1) & (edad>=15 & edad<=24)
 replace UNMPLYMENT15=1 if (tasadeso==1)	       & (edad>=15 & edad<=24)

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
/*
r323 (TELEFONO)
323. ¿TIENE UD. TELÉFONO O BEEPER EN USO?
1. Si, fijo 
2. Si, celular 
3. Si, beeper 
4. Si, fijo y celular 
5. Si, fijo, celular y beeper
6. Fijo y beeper
7. Celular y beeper
8. No tiene
*/

gen telefono = r323

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (telefono>=1 & telefono<=8) /* Total population excluding missing information */
 replace TELCEL=1 if  (telefono==1 | telefono==2 | (telefono>=4 & telefono<=7))

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono>=1 & telefono<=8) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1 | telefono==6 | telefono==4 | telefono==5)

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (telefono>=1 & telefono<=8) /* Total population excluding missing information */
 replace CEL=1 if (telefono==2 | telefono==7 | telefono==4 | telefono==5)

** Target 18, Indicator: "Personal computers in use per 100 population"
/*
325. EQUIPAMIENTO DEL HOGAR 	
						Variable
9. Computadora			1. SI , 2. NO	r32509
   Red Email			1. SI , 2. NO	r32510

*/
** Target 18, Indicator: "Personal computers in use per 100 population"
/*
325. EQUIPAMIENTO DEL HOGAR 	
						Variable
9. Computadora			1. SI , 2. NO	r32509
   Red Email			1. SI , 2. NO	r32510
*/

* Gender classification of the population refers to the head of the household.

 gen computad = r32509
 gen redemail = r32510

 gen     COMPUTER=0 if (computad>=1 & computad<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computad==1)
		
* Target 18, Indicator: "Internet users per 100 population"

* Gender classification of the population refers to the head of the household.

 gen	 INTUSERS=0 if (redemail>=1 & redemail<=2) /* Total population excluding missing information */
 replace INTUSERS=1 if (redemail==1)

*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

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
r407 (PQNOBUS)
407. ¿POR QUÉ NO BUSCÓ TRABAJO? 
 01. Creía que no había trabajo disponible en la zona
 02. No sabía como buscar trabajo 
 03. Se cansó de buscar trabajo 
 04. Espera ciclo agrícola
 05. Asiste a un centro de enseñanza o está de vacaciones escolares 
 06. Por obligaciones familiares o personales
 07. Enfermedad o accidente
 08. Quehaceres domésticos 
 09. Jubilado o pensionado
 10. No puede trabajar (discapacitado, anciano, etc.) 
 11. Asiste a un centro de formación académica
 12. Capacitación personal
 13. Otro
 14. Ya encontró trabajo
 15. Espera respuesta del empleador
 16. Si buscó trabajo
*/

 *gen pqnobus = r407

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (pqnobus==1 | pqnobus==2 | pqnobus==3 | pqnobus==9 | pqnobus==13)
	
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



****************
*afiliado_ci****
****************
gen afiliado_ci= .
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
replace tipocontrato_ci=1 if (contrato==1 & categ==6) & categopri_ci==3
replace tipocontrato_ci=2 if (contrato==1 & categ==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (contrato==2 | tipocontrato_ci==.) & categopri_ci==3
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
replace pension_ci=1 if (jubilcol>0 & jubilcol<999999 ) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

gen ypen_ci=jubilcol*fqjubilcol/12 if pension_ci==1
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
gen lp25_ci =35.43584
label var lp25_ci "Linea de pobreza 2.5 dólares, año base 2005"

*********
*lp4_ci***
*********
gen lp4_ci =56.69735
label var lp4_ci "Linea de pobreza 4 dólares, año base 2005"

*************
**salmm_ci***
*************
gen salmm_ci= 124.79 /*fuente: ILO*/

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
gen categosec_ci=.

**Cambio de moneda a partir de 2000 - Modificación Mayra Sáenz Abril 2014

qui sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch

local varing "ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch"
foreach e of local varing {
replace `e' = `e'/8.755 
}

qui sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch




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
*/

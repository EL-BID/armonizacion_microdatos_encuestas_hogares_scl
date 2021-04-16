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
 


*global ruta = "${surveysFolder}"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2004"
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
gen region_c= r004

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

gen factor_ch= fac01 
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************
*Modificación Mayra Sáenz, Abril-2014
sort lote tipo folio viv 
egen idh_ch=group(lote tipo folio viv)
label var idh_ch "ID del hogar

*************
****idp_ci****
**************

gen idp_ci= id_pers
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

gen anio_c=2004
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c= r016m

label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril"
label define mes_c 5 "Mayo" 6 " Junio" 7 "Julio" 8 "Agosto", add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add

label value mes_c mes_c


*****************
***relacion_ci***
*****************

gen relacion_ci= .
replace relacion_ci=1 if r103==1
replace relacion_ci=2 if r103==2
replace relacion_ci=3 if r103==3
replace relacion_ci=4 if r103>=4 & r103<=8
replace relacion_ci=5 if r103==10
replace relacion_ci=6 if r103==9

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

gen factor_ci= fac01
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=r104

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci= r106
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci= .
replace civil_ci=1 if r108==6
replace civil_ci=2 if r108==1 | r108==2
replace civil_ci=3 if r108==4 | r108==5
replace civil_ci=4 if r108==3

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
generat r405=1
replace r405=2 if r40501==2 & r40502==2 & r40503==2  & r40504==2  & r40505==2 & r40506==2 & r40507==2  & r40508==2  & r40509==2  
replace r405=. if r40501==. & r40502==. & r40503==.  & r40504==.  & r40505==. & r40506==. & r40507==.  & r40508==.  & r40509==.  

gen condocup_ci=.
replace condocup_ci=1 if r403==1 | r404==1 | r405==1
replace condocup_ci=2 if r406==1 & (r408>=1 & r408<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
drop r405
*/
* Se considera el limite inferior de la encuesta que es de 10 anios y mas. MGD 06/09/2014
gen condocup_ci=.
replace condocup_ci=1 if r403==1 | r404==1 | (r40501==1 | r40502==1 | r40503==1 | r40504==1  | r40505==1 | r40506==1 | r40507==1  | r40508==1) 
replace condocup_ci=2 if condocup_ci!=1 & (r406==1 | (r407==13 | r407==14) | r408<=8 | r409<=4)
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
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
replace emp_ci=1 if r403 ==1
replace emp_ci=1 if r403 ==2 &  r404 ==1 
/*Se podría considerar empleado a aquellos que contestan la pregunta 405 con un 1
sin embargo a este individuo no se le sigue preguntando como si fuera empleado
sino como si fuera desempleado, así que lo consideramos desempleado*/

****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 &  r406==1)

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(emp_ci==0 &  r406==1 | ( r406==2 & ( r407==14 | r407==13)))

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

gen desalent_ci=(emp_ci==0 &  r406==2 & ( r407==1 |  r407==3))


*****************
***horaspri_ci***
*****************
*destring hrshab, generate (hrshabi)

egen horaspri_ci=  rsum(r411a )  if r403==1
* replace horaspri_ci=hrshabi if r403==2 & r404==1
replace horaspri_ci=. if r411a==99
replace horaspri_ci=. if emp_ci==0


*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci r432)
replace horastot_ci=. if horaspri_ci==. & r432==.
replace horastot_ci=horaspri_ci if r432==99
replace horastot_ci=. if emp_ci==0

******************************
*	subemp_ci
******************************
*NA
/*no se pregunta en la encuesta si se quiere trabajar mas.La encuesta pregunta porque motivo trabaja menos de 40 horas*/
*Alternativa MGD 06/21/2014: aunque no se pregunta si quiere trabajr ams horas se puede inferir con la pregunta de que trabaja
*menor de 40 horas porque solo consiguio empleo parcial o falta de trabajo.
g subemp_ci=0
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (r413== 3 | r413==2)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.


******************
***categopri_ci***
******************

gen categopri_ci=  .
replace categopri_ci=1 if r417==1
replace categopri_ci=2 if r417==2 | r417==3 | r417==4 
replace categopri_ci=3 if r417==6 | r417==7 | r417==8 | r417==9 
replace categopri_ci=4 if r417==5
replace categopri_ci=0 if r417==10

label define categopri_ci 0"Otros" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

/*Dentro de la categoría empleado se incluyen 4 cooperativistas, esta definición es más amplia de 
lo usual, principalmemte porque se incluye a los cooperativistas,pero la encuesta considera 
a todos estos como trabajadores dependientes. 
En el manual del encuestador: página 59 dice: "En el caso de una persona que está asociada 
a la cooperativa y además trabaja como patrono o cuenta propia en tierras que son propiedad 
de la empresa cooperativa, se le anotará código 01 a 03, según el caso. Cuando la persona 
cumple siempre el primer requisito o sea que es asociada y trabaja como jornalero o empleado 
en el trabajo colectivo de la cooperativa se le anotará código igual 04."
Por otra parte el grupo "otros" que son 31 casos se incorpora dentro de la categoría empleados ya 
que la encuesta los considera empleados dependientes y declaran ingresos*/

******************
***categosec_ci***
******************

gen categosec_ci=.
*replace categosec_ci=1 if catant==1
*replace categosec_ci=2 if catant==2 | catant==3
*replace categosec_ci=3 if catant==6 | catant==7 |catant==8 | catant==9 |catant==4 | catant==10
*replace categosec_ci=4 if catant==5

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"
/*
*****************
***contrato_ci***
*****************

gen contrato_ci= .
replace contrato_ci=1 if r418==1
replace contrato_ci=0 if r418==2

/*Esta pregunta se le hace solamente a los asalariados, aprendices u otros
de la pregunta 417, de manera que no incluye a todos los empleados como se define
categopri_ci==3*/


***************
***segsoc_ci***
***************

gen segsoc_ci= .
replace segsoc_ci=1 if r421==1 
replace segsoc_ci=0 if r421==3 | r421==2

/*Consideramos con seguridad social a los que son afiliados*/
/*En este caso no se le pregunta a los que trabajan como servicio domestico*/
*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 &  r431 ==1
replace nempleos_ci=. if pea_ci==0
/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if  r420<5
replace firmapeq_ci=0 if  r420>=5 &  r420<.

/*En este caso no se le pregunta a los que trabajan como servicio domestico.
Y estos son missings */

*/
*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if r419==2
replace spublico_ci=0 if r419==1
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | r417==9 | r417==4
/*Sólo se le hace esta pregunta a los asalariados, aprendices y otros*/


**************
***ocupa_ci***
**************

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
gen rama_ci=.
replace rama_ci=1 if (r416>=11 & r416<=50) & emp_ci==1
replace rama_ci=2 if (r416>=101 & r416<=142) & emp_ci==1
replace rama_ci=3 if (r416>=151 & r416<=372) & emp_ci==1
replace rama_ci=4 if (r416>=401 & r416<=410) & emp_ci==1
replace rama_ci=5 if (r416>=451 & r416<=455) & emp_ci==1
replace rama_ci=6 if (r416>=501 & r416<=552) & emp_ci==1 
replace rama_ci=7 if (r416>=601 & r416<=642) & emp_ci==1
replace rama_ci=8 if (r416>=651 & r416<=749) & emp_ci==1
replace rama_ci=9 if (r416>=751 & r416<=990) & emp_ci==1

/*

*****************
***ocupault_ci***
*****************

gen ocupault_ci=.
replace ocupault_ci=1 if (r435>=2111 & r435<=3480) 
replace ocupault_ci=2 if (r435>=1110 & r435<=1319) 
replace ocupault_ci=3 if (r435>=4110 & r435<=4223) 
replace ocupault_ci=4 if (r435==9110) | (r435>=5220 & r435<=5230) 
replace ocupault_ci=5 if (r435>=5111 & r435<=5210) | (r435>=9120 & r435<=9162) 
replace ocupault_ci=6 if (r435>=6110 & r435<=6210) | (r435>=9210 & r435<=9220) 
replace ocupault_ci=7 if (r435>=7111 & r435<=8340) | (r435>=9311 & r435<=9333) 
replace ocupault_ci=8 if r435==110 & emp_ci==1
replace ocupault_ci=. if emp_ci==0 | r435==9999 | r435==0

****************
***ramault_ci***
****************

gen ramault_ci=.
replace ramault_ci=1 if (r437>=100 & r437<=500) 
replace ramault_ci=2 if (r437>=1000 & r437<=1429)
replace ramault_ci=3 if (r437>=1500 & r437<=3720)
replace ramault_ci=4 if (r437>=4000 & r437<=4100)
replace ramault_ci=5 if (r437>=4500 & r437<=4550) 
replace ramault_ci=6 if (r437>=5000 & r437<=5520) 
replace ramault_ci=7 if (r437>=6000 & r437<=6420) 
replace ramault_ci=8 if (r437>=6500 & r437<=7499) 
replace ramault_ci=9 if (r437>=7500 & r437<=9503) 

******************
***ocupault2_ci***
******************

gen ocupault2_ci=.
replace ocupault2_ci=1 if (r435>=2111 & r435<=3480) & desemp1_ci==1
replace ocupault2_ci=2 if (r435>=1110 & r435<=1319) & desemp1_ci==1
replace ocupault2_ci=3 if (r435>=4110 & r435<=4223) & desemp1_ci==1
replace ocupault2_ci=4 if (r435==9110) | (r435>=5220 & r435<=5230) & desemp1_ci==1
replace ocupault2_ci=5 if (r435>=5111 & r435<=5210) | (r435>=9120 & r435<=9162) & desemp1_ci==1
replace ocupault2_ci=6 if (r435>=6110 & r435<=6210) | (r435>=9210 & r435<=9220) & desemp1_ci==1
replace ocupault2_ci=7 if (r435>=7111 & r435<=8340) | (r435>=9311 & r435<=9333) & desemp1_ci==1
replace ocupault2_ci=8 if r435==110 & emp_ci==1 & desemp1_ci==1
replace ocupault2_ci=. if emp_ci==0 | r435==9999 | r435==0 & desemp1_ci==1

*****************
***ramault2_ci***
*****************

gen ramault2_ci=.
replace ramault2_ci=1 if (r437>=100 & r437<=500) & desemp1_ci==1
replace ramault2_ci=2 if (r437>=1000 & r437<=1429) & desemp1_ci==1
replace ramault2_ci=3 if (r437>=1500 & r437<=3720) & desemp1_ci==1
replace ramault2_ci=4 if (r437>=4000 & r437<=4100) & desemp1_ci==1
replace ramault2_ci=5 if (r437>=4500 & r437<=4550) & desemp1_ci==1 
replace ramault2_ci=6 if (r437>=5000 & r437<=5520) & desemp1_ci==1 
replace ramault2_ci=7 if (r437>=6000 & r437<=6420) & desemp1_ci==1 
replace ramault2_ci=8 if (r437>=6500 & r437<=7499) & desemp1_ci==1 
replace ramault2_ci=9 if (r437>=7500 & r437<=9503) & desemp1_ci==1 

*/

****************
***durades_ci***
****************

gen durades_ci=r409/4.3 if r409>0 & r409<999 /*La variable debe llevarse a meses*/
*replace durades_ci=. if r409==0 /*No aplica*/
*replace durades_ci=. if desemp_ci==0 
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
replace yprid= r423*30  if r422==1
replace yprid= r423*4.3 if r422==2
replace yprid= r423*2   if r422==3
replace yprid= r423     if r422==4 | r422==5
replace yprid= 0 	if r422==6
replace yprid= 999999 	if r423==999999

gen hrsextrasd=.
replace hrsextrasd= r42401a*r42401b/12
replace hrsextrasd= 999999 if r42401a==999999 | r42401b==999

gen vacacionesd=.
replace vacacionesd= r42402a*r42402b/12
replace vacacionesd=999999 if  r42402a==999999 |  r42402b==999

gen aguinaldod=.
replace aguinaldod= r42403a*r42403b/12
replace aguinaldod=999999 if  r42403a==999999 |  r42403b==999

gen bonificacionesd=.
replace bonificacionesd= r42404a*r42404b/12
replace bonificacionesd=999999 if  r42404a==999999 |  r42404b==999

egen yprijbd=rsum( yprid hrsextrasd vacacionesd aguinaldod bonificacionesd), missing
replace yprijbd=999999 if yprid==999999 | hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999 
replace yprijbd=yprid if yprid>0 & yprid~=999999 & ( hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. & hrsextrasd==. & vacacionesd==. & aguinaldod==. & bonificacionesd==. 

/*Para los trabajadores independientes*/


gen ingrneto= r427- r428
replace ingrneto=0 if ingrneto<0 /*Son 7 observaciones*/
replace ingrneto=999999 if  r427==9999999 |  r428==9999999

gen yprijbi=.
replace yprijbi=ingrneto*30  if r426==1
replace yprijbi=ingrneto*4.3 if r426==2
replace yprijbi=ingrneto*2   if r426==3
replace yprijbi=ingrneto     if r426==4 | r426==9
replace yprijbi=ingrneto/2   if r426==5
replace yprijbi=ingrneto/3   if r426==6
replace yprijbi=ingrneto/6   if r426==7
replace yprijbi=ingrneto/12  if r426==8
replace yprijbi=999999       if ingrneto==999999 | r426==99
replace yprijbi=.            if categopri_ci>2

*replace yprijbi=0 if categopri_ci<=2 & yprijbi==. & emp_ci==1 

/*Ojo con esto último. Originalmente la encuesta computa una serie de 
missings que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/

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
replace food1= r42405a*r42405b/12 
replace food1=0 if emp==1 &  r42405a==.
replace food1=. if  r42405a==999999 |  r42405b==999

gen ropa1=.
replace ropa1= r42406a*r42406b/12 
replace ropa1=0 if emp==1 &  r42406a==.
replace ropa1=. if r42406a==999999 | r42406b==999

gen merca1=.
replace merca1= r42407a*r42407b/12 
replace merca1=0 if emp==1 & r42407a==.
replace merca1=. if  r42407a ==999999 |  r42407b ==999

gen vivi1=.
replace vivi1= r42408a*r42408b/12 
replace vivi1=0 if emp==1 & r42408a==.
replace vivi1=. if  r42408a==999999 |  r42408b==999

gen trans1=.
replace trans1= r42409a*r42409b/12 
replace trans1=0 if emp==1 & r42409a==.
replace trans1=. if r42409a==999999 | r42409b==999

gen segur1=.
replace segur1= r42410a*r42410b/12 
replace segur1=0 if emp==1 & r42410a==.
replace segur1=. if  r42410a==999999 |  r42410b==999

gen otross1=.
replace otross1=r42411a*r42411b/12 
replace otross1=0 if emp==1 & r42411a==.
replace otross1=. if r42411a==999999 | r42411b==999

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0

egen ylnmpri1_ci=rsum(ylnmpri_ci r430), missing
replace ylnmpri1_ci=. if ylnmpri_ci==. & r430==.
replace ylnmpri1_ci=ylnmpri_ci if r430==999999
replace ylnmpri1_ci=. if emp_ci==0


***************
***ylmsec_ci***
***************

gen ysec1=r433
replace ysec1=. if r433==999999

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

egen ylmsec1_ci=rsum(ysec1 hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if ysec1==. & hrsextras==. & vacaciones==. & aguinaldo==. & bonificaciones==. 
replace ylmsec1_ci=. if emp_ci==0 | r431 ==1

******************
****ylnmsec_ci****
******************

gen food2=.
replace food2=r43405a*r43405b/12 
replace food2=0 if emp==1 & r43405a==.
replace food2=. if r43405a==999999 | r43405b==999

gen ropa2=.
replace ropa2=r43406a*r43406b/12 
replace ropa2=0 if emp==1 & r43406a==.
replace ropa2=. if r43406a==999999 | r43406b==999

gen merca2=.
replace merca2=r43407a*r43407b/12 
replace merca2=0 if emp==1 & r43407a==.
replace merca2=. if r43407a==999999 | r43407b==999

gen vivi2=.
replace vivi2=r43408a*r43408b/12 
replace vivi2=0 if emp==1 & r43408a==.
replace vivi2=. if r43408a==999999 | r43408b==999

gen trans2=.
replace trans2=r43409a*r43409b/12 
replace trans2=0 if emp==1 & r43409a==.
replace trans2=. if r43409a==999999 | r43409b==999

gen segur2=.
replace segur2=r43410a*r43410b/12 
replace segur2=0 if emp==1 & r43410a==.
replace segur2=. if r43410a==999999 | r43410b==999

gen otross2=.
replace otross2=r43411a*r43411b/12 
replace otross2=0 if emp==1 & r43411a==.
replace otross2=. if r43411a==999999 | r43411b==999

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
replace remesasext=r44301a*r44301b/12
replace remesasext=. if r44301a==999999 | r44301b==999

gen ayuda=.
replace ayuda=r44302a*r44302b/12
replace ayuda=. if r44302a==999999 | r44302b==999

gen cuotalim=.
replace cuotalim=r44303a*r44303b/12
replace cuotalim=. if r44303a==999999 | r44303b==999

gen alqui=.
replace alqui=r44304a*r44304b/12
replace alqui=. if r44304a==999999 | r44304b==999

gen alqneg=.
replace alqneg=r44305a*r44305b/12
replace alqneg=. if r44305a==999999 | r44305b==999

gen jubil=.
replace jubil=r44306a*r44306b/12
replace jubil=. if r44306a==999999 | r44306b==999

gen deveh=.
replace deveh=r44308a*r44308b/12
replace deveh=. if r44308a==999999 | r44308b==999

* rename otros otros1
gen otros=.
replace otros=r44309a*r44309b/12
replace otros=. if r44309a==999999 | r44309b==999

gen utilidades=.
replace utilidades=r44401/12
replace utilidades=. if r44401==999999

gen dividendos=.
replace dividendos=r44402/12
replace dividendos=. if r44402==999999

gen intereses=.
replace intereses=r44403/12
replace intereses=. if r44403==999999

gen herencias=.
replace herencias=r44404/12
replace herencias=. if r44404==999999

gen indemnizacion=.
replace indemnizacion=r44405/12
replace indemnizacion=. if r44405==999999

gen ayudagob=.
replace ayudagob=r44406/12
replace ayudagob=. if r44406==999999

gen acteventual=.
replace acteventual=r44407/12
replace acteventual=. if r44407==999999

gen arrendamiento=.
replace arrendamiento=r44408/12
replace arrendamiento=. if r44408==999999

gen otross=.
replace otross=r44409/12
replace otross=. if r44409==999999

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross), missing
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & acteventual==. & arrendamiento==. & otross==. 

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=remesasext

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

gen remesash=.
/*
replace remesash=cantida/12 if frecuna==1
replace remesash=cantida/6 if frecuna==2
replace remesash=cantida/3 if frecuna==3
replace remesash=cantida/2 if frecuna==4
replace remesash=cantida if frecuna==5
replace remesash=cantida*2 if frecuna==6
*/
gen remesasnm=.
/*
replace remesasnm=ayudaes/12
replace remesasnm=. if ayudaes==999999

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash remesasnm)
replace remesas_ch=. if remesasi==. & remesash==. & remesasnm==.
*/

*egen remesas_ch=sum(remesasext) if miembros_ci==1
*Modificación Mayra Sáenz - Septiembre 2013
by idh_ch, sort: egen remesas_ch=sum(remesasext) if miembros_ci==1
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

gen autocons_ci=r430
replace autocons_ci=. if r430==999999

*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

* gen rentaimp_ch=alquiler
* replace rentaimp=. if alquiler==99999

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

replace aedu_ci=0 if r204==0 & r203==1
replace aedu_ci=0 if r204==1

replace aedu_ci=1 if r204==2 & r205==1
replace aedu_ci=2 if r204==2 & r205==2
replace aedu_ci=3 if r204==2 & r205==3
replace aedu_ci=4 if r204==2 & r205==4
replace aedu_ci=5 if r204==2 & r205==5
replace aedu_ci=6 if r204==2 & r205==6
replace aedu_ci=7 if r204==2 & r205==7
replace aedu_ci=8 if r204==2 & r205==8
replace aedu_ci=9 if r204==2 & r205==9
replace aedu_ci=9 if r204==2 & r205==10

replace aedu_ci=10 if r204==3 & r205==1
replace aedu_ci=10 if r204==3 & r205==8
replace aedu_ci=10 if r204==3 & r205==9
replace aedu_ci=10 if r204==3 & r205==10
replace aedu_ci=11 if r204==3 & r205==11
replace aedu_ci=12 if r204==3 & r205==12

replace aedu_ci=13 if r204==4 & r205==1
replace aedu_ci=14 if r204==4 & r205==2
replace aedu_ci=15 if r204==4 & r205==3
replace aedu_ci=16 if r204==4 & r205==4
replace aedu_ci=17 if r204==4 & r205==5
replace aedu_ci=18 if r204==4 & r205==6
replace aedu_ci=19 if r204==4 & r205==7
replace aedu_ci=20 if r204==4 & r205==8
replace aedu_ci=22 if r204==4 & r205==10
replace aedu_ci=23 if r204==4 & r205==11
replace aedu_ci=24 if r204==4 & r205==12
replace aedu_ci=27 if r204==4 & r205==15

replace aedu_ci=13 if r204==5 & r205==1
replace aedu_ci=14 if r204==5 & r205==2
replace aedu_ci=15 if r204==5 & r205==3
replace aedu_ci=16 if r204==5 & r205==4

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0

/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if r219a==0 & (r217==1 | r217==2)
replace aedu_ci=0 if r219a==1

replace aedu_ci=1 if r219a==2 & r219b==1
replace aedu_ci=2 if r219a==2 & r219b==2
replace aedu_ci=3 if r219a==2 & r219b==3
replace aedu_ci=4 if r219a==2 & r219b==4
replace aedu_ci=5 if r219a==2 & r219b==5
replace aedu_ci=6 if r219a==2 & r219b==6
replace aedu_ci=7 if r219a==2 & r219b==7
replace aedu_ci=8 if r219a==2 & r219b==8
replace aedu_ci=9 if r219a==2 & r219b==9
replace aedu_ci=9 if r219a==2 & r219b==10
replace aedu_ci=9 if r219a==2 & r219b==11
replace aedu_ci=9 if r219a==2 & r219b==12

replace aedu_ci=10 if r219a==3 & r219b==1
replace aedu_ci=10 if r219a==3 & r219b==10
replace aedu_ci=11 if r219a==3 & r219b==11
replace aedu_ci=12 if r219a==3 & r219b==12

replace aedu_ci=13 if r219a==4 & r219b==1
replace aedu_ci=14 if r219a==4 & r219b==2
replace aedu_ci=15 if r219a==4 & r219b==3
replace aedu_ci=16 if r219a==4 & r219b==4
replace aedu_ci=17 if r219a==4 & r219b==5
replace aedu_ci=18 if r219a==4 & r219b==6
replace aedu_ci=19 if r219a==4 & r219b==7
replace aedu_ci=20 if r219a==4 & r219b==8
replace aedu_ci=21 if r219a==4 & r219b==9
replace aedu_ci=22 if r219a==4 & r219b==10
replace aedu_ci=23 if r219a==4 & r219b==11
replace aedu_ci=24 if r219a==4 & r219b==12
replace aedu_ci=25 if r219a==4 & r219b==13
replace aedu_ci=26 if r219a==4 & r219b==14
replace aedu_ci=27 if r219a==4 & r219b==15

replace aedu_ci=13 if r219a==5 & r219b==1
replace aedu_ci=14 if r219a==5 & r219b==2
replace aedu_ci=15 if r219a==5 & r219b==3
replace aedu_ci=16 if r219a==5 & r219b==4
replace aedu_ci=17 if r219a==5 & r219b==5


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
label variable pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 " Enfermedad o discapacidad", add 
label define pqnoasis_ci 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis_ci 7 "Finalizo sus estudios" 8 "No existe escuela cercana o cupo", add
label define pqnoasis_ci 9 "No quiere o no le interesa" 10 "Repite mucho o no trae para estudiar", add
label define pqnoasis_ci 11 "Quehaceres domesticos" 12 "Centro de ensenanza inhabilitado por terremotos", add
label define pqnoasis_ci 13 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if r221 ==3
replace pqnoasis1_ci = 2 if r221 ==1
replace pqnoasis1_ci = 3 if r221 ==4 | r221 ==5
replace pqnoasis1_ci = 4 if r221 ==9
replace pqnoasis1_ci = 5 if r221 ==2 | r221 ==11
replace pqnoasis1_ci = 6 if r221 ==7
replace pqnoasis1_ci = 7 if r221 ==6 
replace pqnoasis1_ci = 8 if r221 ==8  | r221 ==12
replace pqnoasis1_ci = 9 if r221 ==10 | r221 ==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


***************
***repite_ci***
***************
gen repite_ci=.

******************
***repiteult_ci***
******************

gen repiteult_ci=(r207a==1)
replace repiteult_ci=. if asiste_ci==0
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if r210a==1 
replace edupub_ci=2 if r210a==2 | r210a==3

/* Variable r210a:
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
replace aguadist_ch=3 if r313>2 & r313<=9

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
replace dorm_ch=. if r306==99

gen cuartos_ch=r305
replace cuartos_ch=. if r305==99

gen cocina_ch=.
/*NA*/

gen telef_ch=(r3231a==1)
replace telef_ch=. if r3231a==9

gen refrig_ch=(r32505a==1)
replace refrig_ch=. if r32505a==9

gen freez_ch=.
/*NA*/

gen auto_ch=(r32512a==1)
replace auto_ch=. if r32512a==9


gen compu_ch=(r32509a==1)
replace compu_ch=. if r32509a==9

gen internet_ch=(r32510a==1)
replace internet_ch=. if r32510a==9


gen cel_ch=( r3232a==1)
replace cel_ch=. if r3232a ==9


gen vivi1_ch=1 if r301==1
replace vivi1_ch=2 if r301==2
replace vivi1_ch=3 if r301>2 & r301<9

gen vivi2_ch=(r301>=1 & r301<=2)
replace vivi2_ch=. if r301==9

gen viviprop_ch=0 if r308a==1
replace viviprop_ch=1 if r308a==3
replace viviprop_ch=2 if r308a==2
replace viviprop_ch=3 if r308a>=4 & r308a<=7


gen viviitit_ch=.
/*NA*/

gen vivialq_ch=r308c if r308a==1
replace vivialq_ch=. if r308c==99999

*gen vivialqimp_ch=alquiler
*replace vivialqimp_ch=. if alquiler==99999
************************
*** El SALVADOR 2004 ***
************************

 destring, replace

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
 tab r103
 
 gen	 incl=1 if (r103>=1 & r103<=10)
 replace incl=0 if (r103==9)
 
clonevar sexo	=	r104
clonevar edad	=	r106
clonevar depto	=	r004
clonevar factor	=	fac00
clonevar pers	=	miemh
clonevar alfabet=	r202a
clonevar estudia=	r203
clonevar nivel	=	r204
clonevar grado	=	r205
clonevar ultniv	=	r219a
clonevar ultgrad=	r219b
clonevar categ	=	r417
clonevar rama	=	r416
clonevar combusti=	r319
clonevar servagua=	r313
clonevar servsani=	r317
clonevar nrohabit=	r305
clonevar servbano=	r316
clonevar tenencia=	r308a
clonevar tipoviv =	r301
clonevar paredes =	r303
clonevar piso	 =	r304
clonevar tel_fijo=	r3231a
clonevar celular =	r3232a
clonevar pc	     =	r32509a
clonevar redemail=	r32510a
clonevar tamest	 =	r420
clonevar sectorp =	r419
clonevar ocup	 =	r414
clonevar parentco=	r103
clonevar hasist	 =	r217


** AREA


** Gender classification of the population refering to the head of the household.

 sort case_id id_pers

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(case_id)
 

** Years of education. 
* Population with three or more years of age *

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

r203 (estudia)
203. ¿ESTUDIA ACTUALMENTE O ASISTE A UN CENTRO MATERNAL? 
1. Si 
2. No ==> Si es menor de 4 años termine, si no pase a 216

r204 (nivel)						r205  (grado)
204. ¿QUÉ NIVEL CURSA O ASISTE ACTUALMENTE?		205. ¿QUÉ GRADO O CURSO ESTUDIA ACTUALMENTE EN ESE NIVEL?
0. Maternal 				==> 214			     0
1. Parvularia 							(1º  a 3º )
2. Básica   							(1º  a 9º )
3. Media      							(10º a 12º)
4. Superior universitario 					(1º  a 15º)
5. Superior no universitario 					(1º  a 3º ) 
6. Educación especial 						(1º  a 12º)
7. Otros							    ---

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

r219a	(ultniv)					r219b (ultgrad) 
0. Ninguno 							     0
1. Parvularia 							(1º  a 3º ) 
2. Básica 							(1°  a 9° ) 
3. Media 							(10° a 12°) 
4. Superior universitario 					(1º  a 15º)
5. Superior no universitario 					(1º  a 3º )
6. Educación especial 						(1º  a 12º)
7. Otros
*/

 gen	 anoest=.
 replace anoest=0  if ultniv==0 | ultniv==1 	| (ultniv==2 & ultgrad==0 ) | (ultniv==6 & ultgrad==0) | (nivel==0) | nivel==1 | (nivel==2 & grado==1) | (nivel==6 & grado==1) | hasist==2
 replace anoest=1  if (ultniv==2 & ultgrad==1 ) | (ultniv==6 & ultgrad==1 ) | (nivel==2 & grado==2) | (nivel==6 & grado==2) 
 replace anoest=2  if (ultniv==2 & ultgrad==2 ) | (ultniv==6 & ultgrad==2 ) | (nivel==2 & grado==3) | (nivel==6 & grado==3) 
 replace anoest=3  if (ultniv==2 & ultgrad==3 ) | (ultniv==6 & ultgrad==3 ) | (nivel==2 & grado==4) | (nivel==6 & grado==4) 
 replace anoest=4  if (ultniv==2 & ultgrad==4 ) | (ultniv==6 & ultgrad==4 ) | (nivel==2 & grado==5) | (nivel==6 & grado==5) 
 replace anoest=5  if (ultniv==2 & ultgrad==5 ) | (ultniv==6 & ultgrad==5 ) | (nivel==2 & grado==6) | (nivel==6 & grado==6) 
 replace anoest=6  if (ultniv==2 & ultgrad==6 ) | (ultniv==6 & ultgrad==6 ) | (nivel==2 & grado==7) | (nivel==6 & grado==7) 
 replace anoest=7  if (ultniv==2 & ultgrad==7 ) | (ultniv==6 & ultgrad==7 ) | (nivel==2 & grado==8) | (nivel==6 & grado==8) 
 replace anoest=8  if (ultniv==2 & ultgrad==8 ) | (ultniv==6 & ultgrad==8 ) | (nivel==2 & grado==9) | (nivel==6 & grado==9) 
 replace anoest=9  if (ultniv==2 & ultgrad==9 ) | (ultniv==3 & ultgrad==0 ) | (ultniv==6 & ultgrad==9) | (nivel==3 & grado==10) | (nivel==6 & grado==10) 
 replace anoest=10 if (ultniv==3 & ultgrad==10) | (ultniv==6 & ultgrad==10) | (nivel==3 & grado==11) | (nivel==6 & grado==11) 
 replace anoest=11 if (ultniv==3 & ultgrad==11) | (ultniv==6 & ultgrad==11) | (nivel==3 & grado==12) | (nivel==6 & grado==12) 
 replace anoest=12 if (ultniv==3 & ultgrad==12) | (ultniv==4 & ultgrad==0 ) | (ultniv==5 & ultgrad==0) | (ultniv==6 & ultgrad==12) | (nivel==4 & grado==1) | (nivel==5 & grado==1)
 replace anoest=13 if (ultniv==4 & ultgrad==1 ) | (ultniv==5 & ultgrad==1 ) | (nivel==4 & grado==2) | (nivel==5 & grado==2)
 replace anoest=14 if (ultniv==4 & ultgrad==2 ) | (ultniv==5 & ultgrad==2 ) | (nivel==4 & grado==3) | (nivel==5 & grado==3)
 replace anoest=15 if (ultniv==4 & ultgrad==3 ) | (ultniv==5 & ultgrad==3 ) | (nivel==4 & grado==4)
 replace anoest=16 if (ultniv==4 & ultgrad==4 ) | (nivel==4 & grado==5)
 replace anoest=17 if (ultniv==4 & ultgrad==5 ) | (nivel==4 & grado==6)
 replace anoest=18 if (ultniv==4 & ultgrad==6 ) | (nivel==4 & (grado==7 | grado==8 | grado==9))
 replace anoest=19 if (ultniv==4 & (ultgrad==7|ultgrad==8|ultgrad==9))| (nivel==4 & grado==10)
 replace anoest=20 if (ultniv==4 & ultgrad==10) | (nivel==4 & grado==11)
 replace anoest=21 if (ultniv==4 & ultgrad==11) | (nivel==4 & grado==12) 
 replace anoest=22 if (ultniv==4 & ultgrad==12) | (nivel==4 & grado==13)
 replace anoest=23 if (ultniv==4 & ultgrad==13) | (nivel==4 & grado==14)
 replace anoest=24 if (ultniv==4 & ultgrad==14) | (nivel==4 & grado==15)
 replace anoest=25 if (ultniv==4 & ultgrad==15)

  
 
** Economic Active Population 
/*
actpr
Actividad principal
0: Ocupados
1: Desocupados
2: Inactivos
*/

 gen	 peaa=0
 replace peaa=1 if actpr==1
 replace peaa=2 if actpr==2
 replace peaa=3 if actpr==3

 gen	 tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 
 
 
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

r203 (estudia)
203. ¿ESTUDIA ACTUALMENTE O ASISTE A UN CENTRO MATERNAL? 
1. Si 
2. No ==> Si es menor de 4 años termine, si no pase a 216

r204 (nivel)						r205  (grado)
204. ¿QUÉ NIVEL CURSA O ASISTE ACTUALMENTE?		205. ¿QUÉ GRADO O CURSO ESTUDIA ACTUALMENTE EN ESE NIVEL?
0. Maternal 				==>214			     0
1. Parvularia 							(1º  a 3º )
2. Básica   							(1º  a 9º )
3. Media      							(10º a 12º)
4. Superior universitario 					(1º  a 15º)
5. Superior no universitario 					(1º  a 3º ) 
6. Educación especial 						(1º  a 12º)
7. Otros							    ---
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
r417 (categ)						r416 (rama)
EN SU OCUPACION PRINCIPAL TRABAJA EN CALIDAD DE:	¿A QUÉ SE DEDICA LA EMPRESA O INSTITUCIÓN EN LA CUAL TRABAJA?
 01. ¿Empleador o patrono?				RAMA DE ACTIVIDAD ECONÓMICA  
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

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ==6 | categ==7) & (rama>=101 & rama<=990)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==6 | categ==7) & (rama>=101 & rama<=990) & sexo==2

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categ==6 | categ==7 | categ==9) & (rama>=101 & rama<=990)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==6 | categ==7 | categ==9) & (rama>=101 & rama<=990) & sexo==2
	
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

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (r312>=1 & r312<=5) /* Total population excluding missing information */
 replace ELEC=1 if (r312>=1 & r312<=2)

** Target 9, Indicator: Proportion of the population using solidfuels (%)
/*
r319 (combusti)
319. ¿QUÉ COMBUSTIBLE UTILIZAN PREDOMINANTEMENTE PARA COCINAR? 
 1. Electricidad
 2. Kerosene (gas) 
 3. Gas propano
 4. Leña 
 5. Carbón
 6. Ninguno 		==> 321 
 7. Otro

320. ¿DÓNDE SE ENCUENTRA UBICADA LA COCINA?

321. ¿EXISTE UNA HABITACION EXCLUSIVA PARA LA COCINA?
*/

* Gender classification of the population refers to the head of the household.

 gen	 SFUELS=0 if (combusti>=1 & combusti<=7)  /* Total population excluding missing information */
 replace SFUELS=1 if (combusti==4 | combusti==5)
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
r313 (SERAGUA)
313. EL ABASTECIMIENTO DE AGUA DE ESTA VIVIENDA PROCEDE DE:
 1. ¿Cañería dentro de la vivienda?
 2. ¿Cañería fuera de la vivienda pero dentro de la propiedad?
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
 6. La obtiene de otro lugar (pagado o comprado)
 7. Otros

316. EL SERVICIO DE BAÑO LO SATISFACE POR:
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (servagua>=1 & servagua<=9)  /* Total population excluding missing information */
 replace WATER=1 if ((servagua>=1 & servagua<=5) | servagua==7)

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
317. EL SERVICIO SANITARIO QUE TIENE ESTA VIVIENDA ES DE: 
 1. ¿Inodoro a alcantarillado?
 2. ¿Inodoro a fosa séptica? 
 3. ¿Letrina privada? 
 4. ¿Inodoro común a alcantarillado?
 5. ¿Inodoro común a fosa séptica? 
 6. ¿Letrina común?
 7. No tiene ==> 319

318. ¿DÓNDE SE ENCUENTRA EL SERVICIO SANITARIO?
 1. Dentro de la vivienda
 2. Fuera de la vivienda
*/

* Gender classification of the population refers to the head of the household.

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
 7. Temporal (terremoto)
 8. Otro

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

 gen cuartneg=1 if (r307>=1 & r307<=3)
 recode cuartneg (.=0)

 gen nrocuart= nrohabit-cuartneg if nrohabit>r307 /* Not used */

 gen persroom = (pers/nrohabit)

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenencia>=1 & tenencia<=7) & (tipoviv>=1 & tipoviv<=5)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=4 & tenencia<=7) | (tipoviv>=5 & tipoviv<=8))

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
323. ¿TIENE UD. TELEFONO O BEEPER EN USO?
	
			SI/NO	Cuantos?
	1. Fijo		r3231a	r3231b
	2. Celular	r3232a	r3232b
	3. Beeper	r3233a	r3233b	
*/

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (tel_fijo==1 | tel_fijo==2) & (celular==1 | celular==2) /* Total population excluding missing information */
 replace TELCEL=1 if  (tel_fijo==1 		  |  celular==1)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (tel_fijo==1 | tel_fijo==2) /* Total population excluding missing information */
 replace TEL=1 if (tel_fijo==1)

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (celular==1 | celular==2) /* Total population excluding missing information */
 replace CEL=1 if (celular==1)
	
** Target 18, Indicator: "Personal computers in use per 100 population"
/*
325. EQUIPAMIENTO DEL HOGAR 	

				SI/NO		Cuantos?				
9. Computadora			r32509a		r32509b
   Internet y/o correo 			
   electrónico			r32510a		r32509b
*/

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if (pc>=1 & pc<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (pc==1)
		
* Target 18, Indicator: "Internet users per 100 population"

* Gender classification of the population refers to the head of the household.

 gen	 INTUSERS=0 if (redemail>=1 & redemail<=2) /* Total population excluding missing information */
 replace INTUSERS=1 if (redemail==1)
		
*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

** CCA 19. Proportion of children under 15 who are working

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
 12. Otro
 13. Ya encontró trabajo
 14. Espera respuesta del empleador
 15. Si buscó trabajo
*/

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (r407==1 | r407==2 | r407==3 | r407==9 | r407==13)
	
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
gen afiliado_ci= (r109==1 | r109==2)
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (r421==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
replace tipocontrato_ci=1 if (r418==1 & r417==6) & categopri_ci==3
replace tipocontrato_ci=2 if (r418==1 & r417==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (r418==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=1 if r420>=1 & r420<=5
replace tamemp_ci=2 if r420>=6 & r420<=50
replace tamemp_ci=3 if r420>50 & r420!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (r44307a>0 & r44307a!=.) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=r44307a*r44307b/12 if pension_ci==1
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
replace cesante_ci=1 if r410==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
*zona urbana
replace lpe_ci= 127.80	if mes_c==1 & zona_c==1
replace lpe_ci= 125.40	if mes_c==2 & zona_c==1
replace lpe_ci= 125.40	if mes_c==3 & zona_c==1
replace lpe_ci= 126.60	if mes_c==4 & zona_c==1
replace lpe_ci= 128.70	if mes_c==5 & zona_c==1
replace lpe_ci= 129.90	if mes_c==6 & zona_c==1
replace lpe_ci= 129.90	if mes_c==7 & zona_c==1
replace lpe_ci= 131.10	if mes_c==8 & zona_c==1
replace lpe_ci= 131.10	if mes_c==9 & zona_c==1
replace lpe_ci= 133.50	if mes_c==10 & zona_c==1
replace lpe_ci= 133.50  if mes_c==11 & zona_c==1
replace lpe_ci= 132.30  if mes_c==12 & zona_c==1

*zona rural
replace lpe_ci=92.10	if mes_c==1 & zona_c==0
replace lpe_ci=90.90	if mes_c==2 & zona_c==0
replace lpe_ci=90.90	if mes_c==3 & zona_c==0
replace lpe_ci=92.10	if mes_c==4 & zona_c==0
replace lpe_ci=93.60	if mes_c==5 & zona_c==0
replace lpe_ci=94.80	if mes_c==6 & zona_c==0
replace lpe_ci=100.20	if mes_c==7 & zona_c==0
replace lpe_ci=100.20	if mes_c==8 & zona_c==0
replace lpe_ci=103.20	if mes_c==9 & zona_c==0
replace lpe_ci=103.20	if mes_c==10 & zona_c==0
replace lpe_ci=101.70	if mes_c==11 & zona_c==0
replace lpe_ci=99.00    if mes_c==12 & zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*gen salmm_ci= 134.15 /*fuente: ILO*/

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=78 if rama_ci==1
replace salmm_ci=153 if rama_ci==3
replace salmm_ci=158 if rama_ci==6
replace salmm_ci=130 if salmm_ci==.
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
gen tecnica_ci=(r204==5 | nivaprob==5)
label var tecnica_ci "=1 formacion terciaria tecnica"

*****************
**categoinac_ci**
*****************	
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
gen rentaimp_ch=.
gen vivialqimp_ch=.
gen vivitit_ch=.

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




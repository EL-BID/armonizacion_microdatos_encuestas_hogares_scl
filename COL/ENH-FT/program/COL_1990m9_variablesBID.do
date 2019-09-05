* (Versión Stata 13)
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

local PAIS COL
local ENCUESTA ENH-FT
local ANO "1990"
local ronda m9 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Colombia
Encuesta: ENH-FT
Round: m9
Autores: 
Generación nuevas variables LMK: 
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: noviembre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

***************
***region_c ***
***************
gen region_c=.
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************
* Region_BID *
************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
***factor_ch***
***************
gen factor_ch=weight
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************
sort municipal  sector  section stratum block segment dwelling home persons
egen idh_ch= group(municipal  sector  section stratum block segment dwelling home persons)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************
gen idp_ci=orderpers
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
** NOTA: no hay zona urbana/rural hasta 1992 **
gen byte zona_c=.


label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1990
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=9
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril"
label define mes_c 5 "Mayo" 6 " Junio" 7 "Julio" 8 "Agosto", add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
label value mes_c mes_c


*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if relationhead==1
replace relacion_ci=2 if relationhead==2
replace relacion_ci=3 if relationhead==3 | relationhead==4
replace relacion_ci=4 if relationhead==15
replace relacion_ci=5 if relationhead==16 | relationhead==19
replace relacion_ci=6 if relationhead==17 | relationhead==18
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
gen factor_ci=weight
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci=sex
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=age
label variable edad_ci "Edad del individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.

*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if marstat==5
replace civil_ci=2 if marstat==1 | marstat==2
replace civil_ci=3 if marstat==4 
replace civil_ci=4 if marstat==3

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
* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
*gen miembros_ci=(relacion_ci<6)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************


*************
**salmm_ci***
*************
*2014, 10 incorporacion MLO
gen salmm_ci= 	41025
label var salmm_ci "Salario minimo legal"

************
***emp_ci***
************

gen byte emp_ci=0
replace emp_ci=1 if activprin==1
replace emp_ci=1 if activsec==1
replace emp_ci=1 if activweek==1


****************
***desemp1_ci***
****************


** Defined as "buscando trabajo" AND said they had actively looked in past week ***

gen desemp_ci=0
replace desemp_ci=(emp_ci==0 & activprin==2 & seeking1==1)

****************
***desemp2_ci*** 
****************

** Not possible to distinguish between those awaiting a response and those who are not **

gen desemp2_ci=.

****************
***desemp3_ci***
****************

gen desemp3_ci=(emp_ci==0 & activprin==2 & seeking2==1)


*************
***pea1_ci***
*************

gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1


*************
***pea2_ci***
*************

gen pea2_ci=.


*************
***pea3_ci***
*************

gen pea3_ci=0
replace pea3_ci=1 if emp_ci==1 |desemp3_ci==1

*****************
***desalent_ci***
*****************

** No se pregunta si la persona CREE que no conseguira trabajo **

gen desalent_ci=.

***************
***subemp_ci***
***************

gen subemp_ci= (emp_ci==1 & desirework==1)


*****************
***horaspri_ci***
*****************


** Usamos HORAS TRABAJADAS LA ULTIMA SEMANA, pero encuesta tambien contiene HORAS A LA SEMANA TRABAJADA NORMALMENTE **

gen horaspri_ci=hoursperweek if emp_ci==1

** replace horaspri_ci=hrshabi if trabajo==2 & tienetra==1 **

** replace horaspri_ci=. if hoursperweek==99 **
** replace horaspri_ci=. if emp_ci==0    **


*****************
***horastot_ci***
*****************

* solo hay horas del trabajo principal *

gen horastot_ci = horaspri_ci 
replace horastot_ci=. if horaspri_ci==.
replace horastot_ci=. if emp_ci==0


*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if worktype2==6
replace categopri_ci=2 if worktype2==5 
replace categopri_ci=3 if worktype2==2 | worktype2==3 |worktype2==4 
replace categopri_ci=4 if worktype2==1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.


label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
***contrato_ci***
*****************

gen contrato_ci=.


***************
***segsoc_ci***
***************

gen segsoc_ci=.


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.

*****************
***firmapeq_ci***
*****************

gen tamfirma_ci=.


*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if worktype2==3
replace spublico_ci=0 if worktype2==2

replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | worktype2==4 

/*Sólo se le hace esta pregunta a los obreros */


**************
***ocupa_ci***
**************


** FIND OCCUPATIONAL CODES FOR COLOMBIA **

gen ocupa_ci=.
replace ocupa_ci=1 if (occupation>=1 & occupation<=19) & emp_ci==1
replace ocupa_ci=2 if (occupation>=20 & occupation<=29) & emp_ci==1
replace ocupa_ci=3 if (occupation>=30 & occupation<=39) & emp_ci==1
replace ocupa_ci=4 if (occupation>=40 & occupation<=49) & emp_ci==1
replace ocupa_ci=5 if (occupation>=50 & occupation<=59) & emp_ci==1
replace ocupa_ci=6 if (occupation>=60 & occupation<=69) & emp_ci==1
replace ocupa_ci=7 if (occupation>=70 & occupation<=79) & emp_ci==1
replace ocupa_ci=8 if (occupation>=80 & occupation<=89) & emp_ci==1
replace ocupa_ci=9 if (occupation>=90 & occupation<=99) & emp_ci==1



*************
***rama_ci***
*************

** CHECK RAMA CODES FOR COLOMBIA **

gen rama1=occupation
replace rama1=. if rama1==99 
gen rama_ci=.
replace rama_ci=1 if rama1>=1 & rama1<=9  
replace rama_ci=2 if rama1>=10 & rama1<=14  
replace rama_ci=3 if rama1>=15 & rama1<=37  
replace rama_ci=4 if rama1>=40 & rama1<=41  
replace rama_ci=5 if rama1>=45
replace rama_ci=6 if rama1>=50 & rama1<=55  
replace rama_ci=7 if rama1>=60 & rama1<=64  
replace rama_ci=8 if rama1>=65 & rama1<=74  
replace rama_ci=9 if rama1>=75 & rama1<=99  
drop rama1




****************
***durades_ci***
****************

** We assume that time spent looking for work equals time unemployed **

gen durades_ci=r7lookwork1/4.3   /*La variable debe llevarse a meses, no en semanas*/
replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

****************************
     *** ylmpri_ci ***
****************************

/*Para los trabajadores independientes Y patron/empleador*/

gen yprijbi=.
replace yprijbi=incomenetpmon
replace yprijbi=999999 if incomenetpmon==999999
replace yprijbi=. if categopri_ci>2

*replace yprijbi=0 if categopri_ci<=2 & yprijbd==. & emp_ci==1 

/*Ojo con esto último. AVERIGUA si la encuesta computa una serie de 
missings que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/

/*Para los trabajadores dependientes*/

gen yprijbd=.
replace yprijbd=income*30 if periodicity==5
replace yprijbd=income*4.3 if periodicity==4
replace yprijbd=income*3 if periodicity==3
replace yprijbd=income*2 if periodicity==2
replace yprijbd=income if periodicity==1 
replace yprijbd=999999 if income==999999 | periodicity==9
replace yprijbd=. if categopri_ci<3


egen ylmpri_ci=rsum(yprijbi yprijbd)
replace ylmpri_ci=. if yprijbi==999999 | yprijbd==999999
replace ylmpri_ci=. if yprijbd==. & yprijbi==.
replace ylmpri_ci=. if emp==0

********************************
     ***nrylmpri_ci ***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)



*******************************
  *** ylnmpri_ci ***
*******************************


gen ylnmpri_ci=.


***************
***ylmsec_ci***
***************

gen ylmsec_ci=.


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.

**********************
  *** ylm_ci ***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


************************
   *** ylnm_ci ***
************************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

*************
***ynlm_ci***
*************


gen ynlm_ci=.
replace ynlm_ci= incomeother2 if incomeother1==1
replace ynlm_ci=. if incomeother2==999999 
replace ynlm_ci=. if emp_ci==0 | incomeother1==2


****************
***remesas_ci***
****************

gen remesas_ci=.

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


************************
    *** ylm_ch  ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1


****************************
*** ylmnr_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1


**************************
   *** ylnm_ch ***
**************************

gen ylnm_ch=.

******************
*** remesas_ch ***
******************

gen remesas_ch=. 

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1


****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.

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

gen rentaimp_ch=.

******************************
  *** ylhopri_ci ***
******************************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

**************************
  *** ylmho_ci ***
**************************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

**************************
  *** tcylmpri_ci ***
**************************
 
 ** TOP CODE **
 
 gen tcylmpri_ci= .
 replace tcylmpri_ci= 0 if ylmpri_ci>=0 & ylmpri_ci<=999997
 replace tcylmpri_ci= 1 if ylmpri_ci==999998
 


****************************
***VARIABLES DE EDUCACION***
****************************

/* Las variables ATTEND & LEVEL nos permiten identificar los años de educación
para aquellos individuos que actualmente estan estudiando. 
Las variables ATTEND & LEVEL indican el último nivel alcanzado y el año 
alcanzado en dicho nivel, permiten calcular los años de educación para aquellos que
actualmente no asisten a un establecimiento escolar.

En Colombia, la educación primaria dura 5 años y la educación secundaria 6 años y universitaria hasta 5-7 años, dependiendo la carrera*/


gen byte aedu_ci=.
replace aedu_ci=. if level==99

/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/

/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/

replace aedu_ci=0 if level==10 & attend==1
replace aedu_ci=0 if level==20 & attend==1

replace aedu_ci=1 if level==21 & attend==1
replace aedu_ci=2 if level==22 & attend==1
replace aedu_ci=3 if level==23 & attend==1
replace aedu_ci=4 if level==24 & attend==1
replace aedu_ci=5 if level==25 & attend==1

replace aedu_ci=5 if level==30 & attend==1
replace aedu_ci=6 if level==31 & attend==1
replace aedu_ci=7 if level==32 & attend==1
replace aedu_ci=8 if level==33 & attend==1
replace aedu_ci=9 if level==34 & attend==1
replace aedu_ci=10 if level==35 & attend==1
replace aedu_ci=11 if level==36 & attend==1
replace aedu_ci=11 if level==37 & attend==1 
/*note there is one person in this category even though impossible */


replace aedu_ci=11 if level==40 & attend==1
replace aedu_ci=12 if level==41 & attend==1
replace aedu_ci=13 if level==42 & attend==1
replace aedu_ci=14 if level==43 & attend==1
replace aedu_ci=15 if level==44 & attend==1
replace aedu_ci=16 if level==45 & attend==1
replace aedu_ci=17 if level==46 & attend==1
replace aedu_ci=18 if level==47 & attend==1


/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if level==10 & attend==2
replace aedu_ci=0 if level==20 & attend==2

replace aedu_ci=1 if level==21 & attend==2
replace aedu_ci=2 if level==22 & attend==2
replace aedu_ci=3 if level==23 & attend==2
replace aedu_ci=4 if level==24 & attend==2
replace aedu_ci=5 if level==25 & attend==2

replace aedu_ci=5 if level==30 & attend==2
replace aedu_ci=6 if level==31 & attend==2
replace aedu_ci=7 if level==32 & attend==2
replace aedu_ci=8 if level==33 & attend==2
replace aedu_ci=9 if level==34 & attend==2
replace aedu_ci=10 if level==35 & attend==2
replace aedu_ci=11 if level==36 & attend==2
replace aedu_ci=11 if level==37 & attend==2 
/*note there is one person in this category even though impossible */

replace aedu_ci=11 if level==40 & attend==2
replace aedu_ci=12 if level==41 & attend==2
replace aedu_ci=13 if level==42 & attend==2
replace aedu_ci=14 if level==43 & attend==2
replace aedu_ci=15 if level==44 & attend==2
replace aedu_ci=16 if level==45 & attend==2
replace aedu_ci=17 if level==46 & attend==2
replace aedu_ci=18 if level==47 & attend==2


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
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<5
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==5
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>5 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

** SE DEFINE PRIMER CICLO DE SECUNDARIA LOS PRIMEROS 4 DE 6 ANIOS **

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>5 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo" 

***************
***edus2i_ci***
***************

** SE DEFINE SEGUNDO CICLO DE SECUNDARIA LOS ULTIMOS 2 DE 6 ANIOS **

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<11
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==11
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo" 

**************
***eduui_ci***
**************

** we assume a 5-year university career **

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<15
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=15
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************

** NO EXISTE EN EL FORMULARIO PARA COLOMBIA **

gen byte eduac_ci=.


***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if attend==1
label variable asiste_ci "Asiste actualmente a la escuela"

***************
***asispre_ci**
***************
*Variable creada por Ángela López - 08/31/2018
	g asispre_ci=.
	la var asispre_ci "Asiste a educación prescolar"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis=.
label variable pqnoasis "Razones para no asistir a la escuela"
label define pqnoasis 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis 3 "Muy caro" 4 " Enfermedad o discapacidad", add 
label define pqnoasis 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis 7 "Finalizo sus estudios" 8 "No existe escuela cercana o cupo", add
label define pqnoasis 9 "No quiere o no le interesa" 10 "Repite mucho o no trae para estudiar", add
label define pqnoasis 11 "Quehaceres domesticos" 12 "Centro de ensenanza inhabilitado por terremotos", add
label define pqnoasis 13 "Otros", add
label value pqnoasis pqnoasis

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .


***************
***repite_ci***
***************
gen repite_ci=.

/*NA*/

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************


gen aguared_ch=.
replace aguared_ch=1 if water==1
replace aguared_ch=0 if water>=2 & water <=8


gen aguadist_ch=.

gen aguamala_ch=.
replace aguamala_ch=1 if water==3 |water==4 |water==5 |water==6 |water==7 |water==8
replace aguamala_ch=0 if water==1 |water==2 


gen aguamide_ch=.
/*NA*/

/*THE ELECTRICITY VARIABLE SINCE IT RANGES FROM 1-8, with 99% answering 1 or 3*/
gen luz_ch=.
replace luz_ch=1 if electricity==1 
replace luz_ch=0 if electricity==3 

/* solo pregunta si la vivienda tiene conexion con energia electrica*/
/* we assume that 'la principal fuente de iluminacion' es elecricidad*/

gen luzmide_ch=.
/*NA*/

gen combust_ch=.

gen bano_ch=.
replace bano_ch=1 if toilet1==1 | toilet1==3 | toilet1==5
replace bano_ch=0 if toilet1==7
/*code toilet1= 0 as missing */

gen banoex_ch=.
replace banoex_ch=1 if toilet2==2 
replace banoex_ch=0 if toilet2==4
/* range of answers for toilet2 is 0 to 9 with 97% answering 2 or 4 */


* Modificaciones Marcela Rubio Septiembre 2014: pequeña corrección en sintaxis

/*
gen des1_ch=.
replace des1_ch=0 if toilet1==7
replace des1_ch=1 if toilet1==1
replace des1_ch=2 if toilet1==3 | toilet1==5
*/
gen des1_ch=.
replace des1_ch=0 if toilet1==7
replace des1_ch=1 if toilet1==1 | toilet1==3 
replace des1_ch=2 if toilet1==5

gen des2_ch=.
replace des2_ch=0 if toilet1== 7 
replace des2_ch=1 if toilet1== 1 | toilet1== 3 | toilet1== 5 
 

gen piso_ch=0 if floors==7
replace piso_ch=1 if floors<7 
/* assume 2,4,6,8 is instead coded 1,3,5,7 */

gen pared_ch=. if walls==0
replace pared_ch=0 if walls==2 | walls==3 | walls==4 | walls==6 
replace pared_ch=1 if walls==1 | walls==5
/* treat zero as missing  */

gen techo_ch=.
/* NA */

gen resid_ch=.
/* NA */

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch =.
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch =.

gen dorm_ch=bedrooms
replace dorm_ch=. if bedrooms==99

gen cuartos_ch=rooms2
replace cuartos_ch=. if rooms2==99
/* note: rooms1 is vivienda, rooms2 is hogar */

gen cocina_ch=.
replace cocina_ch=1 if kitchen1==3
replace cocina_ch=0 if kitchen1==1 | kitchen1==2 | kitchen1==4 | kitchen1==5
/*  assume zero equals missing  */
/* code NO COCINAN as missing */

gen telef_ch=.
/* coded in dataset but codes range from 1-5 and not clear which is which*/

gen refrig_ch=.
/*NA*/

gen freez_ch=.
/*NA*/

gen auto_ch=.
/*NA*/

gen compu_ch=.
/*NA*/

gen internet_ch=.
/*NA*/

gen cel_ch=.
/*NA*/



/** CHECK THE ACCURACY OF THE ASSUMPTIONS MADE TO CONSTRUCT THESE 6 VARIABLES */
gen vivi1_ch=1 if dwelltype==1
replace vivi1_ch=2 if dwelltype==2
replace vivi1_ch=3 if dwelltype>=3
replace vivi1_ch=. if dwelltype==0
/* code zeroes as missing, range of answers = 0-7 */

gen vivi2_ch=1 if dwelltype==1 | dwelltype==2
replace vivi2_ch=0 if dwelltype>=3
replace vivi2_ch=. if dwelltype==0
/* code zeroes as missing, range of answers = 0-7 */

gen viviprop_ch=0 if ownership==3
replace viviprop_ch=1 if ownership==1
replace viviprop_ch=2 if ownership==2
replace viviprop_ch=3 if ownership==5 | ownership==4
replace viviprop_ch=. if ownership==0 | ownership==9
/* code zeroes and 9's as missing  */


gen vivitit_ch=.
/*NA*/

gen vivialq_ch=rent if viviprop_ch==0 | viviprop_ch==2 | viviprop_ch==3
replace vivialq_ch=. if rentimput==999999

gen vivialqimp_ch=rentimput if viviprop_ch==1 
replace vivialqimp_ch=. if rentimp==999999

compress
saveold "`base_out'", replace
log off
log close


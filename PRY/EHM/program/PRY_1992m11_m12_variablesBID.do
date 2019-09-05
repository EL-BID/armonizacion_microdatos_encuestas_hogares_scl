
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

local PAIS PRY
local ENCUESTA EHM
local ANO "1992"
local ronda m11_m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Paraguay
Encuesta: EIH
Round: Agosto 1997 - Julio 1998
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 4 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear
/* ocupa_ci CODIGO ANTERIOR 

gen ocupa=real(substr(ocupacio,1,2))
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa==11 | ocupa==12) & emp_ci==1
replace ocupa_ci=2 if (ocupa==21) & emp_ci==1
replace ocupa_ci=3 if (ocupa==51 | ocupa==61 |ocupa==62) & emp_ci==1
replace ocupa_ci=4 if (ocupa==71 | ocupa==72) & emp_ci==1
replace ocupa_ci=5 if (ocupa>13 & ocupa<=14 | ocupa==81) & emp_ci==1
replace ocupa_ci=6 if (ocupa==41) & emp_ci==1
replace ocupa_ci=7 if (ocupa>=52 & ocupa<=55) & emp_ci==1
replace ocupa_ci=8 if (ocupa==83) & emp_ci==1
replace ocupa_ci=9 if (ocupa==82| ocupa==99) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1
*/


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
***************

sort id_hogar
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
*Modificación Mayra Sáenz - Septiembre 2014
*Se decide trabajar estrictamente con el área urbana, pues la muestra rural no es representativa.
*Por lo tanto se elimina.
drop if zona==2

gen byte zona_c=1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_ci

************
****pais****
************

gen str3 pais_c="PRY"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1992
label variable anio_c "Anio de la encuesta"

*****************
*** region según BID ***
*****************
gen region_BID_c=.
replace region_BID_c=4 if pais=="PRY" 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
****muestra****
***************
*1990-1994 es solo Asuncion Metropolitana (AMA)

gen muestra_AMA=1
label variable muestra_AMA "Asuncion Metropolitana"
gen muestra_N=0
label variable muestra_N "Muestra Nacional"
gen muestra_U=0
label variable muestra_U "Muestra Urbana"


*********
***mes***
*********

gen mes_c=mesentre
label variable mes_c "Mes de la encuesta"
label define mes_c 6 "Junio" 7 " Julio" 8 "Agosto" 
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre", add
label value mes_c mes_c

*****************
***relacion_ci***
*****************

gen paren=parentco
gen relacion_ci=.
replace relacion_ci=1 if paren==1
replace relacion_ci=2 if paren==2 
replace relacion_ci=3 if paren==3
replace relacion_ci=4 if paren==4
replace relacion_ci=5 if paren==5
replace relacion_ci=6 if paren==6


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

gen sexo_ci=.
replace sexo_ci=1 if sexo==1
replace sexo_ci=2 if sexo==2

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
replace civil_ci=1 if estcivil==5
replace civil_ci=2 if estcivil==1 | estcivil==2 
replace civil_ci=3 if estcivil==3
replace civil_ci=4 if estcivil==4 

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

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"


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


****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*cotizapri_ci***
****************
gen cotizapri_ci=.
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=.
label var cotizasec_ci "Cotizante a la Seguridad Social en actividad sec."

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instpen_ci "Institucion a la que cotiza - variable original de cada pais" 

****************
****condocup_ci*
****************

* Cambios: se considera el limite de edad de la encuesta que es 12 anios. 05/27/2014 MGD
gen condocup_ci=.
replace condocup_ci=1 if (trabajo1>=1 & trabajo1<=3)
replace condocup_ci=2 if (trabajo1>=4 & trabajo1<=6 ) & bustrab==1 & edad_ci>=12
replace condocup_ci=3 if (trabajo1>=7 & trabajo1<=12 ) & edad_ci>=12
recode condocup_ci (.=4) if trabajo1==0 & edad<12

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if trabajo1==5 & edad_ci>=10
replace cesante_ci=0 if (trabajo1==4 | trabajo1==6) & edad_ci>=10
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************

gen pension_ci=1 if (yjubpen>0 & yjubpen<. & yjubpen!=999999999) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
replace yjubpen=. if (yjubpen >= 999999999 & yjubpen!=.)
gen ypen_ci=yjubpen
replace ypen_ci=. if pension_ci==0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=.
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* PRY 1992
gen salmm_ci= 257198
label var salmm_ci "Salario minimo legal"

************
***emp_ci***
************

gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1


*****************
***desalent_ci***
*****************

gen desalent_ci=.
/*no se puede construir*/

***************
***subemp_ci***
***************
*Modificacion: horas de la actividad principal. MGD 06/20/2014
gen subemp_ci=0
replace subemp_ci=1 if nrohrspn<=30 & trabmas==1 & emp_ci==1 


*****************
***horaspri_ci***
*****************

gen hr_seman=nrohrspn
gen hr_sem_s=nrohrstn
gen horaspri_ci=hr_seman if emp_ci==1 

*****************
***horastot_ci***
*****************

gen horastot_ci=hr_sem_s  if emp_ci==1 


*******************
***tiempoparc_ci***
*******************

/*gen tiempoparc_ci=0
replace tiempoparc_ci=1 if nrohrstn>=30 & trabmas==2 & emp_ci==1 */

*10/21/15 MGD: corrección de sintaxis
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if horaspri_ci>=1 & horaspri_ci<30 & trabmas==2 & emp_ci==1


******************
***categopri_ci***
******************
/*
POSICION EN LA OCUPACION
CODIGO DESCRIPCION
1 Empleado público
2 Empleado privado
3 Obrero o jornalero
4 Empleador o Patron 
5 Trabajador por Cuenta Propia
6 Trabajador Familiar no remunerado 
7 Empleado doméstico
*/

gen categopri_ci=.
replace categopri_ci=1 if cateprin ==4
replace categopri_ci=2 if cateprin ==5 
replace categopri_ci=3 if cateprin ==1 | cateprin ==2 | cateprin ==3 | cateprin==7
replace categopri_ci=4 if cateprin ==6
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************
/*no se puede construir*/
gen categosec_ci=.

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if nrocup==1
replace nempleos_ci=2 if nrocup>=2
replace nempleos_ci=. if pea_ci==0

/*
*****************
***firmapeq_ci***
*****************

gen byte firmapeq_ci=.
replace firmapeq_ci=1 if tamest ==2 | tamest==3
replace firmapeq_ci=0 if tamest>=4 & tamest<=7

*/
*****************
***spublico_ci***
*****************
* 10/21/2015 MGD: corrección pequeña para que se tomen en cuenta a todos los ocupados.
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if cateprin ==1  & emp_ci==1

/*gen spublico_ci=.
replace spublico_ci=1 if cateprin ==1 & emp_ci==1
replace spublico_ci=0 if cateprin ~=1 & emp_ci==1
replace spublico_ci=. if cateprin ==1 ==.*/

**************
***ocupa_ci***
**************
*usando codificacion de 1995 ocupasal

gen ocupa_ci=.
replace ocupa_ci=1 if (ocuprin>=1 & ocuprin<=676) & emp_ci==1
replace ocupa_ci=2 if (ocuprin>=680 & ocuprin<=835) & emp_ci==1
replace ocupa_ci=3 if (ocuprin>=840 & ocuprin<=983) & emp_ci==1
replace ocupa_ci=4 if (ocuprin>=990 & ocuprin<=1103) & emp_ci==1
replace ocupa_ci=5 if (ocuprin>=2120 & ocuprin<=2305) & emp_ci==1
replace ocupa_ci=6 if (ocuprin>=1110 & ocuprin<=1247) & emp_ci==1
replace ocupa_ci=7 if (ocuprin>=1260 & ocuprin<=2016)|(ocuprin>=2020 & ocuprin<=2116)& emp_ci==1

replace ocupa_ci=9 if (ocuprin>=2310 & ocuprin<=2352) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1


*************
***rama_ci***
*************
* ramaprin=rama de ocupados
g rama_ci=.
replace rama_ci=1 if (ramaprin>=110 & ramaprin<=149) & emp_ci==1 
replace rama_ci=2 if (ramaprin>=210 & ramaprin<=290) & emp_ci==1
replace rama_ci=3 if (ramaprin>=310 & ramaprin<=394) & emp_ci==1
replace rama_ci=4 if (ramaprin>=410 & ramaprin<=411) & emp_ci==1
replace rama_ci=5 if (ramaprin>=510 & ramaprin<=511) & emp_ci==1
replace rama_ci=6 if (ramaprin>=610 & ramaprin<=614) & emp_ci==1
replace rama_ci=7 if (ramaprin>=710 & ramaprin<=750) & emp_ci==1
replace rama_ci=8 if (ramaprin>=810 & ramaprin<=836) & emp_ci==1
replace rama_ci=9 if (ramaprin>=910 & ramaprin<=999) & emp_ci==1


/*
gen rama_ci=ramar 
replace rama_ci=. if emp_ci~=1
replace rama_ci=. if rama_ci==0
*/
****************
***durades_ci***
****************


gen durades_ci=.
replace durades_ci= .11627907 if tpobus==1
replace durades_ci= .58139535 if tpobus==2
replace durades_ci= 2 if tpobus==3
replace durades_ci= 7.5 if tpobus==4
replace durades_ci= 15 if tpobus==5

gen tdurades_ci=tpobus
/* tiene un comportamiento muy estraño*/
/* son unas duraciones pero no son en numeros sino en intervalos*/
*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.
/*no disponible*/
*******************
***tamemp_ci***
*******************
/*
tamest:
           0 no aplicable
           1 emp. domestico
           2 solo/cuenta propia
           3 2 a 5 personas
           4 6 a 10 personas
           5 11 a 20 personasa
           6 21 a 50 personas
           7 >50 personas
           8 no sabe

*/
*Paraguay Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50
gen tamemp_ci = 1 if tamest>=2 & tamest<=3
replace tamemp_ci = 2 if (tamest>=4 & tamest<=6)
replace tamemp_ci = 3 if (tamest==7)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (trabajo1==9 & condocup_ci==3)
replace categoinac_ci = 2 if  (trabajo1==7 & condocup_ci==3)
replace categoinac_ci = 3 if  (trabajo1==8 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
gen byte formal_ci=. /*Las variables afiliado y cotizando se generan como valores perdidos*/
label var formal_ci "1=afiliado o cotizante / PEA"
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

****************
***ylmpri_ci ***
****************

/*Ingresos netos por remuneracion al trabajo*/
gen ylmpri_ci=yprin if emp_ci==1 
replace ylmpri_ci=. if yprin<=0 | yprin==9999999
replace ylmpri_ci=0 if categopri_ci==4



*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


*****************
*** ylnmpri_ci***
*****************

gen ylnmpri_ci=.

***************
***ylmsec_ci***
***************

/*existen unas actividades ocacionales-las pongo en secundario*/
gen ysec1=ysec
replace ysec1=. if ysec==0 | ysec==9999999 /*No aplicable*/

gen ylmsec_ci=ysec1  
replace ylmsec_ci=. if emp_ci~=1
replace ylmsec_ci=. if ysec1==. 

drop ysec1 

*************
*ylmotros_ci*
*************
gen yocasio1=yocasio
replace yocasio1=. if yocasio==0 | yocasio==99999999 | yocasio==9999999 /*No aplicable*/

gen ylmotros_ci= yocasio1
replace ylmotros_ci=. if emp_ci~=1
replace ylmotros_ci=. if yocasio1==.

drop yocasio1

******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.

************
***ylm_ci***
************


egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*************
***ylnm_ci***
*************

gen ylnm_ci=.

*************
***ynlm_ci***
*************

local var="yalqrent yinteres yayudafa yotrosy yjubpen"
foreach x of local var {
gen `x'1=`x'
replace `x'1=. if `x'==0 | `x'==9999999/*No aplicable*/
}

egen ynlm_ci=rsum(yalqrent1 yinteres1 yayudafa1 yotrosy1 yjubpen1), missing
replace ynlm_ci=. if yalqrent1==. & yinteres1==. & yayudafa1==. & yotrosy1==. & yjubpen1==.

drop yalqrent1 yinteres1 yayudafa1 yotrosy1 yjubpen1

*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*****************
***remesas_ci***
*****************

gen remesas_ci=.
gen ynlnm_ci=.
************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

******************
*** nrylmpri_ch***
******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

**********************************
*** remesas_ch & remesasnm_ch ***
**********************************

gen remesash=.

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1, missing
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash), missing
replace remesas_ch=. if remesasi==. 

gen remesasnm_ch=.


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1, missing
egen ynlm_ch=rsum(ynlm remesash), missing
replace ynlm_ch=. if ynlm==. 
drop ynlm

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=remesasnm_ch

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=.

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)


****************************
***VARIABLES DE EDUCACION***
****************************




*** people who have missings
gen byte yedc=.
replace yedc=. if ultano==99

**<5 and no education
replace yedc=0 if ultano<=1 

*** preescolar o jardin o pre-primaria
replace yedc=0 if ultano==10 

*** primaria 
replace yedc=1 if ultano==11 
replace yedc=2 if ultano==12 
replace yedc=3 if ultano==13 

replace yedc=4 if ultano==14 
replace yedc=5 if ultano==15 
replace yedc=6 if ultano==16 




*** secundaria 
replace yedc=7 if ultano==21
replace yedc=8 if ultano==22
replace yedc=9 if ultano==23
replace yedc=10 if ultano==24 
replace yedc=11 if ultano==25
replace yedc=12 if ultano==26| ultano==27 |ultano==28



*** universitario o superior
replace yedc=13 if ultano==31
replace yedc=14 if ultano==32
replace yedc=15 if ultano==33
replace yedc=16 if ultano==34 
replace yedc=17 if ultano==35 
replace yedc=18 if ultano==36
gen byte aedu_ci=yedc

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
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
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

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

***************
***repite_ci***
***************

gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.

***************
***tecnica_ci**
***************

gen tecnica_ci=.

label var tecnica_ci "1=formacion terciaria tecnica"

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(agua==4 | agua==5)


****************
**aguadist_ch***
****************

gen aguadist_ch=lugabast


****************
**aguamala_ch***
****************

gen aguamala_ch=(agua<=3)


****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=luz
replace luz_ch=0 if luz_ch==2

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=tipcocin 
replace combust_ch=0 if combust_ch~=6
replace combust_ch=1 if combust_ch==6

****************
****bano_ch*****
****************


gen bano_ch=(servsani<5)



****************
****banoex_ch***
****************

gen banoex_ch=.


****************
****des1_ch*****
****************

gen des1_ch=.
replace des1_ch=0 if servsani==5 
replace des1_ch=1 if servsani==1 | servsani==3
replace des1_ch=2 if servsani==4 | servsani==2



****************
****des2_ch*****
****************

gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
 
****************
****piso_ch*****
****************

gen piso_ch=.
replace piso_ch=0 if piso==0
replace piso_ch=1 if piso>0


****************
****pared_ch****
****************

gen muros1=pared
gen pared_ch=.
replace pared_ch=0 if muros1>=1
replace pared_ch=1 if muros1>=3 & muros1<=5
replace pared_ch=2 if muros1==6
drop muros1

****************
****techo_ch****
****************
gen techo1=techo
gen techo_ch=.
replace techo_ch=0 if techo1>=1 
replace techo_ch=1 if techo1>=2 & techo1<=6
replace techo_ch=2 if techo1==7
drop techo1

****************
****resid_ch****
****************


gen resid_ch=0 if basura==4
replace resid_ch=1 if basura==2 
replace resid_ch=2 if basura==1 | basura==3
replace resid_ch=3 if basura==5 

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if  agua ==1 | (agua >=4 & agua <=5)
replace aguamejorada_ch = 0 if  (agua >=2 & agua <=3)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (servsani >=1 & servsani <=2)
replace banomejorado_ch = 0 if (servsani >=3 & servsani <=5)

****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=nrodormi 


****************
***cuartos_ch***
****************

gen cuartos_ch=nrocuart

****************
***cocina_ch****
****************

gen cocina_ch=.


****************
****telef_ch****
****************

gen telef_ch=.

****************
****refrig_ch***
****************

gen refrig1=heladera
gen refrig_ch=.
replace refrig_ch=1 if refrig1>0
replace refrig_ch=0 if refrig1==0
drop refrig1

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch****
****************
gen automovil=coches
gen camioneta=camionet
gen auto_ch=.
replace auto_ch=1 if automovil>0 | camioneta>0
replace auto_ch=0 if automovil==0 | camioneta==0
drop automovil camioneta

****************
****compu_ch****
****************

gen compu_ch=. 

****************
**internet_ch***
****************

gen internet_ch=.

****************
****cel_ch******
****************

gen cel_ch=.

****************
****vivi1_ch****
****************

gen vivi1_ch=.
replace vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2


****************
****vivi2_ch****
****************

gen vivi2_ch=0
replace vivi2_ch=1 if tipoviv==1
replace vivi2_ch=0 if tipoviv==2

*******************
****viviprop_ch****
*******************

gen viviprop_ch=.
replace viviprop_ch=0 if tenencia==2 
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=3 if tenencia==3 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=.

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.

*********
*raza_ci*
*********
/*
gen raza_ci=1 if idioma==1 
replace raza_ci=0 if idioma==2 | idioma==3 | idioma==4 | idioma==5
*/

/*
*Mayra Sáenz- Octubre 2013
gen raza_ci=.
replace raza_ci= 1 if idioma ==1 
replace raza_ci= 3 if idioma ==2 | idioma ==3 | idioma ==4 |raza_ci==.

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
*/

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña
gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if idioma ==1 
replace raza_idioma_ci= 3 if idioma ==2 | idioma ==3 | idioma ==4 |raza_idioma_ci==.

gen raza_ci=.

label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1 
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_idioma_ci==2 
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 




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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename ocuprin codocupa
rename ramaprin codindustria

compress


saveold "`base_out'", replace


log close



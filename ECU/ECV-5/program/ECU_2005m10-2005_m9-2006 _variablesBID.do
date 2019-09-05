
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

local PAIS ECU
local ENCUESTA ECV-5
local ANO "2005"
local ronda m10-2005_m9-2006 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Ecuador
Encuesta: ECV-5
Round: Octubre,2005 - Septiembre,2006
Autores: Mayra Sáenz
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


sort ciudad zona sector vivienda hogar 

***************
***factor_ch***
***************

gen factor_ch=factor_f
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

egen idh_ch=group(mregion marea mrgnal mciudad mzona msector mviviend mhogar)
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

/*
marea:
           1 urbano
           2 rural
*/

gen byte zona_c=0 	if marea==2 
replace  zona_c=1 	if marea==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="ECU"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2006
label variable anio_c "Año de la encuesta"

*********
***mes***
*********

gen mes=.
label variable mes "Mes de la encuesta"
label value mes mes

*****************
***relacion_ci***
*****************
/*
pd04:
           1 jefe
           2 esposo(a) o convivienda
           3 hijo (a)
           4 yerno / nuera
           5 nieto / nieta
           6 padre / madre
           7 suegros
           8 hermanos
           9 cuñados
          10 otros parientes
          11 empleada(o) doméstico
          12 pensionistas
          13 otros no parientes
*/


gen relacion_ci=.
replace relacion_ci=1 if pd04==1
replace relacion_ci=2 if pd04==2
replace relacion_ci=3 if pd04==3
replace relacion_ci=4 if pd04>=4 & pd04<=10
replace relacion_ci=5 if pd04==13 
replace relacion_ci=5 if pd04==12 
replace relacion_ci=6 if pd04==11


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

gen factor_ci=factor_f
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

recode edad 99=.
gen edad_ci=edad
label variable edad_ci "Edad del individuo"

*****************
***civil_ci******
*****************
/*
pd07:
           1 unión libre
           2 casado
           3 soltero
           4 separado
           5 divorciado
           6 viudo
*/

gen civil_ci=.
replace civil_ci=1 if pd07==3
replace civil_ci=2 if pd07==1 | pd07==2
replace civil_ci=3 if pd07==4 | pd07==5
replace civil_ci=4 if pd07==6

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
***miembro_ci***
****************

gen miembro_ci=(relacion_ci<6)
label variable miembro_ci "Miembro del hogar"

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

************
***emp_ci***
************

gen byte emp_ci=(pa01==1 | pa02==1 | pa03==1)
replace emp_ci = . if pa01==1 & pa02==1 & pa03==1

****************
***desemp1_ci***
****************
/*
pa04:
           1 empleado de gobierno o privado
           2 jornalero peón agropecuario
           3 trabajador independiente
           4 tratando de establecer negocio
           5 tratando adquirir propiedad para trabajar
           6 en lo que salga
           7 no buscó trabajo
*/


gen desemp1_ci=(pa01==2 & pa04>=1 & pa04<=6)
replace desemp1_ci = . if emp_ci ==.

****************
***desemp2_ci*** 
****************

gen desemp2_ci=(desemp1_ci==1 | pa06==1 | pa06==2)
replace desemp2_ci = . if emp_ci ==.

****************
***desemp3_ci***
****************
gen desemp3_ci=(desemp2_ci==1 | pa05>=1 & pa05<=6)
replace desemp3_ci = . if emp_ci ==.

*************
***pea1_ci***
*************

gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 | desemp1_ci==1
replace pea1_ci=. if emp_ci==. & desemp1_ci==.


*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1
replace pea2_ci=. if emp_ci==. & desemp2_ci==.

*************
***pea3_ci***
*************
gen pea3_ci=.

*****************
***desalent_ci***
*****************

gen desalent_ci=(pa01==2 & pa04==7)
replace desalent_ci = . if emp_ci == .

***************
***subemp_ci***
***************
/*
pa68a:
           1 si
           2 no
*/

gen subemp_ci=(emp_ci==1 & pa68a==1 & pa66<=30)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(emp_ci==1 & pa68a==2 & pa66<=30)

*****************
***horaspri_ci***
*****************

gen horaspri_ci=pa18*pa19  if emp_ci==1 

*****************
***horastot_ci***
*****************
/* horas totales la semana pasada */

gen horastot_ci=pa66  if emp_ci==1 
replace horastot_ci=.  if pa66==999


******************
***categopri_ci***
******************
/*
pa21:
           1 empleado de gobierno
           2 empleado privado
           3 jornalero o peón
           4 patrono
           5 cuenta propia
           6 trabajador del hogar sin pago
           7 trabajador no del hogar sin pago
           8 trabajador agropecuario a sueldo
           9 jornalero o peón agropecuario
          10 patrón de la finca
          11 trabajador agropecuario x cta. propia
          12 ayudante agropecuario del hogar sin pago
          13 ayudante agropecuario no del hogar sin pago
          14 empleado(a) doméstico(a)
*/

gen categopri_ci=.
replace categopri_ci=1 if pa21==4 | pa21==10
replace categopri_ci=2 if pa21==5 | pa21==11
replace categopri_ci=3 if pa21==1 | pa21==2 	| pa21==3 	| pa21==8  | pa21==14
replace categopri_ci=4 if pa21==6 | pa21==12 	| pa21==7 | pa21==13

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

/*Dentro de la categoría empleado se incluyen trabajadores agricolas */

******************
**dcategopri_ci **
******************

gen dcategopri_ci=(pa21>=8 & pa21<=13)
label variable dcategopri_ci "Categoria ocupacional trabajo principal agricola"
/*Varible creada para identificar si su posicion ocupacional es agricola(1) o no(0)*/

******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if pa55==4 | pa55==10
replace categosec_ci=2 if pa55==5 | pa55==11
replace categosec_ci=3 if pa55==1 | pa55==2 	| pa55==3 	| pa55==8  | pa55==14
replace categosec_ci=4 if pa55==6 | pa55==12 	| pa55==7 	| pa55==13

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

******************
**dcategopri_ci **
******************

gen dcategosec_ci=(pa55>=8 & pa55<=13)
label variable dcategosec_ci "Categoria ocupacional trabajo secundario agricola"
/*Varible creada para identificar si su posicion ocupacional es agricola(1) o no(0)*/

*****************
***contrato_ci***
*****************
/*
pa24:
           1 contrato escrito de trabajo
           2 nombramiento
           3 no tiene contrato ni nombramiento
*/

gen contrato_ci=.
replace contrato_ci=1 if pa24==1 
replace contrato_ci=1 if pa24==2
replace contrato_ci=0 if pa24==3

***************
***segsoc_ci***
***************

gen segsoc_ci=.
replace segsoc_ci=1 if pa34a==1
replace segsoc_ci=0 if pa34a==2

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & pa47a==1
replace nempleos_ci=. if pea1_ci==0


*****************
***firmapeq_ci***
*****************
/*
pa20:
           1 1 persona
           2 2 personas
           3 3 personas
           4 4 personas
           5 5 personas
           6 6 a 9 personas
           7 10 a 24 personas
           8 25 a 49 personas
           9 50 a 99 personas
          10 100 y más personas

*/


gen firmapeq_ci=.
replace firmapeq_ci=1 if pa20<=5
replace firmapeq_ci=0 if pa20>5
replace firmapeq_ci=. if pa20==.

*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if pa21==1 & emp_ci==1
replace spublico_ci=0 if pa21~=1 & emp_ci==1
replace spublico_ci=. if pa21==.

**************
***ocupa_ci***
**************

gen ocupa_ci=.
/*
replace ocupa_ci=1 if (pa14>=2100 & pa14<=2460) & emp_ci==1
replace ocupa_ci=2 if (pa14>=1100 & pa14<=1320) & emp_ci==1
replace ocupa_ci=3 if (pa14>=4110 & pa14<=4300) & emp_ci==1
replace ocupa_ci=4 if (pa14>=5110 & pa14<=5300) & emp_ci==1
replace ocupa_ci=5 if (pa14>=2100 & pa14<=2460) & emp_ci==1
replace ocupa_ci=6 if ((pa13>=6000 & pa13<=6200) | pa13==9200) & emp_ci==1
replace ocupa_ci=7 if ((pa13>=7100 & pa13<=8300) | pa13==9300) & emp_ci==1
replace ocupa_ci=8 if (pa13==100) & emp_ci==1
replace ocupa_ci=9 if (pa13==5000 | pa13==5300 | pa13==5400 | pa13==9400) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1
*/

*************************************************************************************************
** No se pueden separar las categorias Comerciantes y Vendedores, de Trabajadores en Servicios **
*************************************************************************************************

*************
***rama_ci***
*************

gen rama_ci     = 1 if pa15 <1000
replace rama_ci = 2 if pa15 >1000  & pa15<1500
replace rama_ci = 3 if pa15 >=1500 & pa15<4000
replace rama_ci = 4 if pa15 >=4000 & pa15<4500
replace rama_ci = 5 if pa15 >=4500 & pa15<5000
replace rama_ci = 6 if pa15 >=5000 & pa15<6000
replace rama_ci = 7 if pa15 >=6000 & pa15<6500
replace rama_ci = 8 if pa15 >=6500 & pa15<7400
replace rama_ci = 9 if pa15 >=7400 & pa15!=.

****************
***durades_ci***
****************

gen durades_ci=.

*******************
***antiguedad_ci***
*******************
* en años

gen antiguedad_ci=(pa16a+pa16b/(12)) 
replace antiguedad_ci=. if desemp2_ci==1 
replace antiguedad_ci=. if emp_ci==0

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

****************************
***ylmpri_ci & ylmpri1_ci***
****************************



/*Para los trabajadores dependientes*/
/*pa35:
           0 no recibe ingresos
      999999 no informa
*/

/*
gen yprid=pa35 if emp_ci==1 
replace yprid=. if categopri_ci<=2
replace yprid=. if pa35==999999 

/*Recibio decimo tercer sueldo*/
gen decsueldo1=.
replace decsueldo1=pa36b/12 if emp_ci==1 
replace decsueldo1=. if pa36b==999999

gen aniversario1=.
replace aniversario1=pa28b/12 if emp_ci==1 

gen horaextra1=.
replace horaextra1=pa38b if emp_ci==1 
replace horaextra1= . if pa38b==999999

gen propinas1=.
replace propinas1=pa39b if emp_ci==1 

gen vacaciones1=.
replace vacaciones1=pa30b/12 if emp_ci==1 
replace vacaciones1=. if pa30b==999999 

gen aguinaldo1=.
replace aguinaldo1=pa30d/12 if emp_ci==1 
replace aguinaldo1=. if pa30d==999999 

egen yprijbd=rsum(yprid decsueldo1 aniversario1 horaextra1 propinas1 vacaciones1 aguinaldo1), missing
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. &  horaextra1==. & vacaciones1==. & aguinaldo1==.  
*/


foreach var of varlist pa35  pa36b pa28b  pa38b  pa39b pa30b pa22a pa23a  {
recode `var' (999999=.)
}

gen yprid=pa35

/*Recibio decimo tercer sueldo*/
gen decsueldo1=.
replace decsueldo1=pa36b/12

gen aniversario1=.
replace aniversario1=pa28b/12 

gen horaextra1=pa38b 

gen propinas1=pa39b

gen vacaciones1=pa30b/12

gen aguinaldo1=pa30d/12

egen yprijbd=rowtotal(yprid decsueldo1 aniversario1 horaextra1 propinas1 vacaciones1 aguinaldo1), missing

/*Para los trabajadores independientes*/
/*
pa22b:
           1 día
           2 semana
           3 quincena
           4 mes
           5 trimestre
           6 semestre
           7 año
*/



* MGR Dic 2015: corrección sintáxis
gen ingrneto=(pa22a*pa22c)/12 

/*
gen ingrneto=		pa22a/12 if emp_ci==1 & pa22b==7 
replace ingrneto=	pa22a/6  if emp_ci==1 & pa22b==6
replace ingrneto=	pa22a/3  if emp_ci==1 & pa22b==5
replace ingrneto=	pa22a    if emp_ci==1 & pa22b==4 
replace ingrneto=	pa22a*2  if emp_ci==1 & pa22b==3
replace ingrneto=	pa22a*4.3 if emp_ci==1 & pa22b==2
replace ingrneto=	pa21*4.3*5 if emp_ci==1 & pa22b==1 
replace ingrneto= .     if pa21==999999
*/

/*
pa23b:
           1 día
           2 semana
           3 quincena
           4 mes
*/

gen yprijbi=pa23a if  pa23b==4 
replace yprijbi=pa23a*2 if   pa23b==3
replace yprijbi=pa23a*4.3 if  pa23b==2
replace yprijbi=pa23a*4.3*5 if  pa23b==1 


egen ylmpri_ci=rowtotal(yprid yprijbi ingrneto), missing

egen ylmpri1_ci=rowtotal(yprijbi yprijbd), missing



********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=(ylmpri1_ci==. & emp_ci==1)


*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************
/*
pa40c:
           1 día
           2 semana
           3 quincena
           4 mes
*/

/*
gen food1=.
replace food1=pa40b if emp_ci==1 & pa40c==4
replace food1=pa40b*2 if emp_ci==1 & pa40c==3
replace food1=pa40b*4.3 if emp_ci==1 & pa40c==2
replace food1=pa40b*30 if emp_ci==1 & pa40c==1
*/

foreach var of varlist pa40b pa42b  pa41b pa43b  {
recode `var' (999999=.)
}


gen food1=.
replace food1=pa40b if pa40c==4
replace food1=pa40b*2 if pa40c==3
replace food1=pa40b*4.3 if pa40c==2
replace food1=pa40b*30 if pa40c==1


/*
  pa42c. al |
  año veces |
 que recibe |
    la ropa |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |      2,386       81.68       81.68
          2 |        415       14.21       95.89
          3 |         64        2.19       98.08
          4 |         29        0.99       99.08
          5 |          4        0.14       99.21
          6 |          1        0.03       99.25
          7 |          9        0.31       99.55
          8 |          2        0.07       99.62
          9 |          1        0.03       99.66
         10 |          3        0.10       99.76
         12 |          5        0.17       99.93
         52 |          2        0.07      100.00
*/

gen ropa1=.
replace ropa1=(pa42b* pa42c)/12 

/*
gen ropa1=.
replace ropa1=pa42b*(52/12) if emp_ci==1  & pa42c==52
replace ropa1=pa42b*(12/12) if emp_ci==1  & pa42c==12
replace ropa1=pa42b*(10/12) if emp_ci==1  & pa42c==10
replace ropa1=pa42b*(9/12)  if emp_ci==1   & pa42c==9
replace ropa1=pa42b*(8/12)  if emp_ci==1   & pa42c==8
replace ropa1=pa42b*(7/12)  if emp_ci==1   & pa42c==7
replace ropa1=pa42b*(6/12)  if emp_ci==1   & pa42c==6
replace ropa1=pa42b*(5/12)  if emp_ci==1   & pa42c==5
replace ropa1=pa42b*(4/12)  if emp_ci==1   & pa42c==4
replace ropa1=pa42b*(3/12)  if emp_ci==1   & pa42c==3
replace ropa1=pa42b*(2/12)  if emp_ci==1   & pa42c==2
replace ropa1=pa42b*(1/12)  if emp_ci==1   & pa42c==1
replace ropa1=0 if emp==1 & pa42b==.
*/
gen merca1=.

/*
gen vivi1=.
replace vivi1=pa41b if emp_ci==1 
replace vivi1=0 if emp==1 
*/
gen vivi1=.
replace vivi1=pa41b

/*
gen trans1=.
replace trans1=pa43b if emp_ci==1 
replace trans1=. if pa43b==999999
*/
gen trans1=.
replace trans1=pa43b

gen segur1=.
gen otross1=.

/*
egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0 
*/
egen ylnmpri_ci=rowtotal(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing

gen ylnmpri1_ci=.
gen otros=.

***************
***ylmsec_ci***
***************
/*
gen ysec1=pa58 
replace ysec1=. if categopri_ci<=2
replace ysec1=. if pa58==999999 

gen sdecsueldo1=.
replace sdecsueldo1= pa59b/12 if emp_ci==1 
replace sdecsueldo1=. if pa59b==999999

gen spropinas1=.
replace propinas1=pa61b if emp_ci==1 
*/

foreach var of varlist pa58   pa59b pa61b pa56a pa57a pa62b  pa78b pa77b {
recode `var' (999999=.)
}


gen ysec1=pa58 

gen sdecsueldo1=.
replace sdecsueldo1= pa59b/12

gen spropinas1=pa61b 

** MGR Dic, 2015: agregamos ingresos que faltan y modificaciones a actuales
gen singrneto=(pa56a*pa56c)/12

gen ysecjbi=		pa57a/12 if pa57b==7 
replace ysecjbi=	pa57a/6  if pa57b==6
replace ysecjbi=	pa57a/3  if pa57b==5
replace ysecjbi=	pa57a    if pa57b==4 
replace ysecjbi=	pa57a*2  if pa57b==3
replace ysecjbi=	pa57a*4.3 if pa57b==2
replace ysecjbi=	pa57a*4.3*5 if pa57b==1 

/*
gen singrneto=		pa56a/12 if emp_ci==1 & pa56b==7 
replace singrneto=	pa56a/6  if emp_ci==1 & pa56b==6
replace singrneto=	pa56a/3  if emp_ci==1 & pa56b==5
replace singrneto=	pa56a    if emp_ci==1 & pa56b==4 
replace singrneto=	pa56a*2  if emp_ci==1 & pa56b==3
replace singrneto=	pa56a*4.3 if emp_ci==1 & pa56b==2
replace singrneto=	pa56a*4.3*5 if emp_ci==1 & pa56b==1 
replace singrneto= .     if pa56a==999999



gen ysecjbi=		pa57a/12 if emp_ci==1 & pa57b==7 
replace ysecjbi=	pa57a/6  if emp_ci==1 & pa57b==6
replace ysecjbi=	pa57a/3  if emp_ci==1 & pa57b==5
replace ysecjbi=	pa57a    if emp_ci==1 & pa57b==4 
replace ysecjbi=	pa57a*2  if emp_ci==1 & pa57b==3
replace ysecjbi=	pa57a*4.3 if emp_ci==1 & pa57b==2
replace ysecjbi=	pa57a*4.3*5 if emp_ci==1 & pa57b==1 
replace ysecjbi= .     if pa57a==999999
*/

gen shoraextra1=.

gen vacaciones=.

gen aguinaldo=.

gen bonificaciones=.

egen ylmsec_ci=rowtotal(ysec1 sdecsueldo1 ysecjbi singrneto shoraextra1 spropinas1 vacaciones aguinaldo bonificaciones), missing
*gen ylmsec_ci=ysec1

egen ylmsec1_ci=rowtotal(ysec1 sdecsueldo1 singrneto shoraextra1 spropinas1 vacaciones aguinaldo bonificaciones), missing

******************
****ylnmsec_ci****
******************

gen food2=pa62b

gen ropa2=.

gen merca2=.

gen vivi2=.

gen trans2=.

gen segur2=.

gen otross2=.

gen ylnmsec_ci=food2

**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rowtotal(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylm1_ci=rowtotal(ylmpri1_ci ylmsec1_ci), missing
replace ylm1_ci=. if ylmpri1_ci==. & ylmsec1_ci==.


************************
***ylnm_ci & ylnm1_ci***
************************

egen ylnm_ci=rowtotal(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

egen ylnm1_ci=rowtotal(ylnmpri1_ci ylnmsec_ci), missing
replace ylnm1_ci=. if ylnmpri1_ci==. & ylnmsec_ci==.

*************
***ynlm_ci***
*************

/*

pa78c:
           1 día
           2 semana
           3 quincena
           4 mes
           5 trimestre
           6 semestre
           7 año
*/

gen remesasext=.
replace remesasext=pa78b/12 	if pa78c==7 
replace remesasext=pa78b/6 	if pa78c==6 
replace remesasext=pa78b/3 	if pa78c==5 
replace remesasext=pa78b 	if pa78c==4 
replace remesasext=pa78b*2 	if pa78c==3 
replace remesasext=pa78b*4 	if pa78c==2 
replace remesasext=pa78b*30 	if pa78c==1 

/*
pa77c:
           1 día
           2 semana
           3 quincena
           4 mes
           5 trimestre
           6 semestre
           7 año
*/

gen ayuda=.
replace ayuda=		pa77b/12 		if  pa77c==7 
replace ayuda=		pa77b/6 		if  pa77c==6 
replace ayuda=		pa77b/3 		if  pa77c==5 
replace ayuda=		pa77b 			if  pa77c==4 
replace ayuda=		pa77b*2 		if  pa77c==3 
replace ayuda=		pa77b*4 		if  pa77c==2 
replace ayuda=		pa77b*30		if  pa77c==1 

gen cuotalim=.

gen alqui=ia0102/totper

gen alqneg=ia0104/totper

gen jubil=ib0102/totper

gen deveh=.

rename otros otros1
gen otros=.

gen utilidades=ib0204/totper

gen dividendos=ia0304/totper

gen intereses=ia0204/totper

gen herencias=ic0202/totper

gen indemnizacion=ic0102/totper

gen ayudagob=ib0104/totper

gen acteventual=ic0302/totper

gen arrendamiento=id0202/totper 

gen otross=id0102/totper 

egen ynlm_ci=rowtotal(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross), missing

****************
***remesas_ci***
****************

gen remesas_ci=remesasext
gen ynlnm_ci=.
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

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembro_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

by idh_ch, sort: egen nrylmpri1_ch=sum(nrylmpri1_ci) if miembro_ci==1
replace nrylmpri1_ch=1 if nrylmpri1_ch>0 & nrylmpri1_ch<.
replace nrylmpri1_ch=. if nrylmpri1_ch==.


************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembro_ci==1
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembro_ci==1

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembro_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembro_ci==1
replace ylmnr1_ch=. if nrylmpri1_ch==1

**************************
*** ylnm_ch & ylnm1_ch ***
**************************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembro_ci==1
by idh_ch, sort: egen ylnm1_ch=sum(ylnm1_ci) if miembro_ci==1

**********************************
*** remesas_ch & remesasnm_ch ***
**********************************

gen remesash=.

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembro_ci==1
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash)
replace remesas_ch=. if remesasi==. 

gen remesasnm_ch=.


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembro_ci==1
egen ynlm_ch=rsum(ynlm remesash)
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

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembro_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=.

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

/*
Maximo nivel alcanzado:
pe45:
           1 ninguno
           2 centro de alfabetización
           3 educación básica?????????
           4 primaria
           5 educac. media o bachillerato
           6 secundaria
           7 post bachillerato
           8 superior
           9 postgrado
*/

* years of education

gen byte yedc=. if pe45==. 
replace yedc=0 if pe45>=1 & pe45<=2
replace yedc=pe46 if pe45==4 | pe45==3 
replace yedc=pe46 + 6 if pe45==5 | pe45==6
replace yedc=pe46 + 10 if pe45==7
replace yedc=pe46 + 12 if pe45==8
replace yedc=pe46 + 17 if pe45==9

/* No se tiene en cuenta centros de alfabetizacion y educacion 
para adultos*/
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
***eduuc_ci***
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
replace eduac_ci=1 if (aedu_ci>12 & pe45==7) & aedu_ci~=.
replace eduac_ci=0 if (aedu_ci>12 & pe45==8) & aedu_ci~=.
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
label value pqnoasis_ci pqnoasis_ci

***************
***repite_ci***
***************

gen repite_ci=.

label variable repite_ci "Esta repitendo el grado o curso"

gen repiteult_ci=.

***************
***edupub_ci***
***************

/*
pe28:
           1 fiscal o del estado
           2 particular privado
           3 municipal, consejo prov...
*/

gen edupub_ci=.
replace edupub_ci=1 if pe28==1 |pe28==3
replace edupub_ci=0 if pe28==2 

/*Variables de Infraestructura del hogar*/

***************
**aguared_ch***
***************

/*
vi19:
           1 red pública
           2 pila, pileta o llave pública
           3 otra fuente por tubería
           4 carro repartidor / triciclo
           5 pozo
           6 río vertiente o acequia
           7 otro, cual
*/

gen aguared_ch=.
replace aguared_ch=1 if vi19==1 | vi19==2 | vi19==3
replace aguared_ch=0 if vi19>3

****************
**aguadist_ch***
****************

/*
vi21:
           1 dentro de la vivienda
           2 fuera de la vivienda pero en el lote
           3 fuera de la vivienda, lote o terreno

*/


gen aguadist_ch=.
replace aguadist_ch=1 if vi21==1 
replace aguadist_ch=2 if vi21==2
replace aguadist_ch=3 if vi21==3

****************
**aguamala_ch***
****************

gen aguamala_ch=.
replace aguamala_ch=0 if vi19>0
replace aguamala_ch=1 if vi19==6 

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

/*
vi29:
           1 empresa eléctrica pública
           2 planta eléctrica privada
           3 paneles solares
           4 vela, candil, mechero, gas
           5 ninguno
*/

gen luz_ch=.
replace luz_ch=1 if vi29==1 | vi29==2
replace luz_ch=0 if vi29>2 

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

/*
vi15:
           1 gas
           2 leña / carbón
           3 electricidad
           4 otro, cual
*/

gen combust_ch=.
replace combust_ch=1 if vi15==1 | vi15==3 
replace combust_ch=0 if vi15==2 | vi15==4  

****************
****bano_ch*****
****************
/*
vi16:
           1 inodoro y alcantarillado
           2 inodoro y pozo séptico
           3 inodoro y pozo ciego
           4 letrina
           5 no tiene
*/

gen bano_ch=.
replace bano_ch=1 if vi16~=5
replace bano_ch=0 if vi16==5
replace bano_ch=. if vi16==.

****************
****banoex_ch***
****************

gen banoex_ch=.
replace banoex_ch=1 if vi17b>=1 & vi17b<=8 
replace banoex_ch=0 if vi17b==0
replace banoex_ch=. if vi17b==.

****************
****des1_ch*****
****************

/*
vi16:
           1 inodoro y alcantarillado
           2 inodoro y pozo séptico
           3 inodoro y pozo ciego
           4 letrina
           5 no tiene
*/

gen des1_ch=.
replace des1_ch=0 if vi16==5
replace des1_ch=1 if vi16==1 | vi16==2
replace des1_ch=2 if vi16==3 | vi16==4

****************
****des2_ch*****
****************

gen des2_ch=.
replace des2_ch=0 if vi16==5
replace des2_ch=1 if vi16==1 | vi16==2
replace des2_ch=2 if vi16==3 | vi16==4
 
****************
****piso_ch*****
****************

/*
vi07:
           1 duela / parquet / tabloncillo
           2 cerámica / baldosa / vinyl
           3 mármol / marmetón
           4 cemento / ladrillo
           5 tabla / tablón no tratado
           6 caña
           7 tierra
           8 otro, cual

*/

gen piso_ch=.
replace piso_ch=0 if vi07==6 | vi07==7
replace piso_ch=1 if vi07==1 | vi07==2 | vi07==3 
replace piso_ch=2 if vi07==8 |vi07==4 | vi07==5  

****************
****pared_ch****
****************

/*
vi05:
           1 hormigón / bloque /ladrillo
           2 asbesto / cemento
           3 adobe / tapia
           4 madera
           5 bahareque (caña y carrizo revestido)
           6 caña
           7 otro
*/

gen pared_ch=.
replace pared_ch=0 if vi05==5 | vi05==3
replace pared_ch=1 if vi05==1 | vi05==2
replace pared_ch=2 if vi05==4 | vi05==6 | vi05==7

****************
****techo_ch****
****************

/*
vi03:
           1 hormigón / losa / cemento
           2 asbesto (eternit)
           3 zinc
           4 teja
           5 palma / paja /hoja
           6 otro / cual
*/


gen techo_ch=.
replace techo_ch=0 if vi03==5 
replace techo_ch=1 if vi03>=1 & vi03<=4
replace techo_ch=2 if vi03==6

****************
****resid_ch****
****************

/*
vi41:
           1 servicio municipal
           2 la botan a la calle, quebrada, río, lote
           3 la queman
           4 reciclan, entierran
           5 otro, cual
*/

gen resid_ch=.
replace resid_ch=0 if vi41==1 
replace resid_ch=1 if vi41==4 | vi41==3
replace resid_ch=2 if vi41==2 
replace resid_ch=3 if vi41==5 


****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=vi11


****************
***cuartos_ch***
****************

gen cuartos_ch=vi10

****************
***cocina_ch****
****************

/*
vi13:
           1 en cuarto exclusivo solo para cocinar
           2 en un cuarto utilizado también para dormir
           3 en sala comedor
           4 en patio, corredor u otro sitio
           5 no cocinan
*/

gen cocina_ch=.
replace cocina_ch=0 if vi13~=1
replace cocina_ch=1 if vi13==1
replace cocina_ch=. if vi13==.

****************
****telef_ch****
****************

gen telef_ch=.
replace telef_ch=0 if vi32==2
replace telef_ch=1 if vi32==1

****************
****refrig_ch***
****************

gen refrig_ch=.


****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch****
****************

gen auto_ch=.

****************
****compu_ch****
****************

gen compu_ch=.

****************
**internet_ch***
****************

gen internet_ch=.
replace internet_ch = 0 if vi35 == 2
replace internet_ch = 1 if vi35 == 1

****************
****cel_ch******
****************

gen cel_ch=.

****************
****vivi1_ch****
****************

/*
vi09:
           1 casa / villa
           2 departamento
           3 cuarto en casa de inquilinato
           4 mediagua
           5 rancho / choza / covacha
           6 otro, cual
*/

gen vivi1_ch=.
replace vivi1_ch=1 if vi09==1 
replace vivi1_ch=2 if vi09==2 
replace vivi1_ch=3 if vi09>2 & vi09<7

****************
****vivi2_ch****
****************

gen vivi2_ch=.
replace vivi2_ch=1 if vi09==2 | vi09==1
replace vivi2_ch=0 if vi09>=3 & vi09<=6       

 
*******************
****viviprop_ch****
*******************

/*
vi42:
           1 en arriendo
           2 anticresis y/o arriendo
           3 propia y la está pagando
           4 propia y totalmente pagada
           5 cedida
           6 recibida por servicios
           7 otro, cual
*/

gen viviprop_ch=.
replace viviprop_ch=0 if vi42==1 | vi35==2 
replace viviprop_ch=1 if vi42==4
replace viviprop_ch=2 if vi42==3 
replace viviprop_ch=3 if vi35==5 | vi35==6 | vi35==7 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=vi46
replace vivialq_ch=. if vi46 == 9999
*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.
replace vivialqimp_ch=vi45
replace vivialqimp_ch=. if vi45 == 9999


gen NERP =.
gen NERS =.
gen NERS2 =.
gen LIT =.
gen ALFABET2=. 
gen RATIOPRIM=. 
gen RATIOSEC =.
gen RATIOTER =.
gen RATIOALL =.
gen RATIOLIT2=. 
gen RATIOLIT =.
gen WENAS =.
gen WENASD =.
gen MEASLES =.
gen SKILLED =.
gen CONTRACEPTIVE =.
gen CONDOM =.
gen ELEC =.
gen SFUELS =.
gen WATER =.
gen SANITATION =.
gen SECTEN =.
gen DIRT =.
gen TELEPHONE =.
gen TEL =.
gen CHILDREN =.
gen PERSROOM2 =.
gen PLT2 =.
gen REZ =.
gen PRIMCOMP =.
gen AEDUC_15 =.
gen AEDUC_15_24 =.
gen AEDUC_25 =.
gen GFA =.
gen GFAP =.
gen GFAS=.

gen region_c = region

gen raza_ci = (indigen==100)
replace raza_ci = . if indigen==.


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
compress


*do ruta\labelsBID.do, modify

saveold "`base_out'", replace


log close

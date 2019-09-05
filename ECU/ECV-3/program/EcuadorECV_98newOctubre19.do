****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA ECUADOR 1998*********************************
****************************************************************************

/*
*Modificado 4 agosto de 2005 Maria Fernanda Prada, acordado el 3 de agosto

CODIGO ANTERIOR:

gen asiste_ci=0
replace asiste_ci=1 if PE11~=7 & PE18==2
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==2
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==3
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==4
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==5
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==6
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==8
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==9
replace asiste_ci=1 if PE11~=7 & PE18==1 & PE19==10
replace asiste_ci=. if PE11==.
label variable asiste_ci "Asiste actualmente a la escuela"


NUEVO CODIGO:No condiciona sobre las razones de no asistencia 
sino sobre el numero de semanas que lleva sin asistir, menos de 
una semana (menos de 7 diasm, en este caso)

***Abril 19, 2006 (Analia)***
The following line:
egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross)
replace ynlm_ci=. if emp_ci==0
was replaced with
egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross)
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & acteventual==. & arrendamiento==. & otross==. 

The constraint "if emp_ci==1" was deleted from the following lines:
gen remesasext=PA78A/12 if emp_ci==1 & PA78B==8 & PA77==1
replace remesasext=PA78A/12 if emp_ci==1 & PA78B==7 & PA77==1
replace remesasext=PA78A/6 if emp_ci==1 & PA78==6 & PA77==1
replace remesasext=PA78A/3 if emp_ci==1 & PA78==5 & PA77==1
replace remesasext=PA78A if emp_ci==1 & PA78==4 & PA77==1
replace remesasext=PA78A*2 if emp_ci==1 & PA78==3 & PA77==1
replace remesasext=PA78A*4 if emp_ci==1 & PA78==2 & PA77==1
replace remesasext=PA78A*30 if emp_ci==1 & PA78==1 & PA77==1
gen ayuda=PA80A/12 if emp_ci==1 & PA80B==7 & PA79==1
replace ayuda=PA80A/6 if emp_ci==1 & PA80B==6 & PA79==1
replace ayuda=PA80A/3 if emp_ci==1 & PA80B==5 & PA79==1
replace ayuda=PA80A if emp_ci==1 & PA80B==4 & PA79==1
replace ayuda=PA80A*2 if emp_ci==1 & PA80B==3 & PA79==1
replace ayuda=PA80A*4 if emp_ci==1 & PA80B==2 & PA79==1
replace ayuda=PA80A*30 if emp_ci==1 & PA80B==1 & PA79==1

Besides, the creation of remesasext was modified as follows (the original programmer forgot to include "B" from the third
line onwards) Also, the condition PA78==2 (familiares y amigos de afuera del pais) was added:
gen remesasext=PA78A/12 if emp_ci==1 & PA78B==8 & PA77==1 & PA78==2
replace remesasext=PA78A/12 if emp_ci==1 & PA78B==7 & PA77==1 & PA78==2
replace remesasext=PA78A/6 if emp_ci==1 & PA78B==6 & PA77==1 & PA78==2
replace remesasext=PA78A/3 if emp_ci==1 & PA78B==5 & PA77==1 & PA78==2
replace remesasext=PA78A if emp_ci==1 & PA78B==4 & PA77==1 & PA78==2
replace remesasext=PA78A*2 if emp_ci==1 & PA78B==3 & PA77==1 & PA78==2
replace remesasext=PA78A*4 if emp_ci==1 & PA78B==2 & PA77==1 & PA78==2
replace remesasext=PA78A*30 if emp_ci==1 & PA78B==1 & PA77==1 & PA78==2


The following command (which creates the labor income for independent workers):
gen yprijbi=PA30 if emp_ci==1 
replace yprijbi=. if categopri_ci>2
replace yprijbi=. if PA30==999999  
was replaced with
gen yprijbi=ingrneto if emp_ci==1 
replace yprijbi=. if categopri_ci>2
replace yprijbi=. if PA21==999999  
since PA21 is the labor income for independent workers instead of PA30 (only available for wage employees) and ingrneto
is previously created using PA21
***

*** May 8, 2006 (Analia)
The following line:
replace segsoc_ci=0 if PA28==0
was replaced with
replace segsoc_ci=0 if PA28==2
(PA28 is a 1-2 variable, not a 0-1!)

********Revision January 30/07

Previous code:

ggen categopri_ci=.
replace categopri_ci=1 if PA20==3 | PA20==9
replace categopri_ci=2 if PA20==4 | PA20==10
replace categopri_ci=3 if PA20==1 | PA20==2 | PA20==7 | PA20==8 | PA20==6 | PA20==12 | PA20==13
replace categopri_ci=4 if PA20==5 | PA20==11

Income was wrong, it was created only for salaried workers
previous code:
Previous code
egen ylmpri_ci=rsum(yprijbi yprid)
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp==0

********Revision February 13/07
change income program for workers who are paid in a daily basis. Also change the construction of horaspri, it was created asuming full time 
work (30 days a month, 7 days a week) and the actual data on days and hours is included in the survey, so I used it
Previous code 

replace ingrneto=PA21*30 if emp_ci==1 & PA21b==1 
replace yprijbi=PA22*30 if emp_ci==1 & PA22A==1 
gen horaspri_ci=PA18*7  if emp_ci==1 
*/

/*

*** revision August 2007 (Victoria) ***
With the unification Sociometro/Equis we decided to add two new varibales: howner and floor.
This variables were already created for Atlas

gen howner=(viviprop_ch==1 | viviprop==2);
replace howner=. if viviprop_ch==.;
gen floor=(piso_ch==1);
replace floor=. if piso_ch==.;

Also, the orginal data was replaced with the new Mecovi versions

*****

*/


clear
capture log close
cd X:\ARM\ECU\ECV\1998\Data
set mem 130m
set more off



use "X:\ARM\ECU\ECV\1998\Data\ecu98ecv.dta" 
/*
rename ciudad CIUDAD
rename zona ZONA
rename sector SECTOR
rename vivienda VIVIENDA
rename hogar HOGAR
*/

sort CIUDAD ZONA SECTOR VIVIENDA HOGAR
merge CIUDAD ZONA SECTOR VIVIENDA HOGAR using "X:\ARM\ECU\ECV\1998\Datos_originales\vivi_ecu98.dta" 
drop _merge

***************
***factor_ch***
***************

gen factor_ch=FEXP
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

sort IDHOGAR
gen idh_ch=IDHOGAR
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if AREA1==0 /* Es una buena forma de generar xq lo diferente de 2 = miss*/
replace zona_c=1 if AREA1==1

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

gen anio_c=1998
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes=7
label variable mes "Mes de la encuesta"
label define mes 7 " Julio" 
label value mes mes


/** Se realizo el primer semestre del 1998 entonces puse el ultimo mes
**/

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if PD04==1
replace relacion_ci=2 if PD04==2
replace relacion_ci=3 if PD04==3
replace relacion_ci=4 if PD04>=4 & PD04<=10
replace relacion_ci=5 if PD04==13 /* abonado o huesped */
replace relacion_ci=5 if PD04==12 /* abonado o huesped */
replace relacion_ci=6 if PD04==11


label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci

/*94
----------------------
     pd04 |      Freq.
----------+-----------
1     Jefe |      5,810
2 Esposos- |      4,250
3 Hijo-Hij |     13,297
4 Yerno-Nu |        390
5 Nieto-Ni |      1,550
6 Padres-S |        309
7 Hermano- |        470
8 Otros pa |        556
9 Empleado |        165
10 Pensioni |          9
11 Otros no |        135
----------------------
----------------------
     PD04 |      Freq.
----------+-----------
1     JEFE |      5,801
2 ESPOSO(A |      4,220
3 HIJO - H |     12,460
4 YERNO, N |        334
5 NIETO, N |      1,703
6 PADRE, M |        179
7  SUEGROS |        103
8 HERMANOS |        269
9  CUNADOS |        141
10 OTROS PA |        599
11 EMPLEAD. |        147
12 PENSIONI |         10
13 OTROS NO |        163
----------------------


*/

****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=FEXP
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=.
replace sexo_ci=1 if PD02==1
replace sexo_ci=2 if PD02==2

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad=PD03
recode edad 99=.
gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci******
*****************

gen civil_ci=.
replace civil_ci=1 if PD05==3
replace civil_ci=2 if PD05==1 | PD05==2
replace civil_ci=3 if PD05==4 | PD05==5
replace civil_ci=4 if PD05==6

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

gen byte emp_ci=(PA01==1 | PA02==1 | PA03==1)

/* Es empleado: esta trabajando, o no trabajo pero si tenia trabjao porque estabe en 
vaciones*/

****************
***desemp1_ci***
****************

gen desemp1_ci=(PA01==2 & PA04>=1 & PA04<=6)

****************
***desemp2_ci*** 
****************

gen desemp2_ci=(desemp1_ci==1 | PA06==1 | PA06==2)

****************
***desemp3_ci***
****************
gen desemp3_ci=(desemp2_ci==1 | PA05>=1 & PA05<=6)


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

gen desalent_ci=(PA01==2 & PA04==7)

***************
***subemp_ci***
***************
gen subemp_ci=(emp_ci==1 & PA61==1 & PA59<=30)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(emp_ci==1 & PA61==2 & PA59<=30)

*****************
***horaspri_ci***
*****************

/* sum PA18 [iw=factor_ci] if emp_ci==1 & PA18>0 & PA18<=24
    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
-------------+-----------------------------------------------------------------
        PA18 |   12838  5381104.72    7.706764   3.141089          1         24


. sum PA17 [iw=factor_ci] if emp_ci==1 & PA17>0 & PA17<=7

    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
-------------+-----------------------------------------------------------------
        PA17 |   12839  5380847.92    5.216984    1.58619          1          7

*/
*Aunque son pocos casos donde las horas diarias o los dias a la semana no se reportar, les asigno el promedio observado del grupo para 
*no perder observaciones de ingreso, en el caso en que este si sea reportado.

replace PA18=7 if PA18==99| (PA18>24 & PA18<99)  
replace PA17=5 if PA17>7 & PA17<99

gen horaspri_ci=PA18*PA17  if emp_ci==1 

*****************
***horastot_ci***
*****************

gen horastot_ci=PA59  if emp_ci==1 
replace horastot_ci=.  if PA59==999


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if PA20==3 | PA20==9
replace categopri_ci=2 if PA20==4 | PA20==10
replace categopri_ci=3 if PA20==1 | PA20==2 | PA20==7 | PA20==8  | PA20==13
replace categopri_ci=4 if PA20==5 | PA20==11 | PA20==6 | PA20==12

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

/*Dentro de la categoría empleado se incluyen trabajadores agricolas */

******************
**dcategopri_ci **
******************

gen dcategopri_ci=(PA20>=7 & PA20<=12)
label variable dcategopri_ci "Categoria ocupacional trabajo principal agricola"
/*Varible creada para identificar si su posicion ocupacional es agricola(1) o no(0)*/
******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if PA51==3 | PA51==9
replace categosec_ci=2 if PA51==4 | PA51==10
replace categosec_ci=3 if PA51==1 | PA51==2 | PA51==7 | PA51==8 | PA51==6 | PA51==12
replace categosec_ci=4 if PA51==5 | PA51==11

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

******************
**dcategopri_ci **
******************

gen dcategosec_ci=(PA51>=7 & PA51<=12)
label variable dcategosec_ci "Categoria ocupacional trabajo secundario agricola"
/*Varible creada para identificar si su posicion ocupacional es agricola(1) o no(0)*/

*****************
***contrato_ci***
*****************

gen contrato_ci=.
replace contrato_ci=1 if PA23==1 
replace contrato_ci=1 if PA23==2
replace contrato_ci=0 if PA23==3

*****************
***dcontrato_ci***
*****************

gen dcontrato_ci=.
replace dcontrato_ci=1 if PA23==3
replace dcontrato_ci=0 if PA23>=1 & PA23<=2
label variable dcontrato_ci "Si el contrato fue con el gobierno-nombramiento"
/* Si el contrato fue con el govierno "Nombramiento"*/

******************
***tcontrato_ci***
******************

gen tcontrato_ci=.
replace tcontrato_ci=0 if PA24==1 & PA23==1 
replace tcontrato_ci=1 if PA24==2 & PA23==1 
label variable tcontrato_ci "Si el contrato indefinifo (0) o a termino fijo (1)"
/*Se puede saber si el contrato de trabajo es Indefinifo (0) o a termino fijo (1) */

*******************
***ntcontrato_ci***
*******************

gen ntcontrato_ci=.
replace ntcontrato_ci=PA24A if PA24==2 & PA23==1 
replace ntcontrato_ci=. if PA24A==99

/* Numero de meses que dura el trabajo a termino indefinido */

***************
***segsoc_ci***
***************

*Solo para los asalariados

gen segsoc_ci=.
replace segsoc_ci=1 if PA28==1
replace segsoc_ci=0 if PA28==2


***************
***dsegsoc_ci***
***************
/* Creamos una varible de seguridad pero que esta comprendida para todo la
poblacion, y es la reunion de los siguientes tipo de seguro (1) y si no tieno (0)
----------------------
     PS49 |      Freq.
----------+-----------
 SEGURO P |        770
 SEG.GENE |      2,565
 IESS SEG |      2,345
 SEG.ISSA |        259
  NINGUNO |     20,170
----------------------
*/
gen isegsoc_ci=(PS49>=1 & PS49<=4)


*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & PA43==1
replace nempleos_ci=. if pea1_ci==0

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=0 if tamest<5
replace firmapeq_ci=1 if tamest>=5 & tamest<.
esta varible se encuentra para la actividad primaria y secundaria
No construimos esta varible de esta manera en Ecuador porque en la pregunta en la ecuesta 
se incluye una varible divida en 5 categorias entre las cuales:
1  una persona	 1
2 a 5 Personas	 2
6 a 9 Personas	 3
10 a 29	"	 4	 
30 a 49	"	 5
50 a 99	"	 6
100 +	"	 7
*/


gen firmapeq_ci=.
replace firmapeq_ci=1 if PA19<=3
replace firmapeq_ci=0 if PA19>3
replace firmapeq_ci=. if PA19==.

/* La difinicion es la misma que firmapeq_ci pero se incluyen a los hogares
mas de 5 de la actividad primaria*/


*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if PA20==1 & emp_ci==1
replace spublico_ci=0 if PA20~=1 & emp_ci==1
replace spublico_ci=. if PA20==.

/*Sólo se le hace esta pregunta a los asalariados, aprendices y otros*/


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (PA13==5) & emp_ci==1
replace ocupa_ci=2 if (PA13>=11 & PA13<=17) & emp_ci==1
replace ocupa_ci=3 if (PA13>=41 & PA13<=43) & emp_ci==1
replace ocupa_ci=4 if (PA13==52) & emp_ci==1
replace ocupa_ci=5 if (PA13==51 | PA13==91) & emp_ci==1
replace ocupa_ci=6 if ((PA13>=60 & PA13<=62) | PA13==92) & emp_ci==1
replace ocupa_ci=7 if ((PA13>=71 & PA13<=83) | PA13==93) & emp_ci==1
replace ocupa_ci=8 if (PA13==1) & emp_ci==1
replace ocupa_ci=9 if (PA13==50 | PA13==53 | PA13==54 | PA13==94) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (PA14>=1 & PA14<=5) & emp_ci==1
replace rama_ci=2 if (PA14>=10 & PA14<=14) & emp_ci==1
replace rama_ci=3 if (PA14>=15 & PA14<=37) & emp_ci==1
replace rama_ci=4 if (PA14>=40 & PA14<=42) & emp_ci==1
replace rama_ci=5 if (PA14==45) & emp_ci==1
replace rama_ci=6 if (PA14>=50 & PA14<=55) & emp_ci==1 
replace rama_ci=7 if (PA14>=60 & PA14<=64) & emp_ci==1
replace rama_ci=8 if (PA14>=65 & PA14<=74) & emp_ci==1
replace rama_ci=9 if (PA14>=75 & PA14<99) & emp_ci==1
replace rama_ci=. if emp_ci~=1

****************
***durades_ci***
****************

gen durades_ci=PA11/4/*La variable en meses*/
replace durades_ci=. if PA11==0 /*No aplica*/
replace durades_ci=. if desemp2_ci==0 
replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=PA15+PA15A/12+PA15B/(52) 
replace antiguedad_ci=. if PA15==99 | PA15A==99 | PA15B==9
replace antiguedad_ci=. if desemp2_ci==1 
replace antiguedad_ci=. if emp_ci==0

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

****************************
***ylmpri_ci & ylmpri1_ci***
****************************

/*Para los trabajadores dependientes*/
/*En cuanto a la frecuencia encontre que existe una codificacion 8 
la cual no dice la periodicidad, se convierte en missing en la nueva variable*/

gen yprid=PA30 if emp_ci==1 
replace yprid=. if categopri_ci<=2
replace yprid=. if PA30==9999999 

/*Recibio decimo tercer sueldo*/
gen decsueldo1=.
replace decsueldo1=PA31A/12 if emp_ci==1 & PA31==1

gen aniversario1=.
replace aniversario1=PA27A/12 if emp_ci==1 & PA27==1

gen horaextra1=.
replace horaextra1=PA32A if emp_ci==1 & PA32==1

gen propinas1=.
replace propinas1=PA33A if emp_ci==1 & PA33==1

gen vacaciones1=.
replace vacaciones1=PA29A/12 if emp_ci==1 & PA29==1

gen aguinaldo1=.
replace aguinaldo1=PA29C/12 if emp_ci==1 & PA29B==1


egen yprijbd=rsum(yprid decsueldo1 aniversario1 horaextra1 propinas1 vacaciones1 aguinaldo1)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. &  horaextra1==. & vacaciones1==. & aguinaldo1==.  

/*Para los trabajadores independientes*/

gen ingrneto=PA21/12 if emp_ci==1 & PA21A==7 
replace ingrneto=PA21/6 if emp_ci==1 & PA21A==6
replace ingrneto=PA21/3 if emp_ci==1 & PA21A==5
replace ingrneto=PA21 if emp_ci==1 & PA21A==4 
replace ingrneto=PA21*2 if emp_ci==1 & PA21A==3
replace ingrneto=PA21*4 if emp_ci==1 & PA21A==2
replace ingrneto=PA21*PA17 if emp_ci==1 & PA21A==1 

gen yprijbi=PA22/12 if emp_ci==1 & PA22A==7 
replace yprijbi=PA22/6 if emp_ci==1 & PA22A==6
replace yprijbi=PA22/3 if emp_ci==1 & PA22A==5
replace yprijbi=PA22 if emp_ci==1 & PA22A==4 
replace yprijbi=PA22*2 if emp_ci==1 & PA22A==3
replace yprijbi=PA22*4 if emp_ci==1 & PA22A==2
replace yprijbi=PA22*PA17 if emp_ci==1 & PA22A==1 
replace yprijbi=. if PA22==9999999

replace ingrneto=. if yprid>0 & yprid<9999999 & categopri_ci>2
replace yprijbi=. if yprid>0 & yprid<9999999 & categopri_ci>2


egen ylmpri_ci=rsum(yprid yprijbi ingrneto)
replace ylmpri_ci=. if yprid==. & ingrneto==. & yprijbi==.
replace ylmpri_ci=. if emp==0

egen ylmpri1_ci=rsum(yprijbi yprijbd)
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
replace food1=PA34A if emp_ci==1 & PA34==1 & PA34B==4
replace food1=PA34A*2 if emp_ci==1 & PA34==1 & PA34B==3
replace food1=PA34A*4 if emp_ci==1 & PA34==1 & PA34B==2
replace food1=PA34A*30 if emp_ci==1 & PA34==1 & PA34B==1

/*

      PA36B |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |          6        0.53        0.53
          1 |        874       77.14       77.67
          2 |        186       16.42       94.09
          3 |         34        3.00       97.09
          4 |         12        1.06       98.15
          5 |          4        0.35       98.50
          6 |          4        0.35       98.85
         10 |          1        0.09       98.94
         11 |          2        0.18       99.12
         21 |          1        0.09       99.21
         30 |          1        0.09       99.29
         99 |          8        0.71      100.00
------------+-----------------------------------
      Total |      1,133      100.00

*/
gen ropa1=.
replace ropa1=PA36A*(30/12) if emp_ci==1 & PA36==1 & PA36B==30
replace ropa1=PA36A*(21/12) if emp_ci==1 & PA36==1 & PA36B==21
replace ropa1=PA36A*(11/12) if emp_ci==1 & PA36==1 & PA36B==11
replace ropa1=PA36A*(10/12) if emp_ci==1 & PA36==1 & PA36B==10
replace ropa1=PA36A*(6/12) if emp_ci==1 & PA36==1 & PA36B==6
replace ropa1=PA36A*(5/12) if emp_ci==1 & PA36==1 & PA36B==5
replace ropa1=PA36A*(4/12) if emp_ci==1 & PA36==1 & PA36B==4
replace ropa1=PA36A*(3/12) if emp_ci==1 & PA36==1 & PA36B==3
replace ropa1=PA36A*(2/12) if emp_ci==1 & PA36==1 & PA36B==2
replace ropa1=PA36A*(1/12) if emp_ci==1 & PA36==1 & PA36B==1
replace ropa1=0 if emp==1 & PA36A==.


gen merca1=.

gen vivi1=.
replace vivi1=PA35B if emp_ci==1 & PA35==1
replace vivi1=0 if emp==1 & PA35==.


gen trans1=.
replace trans1=PA37A if emp_ci==1 & PA37==1 
replace trans1=PA37A if emp_ci==1 & PA37==2 
replace trans1=0 if emp==1 & PA37==.


gen segur1=.
gen otross1=.

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1)
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0

gen ylnmpri1_ci=.
gen otros=.

***************
***ylmsec_ci***
***************


gen ysec1=PA54 if emp_ci==1 
replace ysec1=. if categopri_ci<=2
replace ysec1=. if PA54==999999  

gen sdecsueldo1=.

gen shoraextra1=.

gen vacaciones=.

gen aguinaldo=.

gen bonificaciones=.

gen ylmsec_ci=ysec1

egen ylmsec1_ci=rsum(ysec1 sdecsueldo1 shoraextra1 vacaciones aguinaldo bonificaciones)
replace ylmsec1_ci=. if emp_ci==0 

******************
****ylnmsec_ci****
******************

gen food2=.

gen ropa2=.

gen merca2=.

gen vivi2=.

gen trans2=.

gen segur2=.

gen otross2=.
/* la Pregunta incluye todos los benficions no monetarios*/
gen ylnmsec_ci=PA57A if emp_ci==1 & PA57==1
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

gen remesasext=PA78A/12 	if emp_ci==1 & PA78B==8 & PA77==1 & PA78==2
replace remesasext=PA78A/12 	if emp_ci==1 & PA78B==7 & PA77==1 & PA78==2
replace remesasext=PA78A/6 	if emp_ci==1 & PA78B==6 & PA77==1 & PA78==2
replace remesasext=PA78A/3 	if emp_ci==1 & PA78B==5 & PA77==1 & PA78==2
replace remesasext=PA78A 	if emp_ci==1 & PA78B==4 & PA77==1 & PA78==2
replace remesasext=PA78A*2 	if emp_ci==1 & PA78B==3 & PA77==1 & PA78==2
replace remesasext=PA78A*4 	if emp_ci==1 & PA78B==2 & PA77==1 & PA78==2
replace remesasext=PA78A*30 	if emp_ci==1 & PA78B==1 & PA77==1 & PA78==2



gen ayuda=PA80A/12 if  PA80B==7 & PA79==1
replace ayuda=PA80A/6 if  PA80B==6 & PA79==1
replace ayuda=PA80A/3 if  PA80B==5 & PA79==1
replace ayuda=PA80A if  PA80B==4 & PA79==1
replace ayuda=PA80A*2 if  PA80B==3 & PA79==1
replace ayuda=PA80A*4 if  PA80B==2 & PA79==1
replace ayuda=PA80A*30 if  PA80B==1 & PA79==1

gen cuotalim=.

gen alqui=.

gen alqneg=.

gen jubil=.

gen deveh=.

rename otros otros1
gen otros=.

gen utilidades=.

gen dividendos=.

gen intereses=.

gen herencias=.

gen indemnizacion=.

gen ayudagob=.

gen acteventual=.

gen arrendamiento=.

gen otross=.

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otross)
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & acteventual==. & arrendamiento==. & otross==. 

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

/* Las variables NIVEDU y NIVELCURSA nos permiten identificar los años de educación
para aquellos individuos que actualmente estan estudiando. 
Las variables ULTGRADO y ULTNIVEL indican el último nivel alcanzado y el año 
alcanzado en dicho nivel, permiten calcular los años de educación para aquellos que
actualmente no asisten a un establecimiento escolar.
En El Salvador, la educación básica dura nueve años y la educación media tres años*/


/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/
* years of education

gen byte yedc=. if PE29==. 
replace yedc=0 if PE29>=1 & PE29<=3
replace yedc=PE30 if PE29==4 
replace yedc=PE30 if PE29==4 & PE30>=6 
replace yedc=PE30 if PE29==5 
replace yedc=PE30 if PE29==5 & PE30>=6 
replace yedc=7+PE30 if PE29==6
replace yedc=12 if PE29==6 & PE30>=6 /* tiene 7 años de educacion sec pero su titulo es sec*/
replace yedc=12+PE30 if PE29==8 | PE29==7
replace yedc=17 if PE29==7 & PE30>=6
replace yedc=14 if PE29==8 & PE30>=2
replace yedc=18+PE30 if PE29==9
replace yedc=22 if PE29==9 & PE30>=6
/* No se tiene encuentra centros de alfavetizacion y educacion 
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

gen byte edupre_ci=0
replace edupre_ci=1 if PE29==3 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (aedu_ci>12 & PE29==7) & aedu_ci~=.
replace eduac_ci=0 if (aedu_ci>12 & PE29==8) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
*modificada 4 agosto 2005 ver nota al inicio del programa*
gen asiste_ci=0
replace asiste_ci=1 if PE11~=7 
replace asiste_ci=0 if PE20>=7 & PE20!=.
replace asiste_ci=. if PE11==.
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=PE19
label variable pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "Se Retiro" 2 "Enfermedad", add
label define pqnoasis_ci 3 "Lab domesticas" 4 "Por paro o huelga", add 
label define pqnoasis_ci 5 "Falta de dinero" 6 "Trabaja" , add
label define pqnoasis_ci 7 "No le interesa" 8 "Mal clima" 9 "Otro" 10 "Vacaciones" , add 
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

gen edupub_ci=.
replace edupub_ci=1 if PE13==1 |PE13==3
replace edupub_ci=0 if PE13==2 

/*Variables de Infraestructura del hogar*/

***************
**aguared_ch***
***************
gen aguared_ch=.
replace aguared_ch=1 if vi18==1 | vi18==2
replace aguared_ch=0 if vi18>2

****************
**aguadist_ch***
****************

gen aguadist_ch=.
replace aguadist_ch=1 if vi20==1 
replace aguadist_ch=2 if vi20==2
replace aguadist_ch=3 if vi20==3

****************
**aguamala_ch***
****************

gen aguamala_ch=.
replace aguamala_ch=0 if vi18>0
replace aguamala_ch=1 if vi18==3 | vi18==5
replace aguamala_ch=1 if vi18==6 | vi18==7
replace aguamala_ch=1 if vi18==8 | vi18==9 

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=.
replace luz_ch=1 if vi26==1 
replace luz_ch=0 if vi26~=1 
replace luz_ch=. if vi26~=.

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=.
replace combust_ch=1 if vi11==1 | vi11==3 
replace combust_ch=0 if vi11==2 | vi11==4  

****************
****bano_ch*****
****************

gen bano_ch=.
replace bano_ch=1 if vi16~=5
replace bano_ch=0 if vi16==5
replace bano_ch=. if vi16==.

****************
****banoex_ch***
****************

gen banoex_ch=.
replace banoex_ch=1 if vi17==1
replace banoex_ch=0 if vi17==2

****************
****des1_ch*****
****************

gen des1_ch=.
replace des1_ch=0 if vi13==5
replace des1_ch=1 if vi13==1 | vi13==2
replace des1_ch=2 if vi13==3 | vi13==4

****************
****des2_ch*****
****************

gen des2_ch=.
replace des2_ch=0 if vi13==5
replace des2_ch=1 if vi13==1 | vi13==2
replace des2_ch=2 if vi13==3 | vi13==4
 
****************
****piso_ch*****
****************

gen piso_ch=.
replace piso_ch=0 if vi04==6 | vi04==7
replace piso_ch=1 if vi04==1 | vi04==2 | vi04==3 
replace piso_ch=2 if vi04==4 | vi04==5 

****************
****pared_ch****
****************

gen pared_ch=.
replace pared_ch=0 if vi03==5 | vi03==3
replace pared_ch=1 if vi03==1 | vi03==2
replace pared_ch=2 if vi03==4 | vi03==6 

****************
****techo_ch****
****************

gen techo_ch=.
replace techo_ch=0 if vi02==5 
replace techo_ch=1 if vi02>=1 & vi02<=4
replace techo_ch=2 if vi02==6

****************
****resid_ch****
****************

gen resid_ch=.
replace resid_ch=0 if vi27==1 | vi27==2 
replace resid_ch=1 if vi27==4
replace resid_ch=2 if vi27==3 
replace resid_ch=3 if vi27==5 


****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=vi07


****************
***cuartos_ch***
****************

gen cuartos_ch=vi06

****************
***cocina_ch****
****************

gen cocina_ch=.
replace cocina_ch=0 if vi09~=1
replace cocina_ch=1 if vi09==1
replace cocina_ch=. if vi09==.

****************
****telef_ch****
****************

gen telef_ch=.
replace telef_ch=0 if vi30==2
replace telef_ch=1 if vi30==1

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

****************
****cel_ch******
****************

gen cel_ch=.

****************
****vivi1_ch****
****************

gen vivi1_ch=.
replace vivi1_ch=1 if vi05==1 
replace vivi1_ch=2 if vi05==2 


****************
****vivi2_ch****
****************

gen vivi2_ch=.
replace vivi2_ch=1 if vi05==2 | vi05==1
replace vivi2_ch=0 if vi05>=3 & vi05<=6       

 
*******************
****viviprop_ch****
*******************

gen viviprop_ch=.
replace viviprop_ch=0 if vi35==1 
replace viviprop_ch=1 if vi35==3
replace viviprop_ch=2 if vi35==2 
replace viviprop_ch=3 if vi35==4 | vi35==5 | vi35==6 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=vi42

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.
replace vivialqimp_ch=vi41


gen howner=(viviprop_ch==1 | viviprop==2)
replace howner=. if viviprop_ch==.
gen floor=(piso_ch==1)
replace floor=. if piso_ch==.



save "X:\ARM\ECU\ECV\1998\Arm_data\ECU1998EA_BID.dta", intercooled replace

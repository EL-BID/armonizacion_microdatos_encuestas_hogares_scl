****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA EL ECUADOR 1994******************************
****************************************************************************


/*
*Modificado 4 agosto de 2005 Maria Fernanda Prada, acordado el 3 de agosto
CODIGO ANTERIOR:

gen asiste_ci=0
replace asiste_ci=1 if pe12==1 & pe28==2
replace asiste_ci=1 if pe12==1 & pe28==1 & pe29==2
replace asiste_ci=1 if pe12==1 & pe28==1 & pe29==6
replace asiste_ci=1 if pe12==1 & pe28==1 & pe29==8
replace asiste_ci=1 if pe12==1 & pe28==1 & pe29==11
replace asiste_ci=1 if pe12==1 & pe28==1 & pe29==12
replace asiste_ci=. if pe12==.
label variable asiste_ci "Asiste actualmente a la escuela"

NUEVO CODIGO:No condiciona sobre las razones de no asistencia 
sino sobre el numero de semanas que lleva sin asistir, menos de 
una semana

gen asiste_ci=0
replace asiste_ci=1 if pe12==1 
replace asiste_ci=0 if pe12==1 & pe30>=1
replace asiste_ci=. if pe12==.
label variable asiste_ci "Asiste actualmente a la escuela"

********Revision January 30/07

Previous code:

gen categopri_ci=.
replace categopri_ci=1 if pa20==4
replace categopri_ci=2 if pa20==5 | pa20==6
replace categopri_ci=3 if pa20==1 | pa20==2 | pa20==3| pa20==9 
replace categopri_ci=4 if pa20==7  | pa20==8 

Income was wrong, it was created only for salaried workers
previous code:

egen ylmpri_ci=rsum(yprijbi yprid)
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp==0

********Revision February 1/07
ylmsec was created if pa43 (ocupacion)==1 instead of p42==1 (the person holds more than 1 job)

previous code:

gen ysec1=pa52/12 if emp_ci==1 & pa52a==7 & pa43==1
replace ysec1=pa52/6 if emp_ci==1 & pa52a==6 & pa43==1
replace ysec1=pa52/3 if emp_ci==1 & pa52a==5 & pa43==1
replace ysec1=pa52 if emp_ci==1 & pa52a==4 & pa43==1
replace ysec1=pa52*2 if emp_ci==1 & pa52a==3 & pa43==1
replace ysec1=pa52*4 if emp_ci==1 & pa52a==2 & pa43==1
replace ysec1=pa52*30 if emp_ci==1 & pa52a==1 & pa43==1
replace ysec1=. if pa52==9999999 

********Revision February 12/07
Income for independent workers was created using an incorrect variable p21a is MONTH not frequency! the correct variable for
frequency is p21b. This error was responsible for the lack of data for more tha 7% of employees and also an incorrect calculation of 
income
Previous code:

gen ingrneto=pa21/12 if emp_ci==1 & pa21a==7 

********Revision February 13/07
change income program for workers who are paid in a daily basis. Also change the construction of horaspri, it was created asuming full time 
work (30 days a month, 7 days a week) and the actual data on days and hours is included in the survey, so I used it
Previous code 

replace yprid=pa30*30 if emp_ci==1 & pa30a==1 
replace ingrneto=pa21*30 if emp_ci==1 & pa21b==1 
replace ysec1=pa52*30 if emp_ci==1 & pa52a==1 & pa42==1

gen horaspri_ci=pa18*7  if emp_ci==1

SEGSOC was createdusing wrong values because p27=1 yes p27=2 no y p27=0 doesnt exists.
Previous Code 

gen segsoc_ci=.
replace segsoc_ci=1 if pa27==1
replace segsoc_ci=0 if pa27==0

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


*/

clear
capture log close
cd X:\ARM\ECU\ECV\1994\Arm_data
set mem 130m
set more off

use "X:\ARM\ECU\ECV\1994\Datos originales\SEC2.DTA",clear
sort _date nhid1 nhid2 nhid3
save"X:\ARM\ECU\ECV\1994\Datos originales\SEC2EA.DTA",replace

use "X:\ARM\ECU\ECV\1994\Datos originales\SEC6.DTA",clear 
sort _date nhid1 nhid2 nhid3
save"X:\ARM\ECU\ECV\1994\Datos originales\SEC6EA.DTA",replace

use "X:\ARM\ECU\ECV\1994\Datos originales\SEC1.DTA", clear
sort _date nhid1 nhid2 nhid3
save "X:\ARM\ECU\ECV\1994\Datos originales\SEC1EA.DTA",replace

use "X:\ARM\ECU\ECV\1994\Datos originales\BASICDAT.DTA", clear
sort _date nhid1 nhid2 nhid3
save "X:\ARM\ECU\ECV\1994\Datos originales\BASICDATEA.DTA",replace

use "X:\ARM\ECU\ECV\1994\Datos originales\SEC4.DTA"
sort _date nhid1 nhid2 nhid3
save "X:\ARM\ECU\ECV\1994\Datos originales\SEC4EA.DTA",replace


use "X:\ARM\ECU\ECV\1994\Datos originales\SEC2EA.DTA",clear
sort _date nhid1 nhid2 nhid3
merge _date nhid1 nhid2 nhid3 using "X:\ARM\ECU\ECV\1994\Datos originales\SEC6EA.DTA" 
ren _merge _merge1
sort _date nhid1 nhid2 nhid3
merge _date nhid1 nhid2 nhid3 using "X:\ARM\ECU\ECV\1994\Datos originales\SEC1EA.DTA"
ren _merge _merge2
sort _date nhid1 nhid2 nhid3
merge _date nhid1 nhid2 nhid3 using "X:\ARM\ECU\ECV\1994\Datos originales\BASICDATEA.DTA"
ren _merge _merge3
sort _date nhid1 nhid2 nhid3
merge _date nhid1 nhid2 nhid3 using "X:\ARM\ECU\ECV\1994\Datos originales\SEC4EA.DTA"

***************
***factor_ch***
***************

gen factor_ch=factores
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

/* Cambio en idh_ch el 11/10/05 ya que no se estaba generando bien
gen str_zona = string(zona)
gen str_sector = string(sector)
gen str_vivien = string(vivien)
gen str_hogar = string(hogar)

gen IDHOGAR= str_zona+str_sector+str_vivien+str_hogar
drop str_zona str_sector str_vivien str_hogar
sort IDHOGAR
gen idh_ch=IDHOGAR
label variable idh_ch "ID del hogar"
*/

egen idh_ch=group(nhid1 nhid2 nhid3)  
label variable idh_ch "ID del hogar"


*************
****idp_ci***
*************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=0 if area~=1 /* Es una buena forma de generar xq lo diferente de 2 = miss*/
replace zona_c=1 if area==1
replace zona_c=. if area==.

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

gen anio_c=1994
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes=6
label variable mes "Mes de la encuesta"
label define mes 6 " Junio" 
label value mes mes


/** Se realizo el primer semestre del 1998 entonces puse el ultimo mes
**/

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if pd04==1
replace relacion_ci=2 if pd04==2
replace relacion_ci=3 if pd04==3
replace relacion_ci=4 if pd04>=4 & pd04<=8
replace relacion_ci=5 if pd04==11 /* abonado o huesped */
replace relacion_ci=5 if pd04==10 /* abonado o huesped */
replace relacion_ci=6 if pd04==9
/*
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
*/
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

gen factor_ci=_weight_
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=.
replace sexo_ci=1 if pd02==1
replace sexo_ci=2 if pd02==2

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad=pd03
recode edad 99=.
gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if pd05==6
replace civil_ci=2 if pd05==1 | pd05==2
replace civil_ci=3 if pd05==3 | pd05==4
replace civil_ci=4 if pd05==5

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

/*----------------------
     pd05 |      Freq.
----------+-----------
 Uniøn li |      2,543
   Casado |      7,094
 Separado |        551
 Divorcia |        151
    Viudo |        750
  Soltero |      5,587
       NR |          1
----------------------
*/

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

/* Es empleado: esta trabajando, o no trabajo pero si tenia trabjao porque estabe en 
vaciones*/

****************
***desemp1_ci***
****************

gen desemp1_ci=(pa01==2 & pa04==1)

****************
***desemp2_ci*** 
****************

gen desemp2_ci=(desemp1_ci==1 | pa06==1 | pa06==2)

****************
***desemp3_ci***
****************
gen desemp3_ci=(desemp2_ci==1 | pa05>=1 & pa05<=6)


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

gen desalent_ci=(pa01==2 & pa04==2)
replace desalent_ci= 1 if pa01==2 & pa03==2

***************
***subemp_ci***
***************
gen subemp_ci=(emp_ci==1 & pa56==1 & pa54<=30)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(emp_ci==1 & pa56==2 & pa54<=30)

*****************
***horaspri_ci***
*****************
/*Aunque son pocos casos donde las horas diarias o los dias a la semana no se reportar, les asigno el promedio observado del grupo para 
*no perder observaciones de ingreso, en el caso en que este si sea reportado.

. sum pa17 [iw=factor_ci] if emp_ci==1 & pa17>0 & pa17<=7

    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
-------------+-----------------------------------------------------------------
        pa17 |    9657        9657    5.251217   1.488236          1          7


. sum pa18 [iw=factor_ci] if emp_ci==1 & pa18>0 & pa18<=24

    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
-------------+-----------------------------------------------------------------
        pa18 |    9654        9654    7.547856   2.908733          1         24

*/
replace pa18=7 if pa18==99| (pa18>24 & pa18<99)  
replace pa17=5 if pa17>7 & pa17<99

gen horaspri_ci=pa18*pa17 if pa17<=7 & pa18<=24 & emp_ci==1 

*****************
***horastot_ci***
*****************

gen horastot_ci=pa54  if emp_ci==1 
replace horastot_ci=.  if pa54==999


******************
***categopri_ci***
******************


gen categopri_ci=.
replace categopri_ci=1 if pa20==4
replace categopri_ci=2 if pa20==5 | pa20==6
replace categopri_ci=3 if pa20==1 | pa20==2 | pa20==3| pa20==9 
replace categopri_ci=4 if pa20==7  | pa20==8 

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"
*
/*Dentro de la categoría empleado se incluyen trabajadores agricolas */

******************
**dcategopri_ci **
******************

gen dcategopri_ci=(pa20>=7 & pa20<=12)
label variable dcategopri_ci "Categoria ocupacional trabajo principal agricola"
/*Varible creada para identificar si su posicion ocupacional es agricola(1) o no(0)*/
******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if pa50==4
replace categosec_ci=2 if pa50==5 | pa50==6
replace categosec_ci=3 if pa50==1 | pa50==2 | pa50==3 | pa50==8 
replace categosec_ci=4 if pa50==7 

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***contrato_ci***
*****************

gen contrato_ci=.
replace contrato_ci=1 if pa22==1 
replace contrato_ci=0 if pa22==2

*****************
***tcontrato_ci***
*****************

gen tcontrato_ci=.
replace tcontrato_ci=0 if pa23==1 & pa22==1 
replace tcontrato_ci=1 if pa23==2 & pa22==1 
label variable tcontrato_ci "Si el contrato indefinifo (0) o a termino fijo (1)"
/*Se puede saber si el contrato de trabajo es indefinifo (0) o a termino fijo (1) */


***************
***segsoc_ci***
***************
*Solo para los asalariados

gen segsoc_ci=.
replace segsoc_ci=1 if pa27==1
replace segsoc_ci=0 if pa27==2

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & pa42==1
replace nempleos_ci=. if pea1_ci==0


*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=0 if pa19<2
replace firmapeq_ci=1 if pa19>=2
replace firmapeq_ci=. if pa19 ==.

/* La difinicion es la misma que firmapeq_ci pero se incluyen a los hogares
mas de 5 de la actividad primaria*/

*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if pa20==1 & emp_ci==1
replace spublico_ci=0 if pa20~=1 & emp_ci==1
replace spublico_ci=. if pa20==.

/*Sólo se le hace esta pregunta a los asalariados, aprendices y otros*/


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (pa12==5) & emp_ci==1
replace ocupa_ci=2 if (pa12>=11 & pa12<=17) & emp_ci==1
replace ocupa_ci=3 if (pa12>=41 & pa12<=43) & emp_ci==1
replace ocupa_ci=4 if (pa12==52) & emp_ci==1
replace ocupa_ci=5 if (pa12==51 | pa12==91) & emp_ci==1
replace ocupa_ci=6 if ((pa12>=60 & pa12<=62) | pa12==92) & emp_ci==1
replace ocupa_ci=7 if ((pa12>=71 & pa12<=83) | pa12==93) & emp_ci==1
replace ocupa_ci=8 if (pa12==1) & emp_ci==1
replace ocupa_ci=9 if (pa12==50 | pa12==53 | pa12==54 | pa12==94) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (pa13>=1 & pa13<=5) & emp_ci==1
replace rama_ci=2 if (pa13>=10 & pa13<=14) & emp_ci==1
replace rama_ci=3 if (pa13>=15 & pa13<=37) & emp_ci==1
replace rama_ci=4 if (pa13>=40 & pa13<=42) & emp_ci==1
replace rama_ci=5 if (pa13==45) & emp_ci==1
replace rama_ci=6 if (pa13>=50 & pa13<=55) & emp_ci==1 
replace rama_ci=7 if (pa13>=60 & pa13<=64) & emp_ci==1
replace rama_ci=8 if (pa13>=65 & pa13<=74) & emp_ci==1
replace rama_ci=9 if (pa13>=75 & pa13<99) & emp_ci==1
replace rama_ci=. if emp_ci~=1

****************
***durades_ci***
****************

gen durades_ci=pa10/4/*La variable en meses*/
replace durades_ci=. if pa10==0 /*No aplica*/
replace durades_ci=. if desemp2_ci==0 
replace durades_ci=. if emp_ci==1
replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=pa15+pa15a/12+pa15b/(52)
replace antiguedad_ci=. if pa15==99 | pa15a==99 | pa15b==9
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
la cual no dice la periodicidad, se convierte en missing en la 
nueva variable*/

gen yprid=pa30/12 if emp_ci==1 & pa30a==7 
replace yprid=pa30/6 if emp_ci==1 & pa30a==6
replace yprid=pa30/3 if emp_ci==1 & pa30a==5
replace yprid=pa30 if emp_ci==1 & pa30a==4 
replace yprid=pa30*2 if emp_ci==1 & pa30a==3
replace yprid=pa30*4 if emp_ci==1 & pa30a==2
replace yprid=pa30*pa17 if emp_ci==1 & pa30a==1 

replace yprid=. if pa30==9999999 

/*existe un monto que se llama otro el cual no sabemos su periodicidad
la decion fue sacarlo de muestra-reunion con Suzanne*/
/*Recibio decimo tercer sueldo*/
gen decsueldo1=.
/*No existe son lo aparece desde 1998 */
gen aniversario1=.
gen horaextra1=.

gen propinas1=.
/*No existe son lo aparece desde 1998 */

gen vacaciones1=.

gen aguinaldo1=.

egen yprijbd=rsum( yprid decsueldo1 aniversario1 horaextra1 propinas1 vacaciones1 aguinaldo1)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. &  horaextra1==. & vacaciones1==. & aguinaldo1==.  

/*Para los trabajadores independientes*/


gen ingrneto=pa21/12 if emp_ci==1 & pa21b==7 
replace ingrneto=pa21/6 if emp_ci==1 & pa21b==6
replace ingrneto=pa21/3 if emp_ci==1 & pa21b==5
replace ingrneto=pa21 if emp_ci==1 & pa21b==4 
replace ingrneto=pa21*2 if emp_ci==1 & pa21b==3
replace ingrneto=pa21*4 if emp_ci==1 & pa21b==2
replace ingrneto=pa21*pa17 if emp_ci==1 & pa21b==1 
replace ingrneto=. if pa21==9999999 


egen ylmpri_ci=rsum(yprid ingrneto)
replace ylmpri_ci=. if yprid==. & ingrneto==.
replace ylmpri_ci=. if emp==0

/*egen ylmpri1_ci=rsum(yprijbi yprijbd)
replace ylmpri1_ci=. if yprijbd==. & yprijbi==.
replace ylmpri1_ci=. if emp==0*/

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

gen food1=.
replace food1=pa32a if emp_ci==1 & pa32==1 & pa32b==4
replace food1=pa32a*2 if emp_ci==1 & pa32==1 & pa32b==3
replace food1=pa32a*4 if emp_ci==1 & pa32==1 & pa32b==2
replace food1=pa32a*30 if emp_ci==1 & pa32==1 & pa32b==1
replace food1=. if pa32a==999999

gen ropa1=.
replace ropa1=pa34a*(9/12) if emp_ci==1 & pa34==1 & pa34b==9
replace ropa1=pa34a*(8/12) if emp_ci==1 & pa34==1 & pa34b==8
replace ropa1=pa34a*(7/12) if emp_ci==1 & pa34==1 & pa34b==7
replace ropa1=pa34a*(6/12) if emp_ci==1 & pa34==1 & pa34b==6
replace ropa1=pa34a*(5/12) if emp_ci==1 & pa34==1 & pa34b==5
replace ropa1=pa34a*(4/12) if emp_ci==1 & pa34==1 & pa34b==4
replace ropa1=pa34a*(3/12) if emp_ci==1 & pa34==1 & pa34b==3
replace ropa1=pa34a*(2/12) if emp_ci==1 & pa34==1 & pa34b==2
replace ropa1=pa34a*(1/12) if emp_ci==1 & pa34==1 & pa34b==1
replace ropa1=0 if emp==1 & pa34a==.
replace ropa1=. if pa34a==999999

gen merca1=.

gen vivi1=.
replace vivi1=pa33a if emp_ci==1 & pa33==1
replace vivi1=0 if emp==1 & pa33==.
replace vivi1=. if pa33a==999999
replace vivi1=. if pa33a==888888

gen trans1=.
replace trans1=pa35a if emp_ci==1 & pa35==1 
replace trans1=pa35a if emp_ci==1 & pa35==2 
replace trans1=0 if emp==1 & pa35==.
replace trans1=0 if pa35a==999999
replace trans1=0 if pa35a==888888

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


gen ysec1=pa52/12 if emp_ci==1 & pa52a==7 & pa42==1
replace ysec1=pa52/6 if emp_ci==1 & pa52a==6 & pa42==1
replace ysec1=pa52/3 if emp_ci==1 & pa52a==5 & pa42==1
replace ysec1=pa52 if emp_ci==1 & pa52a==4 & pa42==1
replace ysec1=pa52*2 if emp_ci==1 & pa52a==3 & pa42==1
replace ysec1=pa52*4 if emp_ci==1 & pa52a==2 & pa42==1
replace ysec1=pa52*22 if emp_ci==1 & pa52a==1 & pa42==1
replace ysec1=. if pa52==9999999 

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

gen ylnmsec_ci=pa53a if emp_ci==1 & pa53==1
replace ylnmsec_ci=. if emp_ci==0
replace ylnmsec_ci=. if pa53a==999999

**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*egen ylm1_ci=rsum(ylmpri1_ci ylmsec1_ci)
*replace ylm1_ci=. if ylmpri1_ci==. & ylmsec1_ci==.


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
/*----------------------
    pa88B |      Freq.
----------+-----------
1      D¡a |         65
2   Semana |        368
3 Quincena |        164
4      Mes |        847
5 Trimestr |        230
6 Semestre |        109
7      a¤o |        164
8     Otro |        243
9       NR |         18
----------------------
*/


gen ayuda=pa83a/12 if emp_ci==1 & pa83b==8 & pa83<3
replace ayuda=pa83a/12 if emp_ci==1 & pa83b==7 & pa83<3
replace ayuda=pa83a/6 if emp_ci==1 & pa83b==6 & pa83<3
replace ayuda=pa83a/3 if emp_ci==1 & pa83b==5 & pa83<3
replace ayuda=pa83a if emp_ci==1 & pa83b==4 & pa83<3
replace ayuda=pa83a*2 if emp_ci==1 & pa83b==3 & pa83<3
replace ayuda=pa83a*4 if emp_ci==1 & pa83b==2 & pa83<3
replace ayuda=pa83a*30 if emp_ci==1 & pa83b==1 & pa83<3
replace ayuda=. if  pa83a==9999999

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

/*by idh_ch, sort: egen nrylmpri1_ch=sum(nrylmpri1_ci) if miembro_ci==1
replace nrylmpri1_ch=1 if nrylmpri1_ch>0 & nrylmpri1_ch<.
replace nrylmpri1_ch=. if nrylmpri1_ch==.*/


************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembro_ci==1
*by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembro_ci==1

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembro_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

/*by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembro_ci==1
replace ylmnr1_ch=. if nrylmpri1_ch==1*/

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

/*gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)*/

**************************
***ylmho_ci & ylm1ho_ci***
**************************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

/*gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)*/


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

gen byte yedc=.
replace yedc=0 if pe32==1 | pe32==2 
replace yedc=pe32a if pe32==3
replace yedc=6 if pe32==3 & pe32a>=6
replace yedc=pe32a if pe32==4
replace yedc=6 if pe32==4 & pe32a>=6
replace yedc=7+pe32a if pe32==5 | pe32==6
replace yedc=12 if pe32==5 & pe32a>=6 /* tiene 7 años de educacion sec pero su titulo es sec*/
replace yedc=12 if pe32==6 & pe32a>=6
replace yedc=13+pe32a if pe32==7 | pe32==8
replace yedc=18 if pe32==7 & pe32a>=6
replace yedc=15 if pe32==8 & pe32a>=2
replace yedc=19+pe32a if pe32==9
replace yedc=23 if pe32==9 & pe32a>=5

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
replace edupre_ci=1 if pe32==3
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (aedu_ci>12 & pe32==8) & aedu_ci~=.
replace eduac_ci=0 if (aedu_ci>12 & pe32==7) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
*modificada, ver nota al inicio de este programa*

gen asiste_ci=0
replace asiste_ci=1 if pe12==1 
replace asiste_ci=0 if pe12==1 & pe30>=1 & pe30!=.
*si lleva mas de una semana sin asistir, se considera como que NO asiste*
replace asiste_ci=. if pe12==.
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=pe29
label variable pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "Vacaciones" 2 "Enfermedad", add
label define pqnoasis_ci 3 "Lab domesticas" 4 "Por paro o huelga", add 
label define pqnoasis_ci 5 "Falta de Transporte" 6 "Mal clima" , add
label define pqnoasis_ci 7 "Falta de recursos" 8 "Cambio de domicilio" 9 "Trabajo" 10 "No le interesa" , add 
label define pqnoasis_ci 11 "Falta de profesores" 12 "Falta de instalaciones" 13 "Distancia" 14 "Otro" , add
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
replace edupub_ci=1 if pe14>=1 & pe14<=3
replace edupub_ci=0 if pe14==4 

/*Variables de Infraestructura del hogar*/

***************
**aguared_ch***
***************
gen aguared_ch=.
replace aguared_ch=1 if vi10==1 | vi10==2
replace aguared_ch=0 if vi10>2

****************
**aguadist_ch***
****************

gen aguadist_ch=.
replace aguadist_ch=1 if vi11==1 
replace aguadist_ch=2 if vi11==2
replace aguadist_ch=3 if vi11==3

****************
**aguamala_ch***
****************

gen aguamala_ch=.
replace aguamala_ch=0 if vi10>=0 
replace aguamala_ch=1 if vi10==3 | vi10==4 
replace aguamala_ch=1 if vi10==6 | vi10==8 | vi10==9

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=.
replace luz_ch=1 if vi19==1 | vi19==2 
replace luz_ch=0 if vi19==3 | vi19==4

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=.

****************
****bano_ch*****
****************

gen bano_ch=.
replace bano_ch=1 if vi16<5
replace bano_ch=0 if vi16==5

****************
****banoex_ch***
****************

gen banoex_ch=.
replace banoex_ch=1 if vi18==1
replace banoex_ch=0 if vi18==2

****************
****des1_ch*****
****************

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

gen piso_ch=.
replace piso_ch=0 if vi04==6 | vi04==7
replace piso_ch=1 if vi04==2 | vi04==3 | vi04==4 
replace piso_ch=2 if vi04==1 | vi04==5 

****************
****pared_ch****
****************

gen pared_ch=.
replace pared_ch=0 if vi03==5 | vi03==6 
replace pared_ch=1 if vi03==2 | vi03==3 | vi03==4
replace pared_ch=2 if vi03==1 | vi03==7

****************
****techo_ch****
****************

gen techo_ch=.


****************
****resid_ch****
****************

gen resid_ch=.
replace resid_ch=0 if vi27==1 | vi27==2 
replace resid_ch=1 if vi27==4
replace resid_ch=2 if vi27==3 
replace resid_ch=3 if vi27==5 


****************
****dorm_ch****
****************

gen dorm_ch=.
replace dorm_ch=vi06


****************
***cuartos_ch***
****************

gen cuartos_ch=vi05a

****************
***cocina_ch****
****************

gen cocina_ch=.
replace cocina_ch=0 if vi08~=1
replace cocina_ch=1 if vi08==1


****************
****telef_ch****
****************

gen telef_ch=.
replace telef_ch=0 if vi23==2
replace telef_ch=1 if vi23==1

****************
****refrig_ch***
****************

gen refrig_ch=.
replace refrig_ch=0 if vi35012==2 
replace refrig_ch=1 if vi35012==1 

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch****
****************

gen auto_ch=.
replace auto_ch=0 if vi35152==2
replace auto_ch=1 if vi35152==1

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
replace vivi1_ch=0 if vi02==1 
replace vivi1_ch=1 if vi02==2 
replace vivi1_ch=2 if vi02==3 | vi02==4 | vi02==5    

****************
****vivi2_ch****
****************

gen vivi2_ch=.
replace vivi2_ch=1 if vi02==2 | vi02==1
replace vivi2_ch=0 if vi02==3 | vi02==4 | vi02==5    

 
*******************
****viviprop_ch****
*******************

gen viviprop_ch=.
replace viviprop_ch=0 if vi31==1 | vi31==2 | vi31==5
replace viviprop_ch=1 if vi31==4 
replace viviprop_ch=2 if vi31==3 
replace viviprop_ch=3 if vi31==6 | vi31==7 | vi31==8 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=vi34

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.
replace vivialqimp_ch=vi33



gen howner=(viviprop_ch==1 | viviprop==2)
replace howner=. if viviprop_ch==.
gen floor=(piso_ch==1)
replace floor=. if piso_ch==.



save "X:\ARM\ECU\ECV\1994\Arm_data\ECU1994EA_BID.dta", replace



************************************************************************************
************************************************************************************

***********************
*** ECUADOR 1994    ***
***********************

gen sexo = pd02
recode sexo (9=.)
cap gen edad = pd03
gen factorex = factores
gen reljefe = pd04
 
** AREA
/*
 1. Urbana
 2. Rural
*/

** Gender classification of the population refering to the head of the household.

 sort nhid1 nhid2 nhid3 pd01

 gen id_hogar_=1 if reljefe==1
 gen id_hogar=sum(id_hogar_)

 gen	 sexo_d_=1 if reljefe==1 & sexo==1
 replace sexo_d_=2 if reljefe==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)

 tab sexo   [iw=factorex]
 tab sexo_d [iw=factorex]

 tab sexo sexo_d if reljefe==1
 
 sort id_hogar pd01 

** Years of education. 

/*

*¿Cuál es el nivel y el año más alto de educación que aprobó?
* Personas con 6 años y más que no salieron seis meses del hogar

pe32					p32a. Año de instrucción correspondiente 
1. Ninguno
2. Educación Básica de Adultos (EBA)
3. Pre primaria
4. Primario
5. Ciclo básico
6. Ciclo diversificado y/o técnico
7. Superior no universitario
8. Superior universitario
9. postgrado
Missing obs 48
*/

gen ultniv = pe32
gen ultgrad = pe32a

 gen	 anoest=-1 if edad<6
 replace anoest=0  if (ultniv==4 & ultgrad==0) | (ultniv>=1 & ultniv<=3) 
 replace anoest=1  if (ultniv==4 & ultgrad==1) 
 replace anoest=2  if (ultniv==4 & ultgrad==2) 
 replace anoest=3  if (ultniv==4 & ultgrad==3) 
 replace anoest=4  if (ultniv==4 & ultgrad==4) 
 replace anoest=5  if (ultniv==4 & ultgrad==5) 
 replace anoest=6  if (ultniv==4 & (ultgrad>=6 & ultgrad<.)) | (ultniv==5 & ultgrad==0) 
 replace anoest=7  if (ultniv==5 & ultgrad==1) 
 replace anoest=8  if (ultniv==5 & ultgrad==2)
 replace anoest=9  if (ultniv==5 & (ultgrad>=3 & ultgrad<=6)) | (ultniv==6 & ultgrad==0)
 replace anoest=10 if (ultniv==6 & (ultgrad>=1 & ultgrad<=4))
 replace anoest=11 if (ultniv==6 & ultgrad==5)
 replace anoest=12 if (ultniv==6 & ultgrad==6) | (ultniv==7 & ultgrad==0) | (ultniv==8 & ultgrad==0)
 replace anoest=13 if (ultniv==7 & ultgrad==1) | (ultniv==8 & ultgrad==1)
 replace anoest=14 if (ultniv==7 & ultgrad==2) | (ultniv==8 & ultgrad==2)
 replace anoest=15 if (ultniv==7 & ultgrad==3) | (ultniv==8 & ultgrad==3)
 replace anoest=16 if (ultniv==7 & (ultgrad>=4 & ultgrad<=6))  | (ultniv==8 & ultgrad==4)
 replace anoest=17 if (ultniv==8 & ultgrad==5)
 replace anoest=18 if (ultniv==8 & (ultgrad>=6 & ultgrad<=8)) |(ultniv==9 & ultgrad==0)
 replace anoest=19 if (ultniv==9 & (ultgrad==1 | ultgrad==2))
 replace anoest=20 if (ultniv==9 & (ultgrad>=3 & ultgrad<.))

***************************
** CONSUMPTION QUINTILES **
***************************

 gen uno=1 
 egen npers=sum(uno), by(id_hogar)
 
 gen double ypcapita=hhexp/npers

 gen vectoruno=1
 
** Economic Active Population 
* 10 years or more

 gen	 peaa=1 if pa01==1 | pa02==1 | pa03==1
 replace peaa=2 if pa04==1
 replace peaa=3 if peaa==. & (edad>=10 & edad<99)

 gen	 tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 

 rename pa13 ramap
 gen ramar=1 if ramap==1 | ramap==2 | ramap==5 | ramap==9
 replace ramar=2 if ramap>9
 label define ramarlabel 1 "agricultura, ganaderia y pesca" 2 "demas actividades"
 label value ramar ramarlabel
 
 gen	 indigena=1 if (pe11>=2 & pe11<=5) 
 replace indigena=0 if (pe11==1 | pe11==6 | pe11==7)

** Missings. Persons with less than 6 years of age
* Filling missings using the mother language
* 3. "Hijo (a) 5. Nieto (a)

 gen pertn_m=indigena if (reljefe==2 & sexo==2)
 egen pertn_mh=max(pertn_m), by(id_hogar)
 replace indigena=pertn_mh if edad<6 & (reljefe==3 | reljefe==5)

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

* pe12 => ¿Se matriculó en el presente año escolar?

* pe13. ¿Es la 1a vez que se matricula en este grado o curso?

 gen matric = pe12
 
 gen ultgrado6=1 if pe13==1 & (edad>=6 & edad<=11) & matric==1 & (ultniv==4 & ultgrad==6)

*¿Cuál es el nivel y el año más alto de educación que aprobó?
* Personas con 6 años y más que no salieron seis meses del hogar
/*
pe32 (ultniv)					p32a. Año de instrucción correspondiente 
1. Ninguno
2. Educación Básica de Adultos (EBA)
3. Pre primaria
4. Primario
5. Ciclo básico
6. Ciclo diversificado y/o técnico
7. Superior no universitario
8. Superior universitario
9. postgrado
Missing obs 48
*/

/* 
Probably enrolled in primary =>
Persons enrolled that have not aproved any education level, assuming that they do not attend pre-primary
school before primary education. (plausible assumption considering the age range used)
*/

 gen probprim=1 if (edad>=7 & edad<=11) & matric==1 & ultniv==1 

 gen	 NERP=0 if (edad>=6 & edad<=11) & (matric==1 | matric==2)
 replace NERP=1 if (edad>=6 & edad<=11) & ((matric==1) & ((((ultniv==3 & ultgrad==1) | probprim==1)| ((ultniv==4) & ((ultgrad>=0 & ultgrad<=5) | ultgrado6==1)))))
	 
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen ultgradosec6=1 if pe13==1 & (edad>=12 & edad<=17) & matric==1 & (ultniv==6 & ultgrad==6)

 gen	 NERS=0 if (edad>=12 & edad<=17) & (matric==1 | matric==2)
 replace NERS=1 if (edad>=12 & edad<=17) & ((matric==1) & ((ultniv==4 & (ultgrad==6 & ultgrado6!=1))  | (ultniv==5) | ((ultniv==6) & ((ultgrad>=0 & ultgrad<=5) |ultgradosec6==1 ))))

** Upper secondary
* 2o Ciclo de enseñanza secundaria

 gen     NERS2=0 if (edad>=15 & edad<=17) & (matric==1 | matric==2)
 replace NERS2=1 if (edad>=15 & edad<=17) & ((matric==1) & (((ultniv==6 | ultniv==5)  & (ultgrad>=3 & ultgrad<=5)) | ultgradosec6==1))
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

*pe10. ¿Sabe leer y escribir? (personas de 6 años y más)

gen alfabet = pe10

 gen 	 ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1) 

** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if ((matric==1) & ((((ultniv==3 & ultgrad==1) | probprim==1)| ((ultniv==4) & ((ultgrad>=0 & ultgrad<=5) | ultgrado6==1)))))
 gen sec=1  if ((matric==1) & ((ultniv==4 & (ultgrad==6 & ultgrado6!=1))  | (ultniv==5) | ((ultniv==6) & ((ultgrad>=0 & ultgrad<=5) |ultgradosec6==1 ))))
 gen ter=1  if ((matric==1) & ((ultniv==6 & ultgrad==5)  | (ultniv==7 & ultgrad<=3) | (ultniv==8 & ultniv<=5)))
 
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
* Without Domestic Service

/*
Pa20   Posición ocupacional
1. Obrero/empleado de gobierno              
2. Obrero/empleado privado
3.  Jornalero o peón agrícola
4. Patrón/empleador
5. Cuenta propia
6. Trab.propia finc
7. Trab.famil.sin pago
8. Trab.no fam.s.pago
9. Empleado doméstico
*/

gen categp = pa20

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categp>=1 & categp<=2) & (ramar==2) & (peaa==1)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categp>=1 & categp<=2) & (ramar==2) & (peaa==1) & (sexo==2)

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & ((categp>=1 & categp<=2) | categp==9) & (ramar==2) & (peaa==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & ((categp>=1 & categp<=2) | categp==9) & (ramar==2) & (peaa==1) & (sexo==2)

*Proportion of 1 Year Old Children Immunized Against Measles*
* 1 - 4
* De las sigs. vacunas, ¿Cuáles ha recibido?

 cap gen sarampion = ps25g

cap  gen	 MEASLES=0 if (edad>=1 & edad<=4) & (sarampion==1 | sarampion==2)
cap  replace MEASLES=1 if (edad>=1 & edad<=4) & (sarampion==1)

/*Proportion of Births Attended by Skilled Health Personnel*/

/*
15 to 49
PF14. ¿En su último parto por quien se hizo atender?

1. Obstetriz
2. Comadrona
3. Médico               
4. Enfermera 
5. Auxiliar
6. Familiar 
7. Vecino/amigo
8. Ella misma
9. otro

*/

* gen qaten = pf14 

cap gen	 SKILLED=0 if (qaten>=1 & qaten<=9) & (edad>=15 & edad<=49) & sexo==2
cap replace SKILLED=1 if (qaten>=1 & qaten<=5) & (edad>=15 & edad<=49) & sexo==2

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator

/*
vi19
19. ¿Con que tipo de alumbrado cuenta principalmente este hogar?
 1. Empresa eléctrica pública
 2. Planta eléctrica privada
 3. Vela/candil/gas
 4. Ninguno
*/

 gen	 ELEC=0 if (vi19<=4)
 replace ELEC=1 if (vi19<=2)

** Target 9, Indicator: Proportion of the population using solidfuels (%)
/*
vi29 
¿En este hogar se cocina principalmente con?
combustible para cocinar.
			
1. Gas
2. Gasolina
3. Kerex
4. Leña
5. Carbón
6. Electricidad
7. Otro
8. No cocina

*/

gen combusti = vi29

 gen	 SFUELS=0 if (combusti>=1 & combusti<=8)
 replace SFUELS=1 if (combusti==4 | combusti==5)
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

/* Questionnarie pattern not comparable with other years
10. ¿De dónde obtiene el agua  este hogar?	11. Donde esta ubicado el suministro 	12. El suministro queda
						del agu					vi12
											1. Muy cerca
* vi10. fuente de agua				*vi11 					2. Cerca
											3. Lejos
1. red pública			==>11		1. Dentro de la vivienda		4. Muy lejos
2. otra fuente por tubería      ==>11		2. Fuera de la vivienda, pero  
3. agua de lluvia		==>11		   dentro del terreno
4. pozo				==> 12		3. Fuera de la vivienda, lote o terreno
5. carro repartidor y  red	==> 12
6. río, vertiente o similar	==> 12
7. pila o llave pública		==> 12	
8. Carro repartidor		==> 12
9. otro				==> 12
				
*/

gen agua = vi10
gen lugabast = vi11

 gen	 WATER=0 if (agua>=1 & agua<=9)
 replace WATER=1 if ((agua>=1 & agua<=5) | agua==7) 

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*
* vi16. servicio higienico					vi18. usoserv
¿Con qué tipo de servicio higiénico cuenta el hogar?		El servicio higiénico es de uso:

1. excusado y alcantarillado		=>			1. Exclusivo del hogar
2. excusado y pozo séptico		=>			2. Compartido con otros
3. excusado y pozo ciego		=>
4. letrina				=>
5. no tiene	==>19

*/

gen servsani = vi16
gen usoserv = vi18

 gen	 SANITATION=0 if (servsani>=1 & servsani<=5) 
 replace SANITATION=1 if (servsani>=1 & servsani<=2) 
 	
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*

* vi32. Tenencia de la vivienda		vi02
La vivienda que ocupa este hogar es:	Tipo de vivienda	

1. En arriendo				1. Casa, villa o mediagua
2. anticresis y arriendo		2. Departamento
3. propia y la está pagando		3. Cuarto en inquilinato
4. propia y totalmente pagada		4. Rancho o choza			
5. anticresis				5. Otro
6. cedida								
7. recibida por servicios					
8. otra								

vi04. Piso de la vivienda		vi03. Paredes
¿Cuál es el material predominante 	¿Cuál es el material predominante 
del piso de la vivienda?		de las paredes de la vivienda?
1. entablado				1. Bloque o ladrillo
2. madera / parket			2. Adobe o tapia
3. baldosa / vinyl			3. Madera
4. cemento/ ladrillo			4. Bahereque (caña revestida)
5. caña					5. Caña
6. tierra				6. Otro
7. otro	

* vi05a Número de cuartos 
¿De cuántos cuartos dispone este HOGAR, sin incluir cuartos de cocina, baños, garage o los dedicados
exclusivamente para negocios?
a. Total
b. en forma exclusiva
c. en forma compartida

*/


gen tipoviv = vi02
gen tenencia = vi31
gen piso = vi04
gen paredes = vi03
gen nrocuart = vi05a

 gen var1=1
 egen totpers=sum(var1), by(id_hogar)

 gen persroom=totpers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if (tipoviv>=1 & tipoviv<=5) & (tenencia>=1 & tenencia<=8) /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==4) 		 | (tenencia>=6 & tenencia<=7)

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if (paredes>=1 & paredes<=6) & (piso>=1 & piso<=7) /* Total population excluding missing information */
 replace secten_2=1 if (paredes>=4 & paredes<=5) | (piso>=5 & piso<=6)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	    if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator
* vi04. Piso de la vivienda

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=7)
 replace DIRT=1 if (piso==6)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

/*
  Definition of unemployment no comparable with the ENEMDUR Survey,
  thus this indicator is not presented for this year.

  ** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

  noisily display "Unemployment Rate 15 to 24"
  global variable UNMPLYMENT15	
  gen	  UNMPLYMENT15=0 if (tasadeso==0 | tasadeso==1) & (edad>=15 & edad<=24)
  replace UNMPLYMENT15=1 if (tasadeso==1) 		& (edad>=15 & edad<=24)
  global indicador 45 " "

*/

gen telefono = vi23

* Gender classification of the population refers to the head of the household.

 gen	 TELEPHONE=0 if (telefono==1 | telefono==2) /* Total population excluding missing information */
 replace TELEPHONE=1 if (telefono==1)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono==1 | telefono==2) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1)
 	
************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen	 CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & (peaa==1)

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if reljefe==1

	
 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
 	
******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

*** Proportion of population below corresponding grade for age

 gen     rezago=0       if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7
 
 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==17
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if (edad>=7 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=7 & edad<=17) & (rezago==1)
	
* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
	
* Average years of education of the population 15+


 gen     AEDUC_15=anoest if ((edad>=15 & edad<.) & (anoest>=0 & anoest<99))
	
 gen     AEDUC_15_24=anoest if ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if ((edad>=25 & edad<.) & (anoest>=0 & anoest<99))

* Grade for age

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=17) & (anoest>=0 & anoest<99)

* Grade for age primary

 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)

* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=17) & (anoest>=0 & anoest<99)


save "X:\ARM\ECU\ECV\1994\Arm_data\ECU1994EA_BID.dta", replace


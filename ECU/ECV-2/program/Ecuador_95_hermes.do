****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA EL ECUADOR 1995******************************
****************************************************************************

clear
capture log close
cd X:\ARM\ECU\ECV\1995\Data
set mem 130m
set more off

use "X:\ARM\ECU\ECV\1995\Data\ecu95_ecv.dta" 


sort CIUDAD ZONA SECTOR VIVIEN HOGAR 
merge CIUDAD ZONA SECTOR VIVIEN HOGAR using "X:\ARM\ECU\ECV\1995\Datos_originales\vivi_ecu95.dta" 
drop _merge

***************
***factor_ch***
***************

gen factor_ch=FEXP
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************
gen str_zona = string(ZONA)
gen str_sector = string(SECTOR)
gen str_vivien = string(VIVIEN)
gen str_hogar = string(HOGAR)

gen IDHOGAR= str_zona+str_sector+str_vivien+str_hogar
drop str_zona str_sector str_vivien str_hogar
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

gen byte zona_c=0 if AREA~=1 /* Es una buena forma de generar xq lo diferente de 2 = miss*/
replace zona_c=1 if AREA==1
replace zona_c=. if AREA==.

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

gen anio_c=1995
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
replace relacion_ci=4 if PD04>=4 & PD04<=8
replace relacion_ci=5 if PD04==11 /* abonado o huesped */
replace relacion_ci=5 if PD04==10 /* abonado o huesped */
replace relacion_ci=6 if PD04==9

/*
----------------------
     PD04 |      Freq.
----------+-----------
     Jefe |      5,810
 Esposos- |      4,250
 Hijo-Hij |     13,297
 Yerno-Nu |        390
 Nieto-Ni |      1,550
 Padres-S |        309
 Hermano- |        470
 Otros pa |        556
 Empleado |        165
 Pensioni |          9
 Otros no |        135
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
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if PD05==6
replace civil_ci=2 if PD05==1 | PD05==2
replace civil_ci=3 if PD05==3 | PD05==4
replace civil_ci=4 if PD05==5

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci



/*----------------------
     PD05 |      Freq.
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

gen byte emp_ci=(PA01==1 | PA02==1 | PA03==1)

/* Es empleado: esta trabajando, o no trabajo pero si tenia trabjao porque estabe en 
vaciones*/

****************
***desemp1_ci***
****************

gen desemp1_ci=(PA01==2 & PA04==1)

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

gen desalent_ci=(PA01==2 & PA04==2)
replace desalent_ci= 1 if PA01==2 & PA03==2

***************
***subemp_ci***
***************
gen subemp_ci=(emp_ci==1 & PA59==1 & PA57<=30)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(emp_ci==1 & PA59==2 & PA57<=30)

*****************
***horaspri_ci***
*****************

gen horaspri_ci=PA18*7  if emp_ci==1 
replace horaspri_ci=.  if PA18==99

*****************
***horastot_ci***
*****************

gen horastot_ci=PA57  if emp_ci==1 
replace horastot_ci=.  if PA57==999


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if PA20==3 | PA20==9
replace categopri_ci=2 if PA20==4 | PA20==10
replace categopri_ci=3 if PA20==1 | PA20==2 | PA20==7 | PA20==8 | PA20==6 | PA20==12 | PA20==13
replace categopri_ci=4 if PA20==5 | PA20==11

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
replace categosec_ci=1 if PA50==3 | PA50==9
replace categosec_ci=2 if PA50==4 | PA50==10
replace categosec_ci=3 if PA50==1 | PA50==2 | PA50==7 | PA50==8 | PA50==6 | PA50==12
replace categosec_ci=4 if PA50==5 | PA50==11

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

******************
**dcategopri_ci **
******************

gen dcategosec_ci=(PA50>=7 & PA50<=12)
label variable dcategosec_ci "Categoria ocupacional trabajo secundario agricola"
/*Varible creada para identificar si su posicion ocupacional es agricola(1) o no(0)*/

*****************
***contrato_ci***
*****************

gen contrato_ci=.
replace contrato_ci=1 if PA23==1 
replace contrato_ci=0 if PA23==2

*****************
***dcontrato_ci***
*****************

gen dcontrato_ci=.
replace dcontrato_ci=1 if PA23==3
replace dcontrato_ci=0 if PA23>=1 & PA23<=2
label variable dcontrato_ci "Si el contrato fue con el gobierno-nombramiento"
/* Si el contrato fue con el gobierno "Nombramiento"*/

*****************
***tcontrato_ci***
*****************

gen tcontrato_ci=.
replace tcontrato_ci=0 if PA24==1 & PA23==1 
replace tcontrato_ci=1 if PA24==2 & PA23==1 
label variable tcontrato_ci "Si el contrato indefinifo (0) o a termino fijo (1)"
/*Se puede saber si el contrato de trabajo es indefinifo (0) o a termino fijo (1) */


***************
***segsoc_ci***
***************

*Solo para los asalariados

gen segsoc_ci=.
replace segsoc_ci=1 if PA28==1
replace segsoc_ci=0 if PA28==0


***************
***dsegsoc_ci***
***************
/* Creamos una varible de seguridad pero que esta comprendida para todo la
poblacion, y es la reunion de los siguientes tipo de seguro (1) y si no tieno (0)

. table PS36

----------------------
     PS36 |      Freq.
----------+-----------
 Seguro p |        562
 IESS, se |      2,696
 IESS, se |      2,349
     Otro |        234
  Ninguno |     21,090
       NR |          2
----------------------

*/
gen isegsoc_ci=(PS36>=1 & PS36<=4)



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
table PA19

----------------------
     PA19 |      Freq.
----------+-----------
 de 1 per |      2,125
 de 2 a 5 |      6,618
 de 6 a 1 |      1,377
 de 11 a  |        897
 de 31 a  |        298
 de 51 a  |        282
 de 101 y |      1,363
       NR |         19
----------------------
*/

gen firmapeq_ci=.
replace firmapeq_ci=0 if PA19<=2
replace firmapeq_ci=1 if PA19>=2
replace firmapeq_ci=. if PA19 ==.

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
replace durades_ci=. if emp_ci==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=PA15+PA15A/12+PA15B/(4*12)
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
la cual no dice la periodicidad, se convierte en missing en la 
nueva variable*/

gen yprid=PA30/12 if emp_ci==1 & PA30A==7 
replace yprid=PA30/6 if emp_ci==1 & PA30A==6
replace yprid=PA30/3 if emp_ci==1 & PA30A==5
replace yprid=PA30 if emp_ci==1 & PA30A==4 
replace yprid=PA30*2 if emp_ci==1 & PA30A==3
replace yprid=PA30*4 if emp_ci==1 & PA30A==2
replace yprid=PA30*30 if emp_ci==1 & PA30A==1 
replace yprid=. if categopri_ci<=2
replace yprid=. if PA30==9999999 

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


gen ingrneto=PA21/12 if emp_ci==1 & PA21A==7 
replace ingrneto=PA21/6 if emp_ci==1 & PA21A==6
replace ingrneto=PA21/3 if emp_ci==1 & PA21A==5
replace ingrneto=PA21 if emp_ci==1 & PA21A==4 
replace ingrneto=PA21*2 if emp_ci==1 & PA21A==3
replace ingrneto=PA21*4 if emp_ci==1 & PA21A==2
replace ingrneto=PA21*30 if emp_ci==1 & PA21A==1 
replace ingrneto=. if PA21==9999999 
replace ingrneto=. if categopri_ci>2

gen yprijbi=PA30/12 if emp_ci==1 & PA30A==7 
replace yprijbi=PA30/6 if emp_ci==1 & PA30A==6
replace yprijbi=PA30/3 if emp_ci==1 & PA30A==5
replace yprijbi=PA30 if emp_ci==1 & PA30A==4 
replace yprijbi=PA30*2 if emp_ci==1 & PA30A==3
replace yprijbi=PA30*4 if emp_ci==1 & PA30A==2
replace yprijbi=PA30*30 if emp_ci==1 & PA30A==1 
replace yprijbi=. if PA30==9999999
replace yprijbi=. if categopri_ci>2

egen ylmpri_ci=rsum(yprijbi yprid)
replace ylmpri_ci=. if yprid==. & yprijbi==.
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
replace food1=PA33A if emp_ci==1 & PA33==1 & PA33B==4
replace food1=PA33A*2 if emp_ci==1 & PA33==1 & PA33B==3
replace food1=PA33A*4 if emp_ci==1 & PA33==1 & PA33B==2
replace food1=PA33A*30 if emp_ci==1 & PA33==1 & PA33B==1
replace food1=. if PA33A==999999
/*
   
   . table PA33B
   
   ----------------------
       PA33B |      Freq.
   ----------+-----------
         D¡a |      1,528
      Semana |        201
    Quincena |         28
         Mes |        224
    Trimestr |          2
         A¤o |          1
        Otro |          5
          NR |          9
   ----------------------
   
Ropa
----------------------
    PA35B |      Freq.
----------+-----------
        1 |        893
        2 |        270
        3 |         54
        4 |         31
        5 |          7
        6 |          5
        7 |          6
        8 |          3
        9 |         11
----------------------

*/
gen ropa1=.
replace ropa1=PA35A*(9/12) if emp_ci==1 & PA35==1 & PA35B==9
replace ropa1=PA35A*(8/12) if emp_ci==1 & PA35==1 & PA35B==8
replace ropa1=PA35A*(7/12) if emp_ci==1 & PA35==1 & PA35B==7
replace ropa1=PA35A*(6/12) if emp_ci==1 & PA35==1 & PA35B==6
replace ropa1=PA35A*(5/12) if emp_ci==1 & PA35==1 & PA35B==5
replace ropa1=PA35A*(4/12) if emp_ci==1 & PA35==1 & PA35B==4
replace ropa1=PA35A*(3/12) if emp_ci==1 & PA35==1 & PA35B==3
replace ropa1=PA35A*(2/12) if emp_ci==1 & PA35==1 & PA35B==2
replace ropa1=PA35A*(1/12) if emp_ci==1 & PA35==1 & PA35B==1
replace ropa1=0 if emp==1 & PA35A==.
replace ropa1=. if PA35A==999999

gen merca1=.

gen vivi1=.
replace vivi1=PA34A if emp_ci==1 & PA34==1
replace vivi1=0 if emp==1 & PA34==.


gen trans1=.
replace trans1=PA36A if emp_ci==1 & PA36==1 
replace trans1=PA36A if emp_ci==1 & PA36==2 
replace trans1=0 if emp==1 & PA36==.


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


gen ysec1=PA53/12 if emp_ci==1 & PA53A==7 & PA43==1
replace ysec1=PA53/6 if emp_ci==1 & PA53A==6 & PA43==1
replace ysec1=PA53/3 if emp_ci==1 & PA53A==5 & PA43==1
replace ysec1=PA53 if emp_ci==1 & PA53A==4 & PA43==1
replace ysec1=PA53*2 if emp_ci==1 & PA53A==3 & PA43==1
replace ysec1=PA53*4 if emp_ci==1 & PA53A==2 & PA43==1
replace ysec1=PA53*30 if emp_ci==1 & PA53A==1 & PA43==1
replace ysec1=. if PA53==9999999 
replace ysec1=. if categopri_ci<=2

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

gen ylnmsec_ci=PA55A if emp_ci==1 & PA55==1
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
/*----------------------
    PA88B |      Freq.
----------+-----------
1      D¡a |         65
2   Semana |        368
3 Quincena |        164
4      Mes |        847
5 Trimestr |        230
6 Semestre |        109
7      A¤o |        164
8     Otro |        243
9       NR |         18
----------------------
*/


gen ayuda=PA88A/12 if emp_ci==1 & PA88B==8 & PA88<5
replace ayuda=PA88A/12 if emp_ci==1 & PA88B==7 & PA88<5
replace ayuda=PA88A/6 if emp_ci==1 & PA88B==6 & PA88<5
replace ayuda=PA88A/3 if emp_ci==1 & PA88B==5 & PA88<5
replace ayuda=PA88A if emp_ci==1 & PA88B==4 & PA88<5
replace ayuda=PA88A*2 if emp_ci==1 & PA88B==3 & PA88<5
replace ayuda=PA88A*4 if emp_ci==1 & PA88B==2 & PA88<5
replace ayuda=PA88A*30 if emp_ci==1 & PA88B==1 & PA88<5
replace ayuda=. if PA88A==999999

gen cuotalim=.

gen alqui=.

gen alqneg=.

gen jubil=PA89A if emp_ci==1 & PA89==1
replace jubil=. if PA89A==999999

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

gen otross=PA90A if emp_ci==1 & PA90==1
replace otross=otross+PA91A if emp_ci==1 & PA91==1
replace otross=. if PA90A==999999
replace otross=. if PA91A==999999

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

gen byte yedc=.
replace yedc=0 if PE33>=1 & PE33<=2
replace yedc=PE34 if PE33==3 
replace yedc=6 if PE33==3 & PE34>=6  
replace yedc=PE34 if PE33==4 
replace yedc=6 if PE33==4 & PE34>=6  
replace yedc=7+PE34 if PE33==5
replace yedc=12 if PE33==6 & PE34>=6 /* tiene 7 años de educacion sec pero su titulo es sec*/
replace yedc=12+PE34 if PE33==8 | PE33==7
replace yedc=17 if PE33==6 & PE34>=5
replace yedc=14 if PE33==7 & PE34>=2
replace yedc=18+PE34 if PE33==8
replace yedc=22 if PE33==8 & PE34>=5
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
replace edupub_ci=1 if PE15==1 
replace edupub_ci=1 if PE15==2
replace edupub_ci=1 if PE15==3
replace edupub_ci=1 if PE15>=5
replace edupub_ci=0 if PE15==4 

/*Variables de Infraestructura del hogar*/

***************
**aguared_ch***
***************
gen aguared_ch=.

****************
**aguadist_ch***
****************

gen aguadist_ch=.

****************
**aguamala_ch***
****************

gen aguamala_ch=.

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=.

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

****************
****banoex_ch***
****************

gen banoex_ch=.

****************
****des1_ch*****
****************

gen des1_ch=.

****************
****des2_ch*****
****************

gen des2_ch=.
 
****************
****piso_ch*****
****************

gen piso_ch=.

****************
****pared_ch****
****************

gen pared_ch=.

****************
****techo_ch****
****************

gen techo_ch=.

****************
****resid_ch****
****************

gen resid_ch=.

****************
****dorm_ch****
****************

gen dorm_ch=.

****************
***cuartos_ch***
****************

gen cuartos_ch=.

****************
***cocina_ch****
****************

gen cocina_ch=.

****************
****telef_ch*****
****************

gen telef_ch=.

****************
****refrig_ch***
****************

gen refrig_ch=.

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch*****
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

****************
****vivi2_ch****
****************

gen vivi2_ch=.

*******************
****viviprop_ch****
*******************

gen viviprop_ch=.

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch*****
*******************

gen vivialq_ch=.

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.

save "X:\ARM\ECU\ECV\1995\Arm_data\ECU1995EA_BID.dta", intercooled replace
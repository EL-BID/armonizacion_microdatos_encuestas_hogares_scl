/*
February 7/2007 
the following line was included in order to count the people responding salary in hours
replace ypri=l7a*horaspri_ci*4.3  if l7b==1
*/

****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA PERU - ENNIV 1997****************************
****************************************************************************

clear
capture log close
set memory 300m
set more off

local in="X:\ARM\PER\ENNIV\"
use "`in'1997\Orig_data\per_97enniv.dta"


***************
***factor_ch***
***************

gen factor_ch=expan
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************

sort segmento vivienda hogar 
gen first=0
by segmento vivienda hogar: replace first=1 if _n==1
gen vectoru=1
gen double idy=sum(vectoru) if first==1
recode idy .=0
egen double idh_ch=sum(idy), by (segmento vivienda hogar)
drop vectoru idy

label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=b00
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=1 if domi==1 | domi==2 | domi==4 | domi==6
replace zona_c=0 if domi==3 | domi==5 | domi==7

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="PER"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1997
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen monthy=mes_enc if first==1
recode monthy .=0
egen byte mes_c=sum(monthy), by (idh_ch)
drop first monthy

label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*No hay empleados domésticos ni pensionistas*/

gen relacion_ci=.
replace relacion_ci=1 if b1==1
replace relacion_ci=2 if b1==2
replace relacion_ci=3 if b1==3
replace relacion_ci=4 if b1>=4 & b1<=7
replace relacion_ci=5 if b1==0

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci

/*Hay unos 9 casos en donde la relación de parentesco que se reporta es de esposo/a
pero por la edad se nota que son hijos, además ya hay una esposa con una edad razonable en 
el hogar. Por eso los recodifico*/

replace relacion_ci=3 if idh_ch==2537 & idp_ci==3
replace relacion_ci=3 if idh_ch==2537 & idp_ci==4
replace relacion_ci=3 if idh_ch==3793 & idp_ci==3
replace relacion_ci=3 if idh_ch==3793 & idp_ci==4
replace relacion_ci=3 if idh_ch==3793 & idp_ci==5
replace relacion_ci=3 if idh_ch==1315 & idp_ci==3
replace relacion_ci=3 if idh_ch==2615 & idp_ci==4
replace relacion_ci=3 if idh_ch==2673 & idp_ci==2
replace relacion_ci=3 if idh_ch==2673 & idp_ci==3


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=expan
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=b2

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=b3

label variable edad_ci "Edad del individuo"


*****************
***estcivil_ci***
*****************

gen estcivil_ci=.
replace estcivil_ci=1 if b5==6
replace estcivil_ci=2 if b5==1 | b5==2
replace estcivil_ci=3 if b5==4 | b5==5
replace estcivil_ci=4 if b5==3

label variable estcivil_ci "Estado civil"
label define estcivil_ci 1 "Soltero" 2 "Union formal o informal"
label define estcivil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value estcivil_ci estcivil_ci


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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembro_ci***
****************

gen miembro_ci=(relacion_ci<6)
label variable miembro_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los mayores a 6 años.*/ 

************
***emp_ci***
************

gen byte emp_ci=0
replace emp_ci=1 if k6==1 | k10==8 | k11==1

/*Hay un grupo de 110 personas que dicen tener un empleo fijo aunque
no trabajaron en la última semana, a este grupo no se les hacen las preguntas
de empleados, por lo que no tenemos información sobre las características del
empleo e ingresos. Lo mismo sucede con un grupo de 8 trabajadores que están de
vacaciones o licencia. Voy a generar una dummy para identificarlos*/

gen licencia=(k10==8 | k11==1)


****************
***desemp1_ci***
****************

gen desemp1_ci=(emp_ci==0 & k7==1)
replace desemp1_ci=. if emp_ci==.

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(desemp1_ci==1 | (k7==2 & (k10==6 | k10==7 | k10==10)))
replace desemp2_ci=0 if emp_ci==1
replace desemp2_ci=. if emp_ci==. 

****************
***desemp3_ci***
****************
gen desemp3_ci=.

*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1
replace pea1_ci=. if emp_ci==.


*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1
replace pea2_ci=. if emp_ci==.

*************
***pea3_ci***
*************
gen pea3_ci=.

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & k7==2 & k10==12)


*****************
***horaspri_ci***
*****************

gen horaspri_ci=l3*l4
replace horaspri_ci=0 if l3==0 & l4==0
replace horaspri_ci=. if emp_ci~=1 


*****************
***horastot_ci***
*****************

gen horassec=n03*n04
replace horassec=0 if n03==0 & n04==0

egen horastot_ci=rsum(horaspri_ci horassec)
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

***************
***subemp_ci***
***************

gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & horastot_ci<=30) & p1==1 
replace subemp_ci=. if emp_ci==.

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if (emp_ci==1 & horastot_ci<=30) & p1==2
replace tiempoparc_ci=. if emp_ci==. 


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if l8==1 & m18>1
replace categopri_ci=2 if l8==1 & m18==1 
replace categopri_ci=3 if l8==3 
replace categopri_ci=4 if l8==2 

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if n08==1 & o18>1
replace categosec_ci=2 if n08==1 & o18==1
replace categosec_ci=3 if n08==3 
replace categosec_ci=4 if n08==2 

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
***contrato_ci***
*****************
/*Defino como trabajador con un contrato a aquel que ha
firmado un contrato por tiempo indefinido o definido.
Esta pregunta es sólo para los empleados dependientes*/

gen contrato_ci=.
replace contrato_ci=1 if m14==1 | m14==2
replace contrato_ci=0 if m14>2
replace contrato_ci=. if categopri_ci~=3


***************
***segsoc_ci***
***************

/*Esta asegurado al ISSP o tiene otro seguro medico*/

gen segsoc_ci=.
replace segsoc_ci=1 if m15==1 | m15==3 | m15==4
replace segsoc_ci=0 if m15==5 | m15==2
replace segsoc_ci=. if emp_ci~=1


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & m19==1


*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if m18<=2
replace firmapeq_ci=0 if m18>=3 & m18<=8


*****************
***spublico_ci***
*****************
/*Sólo para los empleados dependientes*/

gen spublico_ci=.
replace spublico_ci=1 if (m13==1 | m13==3)
replace spublico_ci=0 if (m13==2 | m13==4 | m13==5)
replace spublico_ci=. if categopri~=3


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (l1>=211 & l1<=395) & emp_ci==1
replace ocupa_ci=2 if (l1>=111 & l1<=148) & emp_ci==1
replace ocupa_ci=3 if (l1>=411 & l1<=462) & emp_ci==1
replace ocupa_ci=4 if (l1>=570 & l1<=583) | (l1>=911 & l1<=931) & emp_ci==1
replace ocupa_ci=5 if (l1>=511 & l1<=565) | (l1>=941 & l1<=961) & emp_ci==1
replace ocupa_ci=6 if (l1>=611 & l1<=641) | (l1>=970 & l1<=973) & emp_ci==1
replace ocupa_ci=7 if (l1>=711 & l1<=886) | (l1>=981 & l1<=987) & emp_ci==1
replace ocupa_ci=8 if (l1>=11 & l1<=23) & emp_ci==1



*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (l2>=100 & l2<=500) & emp_ci==1
replace rama_ci=2 if (l2>=1000 & l2<=1429) & emp_ci==1
replace rama_ci=3 if (l2>=1500 & l2<=3720) & emp_ci==1
replace rama_ci=4 if (l2>=4000 & l2<=4100) & emp_ci==1
replace rama_ci=5 if (l2>=4500 & l2<=4550) & emp_ci==1
replace rama_ci=6 if (l2>=5000 & l2<=5520) & emp_ci==1 
replace rama_ci=7 if (l2>=6000 & l2<=6420) & emp_ci==1
replace rama_ci=8 if (l2>=6500 & l2<=7499) & emp_ci==1
replace rama_ci=9 if (l2>=7500 & l2<=9503) & emp_ci==1


****************
***durades_ci***
****************
/*En meses*/
gen durades_ci=k8/4.3 if desemp2_ci==1



*******************
***antiguedad_ci***
*******************
/*En años*/
gen ant1=l6a
gen ant2=l6b/12
gen ant3=l6c/(52)

egen antiguedad_ci=rsum(ant1 ant2 ant3)
replace antiguedad_ci=. if ant1==. & ant2==. & ant3==.
replace antiguedad_ci=. if emp_ci~=1


*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************

*Salario básico
gen ypri=.
replace ypri=l7a*horaspri_ci*4.3  if l7b==1
replace ypri=l7a*30  if l7b==2
replace ypri=l7a*4.3 if l7b==3
replace ypri=l7a*2   if l7b==4
replace ypri=l7a     if l7b==5
replace ypri=l7a/3   if l7b==6
replace ypri=l7a/6   if l7b==7
replace ypri=l7a/12  if l7b==8
replace ypri=0 if l7a==0 & l7b==0

*Salario adicional
gen adic=.
replace adic=m10b*30  if m10c==2
replace adic=m10b*4.3 if m10c==3
replace adic=m10b*2   if m10c==4
replace adic=m10b     if m10c==5
replace adic=m10b/3   if m10c==6
replace adic=m10b/6   if m10c==7
replace adic=m10b/12  if m10c==8
replace adic=0 if m10a==2
replace adic=. if categopri_ci<3


*Ingreso laboral monetario total

egen ylmpri_ci=rsum(ypri adic)
replace ylmpri_ci=. if ypri==. & adic==. 
replace ylmpri_ci=0 if categopri_ci==4 | licencia==1
replace ylmpri_ci=. if emp_ci~=1 


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

*Remuneración en bienes y servicios

gen bsss=.
replace bsss=m11b*30  if m11c==2
replace bsss=m11b*4.3 if m11c==3
replace bsss=m11b*2   if m11c==4
replace bsss=m11b     if m11c==5
replace bsss=m11b/3   if m11c==6
replace bsss=m11b/6   if m11c==7
replace bsss=m11b/12  if m11c==8
replace bsss=0 if m11a==2
replace bsss=. if categopri_ci<3

*Remuneracion en ropa, vivienda u otros

gen ruvo=.
replace ruvo=m12b*30  if m12c==2
replace ruvo=m12b*4.3 if m12c==3
replace ruvo=m12b*2   if m12c==4
replace ruvo=m12b     if m12c==5
replace ruvo=m12b/3   if m12c==6
replace ruvo=m12b/6   if m12c==7
replace ruvo=m12b/12  if m12c==8
replace ruvo=0 if m12a==2
replace ruvo=. if categopri_ci<3

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(bsss ruvo)
replace ylnmpri_ci=. if bsss==. & ruvo==.
replace ylnmpri_ci=0 if categopri_ci==4
replace ylnmpri_ci=. if emp_ci~=1


***************
***ylmsec_ci***
***************

*Salario básico
gen ysec=.
replace ysec=n7a*30  if n7b==2
replace ysec=n7a*4.3 if n7b==3
replace ysec=n7a*2   if n7b==4
replace ysec=n7a     if n7b==5
replace ysec=n7a/3   if n7b==6
replace ysec=n7a/6   if n7b==7
replace ysec=n7a/12  if n7b==8
replace ysec=0 if n7a==0 & n7b==0

*Salario adicional
gen adicsec=.
replace adicsec=o10b*30  if o10c==2
replace adicsec=o10b*4.3 if o10c==3
replace adicsec=o10b*2   if o10c==4
replace adicsec=o10b     if o10c==5
replace adicsec=o10b/3   if o10c==6
replace adicsec=o10b/6   if o10c==7
replace adicsec=o10b/12  if o10c==8
replace adicsec=0 if o10a==2
replace adicsec=. if categosec_ci<3


*Ingreso laboral monetario total

egen ylmsec_ci=rsum(ysec adicsec)
replace ylmsec_ci=. if ysec==. & adicsec==. 
replace ylmsec_ci=0 if categosec_ci==4
replace ylmsec_ci=. if emp_ci~=1 | nempleos_ci~=2



******************
****ylnmsec_ci****
******************

*Remuneración en bienes y servicios

gen bsssec=.
replace bsssec=o11b*30  if o11c==2
replace bsssec=o11b*4.3 if o11c==3
replace bsssec=o11b*2   if o11c==4
replace bsssec=o11b     if o11c==5
replace bsssec=o11b/3   if o11c==6
replace bsssec=o11b/6   if o11c==7
replace bsssec=o11b/12  if o11c==8
replace bsssec=0 if o11a==2
replace bsssec=. if categosec_ci<3

*Remuneracion en ropa, vivienda u otros

gen ruvosec=.
replace ruvosec=o12b*30  if o12c==2
replace ruvosec=o12b*4.3 if o12c==3
replace ruvosec=o12b*2   if o12c==4
replace ruvosec=o12b     if o12c==5
replace ruvosec=o12b/3   if o12c==6
replace ruvosec=o12b/6   if o12c==7
replace ruvosec=o12b/12  if o12c==8
replace ruvosec=0 if o12a==2
replace ruvosec=. if categosec_ci<3

*Ingreso laboral no monetario para todos

egen ylnmsec_ci=rsum(bsssec ruvosec)
replace ylnmsec_ci=. if bsssec==. & ruvosec==.
replace ylnmsec_ci=0 if categosec_ci==4
replace ylnmsec_ci=. if emp_ci~=1 | nempleos_ci~=2


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. 


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

gen ynlm_ci=.


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.


****************
***remesas_ci***
****************

gen remesas_ci=.

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembro_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembro_ci==1


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembro_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembro_ci==1

******************
*** remesas_ch ***
******************

gen remesas_ch=.

***************
*** ynlm_ch ***
***************

gen ynlm_ch=otro_ing

****************
*** ynlnm_ch ***
****************

gen aut1=autocon1/12
gen aut2=autocon2/12
gen aut3=autocon3/12

egen ynlnm_ch=rsum(aut1 aut2 aut3)
replace ynlnm_ch=. if aut1==. & aut2==. & aut3==.


*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

egen autocons_ch=rsum(aut1 aut2 aut3)
replace autocons_ch=. if aut1==. & aut2==. & aut3==.


*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=rentaimp/12


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

/*En esta sección es sólo para los mayores a 3 años */

/*
gen byte aedu_ci=.

replace aedu_ci=0 if e2a==1 | e2a==2 

replace aedu_ci=1 if (e2a==3 & e2b==1) 
replace aedu_ci=2 if e2a==3 & e2b==2
replace aedu_ci=3 if e2a==3 & e2b==3
replace aedu_ci=4 if e2a==3 & e2b==4
replace aedu_ci=5 if e2a==3 & e2b==5
replace aedu_ci=6 if (e2a==3 & e2b==6) 

replace aedu_ci=7 if (e2a==4 | e2a==5) & e2b==1
replace aedu_ci=8 if (e2a==4 | e2a==5) & e2b==2
replace aedu_ci=9 if (e2a==4 | e2a==5) & e2b==3
replace aedu_ci=10 if (e2a==4 | e2a==5) & e2b==4
replace aedu_ci=11 if (e2a==4 | e2a==5) & e2b==5
replace aedu_ci=12 if (e2a==4 | e2a==5) & e2b>=6

replace aedu_ci=13 if (e2a>=6 & e2a<=7) & e2b==1
replace aedu_ci=14 if (e2a>=6 & e2a<=7) & e2b==2
replace aedu_ci=15 if (e2a>=6 & e2a<=7) & e2b==3
replace aedu_ci=16 if (e2a>=6 & e2a<=7) & e2b==4
replace aedu_ci=17 if (e2a>=6 & e2a<=7) & e2b==5
replace aedu_ci=18 if (e2a>=6 & e2a<=7) & e2b==6
replace aedu_ci=19 if (e2a>=6 & e2a<=7) & e2b==7
replace aedu_ci=20 if (e2a>=6 & e2a<=7) & e2b==8

replace aedu_ci=. if aedu_ci>=edad_ci

*/


gen byte aedu_ci=.
replace aedu_ci=0 if e2a==1 
replace aedu_ci=1 if e2a==2
replace aedu_ci=1+e2b	if e2a==3
replace aedu_ci=6+e2b	if e2a==4 | e2a==5
replace aedu_ci=11+e2b if e2a==6 | e2a==7 

label var aedu_ci "Anios de educación"

* Los que tienen "otro" se asume que tienen primaria completa;
replace aedu_ci=6 if e2a==8 

replace aedu_ci=. if aedu_ci>=edad_ci




**************
***eduno_ci***
**************

*gen byte eduno_ci=(e2a==1 | e2a==2) 
gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

*gen byte edupi_ci=(e2a==3 & e2b<5)
gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

*gen byte edupc_ci=(e2a==3 & e2b>=5)
gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>6 & aedu_ci<11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

*gen byte edusc_ci=(e2a>=4 & e2a<=5 & e2b>=5)
gen byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

*gen byte edus1i_ci=(e2a>=4 & e2a<=5 & e2b<=1)
gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

*gen byte edus1c_ci=(e2a>=4 & e2a<=5 & e2b==2)
gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

*gen byte edus2i_ci=(e2a>=4 & e2a<=5 & e2b>=3 & e2b<=4)
gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

*gen byte edus2c_ci=(e2a>=4 & e2a<=5 & e2b>=5)
gen byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

*gen byte eduui_ci=(e2a>=6 & e2a<=7 & e2b<5)
*gen byte eduui_ci=((aedu_ci>11 & aedu_ci<16) & (e2a==6 | e2a==7))
gen byte eduui_ci=(aedu_ci>11 & aedu_ci<16)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

*gen byte eduuc_ci=(e2a>=6 & e2a<=7 & e2b>=5) 
*gen byte eduuc_ci=((aedu_ci>=16) & (e2a==6 | e2a==7))
gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(e2a==2)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (e2a==7)
replace eduac_ci=0 if (e2a==6)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if e6==1
replace asiste_ci=0 if e6==2

label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis=.

/*NA*/


***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.

***************
***edupub_ci***
***************

gen edupub_ci=(e5p==1 | e5s==1 | e5su==1)
replace edupub_ci=. if asiste_ci~=1


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=.

gen aguadist_ch=.

gen aguamala_ch=.

gen aguamide_ch=.

gen luz_ch=.

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.

gen banoex_ch=.

gen des1_ch=.

gen des2_ch=.

gen piso_ch=.

gen pared_ch=.

gen techo_ch=.

gen resid_ch=.

gen dorm_ch=.

gen cuartos_ch=.

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.

gen vivi2_ch=.

gen viviprop_ch=.

gen viviitit_ch=.

gen vivialq_ch=.

gen vivialqimp_ch=.


save "`in'1997\Arm_data\PER1997EA_ALT_BID.dta", intercooled replace



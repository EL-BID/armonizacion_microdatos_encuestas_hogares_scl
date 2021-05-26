
/*
***revision agosto 15 de 2005
1. correccion de la variable firmapeq, se refiere a la pregunta m18 que 
no existe en la base original, existe una denominada m18r que tiene valore continuos 
de 0 a 131.3197, la variable de tamaño se llama n24.
CODIGO ANTERIOR:
 
gen firmapeq_ci=.
replace firmapeq_ci=1 if m18<=2
replace firmapeq_ci=0 if m18>=3 & m18<=8

2. NOTA: La variable segsoc_ci esta creada con respecto a la salud y no a las pensiones a pesar de existir
una variable exclusiva para esto, PENDIENTE DE CAMBIO

February 7-2007
the following line was included in order to count the people responding salary in hours
replace ypri=l08*horaspri_ci*4.3  if l08c==1
*/

****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA PERU - ENNIV 1994****************************
****************************************************************************

clear
capture log close
set memory 300m
set more off

local in="${surveysFolder}\ARM\PER\ENNIV\"
use "`in'1994\Orig_data\per94.dta"


***************
***factor_ch***
***************

gen factor_ch=peso
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


gen byte zona_c=1 if dominios==1 | dominios==2 | dominios==4 | dominios==6
replace zona_c=0 if dominios==3 | dominios==5 | dominios==7

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

gen anio_c=1994
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen monthy=a18 if first==1
recode monthy .=0
egen byte mes_c=sum(monthy), by (idh_ch)
drop first monthy

label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*No hay empleados domésticos ni pensionistas*/

gen relacion_ci=.
replace relacion_ci=1 if b01==1
replace relacion_ci=2 if b01==2
replace relacion_ci=3 if b01==3
replace relacion_ci=4 if b01>=4 & b01<=7
replace relacion_ci=5 if b01==0

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci

/*Hay unos 7 casos en donde la relación de parentesco que se reporta es de esposo/a
pero por la edad se nota que son hijos, además ya hay una esposa con una edad razonable en 
el hogar. Por eso los recodifico*/

replace relacion_ci=3 if idh_ch==893 & idp_ci==3
replace relacion_ci=3 if idh_ch==1142 & idp_ci==3
replace relacion_ci=3 if idh_ch==1300 & idp_ci==8
replace relacion_ci=3 if idh_ch==2684 & idp_ci==3
replace relacion_ci=3 if idh_ch==2906 & idp_ci==3
replace relacion_ci=3 if idh_ch==2934 & idp_ci==3
replace relacion_ci=3 if idh_ch==3303 & idp_ci==3



****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=peso
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=b02

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=b03

label variable edad_ci "Edad del individuo"


*****************
***estcivil_ci***
*****************

gen estcivil_ci=.
replace estcivil_ci=1 if b05==6
replace estcivil_ci=2 if b05==1 | b05==2
replace estcivil_ci=3 if b05==4 | b05==5
replace estcivil_ci=4 if b05==3

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
/* Esta sección es para los mayores a 6 años*/ 

************
***emp_ci***
************

gen byte emp_ci=0
replace emp_ci=1 if k06==1 | k10==8 | k11==1

/*Hay un grupo de 110 personas que dicen tener un empleo fijo aunque
no trabajaron en la última semana, a este grupo no se les hacen las preguntas
de empleados, por lo que no tenemos información sobre las características del
empleo e ingresos. Lo mismo sucede con un grupo de 8 trabajadores que están de
vacaciones o licencia. Voy a generar una dummy para identificarlos*/

gen licencia=(k10==8 | k11==1)


****************
***desemp1_ci***
****************

gen desemp1_ci=(emp_ci==0 & k07==1)
replace desemp1_ci=. if emp_ci==.

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(desemp1_ci==1 | (k07==2 & (k10==6 | k10==7 | k10==10)))
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

gen desalent_ci=(emp_ci==0 & k07==2 & k10==12)


*****************
***horaspri_ci***
*****************

gen horaspri_ci=l03*l04
replace horaspri_ci=0 if l03==0 & l04==0
replace horaspri_ci=. if emp_ci~=1 


*****************
***horastot_ci***
*****************

gen horassec=o03*o04
replace horassec=0 if o03==0 & o04==0

egen horastot_ci=rsum(horaspri_ci horassec)
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

***************
***subemp_ci***
***************

gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & horastot_ci<=30) & r01==1 
replace subemp_ci=. if emp_ci==.

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if (emp_ci==1 & horastot_ci<=30) & r01==2
replace tiempoparc_ci=. if emp_ci==. 


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if l09==1 & n24>1
replace categopri_ci=2 if l09==1 & n24==1 
replace categopri_ci=3 if l09==3 
replace categopri_ci=4 if l09==2 

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if o08==1 & q19>1
replace categosec_ci=2 if o08==1 & q19==1
replace categosec_ci=3 if o08==3 
replace categosec_ci=4 if o08==2 

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
replace contrato_ci=1 if m16==1 | m16==2
replace contrato_ci=0 if m16>2
replace contrato_ci=. if categopri_ci~=3


***************
***segsoc_ci***
***************
//*Esta asegurado al ISSP o tiene otro seguro medico*/

gen segsoc_ci=.
replace segsoc_ci=1 if n22==1 | n22==3 | n22==4
replace segsoc_ci=0 if n22==5 | n22==2
replace segsoc_ci=. if emp_ci~=1


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & n27==1


*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if n24<=2
replace firmapeq_ci=0 if n24>=3 & n24<=8

*n24 1=solo , 2 de 2 a 5 personas*

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
replace ocupa_ci=1 if l01a>=11 & l01a<=196
replace ocupa_ci=2 if (l01a>=211 & l01a<=229) & (l01a==511 | l01a==513 | l01a==521 | l01a==522)
replace ocupa_ci=3 if (l01a>=311 & l01a<=362) | l01a==512
replace ocupa_ci=4 if l01a>=411 & l01a<=499
replace ocupa_ci=5 if (l01a>=363 & l01a<=399) | (l01a>=531 & l01a<=572) 
replace ocupa_ci=5 if (l01a>=581 & l01a<=583) | (l01a>=587 & l01a<=596)
replace ocupa_ci=6 if l01a>=611 & l01a<=699
replace ocupa_ci=7 if l01a>=701 & l01a<=998
replace ocupa_ci=8 if l01a==194 | l01a==195 |l01a==584 |l01a==585 | l01a==586
replace ocupa_ci=9 if l01a==999
replace ocupa_ci=. if emp_ci~=1

*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (l02>=100 & l02<=500) & emp_ci==1
replace rama_ci=2 if (l02>=1000 & l02<=1429) & emp_ci==1
replace rama_ci=3 if (l02>=1500 & l02<=3720) & emp_ci==1
replace rama_ci=4 if (l02>=4000 & l02<=4100) & emp_ci==1
replace rama_ci=5 if (l02>=4500 & l02<=4550) & emp_ci==1
replace rama_ci=6 if (l02>=5000 & l02<=5520) & emp_ci==1 
replace rama_ci=7 if (l02>=6000 & l02<=6420) & emp_ci==1
replace rama_ci=8 if (l02>=6500 & l02<=7499) & emp_ci==1
replace rama_ci=9 if (l02>=7500 & l02<=9520) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=k08/4.3 if desemp2_ci==1



*******************
***antiguedad_ci***
*******************
/*En años*/

gen ant1=l07a
gen ant2=l07b/12
gen ant3=l07c/(4.3*12)

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
replace ypri=l08*horaspri_ci*4.3 if l08c==1
replace ypri=l08*30  if l08c==2
replace ypri=l08*4.3 if l08c==3
replace ypri=l08*2   if l08c==4
replace ypri=l08     if l08c==5
replace ypri=l08/3   if l08c==6
replace ypri=l08/6   if l08c==7
replace ypri=l08/12  if l08c==8
replace ypri=0 if l08==0 & l08c==0

*Salario adicional
gen adic=.
replace adic=m10b*30 if m10c==2
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
replace ysec=o07a*30  if o07b==2
replace ysec=o07a*4.3 if o07b==3
replace ysec=o07a*2   if o07b==4
replace ysec=o07a     if o07b==5
replace ysec=o07a/3   if o07b==6
replace ysec=o07a/6   if o07b==7
replace ysec=o07a/12  if o07b==8
replace ysec=0 if o07a==0 & o07b==0


*Salario adicional
gen adicsec=.
replace adicsec=p09b*30  if p09c==2
replace adicsec=p09b*4.3 if p09c==3
replace adicsec=p09b*2   if p09c==4
replace adicsec=p09b     if p09c==5
replace adicsec=p09b/3   if p09c==6
replace adicsec=p09b/6   if p09c==7
replace adicsec=p09b/12  if p09c==8
replace adicsec=0 if p09a==2
replace adicsec=. if categopri_ci<3


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
replace bsssec=p10b*30  if p10c==2
replace bsssec=p10b*4.3 if p10c==3
replace bsssec=p10b*2   if p10c==4
replace bsssec=p10b     if p10c==5
replace bsssec=p10b/3   if p10c==6
replace bsssec=p10b/6   if p10c==7
replace bsssec=p10b/12  if p10c==8
replace bsssec=0 if p10a==2
replace bsssec=. if categosec_ci<3

*Remuneracion en ropa, vivienda u otros

gen ruvosec=.
replace ruvosec=p11b*30  if p11c==2
replace ruvosec=p11b*4.3 if p11c==3
replace ruvosec=p11b*2   if p11c==4
replace ruvosec=p11b     if p11c==5
replace ruvosec=p11b/3   if p11c==6
replace ruvosec=p11b/6   if p11c==7
replace ruvosec=p11b/12  if p11c==8
replace ruvosec=0 if p11a==2
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

gen ynlnm_ch=autocon1/12
replace ynlnm_ch=. if autocon1==. 


*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=autocon1/12
replace autocons_ch=. if autocon1==.


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

/*En esta sección es sólo para mayores a los 6 años de edad.
Para ser consistentes con las encuestas posteriores que encuestan 
a los mayores a 3 años, asignamos años de educación cero a los que
están entre 3 y 6 años*/

/*
gen byte aedu_ci=.

replace aedu_ci=0 if edad_ci>=3 & edad<=5
replace aedu_ci=0 if e05a==1 | (e05a==2 & e05b==0)

replace aedu_ci=1 if e05a==2 & e05b==1
replace aedu_ci=2 if e05a==2 & e05b==2
replace aedu_ci=3 if e05a==2 & e05b==3
replace aedu_ci=4 if e05a==2 & e05b==4
replace aedu_ci=5 if e05a==2 & e05b==5
replace aedu_ci=6 if e05a==2 & e05b==6

replace aedu_ci=7 if (e05a==3 | e05a==4) & e05b==1
replace aedu_ci=8 if (e05a==3 | e05a==4) & e05b==2
replace aedu_ci=9 if (e05a==3 | e05a==4) & e05b==3
replace aedu_ci=10 if (e05a==3 | e05a==4) & e05b==4
replace aedu_ci=11 if (e05a==3 | e05a==4) & e05b==5
replace aedu_ci=12 if (e05a==3 | e05a==4) & e05b>=6

replace aedu_ci=13 if (e05a>=5 & e05a<=6) & e05b==1
replace aedu_ci=14 if (e05a>=5 & e05a<=6) & e05b==2
replace aedu_ci=15 if (e05a>=5 & e05a<=6) & e05b==3
replace aedu_ci=16 if (e05a>=5 & e05a<=6) & e05b==4
replace aedu_ci=17 if (e05a>=5 & e05a<=6) & e05b==5
replace aedu_ci=18 if (e05a>=5 & e05a<=6) & e05b==6
replace aedu_ci=19 if (e05a>=5 & e05a<=6) & e05b==7
replace aedu_ci=20 if (e05a>=5 & e05a<=6) & e05b==8

replace aedu_ci=. if aedu_ci>=edad_ci
*/

gen byte aedu_ci=.
replace aedu_ci=0 if e05a==0 | e05a==1 
replace aedu_ci=1+e05b	if e05a==2
replace aedu_ci=6+e05b	if e05a==3 | e05a==4
replace aedu_ci=11+e05b if e05a==5 | e05a==6 

label var aedu_ci "Anios de educación"

* Los que tienen "otro" se asume que tienen primaria completa;
replace aedu_ci=6 if e05a==7 

replace aedu_ci=. if aedu_ci>=edad_ci


**************
***eduno_ci***
**************

*gen byte eduno_ci=(e05a==1) 
gen byte eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

*gen byte edupi_ci=(e05a==2 & e05b<5)
gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

*gen byte edupc_ci=(e05a==2 & e05b>=5)
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

*gen byte edusc_ci=(e05a>=3 & e05a<=4 & e05b>=5)
gen byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

*gen byte edus1i_ci=(e05a>=3 & e05a<=4 & e05b<=1)
gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

*gen byte edus1c_ci=(e05a>=3 & e05a<=4 & e05b==2)
gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

*gen byte edus2i_ci=(e05a>=3 & e05a<=4 & e05b>=3 & e05b<=4)
gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

*gen byte edus2c_ci=(e05a>=3 & e05a<=4 & e05b>=5)
gen byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

*gen byte eduui_ci=(e05a>=5 & e05a<=6 & e05b<5)
*gen byte eduui_ci=((aedu_ci>11 & aedu_ci<16) & (e05a==5 | e05a==6))
gen byte eduui_ci=(aedu_ci>11 & aedu_ci<16) 
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

*gen byte eduuc_ci=(e05a>=5 & e05a<=6 & e05b>=5) 
*gen byte eduuc_ci=((aedu_ci>=16) & (e05a==5 | e05a==6))
gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

/*Esta variable no es comparable con los años que siguen*/

gen byte edupre_ci=(g28>=1 & g28<=4)
replace edupre_ci=. if edad>6

label variable edupre_ci "Educacion preescolar"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (e05a==6)
replace eduac_ci=0 if (e05a==5)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if e09==1
replace asiste_ci=0 if e09==2

label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis=.

/*NA*/


***************
***repite_ci***
***************
/*¿Repitió algún año de estudio?*/

gen repite_ci=(f21>0)
replace repite_ci=. if f21==.

******************
***repiteult_ci***
******************

gen repiteult_ci=.
/*NA*/

***************
***edupub_ci***
***************

gen edupub_ci=(e07==1)
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


save "`in'1994\Arm_data\PER1994EA_ALT_BID.dta", intercooled replace



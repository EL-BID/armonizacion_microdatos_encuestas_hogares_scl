/* ***revision agosto 15 de 2005 NOTA: la variable segsoc_ci esta definida como seguro medico y 
no como pensiones pendiente de revision y cambio. deberia ser con la variable pensiones 
para ser consistente con el resto de las encuestas en los otros paises, para1991 NO existe
asi que no se deberia generar
*/


****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA PERU - ENNIV 1991****************************
****************************************************************************

clear
capture log close
set memory 300m
set more off

local in="X:\ARM\PER\ENNIV\"
use "`in'1991\Orig_data\per91.dta"


***************
***factor_ch***
***************

gen factor_ch=FACTOR91
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************

sort HOG
gen first=0
by HOG: replace first=1 if _n==1
gen vectoru=1
gen double idy=sum(vectoru) if first==1
recode idy .=0
egen double idh_ch=sum(idy), by (HOG)
drop vectoru idy

label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=SEC
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

/*El ámbito geográfico de la investigación no abarcó a todo el
territorio nacional; no se incluyó la costa rural, la costa centro
urbana (excepto Lima Metropolitana), la selva y los departamentos
de Ayacucho, Apurímac y Huancavelica. Las razones de no inclui r
estas áreas fueron varias; en primer lugar, los recursos
financieros fueron limitados lo que determinó la priorización de
áreas de investigación de mayor interés inmediato tales como: el
área urbana porque concentra el mayor porcentaje de la població n
nacional, y la sierra rural porque es el área históricamente más
deprimida del país. Los departamentos mencionados, localizados en
la sierra central, no se consideraron por estar catalogados como
peligrosos por la actividad terrorista; la selva no se incluyó por
cuestión de costos; la costa rural porque en su lugar se priorizó
la sierra rural; y el resto de la costa centro urbana por estar muy
cerca de Lima Metropolitana.*/


gen byte zona_c=1 if DOMINIOS==1 | DOMINIOS==2 | DOMINIOS==4 
replace zona_c=0 if DOMINIOS==5 

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

gen anio_c=1991
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=A0118

label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*No hay empleados domésticos ni pensionistas*/

gen relacion_ci=.
replace relacion_ci=1 if A0302==1
replace relacion_ci=2 if A0302==2
replace relacion_ci=3 if A0302==3
replace relacion_ci=4 if A0302>=4 & A0302<=7
replace relacion_ci=5 if A0302==0 | A0302==9
replace relacion_ci=6 if A0302==8 

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

gen factor_ci=FACTOR91
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=A0303

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=A0305A

label variable edad_ci "Edad del individuo"


*****************
***estcivil_ci***
*****************

gen estcivil_ci=.
replace estcivil_ci=1 if A0306==6
replace estcivil_ci=2 if A0306==1 | A0306==2
replace estcivil_ci=3 if A0306==4 | A0306==5
replace estcivil_ci=4 if A0306==3

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
replace emp_ci=1 if A1008==1 | A1013==8 

/*Se incluyen a las personas que estan de vacaciones o licencia.
En esta encuesta no se pregunta si la persona a pesar de no haber trabajado
tiene un empleo fijo al cual volver. Las encuestas que siguen tienen esa pregunta 
y estas personas estan dentro de los empleados. Igualmente como a estos individuos 
no se les preguntaba nada acerca de su empleo o ingresos y como son pocos no voy a crear
dos variables de empleo para distinguir 1991 del resto.
Voy a identificar a los que estan de vacaciones con una dummy*/

gen licencia=(A1013==8)


****************
***desemp1_ci***
****************

gen desemp1_ci=(emp_ci==0 & A1009==1)
replace desemp1_ci=. if emp_ci==.

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(desemp1_ci==1 | (A1009==2 & (A1013==6 | A1013==7 | A1013==10)))
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

gen desalent_ci=(emp_ci==0 & A1009==2 & A1013==12)


*****************
***horaspri_ci***
*****************

gen horaspri_ci=A1103*A1104
replace horaspri_ci=0 if A1103==0 & A1104==0
replace horaspri_ci=. if emp_ci~=1 


*****************
***horastot_ci***
*****************

gen horassec=A1303*A1304
replace horassec=0 if A1303==0 & A1304==0

egen horastot_ci=rsum(horaspri_ci horassec)
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

***************
***subemp_ci***
***************

gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & horastot_ci<=30) & A1501=="1" 
replace subemp_ci=. if emp_ci==. 

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if (emp_ci==1 & horastot_ci<=30) & A1501=="2"
replace tiempoparc_ci=. if emp_ci==. 


******************
***categopri_ci***
******************

/*Solamente se pueden identificar a los empleados dependientes*/

gen categopri_ci=.
replace categopri_ci=3 if A1111==2

label define categopri_ci 3 "Empleado"
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

/*Solamente se pueden identificar a los empleados dependientes*/

gen categosec_ci=.
replace categosec_ci=3 if A1309==2

label define categosec_ci 3"Empleado"
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
***contrato_ci***
*****************

gen contrato_ci=.
/*NA*/

***************
***segsoc_ci***
***************
/*Esta asegurado al ISSP o tiene otro seguro medico*/

gen segsoc_ci=.
replace segsoc_ci=1 if A1221=="1" | A1221=="3"
replace segsoc_ci=0 if A1221=="4" | A1221=="2"
replace segsoc_ci=. if emp_ci~=1
replace segsoc_ci=. if emp_ci~=1

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & A1222=="1"


*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
/*NA*/

*****************
***spublico_ci***
*****************
/*Sólo para los empleados dependientes*/

gen spublico_ci=.
replace spublico_ci=1 if (A1216==1 | A1216==3)
replace spublico_ci=0 if (A1216==2 | A1216==4 | A1216==5)
replace spublico_ci=. if categopri~=3


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (A1101>=1 & A1101<=197) & emp_ci==1
replace ocupa_ci=2 if (A1101>=211 & A1101<=231) & emp_ci==1
replace ocupa_ci=3 if (A1101>=311 & A1101<=399) & emp_ci==1
replace ocupa_ci=4 if (A1101>=402 & A1101<=499) & emp_ci==1
replace ocupa_ci=5 if (A1101>=511 & A1101<=596) & emp_ci==1
replace ocupa_ci=6 if (A1101>=611 & A1101<=663) & emp_ci==1
replace ocupa_ci=7 if (A1101>=701 & A1101<=998) & emp_ci==1



*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (A1102>=1000 & A1102<=1999) & emp_ci==1
replace rama_ci=2 if (A1102>=2000 & A1102<=2999) & emp_ci==1
replace rama_ci=3 if (A1102>=3000 & A1102<=3999) & emp_ci==1
replace rama_ci=4 if (A1102>=4000 & A1102<=4999) & emp_ci==1
replace rama_ci=5 if (A1102>=5000 & A1102<=5999) & emp_ci==1
replace rama_ci=6 if (A1102>=6000 & A1102<=6999) & emp_ci==1 
replace rama_ci=7 if (A1102>=7000 & A1102<=7999) & emp_ci==1
replace rama_ci=8 if (A1102>=8000 & A1102<=8999) & emp_ci==1
replace rama_ci=9 if (A1102>=9000 & A1102<=9999) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=A1011/4.3 if desemp2_ci==1



*******************
***antiguedad_ci***
*******************
/*En años*/

gen ant1=A1108A
gen ant2=A1108B/12
gen ant3=A1108C/(52)

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
replace ypri=A1110B*30  if A1110E==2
replace ypri=A1110B*4.3 if A1110E==3
replace ypri=A1110B*2   if A1110E==4
replace ypri=A1110B     if A1110E==5
replace ypri=A1110B/3   if A1110E==6
replace ypri=A1110B/6   if A1110E==7
replace ypri=A1110B/12  if A1110E==8
replace ypri=0 if A1110B==0 & A1110E==0
replace ypri=0 if A1109==2

*Salario adicional
gen adic=.
replace adic=A1213B*30  if A1213D==2
replace adic=A1213B*4.3 if A1213D==3
replace adic=A1213B*2   if A1213D==4
replace adic=A1213B     if A1213D==5
replace adic=A1213B/3   if A1213D==6
replace adic=A1213B/6   if A1213D==7
replace adic=A1213B/12  if A1213D==8
replace adic=0 if A1213A=="2"


*Ingreso laboral monetario total

egen ylmpri_ci=rsum(ypri adic)
replace ylmpri_ci=. if ypri==. & adic==. 
replace ylmpri_ci=0 if licencia==1
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
replace bsss=A1214B*30  if A1214D==2
replace bsss=A1214B*4.3 if A1214D==3
replace bsss=A1214B*2   if A1214D==4
replace bsss=A1214B     if A1214D==5
replace bsss=A1214B/3   if A1214D==6
replace bsss=A1214B/6   if A1214D==7
replace bsss=A1214B/12  if A1214D==8
replace bsss=0 if A1214A=="2"

*Remuneracion en ropa, vivienda u otro

gen ruvo=.
replace ruvo=A1215B*30  if A1215D==2
replace ruvo=A1215B*4.3 if A1215D==3
replace ruvo=A1215B*2   if A1215D==4
replace ruvo=A1215B     if A1215D==5
replace ruvo=A1215B/3   if A1215D==6
replace ruvo=A1215B/6   if A1215D==7
replace ruvo=A1215B/12  if A1215D==8
replace ruvo=0 if A1215A=="2"

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
replace ysec=A1308B*30  if A1308D==2
replace ysec=A1308B*4.3 if A1308D==3
replace ysec=A1308B*2   if A1308D==4
replace ysec=A1308B     if A1308D==5
replace ysec=A1308B/3   if A1308D==6
replace ysec=A1308B/6   if A1308D==7
replace ysec=A1308B/12  if A1308D==8
replace ysec=0 if A1308B==0 & A1308D==0
replace ysec=0 if A1307==2

*Salario adicional
gen adicsec=.
replace adicsec=A1412B*30  if A1412D==2
replace adicsec=A1412B*4.3 if A1412D==3
replace adicsec=A1412B*2   if A1412D==4
replace adicsec=A1412B     if A1412D==5
replace adicsec=A1412B/3   if A1412D==6
replace adicsec=A1412B/6   if A1412D==7
replace adicsec=A1412B/12  if A1412D==8
replace adicsec=0 if A1412A=="2"


*Ingreso laboral monetario total

egen ylmsec_ci=rsum(ysec adicsec)
replace ylmsec_ci=. if ysec==. & adicsec==. 
replace ylmsec_ci=. if emp_ci~=1 | nempleos_ci~=2
 

******************
****ylnmsec_ci****
******************

*Remuneración en bienes y servicios

gen bsssec=.
replace bsssec=A1413B*30  if A1413D==2
replace bsssec=A1413B*4.3 if A1413D==3
replace bsssec=A1413B*2   if A1413D==4
replace bsssec=A1413B     if A1413D==5
replace bsssec=A1413B/3   if A1413D==6
replace bsssec=A1413B/6   if A1413D==7
replace bsssec=A1413B/12  if A1413D==8
replace bsssec=0 if A1413A=="2"

*Remuneracion en ropa, vivienda u otros

gen ruvosec=.
replace ruvosec=A1414B*30  if A1414D==2
replace ruvosec=A1414B*4.3 if A1414D==3
replace ruvosec=A1414B*2   if A1414D==4
replace ruvosec=A1414B     if A1414D==5
replace ruvosec=A1414B/3   if A1414D==6
replace ruvosec=A1414B/6   if A1414D==7
replace ruvosec=A1414B/12  if A1414D==8
replace ruvosec=0 if A1414A=="2"

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

gen ynlm_ch=HHYOTHSR

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=AUTOCONS/12
replace ynlnm_ch=. if AUTOCONS==. 


*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=AUTOCONS/12
replace autocons_ch=. if AUTOCONS==.


*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=RENTAIMP/12


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
replace aedu_ci=0 if A0605A==0 | A0605A==1 

replace aedu_ci=1 if A0605A==2 & A0605B==1
replace aedu_ci=2 if A0605A==2 & A0605B==2
replace aedu_ci=3 if A0605A==2 & A0605B==3
replace aedu_ci=4 if A0605A==2 & A0605B==4
replace aedu_ci=5 if A0605A==2 & A0605B==5
replace aedu_ci=6 if A0605A==2 & A0605B==6

replace aedu_ci=7 if (A0605A==3 | A0605A==4) & A0605B==1
replace aedu_ci=8 if (A0605A==3 | A0605A==4) & A0605B==2
replace aedu_ci=9 if (A0605A==3 | A0605A==4) & A0605B==3
replace aedu_ci=10 if (A0605A==3 | A0605A==4) & A0605B==4
replace aedu_ci=11 if (A0605A==3 | A0605A==4) & A0605B==5
replace aedu_ci=12 if (A0605A==3 | A0605A==4) & A0605B>=6

replace aedu_ci=13 if (A0605A==5 | A0605A==6) & A0605B==1
replace aedu_ci=14 if (A0605A==5 | A0605A==6) & A0605B==2
replace aedu_ci=15 if (A0605A==5 | A0605A==6) & A0605B==3
replace aedu_ci=16 if (A0605A==5 | A0605A==6) & A0605B==4
replace aedu_ci=17 if (A0605A==5 | A0605A==6) & A0605B==5
replace aedu_ci=18 if (A0605A==5 | A0605A==6) & A0605B==6
replace aedu_ci=19 if (A0605A==5 | A0605A==6 & A0605B==7
replace aedu_ci=20 if (A0605A==5 | A0605A==6) & A0605B==8
replace aedu_ci=21 if (A0605A==5 | A0605A==6) & A0605B==9


replace aedu_ci=. if aedu_ci>=edad_ci
*/

gen byte aedu_ci=.
replace aedu_ci=0 if A0605A==0 | A0605A==1
replace aedu_ci=1+A0605B if A0605A==2
replace aedu_ci=6+A0605B if A0605A==3 | A0605A==4
replace aedu_ci=11+A0605B if A0605A==5 | A0605A==6 

label var aedu_ci "Anios de educación"


* Los que tienen "otro" se asume que tienen primaria completa;
replace aedu_ci=6 if A0605A==7 

replace aedu_ci=. if aedu_ci>=edad_ci
*/

**************
***eduno_ci***
**************

*gen byte eduno_ci=(A0605A==0 | A0605A==1) 
gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

*gen byte edupi_ci=(A0605A==2 & A0605B<5)
gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

*gen byte edupc_ci=(A0605A==2 & A0605B>=5)
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

*gen byte edusc_ci=(A0605A>=3 & A0605A<=4 & A0605B>=5)
gen byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

*gen byte edus1i_ci=(A0605A>=3 & A0605A<=4 & A0605B<=1)
gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

*gen byte edus1c_ci=(A0605A>=3 & A0605A<=4 & A0605B==2)
gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

*gen byte edus2i_ci=(A0605A>=3 & A0605A<=4 & A0605B>=3 & A0605B<=4)
gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

*gen byte edus2c_ci=(A0605A>=3 & A0605A<=4 & A0605B>=5)
gen byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

*gen byte eduui_ci=(A0605A>=5 & A0605A<=6 & A0605B<5)
*gen byte eduui_ci=((aedu_ci>11 & aedu_ci<16) & (A0605A==5 | A0605A==6))
gen byte eduui_ci=(aedu_ci>11 & aedu_ci<16)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

*gen byte eduuc_ci=(A0605A>=5 & A0605A<=6 & A0605B>=5) 
*gen byte eduuc_ci=((aedu_ci>=16) & (A0605A==5 | A0605A==6))
gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

/*Esta variable no es comparable con los años que siguen*/

gen byte edupre_ci=(A0727>="1" & A0727<="4")
replace edupre_ci=. if edad>6

label variable edupre_ci "Educacion preescolar"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (A0605A==6)
replace eduac_ci=0 if (A0605A==5)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if A0609=="1"
replace asiste_ci=0 if A0609=="2"

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

gen edupub_ci=.

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


save "`in'1991\Arm_data\PER1991EA_ALT_BID.dta", intercooled replace



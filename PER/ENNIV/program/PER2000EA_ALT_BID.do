****************************************************************************
**********PROGRAMA PARA CREAR LAS NUEVAS ENCUESTAS HOMOGENEIZADAS***********
**************************PARA PERU - ENNIV 2000****************************
****************************************************************************

clear
capture log close
set memory 300m
set more off

local in="X:\ARM\PER\ENNIV\"
use "`in'2000\Orig_data\per00.dta"


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

gen idp_ci=idper
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=1 if dominio==1 | dominio==2 | dominio==4 | dominio==6
replace zona_c=0 if dominio==3 | dominio==5 | dominio==7

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

gen anio_c=2000
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen monthy=mesenc if first==1
recode monthy .=0
egen byte mes_c=sum(monthy), by (idh_ch)
drop first monthy

label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*No hay empleados domésticos ni pensionistas*/

gen relacion_ci=.
replace relacion_ci=1 if s1p1==1
replace relacion_ci=2 if s1p1==2
replace relacion_ci=3 if s1p1==3
replace relacion_ci=4 if s1p1>=4 & s1p1<=7
replace relacion_ci=5 if s1p1==10 | s1p1==9
replace relacion_ci=6 if s1p1==8

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

gen factor_ci=expan
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=s1p2a

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=s1p3

label variable edad_ci "Edad del individuo"


*****************
***estcivil_ci***
*****************

gen estcivil_ci=.
replace estcivil_ci=1 if s1p5==6
replace estcivil_ci=2 if s1p5==1 | s1p5==2
replace estcivil_ci=3 if s1p5==4 | s1p5==5
replace estcivil_ci=4 if s1p5==3

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
replace emp_ci=1 if s5a6==1 | s5a10==8 |s5a11==1

/*Hay un grupo de 110 personas que dicen tener un empleo fijo aunque
no trabajaron en la última semana, a este grupo no se les hacen las preguntas
de empleados, por lo que no tenemos información sobre las características del
empleo e ingresos. Lo mismo sucede con un grupo de 8 trabajadores que están de
vacaciones o licencia. Voy a generar una dummy para identificarlos*/

gen licencia=(s5a10==8 | s5a11==1)


****************
***desemp1_ci***
****************

gen desemp1_ci=(emp_ci==0 & s5a7==1)
replace desemp1_ci=. if emp_ci==.

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(desemp1_ci==1 | (s5a7==2 & (s5a10==6 | s5a10==7 | s5a10==10)))
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

gen desalent_ci=(emp_ci==0 & s5a7==2 & s5a10==12)


*****************
***horaspri_ci***
*****************

gen horaspri_ci=s5b3*s5b4
replace horaspri_ci=0 if s5b3==0 & s5b4==0
replace horaspri_ci=. if emp_ci~=1 


*****************
***horastot_ci***
*****************

gen horassec=s5c3*s5c4
replace horassec=0 if s5c3==0 & s5c4==0

/*Hay un caso en donde es evidente que hay un error de codificación
una persona reporta en el trabajo secundario trabajar 101 horas por día,
voy a asumir que son 10 horas*/

replace horassec=s5c3*10 if idp_ci==1 & idh_ch==3671

egen horastot_ci=rsum(horaspri_ci horassec)
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

***************
***subemp_ci***
***************

gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & horastot_ci<=30) & s5d1==1 
replace subemp_ci=. if emp_ci==.

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if (emp_ci==1 & horastot_ci<=30) & s5d1==2
replace tiempoparc_ci=. if emp_ci==.


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if s5b8==1 & s5b18>1
replace categopri_ci=2 if s5b8==1 & s5b18==1 
replace categopri_ci=3 if s5b8==3 
replace categopri_ci=4 if s5b8==2 

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if s5c8==1 & s5c18>1
replace categosec_ci=2 if s5c8==1 & s5c18==1
replace categosec_ci=3 if s5c8==3 
replace categosec_ci=4 if s5c8==2 

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
replace contrato_ci=1 if s5b14==1 | s5b14==2
replace contrato_ci=0 if s5b14>2
replace contrato_ci=. if categopri_ci~=3


***************
***segsoc_ci***
***************
/*Esta asegurado a ESSALUD o tiene otro seguro medico*/

gen segsoc_ci=.
replace segsoc_ci=1 if s5b17==1 | s5b17==3 | s5b17==4
replace segsoc_ci=0 if s5b17==5 | s5b17==2
replace segsoc_ci=. if emp_ci~=1


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s5b19==1


*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if s5b18<=2
replace firmapeq_ci=0 if s5b18>=3 & s5b18<=8


*****************
***spublico_ci***
*****************
/*Sólo para los empleados dependientes*/

gen spublico_ci=.
replace spublico_ci=1 if (s5b13==1 | s5b13==3)
replace spublico_ci=0 if (s5b13==2 | s5b13==4 | s5b13==5)
replace spublico_ci=. if categopri~=3


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (s5b1>=211 & s5b1<=395) & emp_ci==1
replace ocupa_ci=2 if (s5b1>=111 & s5b1<=148) & emp_ci==1
replace ocupa_ci=3 if (s5b1>=411 & s5b1<=462) & emp_ci==1
replace ocupa_ci=4 if (s5b1>=570 & s5b1<=583) | (s5b1>=911 & s5b1<=931) & emp_ci==1
replace ocupa_ci=5 if (s5b1>=511 & s5b1<=565) | (s5b1>=941 & s5b1<=961) & emp_ci==1
replace ocupa_ci=6 if (s5b1>=611 & s5b1<=641) | (s5b1>=970 & s5b1<=973) & emp_ci==1
replace ocupa_ci=7 if (s5b1>=711 & s5b1<=886) | (s5b1>=981 & s5b1<=987) & emp_ci==1
replace ocupa_ci=8 if (s5b1>=11 & s5b1<=23) & emp_ci==1



*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (s5b2>=100 & s5b2<=500) & emp_ci==1
replace rama_ci=2 if (s5b2>=1000 & s5b2<=1429) & emp_ci==1
replace rama_ci=3 if (s5b2>=1500 & s5b2<=3720) & emp_ci==1
replace rama_ci=4 if (s5b2>=4000 & s5b2<=4100) & emp_ci==1
replace rama_ci=5 if (s5b2>=4500 & s5b2<=4550) & emp_ci==1
replace rama_ci=6 if (s5b2>=5000 & s5b2<=5520) & emp_ci==1 
replace rama_ci=7 if (s5b2>=6000 & s5b2<=6420) & emp_ci==1
replace rama_ci=8 if (s5b2>=6500 & s5b2<=7499) & emp_ci==1
replace rama_ci=9 if (s5b2>=7500 & s5b2<=9503) & emp_ci==1


****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=s5a8/4.3 if desemp2_ci==1


*******************
***antiguedad_ci***
*******************
/*En años*/

gen ant1=s5b6a
gen ant2=s5b6b/12
gen ant3=s5b6c/(4.3*12)

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
replace ypri=s5b7a*30  if s5b7b==2
replace ypri=s5b7a*4.3 if s5b7b==3
replace ypri=s5b7a*2   if s5b7b==4
replace ypri=s5b7a     if s5b7b==5
replace ypri=s5b7a/3   if s5b7b==6
replace ypri=s5b7a/6   if s5b7b==7
replace ypri=s5b7a/12  if s5b7b==8
replace ypri=0 if s5b7a==0 & s5b7b==0

*Salario adicional
gen adic=.
replace adic=s5b10b*30  if s5b10c==2
replace adic=s5b10b*4.3 if s5b10c==3
replace adic=s5b10b*2   if s5b10c==4
replace adic=s5b10b     if s5b10c==5
replace adic=s5b10b/3   if s5b10c==6
replace adic=s5b10b/6   if s5b10c==7
replace adic=s5b10b/12  if s5b10c==8
replace adic=0 if s5b10a==2
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
replace bsss=s5b11b*30  if s5b11c==2
replace bsss=s5b11b*4.3 if s5b11c==3
replace bsss=s5b11b*2   if s5b11c==4
replace bsss=s5b11b     if s5b11c==5
replace bsss=s5b11b/3   if s5b11c==6
replace bsss=s5b11b/6   if s5b11c==7
replace bsss=s5b11b/12  if s5b11c==8
replace bsss=0 if s5b11a==2
replace bsss=. if categopri_ci<3

*Remuneracion en ropa, vivienda u otros

gen ruvo=.
replace ruvo=s5b12b*30  if s5b12c==2
replace ruvo=s5b12b*4.3 if s5b12c==3
replace ruvo=s5b12b*2   if s5b12c==4
replace ruvo=s5b12b     if s5b12c==5
replace ruvo=s5b12b/3   if s5b12c==6
replace ruvo=s5b12b/6   if s5b12c==7
replace ruvo=s5b12b/12  if s5b12c==8
replace ruvo=0 if s5b12a==2
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
replace ysec=s5c7a*30  if s5c7b==2
replace ysec=s5c7a*4.3 if s5c7b==3
replace ysec=s5c7a*2   if s5c7b==4
replace ysec=s5c7a     if s5c7b==5
replace ysec=s5c7a/3   if s5c7b==6
replace ysec=s5c7a/6   if s5c7b==7
replace ysec=s5c7a/12  if s5c7b==8
replace ysec=0 if s5c7a==0 & s5c7b==0

*Salario adicional
gen adicsec=.
replace adicsec=s5c10b*30  if s5c10c==2
replace adicsec=s5c10b*4.3 if s5c10c==3
replace adicsec=s5c10b*2   if s5c10c==4
replace adicsec=s5c10b     if s5c10c==5
replace adicsec=s5c10b/3   if s5c10c==6
replace adicsec=s5c10b/6   if s5c10c==7
replace adicsec=s5c10b/12  if s5c10c==8
replace adicsec=0 if s5c10a==2
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
replace bsssec=s5c11b*30  if s5c11c==2
replace bsssec=s5c11b*4.3 if s5c11c==3
replace bsssec=s5c11b*2   if s5c11c==4
replace bsssec=s5c11b     if s5c11c==5
replace bsssec=s5c11b/3   if s5c11c==6
replace bsssec=s5c11b/6   if s5c11c==7
replace bsssec=s5c11b/12  if s5c11c==8
replace bsssec=0 if s5c11a==2
replace bsssec=. if categosec_ci<3

*Remuneracion en ropa, vivienda u otros

gen ruvosec=.
replace ruvosec=s5c12b*30  if s5c12c==2
replace ruvosec=s5c12b*4.3 if s5c12c==3
replace ruvosec=s5c12b*2   if s5c12c==4
replace ruvosec=s5c12b     if s5c12c==5
replace ruvosec=s5c12b/3   if s5c12c==6
replace ruvosec=s5c12b/6   if s5c12c==7
replace ruvosec=s5c12b/12  if s5c12c==8
replace ruvosec=0 if s5c12a==2
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

/* Esta sección es para los mayores a 6 años.*/ 

/*
gen byte aedu_ci=.

replace aedu_ci=0 if s3p2a==1 | s3p2a==2 

replace aedu_ci=0 if s3p2a==4 & s3p2b==0
replace aedu_ci=1 if (s3p2a==4 & s3p2b==1) | s3p2a==3
replace aedu_ci=2 if s3p2a==4 & s3p2b==2
replace aedu_ci=3 if s3p2a==4 & s3p2b==3
replace aedu_ci=4 if s3p2a==4 & s3p2b==4
replace aedu_ci=5 if s3p2a==4 & s3p2b==5
replace aedu_ci=6 if (s3p2a==4 & s3p2b==6) | (s3p2a==5 & s3p2b==0)

replace aedu_ci=7 if (s3p2a==5 | s3p2a==6) & s3p2b==1
replace aedu_ci=8 if (s3p2a==5 | s3p2a==6) & s3p2b==2
replace aedu_ci=9 if (s3p2a==5 | s3p2a==6) & s3p2b==3
replace aedu_ci=10 if (s3p2a==5 | s3p2a==6) & s3p2b==4
replace aedu_ci=11 if (s3p2a==5 | s3p2a==6) & s3p2b==5
replace aedu_ci=12 if (s3p2a==5 | s3p2a==6) & s3p2b>=6

replace aedu_ci=12 if (s3p2a>=7 & s3p2a<=8) & s3p2b==0
replace aedu_ci=13 if (s3p2a>=7 & s3p2a<=8) & s3p2b==1
replace aedu_ci=14 if (s3p2a>=7 & s3p2a<=8) & s3p2b==2
replace aedu_ci=15 if (s3p2a>=7 & s3p2a<=8) & s3p2b==3
replace aedu_ci=16 if (s3p2a>=7 & s3p2a<=8) & s3p2b==4
replace aedu_ci=17 if (s3p2a>=7 & s3p2a<=8) & s3p2b==5
replace aedu_ci=18 if (s3p2a>=7 & s3p2a<=8) & s3p2b==6
replace aedu_ci=19 if (s3p2a>=7 & s3p2a<=8) & s3p2b==7
replace aedu_ci=20 if (s3p2a>=7 & s3p2a<=8) & s3p2b==8

replace aedu_ci=18 if s3p2a==9 & s3p2b==1
replace aedu_ci=19 if s3p2a==9 & s3p2b==2
replace aedu_ci=20 if s3p2a==9 & s3p2b==3
replace aedu_ci=21 if s3p2a==9 & s3p2b==4
replace aedu_ci=22 if s3p2a==9 & s3p2b==5
replace aedu_ci=23 if s3p2a==9 & s3p2b==6
replace aedu_ci=24 if s3p2a==9 & s3p2b==7


/*Hay unos 14 casos en donde la edad es menor o igual que los años de educación
asumo que hay un error de codificación y hago missing estos casos*/

*/

gen byte aedu_ci=.
replace aedu_ci=0 if s3p2a==1 
replace aedu_ci=1 if s3p2a==3 | s3p2a==2
replace aedu_ci=1+s3p2b	if s3p2a==4
replace aedu_ci=6+s3p2b	if s3p2a==5 | s3p2a==6
replace aedu_ci=11+s3p2b if s3p2a==7 | s3p2a==8 
replace	aedu_ci=16+s3p2b if s3p2a==9

label var aedu_ci "Anios de educación"

* Se considera que los que tienen postgrado estudiaron, previamente, una carrera 
* universitaria de 5 años 
replace aedu_ci=16+s3p2b if s3p2a==9 

* Los que tienen "otro" se asume que tienen primaria completa ;
replace aedu_ci=6 if s3p2a==10 

replace aedu_ci=. if aedu_ci>=edad_ci

**************
***eduno_ci***
**************

*gen byte eduno_ci=(s3p2a==1 | s3p2a==2) 
gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

*gen byte edupi_ci=((s3p2a==4 & s3p2b<5) | s3p2a==3)
gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

*gen byte edupc_ci=(s3p2a==4 & s3p2b>=5)
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

*gen byte edusc_ci=(s3p2a>=5 & s3p2a<=6 & s3p2b>=5)
gen byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

*gen byte edus1i_ci=(s3p2a>=5 & s3p2a<=6 & s3p2b<=1)
gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

*gen byte edus1c_ci=(s3p2a>=5 & s3p2a<=6 & s3p2b==2)
gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

*gen byte edus2i_ci=(s3p2a>=5 & s3p2a<=6 & s3p2b>=3 & s3p2b<=4)
gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

*gen byte edus2c_ci=(s3p2a>=5 & s3p2a<=6 & s3p2b>=5)
gen byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

*gen byte eduui_ci=(s3p2a==7 | s3p2a==8) & s3p2b<5
*gen byte eduui_ci=((aedu_ci>11 & aedu_ci<16) & (s3p2a==8 | s3p2a==9))
gen byte eduui_ci=(aedu_ci>11 & aedu_ci<16) 
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

*gen byte eduuc_ci=((s3p2a==7 | s3p2a==8) & s3p2b>=5) | s3p2a==9
*gen byte eduuc_ci=((aedu_ci>=16) & (s3p2a==8 | s3p2a==9))
gen byte eduuc_ci=(aedu_ci>=16)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(s3p2a==2)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (s3p2a==8)
replace eduac_ci=0 if (s3p2a==7)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if s3p6a>0 & s3p6a<.
replace asiste_ci=0 if s3p6a==0

label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis=s3p8
label variable pqnoasis "Razones para no asistir a la escuela"
label define pqnoasis 1 "Trabajando" 2 "Ayudar a trabajar a padres"
label define pqnoasis 3 "Quehaceres del hogar." 4 "Enfermedad", add 
label define pqnoasis 5 "Prob. economicos" 6 "Prob. familiares" , add
label define pqnoasis 7 "Bajas notas" 8 "No le interesa", add
label define pqnoasis 9 "Edad insuficiente" 10 "Termino estudios", add
label define pqnoasis 11 "Se caso" 12 "Embarazo", add
label define pqnoasis 13 "Otros", add
label value pqnoasis pqnoasis

/*Hay un caso en donde la persona está asistiendo a la escuela y responde
razón para no asistir, asumo que es un error de codificación y hago missing
en pqnoasis*/

replace pqnoasis=. if asiste_ci==1 & pqnoasis~=.

***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.

***************
***edupub_ci***
***************

gen edupub_ci=(s3p7==1)
replace edupub_ci=. if asiste_ci~=1

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(s2b7==1 | s2b7==2)

gen aguadist_ch=1 if s2b7==1 
replace aguadist_ch=2 if s2b7==2
replace aguadist_ch=3 if s2b7>2

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(s2b11==1)

gen luzmide_ch=.
/*NA*/

gen combust_ch=(s2b13==1 | s2b13==2)

gen bano_ch=.
/*NA*/

gen banoex_ch=.
/*NA*/

gen des1_ch=0 if s2b10==7
replace des1_ch=1 if s2b10>=1 & s2b10<=3
replace des1_ch=2 if s2b10==4
replace des1_ch=3 if s2b10==5

gen des2_ch=0 if s2b10==7
replace des2_ch=1 if s2b10>=1 & s2b10<=3
replace des2_ch=2 if s2b10==4

gen piso_ch=0 if s2a3==6
replace piso_ch=1 if s2a3>=1 & s2a3<=5
replace piso_ch=2 if s2a3==7

gen pared_ch=0 if s2a2==6
replace pared_ch=1 if s2a2>=1 & s2a2<=5
replace pared_ch=2 if s2a2==7

gen techo_ch=0 if s2a4>=5 & s2a4<=6
replace techo_ch=1 if s2a4>=1 & s2a4<=4
replace techo_ch=2 if s2a4==7

gen resid_ch=.
/*NA*/

gen dorm_ch=.
/*NA*/

gen cuartos_ch=s2a5


gen cocina_ch=.
/*NA*/

gen telef_ch=(s2b15a==1)

gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
/*NA*/

gen internet_ch=.

gen cel_ch=(s2b19>=1)

gen vivi1_ch=1 if s2a1==1
replace vivi1_ch=2 if s2a1==2
replace vivi1_ch=3 if s2a1>2

gen vivi2_ch=(s2a1==1 | s2a1==2)

gen viviprop_ch=0 if s2b1==4
replace viviprop_ch=1 if s2b1==2
replace viviprop_ch=2 if s2b1==3
replace viviprop_ch=3 if s2b1==1  | s2b1==5

gen viviitit_ch=.
/*NA*/

gen vivi=s2b4a*3.49
egen vivialq_ch=rsum(s2b4b vivi) if s2b1==4
replace vivialq_ch=. if s2b4b==1 & vivi==.

gen vivimp=s2b5a*3.49
egen vivialqimp_ch=rsum(s2b5b vivimp) 
replace vivialqimp_ch=. if s2b5b==1 & vivimp==.



save "`in'2000\Arm_data\PER2000EA_ALT_BID.dta", intercooled replace



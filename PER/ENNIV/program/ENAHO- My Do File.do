clear
set more on
cap log close

cd D:\BID\PERU\Data\2004\

log using "ENAHO-2004", replace

use enaho2004-1a5.dta

drop if  result==3 | result==4 | result==5 | result==6 | result==7  
/* Estas son las encuestas incompletas o vacias que estan en la base */



/*****************************************************************************

                   ENCUESTA NACIONAL DE HOGARES - PERU

	               INTER-AMERICAN DEVELOPMENT BANK


This do-file proceses micro-data from :
Country : Peru
Year : 2004-2007
Survey : ENAHO
Author : Dimitris Mavridis
         mavridis.dimitris@gmail.com


*****************************************************************************/



/*
El do-file contiene las siguientes secciones :
I:    IDENTIFICATION VARIABLES
II:   DEMOGRAPHIC VARIABLES
III:  GEOGRAPHICAL VARIABLES
IV:   EDUCATION VARIABLES
V:    LABOR MARKET VARIABLES
		A: Employment, Unemployment and Inactivity
			A.1. Definicion BID
			A.2. Definicion INEI
			A.3. Subempleo
			A.4. Categoria ocupacional, Ocupacion, Rama
VI:   INCOME VARIABLES
		A: LABOR INCOME
			A.1. MAIN JOB
			A.2. SECOND JOB
			A.3. HOURLY EARNINGS
			A.4. LABOR INCOME BY SOURCE
		B: NON-LABOR INCOME
			B.1. TOTAL OF NON LABOR INCOME
		C: TOTAL INCOME (INDIVIDUALS)
			C.1. Income Quintiles (individuals)
		D: TOTAL INCOME (HOUSEHOLDS)
			D.1 Income Quintiles (by households)
*/




*------------------------------------------------------------*
* I. IDENTIFICATION VARIABLES
*------------------------------------------------------------*

*- Identificacion del hogar : 

sort conglome vivienda codperso
egen id = group (conglome vivienda)
label var id "identificacion unica del hogar"


*relacion entre los miembros del hogar
gen relacion=.
replace relacion=1 if p203==1
replace relacion=2 if p203==2
replace relacion=3 if p203==3
replace relacion=4 if p203>=4 & p203<=7
replace relacion=5 if p203==10 | p203==9
replace relacion=6 if p203==8
label variable relacion "Relacion con el jefe del hogar"
label define relacion 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
label value relacion relacion
tab relacion

*- Numero de familiares que habitan el hogar
by id, sort: egen nmiembros= sum(relacion>=1 & relacion<=4)
label variable nmiembros "Numero de familiares en el hogar"

*------------------------------------------------------------*
* II. DEMOGRAPHIC VARIABLES
*------------------------------------------------------------*

* I rename variables that I will use often
rename p208a edad
rename p207 sexo
*drop if edad>75

*- Creation of age groups :

gen gedad=.
replace gedad=1 if edad<14
replace gedad=2 if edad>=14 & edad<19
replace gedad=3 if edad>=19 & edad<25
replace gedad=4 if edad>=25 & edad<30
replace gedad=5 if edad>=30 & edad<40
replace gedad=6 if edad>=40 & edad<50
replace gedad=7 if edad>=50 & edad<65
replace gedad=8 if edad>=65

label var gedad "grupos de edad"
label define gedad 1 "[-,13]"  2 "[14,18]"  3 "[19,24]"  4 "[25,29]"  5 "[30,39]"  6 "[40,49]"  7 "[50,64]"  8 "[65,+]" 
label values gedad gedad
tab gedad, m

*------------------------------------------------------------*
* III . GEOGRAPHICAL VARIABLES
*------------------------------------------------------------*

* I create a dummy for Urban areas
gen urbano=0
replace urbano=1 if estrato>=1 & estrato<=6
label var urbano "=1 if localidad urbana"
tab urbano
tab estrato


*- Regional Dummies

gen lima=1 if dominio==8
replace lima=0 if dominio~=8
label var lima "=1 si Lima Metropolitana"

gen selva=1 if dominio==7
replace selva=0 if dominio~=7
label var selva "=1 si Selva"

gen sierrasur=1 if dominio==6
replace sierrasur=0 if dominio~=6
label var sierrasur "=1 Sierra Sur"

gen sierracen=1 if dominio==5
replace sierracen=0 if dominio~=5
label var sierracen "=1 si Sierra Centro"

gen sierranor=1 if dominio==4
replace sierranor=0 if dominio~=4
label var sierranor "=1 si Sierra Norte"

gen costasur=1 if dominio==3
replace costasur=0 if dominio~=3
label var costasur "=1 si Costa Sur"

gen costacen=1 if dominio==2
replace costacen=0 if dominio~=2
label var costacen"=1 si Costa Centro"

gen costanor=1 if dominio==1
replace costanor=0 if dominio~=1
label var costanor "=1 si Costa Norte"

/*
tab dominio
tab lima
tab costanor
tab costacen
tab costasur
tab sierranor
tab sierracen
tab sierrasur
tab selva
*/


*------------------------------------------------------------*
* IV.  EDUCATION VARIABLES 
*------------------------------------------------------------*

/*En esta sección es sólo para los residentes habituales 
mayores a los 3 años de edad*/

*- Genero Educacion en Anos:

gen aedu=.
replace aedu=0 if p301a==1 | p301a==2
replace aedu=0 if p301a==3 & p301c==0
replace aedu=1 if p301a==3 & p301c==1 
replace aedu=2 if p301a==3 & p301c==2
replace aedu=3 if p301a==3 & p301c==3
replace aedu=4 if p301a==3 & p301c==4
replace aedu=5 if p301a==3 & p301c==5
replace aedu=5 if p301a==3 & p301c==6

replace aedu=6 if p301a==4 

replace aedu=7 if p301a==5 & p301b==1
replace aedu=8 if p301a==5 & p301b==2
replace aedu=9 if p301a==5 & p301b==3
replace aedu=10 if p301a==5 & p301b==4

replace aedu=11 if p301a==6 

replace aedu=12 if (p301a>=7 & p301a<=10) & p301b==1
replace aedu=13 if (p301a>=7 & p301a<=10) & p301b==2
replace aedu=14 if (p301a>=7 & p301a<=10) & p301b==3
replace aedu=15 if (p301a>=7 & p301a<=10) & p301b==4
replace aedu=16 if (p301a>=7 & p301a<=10) & p301b==5
replace aedu=17 if (p301a>=7 & p301a<=10) & p301b==6
replace aedu=18 if (p301a>=7 & p301a<=10) & p301b==7

replace aedu=19 if p301a==11 & p301b==1
replace aedu=20 if p301a==11 & p301b==2
replace aedu=21 if p301a==11 & p301b==3
replace aedu=22 if p301a==11 & p301b==4

replace aedu=. if p212==.
tab aedu


*- Genero Dummies del nivel Educativo :

gen noedu=0
replace noedu=1 if p301a<3
replace noedu=. if p301a==.
label var noedu "sin nivel"

gen prii=0
replace prii=1 if p301a==3
replace prii=. if p301a==.
label var prii "=1 si primaria incompleta"

gen pric=0
replace pric=1 if p301a==4
replace pric=. if p301a==.
label var pric "=1 si primaria completa"

gen seci=0
replace seci=1 if p301a==5
replace seci=. if p301a==.
label var seci "=1 si secundaria incompleta"

gen secc=0
replace secc=1 if p301a==6
replace secc=. if p301a==.
label var secc "=1 si secundaria completa"

gen teri=0
replace teri=1 if p301a==7
replace teri=. if p301a==.
label var teri "=1 terciaria incompleta"

gen terc=0
replace terc=1 if p301a==8
replace terc=. if p301a==.
label var terc "=1 terciaria completa"

gen unii=0
replace unii=1 if p301a==9
replace unii=. if p301a==.
label var unii "=1 universitaria incompleta"

gen unic=0
replace unic=1 if p301a==10 | p301a==11
replace unic=. if p301a==.
label var unic "=1 universitaria completa"

/*
ta p301a
ta noedu
ta prii
ta pric
ta seci
ta secc
ta teri
ta terc
ta unii
ta unic
*/


*------------------------------------------------------------*
* V. LABOR MARKET VARIABLES 
*------------------------------------------------------------*


/* This section applies to all individuals older or equal to 14 years old. */

/*
Information used to build the variables of Labor Market Participation - Unemployment - Employment and Inactivity :

p501 == Tuvo algun trabajo la semana pasada ?  -- Si / No
p502 == Aunque no trabajo la semana pasada, tiene algun empleo fijo ? -- Si / No
p503 == aunque no trabajó la semana pasada tiene algún negocio propio al que próximame volvera?  --- Si / No

realizo alguna de las siguientes actividades para obtener ingresos, al menos una hora ?
   p5041   == Trabajando en algun negocio
   p5042   == Ofreciendo al;gun servicio
   p5043   == Haciendo algo en casa para vender
   p5044   == Vendiendo productos de belleza, ropa, joyas, etc
   p5045   == Realizando alguna labor artesanal
   p5046   == Haciendo practicas pagadas en algun centro de trabajo
   p5047   == Trabajando para algun hogar particular	
   p5048   == Fabricando algun producto
   p5049   == Realizando labores remuneradas en la chacra o cuidado de animales
   p50410  == Ayudando a un familiar sin remuneracion
   p50411  == Otra

p505 = Cual es la ocupacion principal que desempeno = Tipo de Actividad por 3 digitos.

P545 = La semana pasada, hizo algo para conseguir trabajo ? (esta variable no existe en la base!)
p546 = Que estuvo haciendo la semana pasada ?  1=Tramites ; 2=Reparando activos ; 3=esperando el inicio de un trabajo; 4=estudiando; 5=quehaceres del hogar; 6=vivia de su jubilacion; 7= Enfermo o Incapacitado; 8= otro
p547 = La semana pasada, queria usted trabajar ?  -- Si=1  / No=2
p548 = La semana pasada, estuvo disponible para trabajar ?  -- Si=1  / No=2
*/

*- Experiencia laboral en el trabajo principal ( en anos) = p513a1
rename p513a1 exp
gen exp2 = exp*exp



*-------------------------------------------------------------------------------------
* Creation of Employment, Unemployment and Inactivity Variables
*-------------------------------------------------------------------------------------


***************************
*- DEFINITION 1 : BID
***************************


/* Definimos como ocupado al individuo que tiene trabajo, o volvera al trabajo proximamente,
 o recibio ingresos por haber trabajado por lo menos una hora en la semana pasada */

*- EMPLOYED
gen emp1=0
replace emp1=1 if p501==1 
replace emp1=1 if (p502==1 | p503==1)
replace emp1=1 if (p5041==1 | p5042==1 | p5043==1 | p5044==1 | p5045==1 | p5046==1 | p5047==1 | p5048==1 | p5049==1 | p50410==1)
replace emp1=. if p214==.


* We define as unemployed those who did not work last week and really looked for a job 

* Desocupado 
gen desocupado1=.
replace desocupado1=1 if (p501==2 & p502==2 & p503==2 & p5041==2 & p5042==2 & p5043==2 &  p5044==2 & p5045==2 & p5046==2 & p5047==2 & p5048==2 & p5049==2 & p50410==2 &  p548==1)
replace desocupado1=0 if (p501==1 | p5041==1 | p5042==1 | p5043==1 |  p5044==1 | p5045==1 | p5046==1 | p5047==1 | p5048==1 | p5049==1 | p50410==1 | p548==2)
label var desocupado1 "=1 if desocupado - def unemploymnt ILO"
tab desocupado1, m

*- UNEMPLOYED-1
gen desemp1=(emp==0 & (p545==1 | p549==11))
replace desemp1=. if emp==.

*- UNEMPLOYED-2
gen desemp2=(desemp1==1 | p549==10)
replace desemp2=0 if emp==1
replace desemp2=. if emp==. 

*-pea1
gen pea1=0
replace pea1=1 if emp==1 |desemp1==1
replace pea1=. if emp==.

*-pea2
gen pea2=0
replace pea2=1 if emp==1 |desemp2==1
replace pea2=. if emp==.


***************************
*- DEFINITION 2 : INEI
***************************

* Creation of Employment, Unemployment and Inactivity Variables
tab ocu500, m

gen ocupado=.
replace ocupado=1 if ocu500==1
replace ocupado=0 if ocu500==2 | ocu500==3 | ocu500==4
label var ocupado "=1 if ocupado - def ILO"
tab ocupado

gen desocupado=.
replace desocupado=1 if ocu500==2 | ocu500==3
replace desocupado=0 if ocu500==1 | ocu500==4
label var desocupado "=1 if desocupado - def ILO"
tab desocupado

gen inactivo=.
replace inactivo=1 if ocu500==4
replace inactivo=0 if ocu500==1 | ocu500==2 | ocu500==3
label var inactivo "=1 if inactivo- def ILO"
tab inactivo

gen pea=.
replace pea=1 if (ocupado==1 | desocupado==1) & inactivo==0
replace pea=0 if (inactivo==1 & ocupado==0 & desocupado==0)
label var pea "=1 if pea - def ILO"
tab pea

tab ocu500
tab ocupado
tab desocupado
tab inactivo
tab pea
tab pea1




***************************
*- SUBEMPLEO
***************************



*- HORAS TRABAJADAS

rename p513t hhpri    /* total de horas trabajadas en la ocupacion principal en la ultima semana */
rename p518 hhsec     /* total de horas trabajadas en la ocupacion secundaria en la ultima semana */

egen hh = rsum (hhpri hhsec)   /*Total de horas trabajadas en todas ocupaciones */
replace hh=. if hh==0
label var hh "total de horas trabajadas"
*ta hh



*- DEFINICION DEL SUBEMPLEO



*- Definition 1 : Tomamos las horas trabajadas en la ocupacion principal :


/* 
Defino como sub-empleados a los que trabajan menos de 35 horas, quieren trabajar mas y estan disponibles para trabajar mas. 
Variables utilizadas : 
hhpri = total de horas trabajadas en ocupacion principal
p521 = La semana pasada, queria trabajar mas horas de las que normalmente trabaja ?  = Si/No
p521a = La semana pasada, estuvo disponible para trabajar mas horas ? = Si/No
*/

gen subempleo=.
replace subempleo=1 if (hhpri<35 & p521==1 & p521a==1)
replace subempleo=0 if (hhpri>=35 | p521==2 | p521a==2) 
replace subempleo=. if p214==.
label var subempleo "=1 if subempleo en la ocupacion principal"

tab subempleo [aw=fac500]
tab gedad subempleo [aw=fac500]
tab subempleo if sexo==1 [aw=fac500]
tab subempleo if sexo==2 [aw=fac500]
tab subempleo sexo if urbano==1 [aw=fac500]
tab subempleo sexo if urbano==0 [aw=fac500]
tab subempleo if urbano==0 [aw=fac500]
tab subempleo if urbano==1 [aw=fac500]


*- defino ahora los que tienen un segundo trabajo y son sub-empleados en el primer trabajo.
*- I name the variable subemp2
gen subemp2=0
replace subemp2=1 if subempleo==1 & hh>hhpri
replace subemp2=. if p214==.
label var subemp2 "=1 si es subempleado y tiene 2ndo trabajo"
ta subemp2

/* Ahora podemos calcular, de los subempleados, cuantos tienen un segundo trabajo */
tab subempleo subemp2 [aw=fac500]

*- Definition 2 : Tomamos el total de horas trabajadas en todas las ocupaciones:

/* 
Defino como sub-empleados a los que trabajan menos de 35 horas, quieren trabajar mas y estan disponibles para trabajar mas. 
Variables utilizadas : 
hh = total de horas trabajadas
p521 = La semana pasada, queria trabajar mas horas de las que normalmente trabaja ?  = Si/No
p521a = La semana pasada, estuvo disponible para trabajar mas horas ? = Si/No
*/

*ta hh if p521==1 & p521a==1

gen subemp=.
replace subemp=1 if (hh<35 & p521==1 & p521a==1)
replace subemp=0 if (hh>=35 | p521==2 | p521a==2)
replace subemp=. if p214==.
label var subemp "=1 if subempleado" 
tab subemp [aw=fac500]



ta p550 [aw=fac500]

*- Categoria Profesional - Trabajo principal

gen catepri=.
replace catepri=1 if p507==1
replace catepri=2 if p507==2
replace catepri=3 if p507==3 | p507==4 | p507==6
replace catepri=4 if p507==5 | p507==7
label define catepri 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "No remunerado"
label value catepri catepri
label var catepri "Categoria Ocupacional del trabajo principal"


*- Categoria Profesional - Trabajo secundario

gen catesec=.
replace catesec=1 if p517==1
replace catesec=2 if p517==2
replace catesec=3 if p517==3 | p517==4 | p517==6
replace catesec=4 if p517==5 | p517==7
label define catesec 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "No remunerado"
label value catesec catesec
label var catesec "Categoria Ocupacional del trabajo secundario"

*- Tipo de Ocupacion

gen ocupacion=.
replace ocupacion=1 if (p505>=211 & p505<=396) & emp1==1
replace ocupacion=2 if (p505>=111 & p505<=148) & emp1==1
replace ocupacion=3 if (p505>=411 & p505<=462) & emp1==1
replace ocupacion=4 if (p505>=571 & p505<=583) | (p505>=911 & p505<=931) & emp1==1
replace ocupacion=5 if (p505>=511 & p505<=565) | (p505>=941 & p505<=961) & emp1==1
replace ocupacion=6 if (p505>=611 & p505<=641) | (p505>=971 & p505<=973) & emp1==1
replace ocupacion=7 if (p505>=711 & p505<=886) | (p505>=981 & p505<=987) & emp1==1
replace ocupacion=8 if (p505>=11 & p505<=24) & emp1==1


*- Rama de Actividad

gen rama=.
replace rama=1 if (p506>=100 & p506<=500) & emp1==1
replace rama=2 if (p506>=1000 & p506<=1429) & emp1==1
replace rama=3 if (p506>=1500 & p506<=3720) & emp1==1
replace rama=4 if (p506>=4000 & p506<=4100) & emp1==1
replace rama=5 if (p506>=4500 & p506<=4550) & emp1==1
replace rama=6 if (p506>=5000 & p506<=5520) & emp1==1
replace rama=7 if (p506>=6000 & p506<=6420) & emp1==1
replace rama=8 if (p506>=6500 & p506<=7499) & emp1==1
replace rama=9 if (p506>=7500 & p506<=9503) & emp1==1






*------------------------------------------------------------*
* VI. CREATION OF INCOME VARIABLES
*------------------------------------------------------------*


*-------------------------------------
*- I : Creation of Labor market Income
*-------------------------------------

/* Variables usadas : 
*- p530a = Ganancia neta en la ocupacion principal
*- p536 = Valor del autoconsumo de los bienes producidos por el hogar y de los productos adquiridos con fines comerciales en ocupacion principal
*- p541a = Ganancia neta en la ocupacion secundaria
*- p543 = Valor del autoconsumo de los bienes producidos en la ocupacion secundaria
*- p544t = total de ingresos extraordinarios por trabajo 
*/

*- Ingreso Laboral Primario
egen ilp = rsum( p530a p536)
replace ilp=. if ilp==0
label var ilp "ingreso laboral primario"
gen lilp= ln(ilp)
label var lilp "log ingreso laboral primario" 
*kdensity lilp if sexo==1, addplot(kdensity lilp if sexo==2)


*- Ingreso Laboral Secundario
egen ils = rsum( p541a p543)
replace ils=. if ils==0
label var ilp "ingreso laboral secundario"
gen lils= ln(ils)
label var lils "log ingreso laboral secundario" 
*kdensity lils if sexo==1, addplot(kdensity lils if sexo==2)

*- Total Labor Income
egen ilt= rsum(ilp ils p544t)
replace ilt=. if ilt==0
label var ilt "Ingreso Laboral Total"
gen lilt= ln(ilt)
*kdensity lilt 

*- Identifica Perceptor de ingresos laborales
gen percila=1 if ilt>0 & ilt~=.
replace percila=0 if ilt==.
label var percila "Perceptor de ingresos laborales"
tab percila

*- Numero de personas perceptores de ingresos laborales en un hogar
egen n_perila_h= sum(percila), by (id)
label var n_perila_h "nmro de perceptores de ila en hogar"

*-------------------------------------
*- I : Creation of Household Income (Labor income) -correcto, pero no tiene mucho sentido
*-------------------------------------
**- Ingreso Laboral Familiar - total
egen ilf = sum(ilt), by (id)
label var ilf "ingreso laboral familiar -total"

*- Ingreso Laboral Familiar - por persona
gen ilfp = (ilf/nmiembros)
label var ilfp "ingreso laboral familiar por persona"
sum ilfp

**- Wages
*- p518 : horas trabajadas en las ocupaciones secundarias la semana pasada : 11.000 observaciones
*- p520 : horas trabajadas normalmente por semana, en todas las ocupaciones   - 9.000 observaciones
*- p513 = hhp = horas trabajadas en la ocupacion principal la semana pasada : 49.000 observaciones

gen wagep = (ilp/(hhp*4.3))
label var wagep "hourly wage in ocupacion ppal"



*---------------------------------------------*
*- II : Creation of Non-Labor Income
*---------------------------------------------*

*- Ingreso de Pensions
egen ijubi= rsum(p556t1 p556t2)
label var ijubi "ingresos de pensiones"

*- Ingreso rentas de propiedades
gen ipropi = p557t
label var ipropi "ingresos de propiedades"

*- ingresos extraordinarios
gen ingx = p558t
label var ingx "ingresos extraordinarios"


**- Suma de ingresos no laborales
egen inla = rsum (ijubi ipropi ingx)
label var inla "ingresos no laborales"


*---------------------------------------------*
*- III : TOTAL INCOME (individuals)
*---------------------------------------------*


*- Per person
egen itot= rsum (ilt inla) 
label var itot "suma de ingresos laborales y no laborales"

*- Identifica perceptor de ingresos
gen percing=1 if itot>0 & itot~=.
replace percing=0 if itot==.
label var percing "=1 si percibe ingresos de algun tipo"

*- Numero de personas perceptores de ingresos en un hogar
egen n_percing_h= sum(percing), by (id)
label var n_percing_h "nmro de perceptores de ingresos en hogar"


*- Ingreso Familiar total :
by id, sort : egen ift = sum(itot)
label var ilf "ingreso familiar -total"

*- Ingreso Familiar total - por miembro de familia :
gen ift_miembro = (ift/nmiembros)
label var ilf "ingreso familiar total por numero de miembros"



**- Creation of Income Quintiles :

*- Quintiles de ingreso laboral total
xtile qilt = ilt, nq(5)

*- Quintiles de ingreso familiar total
xtile qift = ift, nq(5)

*- Quintiles de ingreso familiar total - por persona
xtile qiftp= ift_miembro, nq(5)



*--------------------------------------------------------------------------*
*-                       TABULATIONS 

***-  TABULATIONS OF PARTICIPATION, SUB-EMPLOYMENT AND UNEMPLOYMENT RATES

/*
tab gedad pea if edad<75 [aw=fac500]
tab gedad pea if sexo==1 & edad<75 [aw=fac500]
tab gedad pea if sexo==2 & edad<75 [aw=fac500]

tab gedad desocupado  if edad<75 [aw=fac500]
tab gedad desocupado if sexo==1 & edad<75 [aw=fac500]
tab gedad desocupado if sexo==2 & edad<75 [aw=fac500]




tab p550 if lima==1 [aw=fac500]
tab p550 if urbano==1 [aw=fac500]
tab p550 if urbano==0 [aw=fac500]

tab gedad pea if urbano==1  & edad<75 [aw=fac500]
tab gedad pea if urbano==0 & edad<75 [aw=fac500]

tab sexo subemp [aw=fac500]
tab gedad subemp  [aw=fac500]
tab gedad subemp if urbano==1 [aw=fac500]
tab gedad subemp if urbano==0 [aw=fac500]
tab gedad subemp if sexo==1 [aw=fac500]
tab gedad subemp if sexo==2 [aw=fac500]

sort qift
by qift : tab sexo subemp [aw=fac500]
by qift : sum subemp [aw=fac500]

****-  LABOR MARKET STATS BY FAMILY INCOME
sort qiftp 
by qiftp : tab pea [aw=fac500]
by qiftp : tab desocupado [aw=fac500]

sort qift
by qift : tab pea1 [aw=fac500]
by qift : tab pea [aw=fac500]

sort qift
by qift : tab pea1 if urbano==1 [aw=fac500]
by qift : tab pea1 if urbano==0 [aw=fac500]

*/



tab pea if urbano==1 [aw=fac500]
tab pea if urbano==0 [aw=fac500]
tab pea if sexo==1 & urbano==1 [aw=fac500]
tab pea if sexo==1 &  urbano==0 [aw=fac500]
tab pea if sexo==2 &  urbano==1 [aw=fac500]
tab pea if sexo==2  & urbano==0 [aw=fac500]

tab ocupado if urbano==1 [aw=fac500]
tab ocupado if urbano==0 [aw=fac500]
tab ocupado if sexo==1  & urbano==1 [aw=fac500]
tab ocupado if sexo==1 &  urbano==0 [aw=fac500]
tab ocupado if sexo==2  & urbano==1 [aw=fac500]
tab ocupado if sexo==2  & urbano==0 [aw=fac500]

tab desocupado if  urbano==1 [aw=fac500]
tab desocupado  if urbano==0 [aw=fac500]
tab desocupado if sexo==1 &  urbano==1 [aw=fac500]
tab desocupado if sexo==1  & urbano==0 [aw=fac500]
tab desocupado if sexo==2  & urbano==1 [aw=fac500]
tab desocupado if sexo==2 &  urbano==0 [aw=fac500]

tab inactivo if  urbano==1 [aw=fac500]
tab inactivo if  urbano==0 [aw=fac500]
tab inactivo if sexo==1 &  urbano==1 [aw=fac500]
tab inactivo if sexo==1 &  urbano==0 [aw=fac500]
tab inactivo if sexo==2  & urbano==1 [aw=fac500]
tab inactivo if sexo==2 &  urbano==0 [aw=fac500]

tab subempleo if  urbano==1 [aw=fac500]
tab subempleo  if urbano==0 [aw=fac500]
tab subempleo if sexo==1 &  urbano==1 [aw=fac500]
tab subempleo if sexo==1  & urbano==0 [aw=fac500]
tab subempleo if sexo==2  & urbano==1 [aw=fac500]
tab subempleo if sexo==2 &  urbano==0 [aw=fac500]

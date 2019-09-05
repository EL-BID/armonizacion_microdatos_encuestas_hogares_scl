                 **********************************
				***ARMONIZACIÓN JAMAICA 2007    ***
				***********************************


*Armonización:
*Mayra Sáenz
*Julio 2013
*Labour Force Survey (LFS)

clear all
*set mem 500m
cd "X:\ARM\JAM\2007\LFS\Original Data"
set more off

use jam07.dta

* Inclusión Mayra Sáenz - Julio 2013- Cambiar los nombres a minúsculas
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}


*====================================================================================================================================*
*                                                    VARIABLES DEL HOGAR                                                             *
*====================================================================================================================================*
* **Inclusión Mayra Sáenz - Julio 2013
***********
*  Región  *
************

gen region_c=  .
label var region_c "División política"

*************************
*  factor de expansión  *
*************************
/* El factor de expansión actual expande la población a 267,745,088,344 individuos, cuando la población de 
Jamaica al año 2007 fue de 2,782,221 habitantes (PennWorldTable 7.1, 2013).
Por lo tanto es necesario cambiar las ponderaciones de la misma manera que se trabajó en el año 2002. 
*/
** POBLACION DA JAMAICA AL 2007 = 2,782,221 **

sum rfact
scalar pob=r(sum)
gen pop=rfact*(2782221/pob)
sum pop
ret list
gen factor_ch=pop 
drop pop
label var factor_ch "Factor de expansion del hogar"
**************************
* identificador del hogar*
**************************

ren  hhid idh_ch
label var idh_ch "ID del hogar"

****************************
* identificador de persona *
****************************

ren ind idp_ci
label var idp_ci "ID de la persona en el hogar"


***************************
* zona urbana o zona rural*
***************************

gen zona_c=.
replace zona_c=1 if urcode==1 |  urcode==2
replace zona_c=0 if urcode==3
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

**********************************
* país***************************

gen pais_c="jam"
label variable pais_c "Pais"

****************************
* anio de la encuesta ******

gen anio_c=2007
label variable anio_c "Anio de la encuesta"


***********************
*  mes de la encuesta *
***********************

gen mes_c=4
label variable mes_c "Mes de la encuesta"
label define mes_c 4 "Abril" 
label value mes_c mes_c


*******************************
*relación con el jefe de hogar*
*******************************

*Modificado Mayra Sáenz Julio 2013
*Antes no se incluía la categoría 4, pero se debe incluir en otros parientes ya que hace referencia al
* yerno o nuera del jefe de hogar.
/*
relat


head			1
spouse			2
child			3
child's spouse	4
grandchild		5
parent hd/sp	6
other relative	7
helper			8
other			9

*/

gen relacion_ci=.
replace relacion_ci=1 if relat==1
replace relacion_ci=2 if relat==2
replace relacion_ci=3 if relat==3
replace relacion_ci=4 if relat==4 |relat==5 | relat==6 | relat==7 
replace relacion_ci=5 if relat==9 
replace relacion_ci=6 if relat==8
label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

*====================================================================================================================================*
*                                                          VARIABLES DEMOGRAFICAS                                                    *
*====================================================================================================================================*

****************************************
*factor de expansión a nivel individual*
****************************************

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"
********
* sexo *
********

ren sex sexo_ci
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci


******
*edad*
******

gen  edad_ci = age
replace edad_ci=. if age==98 | age==99 
label variable edad_ci "Edad del individuo"

**************
*estado civil*
**************

gen civil_ci=.
label variable civil_ci "Estado civil"



***************
*jefe de hogar*
***************

gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"


******************
***nconyuges_ch***
******************

by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Número de conyuges"

***************
***nhijos_ch***
***************

by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Número de hijos"

******************
***notropari_ch***
******************

by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Número de otros familiares"

********************
***notronopari_ch***
********************

by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Número de no familiares"


****************
***nempdom_ch***
****************

by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Número de empleados domésticos"

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

label variable clasehog_ch "tipo de hogar"
label define clasehog_ch 1 " unipersonal" 2 "nuclear" 3 "ampliado" 
label define clasehog_ch 4 "compuesto" 5 " corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Número de familiares en el hogar"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<6)
label variable miembros_ci "miembro del hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Número de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Número de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Número de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Número de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Número de familiares menores a 1 anio"



*====================================================================================================================================*
*                                                          VARIABLES DEL MERCADO LABORAL                                              *
*====================================================================================================================================*
********
*emp_ci*
********

gen emp_ci=1 if q21==1 | q21a==1 | q21a==2 | q22==1  | q23==1 
replace emp_ci =0 if emp_ci==. 
label var emp_ci "Ocupado (empleado)"

************
*desemp1_ci*
************

gen desemp1_ci=1 if q43==1
replace desemp1_ci=0 if desemp1_ci==.
label var desemp1_ci "Desempleado que buscó empleo en el periodo de referencia"
************
*desemp2_ci*
************

gen desemp2_ci=1 if desemp1_ci==1 | q44==3 | q44==6
replace desemp2_ci=0 if desemp2_ci==.
label var desemp2_ci "desemp1_ci + personas que esperan respuesta a solicitud o temporada alta"
************
*desemp3_ci*
************

gen desemp3_ci=1 if (q41>=1 & q41<99) | desemp2_ci==1
replace desemp3_ci=0 if desemp3_ci==.
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"
*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1
label var pea1_ci "Poblacion economicamente activa utilizando la definicion 'desemp1'"


*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1
label var pea2_ci "Población Económicamente Activa con desemp2_ci"


*************
***pea3_ci***
*************
gen pea3_ci=1 if emp_ci==1 |desemp3_ci==1
label var pea3_ci "Población Económicamente Activa con desemp3_ci"

*************
*desalent_ci*
*************

gen desalent_ci=1 if q44==1 | q44==4
replace desalent_ci=0 if desalent_ci==.
label var desalent_ci "Trabajadores desalentados"

****************************************************
*horas totales trabajadas en la actividad principal*
****************************************************

/*sólo se pregunta cuantas horas se trabaja usualmente a la semana. no se pregunta por
actividad principal sino por todo. no es posible identificar las horas dedicadas a la actividad principal*/
gen horaspri_ci=.
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

**********************************************************
***Horas totales trabajadas en la actividad secundaria.***
**********************************************************
* No distingue actividad secundaria
gen horassec_ci=.
label var horassec_ci "Horas trabajadas semanalmente en el trabajo secundario"


***************************************************
*horas totales trabajadas en todas las actividades*
***************************************************

gen horastot_ci=q33
replace horastot_ci=. if horastot_ci==99
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"


***********
*subemp_ci*
***********

gen subemp_ci=1 if q34==1 & q33<30
replace subemp_ci=0 if subemp_ci==.
label var subemp_ci "Personas en subempleo por horas"

***************
*tiempoparc_ci*
***************

gen tiempoparc_ci=1 if q33<30 & q34==3
replace tiempoparc_ci=0 if tiempoparc_ci==.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

**************
*categopri_ci*
**************
/*
q323
 local govt. employe					1
other govt agencies employee			2
private sector employee					3
unpaid family worker					4
employer								5
own account worker						6
not stated								9
*/

gen categopri_ci=.
replace categopri_ci=1 if q323==5
replace categopri_ci=2 if q323==6
replace categopri_ci=3 if q323==1 | q323==2 | q323==3
replace categopri_ci=4 if q323==4

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"



**************
*categosec_ci*
**************

gen categosec_ci=.
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*************
*contrato_ci*
*************

gen contrato_ci=.
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

***********
*segsoc_ci*
***********

gen segsoc_ci=.
label var segsoc_ci "Personas que tienen seguridad social en salud por su trabajo"

*************
*nempleos_ci*
*************

gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if q37>=2 & q37<.
label var nempleos_ci "Número de empleos" 

*************
*firmapeq_ci*
*************

gen firmapeq_ci=.
replace firmapeq_ci=1 if q324==1 | q324 ==2
replace firmapeq_ci=0 if (q324>=3 & q324<=5)
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores formales"


*************
*spublico_ci*
*************
/*
q323
 local govt. employe					1
other govt agencies employee			2
private sector employee					3
unpaid family worker					4
employer								5
own account worker						6
not stated								9
*/
gen spublico_ci=.
replace spublico_ci=1 if q323==1 | q323==2
replace spublico_ci=0 if spublico_ci==.
label var spublico_ci "Personas que trabajan en el sector público"


***************************************
*ocupación laboral actividad principal*
***************************************
gen ocupa=string(q38m)
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=. if ocupa_ci==0
drop ocupa
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


**********************************
*rama laboral actividad principal*
**********************************

gen rama1=string(q39m)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 
drop rama1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


************
*durades_ci*
************

/*
pregunta q41

< 1week						1
1 week but < 1 month		2
1 month but < 3 months		3
3 months but < 6 months		4
6 months but < 9 months		5
9 months but < 12 months	6
12 months but < 2 years		7
2 years and over			8
not stated					99
*/

gen durades_ci=.
replace durades_ci=0.5/4.3 if q41==1
replace durades_ci=(1+4.3)/2 if q41==2
replace durades_ci=(1+3)/2 if q41==3
replace durades_ci=(3+6)/2 if q41==4
replace durades_ci=(6+9)/2 if q41==5
replace durades_ci=(9+12)/2 if q41==6
replace durades_ci=(12+24)/2 if q41==7
replace durades_ci=(24+36)/2 if q41==8
label variable durades_ci "Duracion del desempleo en meses"


***************
*antiguedad_ci*
***************

gen antiguedad_ci=.
replace antiguedad_ci=0.5/4.3 if q311==1
replace antiguedad_ci=(1+4.3)/2 if q311==2
replace antiguedad_ci=(1+3)/2 if q311==3
replace antiguedad_ci=(3+6)/2 if q311==4
replace antiguedad_ci=(6+9)/2 if q311==5
replace antiguedad_ci=(9+12)/2 if q311==6
replace antiguedad_ci=(12+24)/2 if q311==7
replace antiguedad_ci=(24+36)/2 if q311==8

label var antiguedad_ci "Antiguedad en la actividad actual en anios"

                    **************
					***INGRESOS***
					**************


***********************************************
*ingreso laboral monetario actividad principal*
***********************************************

/*no hay manera de encontrar ingreso de actividad principal, secundaria. solamente el total. 
tampoco se pregunta por pago en especie.*/

gen ylmpri_ci=.
replace ylmpri_ci=q325a if q325ap==4
replace ylmpri_ci=q325a*4.2 if q325ap==1
replace ylmpri_ci=q325a/12 if q325ap==7
label var  ylmpri_ci "Ingreso laboral monetario actividad principal"

**************************************************
*ingreso laboral no monetario actividad principal*
**************************************************

gen ylnmpri_ci=.
label var ylnmpri "Ingreso laboral no monetario act. principal"

*********************************************************************
*identificador de no respuesta del ingreso de la actividad principal*
*********************************************************************

gen nrylmpri_ci=.
label var nrylmpri_ci "Identificador de No Respuesta (NR) del ingreso de la actividad principal"


************************************************
*ingreso laboral monetario actividad secundaria*
************************************************

gen ylmsec_ci=.
label var ylmsec_ci "Ingreso laboral monetario actividad secundaria"


***************************************************
*ingreso laboral no monetario actividad secundaria*
***************************************************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"


*****************************************
*ingreso laboral monetario mensual total*
*****************************************

gen ylm_ci=.
replace ylm_ci=q325a if q325ap==4
replace ylm_ci=q325a*4.2 if q325ap==1
replace ylm_ci=q325a/12 if q325ap==7
label var ylm_ci "Ingreso laboral monetario total"

************************************
*ingreso laboral no monetario total*
************************************

gen ylnm_ci=.
label var ylnm_ci "Ingreso laboral no monetario total"

******************************************
*ingreso laboral monetario otros trabajos*
******************************************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario otros trabajos"

*********************************************
*ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral no monetario otros trabajos"

******************************
*ingreso no laboral monetario*
******************************

gen ynlm_ci=.
replace ynlm_ci=q325b if q325bp==4
replace ynlm_ci=q325b*4.2 if q325bp==1
replace ynlm_ci=q325b/12 if q325bp==7
label var ynlm_ci "Ingreso no laboral monetario (otras fuentes)"


*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario"

************
*remesas_ci*
************

gen remesas_ci=.
label var remesas_ci "Remesas reportadas por el individuo"


**********************************************************************
*identificador de los hogares en donde alguno de los miembros no sabe*
*	       no responde el ingreso de la actividad principal          *
**********************************************************************

gen nrylmpri_ch=.
label var nrylmpri_ch "Id. hogar si algun miembro no sabe o no respond el ingreso act. principal"

**************************************************
*identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del Hogar"


***********
**ynlm_ch**
***********

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del Hogar"

***************
*** ylnm_ch ***
***************

gen ylnm_ch=.
label var ylnm_ch  "Ingreso laboral no monetario del Hogar"

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del Hogar considera No respuesta"

************
*remesas_ch*
************

gen remesas_ch=.
label var remesas_ch "Remesas del Hogar"

***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 

*************
*rentaimp_ch*
*************

gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"


*************
*autocons_ch*
*************

gen autocons_ch=.
label var autocons_ch "Autoconsumo del Hogar"

*************
*autocons_ci*
*************

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

*************
*ylmhopri_ci*
*************

gen ylmhopri_ci=.
label var ylmhopri_ci "Salario  monetario de la actividad principal"

**********
*ylmho_ci*
**********

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario  monetario de todas las actividades"




*====================================================================================================================================*
*                                                   VARIABLES DE EDUCACIÓN
*====================================================================================================================================*	

/*
__________________________________________________________________________________________________________

+++++++++++++++++++++++++++++++++++++++++++++++++++++++
NOTA METODOLÓGICA DEL CÁLCULO DE EDUCACIÓN EN JAMAICA
+++++++++++++++++++++++++++++++++++++++++++++++++++++++
Mayra Sáenz-Julio 2013

*Años de educacion adicional a primaria.
En Jamaica el sistema de Educación es distinto al resto de América Latina.

*La primaria tiene seis años (1-6 grado).

*La secundaria puede extenderse a más de 7 años y se divide en lower level (7-9 grado) y 
upper level (10-11 grado) y para aprobar la secundaria deben rendir determinados exámenes que
se encuentran en distintas categorías.

Así, CXC basic, JSC 5 SSC;  CXC Gen, GCE 'O' 1-2 ;  CXC Gen, GCE 'O' 3-4;  CXC Gen, GCE 'O' 5+
Estos cuatro exámenes corresponden a los Caribbean Examination Council's O'Level (Nivel Ordinario) school leaving
examinations - Basic or General Proficiency levels. Los cuales están calificados del 1 al 6, en donde 1=
pasar con distinción, 2= pasar con créditos , 3= pasar con nivel satisfactorio, 4 o más = 'basic level' pass.

Al finalizar el grado 11 pueden optar por extender su educación secundaria hasta dos años más. Esta extensión
se denomina 'Sixth Form', la misma que se divide en upper sixth (grado 13) y lower sixth (grado 12).
Esta extensión también se conoce como 'advanced post secondary program', al final del cual se debe rendir los 
exámenes CAPE (Caribbean Advanced Proficiency Exams), los mismos que equivalen a los GCE (General Certificate
Education) A-level examinations que eran estándar hasta el año 2003.

* La educación terciaria completa sólo puede ser alcanzada en la universidad y corresponde a los que declaran 
´degree´ en la pregunta: What is the highest academic examination that you have / has passed.

Además, esta base considera sólo a las personas mayores de 15 años.
__________________________________________________________________________________________________________
*/

*ocupados
gen aedu_ci=.
replace aedu_ci=q320a if q320a<99
replace aedu_ci=q321a if q321a>0 & q321a<99

*desocupados
replace aedu_ci=q421a if q421a<99
replace aedu_ci=q422a if q422a>0 & q422a<99

*inactivos
replace aedu_ci=q514a if q514a<99
replace aedu_ci=q515a if q515a>0 & q515a<99
label var aedu_ci "Años de educación"

gen exam = .
replace exam = 1 if  (q322==1) | (q423==1) | (q516 ==1)
replace exam = 2 if  (q322==2) | (q423==2) | (q516 ==2)
replace exam = 3 if  (q322==3) | (q423==3) | (q516 ==3)
replace exam = 4 if  (q322==4) | (q423==4) | (q516 ==4)
replace exam = 5 if  (q322==5) | (q423==5) | (q516 ==5)
replace exam = 6 if  (q322==6) | (q423==6) | (q516 ==6)
replace exam = 7 if  (q322==7) | (q423==7) | (q516 ==7)
replace exam = 8 if  (q322==8) | (q423==8) | (q516 ==8)
replace exam = 9 if  (q322==9) | (q423==9) | (q516 ==9)
replace exam = 99 if  (q322==99) | (q423==99) | (q516 ==99)
recode exam (99=.)
 
label define exam 1 "none" 2 "CXC basic, JSC 5 SSC" 3 "CXC Gen, GCE 'O' 1-2" 4 "CXC Gen, GCE 'O' 3-4" ///  
  5 "CXC Gen, GCE 'O' 5+" 6 "GCE 'A' 1-2label defi" 7 "CAPElabel define GCE" 8 "Degree" 9 "other" ///
  99 "not stated"
label value exam exam
label var exam "Highest (academic) examination"


**********
*eduno_ci*
**********

gen eduno_ci=1 if aedu_ci==0
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.
label variable eduno_ci "Cero anios de educacion"

**********
*edupi_ci*
**********
gen edupi_ci=0 if aedu_ci!=.
replace edupi_ci=1 if (aedu_ci >=1 & aedu_ci<6) 
label variable edupi_ci "Primaria incompleta"


**********
*edupc_ci*
**********
gen edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if edupc_ci==. & aedu_ci != .
label variable edupc_ci "Primaria completa"

**********
*edusi_ci*
**********
gen edusi_ci=0 if aedu_ci!=.
replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<11)
label variable edusi_ci "Secundaria incompleta"

**********
*edusc_ci*
**********
/*
Para el caso de Jamaica, tendrán secundaria completa los que tengan más de once años de educación
pero que NO declaren tener título universitario (serían los de terciaria completa). Además. que
hayan rendido los exámenes de culminación de secundaria ya sea cualquiera de las ordinarias CXC gen, GCE 'O'
o las avanzadas GCE 'A' , CAPE. Por otro lado, tendrán cero los que tengan menos de once años de educación.

A PESAR DE QUE ESTA SINTAXIS ES MAS PRECISA, AL HACER UN TAB NO COINCIDE CON EL NÙMERO DE CASOS DE LA VARIABLE
DE AÑOS DE ESCOLARIDAD.

gen edusc_ci=1 if ((aedu_ci>=11)&(exam!=8))& aedu_ci!=. 
replace edusc_ci=1 if (exam >=2 & exam<=7) & ((aedu_ci>=11)& aedu_ci!=.) & edusc_ci !=1
replace edusc_ci=0 if (aedu_ci<11) & (edusc_ci == .)
label variable edusc_ci "Secundaria completa"
*/

gen edusc_ci=1 if aedu_ci>=11 & aedu_ci!=.
replace edusc_ci=0 if aedu_ci<11
label variable edusc_ci "Secundaria completa"

**********
*edus1i_ci*
***********
gen edus1i_ci=0 if aedu_ci!=.
replace edus1i_ci=1 if (aedu_ci>=7 & aedu_ci<9)& aedu_ci!=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***********
*edus1c_ci*
***********
gen edus1c_ci=0 if aedu_ci!=.
replace edus1c_ci=1 if aedu_ci==9 & aedu_ci!=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***********
*edus2i_ci*
***********
gen edus2i_ci=0 if aedu_ci!=.
replace edus2i_ci=1 if aedu_ci==10 
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***********
*edus2c_ci*
***********
gen edus2c_ci=0 if aedu_ci!=.
replace edus2c_ci=1 if aedu_ci>=11 & aedu_ci!=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**********
*eduui_ci*
**********

gen eduui_ci=.
label variable eduui_ci "Superior incompleto"
**********
*eduuc_ci*
**********
/* Se podría considerar a los que respondieron degree, sin embargo, al evaluar esta variable con ese criterio se 
evidencian individuos que no acaban ni la secundaria y responden degree.

gen eduuc_ci=0 if aedu_ci!=.
replace eduuc_ci=1 if exam == 8
label variable eduuc_ci "Superior completo"
*/

gen eduuc_ci=.
label variable eduuc_ci "Superior completo"


************************
***Educacion preescolar.
************************
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************************************************************************
***Educación terciaria académica versus educación terciaria no-académica***
***************************************************************************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************************************************************************
***Personas que actualmente asisten a centros de enseñanza.
***************************************************************************
gen asiste_ci=1 if q21a==5
replace asiste_ci=0 if asiste_ci==.
label variable asiste_ci "Asiste actualmente a la escuela"

***************************************************************************
***Razones para no asistir a la escuela.***
***************************************************************************
gen pqnoasis=.
label variable pqnoasis  " Razón por que no asiste a la escuela"


******************************************************
*Personas que han repetido al menos un año o grado.***
******************************************************

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado o año"

******************************************************
***Personas que han repetido el ultimo grado.
******************************************************

gen repiteult_ci=.
label var repite_ci "Personas que han repetido el último grado"

********************************************************
***Personas que asisten a centros de enseñanza publicos.
********************************************************
gen edupub_ci=.
label var edupub_ci "Personas asisten a centros de enseñanza públicos"



save "X:\ARM\JAM\2007\LFS\Arm data\jam2007ea_bid.dta", replace

/*sólo es posible determinar los anios de educación hasta finalizar la secundaria. posteriormente se pregunta sobre 
los exámenes cxc pero no es posible determinar el nivel educativo con estos


fin encuesta lfs



*/
**Verificación de que se encuentren todas las variables
qui desc factor_ch	idh_ch	idp_c	zona_c	pais_c	anio_c	mes_c	relacion_ci	factor_ci	sexo_ci	edad_ci	civil_ci	///
jefe_ci	nconyuges_ch	nhijos_ch	notropari_ch	notronopari_ch	nempdom_ch	clasehog_ch	nmiembros_ch	///
miembros_ci	nmayor21_ch	nmenor21_ch	nmayor65_ch	nmenor6_ch	nmenor1_ch	ocupa_ci	rama_ci	horaspri_ci	///
horastot_ci	ylmpri_ci	ylnmpri_ci	ylmsec_ci	ylnmsec_ci	ylmotros_ci	ylnmotros_ci	nrylmpri_ci	tcylmpri_ci ///
ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci	nrylmpri_ch	tcylmpri_ch	ylm_ch	ylnm_ch	ylmnr_ch	ynlm_ch	ynlnm_ch	///
ylmhopri_ci	ylmho_ci	rentaimp_ch	autocons_ci	autocons_ch	remesas_ci	remesas_ch	durades_ci	antiguedad_ci ///
emp_ci	desemp1_ci	desemp2_ci	desemp3_ci	pea1_ci	pea2_ci	pea3_ci	desalent_ci	subemp_ci	tiempoparc_ci ///
categopri_ci	categosec_ci	contrato_ci	segsoc_ci	nempleos_ci	firmapeq_ci	spublico_ci	aedu_ci	eduno_ci ///
edupi_ci	edupc_ci	edusi_ci	edusc_ci	eduui_ci	eduuc_ci	edus1i_ci	edus1c_ci	edus2i_ci ///
edus2c_ci	edupre_ci	eduac_ci	asiste_ci	pqnoasis	repite_ci	repiteult_ci	edupub_ci	

/*Estas variables son del módulo de vivienda que no está contemplado en esta encuesta.
aguared_ch	aguadist_ch	aguamala_ch	aguamide_ch	luz_ch	luzmide_ch	combust_ch	bano_ch	banoex_ch	///
des1_ch	des2_ch	piso_ch	pared_ch	techo_ch	resid_ch	dorm_ch	cuartos_ch	cocina_ch	telef_ch ///
refrig_ch	freez_ch	auto_ch	compu_ch	internet_ch	cel_ch	vivi1_ch	vivi2_ch	viviprop_ch	///
vivitit_ch	vivialq_ch	vivialqimp_ch






















































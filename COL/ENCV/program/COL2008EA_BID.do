* Abril, 2010
* Yanira Oviedo

		*******************************
		**** ARMONIZACION ECV 2008 **** 
		*******************************

clear
capture log close
set memory 500m
set more off

local in="X:\ARM\COL\ECV\2008\"
capture log close
log using "`in'Documents\COL2008EA_BID.log", replace
use "`in'Origin_data\col08_ecv.dta"


***************
***factor_ch***
***************

gen factor_ch= factor_expansion
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

gen idh_ch = llave_h
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=orden
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if p3==2 | p3==3
replace zona_c=1 if p3==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0"Rural"
label value zona_c zona_c


************
****pais****
************

gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2008
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=.
label variable mes_c "Mes de la encuesta"


*****************
***relacion_ci***
*****************

gen relacion_ci=1 if p6051==1
replace relacion_ci=2 if p6051==2
replace relacion_ci=3 if p6051==3 | p6051==4
replace relacion_ci=4 if p6051>=5 & p6051<=16
replace relacion_ci=5 if p6051>=18 & p6051<=21
replace relacion_ci=6 if p6051==17

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
*NOTA: para este año la encuesta generó solo un factor de expansión por hogar.
*luego todos los individuos del hogar tienen el mismo factor de expansión.

gen factor_ci=factor_expansion
label variable factor_ci "Factor de expansion del individuo"



**********
***sexo***
**********

gen sexo_ci=p6020

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=p6040
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if p8548==5
replace civil_ci=2 if p8548==1 | p8548==2
replace civil_ci=3 if p8548==4 
replace civil_ci=4 if p8548==3

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


**************
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

************
***emp_ci***
************
gen byte emp_ci=0
replace emp_ci=1 if p6370s1!=.
label var emp_ci "Ocupado (empleado)"


****************
***desemp1_ci***
****************
gen desemp1_ci=0
replace desemp1_ci=1 if p6240==2 & p6280==1
label var desemp1_ci "Desempleado que buscó empleo en el periodo de referencia"


****************
***desemp2_ci*** 
****************
gen desemp2_ci=desemp1_ci
replace desemp2_ci=1 if p8618==3 | p8618==7
label var desemp2_ci "desemp1_ci + personas que esperan respuesta a solicitud o temporada alta"


****************
***desemp3_ci***
****************
gen byte desemp3_ci=desemp2_ci 
replace desemp3_ci=1 if p6340==1
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"


*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1
label var pea1_ci "Población Económicamente Activa con desemp1_ci"

*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1
label var pea2_ci "Población Económicamente Activa con desemp2_ci"

*************
***pea3_ci***
*************
gen pea3_ci=0
replace pea3_ci=1 if emp_ci==1 |desemp3_ci==1
label var pea3_ci "Población Económicamente Activa con desemp3_ci"

*****************
***desalent_ci***
*****************

gen desalent_ci=(p8618==5)
replace desalent_ci=. if p8618==.
label var desalent_ci "Trabajadores desalentados"


***************
***subemp_ci***
***************

gen subemp_ci=0
replace subemp_ci=1 if p6800<=30  & emp_ci==1 & p6810==1
replace subemp_ci =. if emp_ci ==.
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************

gen horaspri_ci=p6800 
replace horaspri_ci=. if p6800==99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************

*NOTA: en la ecv no esta disponible las horas dedicadas al segundo trabajo
gen horastot_ci=horaspri_ci if emp_ci==1 
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"


*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(horastot_ci<=30 & p6810!=1 | p6810!=.)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if p6435==6
replace categopri_ci=2 if p6435==4 | p6435==5 |p6435==7
replace categopri_ci=3 if p6435==1 | p6435==2 |p6435==3 
replace categopri_ci=4 if p6435==8 | p6435==9 |p6435==10
replace categopri_ci=0 if p6435==11
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************

gen categosec_ci=.
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***contrato_ci***
*****************

gen contrato_ci=.
replace contrato_ci = 1 if p6450 == 2
replace contrato_ci = 0 if p6450 == 1
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"


***************
***segsoc_ci***
***************


gen segsoc_ci=.
replace segsoc_ci = 1 if p6920==1 
replace segsoc_ci = 0 if p6920!=1 & emp_ci==1
label var segsoc_ci "Personas que tienen seguridad social en pensiones por su trabajo"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & p8636==2
replace nempleos_ci=2 if emp_ci==1 & p8636==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 

*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if p8632==1 | p8632==2 |p8632==3
replace firmapeq_ci=0 if p8632>=4 & p8632!=.
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"

*****************
***spublico_ci***
*****************

gen spublico_ci=(p6435==2) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (p6370s1>=1 & p6370s1<=19) & emp_ci==1  
replace ocupa_ci=2 if (p6370s1>=20 & p6370s1<=21) & emp_ci==1
replace ocupa_ci=3 if (p6370s1>=30 & p6370s1<=39) & emp_ci==1
replace ocupa_ci=4 if (p6370s1>=40 & p6370s1<=49) & emp_ci==1
replace ocupa_ci=5 if (p6370s1>=50 & p6370s1<=59) & emp_ci==1
replace ocupa_ci=6 if (p6370s1>=60 & p6370s1<=64) & emp_ci==1
replace ocupa_ci=7 if (p6370s1>=70 & p6370s1<=98) & emp_ci==1
replace ocupa_ci=9 if (p6370s1==00 | p6370s1==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

label define ocupa_ci  1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (p6390s1>=01 & p6390s1<=05) & emp_ci==1   
replace rama_ci=2 if (p6390s1>=10 & p6390s1<=14) & emp_ci==1
replace rama_ci=3 if (p6390s1>=15 & p6390s1<=37) & emp_ci==1
replace rama_ci=4 if (p6390s1>=40 & p6390s1<=41) & emp_ci==1
replace rama_ci=5 if (p6390s1==45) & emp_ci==1
replace rama_ci=6 if (p6390s1>=50 & p6390s1<=55) & emp_ci==1
replace rama_ci=7 if (p6390s1>=60 & p6390s1<=64) & emp_ci==1
replace rama_ci=8 if (p6390s1>=65 & p6390s1<=74) & emp_ci==1
replace rama_ci=9 if (p6390s1>=75 & p6390s1<=99) & emp_ci==1
replace rama_ci=. if emp_ci==0

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


****************
***durades_ci***
****************

gen durades_ci= p7250/4.2
replace durades_ci=62 if p7250==998
replace durades_ci=. if p7250==999
label variable durades_ci "Duracion del desempleo en meses"
 
*******************
***antiguedad_ci***
*******************

gen antiguedad_ci= p6426/12 
replace antiguedad_ci=. if emp_ci==0 | p6426==999
label var antiguedad_ci "Antiguedad en la actividad actual en anios"



************************************************************************
**************************INGRESOS**************************************
************************************************************************


***************
***ylmpri_ci***
***************

gen ymensual 	= p8624 if p8620!=98 & p8620!=99
gen yvivienda 	= p6605s1 if p6605s1!=98 & p6605s1!=99 
gen yalimen 	= p6595s1 if p6595s1!=98 & p6595s1!=99
gen ytrans 	= p6615s1 if p6615s1!=98 & p6615s1!=99
gen yespecie 	= p6623s1 if p6623s1!=98 & p6623s1!=99
gen ysubsi1	= p8626s1 if p8626s1!=98 & p8626s1!=99
gen ysubsi2	= p8628s1 if p8628s1!=98 & p8628s1!=99
gen ysubsi3	= p8630s1 if p8630s1!=98 & p8630s1!=99
gen yprimtec	= p8631s1 if p8631s1!=98 & p8631s1!=99
gen yprimser	= p6630s1a1/12 if p6630s1a1!=98 & p6630s1a1!=99
gen yprimnav 	= p6630s2a1/12 if p6630s2a1!=98 & p6630s2a1!=99
gen yprimvac	= p6630s3a1/12 if p6630s3a1!=98 & p6630s3a1!=99
gen yprimbono	= p6630s5a1/12 if p6630s5a1!=98 & p6630s5a1!=99
gen yprimacc  = p6630s6a1/12  if p6630s6a1!=98 & p6630s6a1!=99

gen ynetoind 	= p6750
gen ycosecha 	= p550/12
gen ysecund	= p8636s1
gen ypension	= p8642s1
gen yarrien	= p8646s1
gen yjubila	= p8648s1/12
gen yayudafam	= p8650s1/12
gen ycesantia	= p8654s1/12
gen yremesas	= .




egen ylmpri_ci=rsum(ymensual yprim* ynetoind ycosecha)
replace ylmpri_ci=. if ymensual == . & ycosecha==. & ynetoind==. & yprimtec==.  & yprimser==. & yprimnav==. & yprimva==. & yprimbono==. & yprimacc ==. 
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


****************
***ylnmpri_ci***
****************

egen ylnmpri_ci=rsum(yvivienda yalimen ytrans yespecie ysubsi1 ysubsi2 ysubsi3)
replace ylnmpri_ci=. if yvivienda==. & yalimen==. & ytrans==. & yespecie==. & ysubsi1==. & ysubsi2==. & ysubsi3==. 
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

gen ylmsec_ci=ysecund if emp_ci==1 
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

gen ylm_ci= ylmpri_ci 
replace ylm_ci=. if ylmpri_ci==. 
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(yayudafam yremesas ycesantia)
replace ynlm_ci=. if yayudafam==. & yremesas==. & ycesantia==.
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ylnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 



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
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembro_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembro_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembro_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembro_ci==1
label var ynlm_ch "Ingreso no laboral monetario del hogar"


*************
***ynlnm_ch***
*************

gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"


****************
***remesas_ci***
****************

gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 


****************
***remesas_ch***
****************

gen remesas_ch=.
label var remesas_ch "Remesas mensuales del hogar" 

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 



****************************
***VARIABLES DE EDUCACION***
****************************

*En esta encuesta esta por separado el nivel y grado de quien esta estudiando y el nivel y el 
*grado del resto de la gente

gen byte aedu_ci=.

replace aedu_ci=0 if p6219==1 & p6219s1==0 
replace aedu_ci=0 if p6219==2 & p6219s1==0 
replace aedu_ci=0 if p6219==3 & p6219s1==0 

*Primaria
replace aedu_ci=1 if p6219==2 & p6219s1==1 
replace aedu_ci=1 if p6219==3 & p6219s1==1
replace aedu_ci=2 if p6219==3 & p6219s1==2
replace aedu_ci=3 if p6219==3 & p6219s1==3
replace aedu_ci=4 if p6219==3 & p6219s1==4
replace aedu_ci=5 if p6219==3 & p6219s1==5
replace aedu_ci=5 if p6219==4 & p6219s1==0
**
replace aedu_ci=1 if p6213==1 & p6213s1==1 
replace aedu_ci=1 if p6213==2 & p6213s1==1
replace aedu_ci=2 if p6213==1 & p6213s1==2
replace aedu_ci=2 if p6213==2 & p6213s1==2
replace aedu_ci=3 if p6213==1 & p6213s1==3
replace aedu_ci=3 if p6213==2 & p6213s1==3
replace aedu_ci=4 if p6213==2 & p6213s1==4
replace aedu_ci=5 if p6213==2 & p6213s1==5

*Secundaria
replace aedu_ci=6 if p6219==4 & p6219s1==6
replace aedu_ci=7 if p6219==4 & p6219s1==7
replace aedu_ci=8 if p6219==4 & p6219s1==8
replace aedu_ci=9 if p6219==4 & p6219s1==9
replace aedu_ci=10 if p6219==4 & p6219s1==10
replace aedu_ci=11 if p6219==4 & p6219s1==11
replace aedu_ci=11 if p6219==4 & p6219s1==0
replace aedu_ci=12 if p6219==4 & p6219s1==12
replace aedu_ci=13 if p6219==4 & p6219s1==13
**
replace aedu_ci=6 if p6213==3 & p6213s1==6
replace aedu_ci=7 if p6213==3 & p6213s1==7
replace aedu_ci=8 if p6213==3 & p6213s1==8
replace aedu_ci=9 if p6213==3 & p6213s1==9
replace aedu_ci=10 if p6213==3 & p6213s1==10
replace aedu_ci=11 if p6213==3 & p6213s1==11
replace aedu_ci=12 if p6213==3 & p6213s1==12
replace aedu_ci=13 if p6213==3 & p6213s1==13

*Superior o universitaria
gen temp1=p6219s1/2
replace aedu_ci=12.5 if p6219>=5 & p6219<=8 & temp1==0.5
replace aedu_ci=13   if p6219>=5 & p6219<=8 & temp1==1
replace aedu_ci=13.5 if p6219>=5 & p6219<=8 & temp1==1.5
replace aedu_ci=14   if p6219>=5 & p6219<=8 & temp1==2
replace aedu_ci=14.5 if p6219>=5 & p6219<=8 & temp1==2.5
replace aedu_ci=15   if p6219>=5 & p6219<=8 & temp1==3
replace aedu_ci=15.5 if p6219>=5 & p6219<=8 & temp1==3.5
replace aedu_ci=16   if p6219>=5 & p6219<=8 & temp1==4
replace aedu_ci=16.5 if p6219>=5 & p6219<=8 & temp1==4.5
replace aedu_ci=17   if p6219>=5 & p6219<=8 & temp1==5
replace aedu_ci=17.5 if p6219>=5 & p6219<=8 & temp1==5.5
replace aedu_ci=18   if p6219>=5 & p6219<=8 & temp1==6
replace aedu_ci=18.5 if p6219>=5 & p6219<=8 & temp1==6.5
**
gen temp2=p6213s1/2
replace aedu_ci=12.5 if p6213>=4 & p6213<=6 & temp2==0.5
replace aedu_ci=13   if p6213>=4 & p6213<=6 & temp2==1
replace aedu_ci=13.5 if p6213>=4 & p6213<=6 & temp2==1.5
replace aedu_ci=14   if p6213>=4 & p6213<=6 & temp2==2
replace aedu_ci=14.5 if p6213>=4 & p6213<=6 & temp2==2.5
replace aedu_ci=15   if p6213>=4 & p6213<=6 & temp2==3
replace aedu_ci=15.5 if p6213>=4 & p6213<=6 & temp2==3.5
replace aedu_ci=16   if p6213>=4 & p6213<=6 & temp2==4
replace aedu_ci=16.5 if p6213>=4 & p6213<=6 & temp2==4.5
replace aedu_ci=17   if p6213>=4 & p6213<=6 & temp2==5
replace aedu_ci=17.5 if p6213>=4 & p6213<=6 & temp2==5.5
replace aedu_ci=18   if p6213>=4 & p6213<=6 & temp2==6
replace aedu_ci=18.5 if p6213>=4 & p6213<=6 & temp2==6.5

*Posgrado
replace aedu_ci=16.5 if p6219>=9 & p6219<=10 & temp1==0.5
replace aedu_ci=17   if p6219>=9 & p6219<=10 & temp1==1
replace aedu_ci=17.5 if p6219>=9 & p6219<=10 & temp1==1.5
replace aedu_ci=18   if p6219>=9 & p6219<=10 & temp1==2
replace aedu_ci=18.5 if p6219>=9 & p6219<=10 & temp1==2.5
drop temp* 
label var aedu_ci "Anios de educacion aprobados" 


**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if p6219==1 | p6219==2 | aedu_ci==0
label variable eduno_ci "Sin educacion"


**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<5) 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==5 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=6 & aedu_ci<11) 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if (aedu_ci>=11 & aedu_ci<=13) & (p6219==4 | p6213==3)
label variable edusc_ci "Secundaria completa"


**************
***eduui_ci***
**************

*Para la educación superior no es posible saber cuantos anios dura el ciclo normalmente
*por ello se hace una aproximación a través de titulación. En esta encuesta si se puede saber
*pero se mantiene el procedimiento para mantener la comparabilidad

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<15 & (p6219==7 | p6213==6 )
label variable eduui_ci "Superior incompleto"


***************
***eduuc_ci***
***************

*Para la educación superior no es posible saber cuantos anios dura el ciclo
*por ello se hace una aproximación a través de titulación
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>11 & (p6219==5 | p6219==6 | p6219==8 | p6219==9 | p6219==10 | p6213==7)
label variable eduuc_ci "Superior completo"


***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>=6 & aedu_ci<9)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci==10 
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if (aedu_ci>=11 & aedu_ci<=13) & (p6219==4 | p6213==3)
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if p8586==1
replace asiste_ci=0 if p8586==2
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis=.
label var pqnoasis "Razones para no asistir a la escuela"

***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************

gen edupub_ci=0 if p8592==2
replace edupub_ci=1 if p8592==1 
label var edupub_ci "Asiste a un centro de ensenanza público"




**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************


****************
***aguared_ch***
****************

gen aguared_ch=.
replace aguared_ch=(p8520s5==1)
label var aguared_ch "Acceso a fuente de agua por red"


*****************
***aguadist_ch***
*****************

gen aguadist_ch=.
replace aguadist_ch=p5060
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************

gen aguamala_ch=(p8530==5 | p8530==6)
replace aguamala_ch=. if p8530==.
label var aguamala_ch "Agua unimproved según MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch=0
replace luz_ch=1 if p8520s1==1 
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch=0
replace combust_ch=1 if  p8536==1 | p8536==2 | p8536==4
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

gen bano_ch=1
replace bano_ch=0 if p8526==6 
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=0
replace banoex_ch=1 if p5030==1
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if p8526==1 | p8526==2
replace des1_ch=2 if p8526==3 | p8526==4
replace des1_ch=3 if p8526==5
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if p8526==1 | p8526==2 | p8526==3 | p8526==4
replace des2_ch=3 if p8526==5
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

gen piso_ch=1 if p4015!=7 & p4015!=.
replace piso_ch=0 if p4015==7
replace piso_ch=. if p4015==.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

**************
***pared_ch***
**************

gen pared_ch=0 
replace pared_ch=1 if p4005==1 | p4005==2 | p4005==3 | p4005==5 | p4005==6
replace pared_ch=. if p4005==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch


**************
***techo_ch***
**************

gen techo_ch=.
label var techo_ch "Materiales de construcción del techo"

**************
***resid_ch***
**************

gen resid_ch =0    if p5041==1 | p5041==6
replace resid_ch=1 if p5041==4 | p5041==5
replace resid_ch=2 if p5041==2 | p5041==3
replace resid_ch=. if p5041==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

 
*************
***dorm_ch***
*************

gen dorm_ch=p5010
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=p5000
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************

gen cocina_ch=0 if p8532>=2 & p8532<=6
replace cocina_ch=1 if p8532==1
replace cocina_ch=. if p8532==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=0
replace telef_ch=1 if p5305==1
replace telef_ch=. if p5305==.
label var telef_ch "El hogar tiene servicio telefónico fijo"


***************
***refrig_ch***
***************

gen refrig_ch=0
replace refrig_ch=1 if  p5210s5==1
replace refrig_ch=. if  p5210s5==.
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"


*************
***auto_ch***
*************

gen auto_ch=0
replace auto_ch=1 if p5210s22==1
replace auto_ch=. if p5210s22==.
label var auto_ch "El hogar posee automovil particular"


**************
***compu_ch***
**************

gen compu_ch=0
replace compu_ch=1 if p5210s16==1
replace compu_ch=. if p5210s16==.
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************

gen internet_ch=0
replace internet_ch=1 if p5210s3==1
replace internet_ch=3 if p5210s3==.
label var internet_ch "El hogar posee conexión a Internet"


************
***cel_ch***
************

gen cel_ch=0
replace cel_ch=1 if p8544==1
replace cel_ch=. if p8544==.
label var cel_ch "El hogar tiene servicio telefonico celular"

**************
***vivi1_ch***
**************

gen vivi1_ch=1 if p4000==1
replace vivi1_ch=2 if p4000==2
replace vivi1_ch=3 if p4000==3 | p4000==4 | p4000==5 | p4000==6
replace vivi1_ch=. if p4000==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


**************
***vivi2_ch***
**************

gen vivi2_ch=0
replace vivi2_ch=1 if p4000==1 | p4000==2
replace vivi2_ch=. if p4000==.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************

gen viviprop_ch=0 if p5095==3
replace viviprop_ch=1 if p5095==1 | p5095==2
replace viviprop_ch=3 if p5095==4 |p5095==5 
replace viviprop_ch=. if p5095==.
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch


****************
***vivitit_ch***
****************

gen vivitit_ch=.
replace vivitit_ch=1 if p5120==1
replace vivitit_ch=0 if p5120==2
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch=p5140 if p5140>=10000
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=p5130 if p5130>=10000
label var vivialqimp_ch "Alquiler mensual imputado"

compress

*drop p6016-p200
*drop p5000-p8862s9
*drop p3-p70


save "`in'Arm_data\COL2008EA_BID.dta", replace
log close



* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


global ruta = "${surveysFolder}"

local PAIS JAM
local ENCUESTA LFS
local ANO "1997"
local ronda m4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Jamaica
Encuesta: LFS
Round: Abril, 1997
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
****************************************************************************/


use `base_in', clear

**Inclusión Mayra Sáenz - Julio 2013-  cambiar los nombres a minúsculas
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}


***********
* Region_c *
************
*Modificación Mayra Sáenz - Julio 2013
gen region_c=  par

label define region_c  ///
           1 "Kingston" ///
           2 "St Andrew" ///
           3 "St Thomas" ///
           4 "Portland" ///
           5 "St Mary" ///
           6 "St Ann" ///
           7 "Trelawny" ///
           8 "St James" ///
           9 "Hanover" ///
          10 "Westmoreland" ///
          11 "St Elizabeth" ///
          12 "Manchester" ///
          13 "Clarendon" ///
          14 "St Catherine"
	    
label value region_c region_c
label var region_c "División política, parroquias"

***********
* INE01 *
************
gen ine01=  par

label define ine01  ///
           1 "Kingston" ///
           2 "St Andrew" ///
           3 "St Thomas" ///
           4 "Portland" ///
           5 "St Mary" ///
           6 "St Ann" ///
           7 "Trelawny" ///
           8 "St James" ///
           9 "Hanover" ///
          10 "Westmoreland" ///
          11 "St Elizabeth" ///
          12 "Manchester" ///
          13 "Clarendon" ///
          14 "St Catherine"


**************
* Región BID *
**************
gen region_BID_c=.
replace region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*************************
*  factor de expansión  *
*************************
destring rfact, replace

/* Modificado Mayra Sàenz Agosto 2013. Con este factor de expansión la población llega a  253440167498 individuos, cuando la población de 
Jamaica al año 1997 fue de 2531182 habitantes (PennWorldTable 7.1, 2013).
Por lo tanto es necesario cambiar las ponderaciones de la misma manera que se trabajó en el año 2002. 
*/
** POBLACION DA JAMAICA AL 1997 = 2531182 **

sum rfact
scalar pob=r(sum)
gen pop=rfact*(2531182/pob)
sum pop
ret list
gen factor_ch=pop 
drop pop
label var factor_ch "Factor de expansion del hogar"


**************************
* identificador del hogar*
**************************

ren  hh idh_ch

****************************
* identificador de persona *
****************************

ren id idp_ci

*************** 
******upm_ci*****
***************
* JAM 2001 no tiene UPM
gen upm_ci=. 

*************
* estrato_ci*
**************
gen estrato_ci=.


***************************
* zona urbana o zona rural*
***************************

gen zona_c=.

**********************************
* país***************************

gen pais_c="JAM"


****************************
* anio de la encuesta ******

gen anio_c=1997

***********************
*  mes de la encuesta *
***********************

gen mes_c=4


*******************************
*relación con el jefe de hogar*
*******************************


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
destring relat, replace
gen relacion_ci=.
replace relacion_ci=1 if relat==1
replace relacion_ci=2 if relat==2
replace relacion_ci=3 if relat==3
replace relacion_ci=4 if relat==4 |relat==5 | relat==6 | relat==7 
replace relacion_ci=5 if relat==9 
replace relacion_ci=6 if relat==8

****************************************
*factor de expansión a nivel individual*
****************************************

gen factor_ci=factor_ch

********
* sexo *
********
destring sex, replace
ren sex sexo_ci

******
*edad*
******
destring age, replace
ren age edad_ci

**************
*estado civil*
**************

gen civil_ci=.



***************
*jefe de hogar*
***************

gen jefe_ci=(relacion_ci==1)



******************
***nconyuges_ch***
******************

by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "numero de conyuges"

***************
***nhijos_ch***
***************

by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "numero de hijos"

******************
***notropari_ch***
******************

by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "numero de otros familiares"

********************
***notronopari_ch***
********************

by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "numero de no familiares"


****************
***nempdom_ch***
****************

by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "numero de empleados domesticos"

*****************
***clasehog_ch***
*****************

gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
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
label variable nmiembros_ch "numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "miembro del hogar"

 ******************************
          *** VARIABLES DE DIVERSIDAD **
          ******************************
*Nathalia Maya & Antonella Pereira
*Julio 2021	

	
	
	***************
	***afroind_ci***
	***************
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 


***************************************
*ocupación laboral actividad principal*
***************************************
gen ocupa=string(q38m)
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=. if ocupa_ci==0
drop ocupa

**********************************
*rama laboral actividad principal*
**********************************

gen rama1=string(q39m)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 
drop rama1



****************************************************
*horas totales trabajadas en la actividad principal*
****************************************************

/*sólo se pregunta cuantas horas se trabaja usualmente a la semana. no se pregunta por
actividad principal sino por todo. no es posible identificar las horas dedicadas a la actividad principal*/
gen horaspri_ci=.

**********************************************************
***horas totales trabajadas en la actividad secundaria.***
**********************************************************
* no distingue actividad secundaria
gen horassec_ci=.
label var horassec_ci "horas trabajadas semanalmente en el trabajo secundario"

***************************************************
*horas totales trabajadas en todas las actividades*
***************************************************

gen horastot_ci=q33
replace horastot_ci=. if horastot_ci==99

***********************************************
*ingreso laboral monetario actividad principal*
***********************************************

/*no hay manera de encontrar ingreso de actividad principal, secundaria. solamente el total. 
tampoco se pregunta por pago en especie.*/
* A pesar de que en la encuesta el label dice "main activity" en la pregunta del cuestionario
* no se especifica que el ingreso proviene de la actividad principal o de la secundaria. 

gen ylmpri_ci=.

**************************************************
*ingreso laboral no monetario actividad principal*
**************************************************

gen ylnmpri_ci=.


************************************************
*ingreso laboral monetario actividad secundaria*
************************************************

gen ylmsec_ci=.


***************************************************
*ingreso laboral no monetario actividad secundaria*
***************************************************

gen ylnmsec_ci=.

******************************************
*ingreso laboral monetario otros trabajos*
******************************************

gen ylmotros_ci=.


*********************************************
*ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*********************************************************************
*identificador de no respuesta del ingreso de la actividad principal*
*********************************************************************

gen nrylmpri_ci=.

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*****************************************
*ingreso laboral monetario mensual total*
*****************************************

gen ylm_ci=.
replace ylm_ci=q325e if q325a==4
replace ylm_ci=q325e*4.2 if q325a==1
replace ylm_ci=q325e/12 if q325a==7


************************************
*ingreso laboral no monetario total*
************************************

gen ylnm_ci=.

******************************
*ingreso no laboral monetario*
******************************

gen ynlm_ci=.
replace ynlm_ci=q325o if q325b==4
replace ynlm_ci=q325o*4.2 if q325b==1
replace ynlm_ci=q325o/12 if q325b==7


*******************************************************
*** ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "ingreso no laboral no monetario"


**********************************************************************
*identificador de los hogares en donde alguno de los miembros no sabe*
*	       no responde el ingreso de la actividad principal          *
**********************************************************************

gen nrylmpri_ch=.

**************************************************
*identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.


*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

*************
*ylmhopri_ci*
*************

gen ylmphopri_ci=.

**********
*ylmho_ci*
**********

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)


*************
*rentaimp_ch*
*************

gen rentaimp_ch=.

*************
*autocons_ci*
*************

gen autocons_ci=.

*************
*autocons_ch*
*************

gen autocons_ch=.



************
*remesas_ci*
************

gen remesas_ci=.

************
*remesas_ch*
************

gen remesas_ch=.

*************
*ylmhopri_ci*
*************

gen ylmhopri_ci=.
label var ylmhopri_ci "salario  monetario de la actividad principal"

***********************************************************
*** ingreso no laboral no monetario del hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "ingreso no laboral no monetario del hogar" 

***************
*** ylnm_ch ***
***************

gen ylnm_ch=.
label var ylnm_ch  "ingreso laboral no monetario del hogar"

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
replace durades_ci=(1+4.3)/2 if q41==1
replace durades_ci=(1+3)/2 if q41==2
replace durades_ci=(3+6)/2 if q41==3
replace durades_ci=(6+9)/2 if q41==4
replace durades_ci=(9+12)/2 if q41==5
replace durades_ci=12 if q41==6


***************
*antiguedad_ci*
***************

gen antiguedad_ci=.
replace antiguedad_ci=(1+3)/2 if q311==1
replace antiguedad_ci=(3+6)/2 if q311==2
replace antiguedad_ci=(6+9)/2 if q311==3
replace antiguedad_ci=(9+12)/2 if q311==4
replace antiguedad_ci=(12+24)/2 if q311==5
replace antiguedad_ci=(24+36)/2 if q311==6


************************
*Condición de ocupación*
************************
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
destring q21 q22 q23 q41 q42 q43 q44, replace
gen condocup_ci =.
*Ocupado*
replace condocup_ci=1 if q21==1 | q21==2 |  q22==1  | q23==1 
*Desocupado*
replace condocup_ci=2 if (q21==3 | (q43>=1 & q43<=7) |q44==3 | q44==6 | (q41>=1 & q41<=6) |  (q42 >=1 & q42 <=7)) & condocup_ci==.
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) 
replace condocup_ci =. if q21==. & q22==. & q23==. 
*menores que PET
*replace condocup_ci=4 if edad_ci<14 
replace condocup_ci=4 if edad_ci<10  

label define condocup_ci 1"Ocupados" 2"Desocupados" 3"Inactivos" 4"Menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"

***********
* Cesante *
***********
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if q45==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.

label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	

label var afiliado_ci "Afiliado a la Seguridad Social"
****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 
****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

**************
**ypen_ci*
**************
gen ypen_ci=. /* No preguntan ingresos por esta fuente */
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
***tecnica_ci**
***************
gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"

***************
***pension_ci**
***************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*********
*lp_ci***
*********
gen lp_ci=. 
label var lp_ci "Linea de pobreza local (Adulto Equivalente)"

*********
*lpe_ci***
*********

gen lpe_ci=. 
label var lpe_ci "Linea de pobreza extrema local (Adulto Equivalente)"

*********
*salmm_ci***
*********
gen salmm_ci=.
label var salmm_ci "Salario Minimo Semanal (40 horas) en dolares de Jamaica"

/*
********
*emp_ci*
********

destring q21 q22 q23, replace

gen emp_ci=1 if q21==1 | q21==2 | q22==1  | q23==1 
replace emp_ci =0 if emp_ci==. 

************
*desemp1_ci*
************

gen desemp1_ci=1 if q43==1
replace desemp1_ci=0 if desemp1_ci==.

************
*desemp2_ci*
************

gen desemp2_ci=1 if desemp1_ci==1 | q44==3 | q44==6
replace desemp2_ci=0 if desemp2_ci==.

************
*desemp3_ci*
************

gen desemp3_ci=1 if (q41>=1 & q41<9) | desemp2_ci==1
replace desemp3_ci=0 if desemp3_ci==.
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
gen pea3_ci=1 if emp_ci==1 |desemp3_ci==1
*/

*************
*desalent_ci*
*************

gen desalent_ci=1 if q44==1 | q44==4
replace desalent_ci=0 if desalent_ci==.

***********
*subemp_ci*
***********

gen subemp_ci=1 if q34==1 & q33<30
replace subemp_ci=0 if subemp_ci==.

***************
*tiempoparc_ci*
***************

gen tiempoparc_ci=1 if q33<30 & q34==3
replace tiempoparc_ci=0 if tiempoparc_ci==.

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




**************
*categosec_ci*
**************

gen categosec_ci=.

*************
*contrato_ci*
*************

gen contrato_ci=.

***********
*segsoc_ci*
***********

gen segsoc_ci=.

*************
*nempleos_ci*
*************
* La etiqueta de esta variable está mal en la base, pero sí es la variable correcta

gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if q37>=2 & q37<.
/*
*************
*firmapeq_ci*
*************


gen firmapeq_ci=.
replace firmapeq_ci=1 if q324==1 | q324 ==2
replace firmapeq_ci=0 if (q324>=3 & q324<=5)
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "trabajadores formales"
*/

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

************************
*tamemp_ci*
************************

gen tamemp_ci=1 if q324==1 | q324==2
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if q324==3
*Empresas grandes
replace tamemp_ci=3 if q324==4
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño


************************
*categoinac_ci*
************************

gen categoinac_ci = 1 if (q55 ==7 & condocup_ci==3)
replace categoinac_ci = 2 if (q21 == 5 & condocup_ci==3)
replace categoinac_ci = 3 if (q21 == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"


*******************
***formal***
*******************

* no hay informacion para crear la variable
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"


*********
*aedu_ci*
*********

gen aedu_ci=.
* años de primaria de los ocupados/desocupados/inactivos
replace aedu_ci= 0 if q320 == 1 | q421== 1 | q514==1
replace aedu_ci= 1 if q320 == 2 | q421== 2 | q514==2
replace aedu_ci= 2 if q320 == 3 | q421== 3 | q514==3
replace aedu_ci= 3 if q320 == 4 | q421== 4 | q514==4
replace aedu_ci= 4 if q320 == 5 | q421== 5 | q514==5
replace aedu_ci= 5 if q320 == 6 | q421== 6 | q514==6
replace aedu_ci= 6 if q320 == 7 | q421== 7 | q514==7

* años de secundaria de los ocupados/desocupados/inactivos
replace aedu_ci= 0 if (q321 == 1 | q422== 1 | q515==1)& aedu_ci ==.
replace aedu_ci= 7 if q321 == 2 | q422== 2 | q515==2
replace aedu_ci= 8 if q321 == 3 | q422== 3 | q515==3
replace aedu_ci= 9 if q321 == 4 | q422== 4 | q515==4
replace aedu_ci= 10 if q321 == 5 | q422== 5 | q515==5
replace aedu_ci= 11 if q321 == 6 | q422== 6 | q515==6
replace aedu_ci= 12 if q321 == 7 | q422== 7 | q515==7
replace aedu_ci= 13 if q321 == 8 | q422== 8 | q515==8

**********
*eduno_ci*
**********
gen eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"


**********
*edupi_ci*
**********
gen edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"


**********
*edupc_ci*
**********
gen edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**********
*edusi_ci*
**********
gen edusi_ci=(aedu_ci>6 & aedu_ci<11)
replace edusi_ci=. if aedu_ci==.
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
gen edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"


**********
*edus1i_ci*
***********
gen edus1i_ci=(aedu_ci>=7 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***********
*edus1c_ci*
***********
gen edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***********
*edus2i_ci*
***********
gen edus2i_ci=(aedu_ci==10) 
replace edus2i_ci=. if aedu_ci==.  
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***********
*edus2c_ci*
***********
gen edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"


**********
*eduui_ci*
**********
gen eduui_ci=((aedu_ci>11 & aedu_ci<14) & (q516!=8 & q423!=8 & q322!=8))
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**********
*eduuc_ci*
**********
gen eduuc_ci=(q516==8 | q423==8 | q322==8)
replace eduuc_ci=1 if aedu_ci>=14
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***********
*edupre_ci*
***********
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

************
*asispre_ci*
************
gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

**********
*eduac_ci*
**********
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

*****************
***asiste_ci.
*****************
gen asiste_ci=1 if q21==5
replace asiste_ci=0 if asiste_ci==.
label variable asiste_ci "Asiste actualmente a la escuela"

*************
*pqnoasis_ci*
*************
gen pqnoasis_ci=.
label variable pqnoasis_ci "Razón por que no asiste a la escuela"

**************
*pqnoasis1_ci*
**************
*Daniela Zuluaga-Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz
gen pqnoasis1_ci=.

***********
*repite_ci*
***********
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado o año"

**************
*repiteult_ci*
**************
gen repiteult_ci=.
label var repite_ci "Personas que han repetido el último grado"

***********
*edupub_ci*
***********
gen edupub_ci=.


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
	
	*Esta base de datos no tiene módulo de vivienda.
	
	****************
	***aguared_ch***
	****************
	gen aguared_ch=.
	label var aguared_ch "Acceso a fuente de agua por red"	
	

	*****************
	***aguadist_ch***
	*****************
	gen aguadist_ch=.
	label var aguadist_ch "Ubicación de la principal fuente de agua"
	label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
	label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
	label val aguadist_ch aguadist_ch

	*****************
	***aguamala_ch***
	*****************
	
	gen aguamala_ch=.
	label var aguamala_ch "Agua unimproved según MDG" 
	
	
	*****************
	***aguamide_ch***
	*****************
	gen aguamide_ch=.
	label var aguamide_ch "Usan medidor para pagar consumo de agua"

	************
	***luz_ch***
	************
	gen luz_ch=.
	label var luz_ch "La principal fuente de iluminación es electricidad"	
	
		****************
	***luzmide_ch***
	****************
	gen luzmide_ch=.
	label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

	****************
	***combust_ch***
	****************
	
	gen combust_ch=.
	label var combust_ch "Principal combustible gas o electricidad"	
	
	*************
	***bano_ch***
	*************
	
	gen bano_ch=.
	label var bano_ch "El hogar tiene servicio sanitario"	
	
	***************
	***banoex_ch***
	***************
	gen banoex_ch=.
	label var banoex_ch "El servicio sanitario es exclusivo del hogar"

	*************
	***des1_ch***
	*************
	
	gen des1_ch=.
	label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
	label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
	label val des1_ch des1_ch
		
	*************
	***des2_ch***
	*************
	
	gen des2_ch=.
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
	
	*************
	***piso_ch***
	*************
	gen piso_ch=.
	label var piso_ch "Materiales de construcción del piso"	
	
	**************
	***pared_ch***
	**************
	gen pared_ch=.
	label var pared_ch "Materiales de construcción de las paredes"	
	
	**************
	***techo_ch***
	**************
	
	gen techo_ch=.
	label var techo_ch "Materiales de construcción del techo"	
	
	**************
	***resid_ch***
	**************
	
	gen resid_ch =.
	label var resid_ch "Método de eliminación de residuos"
	label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
	label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
	label val resid_ch resid_ch
	
		**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
      *********************
      ***aguamejorada_ch***
      *********************
      g       aguamejorada_ch = .
		
      *********************
      ***banomejorado_ch***
      *********************
      g       banomejorado_ch = .
	 
	*************
	***dorm_ch***
	*************
	
	gen dorm_ch=.
	label var dorm_ch "Habitaciones para dormir"	
	
	
	****************
	***cuartos_ch***
	****************
	
	gen cuartos_ch=.
	label var cuartos_ch "Habitaciones en el hogar"	
	
	***************
	***cocina_ch***
	***************
	gen cocina_ch=.
	label var cocina_ch "Cuarto separado y exclusivo para cocinar"

	**************
	***telef_ch***
	**************
	gen telef_ch=.
	label var telef_ch "El hogar tiene servicio telefónico fijo"
	
	
	***************
	***refrig_ch***
	***************
	gen refrig_ch=. 
	label var refrig_ch "El hogar posee refrigerador o heladera"
	
		
	**************
	***freez_ch***
	**************
	gen freez_ch=.
	label var freez_ch "El hogar posee congelador"

	*************
	***auto_ch***
	*************
		
	gen auto_ch=.
	label var auto_ch "El hogar posee automovil particular"
		
	**************
	***compu_ch***
	**************
	gen compu_ch=.
	label var compu_ch "El hogar posee computador"
	
	*****************
	***internet_ch***
	*****************
	gen internet_ch=.
	label var internet_ch "El hogar posee conexión a Internet"
	

	************
	***cel_ch***
	************
	
	gen cel_ch=.
	label var cel_ch "El hogar tiene servicio telefonico celular"
	
	**************
	***vivi1_ch***
	**************
		
	gen vivi1_ch=.
	label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
	label val vivi1_ch vivi1_ch
	
	**************
	***vivi2_ch***
	**************
	
	gen vivi2_ch=.
	label var vivi2_ch "La vivienda es casa o departamento"
	
	
	*****************
	***viviprop_ch***
	*****************
	gen viviprop_ch=.
	label var viviprop_ch "Propiedad de la vivienda"
	label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
	label def viviprop_ch 3"Ocupada (propia de facto)", add
	label val viviprop_ch viviprop_ch

	****************
	***vivitit_ch***
	****************
	gen vivitit_ch=.
	label var vivitit_ch "El hogar posee un título de propiedad"

	****************
	***vivialq_ch***
	****************
	gen vivialq_ch=.
	label var vivialq_ch "Alquiler mensual"

	*******************
	***vivialqimp_ch***
	*******************
	gen vivialqimp_ch=.
	label var vivialqimp_ch "Alquiler mensual imputado"
	
	*******************
*** SALUD  ***
*******************

*******************
*** cobsalud_ci ***
*******************

gen cobsalud_ci=.

label var cobsalud_ci "Tiene cobertura de salud"
label define cobsalud_ci 0 "No" 1 "Si" 
label value cobsalud_ci cobsalud_ci

************************
*** tipocobsalud_ci  ***
************************

gen tipocobsalud_ci=.


label var tipocobsalud_ci "Tipo cobertura de salud"
lab def tipocobsalud_ci 0"Sin cobertura" 1"essalud" 2"Privado" 3"entidad prestadora" 4"policiales" 5"sis" 6"universitario" 7"escolar privado" 8"otro" 
lab val tipocobsalud_ci tipocobsalud_ci


*********************
*** probsalud_ci  ***
*********************
* Nota: se pregunta si tuvieron problemas de salud en últimas 4 semanas. 
** En 2001 se preguntó por los últimos 3 meses, entonces no se consideró

gen probsalud_ci=.
label var probsalud_ci "Tuvo algún problema de salud en los ultimos días"
lab def probsalud_ci 0 "No" 1 "Si"
lab val probsalud_ci probsalud_ci


*********************
*** distancia_ci  ***
*********************
gen distancia_ci=.

label var distancia_ci "Dificultad de acceso a salud por distancia"
lab def distancia_ci 0 "No" 1 "Si"
lab val distancia_ci distancia_ci


*****************
*** costo_ci  ***
*****************
gen costo_ci=.

label var costo_ci "Dificultad de acceso a salud por costo"
lab def costo_ci 0 "No" 1 "Si"
lab val costo_ci costo_ci


********************
*** atencion_ci  ***
********************
gen atencion_ci=.

label var atencion_ci "Dificultad de acceso a salud por problemas de atencion"
lab def atencion_ci 0 "No" 1 "Si"
lab val atencion_ci atencion_ci
	
	
	******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	gen migrante_ci=.
	
	**********************
	*** migantiguo5_ci ***
	**********************
	gen migantiguo5_ci=.
	
	**********************
	*** migrantelac_ci ***
	**********************
	gen migrantelac_ci=.
	
	* Variables incluidas por SCL/MIG Juan Camilo Perdomo
	
	**********************
	*** migrantiguo5_ci **
	**********************
	gen migrantiguo5_ci=.
	**********************
	****** miglac_ci *****
	**********************
    gen miglac_ci=.

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close


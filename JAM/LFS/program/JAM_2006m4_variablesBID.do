* (versión stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * se tiene acceso al servidor únicamente al interior del bid.
 * el servidor contiene las bases de datos mecovi.
 *________________________________________________________________________________________________________________*
 


global ruta = "${surveysFolder}"

local pais JAM
local encuesta LFS
local ano "2006"
local ronda m4 

local log_file = "$ruta\harmonized\\`pais'\\`encuesta'\log\\`pais'_`ano'`ronda'_variablesbid.log"
local base_in  = "$ruta\survey\\`pais'\\`encuesta'\\`ano'\\`ronda'\data_orig\\`pais'_`ano'`ronda'.dta"
*local base_in  = "$ruta\survey\\`pais'\\`encuesta'\\`ano'\\`ronda'\data_merge\\`pais'_`ano'`ronda'.dta"
local base_out = "$ruta\harmonized\\`pais'\\`encuesta'\data_arm\\`pais'_`ano'`ronda'_bid.dta"


capture log close
log using "`log_file'", replace 



/***************************************************************************
                 bases de datos de encuesta de hogares - sociometro 
país: jamaica
encuesta: lfs
round: abril, 2006
autores:
versión 2013: mayra sáenz
última versión: mayra sáenz - email: mayras@iadb.org, saenzmayra.a@gmail.com
fecha última modificación: 19 de agosto de 2013

							scl/lmk - iadb
****************************************************************************/
/***************************************************************************
detalle de procesamientos o modificaciones anteriores:
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
****************************************************************************/


use `base_in', clear


* **inclusión mayra sáenz - julio 2013- cambiar los nombres a minúsculas
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}


qui destring _all, replace


*************************
*  factor de expansión  *
*************************

/* modificado mayra sàenz julio 2013. con este factor de expansión la población llega a 266385010286.2968 individuos, cuando la población de 
jamaica al año 2006 fue de 2,759,614 habitantes (pennworldtable 7.1, 2013).
por lo tanto es necesario cambiar las ponderaciones de la misma manera que se trabajó en el año 2002. 
*/
** poblacion da jamaica al 2006 = 2,759,614 **

sum rfact
scalar pob=r(sum)
gen pop=rfact*(2759614/pob)
sum pop
ret list
gen factor_ch=pop 
drop pop
label var factor_ch "factor de expansion del hogar"

* **inclusión mayra sáenz - julio 2013

***********
* region_c *
************
*modificación mayra sáenz - julio 2013
gen region_c=  par

label define region_c  ///
           1 "kingston" ///
           2 "st andrew" ///
           3 "st thomas" ///
           4 "portland" ///
           5 "st mary" ///
           6 "st ann" ///
           7 "trelawny" ///
           8 "st james" ///
           9 "hanover" ///
          10 "westmoreland" ///
          11 "st elizabeth" ///
          12 "manchester" ///
          13 "clarendon" ///
          14 "st catherine"
	    
label value region_c region_c
label var region_c "división política, parroquias"

**************
* Región BID *
**************
gen region_BID_c=.
replace region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

**************************
* identificador del hogar*
**************************

ren  hhid idh_ch

****************************
* identificador de persona *
****************************

ren ind idp_ci


***************************
* zona urbana o zona rural*
***************************

gen zona_c=.
replace zona_c=1 if urcode==1 |  urcode==2
replace zona_c=0 if urcode==3

**********************************
* país***************************

gen pais_c="JAM"


****************************
* anio de la encuesta ******

gen anio_c=2006

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

gen relacion_ci=.
replace relacion_ci=1 if relat==1
replace relacion_ci=2 if relat==2
replace relacion_ci=3 if relat==3
replace relacion_ci=4 if relat==4 | relat==5 | relat==6 | relat==7 
replace relacion_ci=5 if relat==9 
replace relacion_ci=6 if relat==8

****************************************
*factor de expansión a nivel individual*
****************************************

gen factor_ci=factor_ch

********
* sexo *
********

ren sex sexo_ci

******
*edad*
******

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
*Feb 2021	

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



****************************************************
*horas totales trabajadas en la actividad principal*
****************************************************

/*sólo se pregunta cuantas horas se trabaja usualmente a la semana. no se pregunta por
actividad principal sino por todo. no es posible identificar las horas dedicadas a la actividad principal*/
gen horaspri_ci=.


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

gen ylmpri_ci=.
replace ylmpri_ci=q325a if q325ap==4
replace ylmpri_ci=q325a*4.2 if q325ap==1
replace ylmpri_ci=q325a/12 if q325ap==7


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
replace ylm_ci=q325a if q325ap==4
replace ylm_ci=q325a*4.2 if q325ap==1
replace ylm_ci=q325a/12 if q325ap==7

************************************
*ingreso laboral no monetario total*
************************************

gen ylnm_ci=.

******************************
*ingreso no laboral monetario*
******************************

gen ynlm_ci=.
replace ynlm_ci=q325b if q325bp==4
replace ynlm_ci=q325b*4.2 if q325bp==1
replace ynlm_ci=q325b/12 if q325bp==7

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


***********
**ynlm_ch**
***********

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

gen ylnm_ch=.

*************
*ylmhopri_ci*
*************

gen ylmhopri_ci=.

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

***********************************************************
*** ingreso no laboral no monetario del hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "ingreso no laboral no monetario del hogar" 

****************
****condocup_ci*
****************
/*
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
/*
gen condocup_ci=.
replace condocup_ci=1 if empstatus==3
replace condocup_ci=2 if empstatus==4
replace condocup_ci=3 if empstatus==5
replace condocup_ci=4 if edad_ci<14 
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

*/
gen condocup_ci =.
*Ocupado*
replace condocup_ci=1 if q21==1 | q21a==1 | q21a==2 |  q22==1  | q23==1 
*Desocupado*
replace condocup_ci=2 if (q21a==3 | (q43>=1 & q43<=7) |q44==3 | q44==6 | (q41>=1 & q41<=6) |  (q42 >=1 & q42 <=7)) & condocup_ci==.
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) 
*menores que PET
replace condocup_ci=4 if edad_ci<10

/*Ocupado*
replace condocup_ci=1 if q21==1 | q21==2 |  q22==1  | q23==1 
*Desocupado*
replace condocup_ci=2 if (q21==3 | (q43>=1 & q43<=7) |q44==3 | q44==6 | (q41>=1 & q41<=6) |  (q42 >=1 & q42 <=7)) & condocup_ci==.
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) 
*menores que PET
*replace condocup_ci=4 if edad_ci<14 
replace condocup_ci=4 if edad_ci<10 */

label define condocup_ci 1"Ocupados" 2"Desocupados" 3"Inactivos" 4"Menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/

*Se modifico segun la edad minima de la encuesta de 14 anios y la sintaxis. MGD 06/10/2014
gen condocup_ci =.
replace condocup_ci=1 if q21==1 | q21a==1 | q21a==2 | q21b==1 |  q22==1  | q23==1
replace condocup_ci=2 if condocup_ci!=1 & (((q21==2 | q21b==2 |  q22==2  | q23==2) & q21a==3) | (q24==1 & (q25==1 | q25==2)))  
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) & edad_ci>=14
replace condocup_ci=4 if edad_ci<14 
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
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

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

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

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label value tipocontrato_ci tipocontrato_ci

*************
**pension_ci*
*************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*************
*   ypen_ci *
*************
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*tipopen_ci*
***************
gen tipopen_ci=.

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if q45==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

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



/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci =3200*4.3
label var salmm_ci "Salario minimo legal"

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
replace durades_ci=(1+4.3)/2/4.3 if q41==2
replace durades_ci=(1+3)/2 if q41==3
replace durades_ci=(3+6)/2 if q41==4
replace durades_ci=(6+9)/2 if q41==5
replace durades_ci=(9+12)/2 if q41==6
replace durades_ci=(12+24)/2 if q41==7
replace durades_ci=(24+36)/2 if q41==8



***************
*antiguedad_ci*
***************

* Correccion MGD 7/30/2014: ae cambiaron los promedios de acuerdo a la temporalidad correcta de la variable.
gen antiguedad_ci=.
replace antiguedad_ci=((1+3)/2)/12 if q311==1
replace antiguedad_ci=((3+6)/2)/12 if q311==2
replace antiguedad_ci=((6+9)/2)/12 if q311==3
replace antiguedad_ci=((9+12)/2)/12 if q311==4
replace antiguedad_ci=((12+24)/2)/12 if q311==5
replace antiguedad_ci=((24+60)/2)/12 if q311==6
replace antiguedad_ci=((60+72)/2)/12 if q311==7


************************
*tamemp_ci*
************************

gen tamemp_ci=1 if 	q324==1 | q324==2
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if q324==3 | q324==4
*Empresas grandes
replace tamemp_ci=3 if q324==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño


************************
*categoinac_ci*
************************

gen categoinac_ci = 1 if (q55 ==7 & condocup_ci==3)
replace categoinac_ci = 2 if (q21a == 5 & condocup_ci==3)
replace categoinac_ci = 3 if (q21a == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"


*******************
***formal***
*******************

* no hay informacion para crear la variable
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"
/*
*************
*tamemp_ci***
*************
gen tamemp_ci=q324
replace tamemp_ci=. if q324==9
label define tamemp_ci 1 "1 persona" 2 "2 a 4 personas" 3 "5 a 9 personas" ///
4 "10 a 49 personas" 5 "50 +" 
label value tamemp_ci tamemp_ci 
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/



/*
********
*emp_ci*
********

gen emp_ci=1 if q21==1 | q21a==1 | q21a==2 | q22==1  | q23==1 
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

gen desemp3_ci=1 if (q41>=1 & q41<99) | desemp2_ci==1
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

gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if q37>=2 & q37<.

**********************************
*rama laboral actividad principal*
**********************************
/*
gen rama1=string(q39m)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 
replace rama_ci=. if emp_ci==0
drop rama1
*/
g rama_ci=.
replace rama_ci=1 if (q39m>=100 & q39m<=999) & emp_ci==1
replace rama_ci=2 if (q39m>=1000 & q39m<=1999) & emp_ci==1
replace rama_ci=3 if (q39m>=2000 & q39m<=3499) & emp_ci==1
replace rama_ci=4 if (q39m>=4000 & q39m<=4599) & emp_ci==1
replace rama_ci=5 if (q39m>=5000 & q39m<=5999) & emp_ci==1
replace rama_ci=6 if (q39m>=6000 & q39m<=6999) & emp_ci==1
replace rama_ci=7 if (q39m>=7000 & q39m<=7499) & emp_ci==1
replace rama_ci=8 if (q39m>=8000 & q39m<=8499) & emp_ci==1
replace rama_ci=9 if (q39m>=8300 & q39m<=9990) & emp_ci==1

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


*************
*firmapeq_ci*
*************
/*
gen firmapeq_ci=.
replace firmapeq_ci=0 if q34>=3 & q34<9
replace firmapeq_ci=1 if q34<3
*/
/*
*************
*firmapeq_ci*
*************
* modificado mayra sáenz julio 2013. la variable q34 hace referencia a why do you/does usually work 32 hours or less per week.
* la correcta es la q324 how many persons including yourself are working in the business or at the workplace?
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


*********
*aedu_ci*
*********

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

**********
*eduno_ci*
**********

gen eduno_ci=1 if aedu_ci==0
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.

**********
*edupi_ci*
**********

gen edupi_ci=1 if (aedu_ci >=1 & aedu_ci<6)
replace edupi_ci=0 if aedu_ci>=6 & aedu_ci!=.


**********
*edupc_ci*
**********

gen edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if edupc_ci==. & aedu_ci != .


**********
*edusi_ci*
**********

gen edusi_ci=0 if aedu_ci!=.
replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<11)
label variable edusi_ci "secundaria incompleta"

**********
*edusc_ci*
**********

gen edusc_ci=1 if aedu_ci>=11 & aedu_ci!=.
replace edusc_ci=0 if aedu_ci<11
label variable edusc_ci "secundaria completa"

**********
*eduui_ci*
**********

gen eduui_ci=.

**********
*eduuc_ci*
**********
/* se podría considerar a los que respondieron degree, sin embargo, al evaluar esta variable con ese criterio se 
evidencian individuos que no acaban ni la secundaria y responden degree.

gen eduuc_ci=0 if aedu_ci!=.
replace eduuc_ci=1 if exam == 8
label variable eduuc_ci "superior completo"
*/

gen eduuc_ci=.
label variable eduuc_ci "superior completo"

***********
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


************************
***educacion preescolar.
************************
gen edupre_ci=.
label variable edupre_ci "educacion preescolar"

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

***************************************************************************
***educación terciaria académica versus educación terciaria no-académica***
***************************************************************************
gen eduac_ci=.
label variable eduac_ci "superior universitario vs superior no universitario"

***********
*asiste_ci*
***********

gen asiste_ci=1 if q21a==5
replace asiste_ci=0 if asiste_ci==.

***********
*edupub_ci*
***********

gen edupub_ci=.

*************
***tecnica_ci**
*************
gen tecnica_ci=. /*No se puede estimar*/
label var tecnica_ci "=1 formacion terciaria tecnica"


**************************************************************************
***razones para no asistir a la escuela.***
***************************************************************************
gen pqnoasis_ci=.
label variable pqnoasis_ci  " razón por que no asiste a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

******************************************************
*personas que han repetido al menos un año o grado.***
******************************************************

gen repite_ci=.
label var repite_ci "personas que han repetido al menos un grado o año"

******************************************************
***personas que han repetido el ultimo grado.
******************************************************

gen repiteult_ci=.
label var repite_ci "personas que han repetido el último grado"




**********************************
**** variables de la vivienda ****
**********************************
	
	*esta base de datos no tiene módulo de vivienda.
	
	****************
	***aguared_ch***
	****************
	gen aguared_ch=.
	label var aguared_ch "acceso a fuente de agua por red"	
	

	*****************
	***aguadist_ch***
	*****************
	gen aguadist_ch=.
	label var aguadist_ch "ubicación de la principal fuente de agua"
	label def aguadist_ch 1"dentro de la vivienda" 2"fuera de la vivienda pero en el terreno"
	label def aguadist_ch 3"fuera de la vivienda y del terreno", add
	label val aguadist_ch aguadist_ch

	*****************
	***aguamala_ch***
	*****************
	
	gen aguamala_ch=.
	label var aguamala_ch "agua unimproved según mdg" 
	
	
	*****************
	***aguamide_ch***
	*****************
	gen aguamide_ch=.
	label var aguamide_ch "usan medidor para pagar consumo de agua"

	************
	***luz_ch***
	************
	gen luz_ch=.
	label var luz_ch "la principal fuente de iluminación es electricidad"	
	
		****************
	***luzmide_ch***
	****************
	gen luzmide_ch=.
	label var luzmide_ch "usan medidor para pagar consumo de electricidad"

	****************
	***combust_ch***
	****************
	
	gen combust_ch=.
	label var combust_ch "principal combustible gas o electricidad"	
	
	*************
	***bano_ch***
	*************
	
	gen bano_ch=.
	label var bano_ch "el hogar tiene servicio sanitario"	
	
	***************
	***banoex_ch***
	***************
	gen banoex_ch=.
	label var banoex_ch "el servicio sanitario es exclusivo del hogar"

	*************
	***des1_ch***
	*************
	
	gen des1_ch=.
	label var des1_ch "tipo de desague según unimproved de mdg"
	label def des1_ch 0"no tiene servicio sanitario" 1"conectado a red general o cámara séptica"
	label def des1_ch 2"letrina o conectado a pozo ciego" 3"desemboca en río o calle", add
	label val des1_ch des1_ch
		
	*************
	***des2_ch***
	*************
	
	gen des2_ch=.
	label var des2_ch "tipo de desague sin incluir definición mdg"
	label def des2_ch 0"no tiene servicio sanitario" 1"conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"cualquier otro caso", add
	label val des2_ch des2_ch
	
	*************
	***piso_ch***
	*************
	gen piso_ch=.
	label var piso_ch "materiales de construcción del piso"	
	
	**************
	***pared_ch***
	**************
	gen pared_ch=.
	label var pared_ch "materiales de construcción de las paredes"	
	
	**************
	***techo_ch***
	**************
	
	gen techo_ch=.
	label var techo_ch "materiales de construcción del techo"	
	
	**************
	***resid_ch***
	**************
	
	gen resid_ch =.
	label var resid_ch "método de eliminación de residuos"
	label def resid_ch 0"recolección pública o privada" 1"quemados o enterrados"
	label def resid_ch 2"tirados a un espacio abierto" 3"otros", add
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
	label var dorm_ch "habitaciones para dormir"	
	
	
	****************
	***cuartos_ch***
	****************
	
	gen cuartos_ch=.
	label var cuartos_ch "habitaciones en el hogar"	
	
	***************
	***cocina_ch***
	***************
	gen cocina_ch=.
	label var cocina_ch "cuarto separado y exclusivo para cocinar"

	**************
	***telef_ch***
	**************
	gen telef_ch=.
	label var telef_ch "el hogar tiene servicio telefónico fijo"
	
	
	***************
	***refrig_ch***
	***************
	gen refrig_ch=. 
	label var refrig_ch "el hogar posee refrigerador o heladera"
	
		
	**************
	***freez_ch***
	**************
	gen freez_ch=.
	label var freez_ch "el hogar posee congelador"

	*************
	***auto_ch***
	*************
		
	gen auto_ch=.
	label var auto_ch "el hogar posee automovil particular"
		
	**************
	***compu_ch***
	**************
	gen compu_ch=.
	label var compu_ch "el hogar posee computador"
	
	*****************
	***internet_ch***
	*****************
	gen internet_ch=.
	label var internet_ch "el hogar posee conexión a internet"
	

	************
	***cel_ch***
	************
	
	gen cel_ch=.
	label var cel_ch "el hogar tiene servicio telefonico celular"
	
	**************
	***vivi1_ch***
	**************
		
	gen vivi1_ch=.
	label var vivi1_ch "tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"casa" 2"departamento" 3"otros"
	label val vivi1_ch vivi1_ch
	
	**************
	***vivi2_ch***
	**************
	
	gen vivi2_ch=.
	label var vivi2_ch "la vivienda es casa o departamento"
	
	
	*****************
	***viviprop_ch***
	*****************
	gen viviprop_ch=.
	label var viviprop_ch "propiedad de la vivienda"
	label def viviprop_ch 0"alquilada" 1"propia y totalmente pagada" 2"propia y en proceso de pago"
	label def viviprop_ch 3"ocupada (propia de facto)", add
	label val viviprop_ch viviprop_ch

	****************
	***vivitit_ch***
	****************
	gen vivitit_ch=.
	label var vivitit_ch "el hogar posee un título de propiedad"

	****************
	***vivialq_ch***
	****************
	gen vivialq_ch=.
	label var vivialq_ch "alquiler mensual"

	*******************
	***vivialqimp_ch***
	*******************
	gen vivialqimp_ch=.
	label var vivialqimp_ch "alquiler mensual imputado"

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
clonevar  codocupa = q38m
clonevar codindustria = q39m


compress


saveold "`base_out'", replace


log close
































































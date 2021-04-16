*Elaboración (se reelabora en base a varios do-files anteriores): Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1992 ****
*------------------------*

*AREA CABECERA
*--------------
*- Conversion Bases de datos


clear
set more off
local ruta ="${surveysFolder}\"
local in   ="`ruta'survey\COL\ENH-FT\1992\m9\data_orig\"
local out  ="`ruta'\survey\COL\ENH-FT\1992\m9\data_merge\"

/* clear
*** VIVIENDA
infile using "`in'col92_T00C.dct"
keep if ca_TIPOD=="0"
sort  ca_IDENT 
save `in'colc92vivi.dta,  replace
clear

*** caracteristicas generales y Educación
infile using  "`in'col92_T01C.dct"
keep if ca_TIPOD=="1"
sort  ca_IDENT ca_009
save `in'colc92edu.dta,  replace
clear

/* no pego los modulos originales porque la variable subtipo no existe*

***MODULO ESPECIAL Educación  & SUBTIPO=="0"
infile using  "`in'col92_T02_0C.dct"
keep if ca_TIPOD=="2"
sort  ca_IDENT ca_009

	* para limpiar la base porque SUBTIPO no existe
	drop if ca_12A_T020=="4"| ca_12A_T020=="5"| ca_12A_T020=="6"| ca_12A_T020=="9"| ca_12A_T020=="0"
	gen  cuantas=wordcount(ca_12B_T0201)
	destring ca_12B_T0201, generate(pension) force
	gen  cuantas2=wordcount( ca_12B_T0202)
	
	gen  cuantas3=wordcount(ca_12C_T0201)
	gen  cuantas4=wordcount(ca_12C_T0202)
	gen  cuantas5=wordcount(ca_12C_T0203)
	
	drop if (cuantas>1 |cuantas2>1)
	drop if ca_12B_T0203>="1" & ca_12B_T0203<="8"
	
	drop if (cuantas3>1|cuantas4>1|cuantas5>1)
	*drop if (pension>0 & pension<1000)
	drop if ca_12C_T0204=="2"
	*****base limpia
	
save `in'colc92edu1.dta,  replace
clear

***MODULO ESPECIAL SALUD 1 
infile using  "`in'col92_T02_C.dct"
keep if ca_TIPOD=="2" & SUBTIPO=="1"
sort  ca_IDENT ca_009
save `in'colc92salu1.dta,  replace
clear

***MODULO ESPECIAL SALUD (cont)
infile using  "`in'col92_T03_1C.dct"
keep if ca_TIPOD=="3" & SUBTIPO=="1"
sort  ca_IDENT ca_009
save `in'colc92salu2.dta,  replace
clear

***MODULO ESPECIAL SALUD (cont)
infile using  "`in'col92_T03_2C.dct"
keep if ca_TIPOD=="3" & SUBTIPO=="2"
sort  ca_IDENT ca_009
save `in'colc92salu3.dta,  replace
clear
*/


***ATENCION AL MENOR
infile using  "`in'col92_T04_C.dct"
keep if ca_TIPOD=="4" 
sort  ca_IDENT ca_009
save `in'colc92menor.dta,  replace
clear


*** FUERZA DE TRABAJO
infile using  "`in'col92_T05_C.dct"
keep if ca_TIPOD=="5"
sort  ca_IDENT ca_009
save `in'colc92ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col92_T06_C.dct"
keep if ca_TIPOD=="6"
sort  ca_IDENT ca_009
save `in'colc92ocup.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col92_T07C.dct"
keep if ca_TIPOD=="7"
sort  ca_IDENT ca_009
save `in'colc92deso.dta,  replace
clear

*** INACTIVOS 
infile using  "`in'col92_T08C.dct"
keep if ca_TIPOD=="8"
sort  ca_IDENT ca_009
save `in'colc92inac.dta,  replace
*/

*- Merge bases de datos

clear
*** NOW WE MERGE THE FILES
use `in'colc92edu.dta 
merge ca_IDENT using `in'colc92vivi.dta
rename _merge merge_viv
tab merge_viv
save `in'temp.dta, replace 
sort ca_IDENT ca_009
save,replace
merge ca_IDENT ca_009 using `in'colc92edu.dta
rename _merge mergeed
tab mergeed
/*
sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92edu1.dta
rename _merge mergeed1
tab mergeed1 

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92salu1.dta
rename _merge mergesal1
tab mergesal1 

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92salu2.dta
rename _merge mergesal2
tab mergesal2 

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92salu3.dta
rename _merge mergesal3
tab mergesal3 
*/
sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92menor.dta
rename _merge mergemen
tab mergemen

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92ft.dta
rename _merge mergeft
tab mergeft

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92ocup.dta
rename _merge mergeocup
tab mergeocup

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92deso.dta
rename _merge mergedeso
tab mergedeso

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc92inac.dta
rename _merge mergeinac
tab mergeinac

drop merge*
gen factor_ci=real(FEX_DANE)
drop FEX_DANE


saveold "`out'COL_1992m9_cabecera.dta",  replace



* AREA RESTO
*------------*

*- Conversion Bases de datos

/* clear
*** CARACTERISTICAS GENERALES
infile using  "`in'col92_T01R.dct"
keep if re_TIPOD=="1"
sort  re_IDENT re_009
save `in'colr92edu.dta,  replace
clear

*** VIVIENDA
infile using "`in'col92_T00R.dct"
keep if re_TIPOD=="0"
sort  re_IDENT 
save `in'colr92vivi.dta,  replace
clear

**MODULO ESPECIAL SALUD 1 
infile using  "`in'col92_T02_1R.dct"
keep if re_TIPOD=="2" & SUBTIPO=="1"
sort   re_IDENT re_009
save `in'colr92salu1.dta,  replace
clear

***MODULO ESPECIAL SALUD (cont)
infile using  "`in'col92_T02_2R.dct"
keep if re_TIPOD=="2" & SUBTIPO=="2"
sort   re_IDENT re_009
save `in'colr92salu2.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col92_T03_R.dct"
keep if re_TIPOD=="3"
sort  re_IDENT re_009
save `in'colr92ft.dta,  replace
clear

*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col92_T04_R.dct"
keep if re_TIPOD=="4"
sort  re_IDENT re_009
save `in'colr92ocup.dta,  replace
clear

*** OCUPADOS EMPLEO CONTINUACION
infile using  "`in'col92_T05_R.dct"
keep if re_TIPOD=="5"
sort  re_IDENT re_009
save `in'colr92ocupc.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col92_T06_R.dct"
keep if re_TIPOD=="6"
sort  re_IDENT re_009
save `in'colr92deso.dta,  replace
clear

*** ACTIVIDAD SECUNDARIA  
infile using  "`in'col92_T07_R.dct"
keep if re_TIPOD=="7"
sort  re_IDENT re_009
save `in'colr92sec.dta,  replace
clear

**** MENORES
infile using  "`in'col92_T08_R.dct"
keep if re_TIPOD=="8"
sort  re_IDENT re_009
save `in'colr92menor.dta,  replace
*/

clear

*** NOW WE MERGE THE FILES

use `in'colr92edu.dta 
merge re_IDENT using `in'colr92vivi.dta
rename _merge merge_viv
tab merge_viv
sort re_IDENT re_009
save `in'temp.dta, replace 

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92salu1.dta
rename _merge mergesal1
tab mergesal1

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92salu2.dta
rename _merge mergesal2
tab mergesal2

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92ft.dta
rename _merge mergeft
tab mergeft

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92ocup.dta
rename _merge mergeocup
tab mergeocup

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92ocupc.dta
rename _merge mergeocupc
tab mergeocupc

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92deso.dta
rename _merge mergedeso
tab mergedeso

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92sec.dta
rename _merge mergesec
tab mergesec

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr92menor.dta
rename _merge mergemenor
tab mergemenor

drop if re_TIPOD!="1"

drop merge*
gen factor_ci=real(FEX_DANE)
drop FEX_DANE

saveold "`out'COL_1992m9_resto.dta",  replace




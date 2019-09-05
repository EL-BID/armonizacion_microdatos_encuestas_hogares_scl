*Elaboración (se reelabora en base a varios do-files anteriores): Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1993 ****
*---------------------------*


clear
set more off
local ruta ="\\Sdssrv03\surveys\"
local in   ="`ruta'survey\COL\ENH-FT\1993\m9\data_orig\"
local out  ="`ruta'\survey\COL\ENH-FT\1993\m9\data_merge\"

*1. AREA CABECERA
*-----------------

*- Conversion Bases de datos
/*
 clear
*** VIVIENDA
infile using "`in'col93_T01C.dct"
keep if ca_TIPOD=="01"

/*problema: los registros de vivienda y hogar vienen en el mismo archivo y tienen el mismo
	numero de tipo de registro, pero en las viviendas con mas de un hogar solo se encuentran
	datos de la vivienda para el primero de ellos,por eso se separan ambos archivos, pero es 
	necesario deshacerse de esos segundos hogares, con el fin de tener un archivo donde cada 
	vivienda aparezca una sola vez....esto con el fin de hacer un buen merge
*/
gen uno=1
bys ca_VIVI: gen po=sum(uno) 
drop if ca_1_T00=="" &  ca_2_T00=="" & ca_3_T00=="" & ca_4_T00=="" & ca_5_T001=="" & ca_5_T002=="" & ca_5_T003=="" & ca_5_T004=="" & ca_6_T00=="" & ca_7_T00=="" & ca_8_T00=="" 
drop if po>1

sort  ca_VIVI
save `in'colc93vivi.dta,  replace

clear

*** HOGAR
infile using "`in'col93_H01C.dct"
keep if ca_TIPOD=="01"
sort  ca_VIVI ca_HOGA
save `in'colc93hoga.dta,  replace
clear


*** caracteristicas generales 
infile using  "`in'col93_T10C.dct"
keep if ca_TIPOD=="10"
sort  ca_VIVI ca_HOGA ca_009
save `in'colc93edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col93_T50C.dct"
keep if ca_TIPOD=="50"
sort  ca_VIVI ca_HOGA ca_009
save `in'colc93ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col93_T60C.dct"
keep if ca_TIPOD=="60"
sort  ca_VIVI ca_HOGA ca_009
save `in'colc93ocup.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col93_T70C.dct"
keep if ca_TIPOD=="70"
sort  ca_VIVI ca_HOGA ca_009
save `in'colc93deso.dta,  replace
clear

*** INACTIVOS 
infile using  "`in'col93_T80C.dct"
keep if ca_TIPOD=="80"
sort  ca_VIVI ca_HOGA ca_009
save `in'colc93inac.dta,  replace
*/

* - Merge bases de datos

clear
*** NOW WE MERGE THE FILES
use `in'colc93hoga.dta, clear
merge ca_VIVI  using `in'colc93vivi.dta
rename _merge merge_viv
tab merge_viv, m

sort ca_VIVI ca_HOGA
save `in'temp.dta, replace 


use `in'colc93edu.dta 
merge ca_VIVI ca_HOGA using `in'temp.dta
rename _merge merge_temp
tab merge_temp

sort  ca_VIVI ca_HOGA ca_009
merge ca_VIVI ca_HOGA ca_009 using `in'colc93ft.dta
rename _merge mergeft
tab mergeft

sort  ca_VIVI ca_HOGA ca_009
merge ca_VIVI ca_HOGA ca_009 using `in'colc93ocup.dta
rename _merge mergeocup
tab mergeocup


sort  ca_VIVI ca_HOGA ca_009
merge ca_VIVI ca_HOGA ca_009 using `in'colc93deso.dta
rename _merge mergedeso
tab mergedeso

sort  ca_VIVI ca_HOGA ca_009
merge ca_VIVI ca_HOGA ca_009 using `in'colc93inac.dta
rename _merge mergeinac
tab mergeinac

gen factor_ci=real(FEX_DANE)
drop FEX_DANE
drop merge*
saveold "`out'COL_1993m9_cabecera.dta",  replace


*2. AREA RESTO
*---------------

* Conversion Bases de datos
/*
 clear
*** VIVIENDA
infile using "`in'col93_T01R.dct"
keep if re_TIPOD=="01"
gen uno=1
bysort re_VIVI: gen po=sum(uno) 
drop if po>1

sort  re_VIVI
save `in'colr93vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col93_H01R.dct"
keep if re_TIPOD=="01"
sort  re_VIVI re_HOGA 
save `in'colr93hoga.dta,  replace
clear

*** caracteristicas generales (EDUCA)
infile using  "`in'col93_T10R.dct"
keep if re_TIPOD=="10"
sort  re_IDENT re_009
save `in'colr93edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col93_T50R.dct"
keep if re_TIPOD=="50"
sort  re_IDENT re_009
save `in'colr93ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col93_T60R.dct"
keep if re_TIPOD=="60"
sort  re_IDENT re_009
save `in'colr93ocup.dta,  replace
clear

*** OCUPADOS EMPLEO CONTINUACION
infile using  "`in'col93_T61R.dct"
keep if re_TIPOD=="61"
sort  re_IDENT re_009
save `in'colr93ocupc.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col93_T70R.dct"
keep if re_TIPOD=="70"
sort  re_IDENT re_009
save `in'colr93deso.dta,  replace
clear

*** ACTIVIDAD SECUNDARIA  
infile using  "`in'col93_T71R.dct"
keep if re_TIPOD=="71"
sort  re_IDENT re_009
save `in'colr93sec.dta,  replace
clear

*/

*- Merge bases de datos

*** NOW WE MERGE THE FILES

use `in'colr93hoga.dta
merge re_VIVI using `in'colr93vivi.dta
rename _merge merge_viv
tab merge_viv
capture drop _merge
egen re_IDENT = concat(re_VIVI re_HOGA)
sort re_IDENT 
save `in'temp.dta, replace 

use `in'colr93edu.dta 
capture drop _merge
merge re_IDENT using `in'temp.dta
rename _merge mergetemp
tab mergetemp

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr93ft.dta
rename _merge mergeft
tab mergeft

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr93ocup.dta
rename _merge mergeocup
tab mergeocup

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr93ocupc.dta
rename _merge mergeocupc
tab mergeocupc

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr93deso.dta
rename _merge mergedeso
tab mergedeso

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr93sec.dta
rename _merge mergesec
tab mergesec

gen factor_ci=real(FEX_DANE)
drop FEX_DANE

saveold "`out'COL_1993m9_resto.dta",  replace





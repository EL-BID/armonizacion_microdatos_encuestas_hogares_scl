*Elaboración (se reelabora en base a varios do-files anteriores): Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1994 ****
*------------------------*

*1 - Conversion Bases de datos



clear
set more off
local ruta ="${surveysFolder}\"
local in   ="`ruta'survey\COL\ENH-FT\1994\m9\data_orig\"
local out  ="`ruta'survey\COL\ENH-FT\1994\m9\data_merge\"

* AREA CABECERA
*---------------

/* clear
*** VIVIENDA
infile using "`in'col94_T01C.dct"
keep if ca_TIPOD=="01"

gen uno=1
bysort ca_VIVI: gen po=sum(uno) 
drop if po>1

sort  ca_VIVI  
save `in'colc94vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col94_H01C.dct"
keep if ca_TIPOD=="01"
sort  ca_VIVI  ca_HOGAR
save `in'colc94hoga.dta,  replace
clear

*** CARACTERISTICAS GENERALES
infile using  "`in'col94_T10C.dct"
keep if ca_TIPOD=="10"
sort  ca_IDENT ca_009
save `in'colc94edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col94_T50C.dct"
keep if ca_TIPOD=="50"
sort  ca_IDENT ca_009
save `in'colc94ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col94_T60C.dct"
keep if ca_TIPOD=="60"
sort  ca_IDENT ca_009
save `in'colc94ocup.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col94_T70C.dct"
keep if ca_TIPOD=="70"
sort  ca_IDENT ca_009
save `in'colc94deso.dta,  replace
clear

*** INACTIVOS 
infile using  "`in'col94_T80C.dct"
keep if ca_TIPOD=="80"
sort  ca_IDENT ca_009
save `in'colc94inac.dta,  replace
*/


*- Merge bases de datos
*-------------------------

clear
*** NOW WE MERGE THE FILES

use `in'colc94hoga.dta, clear
merge ca_VIVI using `in'colc94vivi.dta
rename _merge merge_viv
tab merge_viv, m

egen ca_IDENT = concat(ca_VIVI ca_HOGAR)
sort ca_IDENT
save `in'temp.dta, replace 

use `in'colc94edu.dta 
sort  ca_IDENT ca_009
merge ca_IDENT using `in'temp.dta
rename _merge merge_temp
drop if merge_temp==2
tab merge_temp,m
/*
algunos ejemplos de viviendas que NO existen en el archivo de viviendas (59 hogares)y si en el de 
personas ,se pierden 273 casos (personas)  en total: 1) 01029215010107001; 2) 01067308030107025
*/

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc94ft.dta
rename _merge mergeft
tab mergeft

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc94ocup.dta
rename _merge mergeocup
tab mergeocup


sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc94deso.dta
rename _merge mergedeso
tab mergedeso

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc94inac.dta
rename _merge mergeinac
tab mergeinac

gen factor_ci=real(FEX_DANE)
drop FEX_DANE

/*
En las anteriores de colombia se perdia al menos el 19% de las observaciones*

. use "${surveysFolder}\MARIAFP\COL-ARM-VIEJOS\1994\Van_data\vancol94.dta", clear

. tab register,m

   register |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     16,590       18.60       18.60
         10 |     72,627       81.40      100.00
------------+-----------------------------------
      Total |     89,217      100.00

. tab sex,m

        sex |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     34,121       38.24       38.24
          2 |     38,506       43.16       81.40
          . |     16,590       18.60      100.00
------------+-----------------------------------
      Total |     89,217      100.00
*/ 

drop merge* 
saveold "`out'COL_1994m9_cabecera.dta",  replace


* AREA RESTO
*------------
*- Conversion Bases de datos

/* clear
*** VIVIENDA
infile using "`in'col94_T01R.dct"
keep if re_TIPOD=="01"

gen uno=1
bysort re_VIVI: gen po=sum(uno) 
drop if po>1

sort  re_VIVI
save `in'colr94vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col94_H01R.dct"
keep if re_TIPOD=="01"
sort  re_VIVI re_HOGA 
save `in'colr94hoga.dta,  replace
clear

*** caracteristicas generales (EDUCA)
infile using  "`in'col94_T10R.dct"
keep if re_TIPOD=="10"
sort  re_IDENT re_009
save `in'colr94edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col94_T50R.dct"
keep if re_TIPOD=="50"
sort  re_IDENT re_009
save `in'colr94ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col94_T60R.dct"
keep if re_TIPOD=="60"
sort  re_IDENT re_009
save `in'colr94ocup.dta,  replace
clear

*** OCUPADOS EMPLEO CONTINUACION
infile using  "`in'col94_T61R.dct"
keep if re_TIPOD=="61"
sort  re_IDENT re_009
save `in'colr94ocupc.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col94_T70R.dct"
keep if re_TIPOD=="70"
sort  re_IDENT re_009
save `in'colr94deso.dta,  replace
clear
/*
*** ACTIVIDAD SECUNDARIA  
infile using  "`in'col94_T71R.dct"
keep if re_TIPOD=="71"
sort  re_IDENT re_009
save `in'colr94sec.dta,  replace
clear
*/
*/

*- Merge bases de datos
*-------------------------

*** NOW WE MERGE THE FILES

use `in'colr94hoga.dta, clear
merge re_VIVI using `in'colr94vivi.dta
rename _merge merge_viv
tab merge_viv
capture drop _merge
egen re_IDENT = concat(re_VIVI re_HOGA)
sort re_IDENT 
save `in'temp.dta, replace 

use `in'colr94edu.dta 
capture drop _merge
merge re_IDENT using `in'temp.dta
rename _merge mergetemp
tab mergetemp

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr94ft.dta
rename _merge mergeft
tab mergeft

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr94ocup.dta
rename _merge mergeocup
tab mergeocup

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr94ocupc.dta
rename _merge mergeocupc
tab mergeocupc

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr94deso.dta
rename _merge mergedeso
tab mergedeso
/*
sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr94sec.dta
rename _merge mergesec
tab mergesec
*/
/* PARA DESHACERSE DE UNA VIVIENDA QUE APARECE EN EL ARCHIVO DE PERSONAS 
Y NO EN EL DE HOGARES 
drop if merge_viv==1 | merge_temp==1
*/
gen factor_ci=real(FEX_DANE)
drop FEX_DANE

drop merge* 
saveold "`out'COL_1994m9_resto.dta",  replace



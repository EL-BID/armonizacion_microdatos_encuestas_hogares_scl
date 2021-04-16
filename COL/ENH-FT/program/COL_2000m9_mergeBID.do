*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 2000 ****
*--------------------------*

*1 - Conversion Bases de datos

**********************************************************
***PROGRAMA PARA CREAR LA CABECERA COLOMBIA 2000       ***
**********************************************************

*Nota: este do-file contiene el merge tanto para areas: Cabecera y Resto

clear
set more off
local ruta ="${surveysFolder}\"
local in   ="`ruta'survey\COL\ENH-FT\2000\m9\data_orig\"
local out  ="`ruta'\survey\COL\ENH-FT\2000\m9\data_merge\"

 *1. AREA CABECERA
 *------------------
/* desactivar si se lo desea correr nuevamente

*** VIVIENDA
infile using `in'col00_T01C.dct
keep if ca_TIPOD=="01"
sort  ca_IDENT 
save `in'colc00vivi.dta,  replace
clear

*** EDUCACION
infile using `in'col00_T10C.dct
keep if ca_TIPOD=="10"
sort  ca_IDENT 
save `in'colc00edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using `in'col00_T50C.dct
keep if ca_TIPOD=="50"
sort  ca_IDENT ca_1_T50_
save `in'colc00ft.dta,  replace
clear

*** OCUPADOS EMPLEO PRINCIPAL
infile using `in'col00_T60C.dct
keep if ca_TIPOD=="60"
sort  ca_IDENT ca_1_T60_
save `in'colc00oep.dta,  replace
clear

*** OCUPADOS SUBEMPLEO
infile using `in'col00_T61C.dct
keep if ca_TIPOD=="61"
sort  ca_IDENT ca_1_T61_
save `in'colc00os.dta,  replace
clear


*** DESOCUPADOS 
infile using `in'col00_T70C.dct
keep if ca_TIPOD=="70"
sort  ca_IDENT ca_1_T70_ 
save `in'colc00des.dta,  replace
clear

*** INACTIVOS
infile using `in'col00_T80C.dct
keep if ca_TIPOD=="80"
sort  ca_IDENT ca_1_T80_
save `in'colc00ina.dta,  replace
*/

*Merge bases de datos de CABECERA
*-------------------------

clear
*** NOW WE MERGE THE FILES
use `in'colc00vivi.dta
sort  ca_IDENT 
merge ca_IDENT using `in'colc00edu.dta
rename _merge merge1
rename ca_1_T10_ ca_1_T50_
tab merge1
sort  ca_IDENT ca_1_T50_
merge ca_IDENT ca_1_T50_ using `in'colc00ft.dta
rename _merge merge2
rename ca_1_T50_ ca_1_T60_
tab merge2
sort  ca_IDENT ca_1_T60_
merge ca_IDENT ca_1_T60_ using `in'colc00oep.dta
rename _merge merge3
rename ca_1_T60_ ca_1_T61_
tab merge3
sort  ca_IDENT ca_1_T61_
merge ca_IDENT ca_1_T61_ using `in'colc00os.dta
rename _merge merge5
rename ca_1_T61_ ca_1_T70_
tab merge5
sort  ca_IDENT ca_1_T70_
merge ca_IDENT ca_1_T70_ using `in'colc00des.dta
rename _merge merge6
rename ca_1_T70_ ca_1_T80_
tab merge6
sort  ca_IDENT ca_1_T80_
merge ca_IDENT ca_1_T80_ using `in'colc00ina.dta
rename _merge merge7
tab merge7
gen factor=real(ca_FACTEXP)
drop ca_FACTEXP
ren factor ca_FACTEXP 

* Y.L. -> Incremento nuevo factor /antes estaba en do-file armonizada
gen idh_ch=ca_IDENT 
gen ide009=ca_1_T80_ 

sort  idh_ch ide009
merge idh_ch ide009 using "`in'COL_nueva00.dta", unique sort
tab _merge
drop if _merge==2
drop _merge* fex idh_ch

saveold "`out'COL_2000m9_cabecera.dta",  replace




 *2. AREA RESTO
 *------------------
 
 /*
 * YL -> desactivar si se lo desea correr nuevamente
*** VIVIENDA
infile using `in'col00_T01R.dct
keep if re_TIPOD=="01"
sort  re_IDENT 
save `in'colr00vivi.dta,  replace
clear

*** EDUCACION
infile using `in'col00_T10R.dct
keep if re_TIPOD=="10"
sort  re_IDENT 
save `in'colr00edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using `in'col00_T50R.dct
keep if re_TIPOD=="50"
sort  re_IDENT re_1_T50_
save `in'colr00ft.dta,  replace
clear

*** OCUPADOS EMPLEO PRINCIPAL
infile using `in'col00_T60R.dct
keep if re_TIPOD=="60"
sort  re_IDENT re_1_T60_
save `in'colr00oep.dta,  replace
clear

*** OCUPADOS SUBEMPLEO
infile using `in'col00_T61R.dct
keep if re_TIPOD=="61"
sort  re_IDENT re_1_T61_
save `in'colr00os.dta,  replace
clear


*** DESOCUPADOS 
infile using `in'col00_T70R.dct
keep if re_TIPOD=="70"
sort  re_IDENT re_1_T70_ 
save `in'colr00des.dta,  replace
clear

*** INACTIVOS
infile using `in'col00_T80R.dct
keep if re_TIPOD=="80"
sort  re_IDENT re_1_T80_
save `in'colr00ina.dta,  replace
*/

*2 - Merge bases de datos
*-------------------------
*** NOW WE MERGE THE FILES

use `in'colr00vivi.dta, clear

sort  re_IDENT 
merge re_IDENT using `in'colr00edu.dta
rename _merge merge1
rename re_1_T10_ re_1_T50_
tab merge1
sort  re_IDENT re_1_T50_
merge re_IDENT re_1_T50_ using `in'colr00ft.dta
rename _merge merge2
rename re_1_T50_ re_1_T60_
tab merge2
sort  re_IDENT re_1_T60_
merge re_IDENT re_1_T60_ using `in'colr00oep.dta
rename _merge merge3
rename re_1_T60_ re_1_T61_
tab merge3
sort  re_IDENT re_1_T61_
merge re_IDENT re_1_T61_ using `in'colr00os.dta
rename _merge merge5
rename re_1_T61_ re_1_T70_
tab merge5
sort  re_IDENT re_1_T70_
merge re_IDENT re_1_T70_ using `in'colr00des.dta
rename _merge merge6
rename re_1_T70_ re_1_T80_
tab merge6
sort  re_IDENT re_1_T80_
merge re_IDENT re_1_T80_ using `in'colr00ina.dta
rename _merge merge7
tab merge7
gen factor=real(re_FACTEXP)
drop re_FACTEXP
ren factor re_FACTEXP 

* Y.L. -> Incremento nuevo factor /antes estaba en do-file armonizada
gen idh_ch=re_IDENT 
gen ide009=re_1_T80

sort  idh_ch ide009
merge idh_ch ide009 using "`in'COL_nueva00.dta", unique sort
tab _merge
drop if _merge==2
drop _merge* fex idh_ch
saveold "`out'COL_2000m9_resto.dta",  replace


 
 
 
 
 
 
 
 
 
 
 
 

*Elaboración (se reelabora en base a varios do-files anteriores): Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1996 ****
*--------------------------*

*1.- AREA CABCERA
*-----------------
clear
set more off
local ruta ="\\Sdssrv03\surveys\"
local in   ="`ruta'survey\COL\ENH-FT\1996\m9\data_orig\"
local out  ="`ruta'\survey\COL\ENH-FT\1996\m9\data_merge\"

* - Conversion Bases de datos

**********************************************************
***PROGRAMA PARA CREAR LA CABECERA COLOMBIA 1996      ***
**********************************************************

/*
clear
*** VIVIENDA
infile using `in'col96_T01C.dct
keep if ca_TIPOD=="01"
sort  ca_IDENT 
save `in'colc96vivi.dta,  replace
clear

*** EDUCACION
infile using `in'col96_T10C.dct
keep if ca_TIPOD=="10"
sort  ca_IDENT 
save `in'colc96edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using `in'col96_T50C.dct
keep if ca_TIPOD=="50"
sort  ca_IDENT ca_1_T50_
save `in'colc96ft.dta,  replace
clear

*** OCUPADOS EMPLEO PRINCIPAL
infile using `in'col96_T60C.dct
keep if ca_TIPOD=="60"
sort  ca_IDENT ca_1_T60_
save `in'colc96oep.dta,  replace
clear

*** DESOCUPADOS 
infile using `in'col96_T70C.dct
keep if ca_TIPOD=="70"
sort  ca_IDENT ca_1_T70_ 
save `in'colc96des.dta,  replace
clear

*** INACTIVOS
infile using `in'col96_T80C.dct
keep if ca_TIPOD=="80"
sort  ca_IDENT ca_1_T80_
save `in'colc96ina.dta,  replace
*/

*- Merge bases de datos
*-------------------------

*** NOW WE MERGE THE FILES

use `in'colc96vivi.dta, clear

sort  ca_IDENT 
merge ca_IDENT using `in'colc96edu.dta
rename _merge merge1
rename ca_1_T10_ ca_1_T50_
tab merge1
sort  ca_IDENT ca_1_T50_
merge ca_IDENT ca_1_T50_ using `in'colc96ft.dta
rename _merge merge2
rename ca_1_T50_ ca_1_T60_
tab merge2
sort  ca_IDENT ca_1_T60_
merge ca_IDENT ca_1_T60_ using `in'colc96oep.dta
rename _merge merge3
rename ca_1_T60_ ca_1_T70_
tab merge3
sort  ca_IDENT ca_1_T70_
merge ca_IDENT ca_1_T70_ using `in'colc96des.dta
rename _merge merge6
rename ca_1_T70_ ca_1_T80_
tab merge6
sort  ca_IDENT ca_1_T80_
merge ca_IDENT ca_1_T80_ using `in'colc96ina.dta
rename _merge merge7
tab merge7
gen factor=real(ca_FACTEXP)
drop ca_FACTEXP
ren factor ca_FACTEXP 
label variable ca_FACTEXP  "Factor de expansion antiguo"
*replace ZONA1="1" if  ZONA1==""
* Y.L. -> Incremento nuevo factor /antes estaba en do-file armonizada
gen idh_ch=ca_IDENT 
gen ide009=ca_1_T80_ 

sort  ident ide009
merge ident ide009 using "`in'zona_fex.dta", unique sort
tab _merge
drop if _merge==2
drop _merge* ZONA2 ZONA1 fex
saveold "`out'COL_1996m9_cabecera.dta",  replace

* 2.- AREA RESTO
*----------------
*- Conversion Bases de datos

/*
clear
*** VIVIENDA
infile using `in'col96_T01R.dct
keep if re_TIPOD=="01"
sort  re_IDENT 
save `in'colr96vivi.dta,  replace
clear

*** EDUCACION
infile using `in'col96_T10R.dct
keep if re_TIPOD=="10"
sort  re_IDENT 
save `in'colr96edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using `in'col96_T50R.dct
keep if re_TIPOD=="50"
sort  re_IDENT re_1_T50_
save `in'colr96ft.dta,  replace
clear

*** OCUPADOS EMPLEO PRINCIPAL
infile using `in'col96_T60R.dct
keep if re_TIPOD=="60"
sort  re_IDENT re_1_T60_
save `in'colr96oep.dta,  replace
clear

*** DESOCUPADOS 
infile using `in'col96_T70R.dct
keep if re_TIPOD=="70"
sort  re_IDENT re_1_T70_ 
save `in'colr96des.dta,  replace
clear

*** INACTIVOS *** NO EXISTEN EN LA ENCUESTA
*/

*- Merge bases de datos
*-------------------------

*** NOW WE MERGE THE FILES

use `in'colr96vivi.dta, clear

sort  re_IDENT 
merge re_IDENT using `in'colr96edu.dta
rename _merge merge1
rename re_1_T10_ re_1_T50_
tab merge1
sort  re_IDENT re_1_T50_
merge re_IDENT re_1_T50_ using `in'colr96ft.dta
rename _merge merge2
rename re_1_T50_ re_1_T60_
tab merge2
sort  re_IDENT re_1_T60_
merge re_IDENT re_1_T60_ using `in'colr96oep.dta
rename re_1_T60_ re_1_T70_
rename _merge merge5
tab merge5
sort  re_IDENT re_1_T70_
merge re_IDENT re_1_T70_ using `in'colr96des.dta
rename _merge merge6
tab merge6
gen factor=real(re_FACTEXP)
drop re_FACTEXP
ren factor re_FACTEXP 
gen ZONA1=2 
label variable ZONA1 "1-Cabecera 2-Resto"
gen ZONA2=2 
label variable ZONA2 "1-Urbano 2-Rural"

* Y.L. -> Incremento nuevo factor /antes estaba en do-file armonizada
gen ident=re_IDENT 
gen ide009=re_1_T70_

sort  ident ide009
merge ident ide009 using "`in'zona_fex.dta", unique sort
tab _merge
drop if _merge==2
drop _merge* ZONA2 ZONA1 fex

saveold "`out'COL_1996m9_resto.dta",  replace







set mem 200m
set more off

****Colombia ECV 2003*****

********************************************************************
*****	UNION DE LOS ARCHIVOS 					****
********************************************************************


***************************************************
*****	Creación del Archivo de Hogares		***
***************************************************

use condic.dta, clear
sort numero
save condic.dta, replace

use hogar.dta,clear
sort numero
save hogar.dta,replace
merge numero using condic.dta
tab _merge
drop _merge
sort numero
save hogares.dta, replace

***************************************************
*****	Creación del Archivo de Personas	***
***************************************************

use menores.dta, clear
sort numero e01
save menores.dta, replace

use labores.dta, clear
sort numero e01
save labores.dta, replace

use trabajo.dta, clear
sort numero e01
save trabajo.dta, replace

use prefcia.dta, clear
sort numero e01
save prefcia.dta, replace

use educa.dta, clear
sort numero e01
save educa.dta, replace

use personas.dta, clear
sort numero e01
merge numero e01 using educa.dta
tab _merge
drop _merge
sort numero e01
merge numero e01 using prefcia.dta
tab _merge
drop _merge
sort numero e01
merge numero e01 using trabajo.dta
tab _merge
drop _merge
sort numero e01
merge numero e01 using labores.dta
tab _merge
drop _merge
sort numero e01
merge numero e01 using menores.dta
tab _merge
drop _merge
sort numero e01
save personas.dta, replace

*****************************
*** UNION DE LOS ARCHIVOS ***
*****************************

merge numero using hogares.dta
tab _merge
drop _merge
sort numero e01
compress
save col03_ecv.dta, replace

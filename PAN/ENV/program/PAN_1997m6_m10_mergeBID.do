************************************
*** ENCUESTA DE NIVELES DE VIDA  ***
***         AÑO 1997		 ***
************************************

set more off
set mem 120m

** MERGE

** HOUSEHOLDS

** e97 base

use e97base.dta, clear
merge form using e97eq10.dta
tab _merge
drop _merge
sort form

merge form using e97vi10.dta
tab _merge
drop _merge
sort form

merge form using e97ga00.dta
tab _merge
drop _merge
sort form

save e97Households.dta, replace

use e97pe10.dta, clear
merge form using e97Households.dta
tab _merge
drop _merge
sort form p200

save pan97_env.dta

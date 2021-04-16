************************************
*** ENCUESTA DE NIVELES DE VIDA  ***
***         AŃO 2003		 ***
************************************

set more off
set mem 120m

** MERGE

** HOUSEHOLDS

** e03 base

use e03base.dta, clear

* Capital Social
merge form using e03cs02.dta
tab _merge
drop _merge
sort form

* Hogar
merge form using e03hg01.dta
tab _merge
drop _merge
sort form

save e03Households.dta, replace

** INDIVIDUALS

use e03pe03.dta, clear

merge form p000 using e03pe04.dta
tab _merge
drop _merge
sort form p000

merge form p000 using e03pe05.dta
tab _merge
drop _merge
sort form p000

merge form p000 using e03pe06.dta
tab _merge
drop _merge
sort form p000

merge form p000 using e03pe07.dta
tab _merge
drop _merge
sort form p000

merge form p000 using e03pe08.dta
tab _merge
drop _merge
sort form p000

merge form p000 using e03pe09.dta
tab _merge
drop _merge
sort form p000

merge form p000 using e03pe13.dta
tab _merge
drop _merge
sort form p000

save e03Individuals.dta, replace

***

use e03Individuals.dta, clear
merge form using e03Households.dta
tab _merge
drop _merge

save pan03_env.dta, replace


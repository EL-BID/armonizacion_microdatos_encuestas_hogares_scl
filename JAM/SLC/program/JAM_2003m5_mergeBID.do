*JSLC 2003

set mem 100m
set more off

* Creacion de archivo de hogares

use annual.dta,clear
merge serial using rec016.dta
sort serial
tab _merge
drop _merge

merge serial using rec010.dta
sort serial
tab _merge
drop _merge

merge serial using rec013.dta
sort serial
tab _merge
drop _merge

merge serial using rec014.dta
sort serial
tab _merge
drop _merge

merge serial using rec015.dta
sort serial
tab _merge
drop _merge

merge serial using rec001.dta
sort serial
tab _merge
drop _merge

save jam03_jslc_h.dta, replace

*Creacion de archivo de personas

use rec018.dta,clear
merge serial ind using rec002.dta
sort serial ind
tab _merge
drop _merge

save jam03_jslc_p.dta, replace

* Merge personas hogares.
use jam03_jslc_p.dta, clear
merge serial using jam03_jslc_h.dta
tab _merge

save jam03_jslc.dta, replace


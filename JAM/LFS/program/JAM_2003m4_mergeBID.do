
clear all
local ruta "Y:\survey\JAM\LFS\2003\m4\data_orig"

*Base 1


use "`ruta'\april2003_rt1.dta"
destring  par dwell hhold indiv, replace
egen idbase = concat(  par dwell hhold indiv)
sort idbase

save "`ruta'\april2003_rt1merge.dta", replace

*Base 2

use "`ruta'\april2003_rt2.dta"
destring  par dwell hhold indiv, replace
egen idbase = concat(  par dwell hhold indiv)
sort idbase

save "`ruta'\april2003_rt2merge.dta", replace

*Base 3

*clear all

use "`ruta'\april2003_rt3.dta"
destring  par dwell hhold indiv, replace
egen idbase = concat(  par dwell hhold indiv)
sort idbase

save "`ruta'\april2003_rt3merge.dta", replace

*Base 4

*clear all

use "`ruta'\april2003_rt4.dta"
destring  par dwell hhold indiv, replace
egen idbase = concat( par dwell hhold indiv)
sort idbase

save "`ruta'\april2003_rt4merge.dta", replace





* Merge


clear all
local ruta "Y:\survey\JAM\LFS\2003\m4\data_orig"
use "`ruta'\april2003_rt1merge.dta"


merge m:m idbase using "`ruta'\april2003_rt2merge.dta"
tab _merge
rename _merge _merge2

merge m:m idbase using "`ruta'\april2003_rt3merge.dta"
tab _merge
rename _merge _merge3

merge m:m idbase using "`ruta'\april2003_rt4merge.dta"
tab _merge
rename _merge _merge4


tab _merge2  _merge3
tab _merge2  _merge4


drop _merge* idbase


save "Y:\survey\JAM\LFS\2003\m4\data_merge\JAM_2003m4.dta", replace

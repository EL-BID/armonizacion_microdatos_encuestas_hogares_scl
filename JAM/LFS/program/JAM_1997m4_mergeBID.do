
clear all
local ruta "${surveysFolder}\survey\JAM\LFS\1997\m4\data_orig\"

*Base 1


use "`ruta'\empl97.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\empl97merge.dta", replace

*Base 2

use "`ruta'\outlf97.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\outlf97merge.dta", replace

*Base 3

*clear all

use "`ruta'\unempl97.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\unempl97merge.dta", replace

*Base 4

*clear all

use "`ruta'\parta97.dta"
destring par dwell hh id, replace
egen idbase = concat(par dwell hh id)
sort idbase

save "`ruta'\parta97merge.dta", replace

* Merge
merge m:m idbase using "`ruta'\empl97merge.dta"
tab _merge
rename _merge _merge1

merge m:m idbase using "`ruta'\unempl97merge.dta"
tab _merge
rename _merge _merge2

merge m:m idbase using "`ruta'\outlf97merge.dta"
tab _merge
rename _merge _merge3

tab _merge1  _merge2
tab _merge1  _merge3

drop _merge* idbase


save "${surveysFolder}\survey\JAM\LFS\1997\m4\data_merge\JAM_1997m4.dta", replace

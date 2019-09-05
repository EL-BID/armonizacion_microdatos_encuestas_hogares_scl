
clear all
local ruta "Y:\survey\JAM\LFS\1999\m4\data_orig"

*Base 1


use "`ruta'\emp99.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\emp99merge.dta", replace

*Base 2

use "`ruta'\outlf99.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\outlf99merge.dta", replace

*Base 3

*clear all

use "`ruta'\unemp99.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\unemp99merge.dta", replace

*Base 4

*clear all

use "`ruta'\parta99.dta"
destring par dwell hh id, replace
egen idbase = concat(par dwell hh id)
sort idbase

save "`ruta'\parta99merge.dta", replace

* Merge
merge m:m idbase using "`ruta'\emp99merge.dta"
tab _merge
rename _merge _merge1

merge m:m idbase using "`ruta'\unemp99merge.dta"
tab _merge
rename _merge _merge2

merge m:m idbase using "`ruta'\outlf99merge.dta"
tab _merge
rename _merge _merge3

tab _merge1  _merge2
tab _merge1  _merge3


drop _merge* idbase


save "Y:\survey\JAM\LFS\1999\m4\data_merge\JAM_1999m4.dta", replace

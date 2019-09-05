
clear all
local ruta "Y:\survey\JAM\LFS\1998\m4\data_orig\"

*Base 1


use "`ruta'\emp98.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\emp98merge.dta", replace

*Base 2

use "`ruta'\outlf98.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\outlf98merge.dta", replace

*Base 3

*clear all

use "`ruta'\unemp98.dta"
destring par dwell hh id, replace
egen idbase = concat( par dwell hh id)
sort idbase

save "`ruta'\unemp98merge.dta", replace

*Base 4

*clear all

use "`ruta'\parta98.dta"
destring par dwell hh id, replace
egen idbase = concat(par dwell hh id)
sort idbase

save "`ruta'\parta98merge.dta", replace

* Merge
merge m:m idbase using "`ruta'\emp98merge.dta"
tab _merge
rename _merge _merge1

merge m:m idbase using "`ruta'\unemp98merge.dta"
tab _merge
rename _merge _merge2

merge m:m idbase using "`ruta'\outlf98merge.dta"
tab _merge
rename _merge _merge3

tab _merge1  _merge2
tab _merge1  _merge3

drop _merge* idbase


save "Y:\survey\JAM\LFS\1998\m4\data_merge\JAM_1998m4.dta", replace



*Modificación Mayra Sáenz Julio 2015

cd "${surveysFolder}\survey\URY\ECH\2006\a\data_orig"

use hog_2006_publica.dta, clear
sort numero, stable

save H2006.dta, replace


use per_2006_publica.dta, clear
sort numero, stable

save P_2006.dta, replace

*Merge 

merge m:1 numero using H2006.dta
sort numero
tab _merge
drop _merge

saveold "${surveysFolder}\survey\URY\ECH\2006\a\data_merge\URY_2006a.dta", replace

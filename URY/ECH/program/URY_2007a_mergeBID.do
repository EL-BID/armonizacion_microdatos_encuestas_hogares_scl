
/*use h2007_interior.dta, clear
append using h2007_montevideo.dta
sort correlativ

save h2007.dta, replace


use p1_2007_interior.dta, clear
append using p1_2007_montevideo.dta
sort correlativ

save p1_2007.dta, replace

use p2_2007_interior.dta, clear
append using p2_2007_montevideo.dta
sort correlativ

save p2_2007.dta, replace

*Merge 

merge correlativ using p1_2007.dta
sort correlativ
drop _merge

save p_2007.dta, replace

merge correlativ using h2007.dta
sort correlativ
drop _merge

save ury07.dta, replace*/


*Modificación Mayra Sáenz Julio 2015

cd "${surveysFolder}\survey\URY\ECH\2007\a\data_orig"

use hog_2007_publica.dta, clear
sort numero, stable

save H2007.dta, replace


use per_2007_publica.dta, clear
sort numero, stable

save P_2007.dta, replace

*Merge 

merge m:1 numero using H2007.dta
sort numero
tab _merge
drop _merge

saveold "${surveysFolder}\survey\URY\ECH\2007\a\data_merge\URY_2007a.dta", replace

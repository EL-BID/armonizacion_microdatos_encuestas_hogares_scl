
cd "${surveysFolder}\survey\URY\ECH\2013\a\data_orig"
use h_2013_terceros.dta, clear
sort numero

save h_2013_terceros.dta, replace


use p_2013_terceros.dta, clear
sort numero

save p_2013_terceros.dta, replace

*Merge 

use p_2013_terceros, clear
merge m:1 numero using h_2013_terceros.dta
sort numero
tab _merge
drop _merge

save ury_2013a.dta, replace

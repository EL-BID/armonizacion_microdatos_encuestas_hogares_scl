* Autor: Marcela G. Rubio


global ruta "M:\survey\URY\ECH\2014\a\data_orig\"
global out "M:\survey\URY\ECH\2014\a\data_merge\"

use "$ruta\h_2014_terceros.dta", clear
sort numero

save "$ruta\h_2014_terceros.dta", replace


use "$ruta\p_2014_terceros.dta", clear
sort numero

save "$ruta\p_2014_terceros.dta", replace

*Merge 

use "$ruta\p_2014_terceros", clear
merge m:1 numero using "$ruta\h_2014_terceros.dta"
sort numero
tab _merge
drop _merge

saveold "$out\URY_2014a.dta", replace

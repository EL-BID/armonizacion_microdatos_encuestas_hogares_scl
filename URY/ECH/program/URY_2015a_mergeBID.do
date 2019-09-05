* Autor: Marcela G. Rubio
* Fecha: 24 de junio de 2016

global ruta "M:\survey\URY\ECH\2015\a\data_orig\"
global out "M:\survey\URY\ECH\2015\a\data_merge\"

use "$ruta\hogares_2015_terceros.dta", clear
sort numero

save "$ruta\hogares_2015_terceros.dta", replace


use "$ruta\personas_2015_terceros.dta", clear
sort numero nper
save "$ruta\personas_2015_terceros.dta", replace

*Merge 

use "$ruta\personas_2015_terceros", clear
merge m:1 numero using "$ruta\hogares_2015_terceros.dta"
sort numero nper
drop _merge

saveold "$out\URY_2015a.dta", replace

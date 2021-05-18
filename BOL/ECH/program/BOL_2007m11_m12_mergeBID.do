* Creado por: Marcela G. Rubio 


global ruta = "${surveysFolder}\survey\BOL\ECH\2007\m11_m12\data_orig\"
global out = "${surveysFolder}\survey\BOL\ECH\2007\m11_m12\data_merge"

use "$ruta/EH2007_poblacion.dta", clear
merge m:1 folio using "$ruta/EH2007_vivienda.dta"

save "$out\BOL_2007m11_m12.dta", replace

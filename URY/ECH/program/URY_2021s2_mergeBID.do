* Autor: Natalia Tosi
* Fecha: Diciembre 2021

global ruta "${surveysFolder}\survey\URY\ECH\2021\a\data_orig"
global out "${surveysFolder}\survey\URY\ECH\2021\a\data_merge"
set more off

use "$ruta\ECH_implantacion_sem2_2021.dta", clear
sort id nper
recast str244 f108_1, force

save "$ruta\ECH_implantacion_sem2_2021.dta", replace


saveold "$out\URY_2021s2.dta", version(12) replace

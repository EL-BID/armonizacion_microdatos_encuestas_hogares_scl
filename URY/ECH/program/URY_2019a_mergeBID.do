* Autor: Daniela Zuluaga
* Fecha: Mayo de 2020

global ruta "${surveysFolder}\survey\URY\ECH\2019\a\data_orig"
global out "${surveysFolder}\survey\URY\ECH\2019\a\data_merge"
set more off
use "$ruta\H_2019_Terceros.dta", clear
sort numero
save "$ruta\H_2019_Terceros.dta", replace


use "$ruta\P1_2019_Terceros.dta", clear
sort numero nper
save "$ruta\P1_2019_Terceros.dta", replace

use "$ruta\P2_2019_Terceros.dta", clear
sort numero nper
save "$ruta\P2_2019_Terceros.dta", replace

*Merge 

use "$ruta\P1_2019_Terceros.dta", clear
merge 1:1 numero nper using "$ruta\P2_2019_Terceros.dta"
drop _merge 

merge m:1 numero using "$ruta\H_2019_Terceros.dta"
sort numero nper
drop _merge

saveold "$out\URY_2019a.dta", version(12) replace

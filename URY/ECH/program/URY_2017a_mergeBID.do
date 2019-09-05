* Autor: Daniela Zuluaga
* Fecha: 26 de Junio de 2018

global ruta "\\Sdssrv03\surveys\survey\URY\ECH\2017\a\data_orig"
global out "\\Sdssrv03\surveys\survey\URY\ECH\2017\a\data_merge"
set more off
use "$ruta\H_2017_Terceros.dta", clear
sort numero
save "$ruta\H_2017_Terceros.dta", replace


use "$ruta\Personas_2017_Terceros_1.dta", clear
sort numero nper
save "$ruta\Personas_2017_Terceros_1.dta", replace

use "$ruta\Personas_2017_Terceros_2.dta", clear
sort numero nper
save "$ruta\Personas_2017_Terceros_2.dta", replace

*Merge 

use "$ruta\Personas_2017_Terceros_1.dta", clear
merge 1:1 numero nper using "$ruta\Personas_2017_Terceros_2.dta"
drop _merge 

merge m:1 numero using "H_2017_Terceros.dta"
sort numero nper
drop _merge

saveold "$out\URY_2017a.dta", version(12) replace

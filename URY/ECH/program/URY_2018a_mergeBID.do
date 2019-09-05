* Autor: Alvaro Altamirano
* Fecha: 15 de Mayo de 2019

global ruta "\\Sdssrv03\surveys\survey\URY\ECH\2018\a\data_orig"
global out "\\Sdssrv03\surveys\survey\URY\ECH\2018\a\data_merge"
set more off
use "$ruta\H_2018_Terceros.dta", clear
sort numero
save "$ruta\H_2018_Terceros.dta", replace


use "$ruta\Personas_2018_Terceros_1.dta", clear
sort numero nper
save "$ruta\Personas_2018_Terceros_1.dta", replace

use "$ruta\Personas_2018_Terceros_2.dta", clear
sort numero nper
save "$ruta\Personas_2018_Terceros_2.dta", replace

*Merge 

use "$ruta\Personas_2018_Terceros_1.dta", clear
merge 1:1 numero nper using "$ruta\Personas_2018_Terceros_2.dta"
drop _merge 

merge m:1 numero using "$ruta\H_2018_Terceros.dta"
sort numero nper
drop _merge

saveold "$out\URY_2018a.dta", version(12) replace

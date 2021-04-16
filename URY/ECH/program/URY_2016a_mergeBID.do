* Autor: Stephanie Gonzalez Rubio
* Fecha: 19 de abril de 2017

global ruta "${surveysFolder}\survey\URY\ECH\2016\a\data_orig"
global out "${surveysFolder}\survey\URY\ECH\2016\a\data_merge"
set more off
use "$ruta\encuesta_continua_de_hogares_2016_hogares.dta", clear
sort numero
save "$ruta\encuesta_continua_de_hogares_2016_hogares.dta", replace


use "$ruta\encuesta_continua_de_hogares_2016_personas.dta", clear
sort numero nper
save "$ruta\encuesta_continua_de_hogares_2016_personas.dta", replace

*Merge 

use "$ruta\encuesta_continua_de_hogares_2016_personas", clear
merge m:1 numero using "$ruta\encuesta_continua_de_hogares_2016_hogares.dta"
sort numero nper
drop _merge

saveold "$out\URY_2016a.dta", version(12) replace

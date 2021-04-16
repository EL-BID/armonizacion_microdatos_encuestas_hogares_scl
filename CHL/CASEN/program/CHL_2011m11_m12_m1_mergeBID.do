clear

*2013-MLO

global ruta = "${surveysFolder}"

local PAIS CHL
local ENCUESTA CASEN
local ANO "2011"
local ronda m11_m12_m1 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 

set more off

 /*base */
use "`base_in'\CHL_2011m11_m12_m1.dta", clear
sort folio o
save "`base_in'\CHL_2011m11_m12_m1_m.dta", replace

* modulo de ingresos -se usan los ingresos ajustados (lo hizo hasta 2011 CEPAL)
use "`base_in'\ingreso_ajustados_casen2011.dta", clear /*educación */
sort folio o
save "`base_in'\ingreso_ajustados_casen2011_m.dta", replace


***************
**** MERGE  ***
***************

use "`base_in'\CHL_2011m11_m12_m1_m.dta", clear
merge folio o using "`base_in'\ingreso_ajustados_casen2011_m.dta"
tab _merge
drop _merge


* Comprime y guarda base
compress
saveold "`base_out'", replace


erase "`base_in'\CHL_2011m11_m12_m1_m.dta"
erase "`base_in'\ingreso_ajustados_casen2011_m.dta"
log close

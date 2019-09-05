clear

*Marcela Rubio

global ruta = "\\Sdssrv03\surveys"

local PAIS CHL
local ENCUESTA CASEN
local ANO "2013"
local ronda m11_m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 

set more off

 /*base */
use "`base_in'\casen_2013_ingresos_originales.dta", clear
sort folio o
save "`base_in'\CHL_2011m11_m12_m1_m.dta", replace

* modulo de ingresos -se usan los ingresos ajustados (lo hizo hasta 2011 CEPAL)
use "`base_in'\ingreso_ajustados_casen2011.dta", clear /*educación */
sort folio o
save "`base_in'\ingreso_ajustados_casen2011_m.dta", replace


***************
**** MERGE  ***
***************

use "`base_in'\casen_2013_ingresos_originales.dta", clear
merge 1:1folio o using "`base_in'\casen_2013_mn_b_principal.dta"
drop _merge
merge 1:1folio o using "`base_in'\casen_2013_ymt.dta"
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close

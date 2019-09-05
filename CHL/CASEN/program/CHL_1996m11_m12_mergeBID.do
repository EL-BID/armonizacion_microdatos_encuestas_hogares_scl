clear

*2013-MLO

global ruta = "\\Sdssrv03\surveys"

local PAIS CHL
local ENCUESTA CASEN
local ANO "1996"
local ronda m11_m12 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 

set more off

 /*base */
use "`base_in'\casen1996.dta", clear
egen idp =concat(seg f o)
sort idp
save "`base_in'\CHL_1996m11_m12_m.dta", replace

* modulo de ingresos -se usan los ingresos con ajuste (lo hizo hasta 2011 CEPAL)
* en la base d eingresos originales no se pueden distinguir los montos
use "`base_in'\casen1996_ingresos_ajustados_.dta", clear 
egen idp =concat(seg f o)
sort idp
save "`base_in'\ingresos_ajustados_casen1996_m.dta", replace


***************
**** MERGE  ***
***************

use "`base_in'\CHL_1996m11_m12_m.dta", clear
merge idp using "`base_in'\ingresos_ajustados_casen1996_m.dta"
drop idp
tab _merge
drop _merge


* Comprime y guarda base
compress
saveold "`base_out'", replace


erase "`base_in'\CHL_1996m11_m12_m.dta"
erase "`base_in'\ingresos_ajustados_casen1996_m.dta"
log close

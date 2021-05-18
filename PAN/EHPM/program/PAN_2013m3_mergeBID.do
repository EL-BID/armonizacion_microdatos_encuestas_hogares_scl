
clear

*global ruta = "${surveysFolder}"

local PAIS PAN
local ENCUESTA EHPM
local ANO "2013"
local ronda m3 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 



					****************
					***MERGE 2010***
					****************
					
use "`base_in'\hogar.dta", clear
sort prov dist LLAVE_SEC estra unidad cuest hogar
save "`base_in'\hogar_mod.dta", replace

use "`base_in'\vivienda.dta", clear
sort prov dist LLAVE_SEC estra unidad cuest hogar
save "`base_in'\vivienda_mod.dta", replace

use "`base_in'\persona.dta", clear
sort prov dist LLAVE_SEC estra unidad cuest hogar
merge prov dist LLAVE_SEC estra unidad cuest hogar using "`base_in'\hogar_mod.dta"
tab _merge
drop _merge

sort prov dist LLAVE_SEC estra unidad cuest hogar
merge prov dist LLAVE_SEC estra unidad cuest hogar using "`base_in'\vivienda_mod.dta"
tab _merge
drop if _merge!=3
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close
erase "`base_in'\hogar_mod.dta"
erase "`base_in'\vivienda_mod.dta"

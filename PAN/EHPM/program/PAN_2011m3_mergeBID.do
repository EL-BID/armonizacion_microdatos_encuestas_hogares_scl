
clear

global ruta = "${surveysFolder}"

local PAIS PAN
local ENCUESTA EHPM
local ANO "2011"
local ronda m3 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


					****************
					***MERGE 2011***
					****************
					
use "`base_in'\hogar.dta", clear
*sort prov dist corre estra unidad cuest hogar
sort prov llave_unic cuest hogar
save "`base_in'\hogar_mod.dta", replace

use "`base_in'\vivienda.dta", clear
sort prov llave_unic cuest hogar
save "`base_in'\vivienda_mod.dta", replace

use "`base_in'\persona.dta", clear
sort prov llave_unic cuest hogar
merge prov llave_unic cuest hogar using "`base_in'\hogar_mod.dta"
tab _merge
drop _merge

//Ojo no se estan uniendo 569 observaciones - MLO: igualmente coincide con la poblacion reportada por el INEC
sort prov llave_unic cuest hogar
merge prov llave_unic cuest hogar using "`base_in'\vivienda_mod.dta"
tab _merge
keep if _merge==3
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close
erase "`base_in'\hogar_mod.dta"
erase "`base_in'\vivienda_mod.dta"

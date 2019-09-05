* Elaborado por: Daniela Zuluaga
* Fecha: Mayo 2018

clear

global ruta = "\\Sdssrv03\surveys"

local PAIS PAN
local ENCUESTA EHPM
local ANO "2017"
local ronda m3 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 



					****************
					***MERGE 2017***
					****************
					
use "`base_in'\hogar.dta", clear
sort llave_sec  hogar
saveold "`base_in'\hogar_mod.dta", replace

use "`base_in'\vivienda.dta", clear
sort llave_sec  hogar
saveold "`base_in'\vivienda_mod.dta", replace

use "`base_in'\persona.dta", clear
sort llave_sec  hogar
merge m:1 llave_sec hogar using "`base_in'\hogar_mod.dta"
drop _merge

sort llave_sec  hogar
merge m:1 llave_sec hogar using "`base_in'\vivienda_mod.dta"
drop if _merge!=3
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace v(12)

log close

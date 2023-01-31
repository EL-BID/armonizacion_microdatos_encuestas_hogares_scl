* Elaborado por: Eric Torres
* Fecha: Enero 2023

clear

global ruta = "${surveysFolder}"

local PAIS PAN
local ENCUESTA EHPM
local ANO "2022"
local ronda m3 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 



					****************
					***MERGE 2022***
					****************

use "`base_in'\vivienda.dta", clear
drop hogar_no
sort llave_sec provincia prov unidad cuestionar
tempfile vivienda_mod
save `vivienda_mod' , replace


use "`base_in'\hogar.dta", clear
rename hogar_no hogar
sort llave_sec  hogar
merge m:1 llave_sec provincia prov unidad cuestionar using `vivienda_mod'
drop _merge
tempfile hogar_viv
save `hogar_viv' , replace



use "`base_in'\poblacio.dta", clear
rename estra estrato
rename cuest cuestionar
sort llave_sec  provincia prov estrato unidad cuestionar hogar
merge m:1 llave_sec provincia prov estrato unidad cuestionar hogar using `hogar_viv' 

drop if _merge!=3
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace v(12)

log close

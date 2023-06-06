* Elaborado por: Daniela Zuluaga
* Fecha: Junio 2020

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
					
use "`base_in'\hogar.dta", clear
rename *, lower
rename hogar_no hogar
sort llave_sec  hogar
tempfile hogar_mod
save `hogar_mod' , replace

use "`base_in'\vivienda.dta", clear
rename hogar_no hogar
sort llave_sec  hogar
tempfile vivienda_mod
save `vivienda_mod' , replace

use "`base_in'\poblacion.dta", clear
sort llave_sec  hogar
merge m:1 llave_sec hogar using `hogar_mod' 
drop _merge

sort llave_sec  hogar
merge m:1 llave_sec hogar using `vivienda_mod'
drop if _merge!=3
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace v(12)

log close

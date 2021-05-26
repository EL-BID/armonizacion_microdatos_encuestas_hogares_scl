clear

global ruta = "${surveysFolder}"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2012"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 

set more off

 /*personas */
use "`base_in'\Ehpmind2012 (1).dta", clear
drop idp
egen idp =concat(lote tipo folio viv orden)
egen idh =concat(lote tipo folio viv)
sort idp
save "`base_in'\bdsec01_m.dta", replace


use "`base_in'\EHPMHOG12.dta", clear /*vivienda*/
egen idh =concat(lote tipo folio viv)
sort idh
save "`base_in'\bdsec02_m.dta", replace


***************
**** MERGE  ***
***************

use "`base_in'\bdsec01_m.dta", clear

merge m:1 idh using "`base_in'\bdsec02_m.dta"
drop _merge


* Comprime y guarda base
compress
saveold "`base_out'", replace


erase "`base_in'\bdsec01_m.dta"
erase "`base_in'\bdsec02_m.dta"

log close

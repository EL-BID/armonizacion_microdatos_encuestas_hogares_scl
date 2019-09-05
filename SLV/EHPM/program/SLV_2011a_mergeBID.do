clear

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2011"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 

set more off

 /*sociodemograficas */
use "`base_in'\bdsec01.dta", clear
drop idp
egen idp =concat(lote tipo folio viv r101)
sort idp
save "`base_in'\bdsec01_m.dta", replace


use "`base_in'\bdsec02.dta", clear /*educación */
drop idp
egen idp =concat(lote tipo folio viv r101)
sort idp
save "`base_in'\bdsec02_m.dta", replace

use "`base_in'\bdsec03.dta", clear /*vivienda*/
drop idh
egen idh =concat(lote tipo folio viv)
sort idh
save "`base_in'\bdsec03_m.dta", replace

use "`base_in'\bdsec04.dta", clear /*empleo */
drop idp
egen idp =concat(lote tipo folio viv r101)
sort idp
save "`base_in'\bdsec04_m.dta", replace

*falta unir la base 05 06 y 07

use "`base_in'\bdsec00.dta", clear /*construidas por dygestic*/
drop idh
egen idh =concat(lote tipo folio viv)
sort idh
save "`base_in'\bdsec00_m.dta", replace


***************
**** MERGE  ***
***************

use "`base_in'\bdsec01_m.dta", clear
merge 1:1 idp using "`base_in'\bdsec02_m.dta"
drop _merge
sort idp

merge 1:1 idp using "`base_in'\bdsec04_m.dta"
drop _merge idp
egen idh =concat(lote tipo folio viv)
sort idh

merge m:1 idh using "`base_in'\bdsec00_m.dta"
drop _merge
sort idh


merge m:1 idh using "`base_in'\bdsec03_m.dta"
drop _merge
egen idp =concat(lote tipo folio viv r101)
sort idp

* Comprime y guarda base
compress
saveold "`base_out'", replace


erase "`base_in'\bdsec01_m.dta"
erase "`base_in'\bdsec02_m.dta"
erase "`base_in'\bdsec03_m.dta"
erase "`base_in'\bdsec04_m.dta"
erase "`base_in'\bdsec00_m.dta"
log close

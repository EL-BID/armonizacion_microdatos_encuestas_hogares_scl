***********************
***** MERGE 2005 ******
***********************
clear

global ruta = "${surveysFolder}"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2005"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
set mem 150m

* Sort 

*SEC00
use "`base_in'\sec00.dta", clear
sort level1id
save "`base_in'\sec00_m.dta", replace

*SEC01
use "`base_in'\sec01.dta", clear
*gen double id_pers=r101
sort level1id id_pers
save "`base_in'\sec01_m.dta", replace

*SEC02
use "`base_in'\sec02.dta", clear
*gen double id_pers=r201
sort level1id id_pers
save "`base_in'\sec02_m.dta", replace

*SEC03
use "`base_in'\sec03.dta", clear
sort level1id
save "`base_in'\sec03_m.dta", replace

*SEC04
use "`base_in'\sec04.dta", clear
*gen double id_pers=r401
sort level1id id_pers
save "`base_in'\sec04_m.dta", replace

*SEC06
use "`base_in'\sec06.dta", clear
*gen double id_pers=r601
sort level1id id_pers
save "`base_in'\sec06_m.dta", replace

*SEC07
use "`base_in'\sec07.dta", clear
sort level1id
save "`base_in'\sec07_m.dta", replace

*** MERGE

**** Persons

use "`base_in'\sec01_m.dta", clear

merge level1id id_pers using "`base_in'\sec02_m.dta"

	tab _merge
	drop _merge
	sort level1id id_pers

merge level1id id_pers using "`base_in'\sec04_m.dta"

	tab _merge
	drop _merge
	sort level1id id_pers

merge level1id id_pers using "`base_in'\sec06_m.dta"

	tab _merge
	drop _merge
	sort level1id id_pers

**** Households

merge level1id using "`base_in'\sec00_m.dta"

	tab _merge
	drop _merge
	sort level1id 


merge level1id using "`base_in'\sec03_m.dta"

	tab _merge
	drop _merge
	sort level1id 

merge level1id using "`base_in'\sec07_m.dta"

	tab _merge
	drop _merge
	sort level1id 

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close

erase "`base_in'\sec00_m.dta"
erase "`base_in'\sec01_m.dta"
erase "`base_in'\sec02_m.dta"
erase "`base_in'\sec03_m.dta"
erase "`base_in'\sec04_m.dta"
erase "`base_in'\sec06_m.dta"
erase "`base_in'\sec07_m.dta"

***********************
***** MERGE 2007 ******
***********************

clear

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2007"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


* Sort 

*SEC00
use "`base_in'\sec00.dta", clear
sort lote  tipo  folio  viv  
save "`base_in'\sec00_m.dta", replace

*SEC01
use "`base_in'\sec01.dta", clear
cap gen numorden = r101
sort lote  tipo  folio  viv numorden 
save "`base_in'\sec01_m.dta", replace

*SEC02
use "`base_in'\sec02.dta", clear
cap gen numorden = r201
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec02_m.dta", replace

*SEC03
use "`base_in'\sec03.dta", clear
sort lote  tipo  folio  viv  
save "`base_in'\sec03_m.dta", replace

*SEC04
use "`base_in'\sec04.dta", clear
cap gen numorden = r401
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec04_m.dta", replace

*SEC06
use "`base_in'\sec06.dta", clear
cap gen numorden = r601
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec06_m.dta", replace

*SEC07
use "`base_in'\sec07.dta", clear
cap gen numorden = r701
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec07_m.dta", replace

*** MERGE

**** Persons

use "`base_in'\sec01_m.dta", clear

merge lote  tipo  folio  viv  numorden using "`base_in'\sec02_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  numorden 
	
merge lote  tipo  folio  viv  numorden using "`base_in'\sec04_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  numorden 
	
merge lote  tipo  folio  viv  numorden using "`base_in'\sec06_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

	
**** Households

merge lote  tipo  folio  viv   using "`base_in'\sec00_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

merge lote  tipo  folio  viv  using "`base_in'\sec03_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

merge lote  tipo  folio  viv  using "`base_in'\sec07_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

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


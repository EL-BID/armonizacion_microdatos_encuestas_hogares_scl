***********************
***** MERGE 2010 ******
***********************

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


* Sort 

*SEC00
use "`base_in'\SEC00.dta", clear
sort lote  tipo  folio  viv  
save "`base_in'\SEC00_m.dta", replace

*SEC01
use "`base_in'\SEC01.dta", clear
cap gen numorden = r101
sort lote  tipo  folio  viv numorden 
save "`base_in'\SEC01_m.dta", replace

*SEC02
use "`base_in'\SEC02.dta", clear
cap gen numorden = r101
sort lote  tipo  folio  viv  numorden
save "`base_in'\SEC02_m.dta", replace

*SEC03
use "`base_in'\SEC03.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
sort lote  tipo  folio  viv  
save "`base_in'\SEC03_m.dta", replace

*SEC04
use "`base_in'\SEC04.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
   cap gen numorden = r101
sort lote  tipo  folio  viv  numorden
save "`base_in'\SEC04_m.dta", replace

*SEC06
use "`base_in'\SEC06.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
cap gen numorden = r101
sort lote  tipo  folio  viv  numorden
save "`base_in'\SEC06_m.dta", replace

*SEC07
use "`base_in'\SEC07.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
cap gen numorden = r701
sort lote  tipo  folio  viv  numorden
save "`base_in'\SEC07_m.dta", replace

*** MERGE

**** Persons

use "`base_in'\SEC01_m.dta", clear

merge lote  tipo  folio  viv  numorden using "`base_in'\SEC02_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  numorden 
	
merge lote  tipo  folio  viv  numorden using "`base_in'\SEC04_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  numorden 
	
merge lote  tipo  folio  viv  numorden using "`base_in'\SEC06_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

	
**** Households

merge lote  tipo  folio  viv   using "`base_in'\SEC00_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

merge lote  tipo  folio  viv  using "`base_in'\SEC03_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

merge lote  tipo  folio  viv  using "`base_in'\SEC07_m.dta"

	tab _merge
	drop _merge
	sort lote  tipo  folio  viv  

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close

erase "`base_in'\SEC00_m.dta"
erase "`base_in'\SEC01_m.dta"
erase "`base_in'\SEC02_m.dta"
erase "`base_in'\SEC03_m.dta"
erase "`base_in'\SEC04_m.dta"
erase "`base_in'\SEC06_m.dta"
erase "`base_in'\SEC07_m.dta"

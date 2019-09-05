***********************
***** MERGE 2013 ******
***********************
*Mayra Sáenz Septiembre 2014
clear

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2013"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


* Sort 

*sec00
use "`base_in'\sec00.dta", clear
sort lote  tipo  folio  viv  
save "`base_in'\sec00_m.dta", replace

*sec01
use "`base_in'\sec01.dta", clear
cap gen numorden = r101
sort lote  tipo  folio  viv numorden 
save "`base_in'\sec01_m.dta", replace

*sec02
use "`base_in'\sec02.dta", clear
cap gen numorden = r101
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec02_m.dta", replace

*sec03
use "`base_in'\sec03.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
sort lote  tipo  folio  viv  
save "`base_in'\sec03_m.dta", replace

*sec04
use "`base_in'\sec04.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
   cap gen numorden = r101
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec04_m.dta", replace

*sec06
use "`base_in'\sec06.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
cap gen numorden = r101
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec06_m.dta", replace

*sec07
use "`base_in'\sec07.dta", clear
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
cap gen numorden = r701
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec07_m.dta", replace

*** merge

**** persons

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

	
**** households

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

* comprime y guarda base
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

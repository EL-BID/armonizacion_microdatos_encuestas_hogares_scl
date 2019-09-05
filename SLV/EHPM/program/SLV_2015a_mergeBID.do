***********************
***** MERGE 2014 ******
***********************
* Melany Gualavisi

clear

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2015"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
* Sort 

* seccion 0
use "`base_in'\sec00.dta", clear
*cap gen numorden = r101
duplicates report lote tipo folio viv 
sort lote  tipo  folio  viv  
save "`base_in'\sec00_n.dta", replace

* seccion 1
use "`base_in'\sec01.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv numorden  
save "`base_in'\sec01_n.dta", replace

* seccion 2
use "`base_in'\sec01a.dta", clear
cap gen numorden = r101 
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec01a_n.dta", replace


use "`base_in'\sec02.dta", clear
cap gen numorden = r101 
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec02_n.dta", replace

* seccion 3
use "`base_in'\sec02a.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv numorden
save "`base_in'\sec02a_n.dta", replace

* seccion 4
use "`base_in'\sec03.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv 
sort lote  tipo  folio  viv  
save "`base_in'\sec03_n.dta", replace

* seccion 5
use "`base_in'\sec04.dta", clear
rename r101 numorden
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec04_n.dta", replace

* seccion 6
use "`base_in'\sec05.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec05_n.dta", replace

* seccion 7
use "`base_in'\sec06.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec06_n.dta", replace

* seccion 8
use "`base_in'\sec07.dta", clear
cap gen numorden = r701
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\sec07_n.dta", replace



*** merge

**** persons

use "`base_in'\sec01_n.dta", clear

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec01a_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 
	
merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec02_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 
	
merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec02a_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec04_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec05_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec06_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\sec07_n.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

**** households

merge m:1 lote  tipo  folio  viv   using "`base_in'\sec00_n.dta"
drop _merge
sort lote  tipo  folio  viv  

merge m:1 lote  tipo  folio  viv  using "`base_in'\sec03_n.dta"
drop _merge
sort lote  tipo  folio  viv  




* comprime y guarda base
compress
saveold "`base_out'", replace


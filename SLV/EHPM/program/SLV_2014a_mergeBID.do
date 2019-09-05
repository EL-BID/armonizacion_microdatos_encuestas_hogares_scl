***********************
***** MERGE 2014 ******
***********************
* Marcela G. Rubio

clear

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2014"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\wetransfer-44a3b9\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
* Sort 

* seccion 0
use "`base_in'\seccion 0 hogar.dta", clear
duplicates report lote tipo folio viv
sort lote  tipo  folio  viv  
save "`base_in'\seccion 0 hogar.dta", replace

use "`base_in'\seccion_0_persona.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv numorden  
save "`base_in'\seccion_0_persona.dta", replace

* seccion 1
use "`base_in'\seccion_1_persona.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv numorden  
save "`base_in'\seccion_1_persona.dta", replace

* seccion 2
use "`base_in'\seccion_2_persona.dta", clear
cap gen numorden = r101 
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\seccion_2_persona.dta", replace


use "`base_in'\seccion_2_a_persona.dta", clear
cap gen numorden = r101 
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\seccion_2_a_persona.dta", replace

* seccion 3
use "`base_in'\seccion_3_hogar.dta", clear
duplicates report lote tipo folio viv 
sort lote  tipo  folio  viv 
save "`base_in'\seccion_3_hogar.dta", replace

* seccion 4
use "`base_in'\seccion_4_persona.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\seccion_4_persona.dta", replace

* seccion 5
use "`base_in'\seccion_5_persona.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\seccion_5_persona.dta", replace

* seccion 6
use "`base_in'\seccion_6_persona.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\seccion_6_persona.dta", replace

* seccion 7
use "`base_in'\seccion_7_hogar.dta", clear
cap gen numorden = r701
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
save "`base_in'\seccion_7_hogar.dta", replace

* seccion 8
use "`base_in'\seccion_8a.dta", clear
duplicates report lote  tipo  folio  viv r802a
sort lote  tipo  folio  viv  r802a
save "`base_in'\seccion_8a.dta", replace

* seccion 9
use "`base_in'\seccion_9_hogar.dta", clear
duplicates report lote  tipo  folio  viv 
sort lote  tipo  folio  viv  
save "`base_in'\seccion_9_hogar.dta", replace

* resumen hogar
use "`base_in'\seccion_resumen_hogar.dta", clear
duplicates report lote  tipo  folio  viv 
sort lote  tipo  folio  viv  
save "`base_in'\seccion_resumen_hogar.dta", replace


*** merge

**** persons

use "`base_in'\seccion_0_persona.dta", clear

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\seccion_1_persona.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 
	
merge m:m lote  tipo  folio  viv  numorden using "`base_in'\seccion_2_a_persona.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 
	
merge m:m lote  tipo  folio  viv  numorden using "`base_in'\seccion_2_persona.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\seccion_4_persona.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\seccion_5_persona.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 

merge m:m lote  tipo  folio  viv  numorden using "`base_in'\seccion_6_persona.dta"
drop _merge
sort lote  tipo  folio  viv  numorden 


**** households

merge m:1 lote  tipo  folio  viv   using "`base_in'\seccion 0 hogar.dta"
drop _merge
sort lote  tipo  folio  viv  

merge m:1 lote  tipo  folio  viv  using "`base_in'\seccion_3_hogar.dta"
drop _merge
sort lote  tipo  folio  viv  

merge m:1 lote  tipo  folio  viv  using "`base_in'\seccion_7_hogar.dta"
drop _merge
sort lote  tipo  folio  viv  

merge m:1 lote  tipo  folio  viv  using "`base_in'\seccion_9_hogar.dta"
drop _merge
sort lote  tipo  folio  viv  

merge m:1 lote  tipo  folio  viv  using "`base_in'\seccion_resumen_hogar.dta"
drop _merge
sort lote  tipo  folio  viv  


* comprime y guarda base
compress
saveold "`base_out'", replace


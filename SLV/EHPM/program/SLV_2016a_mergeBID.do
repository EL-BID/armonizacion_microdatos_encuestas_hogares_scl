***********************
***** MERGE 2016 ******
***********************
* Daniela Zuluaga, danielazu@iadb.org

clear

global ruta = "${surveysFolder}"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2016"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
* Sort 

* ehpm_2016
use "`base_in'\ehpm_2016.dta", clear
cap gen numorden = r101
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv numorden
saveold "`base_in'\ehpm_2016_n.dta", v(12) replace

* seccion 08a
use "`base_in'\ehpm_2016_sec08a.dta", clear
cap gen numorden = orden
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv numorden  
saveold "`base_in'\ehpm_2016_sec08a_n.dta", v(12) replace

* seccion 08b
use "`base_in'\ehpm_2016_sec08b.dta", clear
cap gen numorden = orden 
duplicates report lote tipo folio viv numorden
sort lote  tipo  folio  viv  numorden
saveold "`base_in'\ehpm_2016_sec08b_n.dta", v(12) replace

*seccion 09
use "`base_in'\ehpm_2016_sec09.dta", clear
cap gen numorden = orden 
duplicates report lote tipo folio viv 
sort lote  tipo  folio  viv 
saveold "`base_in'\ehpm_2016_sec09_n.dta", v(12) replace



*** merge

**** hogares y personas 

use "`base_in'\ehpm_2016_n.dta", clear

merge m:1 idboleta  using "`base_in'\ehpm_2016_sec09_n.dta"
drop _merge
sort lote  tipo  folio  viv  


* comprime y guarda base
compress
saveold "`base_out'", v(12) replace

clear
			
			*JSLC 2018*
			***********

*Se consolida una base para construir los indicadores de sociómetro. No obstante, se cuenta con más módulos 
*que el investigador puede unir a este consolidado. 

global ruta = "${surveysFolder}"

local PAIS JAM
local ENCUESTA SLC
local ANO "2019"
local ronda m7_m10 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
  

use "`base_in'\rec001.dta", clear
sort SERIAL
save, replace

use "`base_in'\rec002.dta", clear
sort SERIAL
save, replace

use "`base_in'\rec003.dta", clear
sort SERIAL ind
save, replace

use "`base_in'\rec004.dta", clear
sort SERIAL ind
save, replace

use "`base_in'\rec005.dta", clear
sort SERIAL ind
save, replace

use "`base_in'\rec006.dta", clear
sort SERIAL ind
save, replace

use "`base_in'\rec007.dta", clear	// social protection module
sort SERIAL 
save , replace

use "`base_in'\rec016.dta", clear
sort SERIAL ind
save, replace

use "`base_in'\rec013.dta", clear
sort SERIAL 
save, replace

use "`base_in'\annual.dta", clear
sort SERIAL
save, replace

use "`base_in'\path19.dta", clear
*tostring AGEYRS, replace
sort SERIAL ind
save, replace

use "`base_in'\povline19.dta", clear
*tostring AGEYRS, replace
sort SERIAL ind
save, replace

use "`base_in'\rec017.dta", clear
sort SERIAL
save, replace

use "`base_in'\rec018.dta", clear    // MISCELLANEOUS - RECEIVED FROM SOURCES OUTSIDE OF HOUSEHOLD (includes public assistance and NIS)
tostring SERIAL, replace
sort SERIAL ind
save , replace


* merge
*-------

use "`base_in'\rec003.dta", clear
merge m:1 SERIAL using "`base_in'\rec001.dta"
drop _merge 
sort SERIAL ind

merge m:1 SERIAL using "`base_in'\rec002.dta"
drop _merge 
sort SERIAL ind

merge m:1 SERIAL ind using "`base_in'\rec004.dta"
drop _merge 
sort SERIAL ind

merge m:1 SERIAL ind using "`base_in'\rec005.dta"
drop _merge 
sort SERIAL ind

merge m:1 SERIAL using "`base_in'\rec013.dta"
drop _merge 
sort SERIAL ind

*merge m:1 SERIAL ind using "`base_in'\rec006.dta"
*drop _merge 
*sort SERIAL ind

*merge m:1 SERIAL using "`base_in'\rec007.dta"
*drop _merge 
*sort SERIAL 

merge m:1 SERIAL using "`base_in'\annual.dta"
drop _merge 
sort SERIAL ind

merge m:1 SERIAL ind using "`base_in'\path19.dta"
drop _merge 
sort SERIAL ind

*merge m:1 SERIAL ind using "`base_in'\rec016.dta"
*drop _merge 
*sort SERIAL ind

merge m:m SERIAL ind using "`base_in'\povline19.dta"
drop _merge 
sort SERIAL ind

merge m:m SERIAL using "`base_in'\rec017.dta"
drop _merge 
sort SERIAL ind

merge m:m SERIAL ind using "`base_in'\rec018.dta"
drop _merge 
sort SERIAL ind

/*
foreach var of varlist ageyrs-j625{
destring `var', replace
}
*/

* comprime y guarda base
compress
saveold "`base_out'", replace














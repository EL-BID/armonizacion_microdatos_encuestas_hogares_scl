* Daniela Zuluaga
			
			*JSLC 2014*
			***********

*Se consolida una base para construir los indicadores de sociómetro. No obstante, se cuenta con más módulos 
*que el investigador puede unir a este consolidado. 

global ruta = "${surveysFolder}"

local PAIS JAM
local ENCUESTA SLC
local ANO "2014"
local ronda m6_m11 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
  

use "`base_in'\rec001.dta", clear
sort serial
save, replace

use "`base_in'\rec002.dta", clear
sort serial
save, replace

use "`base_in'\rec003.dta", clear
sort serial ind
save, replace

use "`base_in'\rec004.dta", clear
sort serial ind
save, replace

use "`base_in'\rec005.dta", clear
sort serial ind
save, replace


use "`base_in'\rec013.dta", clear
sort serial 
save, replace

use "`base_in'\rec022.dta", clear
sort serial 
save, replace

use "`base_in'\rec017.dta", clear
sort serial record
save, replace

use "`base_in'\rec018.dta", clear
sort serial record
save, replace

use "`base_in'\annual.dta", clear
sort serial
save, replace

use "`base_in'\path14.dta", clear
sort serial ind
save, replace

use "`base_in'\povline14.dta", clear
sort serial ind
save, replace




* merge
*-------


use "`base_in'\rec003.dta", clear
merge m:1 serial using "`base_in'\rec001.dta"
drop _merge 
sort serial ind

merge m:1 serial using "`base_in'\rec002.dta"
drop _merge 
sort serial ind

merge m:1 serial ind using "`base_in'\rec004.dta"
drop _merge 
sort serial ind

merge m:1 serial ind using "`base_in'\rec005.dta"
drop _merge 
sort serial ind

merge m:1 serial using "`base_in'\annual.dta"
drop _merge 
sort serial ind

merge m:1 serial ind using "`base_in'\path14.dta"
drop _merge 
sort serial ind

merge m:m serial ind using "`base_in'\povline14.dta"
drop _merge 
sort serial ind

merge m:m serial using "`base_in'\rec022.dta"
drop _merge 
sort serial


merge m:1 serial using "`base_in'\rec017.dta"
drop _merge 
sort serial ind

merge m:m serial using "`base_in'\rec018.dta"
drop _merge 
sort serial ind

order edwght finwght a9 a10 a20 a21 b5_distance b6_distance b7_distance t_meal t_noncon i21_1 tot_tax tot_wat tot_telec tot_telel tot_mort rent hhsize1 hhsize2 tcgift, last
order electric-popquint, last
order a13 a16, last
order povline-per, last

foreach var of varlist age-j625{
destring `var', replace
}

* comprime y guarda base
compress
saveold "`base_out'", replace











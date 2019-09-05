**********************************************************
***PROGRAMA PARA CREAR LA BASE PARA EL SALVADOR 2002***
**********************************************************

*** PRIMERO SE ARMARON UNA SERIE DE DICCIONARIOS PARA***
clear

set mem 800m
set more off

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2002"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 



*** IDENTIMUES
infile using "`base_in'\slv02ident.dct"
keep if record=="00"
compress
sort tipo folio
save "`base_in'\slv02ident.dta", replace
clear

*** DEMOGRAFIA
infile using "`base_in'\slv02dem.dct"
keep if record=="01"
compress
sort tipo folio nrorden
save "`base_in'\slv02dem.dta", replace
clear

** EDUCACION
infile using "`base_in'\slv02edu.dct"
keep if record=="02"
compress
sort tipo folio nrorden
save "`base_in'\slv02edu.dta", replace
clear

*** EMPLEO
infile using "`base_in'\slv02emp.dct"
keep if record=="04"
compress
sort tipo folio nrorden
save "`base_in'\slv02emp.dta", replace
clear

*** 09
infile using "`base_in'\slv0209.dct"
keep if record=="09"
compress
sort tipo folio nrorden
save "`base_in'\slv0209.dta", replace
clear

*** SALUD
infile using "`base_in'\slv02salud.dct"
keep if record=="06"
compress
sort tipo folio nrorden
save "`base_in'\slv02salud.dta", replace
clear


*** VIVIENDA
infile using "`base_in'\slv02viv.dct"
keep if record=="03"
compress
sort tipo folio
save "`base_in'\slv02viv.dta", replace
clear

*** REMESAS
infile using "`base_in'\slv02rem.dct"
keep if record=="07"
compress
sort tipo folio
save "`base_in'\slv02rem.dta", replace
clear


*** GASTO
infile using "`base_in'\slv0281.dct"
keep if record=="81"
compress
sort tipo folio
save"`base_in'\slv0281.dta", replace
clear

infile using "`base_in'\slv0282.dct"
keep if record=="82"
compress
sort tipo folio
save "`base_in'\slv0282.dta", replace
clear

infile using "`base_in'\slv0283.dct"
keep if record=="83"
compress
sort tipo folio
save "`base_in'\slv0283.dta", replace
clear

infile using "`base_in'\slv0284.dct"
keep if record=="84"
compress
sort tipo folio
save "`base_in'\slv0284.dta", replace
clear

infile using "`base_in'\slv0285.dct"
keep if record=="85"
compress
sort tipo folio
save "`base_in'\slv0285.dta", replace
clear

*OA

infile using "`base_in'\slv02oA.dct"
keep if record=="0A"
compress
sort tipo folio
save "`base_in'\slv02oA.dta", replace
clear


*** NOW WE MERGE THE FILES

use "`base_in'\slv02ident.dta"

sort tipo folio
merge tipo folio using "`base_in'\slv02dem.dta"
rename _merge merge1
tab merge1
sort tipo folio nrorden

merge tipo folio nrorden using "`base_in'\slv02emp.dta"
rename _merge merge2
tab merge2
sort tipo folio nrorden


merge tipo folio nrorden using "`base_in'\slv02edu.dta"
rename _merge merge3
tab merge3
sort tipo folio nrorden

merge tipo folio nrorden using "`base_in'\slv02salud.dta"
rename _merge merge12
tab merge12
sort tipo folio nrorden


merge tipo folio nrorden using "`base_in'\slv0209.dta"
rename _merge merge4
tab merge4
sort tipo folio nrorden


merge tipo folio using "`base_in'\slv02rem.dta"
rename _merge merge5
tab merge5
sort tipo folio nrorden

merge tipo folio using "`base_in'\slv02viv.dta"
rename _merge merge6
tab merge6
sort tipo folio nrorden
drop merge6

/*
merge tipo folio using "`base_in'\slv0281.dta"
rename _merge merge7
tab merge7
sort tipo folio nrorden


merge tipo folio using "`base_in'\slv0282.dta"
rename _merge merge8
tab merge8
sort tipo folio nrorden


merge tipo folio using "`base_in'\slv0283.dta"
rename _merge merge9
tab merge9
sort tipo folio nrorden


merge tipo folio using  "`base_in'\slv0284.dta"
rename _merge merge10
tab merge10
sort tipo folio nrorden


merge tipo folio using "`base_in'\slv0285.dta"
rename _merge merge11
tab merge11
sort tipo folio nrorden
*/

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close




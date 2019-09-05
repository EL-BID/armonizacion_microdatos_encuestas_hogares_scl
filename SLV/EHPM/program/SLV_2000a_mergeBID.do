**********************************************************
***PROGRAMA PARA CREAR LA VANILLA PARA EL SALVADOR 2000***
**********************************************************

clear
set mem 400m
set more off

clear

global ruta = "\\Sdssrv03\surveys"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2000"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


*** IDENTIMUES
infile using "`base_in'\slv00ident.dct"
keep if record==00
compress
sort tipo folio
save "`base_in'\slv00ident.dta",  replace
clear

*** DEMOGRAFIA
infile using  "`base_in'\slv00dem.dct"
keep if record==01
compress
sort tipo folio nrorden
save  "`base_in'\slv00dem.dta",  replace
clear

** EDUCACION
infile using  "`base_in'\slv00edu.dct"
keep if record==02
compress
sort tipo folio nrorden
save  "`base_in'\slv00edu.dta",  replace
clear

*** EMPLEO
set mem 400m
infile using  "`base_in'\slv00emp.dct"
keep if record==04
compress
sort tipo folio nrorden
save  "`base_in'\slv00emp.dta",  replace
clear

*** VIVIENDA
infile using  "`base_in'\slv00viv.dct"
keep if record=="03"
compress
sort tipo folio
save  "`base_in'\slv00viv.dta",  replace
clear

*** REMESAS
infile using  "`base_in'\slv00rem.dct"
keep if record=="07"
compress
sort tipo folio
save  "`base_in'\slv00rem.dta",  replace
clear

*** NOW WE MERGE THE FILES

use  "`base_in'\slv00ident.dta"

sort tipo folio
merge tipo folio using  "`base_in'\slv00dem.dta"
rename _merge merge1
tab merge1
sort tipo folio nrorden

merge tipo folio nrorden using  "`base_in'\slv00emp.dta"
rename _merge merge2
tab merge2
sort tipo folio nrorden

merge tipo folio nrorden using  "`base_in'\slv00edu.dta"
rename _merge merge3
tab merge3
sort tipo folio nrorden

merge tipo folio using  "`base_in'\slv00rem.dta"
rename _merge merge4
tab merge4
sort tipo folio nrorden

merge tipo folio using  "`base_in'\slv00viv.dta"
rename _merge merge5
tab merge5
sort tipo folio nrorden

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


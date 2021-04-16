**********************************************************
***PROGRAMA PARA CREAR LA VANILLA PARA EL SALVADOR 2000***
**********************************************************

clear
set mem 400m
set more off

clear

global ruta = "${surveysFolder}\DATA.IDB\Documents\Marcela\Research\Sociometro\"

local PAIS SLV
local ENCUESTA EHPM
local ANO "1999"
local ronda a 

local log_file = "$ruta\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\\`PAIS'\\`ANO'\\EHPM`ANO'_nueva\"
local base_out = "$ruta\\`PAIS'\\`ANO'\\`PAIS'_`ANO'`ronda'_BID_merge.dta"
   
capture log close
log using "`log_file'", replace 


*** IDENTIMUES
use "`base_in'\sec00.dta", clear
duplicates report tipo folio 
sort tipo folio 
save "`base_in'\`PAIS'`ANO'ident.dta",  replace
clear

*** DEMOGRAFIA
use "`base_in'\sec01.dta", clear
duplicates report tipo folio r101
rename r101 nrorden
sort tipo folio nrorden
save  "`base_in'\`PAIS'`ANO'dem.dta",  replace
clear

** EDUCACION
use "`base_in'\sec02.dta"
rename r201 nrorden
* hay un caso en el que hay 2 personas con el mismo identificador dentro de la familia, 3, y falta alguien con el identificador 5. Aleatoreamente asignamos el identificador 5 entre ambos inviduos
replace nrorden = 5 if lote==150 &  tipo==0 & folio==11541 & nrorden==3 & r219a==3 & r219b==12
duplicates report tipo folio nrorden
sort tipo folio nrorden
save  "`base_in'\`PAIS'`ANO'edu.dta",  replace
clear

*** EMPLEO
use  "`base_in'\sec04.dta"
duplicates report tipo folio r101
rename r101 nrorden
sort tipo folio nrorden
save  "`base_in'\`PAIS'`ANO'emp.dta",  replace
clear

*** VIVIENDA
use "`base_in'\sec03.dta"
duplicates report tipo folio
sort tipo folio
save  "`base_in'\`PAIS'`ANO'viv.dta",  replace
clear

*** REMESAS
use "`base_in'\sec07.dta"
duplicates report tipo folio
sort tipo folio
save  "`base_in'\`PAIS'`ANO'rem.dta",  replace
clear

*** NOW WE MERGE THE FILES

use  "`base_in'\`PAIS'`ANO'dem.dta"

sort tipo folio
merge m:1 tipo folio using  "`base_in'\`PAIS'`ANO'ident.dta"
rename _merge merge1
sort tipo folio nrorden

sort tipo folio nrorden
merge 1:1 tipo folio nrorden using  "`base_in'\`PAIS'`ANO'emp.dta"
rename _merge merge2
sort tipo folio nrorden


merge 1:1 tipo folio nrorden using  "`base_in'\`PAIS'`ANO'edu.dta"
rename _merge merge3
sort tipo folio nrorden

merge m:1 tipo folio using  "`base_in'\`PAIS'`ANO'rem.dta"
rename _merge merge4
sort tipo folio nrorden

merge m:1 tipo folio using  "`base_in'\`PAIS'`ANO'viv.dta"
rename _merge merge5
sort tipo folio nrorden


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


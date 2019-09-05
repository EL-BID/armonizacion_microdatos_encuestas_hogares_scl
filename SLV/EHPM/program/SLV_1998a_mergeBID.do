**************************************************************
***PROGRAMA PARA HACER MERGE DE BASES PARA EL SALVADOR 1997***
**************************************************************

/*No se uso este do file*/

clear
set mem 400m
set more off
clear

global ruta = "D:\DATA.IDB\Documents\Marcela\Research\Sociometro\"


local PAIS SLV
local ENCUESTA EHPM
local ANO "1998"
local ronda a 

local in  = "$ruta\\`PAIS'\\`ANO'\\EHPM`ANO'_nueva\"
local out = "$ruta\\`PAIS'\\`ANO'\\`PAIS'_`ANO'`ronda'_BID_merge.dta"
   

*** IDENTIMUES
use "`in'\t1eh0.dta", clear
duplicates report tipo_de_ folio
sort tipo_de_ folio 
save "$ruta\\`PAIS'\\`ANO'\slv98ident.dta",  replace
clear

*** DEMOGRAFIA & EDUCACIÓN
use "`in'\t1eh1.dta", clear
duplicates report tipo_de_ folio r101_num
rename r101_num nrorden
sort tipo_de_ folio nrorden
save "$ruta\\`PAIS'\\`ANO'\slv98dem.dta",  replace
clear

*** EMPLEO
set mem 400m
use "`in'\t4eh1.dta", clear
duplicates report tipo_de_ folio r400_num
rename r400_num nrorden
sort tipo_de_ folio nrorden
save "$ruta\\`PAIS'\\`ANO'\slv98emp.dta",  replace
clear

*** VIVIENDA
use "`in'\t3eh1.dta", clear
duplicates report tipo_de_ folio
sort tipo_de_ folio
save "$ruta\\`PAIS'\\`ANO'\slv98viv.dta",  replace
clear

*** REMESAS
use "`in'\t7eh1.dta", clear
duplicates report tipo_de_ folio
sort tipo_de_ folio
save "$ruta\\`PAIS'\\`ANO'\slv98rem.dta",  replace
clear

*** NOW WE MERGE THE FILES

use "$ruta\\`PAIS'\\`ANO'\slv98dem.dta", clear

sort tipo_de_ folio 
merge m:1 tipo_de_ folio using "$ruta\\`PAIS'\\`ANO'\slv98ident.dta"
sort tipo_de_ folio nrorden
rename _merge merge1

merge 1:1 tipo_de_ folio nrorden using "$ruta\\`PAIS'\\`ANO'\slv98emp.dta"
sort tipo_de_ folio nrorden
rename _merge merge2

merge m:1 tipo_de_ folio  using "$ruta\\`PAIS'\\`ANO'\slv98rem.dta"
sort tipo_de_ folio nrorden
rename _merge merge3

merge m:1 tipo_de_ folio  using "$ruta\\`PAIS'\\`ANO'\slv98viv.dta"
sort tipo_de_ folio nrorden
rename _merge merge4
drop merge*


* Comprime y guarda base
compress
saveold "`out'", replace

capture log close

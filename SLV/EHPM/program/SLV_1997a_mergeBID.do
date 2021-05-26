**************************************************************
***PROGRAMA PARA HACER MERGE DE BASES PARA EL SALVADOR 1997***
**************************************************************

/*No se uso este do file*/

clear
set mem 400m
set more off
clear

global ruta = "${surveysFolder}\DATA.IDB\Documents\Marcela\Research\Sociometro\SLV\1998\"


local PAIS SLV
local ENCUESTA EHPM
local ANO "1998"
local ronda a 

local in  = "$ruta\EHPM1997_nueva\SPSS\"
local out = "$ruta\\`PAIS'_`ANO'`ronda'_BID_merge.dta"
   

*** IDENTIMUES
use "`in'\t1eh0.dta", clear
duplicates report folio tipo_de_
sort tipo_de_ folio 
save "$ruta\slv97ident.dta",  replace
clear

*** DEMOGRAFIA
use "`in'\t1eh1.dta", clear
duplicates report tipo_de_ folio r101_num
sort tipo_de_ folio r101_num
save "$ruta\slv97dem.dta",  replace
clear

*** EMPLEO
set mem 400m
use "`in'\t2eh1.dta", clear
duplicates report tipo_de_ folio r200_num
rename r200_num r101_num
sort tipo_de_ folio r101_num
save "$ruta\slv97emp.dta",  replace
clear

*** VIVIENDA
use "`in'\t3eh1.dta", clear
duplicates report tipo_de_ folio
sort tipo_de_ folio
save "$ruta\slv97viv.dta",  replace
clear

*** REMESAS
use "`in'\t4eh2.dta", clear
duplicates report tipo_de_ folio
sort tipo_de_ folio
save "$ruta\slv97rem.dta",  replace
clear

*** NOW WE MERGE THE FILES

use "$ruta\slv97dem.dta", clear

sort tipo_de_ folio 
merge m:1 tipo_de_ folio using "$ruta\slv97ident.dta"
sort tipo_de_ folio r101_num
rename _merge merge1

merge 1:1 tipo_de_ folio r101_num using "$ruta\slv97emp.dta"
sort tipo_de_ folio r101_num
rename _merge merge2

merge m:1 tipo_de_ folio  using "$ruta\slv97rem.dta"
sort tipo_de_ folio r101_num
rename _merge merge3

merge m:1 tipo_de_ folio  using "$ruta\slv97viv.dta"
sort tipo_de_ folio r101_num
rename _merge merge4
drop merge*


* Comprime y guarda base
compress
saveold "`out'", replace

capture log close

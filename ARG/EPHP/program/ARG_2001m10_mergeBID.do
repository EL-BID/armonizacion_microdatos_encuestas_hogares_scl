clear

global ruta = "\\Sdssrv03\surveys"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2001"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 = "`base_in'\bba013ba.dta";
local data2 = "`base_in'\cat013ba.dta";
local data3 = "`base_in'\com013ba.dta";
local data4 = "`base_in'\concor013ba.dta";
local data5 = "`base_in'\cor013ba.dta";
local data6 = "`base_in'\corrien013ba.dta";
local data7 = "`base_in'\for013ba.dta";
local data8 = "`base_in'\gall013ba.dta";
local data9 = "`base_in'\gba013ba.dta";
local data10 = "`base_in'\juj013ba.dta";
local data11 = "`base_in'\lapampa013ba.dta";
local data12 = "`base_in'\larioja013ba.dta";
local data13 = "`base_in'\lpl013ba.dta";
local data14 = "`base_in'\mdp013ba.dta";
local data15 = "`base_in'\men013ba.dta";
local data16 = "`base_in'\neu013ba.dta";
local data17 = "`base_in'\par013ba.dta";
local data18 = "`base_in'\pos013ba.dta";
local data19 = "`base_in'\rcuarto013ba.dta";
local data20 = "`base_in'\res013ba.dta";
local data21 = "`base_in'\ros013ba.dta";
local data22 = "`base_in'\sal013ba.dta";
local data23 = "`base_in'\sgo013ba.dta";
local data24 = "`base_in'\sju013ba.dta";
local data25 = "`base_in'\slu013ba.dta";
local data26 = "`base_in'\tfu013ba.dta";
local data27 = "`base_in'\tuc013ba.dta";
local data28 = "`base_in'\sfe013ba.dta";
*local data29 = "`base_in'\rawson013ba.dta";
*local data30 = "`base_in'\sannic013ba.dta";
*local data31 = "`base_in'\viedma013ba.dta";

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}
sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog013ba.dta"
local data2 = "`base_in'\cathog013ba.dta"
local data3 = "`base_in'\comhog013ba.dta"
local data4 = "`base_in'\concorhog013ba.dta"
local data5 = "`base_in'\corhog013ba.dta"
local data6 = "`base_in'\corrienhog013ba.dta"
local data7 = "`base_in'\forhog013ba.dta"
local data8 = "`base_in'\gallhog013ba.dta"
local data9 = "`base_in'\gbahog013ba.dta"
local data10 = "`base_in'\jujhog013ba.dta"
local data11 = "`base_in'\lapampahog013ba.dta"
local data12 = "`base_in'\lariojahog013ba.dta"
local data13 = "`base_in'\lplhog013ba.dta"
local data14 = "`base_in'\mdphog013ba.dta"
local data15 = "`base_in'\menhog013ba.dta"
local data16 = "`base_in'\neuhog013ba.dta"
local data17 = "`base_in'\parhog013ba.dta"
local data18 = "`base_in'\poshog013ba.dta"
local data19 = "`base_in'\rcuartohog013ba.dta"
local data20 = "`base_in'\reshog013ba.dta"
local data21 = "`base_in'\roshog013ba.dta"
local data22 = "`base_in'\salhog013ba.dta"
local data23 = "`base_in'\sgohog013ba.dta"
local data24 = "`base_in'\sjuhog013ba.dta"
local data25 = "`base_in'\sluhog013ba.dta"
local data26 = "`base_in'\tfuhog013ba.dta"
local data27 = "`base_in'\tuchog013ba.dta"
local data28 = "`base_in'\sfehog013ba.dta"
*local data29 = "`base_in'\rawsonhog013ba.dta";
*local data30 = "`base_in'\sannichog013ba.dta";
*local data31 = "`base_in'\viedmahog013ba.dta";

*MGR Jul, 2015: se había agregado una base de personas por lo que habían muchos duplicados.

* Otro problema está en que base de personas tiene variables con el mismo nombre de variables de vivienda, renombramos estas para hacer merge
* Bases originales quedan en carpeta backup


forvalues i=1/28 {
use `data`i'', clear
foreach var of varlist p01-r06 {
rename `var' `var'_hog
saveold `data`i'', replace
}
}


use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

sort codusu aglomera
save "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta", replace

* merge de hogares e individuos

use "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

*gen codusu=cod  if `ANO'>=1992 & `ANO'<=1997

sort codusu aglomera
merge m:1 codusu aglomera using "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta"
compress
table _merge
drop if _merge!=3


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


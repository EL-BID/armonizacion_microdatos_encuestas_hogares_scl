clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1999"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 = "`base_in'\bba993ba.dta";
local data2 = "`base_in'\cat993ba.dta";
local data3 = "`base_in'\com993ba.dta";
local data4 = "`base_in'\concor993ba.dta";
local data5 = "`base_in'\cor993ba.dta";
local data6 = "`base_in'\corrien993ba.dta";
local data7 = "`base_in'\for993ba.dta";
local data8 = "`base_in'\gall993ba.dta";
local data9 = "`base_in'\gba993ba.dta";
local data10 = "`base_in'\juj993ba.dta";
local data11 = "`base_in'\lapampa993ba.dta";
local data12 = "`base_in'\larioja993ba.dta";
local data13 = "`base_in'\lpl993ba.dta";
local data14 = "`base_in'\mdp993ba.dta";
local data15 = "`base_in'\men993ba.dta";
local data16 = "`base_in'\neu993ba.dta";
local data17 = "`base_in'\par993ba.dta";
local data18 = "`base_in'\pos993ba.dta";
local data19 = "`base_in'\rcuarto993ba.dta";
local data20 = "`base_in'\res993ba.dta";
local data21 = "`base_in'\ros993ba.dta";
local data22 = "`base_in'\sal993ba.dta";
local data23 = "`base_in'\sgo993ba.dta";
local data24 = "`base_in'\sju993ba.dta";
local data25 = "`base_in'\slu993ba.dta";
local data26 = "`base_in'\tfu993ba.dta";
local data27 = "`base_in'\tuc993ba.dta";
local data28 = "`base_in'\sfe993ba.dta";
*local data29 = "`base_in'\rawson993ba.dta";
*local data30 = "`base_in'\sannic993ba.dta";
*local data31 = "`base_in'\viedma993ba.dta";

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog993ba.dta"
local data2 = "`base_in'\cathog993ba.dta"
local data3 = "`base_in'\comhog993ba.dta"
local data4 = "`base_in'\concorhog993ba.dta"
local data5 = "`base_in'\corhog993ba.dta"
local data6 = "`base_in'\corrienhog993ba.dta"
local data7 = "`base_in'\forhog993ba.dta"
local data8 = "`base_in'\gallhog993ba.dta"
local data9 = "`base_in'\gbahog993ba.dta"
local data10 = "`base_in'\jujhog993ba.dta"
local data11 = "`base_in'\lapampahog993ba.dta"
local data12 = "`base_in'\lariojahog993ba.dta"
local data13 = "`base_in'\lplhog993ba.dta"
local data14 = "`base_in'\mdphog993ba.dta"
local data15 = "`base_in'\menhog993ba.dta"
local data16 = "`base_in'\neuhog993ba.dta"
local data17 = "`base_in'\parhog993ba.dta"
local data18 = "`base_in'\poshog993ba.dta"
local data19 = "`base_in'\rcuartohog993ba.dta"
local data20 = "`base_in'\reshog993ba.dta"
local data21 = "`base_in'\roshog993ba.dta"
local data22 = "`base_in'\salhog993ba.dta"
local data23 = "`base_in'\sgohog993ba.dta"
local data24 = "`base_in'\sjuhog993ba.dta"
local data25 = "`base_in'\sluhog993ba.dta"
local data26 = "`base_in'\tfuhog993ba.dta"
local data27 = "`base_in'\tuchog993ba.dta"
local data28 = "`base_in'\sfehog993ba.dta"
*local data29 = "`base_in'\rawsonhog993ba.dta";
*local data30 = "`base_in'\sannichog993ba.dta";
*local data31 = "`base_in'\viedmahog993ba.dta";

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

use "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta"

*gen codusu=cod  if `ANO'>=1992 & `ANO'<=1997

sort codusu aglomera
merge m:1 codusu aglomera using "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta"
compress
table _merge
drop if _merge!=3
drop _merge

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


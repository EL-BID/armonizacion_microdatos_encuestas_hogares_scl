clear

global ruta = "\\Sdssrv03\surveys"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2000"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 = "`base_in'\bba003ba.dta";
local data2 = "`base_in'\cat003ba.dta";
local data3 = "`base_in'\com003ba.dta";
local data4 = "`base_in'\concor003ba.dta";
local data5 = "`base_in'\cor003ba.dta";
local data6 = "`base_in'\corrien003ba.dta";
local data7 = "`base_in'\for003ba.dta";
local data8 = "`base_in'\gall003ba.dta";
local data9 = "`base_in'\gba003ba.dta";
local data10 = "`base_in'\juj003ba.dta";
local data11 = "`base_in'\lapampa003ba.dta";
local data12 = "`base_in'\larioja003ba.dta";
local data13 = "`base_in'\lpl003ba.dta";
local data14 = "`base_in'\mdp003ba.dta";
local data15 = "`base_in'\men003ba.dta";
local data16 = "`base_in'\neu003ba.dta";
local data17 = "`base_in'\par003ba.dta";
local data18 = "`base_in'\pos003ba.dta";
local data19 = "`base_in'\rcuarto003ba.dta";
local data20 = "`base_in'\res003ba.dta";
local data21 = "`base_in'\ros003ba.dta";
local data22 = "`base_in'\sal003ba.dta";
local data23 = "`base_in'\sgo003ba.dta";
local data24 = "`base_in'\sju003ba.dta";
local data25 = "`base_in'\slu003ba.dta";
local data26 = "`base_in'\tfu003ba.dta";
local data27 = "`base_in'\tuc003ba.dta";
local data28 = "`base_in'\sfe003ba.dta";
*local data29 = "`base_in'\rawson003ba.dta";
*local data30 = "`base_in'\sannic003ba.dta";
*local data31 = "`base_in'\viedma003ba.dta";

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog003ba.dta"
local data2 = "`base_in'\cathog003ba.dta"
local data3 = "`base_in'\comhog003ba.dta"
local data4 = "`base_in'\concorhog003ba.dta"
local data5 = "`base_in'\corhog003ba.dta"
local data6 = "`base_in'\corrienhog003ba.dta"
local data7 = "`base_in'\forhog003ba.dta"
local data8 = "`base_in'\gallhog003ba.dta"
local data9 = "`base_in'\gbahog003ba.dta"
local data10 = "`base_in'\jujhog003ba.dta"
local data11 = "`base_in'\lapampahog003ba.dta"
local data12 = "`base_in'\lariojahog003ba.dta"
local data13 = "`base_in'\lplhog003ba.dta"
local data14 = "`base_in'\mdphog003ba.dta"
local data15 = "`base_in'\menhog003ba.dta"
local data16 = "`base_in'\neuhog003ba.dta"
local data17 = "`base_in'\parhog003ba.dta"
local data18 = "`base_in'\poshog003ba.dta"
local data19 = "`base_in'\rcuartohog003ba.dta"
local data20 = "`base_in'\reshog003ba.dta"
local data21 = "`base_in'\roshog003ba.dta"
local data22 = "`base_in'\salhog003ba.dta"
local data23 = "`base_in'\sgohog003ba.dta"
local data24 = "`base_in'\sjuhog003ba.dta"
local data25 = "`base_in'\sluhog003ba.dta"
local data26 = "`base_in'\tfuhog003ba.dta"
local data27 = "`base_in'\tuchog003ba.dta"
local data28 = "`base_in'\sfehog003ba.dta"
*local data29 = "`base_in'\rawsonhog003ba.dta";
*local data30 = "`base_in'\sannichog003ba.dta";
*local data31 = "`base_in'\viedmahog003ba.dta";

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


clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1998"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 = "`base_in'\bba983ba.dta";
local data2 = "`base_in'\cat983ba.dta";
local data3 = "`base_in'\com983ba.dta";
local data4 = "`base_in'\concor983ba.dta";
local data5 = "`base_in'\cor983ba.dta";
local data6 = "`base_in'\corrien983ba.dta";
local data7 = "`base_in'\for983ba.dta";
local data8 = "`base_in'\gall983ba.dta";
local data9 = "`base_in'\gba983ba.dta";
local data10 = "`base_in'\juj983ba.dta";
local data11 = "`base_in'\lapampa983ba.dta";
local data12 = "`base_in'\larioja983ba.dta";
local data13 = "`base_in'\lpl983ba.dta";
local data14 = "`base_in'\mdp983ba.dta";
local data15 = "`base_in'\men983ba.dta";
local data16 = "`base_in'\neu983ba.dta";
local data17 = "`base_in'\par983ba.dta";
local data18 = "`base_in'\pos983ba.dta";
local data19 = "`base_in'\rcuarto983ba.dta";
local data20 = "`base_in'\res983ba.dta";
local data21 = "`base_in'\ros983ba.dta";
local data22 = "`base_in'\sal983ba.dta";
local data23 = "`base_in'\sgo983ba.dta";
local data24 = "`base_in'\sju983ba.dta";
local data25 = "`base_in'\slu983ba.dta";
local data26 = "`base_in'\tfu983ba.dta";
local data27 = "`base_in'\tuc983ba.dta";
local data28 = "`base_in'\sfe983ba.dta";
*local data29 = "`base_in'\rawson983ba.dta";
*local data30 = "`base_in'\sannic983ba.dta";
*local data31 = "`base_in'\viedma983ba.dta";

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog983ba.dta"
local data2 = "`base_in'\cathog983ba.dta"
local data3 = "`base_in'\comhog983ba.dta"
local data4 = "`base_in'\concorhog983ba.dta"
local data5 = "`base_in'\corhog983ba.dta"
local data6 = "`base_in'\corrienhog983ba.dta"
local data7 = "`base_in'\forhog983ba.dta"
local data8 = "`base_in'\gallhog983ba.dta"
local data9 = "`base_in'\gbahog983ba.dta"
local data10 = "`base_in'\jujhog983ba.dta"
local data11 = "`base_in'\lapampahog983ba.dta"
local data12 = "`base_in'\lariojahog983ba.dta"
local data13 = "`base_in'\lplhog983ba.dta"
local data14 = "`base_in'\mdphog983ba.dta"
local data15 = "`base_in'\menhog983ba.dta"
local data16 = "`base_in'\neuhog983ba.dta"
local data17 = "`base_in'\parhog983ba.dta"
local data18 = "`base_in'\poshog983ba.dta"
local data19 = "`base_in'\rcuartohog983ba.dta"
local data20 = "`base_in'\reshog983ba.dta"
local data21 = "`base_in'\roshog983ba.dta"
local data22 = "`base_in'\salhog983ba.dta"
local data23 = "`base_in'\sgohog983ba.dta"
local data24 = "`base_in'\sjuhog983ba.dta"
local data25 = "`base_in'\sluhog983ba.dta"
local data26 = "`base_in'\tfuhog983ba.dta"
local data27 = "`base_in'\tuchog983ba.dta"
local data28 = "`base_in'\sfehog983ba.dta"
*local data29 = "`base_in'\rawsonhog983ba.dta";
*local data30 = "`base_in'\sannichog983ba.dta";
*local data31 = "`base_in'\viedmahog983ba.dta";

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


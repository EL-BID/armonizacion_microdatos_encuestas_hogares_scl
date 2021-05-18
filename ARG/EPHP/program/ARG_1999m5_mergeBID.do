clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1999"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 "`base_in'\bba991ba.dta"
local data2 "`base_in'\cat991ba.dta"
local data3 "`base_in'\com991ba.dta"
local data4 "`base_in'\concor991ba.dta"
local data5 "`base_in'\cor991ba.dta"
local data6 "`base_in'\corrien991ba.dta"
local data7 "`base_in'\for991ba.dta"
local data8 "`base_in'\gall991ba.dta"
local data9 "`base_in'\gba991ba.dta"
local data10 "`base_in'\juj991ba.dta"
local data11 "`base_in'\lapampa991ba.dta"
local data12 "`base_in'\larioja991ba.dta"
local data13 "`base_in'\lpl991ba.dta"
local data14 "`base_in'\mdp991ba.dta"
local data15 "`base_in'\men991ba.dta"
local data16 "`base_in'\neu991ba.dta"
local data17 "`base_in'\par991ba.dta"
local data18 "`base_in'\pos991ba.dta"
local data19 "`base_in'\rcuarto991ba.dta"
local data20 "`base_in'\res991ba.dta"
local data21 "`base_in'\ros991ba.dta"
local data22 "`base_in'\sal991ba.dta"
local data23 "`base_in'\sgo991ba.dta"
local data24 "`base_in'\sju991ba.dta"
local data25 "`base_in'\slu991ba.dta"
local data26 "`base_in'\tfu991ba.dta"
local data27 "`base_in'\tuc991ba.dta"
local data28 "`base_in'\sfe991ba.dta"

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 "`base_in'\bbahog991ba.dta"
local data2 "`base_in'\cathog991ba.dta"
local data3 "`base_in'\comhog991ba.dta"
local data4 "`base_in'\concorhog991ba.dta"
local data5 "`base_in'\corhog991ba.dta"
local data6 "`base_in'\corrienhog991ba.dta"
local data7 "`base_in'\forhog991ba.dta"
local data8 "`base_in'\gallhog991ba.dta"
local data9 "`base_in'\gbahog991ba.dta"
local data10 "`base_in'\jujhog991ba.dta"
local data11 "`base_in'\lapampahog991ba.dta"
local data12 "`base_in'\lariojahog991ba.dta"
local data13 "`base_in'\lplhog991ba.dta"
local data14 "`base_in'\mdphog991ba.dta"
local data15 "`base_in'\menhog991ba.dta"
local data16 "`base_in'\neuhog991ba.dta"
local data17 "`base_in'\parhog991ba.dta"
local data18 "`base_in'\poshog991ba.dta"
local data19 "`base_in'\rcuartohog991ba.dta"
local data20 "`base_in'\reshog991ba.dta"
local data21 "`base_in'\roshog991ba.dta"
local data22 "`base_in'\salhog991ba.dta"
local data23 "`base_in'\sgohog991ba.dta"
local data24 "`base_in'\sjuhog991ba.dta"
local data25 "`base_in'\sluhog991ba.dta"
local data26 "`base_in'\tfuhog991ba.dta"
local data27 "`base_in'\tuchog991ba.dta"
local data28 "`base_in'\sfehog991ba.dta"


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
merge codusu aglomera using "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta"
compress
table _merge
drop if _merge!=3


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


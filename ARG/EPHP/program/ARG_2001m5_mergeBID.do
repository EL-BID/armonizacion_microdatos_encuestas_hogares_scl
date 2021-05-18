clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2001"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 = "`base_in'\bba011ba.dta";
local data2  = "`base_in'\cat011ba.dta";
local data3 = "`base_in'\com011ba.dta";
local data4 = "`base_in'\concor011ba.dta";
local data5 = "`base_in'\cor011ba.dta";
local data6 = "`base_in'\corrien011ba.dta";
local data7 = "`base_in'\for011ba.dta";
local data8 = "`base_in'\gall011ba.dta";
local data9  ="`base_in'\gba011ba.dta";
local data10 = "`base_in'\juj011ba.dta";
local data11 = "`base_in'\lapampa011ba.dta";
local data12 = "`base_in'\larioja011ba.dta";
local data13 = "`base_in'\lpl011ba.dta";
local data14 = "`base_in'\mdp011ba.dta";
local data15 = "`base_in'\men011ba.dta";
local data16 = "`base_in'\neu011ba.dta";
local data17 = "`base_in'\par011ba.dta";
local data18 = "`base_in'\pos011ba.dta";
local data19 = "`base_in'\rcuarto011ba.dta";
local data20 = "`base_in'\res011ba.dta";
local data21 = "`base_in'\ros011ba.dta";
local data22 = "`base_in'\sal011ba.dta";
local data23 = "`base_in'\sgo011ba.dta";
local data24 = "`base_in'\sju011ba.dta";
local data25 = "`base_in'\slu011ba.dta";
local data26 = "`base_in'\tfu011ba.dta";
local data27 = "`base_in'\tuc011ba.dta";
local data28 = "`base_in'\sfe011ba.dta";

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 "`base_in'\bbahog011ba.dta"
local data2 "`base_in'\cathog011ba.dta"
local data3 "`base_in'\comhog011ba.dta"
local data4 "`base_in'\concorhog011ba.dta"
local data5 "`base_in'\corhog011ba.dta"
local data6 "`base_in'\corrienhog011ba.dta"
local data7 "`base_in'\forhog011ba.dta"
local data8 "`base_in'\gallhog011ba.dta"
local data9 "`base_in'\gbahog011ba.dta"
local data10 "`base_in'\jujhog011ba.dta"
local data11 "`base_in'\lapampahog011ba.dta"
local data12 "`base_in'\lariojahog011ba.dta"
local data13 "`base_in'\lplhog011ba.dta"
local data14 "`base_in'\mdphog011ba.dta"
local data15 "`base_in'\menhog011ba.dta"
local data16 "`base_in'\neuhog011ba.dta"
local data17 "`base_in'\parhog011ba.dta"
local data18 "`base_in'\poshog011ba.dta"
local data19 "`base_in'\rcuartohog011ba.dta"
local data20 "`base_in'\reshog011ba.dta"
local data21 "`base_in'\roshog011ba.dta"
local data22 "`base_in'\salhog011ba.dta"
local data23 "`base_in'\sgohog011ba.dta"
local data24 "`base_in'\sjuhog011ba.dta"
local data25 "`base_in'\sluhog011ba.dta"
local data26 "`base_in'\tfuhog011ba.dta"
local data27 "`base_in'\tuchog011ba.dta"
local data28 "`base_in'\sfehog011ba.dta"

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


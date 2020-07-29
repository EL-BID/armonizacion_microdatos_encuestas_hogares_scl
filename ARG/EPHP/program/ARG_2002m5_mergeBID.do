clear

global ruta = "\\Sdssrv03\surveys"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2002"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 "`base_in'\bba021ba.dta"
local data2 "`base_in'\cat021ba.dta"
local data3 "`base_in'\com021ba.dta"
local data4 "`base_in'\concor021ba.dta"
local data5 "`base_in'\cor021ba.dta"
local data6 "`base_in'\corrien021ba.dta"
local data7 "`base_in'\for021ba.dta"
local data8 "`base_in'\gall021ba.dta"
local data9 "`base_in'\gba021ba.dta"
local data10 "`base_in'\juj021ba.dta"
local data11 "`base_in'\lapampa021ba.dta"
local data12 "`base_in'\larioja021ba.dta"
local data13 "`base_in'\lpl021ba.dta"
local data14 "`base_in'\mdp021ba.dta"
local data15 "`base_in'\men021ba.dta"
local data16 "`base_in'\neu021ba.dta"
local data17 "`base_in'\par021ba.dta"
local data18 "`base_in'\pos021ba.dta"
local data19 "`base_in'\rcuarto021ba.dta"
local data20 "`base_in'\res021ba.dta"
local data21 "`base_in'\ros021ba.dta"
local data22 "`base_in'\sal021ba.dta"
local data23 "`base_in'\sgo021ba.dta"
local data24 "`base_in'\sju021ba.dta"
local data25 "`base_in'\slu021ba.dta"
local data26 "`base_in'\tfu021ba.dta"
local data27 "`base_in'\tuc021ba.dta"
local data28 "`base_in'\sfe021ba.dta"

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 "`base_in'\bbahog021ba.dta"
local data2 "`base_in'\cathog021ba.dta"
local data3 "`base_in'\comhog021ba.dta"
local data4 "`base_in'\concorhog021ba.dta"
local data5 "`base_in'\corhog021ba.dta"
local data6 "`base_in'\corrienhog021ba.dta"
local data7 "`base_in'\forhog021ba.dta"
local data8 "`base_in'\gallhog021ba.dta"
local data9 "`base_in'\gbahog021ba.dta"
local data10 "`base_in'\jujhog021ba.dta"
local data11 "`base_in'\lapampahog021ba.dta"
local data12 "`base_in'\lariojahog021ba.dta"
local data13 "`base_in'\lplhog021ba.dta"
local data14 "`base_in'\mdphog021ba.dta"
local data15 "`base_in'\menhog021ba.dta"
local data16 "`base_in'\neuhog021ba.dta"
local data17 "`base_in'\parhog021ba.dta"
local data18 "`base_in'\poshog021ba.dta"
local data19 "`base_in'\rcuartohog021ba.dta"
local data20 "`base_in'\reshog021ba.dta"
local data21 "`base_in'\roshog021ba.dta"
local data22 "`base_in'\salhog021ba.dta"
local data23 "`base_in'\sgohog021ba.dta"
local data24 "`base_in'\sjuhog021ba.dta"
local data25 "`base_in'\sluhog021ba.dta"
local data26 "`base_in'\tfuhog021ba.dta"
local data27 "`base_in'\tuchog021ba.dta"
local data28 "`base_in'\sfehog021ba.dta"

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


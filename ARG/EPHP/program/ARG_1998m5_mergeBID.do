clear

global ruta = "\\Sdssrv03\surveys"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1998"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 "`base_in'\bba981ba.dta"
local data2 "`base_in'\cat981ba.dta"
local data3 "`base_in'\com981ba.dta"
local data4 "`base_in'\concor981ba.dta"
local data5 "`base_in'\cor981ba.dta"
local data6 "`base_in'\corrien981ba.dta"
local data7 "`base_in'\for981ba.dta"
local data8 "`base_in'\gall981ba.dta"
local data9 "`base_in'\gba981ba.dta"
local data10 "`base_in'\juj981ba.dta"
local data11 "`base_in'\lapampa981ba.dta"
local data12 "`base_in'\larioja981ba.dta"
local data13 "`base_in'\lpl981ba.dta"
local data14 "`base_in'\mdp981ba.dta"
local data15 "`base_in'\men981ba.dta"
local data16 "`base_in'\neu981ba.dta"
local data17 "`base_in'\par981ba.dta"
local data18 "`base_in'\pos981ba.dta"
local data19 "`base_in'\rcuarto981ba.dta"
local data20 "`base_in'\res981ba.dta"
local data21 "`base_in'\ros981ba.dta"
local data22 "`base_in'\sal981ba.dta"
local data23 "`base_in'\sgo981ba.dta"
local data24 "`base_in'\sju981ba.dta"
local data25 "`base_in'\slu981ba.dta"
local data26 "`base_in'\tfu981ba.dta"
local data27 "`base_in'\tuc981ba.dta"
local data28 "`base_in'\sfe981ba.dta"
local data29 = "`base_in'\slu981ba.dta"

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 "`base_in'\bbahog981ba.dta"
local data2 "`base_in'\cathog981ba.dta"
local data3 "`base_in'\comhog981ba.dta"
local data4 "`base_in'\concorhog981ba.dta"
local data5 "`base_in'\corhog981ba.dta"
local data6 "`base_in'\corrienhog981ba.dta"
local data7 "`base_in'\forhog981ba.dta"
local data8 "`base_in'\gallhog981ba.dta"
local data9 "`base_in'\gbahog981ba.dta"
local data10 "`base_in'\jujhog981ba.dta"
local data11 "`base_in'\lapampahog981ba.dta"
local data12 "`base_in'\lariojahog981ba.dta"
local data13 "`base_in'\lplhog981ba.dta"
local data14 "`base_in'\mdphog981ba.dta"
local data15 "`base_in'\menhog981ba.dta"
local data16 "`base_in'\neuhog981ba.dta"
local data17 "`base_in'\parhog981ba.dta"
local data18 "`base_in'\poshog981ba.dta"
local data19 "`base_in'\rcuartohog981ba.dta"
local data20 "`base_in'\reshog981ba.dta"
local data21 "`base_in'\roshog981ba.dta"
local data22 "`base_in'\salhog981ba.dta"
local data23 "`base_in'\sgohog981ba.dta"
local data24 "`base_in'\sjuhog981ba.dta"
local data25 "`base_in'\sluhog981ba.dta"
local data26 "`base_in'\tfuhog981ba.dta"
local data27 "`base_in'\tuchog981ba.dta"
local data28 "`base_in'\sfehog981ba.dta"

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


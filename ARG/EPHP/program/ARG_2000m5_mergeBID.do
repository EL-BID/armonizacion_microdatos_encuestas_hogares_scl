clear

global ruta = "\\Sdssrv03\surveys"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2000"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 "`base_in'\bba001ba.dta"
local data2 "`base_in'\cat001ba.dta"
local data3 "`base_in'\com001ba.dta"
local data4 "`base_in'\concor001ba.dta"
local data5 "`base_in'\cor001ba.dta"
local data6 "`base_in'\corrien001ba.dta"
local data7 "`base_in'\for001ba.dta"
local data8 "`base_in'\gall001ba.dta"
local data9 "`base_in'\gba001ba.dta"
local data10 "`base_in'\juj001ba.dta"
local data11 "`base_in'\lapampa001ba.dta"
local data12 "`base_in'\larioja001ba.dta"
local data13 "`base_in'\lpl001ba.dta"
local data14 "`base_in'\mdp001ba.dta"
local data15 "`base_in'\men001ba.dta"
local data16 "`base_in'\neu001ba.dta"
local data17 "`base_in'\par001ba.dta"
local data18 "`base_in'\pos001ba.dta"
local data19 "`base_in'\rcuarto001ba.dta"
local data20 "`base_in'\res001ba.dta"
local data21 "`base_in'\ros001ba.dta"
local data22 "`base_in'\sal001ba.dta"
local data23 "`base_in'\sgo001ba.dta"
local data24 "`base_in'\sju001ba.dta"
local data25 "`base_in'\slu001ba.dta"
local data26 "`base_in'\tfu001ba.dta"
local data27 "`base_in'\tuc001ba.dta"
local data28 "`base_in'\sfe001ba.dta"

use `data1', replace
forvalues i=2/28 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 "`base_in'\bbahog001ba.dta"
local data2 "`base_in'\cathog001ba.dta"
local data3 "`base_in'\comhog001ba.dta"
local data4 "`base_in'\concorhog001ba.dta"
local data5 "`base_in'\corhog001ba.dta"
local data6 "`base_in'\corrienhog001ba.dta"
local data7 "`base_in'\forhog001ba.dta"
local data8 "`base_in'\gallhog001ba.dta"
local data9 "`base_in'\gbahog001ba.dta"
local data10 "`base_in'\jujhog001ba.dta"
local data11 "`base_in'\lapampahog001ba.dta"
local data12 "`base_in'\lariojahog001ba.dta"
local data13 "`base_in'\lplhog001ba.dta"
local data14 "`base_in'\mdphog001ba.dta"
local data15 "`base_in'\menhog001ba.dta"
local data16 "`base_in'\neuhog001ba.dta"
local data17 "`base_in'\parhog001ba.dta"
local data18 "`base_in'\poshog001ba.dta"
local data19 "`base_in'\rcuartohog001ba.dta"
local data20 "`base_in'\reshog001ba.dta"
local data21 "`base_in'\roshog001ba.dta"
local data22 "`base_in'\salhog001ba.dta"
local data23 "`base_in'\sgohog001ba.dta"
local data24 "`base_in'\sjuhog001ba.dta"
local data25 "`base_in'\sluhog001ba.dta"
local data26 "`base_in'\tfuhog001ba.dta"
local data27 "`base_in'\tuchog001ba.dta"
local data28 "`base_in'\sfehog001ba.dta"



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


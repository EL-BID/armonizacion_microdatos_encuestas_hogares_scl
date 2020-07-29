clear

global ruta = "\\Sdssrv03\surveys"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2003"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos


local data1 = "`base_in'\bba031ba.dta";
local data2 = "`base_in'\cat031ba.dta";
local data3 = "`base_in'\com031ba.dta";
local data4 = "`base_in'\concor031ba.dta";
local data5 = "`base_in'\cor031ba.dta";
local data6 = "`base_in'\corrien031ba.dta";
local data7 = "`base_in'\for031ba.dta";
local data8 = "`base_in'\gall031ba.dta";
local data9 = "`base_in'\gba031ba.dta";
local data10 = "`base_in'\juj031ba.dta";
local data11 = "`base_in'\lapampa031ba.dta";
local data12 = "`base_in'\larioja031ba.dta";
local data13 = "`base_in'\lpl031ba.dta";
local data14 = "`base_in'\mdp031ba.dta";
local data15 = "`base_in'\men031ba.dta";
local data16 = "`base_in'\neu031ba.dta";
local data17 = "`base_in'\par031ba.dta";
local data18 = "`base_in'\pos031ba.dta";
local data19 = "`base_in'\rcuarto031ba.dta";
local data20 = "`base_in'\res031ba.dta";
local data21 = "`base_in'\ros031ba.dta";
local data22 = "`base_in'\sal031ba.dta";
local data23 = "`base_in'\sgo031ba.dta";
local data24 = "`base_in'\sju031ba.dta";
local data25 = "`base_in'\slu031ba.dta";
local data26 = "`base_in'\tfu031ba.dta";
local data27 = "`base_in'\tuc031ba.dta";
local data28 = "`base_in'rawson031ba.dta";
local data29 = "`base_in'sannic031ba.dta";
local data30 = "`base_in'viedma031ba.dta";

use `data1', replace
forvalues i=2/30 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog031ba.dta"
local data2 = "`base_in'\cathog031ba.dta"
local data3 = "`base_in'\comhog031ba.dta"
local data4 = "`base_in'\concorhog031ba.dta"
local data5 = "`base_in'\corhog031ba.dta"
local data6 = "`base_in'\corrienhog031ba.dta"
local data7 = "`base_in'\forhog031ba.dta"
local data8 = "`base_in'\gallhog031ba.dta"
local data9 = "`base_in'\gbahog031ba.dta"
local data10 = "`base_in'\jujhog031ba.dta"
local data11 = "`base_in'\lapampahog031ba.dta"
local data12 = "`base_in'\lariojahog031ba.dta"
local data13 = "`base_in'\lplhog031ba.dta"
local data14 = "`base_in'\mdphog031ba.dta"
local data15 = "`base_in'\menhog031ba.dta"
local data16 = "`base_in'\neuhog031ba.dta"
local data17 = "`base_in'\parhog031ba.dta"
local data18 = "`base_in'\poshog031ba.dta"
local data19 = "`base_in'\rcuartohog031ba.dta"
local data20 = "`base_in'\reshog031ba.dta"
local data21 = "`base_in'\roshog031ba.dta"
local data22 = "`base_in'\salhog031ba.dta"
local data23 = "`base_in'\sgohog031ba.dta"
local data24 = "`base_in'\sjuhog031ba.dta"
local data25 = "`base_in'\sluhog031ba.dta"
local data26 = "`base_in'\tfuhog031ba.dta"
local data27 = "`base_in'\tuchog031ba.dta"
local data28 = "`base_in'\rawsonhog031ba.dta"
local data29 = "`base_in'\sannichog031ba.dta"
local data30 = "`base_in'\viedmahog031ba.dta"


use `data1', replace
forvalues i=2/30 {
append using `data`i''
}
/*Daniela Zuluaga- Enero 2018:
Se reemplazan los nombres de las variables p01 a p08 por p01_hogar a p08_hogar y r01 a r06 por r01_hogar a r06_hogar dado que la base de individuos contiene variables con 
el mismo nombre. Esto estaba generando errores al armonizar las bases, pues se reemplazaba la información al hacer el merge entre hogares e individuos*/

foreach var of varlist p01-r06 {
rename `var' `var'_hog
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
drop if _merge==2


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


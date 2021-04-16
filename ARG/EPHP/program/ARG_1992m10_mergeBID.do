clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1992"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos


local data1 = "`base_in'\gba923ba.dta"
local data2 = "`base_in'\com923ba.dta"
local data3 = "`base_in'\gall923ba.dta"
local data4 = "`base_in'\juj923ba.dta"
local data5 = "`base_in'\lapampa923ba.dta"
local data6 = "`base_in'\lpl923ba.dta"
local data7 = "`base_in'\neu923ba.dta"
local data8 = "`base_in'\par923ba.dta"
local data9 = "`base_in'\sal923ba.dta"
local data10 = "`base_in'\sfe923ba.dta"
local data11 = "`base_in'\slu923ba.dta"
local data12 = "`base_in'\tfu923ba.dta"
local data13 = "`base_in'\cor923ba.dta"
local data14 = "`base_in'\sgo923ba.dta"
local data15 = "`base_in'\sju923ba.dta"

/*
forvalues i=1/15 {
use `data`i'', replace
destring , replace
save `data`i'', replace
}
*/

forvalues i=1/15 {
use `data`i'', replace
tostring p56, replace
save `data`i'', replace
}

use `data1', replace
forvalues i=2/15 {
append using `data`i''
}


ren cod codusu

replace aglomerado=aglomera if aglomerado==.

sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares


local data0 = "`base_in'\sgohog923ba.dta"
local data1 = "`base_in'\gbahog923ba.dta"
local data2 = "`base_in'\comhog923ba.dta"
local data3 = "`base_in'\jujhog923ba.dta"
local data4 = "`base_in'\lapampahog923ba.dta"
local data5 = "`base_in'\neuhog923ba.dta"
local data6 = "`base_in'\parhog923ba.dta"
local data7 = "`base_in'\salhog923ba.dta"
local data8 = "`base_in'\sluhog923ba.dta"
local data9 = "`base_in'\tfuhog923ba.dta"
local data10 = "`base_in'\lplhog923ba.dta"
local data11 = "`base_in'\gallhog923ba.dta"
local data12 = "`base_in'\corhog923ba.dta"
local data13 = "`base_in'\sfehog923ba.dta"
local data14 = "`base_in'\sjuhog923ba.dta"

use `data0', replace
forvalues i=1/14 {
append using `data`i''
}


ren cod codusu

replace aglomerado=aglomera if aglomerado==.

destring entrevista onda ano decif deccf, replace

/*Daniela Zuluaga- Enero 2018:
Se reemplazan los nombres de las variables p01 a p08 por p01_hogar a p08_hogar y r01 a r06 por r01_hogar a r06_hogar dado que la base de individuos contiene variables con 
el mismo nombre. Esto estaba generando errores al armonizar las bases, pues se reemplazaba la información al hacer el merge entre hogares e individuos*/

forvalues i=1/8 {
rename p0`i' p0`i'_hogar
} 

forvalues i=1/6 {
rename r0`i' r0`i'_hogar
} 

sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta", replace

* merge de hogares e individuos

use "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta"



sort codusu aglomerado
merge codusu aglomerado using "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta"
compress

table aglomerado _merge
*drop if _merge!=3


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


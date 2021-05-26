clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1997"
local ronda m5

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 "`base_in'\bba971ba.dta"
local data2 "`base_in'\cat971ba.dta"
local data3 "`base_in'\com971ba.dta"
local data4 "`base_in'\concor971ba.dta"
local data5 "`base_in'\cor971ba.dta"
local data6 "`base_in'\corrien971ba.dta"
local data7 "`base_in'\for971ba.dta"
local data8 "`base_in'\gall971ba.dta"
local data9 "`base_in'\gba971ba.dta"
local data10 "`base_in'\sfe971ba.dta"
local data11 "`base_in'\lapampa971ba.dta"
local data12 "`base_in'\larioja971ba.dta"
local data13 "`base_in'\lpl971ba.dta"
local data14 "`base_in'\mdp971ba.dta"
local data15 "`base_in'\men971ba.dta"
local data16 = "`base_in'\slu971ba.dta"
local data17 "`base_in'\par971ba.dta"
local data18 "`base_in'\pos971ba.dta"
local data19 "`base_in'\rcuarto971ba.dta"
local data20 "`base_in'\res971ba.dta"
local data21 "`base_in'\ros971ba.dta"
local data22 "`base_in'\sal971ba.dta"
local data23 "`base_in'\sgo971ba.dta"
local data24 "`base_in'\sju971ba.dta"
local data25 "`base_in'\slu971ba.dta"
local data26 "`base_in'\tfu971ba.dta"
local data27 "`base_in'\tuc971ba.dta"

local data28 "`base_in'\juj971ba.dta"
* CAMBIAR JUJUY BASE PERSONAS Y PEGAR APARTE
/*
forvalues i=24/28 {
use `data`i'', replace
*destring onda, replace
*destring ano, replace
*destring p58b, replace
destring entrevista, replace
save `data`i'', replace
}*/

use `data1', replace
forvalues i=2/27 {
append using `data`i''
}
replace aglomerado=aglomera if aglomerado==.

ren cod codusu

sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_personassinjuj.dta", replace

* append de bases hogares

local data1 "`base_in'\bbahog971ba.dta"
local data2 "`base_in'\cathog971ba.dta"
local data3 "`base_in'\comhog971ba.dta"
local data4 "`base_in'\concorhog971ba.dta"
local data5 "`base_in'\corhog971ba.dta"
local data6 "`base_in'\corrienhog971ba.dta"
local data7 "`base_in'\forhog971ba.dta"
local data8 "`base_in'\gallhog971ba.dta"
local data9 "`base_in'\gbahog971ba.dta"
local data10 "`base_in'\sfehog971ba.dta"
local data11 "`base_in'\lapampahog971ba.dta"
local data12 "`base_in'\lariojahog971ba.dta"
local data13 "`base_in'\lplhog971ba.dta"
local data14 "`base_in'\mdphog971ba.dta"
local data15 "`base_in'\menhog971ba.dta"
local data16 = "`base_in'\sluhog971ba.dta"
local data17 "`base_in'\parhog971ba.dta"
local data18 "`base_in'\poshog971ba.dta"
local data19 "`base_in'\rcuartohog971ba.dta"
local data20 "`base_in'\reshog971ba.dta"
local data21 "`base_in'\roshog971ba.dta"
local data22 "`base_in'\salhog971ba.dta"
local data23 "`base_in'\sgohog971ba.dta"
local data24 "`base_in'\sjuhog971ba.dta"
local data25 "`base_in'\sluhog971ba.dta"
local data26 "`base_in'\tfuhog971ba.dta"
local data27 "`base_in'\tuchog971ba.dta"

local data28 "`base_in'\jujhog971ba.dta"

/*
forvalues i=28/28 {
use `data`i'', replace
tostring entrevista, replace
save `data`i'', replace
}*/


use `data1', replace
forvalues i=2/27 {
append using `data`i''
}

replace aglomerado=aglomera if aglomerado==.

ren cod codusu

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
save "`base_in'\`PAIS'_`ANO'`ronda'_hogaressinjuj.dta", replace

*uno juj
use  "`base_in'\jujhog971ba.dta"

sort codu aglomerado

save   "`base_in'\jujhog971ba.dta", replace

use  "`base_in'\juj971ba.dta"
sort codu aglomerado
merge codu aglomerado using "`base_in'\jujhog971ba.dta"
egen codusu=group(codu)
table aglomerado _merge
drop if _merge!=3
save  "`base_in'\jujhogyper971ba.dta", replace

* merge de hogares e individuos (sin juj)

use "`base_in'\`PAIS'_`ANO'`ronda'_personassinjuj.dta"


sort codusu aglomerado
merge codusu aglomerado using "`base_in'\`PAIS'_`ANO'`ronda'_hogaressinjuj.dta"
compress
table aglomerado _merge
drop if _merge!=3

*ren cod codusu /*`ANO'>=1992 & `ANO'<=1997*/

* append de gba
append using "`base_in'\jujhogyper971ba.dta"



* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


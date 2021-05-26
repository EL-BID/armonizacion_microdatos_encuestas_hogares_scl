clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "2002"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos

local data1 = "`base_in'\bba023ba.dta";
local data2 = "`base_in'\cat023ba.dta";
local data3 = "`base_in'\com023ba.dta";
local data4 = "`base_in'\concor023ba.dta";
local data5 = "`base_in'\cor023ba.dta";
local data6 = "`base_in'\corrien023ba.dta";
local data7 = "`base_in'\for023ba.dta";
local data8 = "`base_in'\gall023ba.dta";
local data9 = "`base_in'\gba023ba.dta";
local data10 = "`base_in'\juj023ba.dta";
local data11 = "`base_in'\lapampa023ba.dta";
local data12 = "`base_in'\larioja023ba.dta";
local data13 = "`base_in'\lpl023ba.dta";
local data14 = "`base_in'\mdp023ba.dta";
local data15 = "`base_in'\men023ba.dta";
local data16 = "`base_in'\neu023ba.dta";
local data17 = "`base_in'\par023ba.dta";
local data18 = "`base_in'\pos023ba.dta";
local data19 = "`base_in'\rcuarto023ba.dta";
local data20 = "`base_in'\res023ba.dta";
local data21 = "`base_in'\ros023ba.dta";
local data22 = "`base_in'\sal023ba.dta";
local data23 = "`base_in'\sgo023ba.dta";
local data24 = "`base_in'\sju023ba.dta";
local data25 = "`base_in'\slu023ba.dta";
local data26 = "`base_in'\tfu023ba.dta";
local data27 = "`base_in'\tuc023ba.dta";
local data28 = "`base_in'\sfe023ba.dta";
local data29 = "`base_in'\rawson023ba.dta";
local data30 = "`base_in'\sannic023ba.dta";
local data31 = "`base_in'\viedma023ba.dta";


use `data1', replace
forvalues i=2/31 {
append using `data`i''
}

save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog023ba.dta"
local data2 = "`base_in'\cathog023ba.dta"
local data3 = "`base_in'\comhog023ba.dta"
local data4 = "`base_in'\concorhog023ba.dta"
local data5 = "`base_in'\corhog023ba.dta"
local data6 = "`base_in'\corrienhog023ba.dta"
local data7 = "`base_in'\forhog023ba.dta"
local data8 = "`base_in'\gallhog023ba.dta"
local data9 = "`base_in'\gbahog023ba.dta"
local data10 = "`base_in'\jujhog023ba.dta"
local data11 = "`base_in'\lapampahog023ba.dta"
local data12 = "`base_in'\lariojahog023ba.dta"
local data13 = "`base_in'\lplhog023ba.dta"
local data14 = "`base_in'\mdphog023ba.dta"
local data15 = "`base_in'\menhog023ba.dta"
local data16 = "`base_in'\neuhog023ba.dta"
local data17 = "`base_in'\parhog023ba.dta"
local data18 = "`base_in'\poshog023ba.dta"
local data19 = "`base_in'\rcuartohog023ba.dta"
local data20 = "`base_in'\reshog023ba.dta"
local data21 = "`base_in'\roshog023ba.dta"
local data22 = "`base_in'\salhog023ba.dta"
local data23 = "`base_in'\sgohog023ba.dta"
local data24 = "`base_in'\sjuhog023ba.dta"
local data25 = "`base_in'\sluhog023ba.dta"
local data26 = "`base_in'\tfuhog023ba.dta"
local data27 = "`base_in'\tuchog023ba.dta"
local data28 = "`base_in'\sfehog023ba.dta"
local data29 = "`base_in'\rawsonhog023ba.dta";
local data30 = "`base_in'\sannichog023ba.dta";
local data31 = "`base_in'\viedmahog023ba.dta";

*MGR Jul, 2015: se había agregado una base de personas por lo que habían muchos duplicados.
*Igual existen un par de duplicados (4) del aglomerado 4, haremos un drops de los casos con valores missing. 

* Otro problema está en que base de personas tiene variables con el mismo nombre de variables de vivienda, renombramos estas para hacer merge
* Bases originales quedan en carpeta backup

/*
forvalues i=1/31 {
use `data`i'', clear
foreach var of varlist p01-r06 {
rename `var' `var'_hog
saveold `data`i'', replace
}
}

*/
use `data1', clear
forvalues i=2/31 {
append using `data`i''
}


duplicates tag codusu aglomerado, gen(dupli)
drop if codusu=="4-02-3-0094" & dupli==1 & (p01_hog-r06_hog==.)
drop if codusu=="4-02-3-0095" & dupli==1 & (p01_hog-r06_hog==.)
drop if codusu=="4-02-3-0097" & dupli==1 & (p01_hog-r06_hog==.)
drop if codusu=="4-02-3-0159" & dupli==1 & (p01_hog==1) /*único caso donde ambos casos tienen valores y hago drop de una de los dos aleatoriamente*/
drop dupli


sort codusu aglomera
save "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta", replace

* merge de hogares e individuos

use "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta"

*gen codusu=cod  if `ANO'>=1992 & `ANO'<=1997

sort codusu aglomerado
merge m:1 codusu aglomera using "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta" 
compress
drop if _merge!=3


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1994"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos


local data1 = "`base_in'\gba943ba.dta"
local data2 = "`base_in'\com943ba.dta"
local data3 = "`base_in'\juj943ba.dta"
local data4 = "`base_in'\lapampa943ba.dta"
local data5 = "`base_in'\neu943ba.dta"
local data6 = "`base_in'\par943ba.dta"
local data7 = "`base_in'\sal943ba.dta"
local data8 =  "`base_in'\slu943ba.dta"
local data9 = "`base_in'\tfu943ba.dta"
local data10 = "`base_in'\cor943ba.dta"
local data11 = "`base_in'\men943ba.dta"
local data12 = "`base_in'\ros943ba.dta"
local data13 = "`base_in'\tuc943ba.dta"
local data14 = "`base_in'\gall943ba.dta"
local data15 =  "`base_in'\lpl943ba.dta"
local data16 = "`base_in'\corrien943ba.dta"
local data17 = "`base_in'\res943ba.dta"
local data18 = "`base_in'\sgo943ba.dta"
local data19 = "`base_in'\sju943ba.dta"
local data20 = "`base_in'\larioja943ba.dta"
local data21 "`base_in'\sfe943ba.dta"
local data22 "`base_in'\cat943ba.dta"

/*
forvalues i=1/25 {
use `data`i'', replace
display "`data`i''"
des h11
tab h11 in 1
}*/
/*
use `data12', replace
display "`data12'"

generate str2 h11da1= substr(h11,1,2)
generate str2 h11mo1 = substr(h11,4,5)
generate str2 h11yr1 = substr(h11,7,8)
/*

gen h11lenght=length(h11)
sort h11lenght
*generate str2 h11mo1= substr(h11,-8,2) if length(h11) == 8
*replace  h11mo1= substr(h11,1,2) if length(h11) == 7
*replace  h11mo1= substr(h11,1,1) if length(h11) == 6
gen h11mo1= regexs(0) if regexm(h11, "^[0-9]+")

generate str2 h11da1 = substr(h11,-5,2) 
replace  h11da1= substr(h11,-4,1) if length(h11) == 6
replace  h11da1= substr(h11,-4,1) if length(h11) == 7 & length(h11mo1) ==2
*replace  h11da1= substr(h11,-4,2) if length(h11) == 7 & length(h11mo1) ==1
generate str2 h11yr1= substr(h11,-2,2)

*/
gen str10 h11_aux1 = h11da1 +"." + h11mo1 +"." + h11yr1
gen h11new = date(h11_aux1, "DM19Y")
format h11new %td
edit h11*
drop h11
ren h11new h11 
save `data12', replace
*/

/*
forvalues i=1/22 {
use `data`i'', replace
destring onda, replace
save `data`i'', replace
}
forvalues i=1/22 {
use `data`i'', replace
destring ano, replace
save `data`i'', replace
}

forvalues i=1/22 {
use `data`i'', replace
destring p20* p41* , replace
save `data`i'', replace
}*/
forvalues i=1/22 {
use `data`i'', replace
tostring p59esp p61esp p62esp p56, replace
save `data`i'', replace
}
forvalues i=1/22 {
use `data`i'', replace
destring decif deccf decind decocu hogar1 ingreso monto , replace
save `data`i'', replace
}
use `data1', replace
forvalues i=2/22 {
append using `data`i''
}
ren cod codusu

replace aglomerado=aglomera if aglomerado==.

sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\gbahog943ba.dta"
local data2 = "`base_in'\comhog943ba.dta"
local data3 = "`base_in'\jujhog943ba.dta"
local data4 = "`base_in'\lapampahog943ba.dta"
local data5 = "`base_in'\neuhog943ba.dta"
local data6 = "`base_in'\parhog943ba.dta"
local data7 = "`base_in'\salhog943ba.dta"
local data8 =  "`base_in'\sluhog943ba.dta"
local data9 = "`base_in'\tfuhog943ba.dta"
local data10 = "`base_in'\corhog943ba.dta"
local data11 = "`base_in'\menhog943ba.dta"
local data12 = "`base_in'\roshog943ba.dta"
local data13 = "`base_in'\tuchog943ba.dta"
local data14 = "`base_in'\gallhog943ba.dta"
local data15 =  "`base_in'\lplhog943ba.dta"
local data16 = "`base_in'\corrienhog943ba.dta"
local data17 = "`base_in'\reshog943ba.dta"
local data18 = "`base_in'\sgohog943ba.dta"
local data19 = "`base_in'\sjuhog943ba.dta"
local data20 = "`base_in'\lariojahog943ba.dta"
local data21 "`base_in'\sfehog943ba.dta"
local data22 "`base_in'\cathog943ba.dta"

use `data1', replace
forvalues i=2/22 {
append using `data`i''
}

ren cod codusu


replace aglomerado=aglomera if aglomerado==.

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

table _merge
tab aglomerado _merge
drop if _merge!=3


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close


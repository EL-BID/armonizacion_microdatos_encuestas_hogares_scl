clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1996"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


set more off

* append de bases individuos
local data1 = "`base_in'\gba963ba.dta"
local data2 = "`base_in'\com963ba.dta"
local data3 = "`base_in'\gall963ba.dta"
local data4 = "`base_in'\juj963ba.dta"
local data5 = "`base_in'\lapampa963ba.dta"
local data6 = "`base_in'\lpl963ba.dta"
local data7 = "`base_in'\neu963ba.dta"
local data8 = "`base_in'\par963ba.dta"
local data9 = "`base_in'\sal963ba.dta"
local data10 = "`base_in'\sfe963ba.dta"
local data11 = "`base_in'\slu963ba.dta"
local data12 = "`base_in'\tfu963ba.dta"
local data13 = "`base_in'\bba963ba.dta"
local data14 = "`base_in'\cat963ba.dta"
local data15 = "`base_in'\concor963ba.dta"
local data16 = "`base_in'\corrien963ba.dta"
local data17 = "`base_in'\for963ba.dta"
local data18 = "`base_in'\larioja963ba.dta"
local data19 = "`base_in'\mdp963ba.dta"
local data20 = "`base_in'\men963ba.dta"
local data21 = "`base_in'\pos963ba.dta"
local data22 = "`base_in'\rcuarto963ba.dta"
local data23 = "`base_in'\res963ba.dta"
local data24 = "`base_in'\sgo963ba.dta"
local data25 = "`base_in'\sju963ba.dta"
local data26 = "`base_in'\tuc963ba.dta"

* cambios de formato para pegar bases
/*
forvalues i=1/26 {
use `data`i'', replace
display "`data`i''"
des h11
tab h11 in 1
}
*/
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


gen str10 h11_aux1 = h11da1 +"." + h11mo1 +"." + h11yr1
gen h11new = date(h11_aux1, "DM19Y")
format h11new %td
*edit h11*
drop h11
ren h11new h11 
save `data12', replace
*/
*//*
forvalues i=1/26 {
use `data`i'', replace
tostring p48esp, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
tostring p08esp, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
tostring p34esp, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
destring onda, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
destring ano, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
tostring p13_ns, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
destring p20, replace
save `data`i'', replace
}

forvalues i=1/26 {
use `data`i'', replace
destring p56 p58b, replace
save `data`i'', replace
}
forvalues i=1/26 {
use `data`i'', replace
destring decif deccf decind decocu hogar1 ingreso monto p20_3, replace
save `data`i'', replace
}

forvalues i=2/26 {
use `data`i'', replace
destring p20* p41*, replace
save `data`i'', replace
}
forvalues i=2/26 {
use `data`i'', replace
tostring p59esp p61esp p62esp, replace
save `data`i'', replace
}
*/



use `data1', replace
forvalues i=2/26 {
append using `data`i''
}
replace aglomerado=aglomera if aglomerado==.

ren cod codusu

sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta", replace

* append de bases hogares

local data1 = "`base_in'\bbahog963ba.dta"
local data2 "`base_in'\neuhog963ba.dta"
local data3 "`base_in'\parhog963ba.dta"
local data4 "`base_in'\salhog963ba.dta"
local data5 "`base_in'\sluhog963ba.dta"
local data6 "`base_in'\tfuhog963ba.dta"
local data7 "`base_in'\gbahog963ba.dta"
local data8 "`base_in'\menhog963ba.dta"
local data9 "`base_in'\comhog963ba.dta"
local data10 "`base_in'\tuchog963ba.dta"
local data11 "`base_in'\gallhog963ba.dta"
local data12 "`base_in'\lplhog963ba.dta"
local data13 "`base_in'\corrienhog963ba.dta"
local data14 "`base_in'\reshog963ba.dta"
local data15 "`base_in'\sgohog963ba.dta"
local data16 "`base_in'\sjuhog963ba.dta"
local data17 "`base_in'\sfehog963ba.dta"
local data18 "`base_in'\lapampahog963ba.dta"
local data19 "`base_in'\cathog963ba.dta"
local data20 "`base_in'\lariojahog963ba.dta"
local data21 "`base_in'\concorhog963ba.dta"
local data22 "`base_in'\forhog963ba.dta"
local data23 "`base_in'\mdphog963ba.dta"
local data24 "`base_in'\poshog963ba.dta"
local data25 "`base_in'\rcuartohog963ba.dta"
local data26 "`base_in'\jujhog963ba.dta"

forvalues i=1/26 {
use `data`i'', replace
destring entrevista onda ano razon pers_ent, replace
save `data`i'', replace
}
use `data1', replace
forvalues i=2/26 {
append using `data`i''
}
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

save "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta", replace

* merge de hogares e individuos

use "`base_in'\`PAIS'_`ANO'`ronda'_personas.dta"

*gen codusu=cod  if `ANO'>=1992 & `ANO'<=1997

sort codusu aglomerado
merge codusu aglomerado using "`base_in'\`PAIS'_`ANO'`ronda'_hogares.dta"
compress
table _merge
drop if _merge!=3


* Comprime y guarda base
compress
saveold "`base_out'", replace

log close



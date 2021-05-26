clear

global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHP
local ANO "1997"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
  
capture log close
log using "`log_file'", replace 


set more off
* append de bases individuos


local data1 = "`base_in'\com973ba.dta";
local data2 = "`base_in'\cor973ba.dta";
local data3 = "`base_in'\gall973ba.dta";
local data4 = "`base_in'\juj973ba.dta";
local data5 = "`base_in'\lpl973ba.dta";
local data6 = "`base_in'\neu973ba.dta";
local data7 = "`base_in'\par973ba.dta";
local data8 = "`base_in'\sal973ba.dta";
local data9 = "`base_in'\sgo973ba.dta";
local data10 = "`base_in'\sju973ba.dta";
local data11 = "`base_in'\slu973ba.dta";
local data12 = "`base_in'\tfu973ba.dta";
local data13 = "`base_in'\sfe973ba.dta";
local data14 = "`base_in'\lapampa973ba.dta";


local data15 = "`base_in'\bba973ba.dta";
local data16 = "`base_in'\cat973ba.dta";
local data17 = "`base_in'\concor973ba.dta";
local data18 = "`base_in'\corrien973ba.dta";
local data19 = "`base_in'\for973ba.dta";
local data20 = "`base_in'\larioja973ba.dta";
local data21 = "`base_in'\mdp973ba.dta";
local data22 = "`base_in'\men973ba.dta";
local data23 = "`base_in'\pos973ba.dta";
local data24 = "`base_in'\rcuarto973ba.dta";
local data25 = "`base_in'\res973ba.dta";
local data26 = "`base_in'\ros973ba.dta";
local data27 = "`base_in'\tuc973ba.dta";

local data28 = "`base_in'\gba973ba.dta";
*________________________________________________________________________________________________________*
*________________________________________________________________________________________________________*

*local data29 = "`base_in'\rawson973ba.dta";
*local data30 = "`base_in'\sannic973ba.dta";
*local data31 = "`base_in'\viedma973ba.dta";
/*
forvalues i=1/28 {
use `data`i'', replace
display "`data`i''"
des h11
tab h11 in 1
}
*/
/*
* para data1 y data2
use `data11', replace
display "`data11'"
generate str2 h11da1= substr(h11,1,2)
generate str2 h11mo1 = substr(h11,4,5)
generate str2 h11yr1 = substr(h11,7,8)


gen str10 h11_aux1 = h11da1 +"." + h11mo1 +"." + h11yr1
gen h11new = date(h11_aux1, "DM19Y")
format h11new %td
*edit h11*
drop h11
ren h11new h11 
save `data11', replace
 */
/*
destring h11da1 h11mo1 h11yr1, replace
generate h11da= day(h11da1)
generate h11mo = month(h11mo1)
generate h11yr = year(h11yr1)
tostring h11da h11mo h11yr, replace

gen str10 h11_aux = h11da + h11mo + h11yr
gen date2 = date(h11_aux, "DMY")
 format date2 %td

drop h11 h11da h11mo h11yr
ren h11_aux h11
save `data2', replace

* se cambio h11 para evitar problemas en el append de las bases (data1, data3)

use `data1', replace
tostring h11, replace
generate str2 h11da= substr(h11,1,2)
generate str3 h11mo = substr(h11,4,6)
generate str2 h11yr = substr(h11,8,9)
destring h11*, replace

gen h11month="Ene" if h11mo==1
replace h11month="Feb" if h11mo==2
replace h11month="Mar" if h11mo==3
replace h11month="Abr" if h11mo==4
replace h11month="May" if h11mo==5
replace h11month="Jun" if h11mo==6
replace h11month="Jul" if h11mo==7
replace h11month="Ago" if h11mo==8
replace h11month="Sep" if h11mo==9
replace h11month="Oct" if h11mo==10
replace h11month="Nov" if h11mo==11
replace h11month="Dic" if h11mo==12

tostring h11*, replace
gen str8 h11_aux = h11da + " " + h11month + " " + h11yr
drop h11
ren h11_aux h11
save `data1', replace
use `data3', replace
tostring h11, replace
generate str2 h11da= substr(h11,1,2)
generate str2 h11mo = substr(h11,4,5)
generate str4 h11yr = substr(h11,7,9)
destring h11*, replace

gen h11month="Ene" if h11mo==1
replace h11month="Feb" if h11mo==2
replace h11month="Mar" if h11mo==3
replace h11month="Abr" if h11mo==4
replace h11month="May" if h11mo==5
replace h11month="Jun" if h11mo==6
replace h11month="Jul" if h11mo==7
replace h11month="Ago" if h11mo==8
replace h11month="Sep" if h11mo==9
replace h11month="Oct" if h11mo==10
replace h11month="Nov" if h11mo==11
replace h11month="Dic" if h11mo==12

tostring h11*, replace
gen str9 h11_aux = h11da + " " + h11month + " " + h11yr
drop h11
ren h11_aux h11

save `data3', replace

use `data2', replace
tostring h11, replace
save `data2', replace

forvalues i=10/28 {
use `data`i'', replace
tostring p13_ns, replace
save `data`i'', replace
}

forvalues i=1/28 {
use `data`i'', replace
destring p20, replace
save `data`i'', replace
}
*/
/*
forvalues i=1/28 {
use `data`i'', replace
destring p41, replace
save `data`i'', replace

}
forvalues i=1/28 {
use `data`i'', replace
tostring p48esp, replace
save `data`i'', replace
}
forvalues i=1/28 {
use `data`i'', replace
destring monto, replace
save `data`i'', replace

}*/
/*
use `data3', replace
drop h11da  h11month  h11mo  h11yr
save `data3', replace

forvalues i=1/28 {
use `data`i'', replace
destring p56, replace
save `data`i'', replace
}
forvalues i=1/28 {
use `data`i'', replace
destring p58b, replace
save `data`i'', replace
}

forvalues i=1/28 {
use `data`i'', replace
destring decif deccf decind decocu, replace
save `data`i'', replace
}
forvalues i=1/28 {
use `data`i'', replace
destring hogar1, replace
save `data`i'', replace
}
forvalues i=1/28 {
use `data`i'', replace
destring ingreso, replace
save `data`i'', replace
}*/
*________________________________________________________________________________________________________*
*________________________________________________________________________________________________________*

use `data1', replace
forvalues i=2/27 {
append using `data`i''
}

ren cod codusu
replace aglomerado=aglomera if aglomerado==.

sort codusu aglomerado
save "`base_in'\`PAIS'_`ANO'`ronda'_personassingba.dta", replace

* append de bases hogares

local data1 = "`base_in'\tuchog973ba.dta"
local data2 = "`base_in'\comhog973ba.dta"
local data3 = "`base_in'\corhog973ba.dta"
local data4 = "`base_in'\jujhog973ba.dta"
local data5 = "`base_in'\lplhog973ba.dta"
local data6 = "`base_in'\neuhog973ba.dta"
local data7 = "`base_in'\parhog973ba.dta"
local data8 = "`base_in'\gallhog973ba.dta"
local data9 = "`base_in'\salhog973ba.dta"
local data10 = "`base_in'\sjuhog973ba.dta"
local data11 = "`base_in'\sluhog973ba.dta"
local data12 = "`base_in'\lapampahog973ba.dta"
local data13 = "`base_in'\sfe973ba.dta"
local data14 = "`base_in'\sgohog973ba.dta"
local data15 = "`base_in'\tfuhog973ba.dta"

local data16 = "`base_in'\bbahog973ba.dta"
local data17 = "`base_in'\cathog973ba.dta"
local data18 = "`base_in'\concorhog973ba.dta"
local data19 = "`base_in'\corrienhog973ba.dta"
local data20 = "`base_in'\forhog973ba.dta"
local data21 = "`base_in'\lariojahog973ba.dta"
local data22 = "`base_in'\mdphog973ba.dta"
local data23 = "`base_in'\menhog973ba.dta"
local data24 = "`base_in'\poshog973ba.dta"
local data25 = "`base_in'\rcuartohog973ba.dta"
local data26 = "`base_in'\reshog973ba.dta"
local data27 = "`base_in'\roshog973ba.dta"

local data28 = "`base_in'\gbahog973ba.dta"
*local data29 = "`base_in'\rawsonhog973ba.dta";
*local data30 = "`base_in'\sannichog973ba.dta";
*local data31 = "`base_in'\viedmahog973ba.dta";*/


forvalues i=1/28 {
use `data`i'', replace
destring decif deccf , replace
save `data`i'', replace
}
use `data1', replace
forvalues i=2/27 {
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
save "`base_in'\`PAIS'_`ANO'`ronda'_hogaressingba.dta", replace

*uno gba
use  "`base_in'\gbahog973ba.dta", clear

sort codu aglomerado
save   "`base_in'\gbahog973ba.dta", replace

use  "`base_in'\gba973ba.dta"
sort codu aglomerado
merge codu aglomerado using "`base_in'\gbahog973ba.dta"
save  "`base_in'\gbahogyper973ba.dta", replace

* merge de hogares e individuos (sin gba)

*Modificación Mayra Sáenz- Marzo 2014
clear
use "`base_in'\gbahogyper973ba.dta", clear
gen codusu = substr(codu,8,4)
destring codusu, replace
replace aglomerado=aglomera if aglomerado==.
saveold "`base_in'\gbahogyper973ba_.dta", replace




use "`base_in'\`PAIS'_`ANO'`ronda'_personassingba.dta", clear


sort codusu aglomerado
merge codusu aglomerado using "`base_in'\`PAIS'_`ANO'`ronda'_hogaressingba.dta"
compress
table _merge
drop if _merge!=3

*ren cod codusu /*`ANO'>=1992 & `ANO'<=1997*/

* append de gba
*append using "`base_in'\gbahogyper973ba.dta" /*Modificación Mayra Sáenz - Marzo 2014*/
append using "`base_in'\gbahogyper973ba_.dta"

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close



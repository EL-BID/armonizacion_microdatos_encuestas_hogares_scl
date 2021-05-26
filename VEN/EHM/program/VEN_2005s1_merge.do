*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "${surveysFolder}\survey\VEN\EHM\2005\s1\data_orig\"

*Identificacion bases de datos
*--------------------------------
use "hog051.dta", clear 
sort entidad control linea serie 
cap ren peso pesoh
save, replace

use "viv051.dta", clear  
sort entidad control linea serie 
cap rename peso pesov
save, replace

use "per051.dta", clear
cap rename peso pesop
sort entidad control linea serie num_hog 
save, replace

*Merge
*--------
use "hog051.dta", clear
joinby entidad control linea serie  using "viv051.dta", _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog  using "per051.dta", _merge(_merge)
tab _merge
drop _merge
save "${surveysFolder}\survey\VEN\EHM\2005\s1\data_merge\VEN_2005s1.dta", replace

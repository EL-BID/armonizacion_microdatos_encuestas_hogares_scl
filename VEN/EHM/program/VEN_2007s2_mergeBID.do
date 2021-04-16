*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off
cd "${surveysFolder}\survey\VEN\EHM\2007\s2\data_orig\"

*Identificacion bases de datos
*--------------------------------
use hog072.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesoh
save, replace

use viv072.dta, clear 
sort entidad control linea serie
cap rename peso pesov
save, replace

use per072.dta, clear
cap rename peso pesop
sort entidad control linea serie num_hog
save, replace

*Merge
*--------
use hog072.dta, clear
joinby entidad control linea serie using viv072.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per072.dta, _merge(_merge)
tab _merge
drop _merge
saveold "${surveysFolder}\survey\VEN\EHM\2007\s2\data_merge\VEN_2007s2.dta", replace


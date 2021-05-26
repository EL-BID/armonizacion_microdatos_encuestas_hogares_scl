*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

clear
set more off 
cd "${surveysFolder}\survey\VEN\EHM\2009\s1\data_orig\"

*Identificacion bases de datos
*--------------------------------

use per091.dta, clear
sort entidad control linea serie num_hog
cap drop id_*
cap rename peso pesop
save per091.dta, replace

use hog091.dta, clear
sort entidad control linea serie
cap drop id_* 
cap rename peso pesoh
save hog091.dta, replace

use viv091.dta, clear 
sort entidad control linea serie
cap drop id_*
cap rename peso pesov
save viv091.dta, replace

*Merge
*--------
use hog091.dta, clear
joinby entidad control linea serie using viv091.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per091.dta, _merge(_merge)
tab _merge
drop _merge

saveold "${surveysFolder}\survey\VEN\EHM\2009\s1\data_merge\VEN_2009s1.dta", replace

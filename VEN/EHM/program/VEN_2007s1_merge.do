*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off
cd "\\Sdssrv03\surveys\survey\VEN\EHM\2007\s1\data_orig\"

*Identificacion bases de datos
*--------------------------------
use hog071.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesoh
save, replace

use viv071.dta, clear 
sort entidad control linea serie
cap rename peso pesov
save, replace

use per071.dta, clear
cap rename peso pesop
sort entidad control linea serie num_hog

*Merge
*--------
use hog071.dta, clear
joinby entidad control linea serie using viv071.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per071.dta, _merge(_merge)
tab _merge
drop _merge
saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2007\s1\data_merge\VEN_2007s1.dta", replace


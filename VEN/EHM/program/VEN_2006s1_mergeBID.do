*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off
cd "\\Sdssrv03\surveys\survey\VEN\EHM\2006\s1\data_orig\"

*Identificacion bases de datos
*--------------------------------
use hog061.dta, clear
cap rename peso pesoh
sort entidad control linea serie num_hog
save, replace

use viv061.dta, clear
sort entidad control linea serie
cap rename peso pesov
save, replace

use per061.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesop
save, replace

*Merge
*-----------
use hog061.dta, clear
joinby entidad control linea serie using viv061.dta, _merge(_merge)
tab _merge
drop _merge
joinby entidad control linea serie  num_hog using per061.dta, _merge(_merge)
tab _merge
drop _merge
save "\\Sdssrv03\surveys\survey\VEN\EHM\2006\s1\data_merge\VEN_2006s1.dta", replace


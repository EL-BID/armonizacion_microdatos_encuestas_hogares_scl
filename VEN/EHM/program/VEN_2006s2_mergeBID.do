*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off
cd "\\Sdssrv03\surveys\survey\VEN\EHM\2006\s2\data_orig\"

*Identificacion bases de datos
*--------------------------------
use hog062.dta, clear
cap rename peso pesoh
sort entidad control linea serie num_hog
save, replace

use viv062.dta, clear
sort entidad control linea serie
cap rename peso pesov
save, replace

use per062.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesop
save, replace

*Merge
*--------
use hog062.dta, clear
joinby entidad control linea serie using viv062.dta, _merge(_merge)
tab _merge
drop _merge
joinby entidad control linea serie  num_hog using per062.dta, _merge(_merge)
tab _merge
drop _merge
save "\\Sdssrv03\surveys\survey\VEN\EHM\2006\s2\data_merge\VEN_2006s2.dta", replace


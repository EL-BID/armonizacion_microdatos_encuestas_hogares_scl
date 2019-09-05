*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

clear
set more off
cd "\\Sdssrv03\surveys\survey\VEN\EHM\2008\s1\data_orig\"

*Identificacion bases de datos
*--------------------------------
use per081.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesop
cap drop id_*
save per081.dta, replace

use hog081.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesoh
save hog081.dta, replace

use viv081.dta, clear
sort entidad control linea serie
cap rename peso pesov
save viv081.dta, replace

*Merge
*--------
use hog081.dta, clear
joinby entidad control linea serie using viv081.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per081.dta, _merge(_merge)
tab _merge
drop _merge

saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2008\s1\data_merge\VEN_2008s1.dta", replace

 



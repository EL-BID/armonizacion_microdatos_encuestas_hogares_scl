*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

clear
cd "\\sdssrv03\Surveys\survey\VEN\EHM\2011\s1\data_orig\"  

use per111.dta, clear
cap drop id_*
sort entidad control linea serie num_hog 
cap rename peso pesop
save, replace
  
use hog111.dta, clear
sort entidad control linea serie
cap rename peso pesoh
cap drop id_*
save, replace

use viv111.dta, clear
sort entidad control linea serie
cap drop id_*
cap rename peso pesov
save, replace

*Merge
*--------
use hog111.dta, clear
joinby entidad control linea serie using viv111.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per111.dta, _merge(_merge)
tab _merge
drop _merge

saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2011\s1\data_merge\VEN_2011s1.dta", replace



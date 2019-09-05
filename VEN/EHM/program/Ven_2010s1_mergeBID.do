*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

clear
cd "\\sdssrv03\Surveys\survey\VEN\EHM\2010\s1\data_orig\"  

use per101.dta, clear
cap drop id_*
sort entidad control linea serie num_hog 
cap rename peso pesop
save per102.dta, replace
  
use hog101.dta, clear
sort entidad control linea serie
cap rename peso pesoh
cap drop id_*
save hog102.dta, replace

use viv101.dta, clear
sort entidad control linea serie
cap drop id_*
cap rename peso pesov
save viv102.dta, replace

*Merge
*--------
use hog101.dta, clear
joinby entidad control linea serie using viv101.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per101.dta, _merge(_merge)
tab _merge
drop _merge

saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2010\s1\data_merge\VEN_2010s1.dta", replace



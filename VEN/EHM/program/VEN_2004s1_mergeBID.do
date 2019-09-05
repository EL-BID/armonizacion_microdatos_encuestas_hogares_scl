*Elaboracion:  Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Octubre, 2013

******************************
**** MERGE VENEZUELA 2004 ****
******************************
* First Semester
clear
cd "\\sdssrv03\Surveys\survey\VEN\EHM\2004\s1\data_orig\"

* Ordeno variables
*----------------------
use "viv041.dta", clear
sort entidad control localidad area linea
save , replace

use "hog041.dta", clear
sort entidad control localidad area linea num_hog
save, replace

use "per041.dta", clear
sort entidad control localidad area linea num_hog 
save, replace

*Merge
*----------------------
use "hog041.dta", clear
joinby entidad control localidad area linea using "viv041.dta", _merge(_merge)
tab _merge
drop _merge

joinby entidad control localidad area linea num_hog using "per041.dta", _merge(_merge)
tab _merge
drop _merge

save "\\sdssrv03\Surveys\survey\VEN\EHM\2004\s1\data_merge\VEN_2004s1.dta", replace


*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off
cd "${surveysFolder}\survey\VEN\EHM\2004\s2\data_orig\"

*Genero IDH
*-------------------
use "hog042.dta", clear
sort entidad control localidad area linea
cap ren peso pesoh
save, replace

use "viv042.dta", clear
sort entidad control localidad area linea
cap rename peso pesov
save, replace

use "per042.dta", clear
sort entidad control localidad area line num_hog
cap rename peso pesop
save, replace

*Merge
*--------------
use "hog042.dta", clear
joinby entidad control localidad area linea using "viv042.dta", _merge (_merge)
tab _merge
drop _merge

joinby entidad control localidad area linea num_hog using "per042.dta", _merge (_merge)
tab _merge
drop _merge
save "${surveysFolder}\survey\VEN\EHM\2004\s2\data_merge\VEN_2004s2.dta", replace

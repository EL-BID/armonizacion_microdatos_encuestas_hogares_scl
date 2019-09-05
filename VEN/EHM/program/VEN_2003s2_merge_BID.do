*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

clear
set more off

cd "\\Sdssrv03\surveys\survey\VEN\EHM\2003\s2\data_orig\"

use viv032.dta, clear
rename peso pesov
sort entidad control localidad area linea 
save viv032_modif.dta, replace

use hog032.dta, clear
rename peso pesoh
sort entidad control localidad area linea num_hog
merge entidad control localidad area linea using viv032_modif.dta
tab _merge
drop _merge
sort entidad control localidad area linea num_hog
save hog032_modif.dta, replace

use per032, clear
rename peso pesop
sort entidad control localidad area linea num_hog
merge entidad control localidad area linea num_hog using hog032_modif.dta
tab _merge
drop _merge

saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2003\s2\data_merge\VEN_2003s2", replace

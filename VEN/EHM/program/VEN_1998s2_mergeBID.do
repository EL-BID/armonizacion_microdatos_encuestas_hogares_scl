*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "${surveysFolder}\survey\VEN\EHM\1998\s2\data_orig\"

use "viv982.dta", clear
destring _all, replace
gen num_hog=real(num_hoga)
ren peso pesov
sort entidad control area linea num_hog subdominio localidad
save "viv982_ci.dta", replace


*No encuentro bases originales de personas por eso utilizo vanven96, la cual utilizaron para armonizar la base.
use "vanven98.dta", clear
sort entidad control area linea num_hog subdominio localidad
merge entidad control area linea num_hog subdominio localidad using "viv982_ci.dta"
tab _merge
drop _merge

saveold "${surveysFolder}\survey\VEN\EHM\1998\s2\data_merge\VEN_1998s2.dta", replace

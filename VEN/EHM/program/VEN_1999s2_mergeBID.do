*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "\\Sdssrv03\surveys\survey\VEN\EHM\1999\s2\data_orig\"
use "viv992.dta", clear
destring _all, replace
ren num_hoga num_hog
ren peso pesov
sort entidad control area linea num_hog subdominio localidad
save "viv992_ci.dta", replace

*base vanven99 contiene merge personas y hogares
use "vanven99.dta", clear
sort entidad control area linea num_hog subdominio localidad
merge entidad control area linea num_hog subdominio localidad using "viv992_ci.dta"
tab _merge

drop _merge
saveold "\\Sdssrv03\surveys\survey\VEN\EHM\1999\s2\data_merge\VEN_1999s2.dta", replace

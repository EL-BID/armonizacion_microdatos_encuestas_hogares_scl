*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "\\Sdssrv03\surveys\survey\VEN\EHM\2001\s2\data_orig\"


****************************************************
use "viv012.dta", clear
destring _all, replace
ren num_hoga num_hog
ren peso pesov
ren num_per num_persv
sort entidad control area linea num_hog subdominio localida
save "viv012_ci.dta", replace


*base vanven99 contiene merge personas y hogaresuse "vanven01.dta", clear
sort entidad control area linea num_hog subdomin localida num_per
merge entidad control area linea num_hog subdomin localida using "viv012_ci.dta"
tab _merge
drop _merge*
saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2001\s2\data_merge\VEN_2001s2.dta", replace

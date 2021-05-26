*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

*Modificación : Mayra Sáenz Julio 2015

clear
set more off
cd "${surveysFolder}\survey\VEN\EHM\1996\s2\data_orig\"
use "viv962.dta", clear
destring _all, replace
ren num_hogare num_hog
ren peso pesov
sort entidad control area linea num_hog subdominio, stable
save "viv962_ci.dta", replace

*No encuentro bases originales de personas por eso utilizo vanven96, la cual utilizaron para armonizar la base.
use "vanven96.dta", clear
destring _all, replace
sort entidad control area linea num_hog subdominio, stable
merge entidad control area linea num_hog subdominio using "viv962_ci.dta"

saveold "${surveysFolder}\survey\VEN\EHM\1996\s2\data_merge\VEN_1996s2.dta", replace

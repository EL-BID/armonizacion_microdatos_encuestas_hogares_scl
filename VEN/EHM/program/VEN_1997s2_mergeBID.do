*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "\\Sdssrv03\surveys\survey\VEN\EHM\1997\s2\data_orig\"
use "viv972.dta", clear
destring _all, replace
gen area2=real(area)
drop area
ren area2 area
ren num_hogare num_hog
ren peso pesov
sort entidad control area linea num_hog subdominio localidad
save "viv972_ci.dta", replace

*No encuentro bases originales de personas por eso utilizo vanven96, la cual utilizaron para armonizar la base.
use "vanven97.dta", clear
destring _all, replace
sort entidad control area linea num_hog subdominio localidad

merge entidad control area linea num_hog subdominio localidad using "viv972_ci.dta"
tab _merge
drop _merge
save "\\Sdssrv03\surveys\survey\VEN\EHM\1997\s2\data_merge\VEN_1997s2.dta", replace
                       

*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "\\Sdssrv03\surveys\survey\VEN\EHM\2002\s2\data_orig\"
                
use "viv022.dta", clear
destring _all, replace
ren num_hoga num_hog
ren peso pesov
ren num_pers num_persv
sort entidad control area linea num_hog subdomin localida
save "viv022_ci.dta", replace

*base vanven99 contiene merge personas y hogares
use "vanven02.dta", clear
ren subdomin subdominio
ren localida localidad
sort entidad control area linea num_hog subdominio localidad

merge entidad control area linea num_hog subdomin localida using "viv022_ci.dta"
tab _merge
drop mergeh 
saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2002\s2\data_merge\VEN_2002s2.dta", replace

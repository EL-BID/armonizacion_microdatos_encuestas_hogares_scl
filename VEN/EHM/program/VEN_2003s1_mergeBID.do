*Elaboracion: Yessenia Loayza
*Fecha: Octubre, 2013

clear
set more off

cd "Y:\survey\VEN\EHM\2003\s1\data_orig\"
*Ordeno variables
*----------------------
use "viv031.dta", clear
sort entidad control subdominio localidad area linea
saveold, replace

use "hog031.dta", clear
sort entidad control subdominio localidad area linea num_hog
save, replace

use "per031.dta", clear
sort   entidad control subdominio localidad area linea num_hog 
save, replace

*Merge
*---------------------
use "viv031.dta", clear
joinby entidad control subdominio localidad area linea using "hog031.dta", _merge (_merge)
tab  _merge
drop _merge
sort  entidad control subdominio localidad area linea num_hog 

joinby entidad control subdominio localidad area linea num_hog using "per031.dta", _merge (_merge)
tab _merge
drop _merge
saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2003\s1\data_merge\VEN_2003s1.dta"


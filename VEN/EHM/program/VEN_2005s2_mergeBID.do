*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013


clear
set more off

cd "\\Sdssrv03\surveys\survey\VEN\EHM\2005\s2\data_orig\"

*Identificacion bases de datos
*--------------------------------
use "hog_205.dta", clear /*variables: linea y area sin observaciones localidad no esta en todas las bases*/
qui destring _all, replace
sort control entidad municipio parroquia serie 
cap ren peso pesoh
save, replace

use "viv_205.dta", clear  /*La base de Vivienda no contiene la var num_hog(esta vacia) no se puede hacer el merge */
qui destring _all, replace
sort control entidad municipio parroquia serie 
cap rename peso pesov
save, replace

use "per_205.dta", clear
qui destring _all, replace
rename peso pesop
sort control entidad municipio parroquia serie num_hog 
save, replace

*Merge
*--------
use "hog_205.dta", clear
joinby control entidad municipio parroquia serie  using "viv_205.dta", _merge(_merge)
tab _merge
drop _merge

joinby control entidad municipio parroquia serie num_hog using "per_205.dta", _merge(_merge)
tab _merge
drop _merge
save "\\Sdssrv03\surveys\survey\VEN\EHM\2005\s2\data_merge\VEN_2005s2.dta", replace

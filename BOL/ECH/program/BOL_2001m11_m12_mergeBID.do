************************
** MERGE BOLIVIA 2001 **
************************

* Creado por: Marcela G. Rubio  

clear
set more off

global ruta = "\\Sdssrv03\surveys\survey\BOL\ECH\2001\m11_m12\data_orig\"
global out = "\\Sdssrv03\surveys\survey\BOL\ECH\2001\m11_m12\data_merge"

use "$ruta\hogar.dta", clear

sort folio
save "$ruta\hogar.dta", replace

use "$ruta\mcv01.dta", clear
sort folio nro1 

merge m:1 folio using "$ruta\hogar.dta"
drop if _merge!=3
drop _merge

save "$out\BOL_2001m11_m12.dta", replace


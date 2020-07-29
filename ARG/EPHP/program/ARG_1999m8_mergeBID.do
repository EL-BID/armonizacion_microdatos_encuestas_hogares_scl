* 28 Aglomerados
*global ruta = "Y:/survey"

clear
set mem 30m
set more off
use "$ruta\ARG\EPHP\1999\m8\data_orig\per_arg99ag.dta", clear
sort aglomera codusu componen
by aglomera codusu componen : assert _N==1
save "$ruta\ARG\EPHP\1999\m8\data_orig\per_arg99ag_aux.dta", replace
use "$ruta\ARG\EPHP\1999\m8\data_orig\hog_arg99ag.dta", clear
sort aglomera codusu
by aglomera codusu : assert _N==1
save "$ruta\ARG\EPHP\1999\m8\data_orig\hog_arg99ag_aux.dta", replace

use "$ruta\ARG\EPHP\1999\m8\data_orig\hog_arg99ag_aux.dta", clear
sort aglomera codusu
by aglomera codusu : assert _N==1
merge aglomera codusu using "$ruta\ARG\EPHP\1999\m8\data_orig\per_arg99ag_aux.dta"
ta _merge
keep if realizad==1
drop _merge
save "$ruta\ARG\EPHP\1999\m8\data_merge\ARG_1999m8.dta", replace

erase "$ruta\ARG\EPHP\1999\m8\data_orig\per_arg99ag_aux.dta"
erase "$ruta\ARG\EPHP\1999\m8\data_orig\hog_arg99ag_aux.dta"

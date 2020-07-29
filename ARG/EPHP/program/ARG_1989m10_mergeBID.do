global ruta = "Y:/survey"

clear
set mem 30m
set more off
use "$ruta\ARG\EPHP\1989\m10\data_orig\hog_tot.dta", clear
sort cod
save "$ruta\ARG\EPHP\1989\m10\data_orig\hog_tot_aux.dta", replace
use "$ruta\ARG\EPHP\1989\m10\data_orig\pers_tot.dta", clear
sort cod componen
save "$ruta\ARG\EPHP\1989\m10\data_orig\pers_tot_aux.dta", replace

use "$ruta\ARG\EPHP\1989\m10\data_orig\hog_tot_aux.dta", clear
merge cod using "$ruta\ARG\EPHP\1989\m10\data_orig\pers_tot_aux.dta"
ta _merge
keep if realizad==1
drop _merge
sort cod componen
save "$ruta\ARG\EPHP\1989\m10\data_merge\ARG_1989m10.dta", replace

erase "$ruta\ARG\EPHP\1989\m10\data_orig\pers_tot_aux.dta"
erase "$ruta\ARG\EPHP\1989\m10\data_orig\hog_tot_aux.dta"

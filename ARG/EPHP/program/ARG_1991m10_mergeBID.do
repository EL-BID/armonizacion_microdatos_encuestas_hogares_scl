
global ruta = "Y:/survey"

clear
set mem 30m
set more off
use "$ruta\ARG\EPHP\1991\m10\data_orig\bthog.dta", clear
sort cod
save "$ruta\ARG\EPHP\1991\m10\data_orig\bthog_aux.dta", replace
use "$ruta\ARG\EPHP\1991\m10\data_orig\btper.dta", clear
sort cod componen
save "$ruta\ARG\EPHP\1991\m10\data_orig\btper_aux.dta", replace

use "$ruta\ARG\EPHP\1991\m10\data_orig\bthog_aux.dta", clear
merge cod using "$ruta\ARG\EPHP\1991\m10\data_orig\btper_aux.dta"
ta _merge
keep if realizad==1
drop _merge
sort cod componen
save "$ruta\ARG\EPHP\1991\m10\data_merge\ARG_1991m10.dta", replace

erase "$ruta\ARG\EPHP\1991\m10\data_orig\btper_aux.dta"
erase "$ruta\ARG\EPHP\1991\m10\data_orig\bthog_aux.dta"

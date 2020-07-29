
global ruta = "Y:/survey"

clear
set mem 30m
set more off
use "$ruta\ARG\EPHP\1987\m10\data_orig\bthog.dta", clear
sort cod
save "$ruta\ARG\EPHP\1987\m10\data_orig\bthog.dta", replace
use "$ruta\ARG\EPHP\1987\m10\data_orig\btper.dta", clear
sort cod componen
save "$ruta\ARG\EPHP\1987\m10\data_orig\btper.dta", replace

use "$ruta\ARG\EPHP\1987\m10\data_orig\bthog.dta", clear
merge cod using "$ruta\ARG\EPHP\1987\m10\data_orig\btper.dta"
ta _merge
drop _merge
sort cod componen
save "$ruta\ARG\EPHP\1987\m10\data_merge\ARG_1987m10.dta", replace


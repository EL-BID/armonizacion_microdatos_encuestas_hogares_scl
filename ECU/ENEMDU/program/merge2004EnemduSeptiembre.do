****************************
**** MERGE ECUADOR 2004 ****
****************************
clear
set mem 100m
set more off

use vi0304.dta, clear
destring area ciudad zona sector mpanel vivienda hogar, replace
sort area ciudad zona sector mpanel vivienda hogar 
save vi0304_s.dta, replace

use per0304.dta, clear
destring area ciudad zona sector mpanel vivienda hogar, replace
sort area ciudad zona sector mpanel vivienda hogar persona
merge area ciudad zona sector mpanel vivienda hogar using vi0304_s.dta
tab _merge
drop _merge

save "ecu04_sep.dta",replace





****************************
**** MERGE ECUADOR 2004 ****
****************************

set mem 100m
set more off

use vivienda0204.dta, clear
destring area ciudad zona sector panelm vivienda hogar, replace
sort area ciudad zona sector panelm vivienda hogar 
save vivienda0204_s.dta, replace

use personas0204.dta, clear
destring area ciudad zona sector panelm vivienda hogar, replace
sort area ciudad zona sector panelm vivienda hogar persona
merge area ciudad zona sector panelm vivienda hogar using vivienda0204_s.dta
tab _merge
drop _merge

save "ecu04_jun.dta",replace





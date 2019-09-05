*_:*:_*_:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:_*
* Emparejamiento de la base de personas con la de vivienda*
* Encuesta Integrada de Hogares                           *
* Bolivia 1993                                            *
*_:*:_*_:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:_*


*Mayra Sáenz Julio 2013
clear all
set more off


use "X:\ARM\BOL\1993\Orig_data\bd10_nueva\eih6_viv.dta"
sort hide
gen h903h = h903
label var h903h "Ponderador del hogar"

save "X:\ARM\BOL\1993\Orig_data\bd10_nueva\eih6_vivtemp.dta", replace

use "X:\ARM\BOL\1993\Orig_data\bd10_nueva\eih6_pob.dta", clear
rename hide hide1
tostring hide1, replace
gen hide=real(substr(hide1,1,6))
sort hide
gen h903i = h903
label var h903i "Ponderador del individuo"

merge m:m hide using "X:\ARM\BOL\1993\Orig_data\bd10_nueva\eih6_vivtemp.dta"
tab _merge

/*
                 _merge |      Freq.     Percent        Cum.
------------------------+-----------------------------------
            matched (3) |     20,160      100.00      100.00
------------------------+-----------------------------------
                  Total |     20,160      100.00
*/


drop _merge



label var hide "Identificador del hogar"
label var hide1 "Identificador del individuo"

 save "X:\ARM\BOL\1993\Orig_data\bol93.dta", replace

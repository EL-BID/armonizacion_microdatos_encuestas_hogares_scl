*_:*:_*_:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:_*
* Emparejamiento de la base de personas con la de vivienda*
* Encuesta Integrada de Hogares                           *
* Bolivia 1994                                            *
*_:*:_*_:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:__:*:_*


*Mayra Sáenz Julio 2013
clear all
set more off

use "${surveysFolder}\ARM\BOL\1994\Orig_data\eih7_viv.dta"
sort hide
gen h901h = h901
label var h901h "Ponderador del hogar"

save "${surveysFolder}\ARM\BOL\1994\Orig_data\eih7_vivtemp.dta", replace

clear all

use "${surveysFolder}\ARM\BOL\1994\Orig_data\eih7_pob.dta", clear
rename hide hide1
tostring hide1, replace
gen hide=real(substr(hide1,1,6))
sort hide
gen h901i = h901
label var h901i "Ponderador del individuo"

merge m:m hide using "${surveysFolder}\ARM\BOL\1994\Orig_data\eih7_vivtemp.dta"
tab _merge

/*
                _merge |      Freq.     Percent        Cum.
------------------------+-----------------------------------
            matched (3) |     28,048      100.00      100.00
------------------------+-----------------------------------
                  Total |     28,048      100.00
*/


drop _merge



label var hide "Identificador del hogar"
label var hide1 "Identificador del individuo"

 save "${surveysFolder}\ARM\BOL\1994\Orig_data\bol94.dta", replace

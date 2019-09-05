clear
set more off


local in1 "X:\ARM\NIC\2001\Orig_data\emnv01datosdeviviendayhogar.dta"
local in2 "X:\ARM\NIC\2001\Orig_data\emnv03poblacion.dta"
local in3 "X:\ARM\NIC\2001\Orig_data\emnv06negociopartea.dta"
local in4 "X:\ARM\NIC\2001\Van_data\ynlmmo.dta"
local in5 "X:\ARM\NIC\2001\Van_data\ynlman.dta"
local in6 "X:\ARM\NIC\2001\Van_data\remesas.dta"
local in7 "X:\ARM\NIC\2001\Van_data\Equipos01.dta"
local in8 "X:\ARM\NIC\2001\Van_data\CONSING.dta"

forvalues i=1(1)8 {
use `in`i''
drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
save `in`i'', replace
}


use `in1'
forvalues h=2(1)8 {
merge id_hogar using `in`h''
tab _merge
drop _merge
sort id_hogar
}

save "X:\ARM\NIC\2001\Van_data\Nic01.dta", replace

clear
set mem 300m

local in1 "\\\resb636-a\database\ARM\NIC\1998\Orig_data\nvperson.dta"
local in2 "\\\resb636-a\database\ARM\NIC\1998\Orig_data\vivienv.dta"
local in3 "\\\resb636-a\database\ARM\NIC\1998\Van_data\Equipos98.dta"
local in4 "\\\resb636-a\database\ARM\NIC\1998\Orig_data\gastosb.dta"

/*forvalues i=1(1)4 {
use `in`i''
sort i00

gen first=0
quietly by i00: replace first=1 if _n==1

gen vectoru=1
gen double idy=sum(vectoru) if first==1
recode idy .=0
egen double id_hogar=sum(idy), by (i00)

drop first vectoru idy
sort id_hogar

save `in`i'', replace
}
*/

use `in1'
forvalues h=2(1)4 {
merge id_hogar using `in`h''
tab _merge
drop _merge
sort id_hogar
}

save "\\\resb636-a\database\ARM\NIC\1998\Van_data\Nic98.dta", replace

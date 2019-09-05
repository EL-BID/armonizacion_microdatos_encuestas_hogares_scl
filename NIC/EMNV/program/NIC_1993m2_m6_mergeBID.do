clear 
set mem 300m
set more on

use "\\\resb636-a\database\ARM\NIC\1993\Orig_data\vemnv93.dta"
gen aux=string(noformu)
destring aux, replace
drop noformu
rename aux noformu
sort noformu
save"\\\resb636-a\database\ARM\NIC\1993\Orig_data\vemnv93.dta", replace
clear

use "\\\resb636-a\database\ARM\NIC\1993\Orig_data\othy.dta"
gen aux=string(noformu)
destring aux, replace
drop noformu
rename aux noformu
sort noformu
save"\\\resb636-a\database\ARM\NIC\1993\Orig_data\othy.dta", replace
clear

use "\\\resb636-a\database\ARM\NIC\1993\Van_data\pemnv93.dta"
gen aux=string(noformu)
destring aux, replace
drop noformu
rename aux noformu
sort noformu

merge noformu using \\\resb636-a\database\ARM\NIC\1993\Orig_data\othy.dta
table _merge
drop _merge

sort noformu
merge noformu using \\\resb636-a\database\ARM\NIC\1993\Orig_data\vemnv93.dta
table _merge

save "\\\resb636-a\database\ARM\NIC\1993\Van_data\Nic93.dta"

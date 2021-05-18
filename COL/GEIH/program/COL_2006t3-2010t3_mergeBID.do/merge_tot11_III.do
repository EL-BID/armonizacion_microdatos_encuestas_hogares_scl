*Yessenia L -> Agrego el ingreso homologado
*----------------------------------------------

clear
use "${surveysFolder}\survey\COL\GEIH\2011\a\data_orig\personas 2011.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
save "${surveysFolder}\survey\COL\GEIH\2011\a\data_orig\personas 2011.dta", replace


clear
use "${surveysFolder}\survey\COL\GEIH\2011\a\data_orig\hogares 2011.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
merge idh using "${surveysFolder}\survey\COL\GEIH\2011\a\data_orig\personas 2011.dta"
tab _merge
drop _merge idh
cap egen id =concat (directorio secuencia_p orden)
sort id
destring mes, replace
keep if mes>=7 & mes<=9
keep  id nper-fex_dpto_c pet- ingtot
save "${surveysFolder}\survey\COL\GEIH\2011\a\data_merge\pov_2011t3.dta", replace

clear
use "${surveysFolder}\\survey\COL\GEIH\2011\t3\data_merge\COL_2011t3.dta", clear
cap egen id =concat (directorio secuencia_p orden)
sort id
merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2011\a\data_merge\pov_2011t3.dta"
tab _merge
drop _merge
save "${surveysFolder}\\survey\COL\GEIH\2011\t3\data_merge\COL_2011t3.dta", replace

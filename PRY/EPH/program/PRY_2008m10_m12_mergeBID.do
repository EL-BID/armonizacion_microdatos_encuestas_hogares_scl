* MERGE PARAGUAY 2008
clear
set more off

* Households

use "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r01_08.dta", clear
sort upm nvivi nhoga
save "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r01_08.dta", replace

use "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\ingrefam_08.dta", clear
sort upm nvivi nhoga
save "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\ingrefam_08.dta", replace

* Individuals

use "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r02_08.dta", clear
sort upm nvivi nhoga l02 
save "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r02_08.dta", replace

use "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r03_08.dta", clear
sort upm nvivi nhoga l02 
save "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r03_08.dta", replace

* merge

use "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r02_08.dta", clear

merge upm nvivi nhoga l02 using "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r03_08.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\r01_08.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "\\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_mecovi\stata\ingrefam_08.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

saveold \\sdssrv03\surveys\survey\PRY\EPH\2008\m10_m12\data_orig\PRY_2008m10_m12.dta,replace



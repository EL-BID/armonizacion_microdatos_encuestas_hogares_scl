
*** MR_OCT_06_2014


* MERGE PARAGUAY 2005

* Households

use "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r01_05.dta", clear
sort upm nvivi nhoga
save ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r01_05_s.dta, replace

use "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\ingresfam2005.dta", clear
sort upm nvivi nhoga
save ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\ingresfam2005_s.dta, replace

* Individuals

use "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r02_05.dta", clear
sort upm nvivi nhoga l02 
save ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r02_05_s.dta, replace

use "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r03_05.dta", clear
sort upm nvivi nhoga l02 
save ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r03_05_s.dta, replace

use "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r04_05.dta", clear
sort upm nvivi nhoga l02 
save ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r04_05_s.dta, replace

use "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r02_05_s.dta", clear

merge upm nvivi nhoga using ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r01_05_s.dta
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\ingresfam2005_s.dta
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r03_05_s.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\Stata\r04_05_s.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

saveold ${surveysFolder}\survey\PRY\EPH\2005\m10_m12\data_orig\PRY_2005m10_m12.dta,replace

clear

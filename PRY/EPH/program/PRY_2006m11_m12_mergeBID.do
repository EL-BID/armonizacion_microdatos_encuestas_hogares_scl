
***************************************************
***Revised (by Ted) March, 2008********************
***r03_05.dta, r04_05.dta merge for Sociometro ****
***************************************************

clear
cd ${surveysFolder}\ARM\PRY\EIH\2006\Orig_data

capture log close
log using MERGEPRY2006_BID.log, replace 

* MERGE PARAGUAY 2006

* Households

use "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r01_06.dta", clear
sort upm nvivi nhoga
save "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r01_06_s.dta", replace

use "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\ingresfam2006.dta", clear
sort upm nvivi nhoga
save "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\ingresfam2006_s.dta", replace

* Individuals

use "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r02_06.dta", clear
sort upm nvivi nhoga l02 
save "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r02_06_s.dta", replace

use "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r03_06.dta", clear
sort upm nvivi nhoga l02 
save "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r03_06_s.dta", replace

use "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r04_06.dta", clear
sort upm nvivi nhoga l02 
save "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r04_06_s.dta", replace

use "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r02_06_s.dta", clear

merge upm nvivi nhoga using "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r01_06_s.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\ingresfam2006_s.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r03_06_s.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\Stata\r04_06_s.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

saveold "${surveysFolder}\ARM\PRY\EIH\2006\Orig_data\pry06.dta",replace

clear

log close


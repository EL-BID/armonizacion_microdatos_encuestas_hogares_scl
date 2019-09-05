clear
set more off


use "X:\ARM\BLZ\2007\Orig_data\merged_10.dta" 

foreach var of varlist q* {
rename `var' c`var'
}



sort  district urbrur ctv ednumber hhnumber

save "X:\ARM\BLZ\2007\Orig_data\CoreBelize.dta", replace  





use "X:\ARM\BLZ\2007\Orig_data\housing_cleaned_.dta"

sort  district urbrur ctv ednumber hhnumber

merge district urbrur ctv ednumber hhnumber using "X:\ARM\BLZ\2007\Orig_data\CoreBelize.dta"

drop _merge

sort  district urbrur ctv ednumber hhnumber cq10 

save "X:\ARM\BLZ\2007\Orig_data\CoreBelize.dta", replace  




use "X:\ARM\BLZ\2007\Orig_data\qls_household_cleaning.dta" 

sort  district urbrur ctv ednumber hhnumber

save "X:\ARM\BLZ\2007\Orig_data\qls_household_cleaning.dta", replace 




use "X:\ARM\BLZ\2007\Orig_data\qls_main_cleaning.dta" 

gen cq10=q40

sort district urbrur ctv ednumber hhnumber 

merge district urbrur ctv ednumber hhnumber  using "X:\ARM\BLZ\2007\Orig_data\qls_household_cleaning.dta"


drop _merge

sort  district urbrur ctv ednumber hhnumber cq10 

save "X:\ARM\BLZ\2007\Orig_data\QualityBelize.dta", replace 





use "X:\ARM\BLZ\2007\Orig_data\CoreBelize.dta"

sort  district urbrur ctv ednumber hhnumber cq10 

merge district urbrur ctv ednumber hhnumber cq10  using "X:\ARM\BLZ\2007\Orig_data\QualityBelize.dta"



tab _merge

sort  district urbrur ctv ednumber hhnumber cq10

drop if cq10==.

gen miss=0
replace miss=1 if q51!=.
by district urbrur ctv ednumber hhnumber, sort: egen test1=sum(miss)
tab test1

drop if test1>1


save "X:\ARM\BLZ\2007\Orig_data\Core&QualityBelize2007.dta", replace

br district urbrur ctv ednumber hhnumbe  cq10 q10  q40


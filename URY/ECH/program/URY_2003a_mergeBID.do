************************
** MERGE URUGUAY 2003 **
************************

clear
set more off
set mem 100m

use p2003web.dta

merge correlativ using h2003web.dta
tab _merge
drop _merge

sort correlativ nper
save ury03.dta


/*
clear
set mem 50m
set more off


use "${surveysFolder}\ARM\URU\ECH\2003\Orig_data\h2003web.dta", clear
sort correlativ
save "${surveysFolder}\ARM\URU\ECH\2003\Orig_data\h2003web.dta", replace

use "${surveysFolder}\ARM\URU\ECH\2003\Orig_data\p2003web.dta", clear
sort correlativ nper
save "${surveysFolder}\ARM\URU\ECH\2003\Orig_data\p2003web.dta", replace

merge correlativ using "${surveysFolder}\ARM\URU\ECH\2003\Orig_data\h2003web.dta"
tab _merge
drop _merge
sort correlativ nper
compress
save "${surveysFolder}\ARM\URU\ECH\2003\Van_data\ury03.dta", replace
*/

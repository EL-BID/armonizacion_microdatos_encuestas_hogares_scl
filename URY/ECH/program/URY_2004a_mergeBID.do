************************
** MERGE URUGUAY 2004 **
************************

clear
set more off
set mem 100m

use p2004web.dta

merge correlativ using h2004web.dta
tab _merge
drop _merge

sort correlativ nper

save ury04.dta


/*
clear
set mem 50m
set more off


use "${surveysFolder}\ARM\URU\ECH\2004\Orig_data\h2004web.dta", clear
sort correlativ
save "${surveysFolder}\ARM\URU\ECH\2004\Orig_data\h2004web.dta", replace

use "${surveysFolder}\ARM\URU\ECH\2004\Orig_data\p2004web.dta", clear
sort correlativ nper
save "${surveysFolder}\ARM\URU\ECH\2004\Orig_data\p2004web.dta", replace

merge correlativ using "${surveysFolder}\ARM\URU\ECH\2004\Orig_data\h2004web.dta"
tab _merge
drop _merge
sort correlativ nper
compress
save "${surveysFolder}\ARM\URU\ECH\2004\Van_data\ury04.dta", replace

*/

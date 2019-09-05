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


use "X:\ARM\URU\ECH\2003\Orig_data\h2003web.dta", clear
sort correlativ
save "X:\ARM\URU\ECH\2003\Orig_data\h2003web.dta", replace

use "X:\ARM\URU\ECH\2003\Orig_data\p2003web.dta", clear
sort correlativ nper
save "X:\ARM\URU\ECH\2003\Orig_data\p2003web.dta", replace

merge correlativ using "X:\ARM\URU\ECH\2003\Orig_data\h2003web.dta"
tab _merge
drop _merge
sort correlativ nper
compress
save "X:\ARM\URU\ECH\2003\Van_data\ury03.dta", replace
*/

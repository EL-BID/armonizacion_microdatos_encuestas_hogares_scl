************************
** MERGE URUGUAY 2005 **
************************

clear
set more off
set mem 100m

use p2005.dta

merge correlativ using h2005.dta
tab _merge
drop _merge

sort correlativ nper

save ury05.dta


/*
clear
set mem 50m
set more off


use "X:\ARM\URU\ECH\2005\Orig_data\h2005.dta", clear
sort correlativ
save "X:\ARM\URU\ECH\2005\Orig_data\h2005.dta", replace

use "X:\ARM\URU\ECH\2005\Orig_data\p2005.dta", clear
sort correlativ nper
save "X:\ARM\URU\ECH\2005\Orig_data\p2005.dta", replace

merge correlativ using "X:\ARM\URU\ECH\2005\Orig_data\h2005.dta"
tab _merge
drop _merge
sort correlativ nper
compress
save "X:\ARM\URU\ECH\2005\Van_data\ury05.dta", replace


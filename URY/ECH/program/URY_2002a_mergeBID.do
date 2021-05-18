************************
** MERGE URUGUAY 2002 **
************************

clear
set more off
set mem 100m

use p2002web.dta

merge correlativ using h2002web.dta
tab _merge
drop _merge

sort correlativ nper

save ury02.dta

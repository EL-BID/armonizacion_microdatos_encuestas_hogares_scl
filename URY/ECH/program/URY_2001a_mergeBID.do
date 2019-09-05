************************
** MERGE URUGUAY 2001 **
************************

clear
set more off
set mem 100m

use p2001web.dta

merge correlativ using h2001web.dta
tab _merge
drop _merge

sort correlativ nper

save ury01.dta
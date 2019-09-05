
use H_2008_Publico.dta, clear
sort correlat

save H2008.dta, replace


use P_2008_Publico.dta, clear
sort correlat

save P_2008.dta, replace

*Merge 

merge correlat using H2008.dta
sort correlat
tab _merge
drop _merge

save ury08.dta, replace

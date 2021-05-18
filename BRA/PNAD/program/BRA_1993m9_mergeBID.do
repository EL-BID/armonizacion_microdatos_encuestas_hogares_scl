*Yessenia Loayza
*Septiembre 2013

clear
set more off
cd "${surveysFolder}\survey\BRA\PNAD\1993\m9\data_orig\"

quietly infile using "brasil93_dom.dct", using (DOM93.DAT) clear
egen id=concat(uf control v0103)
sort id
saveold "brasil93_dom.dta", replace

quietly infile using "brasil93_per.dct", using(PES93.DAT) clear
egen id=concat(uf control v0103)
sort id
saveold "brasil93_per.dta", replace

clear
use "brasil93_dom.dta", clear
merge id using "brasil93_per.dta"
keep if v0201==1 & v0104==1
tab _merge
drop if _merge==1
saveold "${surveysFolder}\survey\BRA\PNAD\1993\m9\data_merge\BRA_1993m9.dta", replace




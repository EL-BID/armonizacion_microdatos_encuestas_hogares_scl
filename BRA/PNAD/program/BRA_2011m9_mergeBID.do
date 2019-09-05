*Febrero 2011
*Yessenia Loayza

clear
set more off
cd "\\Sdssrv03\surveys\survey\BRA\PNAD\2011\m9\data_orig\"

*Convierto la base a formato .dta 
*------------------------------------

infile using "dom2011.do", using("DOM2011.TXT")
sort uf v0102 v0103
saveold "dom2011.dta", replace
clear
set more off
infile using "pes2011.do", using("PES2011.txt")
sort uf v0102 v0103
saveold "pes2011.dta", replace


*2.- Merge
*-------------------------------
clear
use "pes2011.dta" , clear
merge uf v0102 v0103 using "dom2011.dta"
tab _merge
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
drop _merge
saveold "\\Sdssrv03\surveys\survey\BRA\PNAD\2011\m9\data_merge\BRA_2011m9.dta", replace





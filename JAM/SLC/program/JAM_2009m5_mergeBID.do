*Yanira Oviedo
			
			*JSLC 2009*
			***********

*Se consolida una base para construir los indicadores de sociómetro. No obstante, se cuenta con más módulos 
*que el investigador puede unir a este consolidado. 

cd "${surveysFolder}\Jamaica\2009\JSLC\Datos Originales\STATA"

use rec001.dta, clear
sort serial
save, replace

use rec002.dta, clear
sort serial
save, replace

use rec003.dta, clear
ren  r2_indv id_indv
sort serial id_indv
save, replace

use rec004.dta, clear
ren  a1_indv id_indv
sort serial id_indv
save, replace

use rec005.dta, clear
ren  a2_indv id_indv
sort serial id_indv
save, replace

use rec006.dta, clear
ren  a3_indv id_indv
sort serial id_indv
save, replace

use rec007.dta, clear
ren  b1_indv id_indv
sort serial id_indv
save, replace

use rec008.dta, clear
ren  b2_indv id_indv
sort serial id_indv
save, replace

use rec022.dta, clear
sort serial 
save, replace

use rec023.dta, clear
ren  k2_indv id_indv
sort serial id_indv
save, replace


* merge
*-------
use rec003.dta, clear
merge serial id_indv using rec004.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec005.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec006.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec007.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec008.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec023.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial using rec001.dta
tab _merge
drop _merge 
sort serial 

merge serial using rec002.dta
tab _merge
drop _merge 
sort serial 

merge serial using rec022.dta
tab _merge
drop _merge 

save jam09.dta, replace

*Inclusión de los agregados de consumo
*Mayra Sáenz Diciembre 2013

clear all
global path = "${surveysFolder}\BID\JAM\Jam SLC"
use "$path\SLC\2009\m5\data_orig\jam09.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
sort serial
capture drop _merge
merge m:m serial using  "$path\SLC\2009\m5\data_orig\STATA anual con agregados de Consumo\annual09.dta",force
tab _merge
drop _merge


ds, has(type string)
    foreach var of varlist `r(varlist)' {
        replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"N")
        destring `var', replace
    }


saveold "$path\SLC\2009\m5\data_merge\JAM_2009m5.dta", replace












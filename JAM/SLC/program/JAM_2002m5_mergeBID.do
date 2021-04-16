/*
use "${surveysFolder}\ARM\JAM\2002\Orig_data\jam02-LFS.dta"
destring _all, replace
compress
format ID %20.0f

ren PARISH parish
ren CONST const
ren ED district
ren DWELL dwelling
ren HHNO hh
ren SERIAL serial
ren IND ind

sort parish const district dwelling hh ind

save "${surveysFolder}\ARM\JAM\2002\Van_data\jam02_lfs.dta", replace

clear

***********************************************************************************************************
*****                                      JAMAICA 2002                                               *****
*****                            SLC 2002 (SURVEY OF LIVING CONDITIONS)                               *****
*****                                         MAYO                                                    *****
***********************************************************************************************************
use "${surveysFolder}\ARM\JAM\2002\Orig_data\jam02_jslc.dta", clear
destring _all, replace
compress
drop _merge

sort serial ind
save "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", replace

use "${surveysFolder}\ARM\JAM\2002\Orig_data\rec033.dta"
destring _all, replace

gen h2c=real(h02)
drop h02
ren h2c h02
reshape wide h02 h03 h04, i(serial) j(item_cd)
sort serial 
save "${surveysFolder}\ARM\JAM\2002\Van_data\rec33.dta", replace

use "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", clear
merge serial using "${surveysFolder}\ARM\JAM\2002\Van_data\rec33.dta"
drop _merge
sort serial ind
save "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", replace

clear
use "${surveysFolder}\ARM\JAM\2002\Orig_data\rec037b.dta"
destring _all, replace
reshape wide k02, i(serial ind) j(item_cd)
sort serial ind
save "${surveysFolder}\ARM\JAM\2002\Van_data\rec37b.dta", replace

use "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", clear
merge serial ind using "${surveysFolder}\ARM\JAM\2002\Van_data\rec37b.dta"
tab _merge
drop if _merge==2
drop _merge
sort serial ind
save "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", replace
clear

use "${surveysFolder}\ARM\JAM\2002\Orig_data\rec030.dta"
destring _all, replace
reshape wide f06 f07 f08, i(serial) j(item_cd)

** HAY HOGARES CON CONSUMO NEGATIVO (AUNQUE CASI CERO) EN MUCHOS ITEMS **
** f07... es el autoconsumo **

foreach var of varlist f07* {
tab serial if `var'<0
sum `var' if `var'<0
}
foreach var of varlist f06* f07* f08* {
replace `var'=. if `var'<0
}

sort serial 
save "${surveysFolder}\ARM\JAM\2002\Van_data\rec30.dta", replace

use "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", clear
merge serial using "${surveysFolder}\ARM\JAM\2002\Van_data\rec30.dta"
drop _merge
sort parish const district dwelling hh ind

save "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", replace

merge parish const district dwelling hh ind using "${surveysFolder}\ARM\JAM\2002\Van_data\jam02_lfs.dta"
tab _merge

/* _merge	Freq.	Percent	Cum.
			
1	6,995	27.96	27.96
3	18,021	72.04	100.00
			
Total	25,016	100.00 */
gen muestra=0
replace muestra=1 if _merge==3

drop _merge
sort serial ind
save "${surveysFolder}\ARM\JAM\2002\Van_data\jam02.dta", replace


*/


*JSLC 2002
set mem 100m
set more off

* Archivo de ingresos
use rec037.dta,clear

collapse(sum) k02, by(serial ind)
sort serial ind

save rec037_inc, replace 
collapse(sum) k02, by(serial)
destring serial, replace
sort serial
rename k02 ytoth
save rec037_inch, replace 

* Creacion de archivo de hogares

use rec001.dta,clear
merge serial using rec043.dta
sort serial
tab _merge
drop _merge

merge serial using annual.dta
sort serial
tab _merge
drop _merge

merge serial using rec034.dta
sort serial
tab _merge
drop _merge

merge serial using rec035.dta
sort serial
tab _merge
drop _merge

merge serial using rec039.dta
sort serial
tab _merge
drop _merge

merge serial using rec037_inch.dta
sort serial
tab _merge
drop _merge

merge serial using rec036.dta

sort serial
tab _merge
drop _merge

save jam02_jslc_h.dta, replace

*Creacion de archivo de personas

use educ.dta,clear
drop age sex relatn parish area edwght parwght finwght  decile quintile
save educ_s.dta, replace

use health.dta,clear
drop age sex area  parish newwght edwght parwght finwght hhsize cons decile quintile
save health_s.dta, replace

use ssnet.dta,clear
drop  hhsize quintile parish area edwght parwght  age sex relatn bpov apov pov fpov
save ssnet_s.dta, replace

* merge

use rec044.dta,clear
merge serial ind using educ_s.dta
sort serial ind
tab _merge
drop _merge

merge serial ind using health_s.dta
sort serial ind
tab _merge
drop _merge

merge serial ind using ssnet_s.dta
sort serial ind
tab _merge
drop _merge

merge serial ind using rec038.dta
sort serial ind
tab _merge
drop if _merge==2
drop _merge

merge serial ind using rec040.dta
sort serial ind
tab _merge
drop _merge

merge serial ind using rec041.dta
sort serial ind
tab _merge
drop _merge

merge serial ind using rec011.dta
sort serial ind
tab _merge
drop _merge

merge serial ind using rec012.dta
sort serial ind
tab _merge
drop _merge

save jam02_jslc_p.dta, replace

* Merge personas hogares.
use jam02_jslc_p.dta, clear
merge serial using jam02_jslc_h.dta
tab _merge

save jam02_jslc.dta, replace

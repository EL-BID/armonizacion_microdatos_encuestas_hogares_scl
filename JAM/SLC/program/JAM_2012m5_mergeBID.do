* Marcela G. Rubio
			
			*JSLC 2012*
			***********

*Se consolida una base para construir los indicadores de sociómetro. No obstante, se cuenta con más módulos 
*que el investigador puede unir a este consolidado. 
*Modificación Mayra Sáenz - Septiembre 2015: se cambia el path
*cd "M:\survey\JAM\SLC\2012\m5\data_orig"

cd "\\Sdssrv03\surveys\survey\JAM\SLC\2012\m5\data_orig\\"

use rec001.dta, clear
sort serial
save, replace

use rec002.dta, clear
sort serial
save, replace

use rec003.dta, clear
sort serial ind
save, replace

use rec004.dta, clear
sort serial ind
save, replace

use rec005.dta, clear
sort serial ind
save, replace

use rec006.dta, clear
sort serial ind
save, replace

use rec007.dta, clear
sort serial ind
save, replace

use rec008.dta, clear
sort serial ind
save, replace

use rec022.dta, clear
sort serial 
save, replace

use annual.dta, clear
sort serial
save, replace

use path.dta, clear
sort serial ind
save, replace

use povline.dta, clear
sort serial ind
save, replace

use rec033.dta, clear
sort serial ind
save, replace

use rec034.dta, clear
sort serial ind
save, replace

use rec035.dta, clear
sort serial ind
save, replace

use rec036.dta, clear
sort serial ind
save, replace

use rec037.dta, clear
sort serial ind
save, replace

use rec038.dta, clear
sort serial ind
save, replace

use rec039.dta, clear
sort serial ind
save, replace

*Modificación Mayra Sáenz - Septiembre 2015
*Incluyo el módulo de housing.
use rec026.dta, clear
sort serial record
save, replace


* merge
*-------
*Modificación Mayra Sáenz - Septiembre 2015: se cambia el path
*global ruta = "M:\survey\JAM\SLC\2012\m5\data_merge\"
global ruta = "\\Sdssrv03\surveys\\survey\JAM\SLC\2012\m5\data_merge\"

use rec003.dta, clear
merge m:1 serial using rec001.dta
drop _merge 
sort serial ind

merge m:1 serial using rec001.dta
drop _merge 
sort serial ind

merge m:1 serial using rec002.dta
drop _merge 
sort serial ind

merge m:1 serial ind using rec004.dta
drop _merge 
sort serial ind

merge m:1 serial ind using rec005.dta
drop _merge 
sort serial ind

merge m:1 serial ind using rec006.dta
drop _merge 
sort serial ind

merge m:1 serial ind using rec007.dta
drop _merge 
sort serial ind

merge m:1 serial ind using rec008.dta
drop _merge 
sort serial ind

merge m:1 serial using annual.dta
drop _merge 
sort serial ind

merge m:1 serial ind using path.dta
drop _merge 
sort serial ind

merge m:m serial ind using povline.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec033.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec034.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec035.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec036.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec037.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec038.dta
drop _merge 
sort serial ind

merge m:m serial ind using rec039.dta
drop _merge 
sort serial ind


merge m:1 serial using rec026.dta
drop _merge 
sort serial ind

order edwght finwght a9 a10 a20 a21 b5_distance b6_distance b7_distance t_meal t_noncon i21_1 tot_tax tot_wat tot_telec tot_telel tot_mort rent hhsize1 hhsize2 tcgift, last
order electric-popquint, last
order a13 a16, last
order povline-per, last

foreach var of varlist ageyrs-i41{
destring `var', replace
}

saveold "$ruta\JAM_2012m5.dta", replace












*Yanira Oviedo
			
			*JSLC 2008*
			***********

*Se consolida una base para construir los indicadores de sociómetro. No obstante, se cuenta con más módulos 
*que el investigador puede unir a este consolidado. 

cd "\\sdssrv03\Surveys\Jamaica\2008\JSLC\Datos Originales\STATA"

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

use rec009.dta, clear
ren  b3_indv id_indv
sort serial id_indv
save, replace

use rec010.dta, clear
ren  b4_indv id_indv
sort serial id_indv
save, replace

use rec011.dta, clear
ren  b5_indv id_indv
sort serial id_indv
save, replace

use rec027.dta, clear
sort serial 
save, replace

use rec028.dta, clear
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

merge serial id_indv using rec009.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec010.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec011.dta
tab _merge
drop _merge 
sort serial id_indv

merge serial id_indv using rec028.dta
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

merge serial using rec027.dta
tab _merge
drop _merge 


save jam08.dta, replace



************************
** MERGE BOLIVIA 2002 **
************************

* Última versión: Marcela G. Rubio 

/*
clear
set more off
set mem 100m

use "mcv02 - hogar.dta", clear
sort folio
save "mcv02 - hogar.dta", replace

use "mcv02 - individuos.dta", clear
sort folio nro1 

merge folio using "mcv02 - hogar.dta"
tab _merge
drop _merge

save bol02.dta
*/


* Se vuelve a hacer el merge ya que algunas variables de vivienda quedaban fuera
clear
set more off
set mem 100m

cd "${surveysFolder}\survey\BOL\ECH\2002\m11_m12\data_orig"

use "hogar.dta", clear
sort folio
saveold "hogar.dta", replace

use "mcv02final1.dta", clear
sort folio

merge m:1 folio using "hogar.dta", force
drop _merge

save "${surveysFolder}\survey\BOL\ECH\2002\m11_m12\data_merge\BOL_2002m11_m12.dta", replace

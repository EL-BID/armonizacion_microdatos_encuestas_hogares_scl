		******************************
		***BASE TOTAL NACIONAL 2008***
		******TERCER TRIMESTRE********
		******************************

/* Y.L -. comento temporalmente

cd "${surveysFolder}\Colombia\2008\GEIH\Datos" 
use IIItrimestre_cab.dta
append using IIItrimestre_res.dta

gen temp=fex_c/3
replace fex_c=round(temp,1)
drop temp
*/


*Yessenia L -> Agrego el ingreso homologado
*----------------------------------------------
/*
clear
use "${surveysFolder}\survey\COL\GEIH\2008\a\data_orig\personas 2008.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
save "${surveysFolder}\survey\COL\GEIH\2008\a\data_orig\personas 2008.dta", replace
*/

clear
use "${surveysFolder}\survey\COL\GEIH\2008\a\data_orig\hogares2008.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
merge idh using "${surveysFolder}\survey\COL\GEIH\2008\a\data_orig\personas 2008.dta"
tab _merge
drop _merge idh
cap egen id =concat (directorio secuencia_p orden)
sort id
destring mes, replace
keep if mes>=7 & mes<=9
keep  id mes nper-fex_dpto_c pet- ingtot
save "${surveysFolder}\survey\COL\GEIH\2008\a\data_merge\pov_2008t3.dta", replace

clear
use "${surveysFolder}\\survey\COL\GEIH\2008\t3\data_merge\COL_2008t3.dta", clear
cap egen id =concat (directorio secuencia_p orden)
sort id
merge id using "${surveysFolder}\survey\COL\GEIH\2008\a\data_merge\pov_2008t3.dta"
tab _merge
drop _merge
save "${surveysFolder}\\survey\COL\GEIH\2008\t3\data_merge\COL_2008t3.dta", replace


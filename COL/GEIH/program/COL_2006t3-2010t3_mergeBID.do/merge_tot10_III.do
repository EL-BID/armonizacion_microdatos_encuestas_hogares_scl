		******************************
		***BASE TOTAL NACIONAL 2010***
		******TERCER TRIMESTRE********
		******************************

/* Y.L. -> lo comento temp
cd "\\sdssrv03\Surveys\Colombia\2010\GEIH\3thquarter\Datos" 
use IIItrimestre_cab.dta, clear
append using IIItrimestre_res.dta

gen temp=fex_c/3
replace fex_c=round(temp,1)
drop temp
compress
saveold "\\sdssrv03\Surveys\Colombia\2010\GEIH\3thquarter\Datos\col10.dta", replace
*/

*Yessenia L -> Agrego el ingreso homologado
*----------------------------------------------
/*
clear
use "\\sdssrv03\Surveys\survey\COL\GEIH\2010\a\data_orig\personas 2010.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
save "\\sdssrv03\Surveys\survey\COL\GEIH\2010\a\data_orig\personas 2010.dta", replace
*/

clear
use "\\sdssrv03\Surveys\survey\COL\GEIH\2010\a\data_orig\hogares 2010.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
merge idh using "\\sdssrv03\Surveys\survey\COL\GEIH\2010\a\data_orig\personas 2010.dta"
tab _merge
drop _merge idh
cap egen id =concat (directorio secuencia_p orden)
sort id
destring mes, replace
keep if mes>=7 & mes<=9
keep  id nper-fex_dpto_c pet- ingtot
save "Y:\survey\COL\GEIH\2010\a\data_merge\pov_2010t3.dta", replace

clear
use "\\sdssrv03\Surveys\\survey\COL\GEIH\2010\t3\data_merge\COL_2010t3.dta", clear
cap egen id =concat (directorio secuencia_p orden)
sort id
merge 1:1 id using "Y:\survey\COL\GEIH\2010\a\data_merge\pov_2010t3.dta"
tab _merge
drop _merge
save "\\sdssrv03\Surveys\\survey\COL\GEIH\2010\t3\data_merge\COL_2010t3.dta", replace

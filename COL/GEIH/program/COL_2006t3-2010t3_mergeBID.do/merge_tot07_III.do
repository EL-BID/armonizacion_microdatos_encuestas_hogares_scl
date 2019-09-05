		******************************
		***BASE TOTAL NACIONAL 2007***
		******TERCER TRIMESTRE********
		******************************

cd "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos" 

use IIItrimestre_cab.dta
append using IIItrimestre_res.dta

gen temp=FEX1/3
replace FEX1=round(temp,1)
drop temp

gen temp=FEXHOG/3
replace FEXHOG=round(temp,1)
drop temp

compress

saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\col07.dta", replace

		******************************
		***BASE TOTAL NACIONAL 2006***
		******TERCER TRIMESTRE********
		******************************

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos" 

use IIItrimestre_cab.dta
append using IIItrimestre_res.dta

gen temp=FEX1/2
replace FEX1=round(temp,1)
drop temp

gen temp=FEXHOG/2
replace FEXHOG=round(temp,1)
drop temp

compress

saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\col06.dta", replace


*NOTA: aquí el factor se divide entre 2 porque realmente es un bimestre y no un trimestre ya que por
*el momento no se está incluyendo julio

*********************************************
****** Merge modulos de Vivienda y Hogar ****
******       Diciembre 2000              ****
*********************************************

*Name: Yessenia Loayza
*Date: July, 2012


clear all
set more off


cd "Y:\Ecuador\2000\ENEMDU\Dec\Datos Originales\Stata"
use "emedinho1200.dta", clear
sum idhogar edad sexo
generat id_persona= idhogar*1000+edad*10+sexo
sort id_persona
save "emedinho1200_idper.dta", replace

use "per1200.dta", clear
sort  ciudad zona sector vivienda hogar
egen idhogar = group(ciudad zona sector vivienda hogar)
sum idhogar edad sexo
generat id_persona= idhogar*1000+edad*10+sexo
sort id_persona

merge id_persona using "emedinho1200_idper.dta", keep(b1004) 
tab _merge
drop _merge
save "Y:\Ecuador\2000\ENEMDU\Dec\Data\ecu00.dta", replace
save "X:\ARM\ECU\ENEMDU\2000\Orig_data\ecu00.dta", replace

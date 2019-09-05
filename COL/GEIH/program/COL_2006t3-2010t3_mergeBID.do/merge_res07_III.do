	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2007 ***
	*********         RESTO         ***************
	***********************************************


*************************
**BASE DEL MES DE JULIO**
*************************

clear

cd "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos originales\0707\RESTO\Stata\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados7.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use inactivos7.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use ocupados7.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use otrosingresos7.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use otrasactividades7.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear
use ftrab7.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear
use educacion7.dta    
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

merge DIRECTORIO SECUENCIA_P ORDEN using ftrab7.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using otrasactividades7.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using otrosingresos7.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using ocupados7.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using inactivos7.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using desocupados7.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace




*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda7.dta
sort DIRECTORIO SECUENCIA_P  
save, replace

use "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta"
sort DIRECTORIO SECUENCIA_P 
merge DIRECTORIO SECUENCIA_P  using vivienda7.dta
tab _merge 
drop _merge
compress
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace




**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos originales\0708\RESTO\Stata\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados8.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use inactivos8.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use ocupados8.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use otrosingresos8.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use otrasactividades8.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear
use ftrab8.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear
use educacion8.dta    
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

merge DIRECTORIO SECUENCIA_P ORDEN using ftrab8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace


merge DIRECTORIO SECUENCIA_P ORDEN using otrasactividades8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using otrosingresos8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using ocupados8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using inactivos8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using desocupados8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda8.dta
sort DIRECTORIO SECUENCIA_P  
save, replace

use "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta"
sort DIRECTORIO SECUENCIA_P 
merge DIRECTORIO SECUENCIA_P  using vivienda8.dta
tab _merge 
drop _merge
compress
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace
clear


******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos originales\0709\RESTO\Stata\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados9.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use inactivos9.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use ocupados9.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use otrosingresos9.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear 
use otrasactividades9.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear
use ftrab9.dta
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

clear
use educacion9.dta    
sort DIRECTORIO SECUENCIA_P ORDEN
save, replace

merge DIRECTORIO SECUENCIA_P ORDEN using ftrab9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace


merge DIRECTORIO SECUENCIA_P ORDEN using otrasactividades9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using otrosingresos9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using ocupados9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using inactivos9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using desocupados9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda9.dta
sort DIRECTORIO SECUENCIA_P  
save, replace

use "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta"
sort DIRECTORIO SECUENCIA_P 
merge DIRECTORIO SECUENCIA_P  using vivienda9.dta
tab _merge 
drop _merge
compress
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace
clear


****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\"

use july07_res.dta
append using august07_res.dta
append using september07_res.dta
compress
saveold "\\sdssrv03\Surveys\Colombia\2007\GEIH\3thquarter\Datos\IIItrimestre_res.dta", replace






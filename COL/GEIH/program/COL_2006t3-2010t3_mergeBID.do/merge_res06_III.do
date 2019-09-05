	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2006 ***
	*********         RESTO         ***************
	***********************************************



**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos originales\0608\RESTO\Stata\"


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
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace


merge DIRECTORIO SECUENCIA_P ORDEN using otrasactividades8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using otrosingresos8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using ocupados8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using inactivos8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using desocupados8.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda8.dta
sort DIRECTORIO SECUENCIA_P  
save, replace

use "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta"
sort DIRECTORIO SECUENCIA_P 
merge DIRECTORIO SECUENCIA_P  using vivienda8.dta
tab _merge 
drop _merge
compress
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_res.dta", replace






******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos originales\0609\RESTO\Stata\"


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
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace


merge DIRECTORIO SECUENCIA_P ORDEN using otrasactividades9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN 
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using otrosingresos9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using ocupados9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using inactivos9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace

merge DIRECTORIO SECUENCIA_P ORDEN using desocupados9.dta
tab _merge
drop _merge
sort DIRECTORIO SECUENCIA_P ORDEN
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda9.dta
sort DIRECTORIO SECUENCIA_P  
save, replace

use "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta"
sort DIRECTORIO SECUENCIA_P 
merge DIRECTORIO SECUENCIA_P  using vivienda9.dta
tab _merge 
drop _merge
compress
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_res.dta", replace






****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\"

clear
use using august06_res.dta
append using september06_res.dta
gen CABECERA=0
gen TRIMESTRE=3
egen LLAVE_H=group(DIRECTORIO SECUENCIA_P )
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
compress
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\IIItrimestre_res.dta", replace



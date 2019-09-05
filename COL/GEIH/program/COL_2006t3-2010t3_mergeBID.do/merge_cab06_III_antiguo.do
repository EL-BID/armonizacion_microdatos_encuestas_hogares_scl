	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2006 ***
	********* CABECERAS MUNICIPALES ***************
	***********************************************



**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos originales\0608\CABECERA\Stata\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados8.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use inactivos8.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use ocupados8.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use otrosingresos8.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use otrasactividades8.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear
use ftrab8.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear
use educacion8.dta    
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

merge LLAVE using ftrab8.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace

merge LLAVE using otrasactividades8.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace

merge LLAVE using otrosingresos8.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace

merge LLAVE using ocupados8.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace

merge LLAVE using inactivos8.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace

merge LLAVE using desocupados8.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda8.dta
gen TRIMESTRE=3
sort DIRECTORIO SECUENCIA_P
save, replace

clear
use "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta"
sort DIRECTORIO SECUENCIA_P
merge DIRECTORIO SECUENCIA_P using vivienda8.dta
tab _merge 
drop _merge
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\august06_cab.dta", replace

clear


******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos originales\0609\CABECERA\Stata\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados9.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use inactivos9.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use ocupados9.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use otrosingresos9.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use otrasactividades9.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear
use ftrab9.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear
use educacion9.dta    
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

merge LLAVE using ftrab9.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace

merge LLAVE using otrasactividades9.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace

merge LLAVE using otrosingresos9.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace

merge LLAVE using ocupados9.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace

merge LLAVE using inactivos9.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace

merge LLAVE using desocupados9.dta
tab _merge
drop _merge
sort LLAVE
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda9.dta
gen TRIMESTRE=3
sort DIRECTORIO SECUENCIA_P
save, replace

clear
use "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta"
sort DIRECTORIO SECUENCIA_P
merge DIRECTORIO SECUENCIA_P using vivienda9.dta
tab _merge 
drop _merge
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\september06_cab.dta", replace

clear


****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\"

use august06_cab.dta
append using september06_cab.dta
compress
saveold "\\sdssrv03\Surveys\Colombia\2006\GEIH\3thquarter\Datos\IIItrimestre_cab.dta", replace







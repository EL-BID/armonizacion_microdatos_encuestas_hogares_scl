	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2007 ***
	*********         RESTO         ***************
	***********************************************


*************************
**BASE DEL MES DE JULIO**
*************************

clear

cd "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos originales\0707\RESTO\Stata\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados7.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use inactivos7.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use ocupados7.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use otrosingresos7.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear 
use otrasactividades7.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear
use ftrab7.dta
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

clear
use educacion7.dta    
gen TRIMESTRE=3
egen LLAVE=group(DIRECTORIO SECUENCIA_P ORDEN)
move LLAVE  I_DPTO
sort LLAVE
save, replace

merge LLAVE using ftrab7.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge LLAVE using otrasactividades7.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge LLAVE using otrosingresos7.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge LLAVE using ocupados7.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge LLAVE using inactivos7.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace

merge LLAVE using desocupados7.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda7.dta
gen TRIMESTRE=3
sort DIRECTORIO SECUENCIA_P
save, replace

clear
use "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta"
sort DIRECTORIO SECUENCIA_P
merge DIRECTORIO SECUENCIA_P using vivienda7.dta
tab _merge 
drop _merge
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\july07_res.dta", replace




**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos originales\0708\RESTO\Stata\"


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
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge LLAVE using otrasactividades8.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge LLAVE using otrosingresos8.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge LLAVE using ocupados8.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge LLAVE using inactivos8.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

merge LLAVE using desocupados8.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda8.dta
gen TRIMESTRE=3
sort DIRECTORIO SECUENCIA_P
save, replace

clear
use "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta"
sort DIRECTORIO SECUENCIA_P
merge DIRECTORIO SECUENCIA_P using vivienda8.dta
tab _merge 
drop _merge
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\august07_res.dta", replace

clear


******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos originales\0709\RESTO\Stata\"


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
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge LLAVE using otrasactividades9.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge LLAVE using otrosingresos9.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge LLAVE using ocupados9.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge LLAVE using inactivos9.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

merge LLAVE using desocupados9.dta
tab _merge
drop _merge
sort LLAVE
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda9.dta
gen TRIMESTRE=3
sort DIRECTORIO SECUENCIA_P
save, replace

clear
use "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta"
sort DIRECTORIO SECUENCIA_P
merge DIRECTORIO SECUENCIA_P using vivienda9.dta
tab _merge 
drop _merge
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\september07_res.dta", replace

clear


****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\"

use july07_res.dta
append using august07_res.dta
append using september07_res.dta
compress
saveold "${surveysFolder}\Colombia\2007\GEIH\3thquarter\Datos\IIItrimestre_res.dta", replace







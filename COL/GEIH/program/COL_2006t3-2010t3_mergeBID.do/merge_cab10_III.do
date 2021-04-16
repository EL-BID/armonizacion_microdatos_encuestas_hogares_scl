	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2010 ***
	********* CABECERAS MUNICIPALES ***************
	***********************************************


*************************
**BASE DEL MES DE JULIO**
*************************

clear

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos originales\1007\cabecera\"


*Pegando los archivos de personas*
**********************************

clear 
use sas70cab1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas80cab1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas60cab1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas95cab1007.dta 
sort directorio secuencia_p orden
save, replace

clear 
use sas90cab1007.dta
sort directorio secuencia_p orden
save, replace

clear
use sas50cab1007.dta
sort directorio secuencia_p orden
save, replace

clear
use sas10cab1007.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using sas50cab1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace

merge directorio secuencia_p orden using sas90cab1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace

merge directorio secuencia_p orden using sas95cab1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace

merge directorio secuencia_p orden using sas60cab1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace

merge directorio secuencia_p orden using sas80cab1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace

merge directorio secuencia_p orden using sas70cab1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace




*Pegando el archivo de vivienda y hogar*
****************************************

clear
use sas01cab1007.dta
sort directorio secuencia_p  
save, replace

use "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta"
sort directorio secuencia_p
merge directorio secuencia_p  using sas01cab1007.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_cab.dta", replace




**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos originales\1008\cabecera\"


*Pegando los archivos de personas*
**********************************

clear 
use sas70cab1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas80cab1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas60cab1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas95cab1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas90cab1008.dta
sort directorio secuencia_p orden
save, replace

clear
use sas50cab1008.dta
sort directorio secuencia_p orden
save, replace

clear
use sas10cab1008.dta     
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using sas50cab1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace


merge directorio secuencia_p orden using sas90cab1008.dta 
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace

merge directorio secuencia_p orden using sas95cab1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace

merge directorio secuencia_p orden using sas60cab1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace

merge directorio secuencia_p orden using sas80cab1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace

merge directorio secuencia_p orden using sas70cab1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use sas01cab1008.dta
sort directorio secuencia_p  
save, replace

use "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta"
sort directorio secuencia_p 
merge directorio secuencia_p  using sas01cab1008.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_cab.dta", replace
clear


******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos originales\1009\cabecera\"


*Pegando los archivos de personas*
**********************************

clear 
use  sas70cab1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas80cab1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas60cab1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas95cab1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas90cab1009.dta
sort directorio secuencia_p orden
save, replace

clear
use sas50cab1009.dta
sort directorio secuencia_p orden
save, replace

clear
use sas10cab1009.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using sas50cab1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace

merge directorio secuencia_p orden using sas90cab1009.dta 
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace

merge directorio secuencia_p orden using sas95cab1009.dta 
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace

merge directorio secuencia_p orden using sas60cab1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace

merge directorio secuencia_p orden using sas80cab1009.dta 
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace

merge directorio secuencia_p orden using sas70cab1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use sas01cab1009.dta
sort directorio secuencia_p 
save, replace

use "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta"
sort directorio secuencia_p
merge directorio secuencia_p using sas01cab1009.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_cab.dta", replace
clear


****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\"

use july10_cab.dta
append using august10_cab.dta
append using september10_cab.dta

compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\IIItrimestre_cab.dta", replace






